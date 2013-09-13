package com.giyeok.gitexplorer.model

import scala.Array.canBuildFrom
import scala.Option.option2Iterable

import com.giyeok.dexdio.dexreader.EndianRandomAccessFile
import com.giyeok.gitexplorer.Util.BitOperableInt

trait GitPackfiles {
    this: GitRepository =>

    class GitPackfile(idxpath: String, packpath: String) {
        def this(path: String) = this(path + ".idx", path + ".pack")

        val idx = {
            var idx: EndianRandomAccessFile = null
            var pack: EndianRandomAccessFile = null
            try {
                idx = new EndianRandomAccessFile(idxpath, "r")
                pack = new EndianRandomAccessFile(packpath, "r")

                // reading pack file
                pack.setEndian(true)
                pack.seek(0)

                // validating pack file
                // 1. signature
                val packSignature = pack.readLength(4).toSeq
                if (packSignature != Seq('P', 'A', 'C', 'K'))
                    throw InvalidFormat("Invalid Packfile")

                // 2. version
                val packVersion = pack.readInt()
                packVersion match {
                    case 2 =>
                    case 3 => throw new NotImplementedError("Pack version 3 is not supported yet")
                    case _ => throw InvalidFormat(s"Unknown Pack version: $packVersion")
                }

                val objectsCount = pack.readInt()

                // reading idx file
                idx.setEndian(true)
                idx.seek(0)

                val firstValue = idx.readLength(4).toSeq
                val idxversion = idx.readInt()
                val idxfile = (firstValue, idxversion) match {
                    case (Seq(-1, 't', 'O', 'c'), 2) =>
                        // version 2
                        val fanout = (0 until 256) map { _ =>
                            idx.readInt()
                        }
                        // println(fanout)

                        val objectNames = (0 until objectsCount) map { _ =>
                            GitSHA1(idx.readLength(20))
                        }
                        // objectNames takeRight 100 foreach { x => println(x.string) }

                        val crc32 = (0 until objectsCount) map { _ =>
                            idx.readInt()
                        }
                        // println(crc32 take 100)

                        val offsets4 = (0 until objectsCount) map { _ => idx.readInt() }
                        val eights = offsets4 filter { x => (x & 0x10000000) != 0 }
                        val offsets8 = (0 until eights.length) map { _ => idx.readLong() }

                        val packSHA1 = GitSHA1(idx.readLength(20))

                        val idxSHA1 = GitSHA1(idx.readLength(20))

                        new IdxFile(objectsCount, fanout, objectNames, Some(crc32), offsets4, offsets8, pack.length() - 20)
                    case _ =>
                        // version 1
                        throw new NotImplementedError("idx version 1 is not implemented yet")
                }

                idxfile
            } finally {
                if (idx != null) idx.close()
                if (pack != null) pack.close()
            }
        }

        class IdxFile(
            val objectsCount: Int,
            val fanout: Seq[Int],
            val objectNames: Seq[GitId],
            val crc32: Option[Seq[Int]],
            val offsets4: Seq[Int],
            offsets8: Seq[Long],
            packSize: Long) {

            assert({
                def isSorted[T <% Ordered[T]](l: List[T], canEqual: Boolean = false): Boolean = l match {
                    case (_ +: Nil) | Nil => true
                    case x1 +: x2 +: xs if x1 < x2 || !(!canEqual && x1 != x2) => isSorted(x2 +: xs)
                    case _ => false
                }
                val fanoutOrdered = fanout.length == 256 && isSorted(fanout.toList, true)
                val objectsOrdered = isSorted(objectNames.toList)
                fanoutOrdered && objectsOrdered
            })
            assert(objectsCount == objectNames.length && objectsCount == offsets4.length)

            val sizeFromOffset = {
                val orderedOffsets = (offsets4 map { realOffset(_) }).sorted.toList
                val mymap = scala.collection.mutable.Map[Long, Long]()
                def diffOffset(offsets: List[Long]): Unit = {
                    offsets match {
                        case o1 +: o2 +: os =>
                            mymap(o1) = o2 - o1
                            diffOffset(o2 +: os)
                        case Seq(l) =>
                            mymap(l) = packSize - l
                    }
                }
                diffOffset(orderedOffsets)
                Map(mymap.toList: _*)
            }
            val objectNameFromOffset = {
                (offsets4 map { realOffset _ } zip objectNames).toMap
            }

            def findOffsetFor(id: GitId): Option[Long] = {
                def binarySearch(left: Int, right: Int): Option[Int] = {
                    if (left > right) None
                    else {
                        val mid = (right + left) / 2
                        val midv = objectNames(mid)
                        if (id == midv) Some(mid)
                        else if (id < midv) binarySearch(left, mid - 1)
                        else binarySearch(mid + 1, right)
                    }
                }
                // TODO improve this using fanout
                binarySearch(0, objectNames.length - 1) match {
                    case Some(offsetIndex) =>
                        Some(realOffset(offsets4(offsetIndex)))
                    case None => None
                }
            }

            def realOffset(offset4: Int): Long = {
                if ((offset4 & 0x10000000) != 0) {
                    // TODO offset8
                    throw new UnsupportedOperationException
                } else {
                    offset4
                }
            }
        }

        def findObject(pack: EndianRandomAccessFile, id: GitId): Option[GitObject] = {
            val offset = idx.findOffsetFor(id)
            offset match {
                case Some(offset) => readFromOffset(pack, offset, id)
                case None => None
            }
        }

        def readFromOffset(pack: EndianRandomAccessFile, offset: Long, id: GitId): Option[GitObject] = {
            pack.seek(offset)
            var read = pack.readByte()
            val objectType = (read & 0x70) >> 4
            val size = {
                var value = read & 0xf
                var counter = 4
                while ((read & 0x80) != 0) {
                    read = pack.readByte()
                    value = value | ((read & 0x7f) << counter)
                    counter += 7
                }
                value
            }
            val sizeInPack = idx.sizeFromOffset(offset)
            if (size > Integer.MAX_VALUE || sizeInPack > Integer.MAX_VALUE)
                throw new UnsupportedOperationException(s"Too big object: $id")
            // println(s"$id $objectType      $size $sizeInPack $offset")

            val delta = objectType & 0x4
            val realType = objectType & 0x3
            try {
                /* git - cache.h
                 *  enum object_type {
						OBJ_BAD = -1,
						OBJ_NONE = 0,
						OBJ_COMMIT = 1,
						OBJ_TREE = 2,
						OBJ_BLOB = 3,
						OBJ_TAG = 4,
						/* 5 for future expansion */
						OBJ_OFS_DELTA = 6,
						OBJ_REF_DELTA = 7,
						OBJ_ANY,
						OBJ_MAX
					};
                 * 
                 */
                def readAndInflate(size: Int) = {
                    val raw = pack.readLength(size.toInt)
                    GitRepository.inflate(raw)
                }
                objectType match {
                    case 0x1 =>
                        // raw commit
                        println(s"$id commit $size $sizeInPack $offset")
                        Some(GitCommit(id, readAndInflate(sizeInPack.toInt)))
                    case 0x2 =>
                        // tree
                        println(s"$id tree   $size $sizeInPack $offset")
                        Some(GitTree(id, readAndInflate(sizeInPack.toInt)))
                    case 0x3 =>
                        // blob
                        println(s"$id blob   $size $sizeInPack $offset")
                        Some(GitBlob(id, readAndInflate(sizeInPack.toInt)))
                    case 0x4 =>
                        // tag
                        println(s"$id tag    $size $sizeInPack $offset")
                        Some(GitTag(id, readAndInflate(sizeInPack.toInt)))
                    case 0x6 =>
                        // ofs_delta
                        val (negOffset, offsetLen) = {
                            var read = pack.readByte()
                            var value = read & 0x7f
                            var aug = 0
                            var len = 1
                            while (read &? 0x80) {
                                read = pack.readByte()
                                value = (value << 7) | (read & 0x7f)
                                aug = (aug << 7) | (1 << 7)
                                len += 1
                            }
                            (value + aug, len)
                        }
                        val originalOffset = offset - negOffset
                        val inflated = readAndInflate(sizeInPack.toInt - offsetLen)
                        val original = idx.objectNameFromOffset.getOrElse(originalOffset, { throw InvalidFormat("wrong ofs_delta offset") })
                        println(s"$id delta  $size $sizeInPack $offset \\ $original")
                        Some(GitDelta(id, inflated, original))
                    case t =>
                        // unknown?
                        println(s"$id unknown $objectType $size $sizeInPack $offset")
                        Some(GitUnknown(id, objectType))
                }
            } catch {
                case x: Throwable =>
                    x.printStackTrace()
                    None
            }
        }

        def findObject(id: GitId): Option[GitObject] = {
            var pack: EndianRandomAccessFile = null
            try {
                pack = new EndianRandomAccessFile(packpath, "r")
                findObject(pack, id)
            } finally {
                if (pack == null) pack.close()
            }
        }

        def allObjects = {
            var pack: EndianRandomAccessFile = null
            try {
                pack = new EndianRandomAccessFile(packpath, "r")
                val objs = (idx.objectNames zip idx.offsets4) flatMap {
                    case (objName, offset) =>
                        readFromOffset(pack, idx.realOffset(offset), objName)
                }
                objs
            } finally {
                if (pack == null) pack.close()
            }
        }

        def objectNames = idx.objectNames
    }
}

object PackfileTester {
    def main(args: Array[String]): Unit = {
        val repo = new GitRepository
        val packfile = new repo.GitPackfile("samples/my/.git/objects/pack/pack-7f3a02f7e5046988a88c86e37fcf7de5816f73f4")
        val all = packfile.allObjects
        println(all flatMap {
            case repo.GitUnknown(id, objType, _) => Some(objType)
            case _ => None
        } toSet)
    }
}
