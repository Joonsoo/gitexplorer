package com.giyeok.gitexplorer.model

import com.giyeok.commons.io.EndianRandomAccessFile
import com.giyeok.gitexplorer.Util._

trait GitPackfiles {
    this: GitRepository =>

    class GitPackfile(val idxpath: String, val packpath: String) extends GitObjectStore {
        def this(path: String) = this(path + ".idx", path + ".pack")

        // TODO Maybe make it to load on-demand (for small jobs)
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

        case class GitUnknown(id: GitId, objType: Int, actualContent: Array[Byte] = new Array[Byte](0)) extends GitVirtualObject

        case class GitDelta(id: GitId, original: GitId, delta: Array[Byte]) extends GitVirtualObject {
            // NOTE Currently, assumes pack version 3

            abstract class DeltaOp
            case class DeltaInsert(content: Array[Byte]) extends DeltaOp {
                override def toString = s"DeltaInsert(${content.toContent})"
            }
            case class DeltaCopy(offset: Long, size: Int) extends DeltaOp

            lazy val (baseObjectLength, resultObjectLength, deltaOps) = {
                // from patch-delta.c
                var pointer = 0
                def next = {
                    pointer += 1
                    last
                }
                def last = {
                    delta(pointer - 1)
                }

                def readLittleEndian128Int = {
                    var value = next & 0x7f
                    var counter = 7
                    while (last &? 0x80) {
                        value = value | ((next & 0x7f) << counter)
                        counter += 7
                    }
                    value
                }
                val baseObjectLength = readLittleEndian128Int
                val resultObjectLength = readLittleEndian128Int

                // println(baseObjectLength, resultObjectLength)
                var deltaOps = List[DeltaOp]()
                while (pointer < delta.length) {
                    // println(s"opcode ${opcode.toBinaryString}")
                    deltaOps +:= (next.toUB match {
                        case 0 =>
                            throw InvalidFormat("Delta opcode 0 is for future expansion: not supported")
                        case opcode if opcode &? 0x80 =>
                            // "copy"
                            // println(s"copy ${(opcode & 0x7f).toBinaryString}")
                            var (offset, size) = (0L, 0)
                            if (opcode &? 0x01) offset = next.toUB
                            if (opcode &? 0x02) offset |= next.toUB << 8
                            if (opcode &? 0x04) offset |= next.toUB << 16
                            if (opcode &? 0x08) offset |= next.toUB.toLong << 24
                            if (opcode &? 0x10) size = next.toUB
                            if (opcode &? 0x20) size |= next.toUB << 8
                            if (opcode &? 0x40) size |= next.toUB << 16
                            if (size == 0) size = 0x10000
                            DeltaCopy(offset, size)
                        case opcode =>
                            // "insert"
                            val inserted = delta slice (pointer, pointer + opcode)
                            pointer += opcode
                            DeltaInsert(inserted)
                    })
                }
                (baseObjectLength, resultObjectLength, deltaOps.reverse)
            }

            def actualContent: Array[Byte] = {
                val source = getObject(original).getOrElse(throw InvalidFormat(s"Delta object refers to invalid object $original")).content.toSeq
                val blocks = deltaOps flatMap {
                    case DeltaCopy(offset, size) =>
                        assert(offset < Integer.MAX_VALUE)
                        source slice (offset.toInt, offset.toInt + size)
                    case DeltaInsert(content) => content.toSeq
                }
                blocks.toArray
            }
        }

        private def readFromOffset(pack: EndianRandomAccessFile, offset: Long, id: GitId): Option[GitVirtualObject] = {
            pack.seek(offset)
            var read = pack.readByte()
            val objectType = (read & 0x70) >> 4
            val size = {
                var value = read & 0xf
                var counter = 4
                while (read &? 0x80) {
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
                    inflate(raw)
                }
                objectType match {
                    case 0x1 =>
                        // raw commit
                        // println(s"$id commit $size $sizeInPack $offset")
                        val _content = readAndInflate(sizeInPack.toInt)
                        Some(new GitCommitExisting(id, _content))
                    case 0x2 =>
                        // tree
                        // println(s"$id tree   $size $sizeInPack $offset")
                        val _content = readAndInflate(sizeInPack.toInt)
                        Some(new GitTreeExisting(id, _content))
                    case 0x3 =>
                        // blob
                        // println(s"$id blob   $size $sizeInPack $offset")
                        val _content = readAndInflate(sizeInPack.toInt)
                        Some(new GitBlobExisting(id, _content))
                    case 0x4 =>
                        // tag
                        // println(s"$id tag    $size $sizeInPack $offset")
                        val _content = readAndInflate(sizeInPack.toInt)
                        Some(new GitTagExisting(id, _content))
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
                        val original = idx.objectNameFromOffset.getOrElse(originalOffset, { throw InvalidFormat("wrong ofs_delta offset") })
                        val inflated = readAndInflate(sizeInPack.toInt - offsetLen)
                        // println(s"$id delta  $size $sizeInPack $offset \\ $original")
                        Some(new GitDelta(id, original, inflated))
                    case t =>
                        // unknown?
                        // println(s"$id unknown $objectType $size $sizeInPack $offset")
                        Some(GitUnknown(id, objectType))
                }
            } catch {
                case x: Throwable =>
                    x.printStackTrace()
                    None
            }
        }

        private val knownObjects = scala.collection.mutable.Map[GitId, GitVirtualObject]()
        def registerObject(id: GitId, obj: GitVirtualObject): GitVirtualObject = {
            knownObjects(id) = obj
            obj
        }

        def getObject(pack: EndianRandomAccessFile, id: GitId): Option[GitVirtualObject] = {
            (knownObjects get id) match {
                case Some(obj) => Some(obj)
                case None =>
                    val offset = idx.findOffsetFor(id)
                    offset match {
                        case Some(offset) =>
                            readFromOffset(pack, offset, id) match {
                                case Some(obj) => Some(registerObject(id, obj))
                                case None => None
                            }
                        case None => None
                    }
            }
        }

        def getRawObject(id: GitId): Option[GitVirtualObject] = {
            (knownObjects get id) match {
                case Some(obj) => Some(obj)
                case None =>
                    var pack: EndianRandomAccessFile = null
                    try {
                        pack = new EndianRandomAccessFile(packpath, "r")
                        getObject(pack, id)
                    } finally {
                        if (pack == null) pack.close()
                    }
            }
        }

        private def getActualObject(id: GitId): GitObject = {
            // TODO improve performance
            allObjects(id)
        }

        def hasObject(id: GitId): Boolean = {
            // TODO improve performance
            getObject(id).isDefined
        }

        def getObject(id: GitId): Option[GitObject] = {
            Some(getActualObject(id))
        }

        private def readAllObjects = {
            var pack: EndianRandomAccessFile = null
            try {
                pack = new EndianRandomAccessFile(packpath, "r")
                val objs = (idx.objectNames zip idx.offsets4) flatMap {
                    case (objId, _) if knownObjects contains objId =>
                        Some(knownObjects(objId))
                    case (objId, offset) =>
                        readFromOffset(pack, idx.realOffset(offset), objId)
                }
                // println(s"${objs.length - (objs count { _.isInstanceOf[GitDelta] })} non-delta objects")
                objs
            } finally {
                if (pack == null) pack.close()
            }
        }

        lazy val allObjects: Map[GitId, GitObject] = {
            val objects = readAllObjects groupBy { _.id } map { case (key, value) => (key, value.head) ensuring value.length == 1 }

            val roots = scala.collection.mutable.Map[GitId, GitObject]()
            def rootOf(obj: GitVirtualObject): GitObject = obj match {
                case GitDelta(id, original, _) =>
                    roots.get(id) match {
                        case Some(root) => root
                        case None =>
                            val root = rootOf(objects(original))
                            roots(id) = root
                            root
                    }
                case x: GitObject => x
            }

            objects flatMap {
                case (id: GitId, delta: GitDelta) =>
                    val root = rootOf(delta)
                    Some(id, GitObject.fromTypes(id, root.objectType, () => delta.content))
                case (id: GitId, GitUnknown(_, objType, _)) =>
                    println(s"Unknown packfile object type: $id $objType")
                    None
                case (id: GitId, o: GitObject) => Some(id, o)
            }
        }

        lazy val allObjectIds = idx.objectNames.toSet
    }
}

object PackfileTester {
    def main(args: Array[String]): Unit = {
        val repo = new GitRepository("samples/git/.git")
        val packfile = new repo.GitPackfile("samples/git/.git/objects/pack/pack-144a2409b5481eff97e37a82cc508f347198e4cc")
        println(s"Start loading ${packfile.idxpath}")
        val all = packfile.allObjects
        println(s"${all.size} objects")
    }
}
