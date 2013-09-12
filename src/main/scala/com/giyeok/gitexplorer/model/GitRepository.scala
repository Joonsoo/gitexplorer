package com.giyeok.gitexplorer.model

import java.io.BufferedInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.util.zip.Inflater

class GitRepository extends GitObjects with GitPackfiles {
    case class InvalidFormat(msg: String) extends Exception

    case class GitSHA1(sha1: Seq[Byte]) extends Ordered[GitSHA1] {
        def this(sha1: String) = this(GitSHA1.arrayFromString(sha1))

        assert(sha1.length == 20)

        override def equals(other: Any): Boolean = other match {
            case that: GitSHA1 => this.sha1 == that.sha1
            case _ => false
        }

        override def compare(other: GitSHA1): Int = {
            // NOTE Is there an API to compare two seqs?
            (sha1 zip other.sha1).foldLeft(0)({
                case (0, (a, b)) =>
                    val (aa, bb) = (a.toChar & 0xff, b.toChar & 0xff)
                    if (aa < bb) -1
                    else if (aa > bb) 1
                    else 0
                case (-1, _) => -1
                case (1, _) => 1
            })
        }

        override def toString = string

        lazy val string = {
            (sha1 map { x => ((x.toInt & 0xff) | 0x100).toHexString.substring(1) }).mkString
        }
    }
    object GitSHA1 {
        def arrayFromString(s: String): Seq[Byte] = {
            val ss = ((1 to (40 - s.length)) map { _ => "0" } mkString) + s
            assert(ss.length == 40)
            def hexCharToByte(x: Char) = x match {
                case n if ('0' to '9') contains n => n - '0'
                case a if ('a' to 'f') contains a => a - 'a' + 10
                case a if ('A' to 'f') contains a => a - 'A' + 10
            }
            for (i <- (0 until 40 by 2)) yield {
                val x1 = hexCharToByte(ss.charAt(i))
                val x2 = hexCharToByte(ss.charAt(i + 1))
                (x1 * 16 + x2).toByte
            }
        }
    }

    type GitId = GitSHA1

    val objects = Map[String, GitObject]()
}

object GitRepository {
    private val inflater = new Inflater()

    def inflate(content: Array[Byte]) = {
        inflater.reset()
        inflater.setInput(content)
        var result = List[Array[Byte]]()
        var length = 1
        var totalLegnth = 0
        while (length > 0) {
            val inflated = new Array[Byte](100)
            length = inflater.inflate(inflated, 0, 100)
            result +:= inflated take length
            totalLegnth += length
        }
        val inflated = new Array[Byte](totalLegnth)
        result.foldRight(0) { (block, pointer) =>
            Array.copy(block, 0, inflated, pointer, block.length)
            pointer + block.length
        }
        inflated
    }

    def loadFrom(path: String) = {
        // load git repository at `path` and returns GitRepository
        def readFile(f: File): Array[Byte] = {
            val fos = new ByteArrayOutputStream(65535)
            val bis = new BufferedInputStream(new FileInputStream(f))
            val buf = new Array[Byte](1024)
            Stream.continually(bis.read(buf))
                .takeWhile(_ != -1)
                .foreach(fos.write(buf, 0, _))
            fos.toByteArray
        }
        def inflateFile(f: File) = {
            inflate(readFile(f))
        }

        val objects = new File(path, "objects")
        objects.list foreach { subpath =>
            if (subpath.length == 2) {
                val subdirs = new File(objects, subpath)
                subdirs.list foreach { obj =>
                    val f = new File(subdirs, obj)
                    val inflated = inflateFile(f)
                    println(inflated.length, new String(inflated))
                }
            }
        }
    }
}
