package com.giyeok.gitexplorer

import java.util.zip.Inflater
import scala.util.matching.Regex
import java.io.ByteArrayOutputStream
import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream

object Util {
    implicit class BitOperableInt(i: Int) {
        def &?(x: Int) = (i & x) != 0
    }
    implicit class UnsignedByte(b: Byte) {
        def toUB = b.toInt & 0xff
    }
    implicit class ToContentString(a: Array[Byte]) {
        def toContent = new String(a map { _.toChar })
    }
    implicit class RegexMatchable(r: Regex) {
        def matches(s: String) = !(r findAllIn s).isEmpty
    }

    object LastDotSplittedString {
        def apply(string: String): (String, String) = {
            val i = string.lastIndexOf('.')
            if (i >= 0) (string.substring(0, i), string.substring(i + 1)) else (string, "")
        }
        def unapply(string: String): Option[(String, String)] = {
            val i = string.lastIndexOf('.')
            if (i >= 0) Some(string.substring(0, i), string.substring(i + 1)) else None
        }
    }
    object SpaceSplittedString {
        def unapply(string: String): Option[(String, String)] = {
            val i = string.indexOf(' ')
            if (i >= 0) Some(string.substring(0, i), string.substring(i + 1)) else None
        }
    }
    object NullSplittedByteArray {
        def unapply(array: Array[Byte]): Option[(Array[Byte], Array[Byte])] = {
            array.zipWithIndex.find(_._1 == 0) match {
                case Some((_, i)) => Some(array take i, array drop (i + 1))
                case _ => None
            }
        }
    }

    class LineIterator(content: Array[Byte]) extends Iterator[String] {
        self =>

        private var _pointer = 0
        private var _last = ""
        private var lastConsumed = true

        def pointer = _pointer
        override def hasNext = _pointer < content.length
        def last = _last
        override def next() = {
            // TODO improve performance
            if (!lastConsumed) {
                lastConsumed = true
                last
            } else {
                val line = content drop pointer takeWhile (_ != '\n') map { _.toChar }
                _pointer += line.length + 1
                _last = new String(line)
                last
            }
        }

        def process[A](block: String => (Boolean, A)): A = {
            val (consumed, result) = block(next)
            lastConsumed = consumed
            result
        }
        def processWhile[A](block: String => Option[A]): List[A] = {
            var result = List[A]()
            var continue = true
            while (continue) {
                block(next) match {
                    case Some(x) => result +:= x
                    case _ => continue = false
                }
            }
            lastConsumed = false
            result.reverse
        }
    }

    def readFile(f: File): Array[Byte] = {
        val fos = new ByteArrayOutputStream(65535)
        val bis = new BufferedInputStream(new FileInputStream(f))
        val buf = new Array[Byte](1024)
        Stream.continually(bis.read(buf))
            .takeWhile(_ != -1)
            .foreach(fos.write(buf, 0, _))
        fos.toByteArray
    }

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
}
