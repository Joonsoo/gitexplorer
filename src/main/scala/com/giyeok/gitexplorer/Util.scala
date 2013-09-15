package com.giyeok.gitexplorer

import java.util.zip.Inflater

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
