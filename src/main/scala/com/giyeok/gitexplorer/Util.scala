package com.giyeok.gitexplorer

object Util {
    implicit class BitOperableInt(i: Int) {
        def &?(x: Int) = (i & x) != 0
    }
    implicit class UnsignedByte(b: Byte) {
        def toUB = b.toInt & 0xff
    }

}