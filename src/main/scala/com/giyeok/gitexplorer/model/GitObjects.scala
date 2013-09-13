package com.giyeok.gitexplorer.model

import scala.Array.canBuildFrom

import com.giyeok.gitexplorer.Util.BitOperableInt
import com.giyeok.gitexplorer.Util.UnsignedByte

trait GitObjects {
    this: GitRepository =>

    sealed abstract class GitObject {
        val id: GitId
        val content: Array[Byte]
    }
    object GitObject {
        def fromPackfile(pack: GitPackfile, id: GitId, objType: Int, raw: Array[Byte]): GitObject = {
            if (objType == 1) {
                val content = GitRepository.inflate(raw)
                println(s"== $id type: ${objType.toHexString} ==========")
                println(new String(content map { _.toChar }))
                println("====================================")
            } else {
                println(s"Raw: ${new String(raw map { _.toChar })}")
            }
            GitTree(id, new Array[Byte](0))
        }
    }

    case class GitTree(id: GitId, content: Array[Byte]) extends GitObject
    case class GitBlob(id: GitId, content: Array[Byte]) extends GitObject
    case class GitCommit(id: GitId, content: Array[Byte]) extends GitObject
    case class GitTag(id: GitId, content: Array[Byte]) extends GitObject
    case class GitDelta(id: GitId, content: Array[Byte], original: GitId) extends GitObject {
        // NOTE Currently, assumes pack version 3
        val (baseObjectLength, resultObjectLength, deltaOps) = {
            // from patch-delta.c
            var pointer = 0
            def next = {
                pointer += 1
                last
            }
            def last = {
                content(pointer - 1)
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
            while (pointer < content.length) {
                var opcode = next.toUB
                // println(s"opcode ${opcode.toBinaryString}")
                if ((opcode & 0x80) == 0) {
                    // "insert"
                    val inserted = content slice (pointer, pointer + opcode)
                    pointer += opcode
                    // println(s"insert $opcode ${new String(inserted map { _.toChar })}")
                } else {
                    // "copy"
                    // println(s"copy ${(opcode & 0x7f).toBinaryString}")
                    var (offset, size) = (0L, 0L)
                    if (opcode &? 0x01) offset = next.toUB
                    if (opcode &? 0x02) offset |= next.toUB << 8
                    if (opcode &? 0x04) offset |= next.toUB << 16
                    if (opcode &? 0x08) offset |= next.toUB.toLong << 24
                    if (opcode &? 0x10) size = next.toUB
                    if (opcode &? 0x20) size |= next.toUB << 8
                    if (opcode &? 0x40) size |= next.toUB << 16
                    if (size == 0) size = 0x10000
                    // println(s"offset:$offset/${offset.toBinaryString} length:$size/${size.toBinaryString} pointer:$pointer")
                }
            }
            (baseObjectLength, resultObjectLength, 0)
        }

        def recover: GitObject = {
            // calculate its real value
            GitUnknown(id, 0)
        }
    }
    case class GitUnknown(id: GitId, objType: Int, content: Array[Byte] = new Array[Byte](0)) extends GitObject
}
