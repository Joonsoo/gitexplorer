package com.giyeok.gitexplorer.model

import java.io.BufferedInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.util.zip.Inflater

class GitRepository {

    val objects = Map[String, GitObject]()

}

object GitRepository {
    def loadFrom(path: String) = {
        // load git repository at `path` and returns GitRepository
        val inflater = new Inflater()

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
            val content = readFile(f)
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
