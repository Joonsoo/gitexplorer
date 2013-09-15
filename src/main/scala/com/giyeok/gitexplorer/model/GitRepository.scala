package com.giyeok.gitexplorer.model

import java.io.BufferedInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.util.zip.Inflater
import com.giyeok.gitexplorer.Util

class GitRepository extends GitObjects with GitPackfiles with GitHash {
    case class InvalidFormat(msg: String) extends Exception

    type GitId = GitSHA1
}

object GitRepository {
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
            Util.inflate(readFile(f))
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
