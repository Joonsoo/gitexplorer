package com.giyeok.gitexplorer.model

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

    case class GitAbstractObject(id: GitId, objType: Int, content: Array[Byte] = new Array[Byte](0)) extends GitObject
    case class GitTree(id: GitId, content: Array[Byte]) extends GitObject
    case class GitBlob(id: GitId, content: Array[Byte]) extends GitObject
    case class GitCommit(id: GitId, content: Array[Byte]) extends GitObject
    case class GitTag(id: GitId, content: Array[Byte]) extends GitObject
}
