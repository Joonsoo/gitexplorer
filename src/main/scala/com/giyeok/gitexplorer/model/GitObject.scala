package com.giyeok.gitexplorer.model

sealed abstract class GitObject(val id: String) {

}

class GitTree(id: String) extends GitObject(id)
class GitBlob(id: String) extends GitObject(id)
class GitCommit(id: String) extends GitObject(id)
class GitTag(id: String) extends GitObject(id)
