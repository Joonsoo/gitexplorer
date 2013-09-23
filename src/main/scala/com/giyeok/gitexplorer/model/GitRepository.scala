package com.giyeok.gitexplorer.model

import java.io.BufferedInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream

import com.giyeok.gitexplorer.Util._

class GitRepository(val path: String) extends GitObjects with GitPackfiles with GitHash {
    case class InvalidFormat(msg: String) extends Exception

    type GitId = GitSHA1

    abstract class GitObjectStore {
        def hasObject(id: GitId): Boolean
        def getObject(id: GitId): Option[GitObject]

        def allObjects: Map[GitId, GitObject]
        def allObjectIds: Set[GitId]
    }

    // add Packfiles
    private val packfiles: List[GitPackfile] = {
        val packFolder = new File(path + "/objects/pack")
        if (!packFolder.exists()) List()
        else {
            val packs = packFolder.list().toSet
            val packIdx = packs groupBy { LastDotSplittedString(_)._2.toLowerCase } map {
                case (ext, full) => (ext, full map { LastDotSplittedString(_)._1 })
            }
            val commons = packIdx.getOrElse("pack", Set()) & packIdx.getOrElse("idx", Set())
            (commons map { name => new GitPackfile(path + "/objects/pack/" + name) }).toList
        }
    }

    protected val _objectStores = List[GitObjectStore](GitObjects) ++ packfiles

    def allObjects = (_objectStores flatMap { _.allObjects }).toMap
    def allObjectIds = (_objectStores flatMap { _.allObjectIds }).toSet
    def getObject(id: GitId) = {
        // NOTE (_objectStores foldLeft None) does not work.. it is understandable, but a little bit odd
        // Wouldn't None[GitObject] be better?
        (_objectStores foldLeft Option.empty[GitObject]) {
            case (result @ Some(_), store) => result
            case (None, store) => store.getObject(id)
        }
    }
}
