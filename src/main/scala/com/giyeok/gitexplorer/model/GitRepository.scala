package com.giyeok.gitexplorer.model

import java.io.File

import com.giyeok.gitexplorer.Util.LastDotSplittedString
import com.giyeok.gitexplorer.ui.GitObjectViews

class GitRepository(val path: String)
    extends GitObjects with GitObjectViews with GitPackfiles with GitRefs {
    case class InvalidFormat(msg: String) extends Exception

    val root = new File(path)

    type GitId = GitSHA1

    abstract class GitObjectStore {
        def hasObject(id: GitId): Boolean
        def getObject(id: GitId): Option[GitObject]

        def allObjects: Map[GitId, GitObject]
        def allObjectIds: Set[GitId]
    }

    // add Packfiles
    private val packfiles: List[GitPackfile] = {
        val packFolder = new File(root, "/objects/pack")
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
    // read refs
    val refs = loadRefs

    protected val _objectStores = List[GitObjectStore](GitObjects) ++ packfiles

    lazy val allObjects = (_objectStores flatMap { _.allObjects }).toMap
    lazy val allObjectIds = (_objectStores flatMap { _.allObjectIds }).toSet
    def getObject(id: GitId) = {
        // NOTE (_objectStores foldLeft None) does not work.. it is understandable, but a little bit odd
        // Wouldn't None[GitObject] be better?
        (_objectStores foldLeft Option.empty[GitObject]) {
            case (result @ Some(_), store) => result
            case (None, store) => store.getObject(id)
        }
    }

    lazy val allCommits = allObjects flatMap {
        case (_, commit: GitCommit) => Some(commit)
        case _ => None
    }
    lazy val allTags = allObjects flatMap {
        case (_, tag: GitTag) => Some(tag)
        case _ => None
    }
}
