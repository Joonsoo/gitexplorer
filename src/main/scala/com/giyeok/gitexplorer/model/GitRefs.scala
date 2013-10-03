package com.giyeok.gitexplorer.model

import java.io.File
import scala.io.Source
import com.giyeok.gitexplorer.Util._

trait GitRefs {
    this: GitRepository =>

    def loadRefs: Map[String, GitId] = {
        val refs = new File(root, "refs")
        val packed = new File(root, "packed-refs")

        var list = List[(String, GitId)]()

        if (refs.exists) {
            implicit class FindAllSubs(file: File) {
                def allsubs(path: String): Seq[(String, File)] = {
                    if (file.isDirectory()) file.listFiles() flatMap { sub => sub.allsubs(path + file.getName() + "/") }
                    else Seq((path + file.getName(), file))
                }
            }
            val heads = new File(refs, "heads")
            val tags = new File(refs, "tags")
            val remotes = new File(refs, "remotes")
            // replace?
            def traverse(prefix: String, items: Seq[(String, File)]) = {
                items foreach { i =>
                    val lines = Source.fromFile(i._2).getLines
                    if (lines.hasNext) {
                        val line = lines.next
                        println(i._1, line)
                        line match {
                            case SpaceSplittedString("ref:", symref) => // TODO
                            case GitSHA1(id) => list +:= (i._1, id)
                        }
                    } else {
                        // should not be here!
                    }
                }
            }
            traverse("refs/heads", heads.allsubs("refs/"))
            traverse("refs/tags", tags.allsubs("refs/"))
            traverse("refs/remotes", remotes.allsubs("refs/"))
        }
        if (packed.exists) {
            val text = Source.fromFile(packed)
            text.getLines foreach {
                case SpaceSplittedString(GitSHA1(id), name) =>
                    list +:= (name, id)
                case _ => // ignore
            }
        }

        list.toMap
    }
}
