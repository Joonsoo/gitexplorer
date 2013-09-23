package com.giyeok.gitexplorer

import com.giyeok.gitexplorer.Util._

trait RepositoryPrinter {
    this: Application =>

    def printAllCommits() = {
        for (obj <- allObjects) {
            obj match {
                case (_, commit: repo.GitCommit) =>
                    println(s"commit ${commit.id}")
                    if (commit.author.isDefined) {
                        println(s"Author: ${commit.author.get}")
                        println(s"Date:   ${commit.author.get.date}")
                    }
                    for (parent <- commit.parents) {
                        println(s"Parent: ${parent.string}")
                    }
                    println(commit.message.toContent)
                case _ =>
            }
        }
    }
}
