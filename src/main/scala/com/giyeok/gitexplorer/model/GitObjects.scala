package com.giyeok.gitexplorer.model

import com.giyeok.gitexplorer.Util._

trait GitObjects {
    this: GitRepository =>

    case class GitUser(name: String, email: String, date: String)
    object GitUser {
        def fromString(spec: String) = {
            val (lt, gt) = (spec.indexOf('<'), spec.lastIndexOf('>'))
            GitUser(spec.substring(0, lt).trim, spec.substring(lt + 1, gt).trim, spec.substring(gt + 1).trim)
        }
    }
    object SpaceSplittedString {
        def unapply(string: String): Option[(String, String)] = {
            val i = string.indexOf(' ')
            if (i >= 0) Some(string.substring(0, i), string.substring(i + 1)) else None
        }
    }
    class LineIterator(content: Array[Byte]) extends Iterator[String] {
        self =>

        private var _pointer = 0
        private var _last = ""
        private var lastConsumed = true

        def pointer = _pointer
        override def hasNext = _pointer < content.length
        def last = _last
        override def next() = {
            // TODO improve performance
            if (!lastConsumed) {
                lastConsumed = true
                last
            } else {
                val line = content drop pointer takeWhile (_ != '\n') map { _.toChar }
                _pointer += line.length + 1
                _last = new String(line)
                last
            }
        }

        def process[A](block: String => (Boolean, A)): A = {
            val (consumed, result) = block(next)
            lastConsumed = consumed
            result
        }
        def processWhile[A](block: String => Option[A]): List[A] = {
            var result = List[A]()
            var continue = true
            while (continue) {
                block(next) match {
                    case Some(x) => result +:= x
                    case _ => continue = false
                }
            }
            lastConsumed = false
            result.reverse
        }
    }

    trait GitVirtualObject {
        val id: GitId

        lazy val content: Array[Byte] = actualContent

        protected def actualContent: Array[Byte]

        // TODO make it abstract and implement it on inherited classes
        def verify: Boolean = true
    }

    sealed abstract class GitObject extends GitVirtualObject {
        val objectType: GitObject.Types
    }
    object GitObject {
        type Types = Types.Value
        object Types extends Enumeration {
            val BLOB, TREE, COMMIT, TAG = Value
        }

        def fromTypes(id: GitId, objType: Types, _actualContent: () => Array[Byte]) = {
            import Types._
            objType match {
                case BLOB => new GitBlob(id) { def actualContent = _actualContent() }
                case TREE => new GitTree(id) { def actualContent = _actualContent() }
                case COMMIT => new GitCommit(id) { def actualContent = _actualContent() }
                case TAG => new GitTag(id) { def actualContent = _actualContent() }
            }
        }
    }

    abstract class GitTree(val id: GitId) extends GitObject {
        val objectType = GitObject.Types.TREE

        case class TreeEntry(octalMode: String, name: String, objId: GitId) {
            def this(title: String, objId: GitId) = this(title.substring(0, title.indexOf(' ')), title.substring(title.indexOf(' ') + 1), objId)
        }

        lazy val entries: List[TreeEntry] = {
            val reader = new Object {
                var pointer = 0
                def hasNext = pointer < content.length
                def nextTitle = {
                    val line = content drop pointer takeWhile (_ != '\0') map { _.toChar }
                    pointer += line.length + 1
                    new String(line)
                }
                def nextSHA1 = {
                    val sha1 = content slice (pointer, pointer + 20)
                    pointer += 20
                    GitSHA1(sha1)
                }
            }
            var entries = List[TreeEntry]()
            while (reader hasNext) {
                val title = reader.nextTitle
                val sha1 = reader.nextSHA1
                entries +:= new TreeEntry(title, sha1)
            }
            entries.reverse
        }
    }
    abstract class GitBlob(val id: GitId) extends GitObject {
        val objectType = GitObject.Types.BLOB
    }
    abstract class GitCommit(val id: GitId) extends GitObject {
        val objectType = GitObject.Types.COMMIT

        // println(s"=========== commit $id =============")
        // println(new String(content map { _.toChar }))
        // println(tree, parents, author, committer, new String(content drop messageFrom map { _.toChar }))

        lazy val (tree: GitId, parents: List[GitId], author: Option[GitUser], committer: Option[GitUser], messageFrom: Int) = retrieveInfo

        private def retrieveInfo = {
            val lines = new LineIterator(content)

            val treePattern = "^tree ([0-9a-f]{40})$".r
            val tree = lines.next match {
                case treePattern(treeId) => GitSHA1(treeId)
                case _ => throw InvalidFormat("Invalid commit content")
            }

            val parentPattern = "^parent ([0-9a-f]{40})$".r
            val parents = (lines processWhile {
                case parentPattern(parentId) => Some(GitSHA1(parentId))
                case _ => None
            }).toList

            val author = lines.process {
                case SpaceSplittedString("author", author) => (true, Some(GitUser.fromString(author)))
                case _ => (false, None)
            }

            val committer = lines.process {
                case SpaceSplittedString("committer", committer) => (true, Some(GitUser.fromString(committer)))
                case _ => (false, None)
            }

            (tree, parents, author, committer, lines.pointer)
        }

        override def verify = {
            try {
                retrieveInfo
                true
            } catch {
                case _: Throwable => false
            }
        }
    }
    abstract class GitTag(val id: GitId) extends GitObject {
        val objectType = GitObject.Types.TAG

        // println(s"========== tag $id ============")
        // println(new String(content map { _.toChar }))
        // println(objId, objType, tagName, tagger, new String(content drop messageFrom map { _.toChar }))

        lazy val (objId: GitId, objType: String, tagName: String, tagger: Option[GitUser], messageFrom: Int) = retrieveInfo

        private def retrieveInfo = {
            val lines = new LineIterator(content)

            val objectPattern = "^object ([0-9a-f]{40})$".r
            val objId = lines.next match {
                case objectPattern(objId) => GitSHA1(objId)
                case _ => throw InvalidFormat("Invalid tag content - object?")
            }

            val objType = lines.next match {
                case SpaceSplittedString("type", objType) => objType
                case _ => throw InvalidFormat("Invalid tag content - type?")
            }

            val tagName = lines.next match {
                case SpaceSplittedString("tag", tagName) => tagName
                case _ => throw InvalidFormat("Invalid tag content - name?")
            }

            val tagger = lines.process {
                case SpaceSplittedString("tagger", tagger) => (true, Some(GitUser.fromString(tagger)))
                case _ => (false, None)
            }

            (objId, objType, tagName, tagger, lines.pointer)
        }
    }
}
