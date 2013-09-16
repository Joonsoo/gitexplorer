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

        def fromTypes(id: GitId, objType: Types, actualContent: () => Array[Byte]) = {
            import Types._
            objType match {
                case BLOB => new GitBlobExisting(id, actualContent)
                case TREE => new GitTreeExisting(id, actualContent)
                case COMMIT => new GitCommitExisting(id, actualContent)
                case TAG => new GitTagExisting(id, actualContent)
            }
        }
    }

    trait GitIdCalculator extends GitObject {
        val objectType: GitObject.Types

        lazy val objectContent: Array[Byte] =
            (objectType.toString.toLowerCase + " " + content.length + "\\0").getBytes() ++ content

        lazy val id = {
            // TODO implement this
            new GitSHA1("")
        }
    }

    abstract class GitBlob extends GitObject {
        val objectType = GitObject.Types.BLOB
    }
    class GitBlobExisting(val id: GitId, _actualContent: () => Array[Byte]) extends GitBlob {
        def this(id: GitId, _actualContent: Array[Byte]) = this(id, () => _actualContent)
        def actualContent = _actualContent()
    }
    class GitBlobNew(_content: Array[Byte]) extends GitBlob with GitIdCalculator {
        def actualContent = _content
    }

    object GitTree {
        case class Entry(octalMode: String, name: String, objId: GitId) {
            def this(title: String, objId: GitId) = this(title.substring(0, title.indexOf(' ')), title.substring(title.indexOf(' ') + 1), objId)
        }
    }
    abstract class GitTree extends GitObject {
        val objectType = GitObject.Types.TREE

        val entries: List[GitTree.Entry]
    }
    class GitTreeExisting(val id: GitId, _actualContent: () => Array[Byte]) extends GitTree {
        def this(id: GitId, _actualContent: Array[Byte]) = this(id, () => _actualContent)
        def actualContent = _actualContent()

        lazy val entries: List[GitTree.Entry] = {
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
            var entries = List[GitTree.Entry]()
            while (reader hasNext) {
                val title = reader.nextTitle
                val sha1 = reader.nextSHA1
                entries +:= new GitTree.Entry(title, sha1)
            }
            entries.reverse
        }
    }
    class GitTreeNew(val entries: List[GitTree.Entry]) extends GitTree with GitIdCalculator {
        def actualContent = {
            // TODO generate from entries
            new Array[Byte](0)
        }
    }

    abstract class GitCommit extends GitObject {
        val objectType = GitObject.Types.COMMIT

        // println(s"=========== commit $id =============")
        // println(new String(content map { _.toChar }))
        // println(tree, parents, author, committer, new String(content drop messageFrom map { _.toChar }))
        val tree: GitId
        val parents: List[GitId]
        val author: Option[GitUser]
        val committer: Option[GitUser]
        val message: Array[Byte]
    }
    class GitCommitExisting(val id: GitId, _actualContent: () => Array[Byte]) extends GitCommit {
        def this(id: GitId, _actualContent: Array[Byte]) = this(id, () => _actualContent)
        def actualContent = _actualContent()

        lazy val (tree: GitId, parents: List[GitId], author: Option[GitUser], committer: Option[GitUser], messageFrom: Int) = retrieveInfo
        lazy val message = content drop messageFrom

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
    class GitCommitNew(val tree: GitId, val parents: List[GitId], val author: Option[GitUser], val committer: Option[GitUser],
        val message: Array[Byte]) extends GitCommit with GitIdCalculator {

        def actualContent = {
            // TODO generate from data
            new Array[Byte](0)
        }
    }

    abstract class GitTag extends GitObject {
        val objectType = GitObject.Types.TAG

        val objId: GitId
        val objType: String
        val tagName: String
        val tagger: Option[GitUser]
        val message: Array[Byte]
    }
    class GitTagExisting(val id: GitId, _actualContent: () => Array[Byte]) extends GitTag {
        def this(id: GitId, _actualContent: Array[Byte]) = this(id, () => _actualContent)
        def actualContent = _actualContent()

        lazy val (objId: GitId, objType: String, tagName: String, tagger: Option[GitUser], messageFrom: Int) = retrieveInfo
        lazy val message = content drop messageFrom

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
    class GitTagNew(val objId: GitId, val objType: String, val tagName: String, val tagger: Option[GitUser],
        val message: Array[Byte]) extends GitTag with GitIdCalculator {

        def actualContent = {
            // TODO generate from data
            new Array[Byte](0)
        }
    }
}
