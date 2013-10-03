package com.giyeok.gitexplorer.ui

import org.eclipse.swt.widgets.Canvas
import org.eclipse.swt.widgets.Composite
import com.giyeok.gitexplorer.model.GitRepository
import org.eclipse.swt.graphics.GC
import org.eclipse.swt.graphics.Rectangle
import org.eclipse.swt.events.PaintListener
import org.eclipse.swt.events.PaintEvent
import org.eclipse.swt.graphics.Font
import org.eclipse.swt.SWT
import com.giyeok.gitexplorer.Util._
import com.giyeok.commons.ui.widgets.SimpleFancyStackView
import org.eclipse.swt.events.MouseListener
import org.eclipse.swt.events.MouseEvent

trait GitObjectViews {
    this: GitRepository =>

    object GitObjectView {
        def apply(obj: GitObject, parent: SimpleFancyStackView, style: Int = SWT.NONE): GitObjectView = obj match {
            case tag: GitTag => new GitTagView(tag, parent, style)
            case commit: GitCommit => new GitCommitView(commit, parent, style)
            case tree: GitTree => new GitTreeView(tree, parent, style)
            case blob: GitBlob => new GitBlobView(blob, parent, style)
        }

        def push(objId: GitId, stackView: SimpleFancyStackView) = getObject(objId) match {
            case Some(obj) => stackView.push(GitObjectView(obj, stackView))
            case _ => stackView.push(new UnknownObject(objId, stackView, SWT.NONE))
        }

        val titleFont = new Font(null, "Courier New", 12, SWT.NONE)
        val titleHeight = 20

        val contentFont = new Font(null, "", 10, SWT.NONE)
        val codeFont = new Font(null, "Courier New", 10, SWT.NONE)
    }

    abstract class GitObjectView(parent: SimpleFancyStackView, style: Int) extends Canvas(parent, style) {
        val obj: GitObject
        def draw(g: GC, bounds: Rectangle): Unit

        addPaintListener(new PaintListener {
            def paintControl(e: PaintEvent) = {
                val g = e.gc

                g.setFont(GitObjectView.titleFont)
                g.drawString(s"${obj.objectType} ${obj.id}", 0, 0)

                val bounds = getBounds()
                if (bounds.height > GitObjectView.titleHeight) {
                    draw(g, new Rectangle(0, GitObjectView.titleHeight, bounds.width, bounds.height - GitObjectView.titleHeight))
                }
            }
        })
    }

    trait AreaClickListener extends GitObjectView {
        protected var listeners = Map[String, (Rectangle, (MouseEvent) => Unit)]()

        def addListener(id: String, area: Rectangle, listener: (MouseEvent) => Unit) = {
            listeners += (id -> (area, listener))
        }
        addMouseListener(new MouseListener {
            def mouseDoubleClick(e: MouseEvent): Unit = {}
            def mouseDown(e: MouseEvent): Unit = {
                listeners.values foreach { l =>
                    if (l._1.contains(e.x, e.y)) l._2(e)
                }
            }
            def mouseUp(e: MouseEvent): Unit = {}
        })
    }
    class ContinuousTextDrawer(g: GC, x: Int, var y: Int) {
        def drawText(text: String) = {
            val extent = g.textExtent(text)
            g.drawText(text, x, y)
            val rect = new Rectangle(x, y, extent.x, extent.y)
            y += extent.y
            rect
        }
    }
    class GitTagView(val tag: GitTag, parent: SimpleFancyStackView, style: Int) extends GitObjectView(parent, style) {
        val obj = tag
        def draw(g: GC, bounds: Rectangle) = {
            // TODO
        }
    }
    class GitCommitView(val commit: GitCommit, parent: SimpleFancyStackView, style: Int)
        extends GitObjectView(parent, style) with AreaClickListener {
        val obj = commit
        def draw(g: GC, bounds: Rectangle) = {
            var d = new ContinuousTextDrawer(g, bounds.x, bounds.y)

            g.setFont(GitObjectView.codeFont)
            commit.parents foreach { parent => d.drawText(s"Parent $parent") }
            commit.author match {
                case Some(author) => d.drawText(s"Author: $author")
                case _ =>
            }

            g.setFont(GitObjectView.contentFont)
            d.drawText(commit.message.toContent.trim)

            g.setFont(GitObjectView.codeFont)
            addListener("tree", d.drawText(s"tree ${commit.tree}"), (e) => {
                parent.popTo(this)
                GitObjectView.push(commit.tree, parent)
            })
        }
    }
    class GitTreeView(val tree: GitTree, parent: SimpleFancyStackView, style: Int)
        extends GitObjectView(parent, style) with AreaClickListener {
        val obj = tree
        def draw(g: GC, bounds: Rectangle) = {
            g.setFont(GitObjectView.codeFont)

            var d = new ContinuousTextDrawer(g, bounds.x, bounds.y)
            tree.entries foreach { entry =>
                val entryId = entry.objId.string
                addListener(entryId, d.drawText(s"${entryId.substring(0, 8)} ${entry.octalMode} ${entry.name}"), (e) => {
                    parent.popTo(this)
                    GitObjectView.push(entry.objId, parent)
                })
            }
        }
    }
    class GitBlobView(val blob: GitBlob, parent: SimpleFancyStackView, style: Int) extends GitObjectView(parent, style) {
        val obj = blob
        def draw(g: GC, bounds: Rectangle) = {
            g.setFont(GitObjectView.codeFont)
            g.drawText(blob.content.toContent, bounds.x, bounds.y)
        }
    }
    class UnknownObject(val id: GitId, parent: SimpleFancyStackView, style: Int) extends Canvas(parent, style) {
        addPaintListener(new PaintListener {
            def paintControl(e: PaintEvent) = {
                val g = e.gc

                g.setFont(GitObjectView.titleFont)
                g.drawString(s"UNKNOWN $id", 0, 0)

                g.setFont(GitObjectView.contentFont)
                g.drawString("Unknown object $id", 0, GitObjectView.titleHeight)
            }
        })
    }
}
