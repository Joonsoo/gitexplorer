package com.giyeok.gitexplorer

import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import com.giyeok.commons.ui.widgets.SimpleFancyList
import com.giyeok.commons.ui.widgets.SimpleFancyList.Category
import com.giyeok.commons.ui.widgets.SimpleFancyList.Leaf
import com.giyeok.commons.ui.widgets.SimpleFancyList.Root
import com.giyeok.commons.ui.widgets.SimpleFancyStackView
import com.giyeok.gitexplorer.model.GitRepository
import com.giyeok.gitexplorer.ui.RepoGraph
import com.giyeok.gitexplorer.model.GitSHA1

import com.giyeok.gitexplorer.Util._

class Application(path: String) extends RepositoryPrinter {
    val repo = new GitRepository(path)

    println(s"${repo.allCommits.size} commits")

    def startGUI() = {
        val display = new Display()
        val shell = new Shell(display)
        shell.setText("Git Explorer: " + repo.root.getCanonicalPath())

        val layout = new FillLayout
        layout.`type` = SWT.HORIZONTAL

        shell.setLayout(layout)
        val sidebar = {
            import SimpleFancyList._
            // TODO read HEAD, refs and list them up
            // ... and think about how to show index
            val refsMap = repo.refs.keySet.groupBy {
                case SlashSplittedString("refs", SlashSplittedString(ref, name)) => ref
                case x => x
            }

            val items = List(
                Leaf("HEAD", "HEAD"),
                Leaf("index", "index")) ++
                (refsMap.keySet.toList.sortBy(identity) map { ref =>
                    Category("~" + ref, ref, refsMap(ref).toList map {
                        ref => Leaf(ref, ref) // Leaf(ref, s"$ref ${repo.refs(ref)}") // repo.refs(ref)
                    })
                })
            new SimpleFancyList(shell, SWT.NONE, Root(items))
        }
        val graph = new RepoGraph(shell, repo)
        val detailbar = new SimpleFancyStackView(shell, SWT.NONE)
        graph.objectClickListener = List((id: graph.repo.GitId) => {
            detailbar.clear(true)
            graph.repo.GitObjectView.push(id, detailbar)
        })

        sidebar.leafClickedListener = { id =>
            println(s"leaf $id clicked : ${repo.refs(id)}")
            graph.nodeClicked(repo.refs(id))
        }

        shell.open()
        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) display.sleep()
        }
        display.dispose()
    }
}
