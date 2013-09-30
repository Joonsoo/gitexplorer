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
            val items = List(
                Leaf("HEAD", "HEAD"),
                Leaf("index", "index"),
                Category("~branches", "branches", List(
                    Leaf("master", "master"))),
                Category("~remotes", "remotes", List()),
                Category("~tags", "tags", List()))
            new SimpleFancyList(shell, SWT.NONE, Root(items))
        }
        val graph = new RepoGraph(shell, repo)
        val detailbar = new SimpleFancyStackView(shell, SWT.NONE)
        graph.commitClickListener = List((commit: graph.repo.GitCommit) => {
            detailbar.clear(true)
            detailbar.push(graph.repo.GitObjectView(commit, detailbar, SWT.NONE))
        })

        shell.open()
        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) display.sleep()
        }
        display.dispose()
    }
}
