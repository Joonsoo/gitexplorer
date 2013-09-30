package com.giyeok.gitexplorer.ui

import org.eclipse.draw2d.Label
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Font
import org.eclipse.swt.widgets.Shell
import org.eclipse.zest.core.widgets.Graph
import org.eclipse.zest.core.widgets.GraphConnection
import org.eclipse.zest.core.widgets.GraphNode
import org.eclipse.zest.core.widgets.ZestStyles
import org.eclipse.zest.layouts.LayoutStyles
import org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm

import com.giyeok.gitexplorer.Util.ToContentString
import com.giyeok.gitexplorer.model.GitRepository

class RepoGraph(val shell: Shell, val repo: GitRepository) {
    val graph = new Graph(shell, SWT.NONE)

    val commitFont = new Font(null, "Courier New", 12, SWT.NONE)
    val commitNodes = (repo.allCommits map { commit =>
        val node = new GraphNode(graph, SWT.NONE, commit.id.string.substring(0, 8))
        node.setFont(commitFont)
        node.setTooltip(new Label(commit.message.toContent.trim))
        (commit.id, node)
    }).toMap

    val commitEdges = (repo.allCommits flatMap { commit =>
        def edgeOf(child: repo.GitId, parent: repo.GitId) = {
            new GraphConnection(graph, ZestStyles.CONNECTIONS_DIRECTED, commitNodes(child), commitNodes(parent))
        }
        commit.parents map { edgeOf(commit.id, _) }
    })

    graph.setLayoutAlgorithm(new TreeLayoutAlgorithm(LayoutStyles.NO_LAYOUT_NODE_RESIZING), true)
}
