package com.giyeok.gitexplorer.ui

import java.util.ArrayList

import org.eclipse.draw2d.ChopboxAnchor
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.FigureCanvas
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.Label
import org.eclipse.draw2d.LineBorder
import org.eclipse.draw2d.MouseEvent
import org.eclipse.draw2d.MouseListener
import org.eclipse.draw2d.Panel
import org.eclipse.draw2d.PolylineConnection
import org.eclipse.draw2d.PolylineDecoration
import org.eclipse.draw2d.XYLayout
import org.eclipse.draw2d.geometry.PointList
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.graph.DirectedGraph
import org.eclipse.draw2d.graph.DirectedGraphLayout
import org.eclipse.draw2d.graph.Edge
import org.eclipse.draw2d.graph.EdgeList
import org.eclipse.draw2d.graph.Node
import org.eclipse.draw2d.graph.NodeList
import org.eclipse.swt.SWT
import org.eclipse.swt.events.PaintEvent
import org.eclipse.swt.events.PaintListener
import org.eclipse.swt.graphics.Font
import org.eclipse.swt.widgets.Shell

import com.giyeok.gitexplorer.Util.ToContentString
import com.giyeok.gitexplorer.model.GitRepository

class RepoGraph(val shell: Shell, val repo: GitRepository) {
    val canvas = new FigureCanvas(shell)

    val contents = new Panel
    val canvaslayout = new XYLayout
    contents.setLayoutManager(canvaslayout)

    val graph = new DirectedGraph
    graph.nodes = new NodeList
    graph.edges = new EdgeList

    var commitClickListener = List[(repo.GitCommit) => Unit]()

    private var highlighted = Option.empty[IFigure]
    val highlightedBkgColor = ColorConstants.green

    val commitFont = new Font(null, "Courier New", 20, SWT.NONE)
    val commitBkgColor = ColorConstants.white
    val commitBorder = new LineBorder(ColorConstants.black, 1)
    val commitNodes = (repo.allCommits map { commit =>
        val fig = new Label(commit.id.string.substring(0, 8))
        fig.setFont(commitFont)
        fig.setOpaque(true)
        fig.setBackgroundColor(commitBkgColor)
        fig.setBorder(commitBorder)
        fig.setToolTip(new Label(commit.message.toContent.trim))
        fig.addMouseListener(new MouseListener {
            def mousePressed(e: MouseEvent) = {
                if (highlighted.isDefined) highlighted.get.setBackgroundColor(commitBkgColor)
                fig.setBackgroundColor(highlightedBkgColor)
                highlighted = Some(fig)

                commitClickListener foreach { _(commit) }
            }
            def mouseReleased(e: MouseEvent) = {}
            def mouseDoubleClicked(e: MouseEvent) = {}
        })
        val node = new Node(fig)
        (commit.id, (fig, node))
    }).toMap

    val commitEdges = (repo.allCommits flatMap { commit =>
        def edgeOf(child: repo.GitId, parent: repo.GitId): Edge = {
            val (fr, to) = (commitNodes(child), commitNodes(parent))
            val newconn = new PolylineConnection()
            newconn.setSourceAnchor(new ChopboxAnchor(fr._1))
            newconn.setTargetAnchor(new ChopboxAnchor(to._1))
            newconn.setLineDash(List(5.0f, 3.0f).toArray)
            newconn.setForegroundColor(ColorConstants.red)
            newconn.setLineWidth(2)
            newconn.setTargetDecoration({
                val pl = new PointList()
                List((-2, 2), (0, 0), (-2, -2)) foreach { p => pl.addPoint(p._1, p._2) }

                val arrow = new PolylineDecoration()
                arrow.setTemplate(pl)
                arrow
            })
            new Edge(newconn, fr._2, to._2)
        }
        commit.parents map { edgeOf(commit.id, _) }
    })

    canvas.addPaintListener(new PaintListener {
        def paintControl(e: PaintEvent) = {
            import scala.collection.JavaConversions._
            val graphnodes = graph.nodes.asInstanceOf[ArrayList[Any]]
            commitNodes foreach {
                case (_, (fig, node)) =>
                    contents.add(fig)
                    val size = fig.getPreferredSize()
                    node.width = size.width
                    node.height = size.height
                    graphnodes.add(node)
            }
            val graphedges = graph.edges.asInstanceOf[ArrayList[Any]]
            commitEdges foreach { edge =>
                contents.add(edge.data.asInstanceOf[IFigure])
                graphedges.add(edge)
            }

            val graphlayout = new DirectedGraphLayout
            graphlayout.visit(graph)

            commitNodes.values foreach { figNode =>
                val (fig, node) = figNode
                canvaslayout.setConstraint(fig, new Rectangle(node.x, node.y, node.width, node.height))
            }

            canvas.removePaintListener(this)
        }
    })

    canvas.setContents(contents)
}
