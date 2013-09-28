package com.giyeok.commons.ui.widgets

import org.eclipse.swt.events.PaintEvent
import org.eclipse.swt.events.PaintListener
import org.eclipse.swt.graphics.GC
import org.eclipse.swt.widgets.Canvas
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Font
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.layout.RowLayout
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.events.MouseListener
import org.eclipse.swt.events.MouseEvent

/**
 * "Simple" means a list item is a string
 * "Fancy" means it is animated
 */
class SimpleFancyList(parent: Composite, style: Int, defaultData: SimpleFancyList.Root)
    extends Canvas(parent, style | SWT.DOUBLE_BUFFERED) {

    import SimpleFancyList._

    private abstract class ItemLocator {
        val item: NonRoot

        def left: Int
        def top: Int
        def clippingTop: Int
        def clippingBottom: Int
        def bottom = top + item.height

        def isVisible: Boolean = (top + item.height >= clippingTop) && (top < clippingBottom) && (top + item.height >= scrollTop)
        def isVisible(widgetHeight: Int): Boolean = isVisible && (top - scrollTop < widgetHeight)

        def contains(x: Int, y: Int) =
            (clippingTop < y) && (top < y) && (y < top + item.height)

        def draw(gc: GC, widgetWidth: Int, widgetHeight: Int) = {
            if (isVisible(widgetHeight)) {
                gc.setClipping(0, clippingTop, widgetWidth, clippingBottom - clippingTop)
                item match {
                    case Category(_, title, _, _) =>
                        gc.setFont(parentFont)
                        gc.drawText(title, left, top, true)
                    case Leaf(_, name) =>
                        gc.setFont(leafFont)
                        gc.drawText(name, left, top, true)
                }
            }
        }
    }
    private case class ItemLocation(item: NonRoot, left: Int, top: Int, clippingTop: Int, clippingBottom: Int) extends ItemLocator {
        def this(item: NonRoot, left: Int, top: Int) = this(item, left, top, top, item.height)
        def withLeft(newLeft: Int) = ItemLocation(item, newLeft, top, clippingTop, clippingBottom)
        def mutable = new MutableItemLocator(item, left, top, clippingTop, clippingBottom)
    }
    private class MutableItemLocator(val item: NonRoot, var _left: Int, var _top: Int, var _clippingTop: Int, var _clippingBottom: Int) extends ItemLocator {
        def left = _left
        def top = _top
        def clippingTop = _clippingTop
        def clippingBottom = _clippingBottom

        override def toString() = super.toString + s" ($left, $top, $clippingTop, $clippingBottom)"
    }

    private implicit class LocatableItem(item: Item) {
        def height = item match {
            case _: Parent => parentHeight
            case _: Leaf => leafHeight
        }
        def font = item match {
            case _: Parent => parentFont
            case _: Leaf => leafFont
        }
    }

    private class ListStatus(root: Root) {
        val items = root.children
        var expanded = {
            def traverse(item: Item): List[String] = item match {
                case Root(children) => children flatMap traverse
                case Category(id, _, children, (expanded, _)) =>
                    if (expanded) id +: (children flatMap traverse) else (children flatMap traverse)
                case Leaf(id, _) => List()
            }
            (root.children flatMap traverse).toSet
        }
        var selected = Set[String]()

        def isExpanded(id: String) = expanded contains id
        def expand(id: String) = expanded += id
        def collapse(id: String) = expanded -= id
        def toggle(id: String) = if (isExpanded(id)) collapse(id) else expand(id)
    }

    private var scrollTop = 0

    private var parentHeight = 20
    private var parentIndent = 15
    private var parentFont = new Font(null, "", 15, SWT.NONE)
    private var leafHeight = 20
    private var leafFont = new Font(null, "", 15, SWT.NONE)

    private var currentData: Root = defaultData
    private val currentStatus = new ListStatus(defaultData)

    private var leftAnimationTime = 0
    private var parents: Map[String, String] = makeParentMap(defaultData)
    private var finalLocations: Map[String, ItemLocation] = calculateLocations(currentData, currentStatus)
    private var destLocations: Map[String, ItemLocation] = finalLocations
    private val currentLocations =
        scala.collection.mutable.Map[String, MutableItemLocator]((finalLocations.toList map { p => (p._1 -> p._2.mutable) }): _*)

    private def makeParentMap(item: Item): Map[String, String] = item match {
        case Root(children) => (children.foldLeft(Map[String, String]())((map, item) => map ++ makeParentMap(item)))
        case Category(id, _, children, _) =>
            Map((children map { _.id -> id }): _*) ++ (children.foldLeft(Map[String, String]())((map, item) => map ++ makeParentMap(item)))
        case _: Leaf => Map()
    }

    // returns (height, map of item -> locations)
    private def calculateChildrenLocations(children: Seq[NonRoot], status: ListStatus, left: Int, top: Int): (Int, Map[String, ItemLocation]) = {
        children.foldLeft((0, Map[String, ItemLocation]())) { (topLocations, child) =>
            val (nextTop, locations) = calculateLocations(child, status, left, top + topLocations._1)
            (topLocations._1 + nextTop, topLocations._2 ++ locations)
        }
    }
    private def calculateLocations(item: Item, status: ListStatus, left: Int, top: Int): (Int, Map[String, ItemLocation]) = {
        item match {
            case Root(children) =>
                calculateChildrenLocations(children, status, left, top)
            case item @ Category(id, _, children, _) =>
                if (status.isExpanded(id)) {
                    val childrenLocations = calculateChildrenLocations(children, status, left + parentIndent, top + parentHeight)
                    (childrenLocations._1 + parentHeight, childrenLocations._2 + (id -> ItemLocation(item, left, top, top, top + parentHeight)))
                } else {
                    (parentHeight, Map(id -> ItemLocation(item, left, top, top, top + parentHeight)))
                }
            case item @ Leaf(id, _) =>
                (leafHeight, Map(id -> ItemLocation(item, left, top, top, top + leafHeight)))
        }
    }
    private def calculateLocations(item: Root, status: ListStatus): Map[String, ItemLocation] = {
        calculateLocations(item, status, 0, 0)._2
    }

    addPaintListener(new PaintListener {
        def paintControl(event: PaintEvent) = {
            val gc = event.gc
            val bounds = getBounds()
            val (widgetWidth, widgetHeight) = (bounds.width, bounds.height)

            gc.setClipping(0, 0, widgetWidth, widgetHeight)
            // TODO draw something (background, etc.)

            currentLocations.values foreach { _.draw(gc, widgetWidth, widgetHeight) }
        }
    })

    addMouseListener(new MouseListener {
        def mouseUp(e: MouseEvent) = ()
        def mouseDoubleClick(e: MouseEvent) = ()
        def mouseDown(e: MouseEvent) = {
            (currentLocations find { _._2.contains(e.x, e.y) }) match {
                case Some((id, item)) if finalLocations contains id =>
                    item.item match {
                        case Category(_, _, children, (_, collapsable)) if collapsable =>
                            println(id)
                            val bottom = finalLocations(id).bottom
                            def calculateCollapsedChildrenLocationsOf(id: String) = {
                                val (childrenHeight, childrenLocations) = calculateChildrenLocations(children, currentStatus, finalLocations(id).left + parentIndent, 0)
                                childrenLocations mapValues { location =>
                                    ItemLocation(location.item, location.left, bottom - childrenHeight + location.top, bottom, bottom)
                                }
                            }
                            if (currentStatus.isExpanded(id)) {
                                currentStatus.collapse(id)
                                finalLocations = calculateLocations(currentData, currentStatus)
                                destLocations = finalLocations ++ calculateCollapsedChildrenLocationsOf(id)
                                val hidings = currentLocations.keySet -- destLocations.keySet
                                hidings foreach { id =>
                                    val curloc = currentLocations(id)
                                    if (curloc.isVisible) {
                                        def findShowingAncestorLocation(id: String, depth: Int = 0): (Int, ItemLocation) =
                                            (finalLocations get id) match {
                                                case Some(loc) => (depth, loc)
                                                case None => findShowingAncestorLocation(parents(id), depth + 1)
                                            }
                                        val (dep, anc) = findShowingAncestorLocation(id)
                                        val finloc = ItemLocation(curloc.item, anc.left + parentIndent * dep, anc.top, anc.bottom, anc.clippingBottom)
                                        destLocations += (id -> finloc)
                                    }
                                }
                            } else {
                                currentStatus.expand(id)
                                finalLocations = calculateLocations(currentData, currentStatus)
                                val itemLocation = finalLocations(id)
                                val afterItems =
                                    for ((id, l) <- finalLocations if l.top > itemLocation.top)
                                        yield (id, ItemLocation(l.item, l.left, l.top, bottom, l.clippingBottom))
                                for ((id, l) <- afterItems) {
                                    currentLocations get id match {
                                        case Some(loc) =>
                                            loc._clippingTop = bottom
                                            loc._clippingBottom = l.clippingBottom
                                        case _ =>
                                    }
                                }
                                val newCollapsedItems = calculateCollapsedChildrenLocationsOf(id) filter { p =>
                                    (currentLocations get p._1) match {
                                        case Some(l) if l.isVisible => false
                                        case _ => true
                                    }
                                }
                                currentLocations ++= newCollapsedItems mapValues { _.mutable }
                                destLocations = finalLocations ++ afterItems
                            }
                            Animation.start()
                        case _ => // ignore
                    }
                case _ => // ignore
            }
        }
    })

    def changeData(newData: Root, animationDuration: Int = 0) = {
        finalLocations = calculateLocations(newData, currentStatus)
        finalLocations foreach { p =>
            if (!(currentLocations contains p._1))
                currentLocations(p._1) = p._2.mutable
        }
        Animation.start()
    }

    object Animation extends Runnable {
        // NOTE 30 is an HARD CODED arbitrary number to iterate animation
        val iterationTime = 30
        val defaultDuration = 200
        var leftDuration: Int = 0

        def iterate(): Boolean = {
            if (leftDuration < iterationTime || leftDuration < 1) {
                leftDuration = 0
                val hidings = currentLocations.keySet -- finalLocations.keySet
                println(finalLocations.keySet)
                currentLocations --= hidings
                finalLocations foreach { pair =>
                    val (id, finloc) = pair
                    currentLocations get id match {
                        case Some(loc) =>
                            loc._left = finloc.left
                            loc._top = finloc.top
                            loc._clippingTop = finloc.clippingTop
                            loc._clippingBottom = finloc.clippingBottom
                        case None =>
                            currentLocations += (id -> finloc.mutable)
                    }
                }
                if (!isDisposed()) redraw()
                false
            } else {
                def interpolation(cur: Int, fin: Int) = cur + (((fin - cur) * iterationTime) / leftDuration)
                // val showings = finalLocations.keySet -- currentLocations.keySet
                val movings = currentLocations.keySet & destLocations.keySet
                movings foreach { id =>
                    val (cur, fin) = (currentLocations(id), destLocations(id))
                    cur._left = interpolation(cur._left, fin.left)
                    cur._top = interpolation(cur._top, fin.top)
                    cur._clippingTop = fin.clippingTop
                    cur._clippingBottom = interpolation(cur._clippingBottom, fin.clippingBottom)
                }
                leftDuration -= iterationTime
                if (!isDisposed()) redraw()
                true
            }
        }

        def run() = {
            if (iterate()) {
                getDisplay().timerExec(iterationTime, Animation)
            }
        }

        def start(): Unit = {
            start(defaultDuration)
        }
        def start(duration: Int): Unit = {
            leftDuration = duration
            getDisplay().timerExec(iterationTime, Animation)
        }
    }
}
object SimpleFancyList {
    abstract sealed class Item
    sealed trait Parent extends Item { val children: List[NonRoot] }
    sealed trait NonRoot extends Item { val id: String }
    case class Root(children: List[NonRoot]) extends Item with Parent
    case class Category(id: String, title: String, children: List[NonRoot], expandedCollapsable: (Boolean, Boolean) = (true, true))
        extends Item with Parent with NonRoot
    case class Leaf(id: String, name: String) extends Item with NonRoot

    def main(args: Array[String]): Unit = {
        val display = new Display()
        val shell = new Shell(display)
        shell.setText("SimpleFancyList test")

        val sidebar = {
            // import SimpleFancyList._
            val items = List(
                Leaf("HEAD", "HEAD"),
                Leaf("index", "index"),
                Category("~branches", "branches", List(
                    Leaf("master", "master"),
                    Leaf("develop", "develop"))),
                Category("~refs", "refs", List(
                    Category("refs/remote", "remote", List(
                        Leaf("refs/remote/origin", "origin"))))),
                Leaf("last", "last"))
            new SimpleFancyList(shell, SWT.NONE, Root(items))
        }

        shell.setLayout(new FillLayout())
        shell.open()
        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) display.sleep()
        }
        display.dispose()
    }
}
