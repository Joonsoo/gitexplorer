package com.giyeok.commons.ui.widgets

import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Canvas
import org.eclipse.swt.widgets.Control
import scala.collection.immutable.Stack
import org.eclipse.swt.widgets.Layout
import org.eclipse.swt.layout.FillLayout

class SimpleFancyStackView(parent: Composite, style: Int) extends Composite(parent, style) {

    def clear(dispose: Boolean) = {
        children foreach { child =>
            if (dispose) child.dispose()
        }
        children = Stack.empty
        layout(true)
    }

    var children = Stack[Control]()

    def push(view: Control) = {
        children = children push view
        layout(true)
    }

    def popTo(view: Control) = {
        while (children.top ne view) {
            children.top.dispose()
            children = children.pop
        }
        layout(true)
    }

    {
        // TODO change this FillLayout to fancy animated one
        val layout = new FillLayout
        layout.`type` = SWT.VERTICAL

        setLayout(layout)
    }
}
