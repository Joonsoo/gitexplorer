package com.giyeok.gitexplorer

import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import com.giyeok.gitexplorer.model.GitRepository
import com.giyeok.gitexplorer.Util._
import com.giyeok.commons.widgets.SimpleFancyList
import org.eclipse.swt.SWT

class Application(path: String) extends RepositoryPrinter {
    val repo = new GitRepository(path)
    val allObjects = repo.allObjects

    def startGUI() = {
        val display = new Display()
        val shell = new Shell(display)
        shell.setText("Git Explorer: " + path)

        val sidebar = new SimpleFancyList(shell, SWT.NONE, SimpleFancyList.Root(Nil))

        shell.open()
        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) display.sleep()
        }
        display.dispose()
    }
}
