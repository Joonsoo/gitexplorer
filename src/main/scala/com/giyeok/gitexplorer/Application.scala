package com.giyeok.gitexplorer

import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import com.giyeok.gitexplorer.model.GitRepository
import com.giyeok.gitexplorer.Util._

class Application(path: String) extends RepositoryPrinter {

    val display = new Display()
    val shell = new Shell(display)
    shell.setText("Git Explorer: " + path)

    val repo = new GitRepository(path)

    val allObjects = repo.allObjects

    def start() = {
        shell.open()
        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) display.sleep()
        }
        display.dispose()
    }
}
