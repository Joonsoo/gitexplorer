package com.giyeok.gitexplorer

import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell

import com.giyeok.gitexplorer.model.GitRepository

class Application(path: String) {

    val display = new Display()
    val shell = new Shell(display)
    shell.setText("Git Explorer: " + path)

    val repo = GitRepository.loadFrom(path)

    def start() = {
        shell.open()
        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) display.sleep()
        }
        display.dispose()
    }
}
