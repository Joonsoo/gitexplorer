package com.giyeok.gitexplorer

object Main {
    def main(args: Array[String]) = {
    	val app = new Application("./.git")
    	
    	app.start()
    }
}
