package se.hardchee.MailConverter

import java.io.File
import java.io.FileOutputStream

object MailReader {
    def processDirectories(outputFormat: String, directories: List[String]) {
        for(path <- directories) {
            val dir = new File(path)
            if(dir.isDirectory) {
                Option(dir.listFiles).map({ fpaths => processFiles(outputFormat, fpaths.toList) })
            }
        }
    }

    def processFiles(outputFormat: String, files: List[java.io.File]) {
        def stripFilename(path: String) = path.split("/").toList.last

        def readRaw(file: File): Option[String] = {
            if(file.isFile) {
                Some(io.Source.fromFile(file).mkString)
            } else {
                None
            }
        }


        for(file <- files) {
            val fpath = file.getPath
            val outfile = outputFormat.format(stripFilename(fpath))
            println("Output: " + outfile)
            readRaw(file).map { app.handleRawMessage } map { writeOut(outfile, _) }
        }
    }

    def processFilePaths(outputFormat: String, _files: List[String]) {
        val files = _files.map({ new java.io.File(_) })
        processFiles(outputFormat, files)
    }


    def writeOut(outpath: String, message: String) {
        val output = new FileOutputStream(new File(outpath))
        output.write(message.getBytes)
    }
}

// vim: set ts=4 sw=4 et:
