package se.hardchee.MailConverter

object app {
    def main(args: Array[String]) {
        args.toList match {
            case outformat :: files => MailHandler.processFilePaths(outformat, files)
            case _ => println("Usage: \"/tmp/out-%s.txt\" \"file1\" \"file2\"")
        }
    }
}

// vim: set ts=4 sw=4 et:
