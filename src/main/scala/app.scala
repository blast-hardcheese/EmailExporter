package se.hardchee.MailConverter

object app {
    def main(args: Array[String]) {
        val outformat :: files = args.toList
        MailHandler.processFilePaths(outformat, files)
    }
}

// vim: set ts=4 sw=4 et:
