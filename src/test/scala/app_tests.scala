package test.scala

import se.hardchee.MailConverter

object AppTester {
    def main(_args: Array[String]) {
        val args = if(_args.isEmpty) { Array("sample/95.", "sample/99.") } else { _args }
        MailConverter.app.main(args)
    }
}

// vim: set ts=4 sw=4 et:
