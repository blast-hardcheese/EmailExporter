package test.scala

import se.hardchee.MailConverter

object AppTester {
    def main(_args: Array[String]) {
        val args = if(_args.isEmpty) { Array("/tmp/mail/out-%s.txt", "sample/99.", "sample/95.", "sample/foofoofoo") } else { _args }
        MailConverter.app.main(args)
    }
}

// vim: set ts=4 sw=4 et:
