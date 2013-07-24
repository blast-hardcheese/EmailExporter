package se.hardchee.MailConverter

import javax.mail.Session
import java.util.Properties
import javax.mail.internet.MimeMessage
import java.io.ByteArrayInputStream

object app {
    def readMessage(path: String): MimeMessage = {
        // from http://stackoverflow.com/questions/3444660/java-email-message-parser
        val content = io.Source.fromFile(new java.io.File(path)).mkString
        val s = Session.getDefaultInstance(new Properties())
        val is = new ByteArrayInputStream(content.getBytes())
        val message = new MimeMessage(s, is)
        message
    }

    def main(args: Array[String]) {
        val messages = args.toList.map { readMessage }
        println(messages)
    }
}

// vim: set ts=4 sw=4 et:
