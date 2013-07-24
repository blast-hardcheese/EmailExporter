package se.hardchee.MailConverter

import javax.mail.Session
import java.util.Properties
import javax.mail.internet.MimeMessage
import java.io.ByteArrayInputStream

object app {
    def readRaw(path: String): String = {
        val content = io.Source.fromFile(new java.io.File(path)).mkString
        content
    }

    def readMessage(content: String): MimeMessage = {
        // from http://stackoverflow.com/questions/3444660/java-email-message-parser
        val s = Session.getDefaultInstance(new Properties())
        val is = new ByteArrayInputStream(content.getBytes())
        val message = new MimeMessage(s, is)
        message
    }

    def generateFrom(message: MimeMessage) = {
        "foo"
    }

    def main(args: Array[String]) {
        args.toList.map { readRaw }.map { content => generateFrom(readMessage(content)) } map { println }
    }
}

// vim: set ts=4 sw=4 et:
