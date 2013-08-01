package se.hardchee.MailConverter

import java.io.ByteArrayInputStream
import java.io.File

import org.joda.time.DateTime
import org.joda.time.DateTimeZone

import java.util.Properties
import javax.mail.Session
import javax.mail.internet.MimeMessage
import javax.mail.Message
import javax.mail.internet.MimeMultipart
import javax.mail.internet.MimeBodyPart

object app {
    def readRaw(path: String): Option[String] = {
        val file = new File(path)
        if(file.isFile) {
            Some(io.Source.fromFile(file).mkString)
        } else {
            None
        }
    }

    def readMessage(content: String): MimeMessage = {
        // from http://stackoverflow.com/questions/3444660/java-email-message-parser
        val s = Session.getDefaultInstance(new Properties())
        val is = new ByteArrayInputStream(content.getBytes())
        val message = new MimeMessage(s, is)
        message
    }


    def main(args: Array[String]) {
        val target :: files = args.toList
        val messages = files.flatMap { readRaw }.map { readMessage }
        println(messages.mkString)
    }
}

// vim: set ts=4 sw=4 et:
