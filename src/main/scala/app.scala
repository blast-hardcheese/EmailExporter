package se.hardchee.MailConverter

import javax.mail.Session
import java.util.Properties
import javax.mail.internet.MimeMessage
import java.io.ByteArrayInputStream

import scala.util.parsing.combinator.RegexParsers

object parser extends RegexParsers {
    val hostname = "mail.en2go.com"

    val number = rep( "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" )
    val character = ".".r

    val ipv4 = number ~ "." ~ number ~ "." ~ number ~ "." ~ number

    val headerReceived = "from" ~> hostname ~ ( "(" ~> ( ipv4 | "[unix socket]" ) <~ ")" ) ~ "(.|\\s)*$".r ^^ {
        case hostname ~ connection ~ rest => hostname
    }

    val versionIdentifier = rep(character)

    def parseReceived(input: String) = {
        parseAll(headerReceived, input) match {
            case Success(hostname, _) => Some(hostname)
            case failure: NoSuccess => scala.sys.error(failure.msg)
        }
    }
}

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

    def generateFrom(message: MimeMessage) = {
        "foo"
    }

    def prependFrom(raw: String, message: MimeMessage) = {
        generateFrom(message) + "\r\n" + raw
    }

    def main(args: Array[String]) {
        val target :: files = args.toList
        println(files)
        println(target)
        val output = new FileOutputStream(new File(target))
        val messages = files.flatMap { readRaw }.map { content => prependFrom(content, readMessage(content)) }
        output.write(messages.mkString("\r\n\r\n").getBytes)
    }
}

// vim: set ts=4 sw=4 et:
