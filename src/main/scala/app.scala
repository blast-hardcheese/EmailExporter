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
    val desiredTimezone = DateTimeZone.forID("America/Los_Angeles")

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

    def formatMail(message: MimeMessage) = {
        def makeDate(date: java.util.Date) = {
            Option(date).map({
                new DateTime(_)
                    .withZone(desiredTimezone)
                    .toString("Y/M/d H:m:s (Z)")
            })
        }

        def getBodyPart(multipart: MimeMultipart, identifier: String) = {
            val preamble = Option(multipart.getPreamble())

            (0 until multipart.getCount()).foldLeft[Option[String]](None){ case (last, i: Int) =>
                if(last != None)
                    last
                else {
                    multipart.getBodyPart(i) match {
                        case mbp: MimeBodyPart if( mbp.getContentType().startsWith(identifier)
                                                && mbp.getContent().toString != "") => Some(mbp.getContent().toString)
                        case _ => None
                    }
                }
            }
        }

        def wrapList(x: Array[_]) = {
            Option(x).map { _.toList }
        }

        val from = wrapList(message.getFrom())
        val to = wrapList(message.getRecipients(Message.RecipientType.TO))
        val cc = wrapList(message.getRecipients(Message.RecipientType.CC))
        val bcc = wrapList(message.getRecipients(Message.RecipientType.BCC))
        val date = makeDate(message.getSentDate())
        val subject = Option(message.getSubject())
        val body = (Option(message.getContent()) match {
            case Some(mp:MimeMultipart) => getBodyPart(mp, "text/plain")
            case Some(m) => Some(m.toString)
            case x: Option[_] => x
        }).getOrElse("// This message has no content")

        def convertList(label: String, x: Option[List[_]]) = x map { label + ": " + _.mkString(", ") }
        def convert(label: String, x: Option[String]) = x map { label + ": " + _ }

        val headers = List(
            convertList("From", from),
            convertList("To", to),
            convertList("CC", cc),
            convertList("BCC", bcc),
            convert("Date", date),
            convert("Subject", subject)
        ).flatten.mkString("\n") // convert* produce Option[_]'s, we can use this to generate quick and easy optional headers!

        s"""|$headers
            |================================================================================
            |$body
            |""".stripMargin
    }

    def main(args: Array[String]) {
        val target :: files = args.toList
        val messages = files.flatMap { readRaw }.map { readMessage }.map { formatMail }
        println(messages.mkString)
    }
}

// vim: set ts=4 sw=4 et:
