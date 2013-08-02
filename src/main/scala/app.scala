package se.hardchee.MailConverter

import java.io.ByteArrayInputStream

import java.util.Properties
import javax.mail.Session
import javax.mail.internet.MimeMessage
import javax.mail.Message
import javax.mail.internet.MimeMultipart

object app {
    def readMessage(content: String): MimeMessage = {
        // from http://stackoverflow.com/questions/3444660/java-email-message-parser
        val s = Session.getDefaultInstance(new Properties())
        val is = new ByteArrayInputStream(content.getBytes())
        val message = new MimeMessage(s, is)
        message
    }

    def formatMail(message: MimeMessage): String = {
        import se.hardchee.MailConverter.ImplicitFieldFormatters._

        val from = message.getFrom()
        val to = message.getRecipients(Message.RecipientType.TO)
        val cc = message.getRecipients(Message.RecipientType.CC)
        val bcc = message.getRecipients(Message.RecipientType.BCC)
        val date = message.getSentDate()
        val subject = message.getSubject()
        val body = (Option(message.getContent()) match {
            case Some(mp:MimeMultipart) => MailUtilities.getBodyPart(mp, "text/plain")
            case Some(m) => Some(m.toString)
            case x: Option[_] => x
        }).getOrElse("// This message has no content")
        val attachmentNames: Option[List[String]] = MailUtilities.extractAttachmentNames(message)

        def convertList(label: String, x: Option[List[_]]) = x map { label + ": " + _.mkString(", ") }
        def convert(label: String, x: Option[String]) = x map { label + ": " + _ }

        val headers = List(
            convertList("From", from),
            convertList("To", to),
            convertList("CC", cc),
            convertList("BCC", bcc),
            convert("Date", date),
            convert("Subject", subject),
            convertList("Attachments", attachmentNames)
        ).flatten.mkString("\n") // convert* produce Option[_]'s, we can use this to generate quick and easy optional headers!

        s"""|$headers
            |================================================================================
            |$body
            |""".stripMargin
    }

    def handleRawMessage(raw: String) = {
        val message: MimeMessage = readMessage(raw)
        val output: String = formatMail(message)
        output
    }

    def main(args: Array[String]) {
        val outformat :: files = args.toList
        MailReader.processFilePaths(outformat, files)
    }
}

// vim: set ts=4 sw=4 et:
