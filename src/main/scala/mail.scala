package se.hardchee.MailConverter

import java.util.Properties
import javax.mail.Session
import javax.mail.internet.MimeMessage
import java.io.ByteArrayInputStream

object MailCore {
    def handleRawMessage(raw: String) = {
        val message: MimeMessage = readMessage(raw)
        val output: String = MailFormatter.toString(message)
        output
    }

    def readMessage(content: String): MimeMessage = {
        // from http://stackoverflow.com/questions/3444660/java-email-message-parser
        val s = Session.getDefaultInstance(new Properties())
        val is = new ByteArrayInputStream(content.getBytes())
        val message = new MimeMessage(s, is)
        message
    }
}

import java.io.File
import java.io.FileOutputStream

import org.joda.time.DateTime
import org.joda.time.DateTimeZone

object MailHandler {
    def processDirectories(outputFormat: String, directories: List[String]) {
        for(path <- directories) {
            val dir = new File(path)
            if(dir.isDirectory) {
                Option(dir.listFiles).map({ fpaths => processFiles(outputFormat, fpaths.toList) })
            }
        }
    }

    def processFiles(outputFormat: String, files: List[java.io.File]) {
        def stripFilename(path: String) = path.split("/").toList.last

        def readRaw(file: File): Option[String] = {
            if(file.isFile) {
                Some(io.Source.fromFile(file).mkString)
            } else {
                None
            }
        }


        for(file <- files) {
            val fpath = file.getPath
            val outfile = outputFormat.format(stripFilename(fpath))
            println("Output: " + outfile)
            readRaw(file).map { MailCore.handleRawMessage } map { writeOut(outfile, _) }
        }
    }

    def processFilePaths(outputFormat: String, _files: List[String]) {
        val files = _files.map({ new java.io.File(_) })
        processFiles(outputFormat, files)
    }


    def writeOut(outpath: String, message: String) {
        val output = new FileOutputStream(new File(outpath))
        output.write(message.getBytes)
    }
}

object ImplicitFieldFormatters {
    import scala.language.implicitConversions

    val desiredTimezone = DateTimeZone.forID("America/Los_Angeles")

    implicit def makeDate(date: java.util.Date): Option[String] = {
        Option(date).map({
            new DateTime(_)
                .withZone(desiredTimezone)
                .toString("Y/M/d H:m:s (Z)")
        })
    }

    implicit def wrapList(x: Array[_]): Option[List[_]] = {
        Option(x).map { _.toList }
    }

    implicit def wrapString(x: String): Option[String] = Option(x)
}

import javax.mail.internet.MimeBodyPart
import javax.mail.internet.MimeMultipart
import javax.mail.internet.MimeMessage
import javax.mail.BodyPart

object MailUtilities {
    def makeFileName(bp: BodyPart): Option[String] = Option(bp.getFileName()).map({ name => val size = makeFileSize(bp.getSize()); s"$name ($size)" })
    def makeFileSize(_size: Int) = {
        def findFileSize(size: Int) = "KMG".foldLeft[Tuple2[Double, Char]]( (size.toDouble, 'B') ) {
            case ((size, last), next) if(size / 1024 > 1.5) => (size / 1024, next)
            case (last, next) => last
        }

        val (size, suffix) = findFileSize(_size)
        f"$size%.2f $suffix%c"
    }

    def getBodyPart(multipart: MimeMultipart, identifier: String): Option[String] = {
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

    def extractAllBodyParts(mp: MimeMultipart): List[BodyPart] = {
        ((0 until mp.getCount()).flatMap { i => {
            val bp = mp.getBodyPart(i)
            val r = Option(bp.getContent()) match {
                case Some(mp: MimeMultipart) => extractAllBodyParts(mp)
                case _ => List()
            }
            bp +: r
        }
        }).toList
    }

    def extractAttachmentNames(message: MimeMessage): Option[List[String]] = {
        (Option(message.getContent()) match {
            case Some(mp:MimeMultipart) => Some(extractAllBodyParts(mp).flatMap( makeFileName ))
            case _ => None
        }) match {
            case Some(List()) => None // Empty lists shouldn't be displayed.
            case x => x
        }
    }

    def extractBody(message: MimeMessage): String = {
        (Option(message.getContent()) match {
            case Some(mp:MimeMultipart) => MailUtilities.getBodyPart(mp, "text/plain")
            case Some(m) => Some(m.toString)
            case None => None
        }).getOrElse("// This message has no content")
    }
}

import javax.mail.Message

object MailFormatter {
    def toString(message: MimeMessage): String = {
        import se.hardchee.MailConverter.ImplicitFieldFormatters._

        val from = message.getFrom()
        val to = message.getRecipients(Message.RecipientType.TO)
        val cc = message.getRecipients(Message.RecipientType.CC)
        val bcc = message.getRecipients(Message.RecipientType.BCC)
        val date = message.getSentDate()
        val subject = message.getSubject()
        val body = MailUtilities.extractBody(message)
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
}

// vim: set ts=4 sw=4 et:
