package se.hardchee.MailConverter

import java.util.Properties
import javax.mail.Session
import javax.mail.internet.MimeMessage
import java.io.ByteArrayInputStream

import org.joda.time.DateTime
import javax.mail.internet.InternetAddress
import javax.mail.Message.RecipientType

object MailComparisons {
    import scala.language.implicitConversions
    implicit def foldBoolList(bools: List[Boolean]): Boolean = bools.foldLeft(true) { (last: Boolean, next: Boolean) => last && next }
    implicit def fromOption(opt: Option[Boolean]): Boolean = opt.getOrElse(true)
    def foldBoolListFindTrue(bools: List[Boolean]): Boolean = bools.foldLeft(false) { (last: Boolean, next: Boolean) => last || next }

    def compareAddresses(lookingFor: List[InternetAddress], mailHas: List[InternetAddress], allRequired: Boolean = false): Boolean = {
        lazy val comparisons = lookingFor.map { lhs => foldBoolListFindTrue(mailHas.map { rhs => { lhs.getAddress.toLowerCase == rhs.getAddress.toLowerCase } }) }

        if(!allRequired) foldBoolListFindTrue( comparisons )
        else foldBoolList( comparisons )
    }
}

object MailCore {
    def getRecipientType(message: MimeMessage, rtype: RecipientType) = Option(message.getRecipients(rtype)).map { _.toList } getOrElse(List())

    def filter(message: MimeMessage)(implicit config: Config): Option[MimeMessage] = {
        val sentDate = Option(message.getSentDate) map { new DateTime(_) }
        val senders:List[InternetAddress] = Option(message.getFrom).map({ _.toList.map({ address => new InternetAddress(address.toString) }) }).getOrElse(List())
        val allRecipients: List[InternetAddress] = List(
            getRecipientType(message, RecipientType.TO),
            getRecipientType(message, RecipientType.CC),
            getRecipientType(message, RecipientType.BCC)
        ).flatten map { address => new InternetAddress(address.toString) }

        import MailComparisons._

        val shouldDisplay: Boolean = List[Boolean](
            config.senders.isEmpty || compareAddresses(config.senders, senders),
            config.recipients.isEmpty || compareAddresses(config.recipients, allRecipients, config.allRecipientsRequired),

            (sentDate flatMap { date => (config.untilDate map { untilDate => date.isBefore(untilDate) }) } ),
            (sentDate flatMap { date => (config.fromDate map { fromDate => date.isAfter(fromDate) }) } )
        )

        if(shouldDisplay) Some(message)
        else None
    }

    def handleRawMessage(raw: String)(implicit config: Config): Option[String] = {
        readMessage(raw).flatMap { filter }.map { message =>
            config.outputFormat match {
                case OutputFormat(f) if(f.toLowerCase() == "rtf") => MailFormatter.toRTF(message)
                case OutputFormat(f) if(f.toLowerCase() == "txt") => MailFormatter.toString(message)
                case _ => MailFormatter.toString(message)
            }
        }
    }

    def readMessage(content: String): Option[MimeMessage] = {
        // from http://stackoverflow.com/questions/3444660/java-email-message-parser
        val s = Session.getDefaultInstance(new Properties())
        val is = new ByteArrayInputStream(content.getBytes())
        try {
            Some(new MimeMessage(s, is))
        } catch {
            case x: java.nio.charset.MalformedInputException => { println("Exception: " + x); None }
        }
    }
}

import java.io.File
import java.io.FileOutputStream

import org.joda.time.DateTime
import org.joda.time.DateTimeZone

object MailHandler {
    val defaultConfig = Config()

    def processFile(file: File, base: Option[File] = None)(implicit config: Config = defaultConfig) {
        def stripFilename(path: String) = path.split("/").toList.last

        def readRaw(file: File): Option[String] = {
            if(file.isFile) {
                try {
                    Some(io.Source.fromFile(file).mkString)
                } catch {
                    case x: java.nio.charset.MalformedInputException => { println("Exception: " + x); None }
                }
            } else {
                None
            }
        }

        def addExtension(fpath: String): String = {
            if(config.appendExtension) {
                val dot = if(!fpath.endsWith(".")) "." else ""
                fpath + dot + config.outputFormat.extension
            } else fpath
        }

        def getUniquePrefix(file: File, base: Option[File]): String = {
            base.flatMap({ b =>
                val bslash = b.getPath + "/"
                if(file.getPath.startsWith(bslash)) {
                    Some(file.getPath.substring(bslash.length))
                } else {
                    None
                }
            }).getOrElse("")
        }

        if(file.exists && file.isDirectory) {
            processDirectory(file, base)
        } else {
            val uniquePrefix = getUniquePrefix(file, base)
            val fpath = file.getPath
            val outpath = config.outputDirectory + addExtension(uniquePrefix)

            val outputFormatExtension = config.outputFormat.extension

            readRaw(file).flatMap { MailCore.handleRawMessage } map { println("Output: " + outpath); writeOut(outpath, _) }
        }
    }

    def processFiles(files: List[File], base: Option[File] = None)(implicit config: Config = defaultConfig) {
        files.map { file => processFile(file, base) }
    }

    def processFilePaths(_files: List[String])(implicit config: Config = defaultConfig) {
        val files = _files.map({ new File(_) })
        processFiles(files)
    }

    def processDirectory(directory: File, base: Option[File] = None)(implicit config: Config = defaultConfig) {
            if(directory.isDirectory) {
                Option(directory.listFiles).map({ fpaths => processFiles(fpaths.toList, base) })
            }
    }

    def processDirectories(directories: List[String])(implicit config: Config = defaultConfig) {
        for(path <- directories) {
            val dir = new File(path)
            processDirectory(dir)
        }
    }

    def processConfig(implicit config: Config) {
        for(file <- config.paths) file match {
            case dir if(dir.exists && dir.isDirectory) => processDirectory(dir, Some(dir))
            case file if(file.exists && file.isFile) => processFile(file)
        }
    }

    def writeOut(outpath: String, message: String) {
        val output = new FileOutputStream(new File(outpath))
        output.write(message.getBytes)
        output.close()
    }
}

object ImplicitFieldFormatters {
    import scala.language.implicitConversions

    val desiredTimezone = DateTimeZone.forID("America/Los_Angeles")

    implicit def makeDate(date: Option[java.util.Date]): Option[String] = {
        date.map({
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
    case class Header(label: String, value: List[Any]) {
        override def toString = "%s: %s".format(label, value.mkString(", "))
    }
    case class ParsedMessage(headers: List[Header] = List(), body: String)

    def parse(message: MimeMessage): ParsedMessage = {
        import se.hardchee.MailConverter.ImplicitFieldFormatters._

        val from = message.getFrom()
        val to = message.getRecipients(Message.RecipientType.TO)
        val cc = message.getRecipients(Message.RecipientType.CC)
        val bcc = message.getRecipients(Message.RecipientType.BCC)
        val date = Option(message.getSentDate())
        val subject = Option(message.getSubject())
        val body = MailUtilities.extractBody(message)
        val attachmentNames: Option[List[String]] = MailUtilities.extractAttachmentNames(message)

        def convertList(label: String, x: Option[List[_]]): Option[Header] = x map { vals => Header(label, vals) }
        def convert(label: String, x: Option[String]):      Option[Header] = x map { vals => Header(label, List(vals)) }

        val headers = List(
            convertList("From", from),
            convertList("To", to),
            convertList("CC", cc),
            convertList("BCC", bcc),
            convert("Date", date),
            convert("Subject", subject),
            convertList("Attachments", attachmentNames)
        ).flatten // convert* produce Option[_]'s, we can use this to generate quick and easy optional headers!

        ParsedMessage(headers, body)
    }

    def toString(message: MimeMessage): String = {
        val ParsedMessage(_headers, body) = parse(message)
        val headers = _headers.mkString("\n")

        s"""|$headers
            |================================================================================
            |$body
            |""".stripMargin
    }

    def toRTF(message: MimeMessage)(implicit config: Config): String = {
        import com.tutego.jrtf.Rtf.rtf
        import com.tutego.jrtf.RtfHeader.{color => hcolor, font => hfont}
        import com.tutego.jrtf.RtfPara
        import com.tutego.jrtf.RtfPara.p
        import com.tutego.jrtf.RtfTextPara
        import com.tutego.jrtf.RtfText
        import com.tutego.jrtf.RtfText.{color => tcolor, font => tfont, text, backgroundcolor => bgcolor}

        import scala.collection.JavaConversions._

        import java.io.FileWriter

        val ParsedMessage(_headers, body) = parse(message)

        def highlightNames(normal:Object=>RtfText, highlight:Object=>RtfText, vals :List[Any]): List[RtfText] = {
            import scala.language.implicitConversions
            implicit def anyToObject(x: Any): Object = x match {
                case x: Object => x
                case x => x.toString
            }

            val lookingFor: List[InternetAddress] = config.highlightRecipients
            vals match {
                case Nil => Nil
                case (x:InternetAddress) :: xs if(!lookingFor.isEmpty && MailComparisons.compareAddresses(lookingFor, List[InternetAddress](x))) => highlight(x) :: highlightNames(normal, highlight, xs)
                case x :: xs => normal(x) :: highlightNames(normal, highlight, xs)
            }
        }

        def interleave(text: RtfText, elems:List[RtfText]): List[RtfText] = elems match {
            case Nil => Nil
            case elem :: Nil => elem :: Nil
            case elem :: xs => elem :: text :: interleave(text, xs)
        }

        def highlightToCcBcc(l: String, vals: List[Any]) = {
            text( ( text(l) :: text(": ") :: interleave(text(", "), highlightNames(text(_), bgcolor(3, _), vals) ) ).toIterable: java.util.Collection[RtfText] )
        }

        val headers: java.util.Collection[RtfText] = interleave(text("\n"), _headers.map({
            case h@Header("From", vals) => bgcolor(0, h)
            case h@Header("Date", vals) => bgcolor(1, h)
            case h@Header("Subject", vals) => bgcolor(2, h)
            case h@Header(l@"To", vals) => highlightToCcBcc(l, vals)
            case h@Header(l@"Cc", vals) => highlightToCcBcc(l, vals)
            case h@Header(l@"Bcc", vals) => highlightToCcBcc(l, vals)
            case h@Header(label, vals) => text(h)
        })).toIterable

        val headerPara = p(headers)
        val sepPara = p(text("================================================================================"))
        val bodyPara = p(body)

        val paras: java.util.Collection[RtfPara] = List(headerPara, sepPara, bodyPara).toIterable

        val r = rtf
            .header(
                hcolor(110, 201, 255).at(3), // Green
                hcolor(74, 223, 103).at(2), // Green
                hcolor(248, 239, 116).at(1), // Yellow
                hcolor(255, 49, 160).at(0) // Magenta
            ).section(paras)

        r.toString
    }
}

// vim: set ts=4 sw=4 et:
