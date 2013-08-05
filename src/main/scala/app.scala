package se.hardchee.MailConverter

import org.joda.time.DateTime
import javax.mail.internet.InternetAddress

case class Config(
    concat: Boolean = false,
    outputDirectory: String = "/tmp/",
    appendExtension: Boolean = true,

    fromDate: Option[DateTime] = None,
    untilDate: Option[DateTime] = None,

    paths: List[java.io.File] = List(),

    senders: List[InternetAddress] = List(),
    recipients: List[InternetAddress] = List(),
    allRecipientsRequired: Boolean = false,
    highlightRecipients: List[InternetAddress] = List(),

//    inputFormat: String = "cyrus", // TODO: Support other formats
    outputFormat: OutputFormat = OutputFormat("txt")
)

case class OutputFormat(extension: String)


object app {
    def useConfig(config: Config) {
        MailHandler.processConfig(config)
    }

    def main(args: Array[String]) {
        val parser = new scopt.OptionParser[Config]("EmailExporter") {
            import scopt.Read
            import scopt.Read.reads

            implicit val dateRead: Read[DateTime] = reads { new DateTime(_) }
            implicit val optionalDateRead: Read[Option[DateTime]] = reads { time => Some(new DateTime(time)) }
            implicit val mailAddressRead: Read[InternetAddress] = reads { new InternetAddress(_) }

            head("EmailExporter", "0.1")

            opt[Unit]('c', "concat") action { (_, c: Config) => c.copy(concat = true) } text("Concatenate mail files into one big output file")

            opt[String]('o', "outputdir") action { (v: String, c: Config) => c.copy(outputDirectory = v) } text("Directory to put output files")
            opt[Unit]('x', "extension") action { (_, c: Config) => c.copy(appendExtension = true) } text("Append file extension to format")

//            opt[String]('i', "input-format") action { (v: String, c: Config) => c.copy(inputFormat = v) } text("Input file format")
            opt[String]('f', "output-format") action { (v: String, c: Config) => c.copy(outputFormat = OutputFormat(v)) } text("Output file format")

            opt[Option[DateTime]]("date-from") action { (v: Option[DateTime], c: Config) => c.copy(fromDate = v) } text("Only process messages after this date")
            opt[Option[DateTime]]("date-until") action { (v: Option[DateTime], c: Config) => c.copy(untilDate = v) } text("Only process messages before this date")

            opt[InternetAddress]('s', "sender") action { (v: InternetAddress, c: Config) => c.copy(senders = c.senders :+ v) } text("Only match senders in this list") unbounded()
            opt[InternetAddress]('r', "recipient") action { (v: InternetAddress, c: Config) => c.copy(recipients = c.recipients :+ v) } text("Only match recipients in this list") unbounded()
            opt[Unit]('a', "all-recipients") action { (_, c: Config) => c.copy(allRecipientsRequired = true) } text("Require all recipients (instead of just some)")
            opt[InternetAddress]('l', "highlight") action { (v: InternetAddress, c: Config) => c.copy(highlightRecipients = c.highlightRecipients :+ v) } text("Highlight particular recipients in To fields") unbounded()

            help("help") text("prints this usage text")

            arg[java.io.File]("<path>...") action {
                case (v: java.io.File, c: Config) if(v.exists) => c.copy(paths = c.paths :+ v)
                case _ => sys.error("File doesn't exist")
            } unbounded() text("Paths to process")
        }

        parser.parse(args, Config()) map { config =>
            useConfig(config)
        }
    }
}

// vim: set ts=4 sw=4 et:
