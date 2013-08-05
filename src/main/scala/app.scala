package se.hardchee.MailConverter

case class Config(
    concat: Boolean = false,
    outputDirectory: String = "/tmp/",
    appendExtension: Boolean = true,

    paths: List[java.io.File] = List(),

//    inputFormat: String = "cyrus", // TODO: Support other formats
    outputFormat: String = "txt"
)


object app {
    def useConfig(config: Config) {
        MailHandler.processFiles(config.outputDirectory + "/%s", config.paths)
    }

    def main(args: Array[String]) {
        val parser = new scopt.OptionParser[Config]("EmailExporter") {
            head("EmailExporter", "0.1")

            opt[Unit]('c', "concat") action { (_, c: Config) => c.copy(concat = true) } text("Concatenate mail files into one big output file")

            opt[String]('o', "outputdir") action { (v: String, c: Config) => c.copy(outputDirectory = v) } text("Directory to put output files")
            opt[Unit]('x', "extension") action { (_, c: Config) => c.copy(appendExtension = true) } text("Append file extension to format")

//            opt[String]('i', "input-format") action { (v: String, c: Config) => c.copy(inputFormat = v) } text("Input file format")
            opt[String]('f', "output-format") action { (v: String, c: Config) => c.copy(outputFormat = v) } text("Output file format")

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
