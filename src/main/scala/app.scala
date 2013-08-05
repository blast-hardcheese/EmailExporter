package se.hardchee.MailConverter

import scopt.immutable.OptionParser
case class Config(
    concat: Boolean = false,
    outputDirectory: String = "/tmp/",
    appendExtension: Boolean = true,

    paths: List[String] = List(),

//    inputFormat: String = "cyrus", // TODO: Support other formats
    outputFormat: String = "txt"
)


object app {
    def useConfig(config: Config) {
        MailHandler.processFilePaths(config.outputDirectory + "/%s", config.paths)
    }

    def main(args: Array[String]) {
        val parser = new OptionParser[Config]("EmailExporter", "0.1") { def options = Seq(
            flag("c", "concat", "Concatenate mail files into one big output file") { (c: Config) => c.copy(concat = true) },
            opt("o", "outputdir", "Directory to put output files") { (v: String, c: Config) => c.copy(outputDirectory = v) },
            flag("x", "extension", "Append file extension to format") { (c: Config) => c.copy(appendExtension = true) },

            arglistOpt("<files|directories>", "Paths to process") { (v: String, c: Config) => c.copy(paths = c.paths :+ v) }
        ) }

        parser.parse(args, Config()) map { config =>
            useConfig(config)
        }
    }
}

// vim: set ts=4 sw=4 et:
