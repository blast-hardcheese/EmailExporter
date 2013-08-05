EmailExporter
=============

Basic usage:

    EmailExporter 0.1
    Usage: EmailExporter [options] <path>...

      -o <value> | --outputdir <value>
            Directory to put output files
      -x | --extension
            Append file extension to format
      -f <value> | --output-format <value>
            Output file format
      --date-from <value>
            Only process messages after this date
      --date-until <value>
            Only process messages before this date
      -s <value> | --sender <value>
            Only match senders in this list
      -r <value> | --recipient <value>
            Only match recipients in this list
      -a | --all-recipients
            Require all recipients (instead of just some)
      -l <value> | --highlight <value>
            Highlight particular recipients in To fields
      --help
            prints this usage text
      <path>...
            Paths to process

The basic usage is something like this:

    run --extension
        --outputdir /tmp/mail/
        --output-format rtf
        --date-from "2009-06-26T19:00:00-07:00"
        --date-until "2009-06-26T20:00:00-07:00"
        --sender blast@hardchee.se
        --sender noreply@spamarrest.com
        --recipient devon@work.tld
        --highlight noreply@spamarrest.com
        sample/99.

Here we're processing one file (sample/99.), but we could easily process the entire directory by passing "sample" instead.
This example shows date constraints, sender/recipient constraints, and highlighting of recipients (only supported by the RTF exporter).
