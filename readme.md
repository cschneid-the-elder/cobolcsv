## Parsing CSV files with COBOL

It turns out there is more to parsing delimited files with COBOL than a simple
UNSTRING statement.  There are different styles of delimited files, not just
comma separated but also other delimiters and other methods of escaping the
delimiter when it appears within the data.

The two resources I used 
are [here](http://www.catb.org/~esr/writings/taoup/html/ch05s02.html) 
and [here](https://www.rfc-editor.org/rfc/rfc4180).

You must know some things about your data before
you begin: is there a header record? what is the delimiter? how is the 
delimiter escaped when it appears in the data? are leading equal signs
allowed as an encouragement to accept the data literally?

Once you know these things, you can use
the EXAMPLE1.cbl code as a guide to writing your own program to call the
CSVPARSE subroutine with the appropriate parameters to parse your delimited file.


