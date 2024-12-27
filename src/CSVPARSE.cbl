       Identification Division.
       Program-ID. CSVPARSE.
      * 
      * The purpose of this program is to encapsulate the processing
      * necessary to parse a CSV (Comma Separated Value) file.  This
      * is a bit of a misnomer, as the field delimiter may be specified
      * by the caller, and can be any single character.
      * 
      * Like many people, I was under the impression that this sort of
      * processing was trivial in COBOL and could easily be done with
      * a simple UNSTRING.  I was wrong.  There are two major sets of
      * guidelines for such files, one originating with Unix and one
      * now documented as RFC-4180.  As of this writing, the guidelines
      * are available at...
      * 
      *   http://www.catb.org/~esr/writings/taoup/html/ch05s02.html
      *   ...and...
      *   https://www.rfc-editor.org/rfc/rfc4180
      * 
      * ...and are well worth reading as callers of this program must
      * know some things about the data they seek to parse in order to
      * correctly set the fields in the CSVOPTS copybook.
      * 
      * 
      * 
      * 
       Environment Division.
       Configuration Section.
       Repository.
           Function All Intrinsic.
       Data Division.
       Working-Storage Section.
       01  WS-CONSTANTS.
           05  MYNAME                  PIC X(008) VALUE 'CSVPARSE'.
           05  DEFAULT-FIELD-LENGTH    PIC S9(009) BINARY VALUE +100.
       
       01  WS-WORK-AREAS.
           05  CSV-FIELD-PTR           POINTER     VALUE NULL.
           05  OLD-FIELD-PTR           POINTER     VALUE NULL.
           05  CSV-FIELD-LENGTH        PIC S9(009) BINARY VALUE +0.
           05  OLD-FIELD-LENGTH        PIC S9(009) BINARY VALUE +0.
           05  PREV-REC-POSN           PIC S9(009) PACKED-DECIMAL 
                                                   VALUE +1.
           05  CURR-FIELD-POSN         PIC S9(009) PACKED-DECIMAL 
                                                   VALUE +0.

       01  WS-SWITCHES.
           05  PREV-RETURN-CODE        PIC S9(004) BINARY VALUE +0.
           Copy CSVRC Replacing ==:PRFX:== By ==PREV-==.
           05  FIRST-TIME-SW           PIC X(001) VALUE 'Y'.
               88  FIRST-TIME                     VALUE 'Y' FALSE 'N'.
           05  BACKSLASH-FOUND-SW      PIC X(001) VALUE 'N'.
               88  BACKSLASH-FOUND                VALUE 'Y' FALSE 'N'.
           05  QUOTE-FOUND-SW          PIC X(001) VALUE 'N'.
               88  QUOTE-FOUND                    VALUE 'Y' FALSE 'N'.
           05  ESCAPE-QUOTE-FOUND-SW   PIC X(001) VALUE 'N'.
               88  ESCAPE-QUOTE-FOUND             VALUE 'Y' FALSE 'N'.
               
       Local-Storage Section.
       01  LS-WORK-AREAS.
           05  CURR-REC-POSN           PIC S9(009) PACKED-DECIMAL 
                                                   VALUE +0.
           05  NEXT-REC-POSN           PIC S9(009) PACKED-DECIMAL 
                                                   VALUE +0.
       
       01  LS-SWITCHES.
           05  MY-RETURN-CODE          PIC S9(004) BINARY VALUE +0.
           Copy CSVRC Replacing ==:PRFX:== By ==RC-==.
               
       Linkage Section.
       Copy CSVOPTS Replacing ==:PRFX:== By ==OPT-==.
       Copy CSVPARMS Replacing ==:PRFX:== By ==PARM-==.
       
       01  CSV-RECORD.
           05  OCCURS 0 TO UNBOUNDED 
               DEPENDING PARM-RECORD-LENGTH PIC X(001).
       
       01  CSV-FIELD.
           05  OCCURS 0 TO UNBOUNDED 
           DEPENDING CSV-FIELD-LENGTH PIC X(001).
       
       01  OLD-CSV-FIELD PIC X(999999999).
       
       Procedure Division Using
         OPT-OPTS
         PARM-RECORD-LENGTH
         PARM-RECORD-ADDRESS
         PARM-FIELD-LENGTH
         PARM-FIELD-ADDRESS
         .
         
           PERFORM 0100-INIT
           
           IF PREV-REC-POSN > PARM-RECORD-LENGTH
           AND PREV-FIELD-COMPLETE
           AND NOT QUOTE-FOUND
               *> Final field on this record is of length zero
               SET RC-RECORD-COMPLETE TO TRUE
           END-IF
           
           PERFORM 1000-PROCESS-CSV-RECORD
             VARYING CURR-REC-POSN FROM PREV-REC-POSN BY 1
             UNTIL CURR-REC-POSN > PARM-RECORD-LENGTH
             OR MY-RETURN-CODE NOT = +0
           
           IF QUOTE-FOUND OR BACKSLASH-FOUND
               SET RC-FIELD-INCOMPLETE TO TRUE
           END-IF
           MOVE MY-RETURN-CODE TO PREV-RETURN-CODE
           MOVE CURR-FIELD-POSN TO PARM-FIELD-LENGTH
           SET  PARM-FIELD-ADDRESS TO CSV-FIELD-PTR
           MOVE CURR-REC-POSN TO PREV-REC-POSN
           
           EVALUATE TRUE
             WHEN RC-FIELD-COMPLETE 
                  INITIALIZE CURR-FIELD-POSN ALL VALUE
             WHEN RC-FIELD-INCOMPLETE 
                  INITIALIZE PREV-REC-POSN ALL VALUE
             WHEN RC-RECORD-COMPLETE
                  INITIALIZE CURR-FIELD-POSN PREV-REC-POSN ALL VALUE
           END-EVALUATE
           
           MOVE MY-RETURN-CODE TO RETURN-CODE
           GOBACK
           .

       0100-INIT.
           IF PARM-RECORD-LENGTH <= 0
               SET RC-ERROR-PARM TO TRUE
               MOVE MY-RETURN-CODE TO RETURN-CODE
               GOBACK
           END-IF
           
           IF PARM-RECORD-ADDRESS = NULL
               SET RC-ERROR-PARM TO TRUE
               MOVE MY-RETURN-CODE TO RETURN-CODE
               GOBACK
           END-IF

           EVALUATE TRUE
             WHEN OPT-UNIX
             WHEN OPT-RFC4180
                  CONTINUE
             WHEN OTHER
                  SET RC-ERROR-OPT TO TRUE
                  MOVE MY-RETURN-CODE TO RETURN-CODE
                  GOBACK
           END-EVALUATE
                      
           SET ADDRESS OF CSV-RECORD TO PARM-RECORD-ADDRESS
           
           IF FIRST-TIME
               PERFORM 8010-ALLOCATE-FIELD
               SET PREV-RECORD-COMPLETE TO TRUE
               SET FIRST-TIME TO FALSE
           END-IF
           .
           
       1000-PROCESS-CSV-RECORD.
           EVALUATE TRUE ALSO CSV-RECORD(CURR-REC-POSN:1)
             WHEN OPT-UNIX    ALSO '\'
                  IF BACKSLASH-FOUND
                      SET BACKSLASH-FOUND TO FALSE
                      PERFORM 2000-MOVE-DATA-TO-FIELD
                      IF CURR-REC-POSN = PARM-RECORD-LENGTH
                          SET RC-RECORD-COMPLETE TO TRUE
                      END-IF
                  ELSE
                      SET BACKSLASH-FOUND TO TRUE
                  END-IF
             WHEN OPT-UNIX    ALSO OPT-FIELD-DELIMITER
                  IF BACKSLASH-FOUND
                      SET BACKSLASH-FOUND TO FALSE
                      PERFORM 2000-MOVE-DATA-TO-FIELD
                      IF CURR-REC-POSN = PARM-RECORD-LENGTH
                          SET RC-RECORD-COMPLETE TO TRUE
                      END-IF
                  ELSE
                      SET RC-FIELD-COMPLETE TO TRUE
                  END-IF
             WHEN OPT-RFC4180 ALSO OPT-FIELD-DELIMITER
                  IF QUOTE-FOUND
                      PERFORM 2000-MOVE-DATA-TO-FIELD
                  ELSE
                      SET RC-FIELD-COMPLETE TO TRUE
                  END-IF
             WHEN OPT-RFC4180 ALSO '"'
                  *> Consider """" where the field value is just "
                  *> Also "," where the field value is just ,
                  *> Or even """," where the field value is ",
                  *> And then there's "", where the field is empty
                  EVALUATE TRUE
                    WHEN ESCAPE-QUOTE-FOUND
                         *> This quote was escaped by another quote
                         PERFORM 2000-MOVE-DATA-TO-FIELD
                         SET ESCAPE-QUOTE-FOUND TO FALSE
                    WHEN QUOTE-FOUND
                         COMPUTE NEXT-REC-POSN = CURR-REC-POSN + 1
                         IF CSV-RECORD(NEXT-REC-POSN:1) = '"'
                             SET ESCAPE-QUOTE-FOUND TO TRUE
                         ELSE
                             *> Closing quote for field found
                             SET QUOTE-FOUND TO FALSE
                             *> The quote ended both the field 
                             *> _and_ the record
                             IF CURR-REC-POSN = PARM-RECORD-LENGTH
                                 SET RC-RECORD-COMPLETE TO TRUE
                             END-IF
                         END-IF
                    WHEN OTHER
                         SET QUOTE-FOUND TO TRUE
                  END-EVALUATE
             WHEN OPT-TOLERATE-LEADING-EQUAL ALSO '='
                  EVALUATE TRUE
                    WHEN PREV-RECORD-COMPLETE
                    WHEN PREV-FIELD-COMPLETE
                         CONTINUE
                    WHEN OTHER
                         PERFORM 2000-MOVE-DATA-TO-FIELD
                  END-EVALUATE
             WHEN CURR-REC-POSN = PARM-RECORD-LENGTH ALSO ANY
                  PERFORM 2000-MOVE-DATA-TO-FIELD
                  SET RC-RECORD-COMPLETE TO TRUE
             WHEN OTHER
                  PERFORM 2000-MOVE-DATA-TO-FIELD
                  SET BACKSLASH-FOUND TO FALSE
           END-EVALUATE
           .

       2000-MOVE-DATA-TO-FIELD.
           ADD 1 TO CURR-FIELD-POSN
           IF CURR-FIELD-POSN > CSV-FIELD-LENGTH
               PERFORM 8020-REALLOCATE-FIELD
           END-IF
           MOVE CSV-RECORD(CURR-REC-POSN:1) 
             TO CSV-FIELD(CURR-FIELD-POSN:1)
           .

       8010-ALLOCATE-FIELD.
           MOVE DEFAULT-FIELD-LENGTH TO CSV-FIELD-LENGTH
           ALLOCATE 
             CSV-FIELD-LENGTH CHARACTERS 
             INITIALIZED 
             RETURNING CSV-FIELD-PTR
           SET ADDRESS OF CSV-FIELD TO CSV-FIELD-PTR
           .
       
       8020-REALLOCATE-FIELD.
           SET OLD-FIELD-PTR TO CSV-FIELD-PTR
           SET ADDRESS OF OLD-CSV-FIELD TO OLD-FIELD-PTR
           MOVE CSV-FIELD-LENGTH TO OLD-FIELD-LENGTH
           COMPUTE CSV-FIELD-LENGTH = CSV-FIELD-LENGTH * 2
           ALLOCATE 
             CSV-FIELD-LENGTH CHARACTERS 
             INITIALIZED
             RETURNING CSV-FIELD-PTR
           SET ADDRESS OF CSV-FIELD TO CSV-FIELD-PTR
           MOVE OLD-CSV-FIELD(1:OLD-FIELD-LENGTH) TO CSV-FIELD
           FREE OLD-FIELD-PTR
           SET ADDRESS OF OLD-CSV-FIELD TO NULL
           .
       
