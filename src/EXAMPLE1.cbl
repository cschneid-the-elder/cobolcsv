       Identification Division.
       Program-ID. EXAMPLE1.
      * 
      * The purpose of this program is to test the CSVPARSE
      * subroutine and to serve as an example of how to
      * correctly call the same.
      * 
      * Note that this program makes use of conditional
      * compilation constructs.  I don't have a mainframe 
      * to test with so I've just freehanded those portions
      * of the code intended for IBM Z and conditionally
      * compiled if IGY-COMPILER-VRM is defined.
      * 
      * Your primary interest is likely to be paragraphs 1000
      * and 1010, which are the heart of the state machine
      * for calling CSVPARSE.
      * 
       Environment Division.
       Configuration Section.
       Repository.
           Function All Intrinsic.
       Input-Output Section.
       File-Control.
           >>IF IGY-COMPILER-VRM DEFINED
           Select INPUT01 Assign INPUT01.
           >>ELSE
           Select INPUT01 Assign WS-INPUT01-NAME
             Organization Line Sequential.
           >>END-IF
       Data Division.
       File Section.
       FD  INPUT01
           Record Varying 1 To 27990 Depending INPUT01-RECORD-LENGTH
           Block 0
           Recording V.
       01  INPUT01-RECORD PIC X(27990).
       Working-Storage Section.
       01  WS-CONSTANTS.
           05  MYNAME                  PIC X(008) VALUE 'EXAMPLE1'.

       01  WS-WORK-AREAS.
           >>IF IGY-COMPILER-VRM DEFINED
           05  WS-ABND-CD              PIC 9(008) COMP VALUE 42.
           05  WS-ABND-DUMP            PIC 9(008) COMP VALUE 1.
           >>ELSE
           05  WS-ARG-NB               PIC S9(004) BINARY VALUE +0.
           >>END-IF
           05  INPUT01-RECORD-LENGTH   PIC S9(004) BINARY VALUE +0.
           05  FIELD-COUNT             PIC S9(004) BINARY VALUE +0.
           05  FIRST-FIELD-LENGTH      PIC S9(009) BINARY VALUE +0.
           05  SECOND-FIELD-LENGTH     PIC S9(009) BINARY VALUE +0.
           05  THIRD-FIELD-LENGTH      PIC S9(009) BINARY VALUE +0.
           05  FOURTH-FIELD-LENGTH     PIC S9(009) BINARY VALUE +0.
           05  FIFTH-FIELD-LENGTH      PIC S9(009) BINARY VALUE +0.
           05  FIRST-FIELD             PIC X(128)         VALUE SPACES.
           05  SECOND-FIELD            PIC X(128)         VALUE SPACES.
           05  THIRD-FIELD             PIC X(128)         VALUE SPACES.
           05  FOURTH-FIELD            PIC X(128)         VALUE SPACES.
           05  FIFTH-FIELD             PIC X(128)         VALUE SPACES.
           >>IF IGY-COMPILER-VRM NOT DEFINED
           05  WS-INPUT01-NAME         PIC X(128)         VALUE SPACES.
           >>END-IF
           05  WS-ARG-DLIM             PIC X(001)         VALUE SPACE.
           05  WS-ARG-STYLE            PIC X(001)         VALUE SPACE.
               88  WS-ARG-STYLE-UNIX                      VALUE 'U'.
               88  WS-ARG-STYLE-RFC4180                   VALUE 'R'.
           05  WS-ARG-EQUAL            PIC X(001)         VALUE SPACE.
               88  WS-ARG-EQUAL-TRUE                      VALUE 'T'.
               88  WS-ARG-EQUAL-FALSE                     VALUE 'F'.
           
       01  WS-SWITCHES.
           >>IF IGY-COMPILER-VRM DEFINED
           05  CSVPARSE-RETURN-CODE    PIC S9(004) BINARY VALUE +0.
           >>ELSE
           05  CSVPARSE-RETURN-CODE    PIC S9(009) BINARY VALUE +0.
           >>END-IF
           Copy CSVRC Replacing ==:PRFX:== By ==CSVPARSE-==.
           05  INPUT01-EOF-SW          PIC X(001) VALUE 'N'.
               88  INPUT01-EOF                    VALUE 'Y' FALSE 'N'.

       Copy CSVOPTS Replacing ==:PRFX:== By ==CSVPARSE-==.
       Copy CSVPARMS Replacing ==:PRFX:== By ==CSVPARSE-==.

       Linkage Section.
       >>IF IGY-COMPILER-VRM NOT DEFINED
       01  OS-PARM.
           05  OS-PARM-LENGTH      PIC S9(004) BINARY.
           05  OS-PARM-VALUE       PIC X(003).
       >>END-IF
           
       77  DUMMY-FIELD             PIC X(999).
       
       >>IF IGY-COMPILER-VRM NOT DEFINED
       77  DUMPER                  PIC S9(004) BINARY.
       >>END-IF
       
       >>IF IGY-COMPILER-VRM DEFINED
       Procedure Division Using OS-PARM.
       >>ELSE
       Procedure Division.
        >>END-IF
          PERFORM 0100-INIT
           
           OPEN INPUT INPUT01
           
           PERFORM 8010-READ-INPUT01
             UNTIL INPUT01-RECORD-LENGTH > 0
             OR INPUT01-EOF
           
           PERFORM 1000-PROCESS-INPUT01
             UNTIL INPUT01-EOF
           
           CLOSE INPUT01
           MOVE +0 TO RETURN-CODE
           GOBACK.

       0100-INIT.
           >>IF IGY-COMPILER-VRM DEFINED
           IF OS-PARM-LENGTH >= 3
               MOVE OS-PARM-VALUE(1:1) TO WS-ARG-DLIM
               MOVE UPPER-CASE(OS-PARM-VALUE(2:1)) TO WS-ARG-STYLE
               MOVE UPPER-CASE(OS-PARM-VALUE(3:1)) TO WS-ARG-EQUAL
           ELSE
               DISPLAY MYNAME ' required parameters not provided'
               PERFORM 9999-ABEND
           END-IF
           >>ELSE
           SET ADDRESS OF DUMPER TO NULL
           ACCEPT WS-ARG-NB FROM ARGUMENT-NUMBER
           IF WS-ARG-NB >= 4
               DISPLAY 1 UPON ARGUMENT-NUMBER
               ACCEPT WS-INPUT01-NAME FROM ARGUMENT-VALUE
               DISPLAY 2 UPON ARGUMENT-NUMBER
               ACCEPT WS-ARG-DLIM FROM ARGUMENT-VALUE
               DISPLAY 3 UPON ARGUMENT-NUMBER
               ACCEPT WS-ARG-STYLE FROM ARGUMENT-VALUE
               MOVE UPPER-CASE(WS-ARG-STYLE) TO WS-ARG-STYLE
               DISPLAY 4 UPON ARGUMENT-NUMBER
               ACCEPT WS-ARG-EQUAL FROM ARGUMENT-VALUE
               MOVE UPPER-CASE(WS-ARG-EQUAL) TO WS-ARG-EQUAL
           ELSE
               DISPLAY MYNAME ' required parameters not provided'
               PERFORM 9999-ABEND
           END-IF
           >>END-IF
           
           MOVE WS-ARG-DLIM TO CSVPARSE-FIELD-DELIMITER
           EVALUATE TRUE
             WHEN WS-ARG-EQUAL-TRUE
                  SET CSVPARSE-TOLERATE-LEADING-EQUAL TO TRUE
             WHEN WS-ARG-EQUAL-FALSE
                  SET CSVPARSE-TOLERATE-LEADING-EQUAL TO FALSE
             WHEN OTHER
                  DISPLAY 
                    MYNAME 
                    ' tolerate leading equal parm must be T or F'
                  PERFORM 9999-ABEND
           END-EVALUATE
           EVALUATE TRUE
             WHEN WS-ARG-STYLE-UNIX
                  SET CSVPARSE-UNIX TO TRUE
             WHEN WS-ARG-STYLE-RFC4180
                  SET CSVPARSE-RFC4180 TO TRUE
             WHEN OTHER
                  DISPLAY 
                    MYNAME 
                    ' file format style must be U or R'
                  PERFORM 9999-ABEND
           END-EVALUATE
           .
           
       1000-PROCESS-INPUT01.
           PERFORM 8020-SET-CSVPARSE-PARMS
           CALL "CSVPARSE" USING
             CSVPARSE-OPTS
             CSVPARSE-RECORD-LENGTH
             CSVPARSE-RECORD-ADDRESS
             CSVPARSE-FIELD-LENGTH
             CSVPARSE-FIELD-ADDRESS
           END-CALL
           MOVE RETURN-CODE TO CSVPARSE-RETURN-CODE
           DISPLAY 
             MYNAME 
             ' CSVPARSE-RETURN-CODE = ' 
             CSVPARSE-RETURN-CODE
           EVALUATE TRUE
             WHEN FIELD-COUNT > 5
                  DISPLAY MYNAME ' something is wrong'
                  PERFORM 9999-ABEND
             WHEN CSVPARSE-RECORD-COMPLETE
                  ADD 1 TO FIELD-COUNT
                  PERFORM 1010-MOVE-TO-FIELD
                  DISPLAY MYNAME ' FIRST-FIELD  = ' 
                    FIRST-FIELD(1:FIRST-FIELD-LENGTH)
                  DISPLAY MYNAME ' SECOND-FIELD = ' 
                    SECOND-FIELD(1:SECOND-FIELD-LENGTH)
                  DISPLAY MYNAME ' THIRD-FIELD  = ' 
                    THIRD-FIELD(1:THIRD-FIELD-LENGTH)
                  DISPLAY MYNAME ' FOURTH-FIELD = ' 
                    FOURTH-FIELD(1:FOURTH-FIELD-LENGTH)
                  DISPLAY MYNAME ' FIFTH-FIELD  = ' 
                    FIFTH-FIELD(1:FIFTH-FIELD-LENGTH)
                  INITIALIZE FIELD-COUNT INPUT01-RECORD-LENGTH
                  PERFORM 8010-READ-INPUT01
                    UNTIL INPUT01-RECORD-LENGTH > 0
                    OR INPUT01-EOF
             WHEN CSVPARSE-FIELD-COMPLETE
                  ADD 1 TO FIELD-COUNT
                  PERFORM 1010-MOVE-TO-FIELD
             WHEN CSVPARSE-FIELD-INCOMPLETE
                  INITIALIZE INPUT01-RECORD-LENGTH
                  PERFORM 8010-READ-INPUT01
                    UNTIL INPUT01-RECORD-LENGTH > 0
                    OR INPUT01-EOF
             WHEN CSVPARSE-ERROR-OPT
                  DISPLAY MYNAME ' error in CSVPARSE options'
                  PERFORM 9999-ABEND
             WHEN CSVPARSE-ERROR-PARM
                  DISPLAY MYNAME ' error in CSVPARSE parms'
                  PERFORM 9999-ABEND
             WHEN OTHER
                  DISPLAY MYNAME ' something else is wrong'
                  PERFORM 9999-ABEND
           END-EVALUATE
           .
           
       1010-MOVE-TO-FIELD.
           SET ADDRESS OF DUMMY-FIELD TO CSVPARSE-FIELD-ADDRESS
           EVALUATE FIELD-COUNT ALSO CSVPARSE-FIELD-LENGTH
             WHEN 1 ALSO 0
                  INITIALIZE FIRST-FIELD 
                  MOVE 1 TO FIRST-FIELD-LENGTH
             WHEN 1 ALSO ANY
                  MOVE DUMMY-FIELD(1:CSVPARSE-FIELD-LENGTH)
                    TO FIRST-FIELD
                  MOVE CSVPARSE-FIELD-LENGTH TO FIRST-FIELD-LENGTH
             WHEN 2 ALSO 0
                  INITIALIZE SECOND-FIELD 
                  MOVE 1 TO SECOND-FIELD-LENGTH
             WHEN 2 ALSO ANY
                  MOVE DUMMY-FIELD(1:CSVPARSE-FIELD-LENGTH)
                    TO SECOND-FIELD
                  MOVE CSVPARSE-FIELD-LENGTH TO SECOND-FIELD-LENGTH
             WHEN 3 ALSO 0
                  INITIALIZE THIRD-FIELD 
                  MOVE 1 TO THIRD-FIELD-LENGTH
             WHEN 3 ALSO ANY
                  MOVE DUMMY-FIELD(1:CSVPARSE-FIELD-LENGTH)
                    TO THIRD-FIELD
                  MOVE CSVPARSE-FIELD-LENGTH TO THIRD-FIELD-LENGTH
             WHEN 4 ALSO 0
                  INITIALIZE FOURTH-FIELD 
                  MOVE 1 TO FOURTH-FIELD-LENGTH
             WHEN 4 ALSO ANY
                  MOVE DUMMY-FIELD(1:CSVPARSE-FIELD-LENGTH)
                    TO FOURTH-FIELD
                  MOVE CSVPARSE-FIELD-LENGTH TO FOURTH-FIELD-LENGTH
             WHEN 5 ALSO 0
                  INITIALIZE FIFTH-FIELD 
                  MOVE 1 TO FIFTH-FIELD-LENGTH
             WHEN 5 ALSO ANY
                  MOVE DUMMY-FIELD(1:CSVPARSE-FIELD-LENGTH)
                    TO FIFTH-FIELD
                  MOVE CSVPARSE-FIELD-LENGTH TO FIFTH-FIELD-LENGTH
             WHEN OTHER
                  DISPLAY MYNAME ' FIELD-COUNT has an invalid value'
                  PERFORM 9999-ABEND
           END-EVALUATE
           .
           
       8010-READ-INPUT01.
           READ INPUT01
             AT END SET INPUT01-EOF TO TRUE
           END-READ
           .
           
       8020-SET-CSVPARSE-PARMS.
           SET CSVPARSE-RECORD-ADDRESS TO ADDRESS OF INPUT01-RECORD
           MOVE INPUT01-RECORD-LENGTH TO CSVPARSE-RECORD-LENGTH
           .
           
       9999-ABEND.
           >>IF IGY-COMPILER-VRM DEFINED
           CALL 'CEE3ABD' USING
               WS-ABND-CD
               WS-ABND-DUMP
           END-CALL
           >>ELSE
           MOVE +0 TO DUMPER
           >>END-IF
           .
