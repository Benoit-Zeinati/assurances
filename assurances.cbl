       IDENTIFICATION DIVISION.
       PROGRAM-ID. assurances.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ASSURANCES ASSIGN TO
                                 'assurances-68259db4e2e6f768575516.csv'
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ASSURANCES.
       01 FD-ASSUR-REC.
          05 FD-ASSUR-CODE      PIC X(08).
          05 FILLER             PIC X(01).
          05 FD-ASSUR-CONTRACT  PIC X(14).
          05 FILLER             PIC x(01).
          05 FD-ASSUR-PRODUCT   PIC X(14).
          05 FILLER             PIC X(01).
          05 FD-ASSUR-CLIENT    PIC X(41).
          05 FILLER             PIC x(01).
          05 FD-ASSUR-STATUS    PIC X(08).
          05 FILLER             PIC x(01).
          05 FD-ASSUR-STDATE    PIC X(08).
          05 FILLER             PIC X(01).
          05 FD-ASSUR-ENDDATE   PIC X(08).
          05 FILLER             PIC x(01).
          05 FD-ASSUR-AMOUNT    PIC X(09).
          05 FILLER             PIC X(01).
          05 FD-ASSUR-CURRENCY  PIC X(03).


       WORKING-STORAGE SECTION.
      * 
       01 WS-ASSURANCES-TBL.
          05 WS-ASSUR OCCURS 100 TIMES.
             10 WS-ASSUR-CODE      PIC X(08).
             10 WS-ASSUR-CONTRACT  PIC X(14).
             10 WS-ASSUR-PRODUCT   PIC X(14).
             10 WS-ASSUR-CLIENT    PIC X(41).
             10 WS-ASSUR-STATUS    PIC X(08).
             10 WS-ASSUR-STDATE    PIC X(08).
             10 WS-ASSUR-ENDDATE   PIC X(08).
             10 WS-ASSUR-AMOUNT    PIC X(09).
             10 WS-ASSUR-CURRENCY  PIC X(03).

       01 WS-DATE-OUTPUT.
          05 WS-DAY                PIC X(02).
          05 FILLER                PIC X VALUE '/'.
          05 WS-MONTH              PIC X(02).
          05 FILLER                PIC X VALUE '/'.
          05 WS-YEAR               PIC X(04).
       
       77 WS-ASSUR-IDX             PIC 9(03) VALUE 1.
       77 WS-ASSUR-MAX             PIC 9(03).
       77 WS-EOF                   PIC X VALUE 'N'.
       77 WS-TEST                  PIC X.

       PROCEDURE DIVISION.
      *Open ASSURANCES file (assurances-68259db4e2e6f768575516.csv). 
       OPEN INPUT ASSURANCES.

      *Initialize WS-EOF flag
       MOVE 'N' TO WS-EOF.
       
      *COPY assurances file into assurances table 
       PERFORM UNTIL WS-EOF = 'Y'
         READ ASSURANCES
            AT END 
               MOVE 'Y' TO WS-EOF
            NOT AT END
               MOVE FD-ASSUR-CODE TO WS-ASSUR-CODE(WS-ASSUR-IDX)
               MOVE FD-ASSUR-CONTRACT TO WS-ASSUR-CONTRACT(WS-ASSUR-IDX)
               MOVE FD-ASSUR-PRODUCT TO WS-ASSUR-PRODUCT(WS-ASSUR-IDX)
               MOVE FD-ASSUR-CLIENT TO WS-ASSUR-CLIENT(WS-ASSUR-IDX)
               MOVE FD-ASSUR-STATUS TO WS-ASSUR-STATUS(WS-ASSUR-IDX)
               MOVE FD-ASSUR-STDATE TO WS-ASSUR-STDATE(WS-ASSUR-IDX)
               MOVE FD-ASSUR-ENDDATE TO WS-ASSUR-ENDDATE(WS-ASSUR-IDX)
               MOVE FD-ASSUR-AMOUNT TO WS-ASSUR-AMOUNT(WS-ASSUR-IDX)
               MOVE FD-ASSUR-CURRENCY TO WS-ASSUR-CURRENCY(WS-ASSUR-IDX)
               ADD 1 TO WS-ASSUR-IDX
         END-READ
       END-PERFORM.

      *Close assurances file
       CLOSE ASSURANCES. 

      *Save file size in WS-ASSUR-MAX
       SUBTRACT 1 FROM WS-ASSUR-IDX.
       MOVE WS-ASSUR-IDX TO WS-ASSUR-MAX.

      *DISPLAY records 3 and 7
       MOVE 3 TO WS-ASSUR-IDX.

      *Display headrer line
       PERFORM PARA-DISP-HDR.
       MOVE 3 TO WS-ASSUR-IDX.
       PERFORM PARA-DISP-REC.
       MOVE 7 TO WS-ASSUR-IDX.
       PERFORM PARA-DISP-REC.

       STOP RUN.
      *
      ******************************************************************
      *
       PARA-DISP-REC.
       DISPLAY WS-ASSUR-CODE(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.
       DISPLAY WS-ASSUR-CONTRACT(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.
       DISPLAY WS-ASSUR-PRODUCT(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.

       DISPLAY WS-ASSUR-CLIENT(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.
       DISPLAY WS-ASSUR-STATUS(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.
       MOVE WS-ASSUR-STDATE(WS-ASSUR-IDX)(1:4) TO WS-YEAR.
       MOVE WS-ASSUR-STDATE(WS-ASSUR-IDX)(5:2) TO WS-MONTH.
       MOVE WS-ASSUR-STDATE(WS-ASSUR-IDX)(7:2) TO WS-DAY.
      * DISPLAY WS-ASSUR-STDATE(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.
       DISPLAY WS-DATE-OUTPUT SPACE WITH NO ADVANCING.
       MOVE WS-ASSUR-ENDDATE(WS-ASSUR-IDX)(1:4) TO WS-YEAR.
       MOVE WS-ASSUR-ENDDATE(WS-ASSUR-IDX)(5:2) TO WS-MONTH.
       MOVE WS-ASSUR-ENDDATE(WS-ASSUR-IDX)(7:2) TO WS-DAY.
      * DISPLAY WS-ASSUR-ENDDATE(WS-ASSUR-IDX) SPACE WITH NO ADVANCING.
       DISPLAY WS-DATE-OUTPUT SPACE WITH NO ADVANCING.
       DISPLAY WS-ASSUR-AMOUNT(WS-ASSUR-IDX) SPACE SPACE SPACE WITH NO
                                                              ADVANCING.
       DISPLAY WS-ASSUR-CURRENCY(WS-ASSUR-IDX).

       
      *
      ******************************************************************
      * 
       PARA-DISP-HDR.
       DISPLAY 'CODE    *CONTRACT      *PRODUCT       *'WITH NO
                                                            ADVANCING.
       DISPLAY 'CLIENT                                   *STATUS  *' 
                                                     WITH NO ADVANCING. 
       DISPLAY 'ST DATE   *END DATE  *AMOUNT     *CURRENCY'.
       DISPLAY '---------------------------------------'WITH NO 
                                                            ADVANCING.
       DISPLAY '-----------------------------------------' WITH NO
                                                              ADVANCING.
       DISPLAY '----------------------------------------------------'.
