       IDENTIFICATION DIVISION.
       PROGRAM-ID. assurances.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ASSURANCES ASSIGN TO
                                 'assurances-68259db4e2e6f768575516.csv'
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT RAPPORT-ASSURANCES ASSIGN TO
                           'rapport-assurances.dat'
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

       FD RAPPORT-ASSURANCES.
       01 FD-RAPPORT-ASSUR-REC.
          05 FD-RAPPORT-CODE      PIC X(08).
          05 FILL01               PIC X(01).
          05 FD-RAPPORT-CONTRACT  PIC X(14).
          05 FILL02               PIC x(01).
          05 FD-RAPPORT-PRODUCT   PIC X(14).
          05 FILL03               PIC X(01).
          05 FD-RAPPORT-CLIENT    PIC X(41).
          05 FILL04               PIC x(01).
          05 FD-RAPPORT-STATUS    PIC X(08).
          05 FILL05               PIC x(01).
          05 FD-RAPPORT-STDAY     PIC X(02).
          05 FILL06               PIC X(01).
          05 FD-RAPPORT-STMONTH   PIC X(02).
          05 FILL07               PIC X(01).
          05 FD-RAPPORT-STYEAR    PIC X(02).
          05 FILL08               PIC X(01).
          05 FD-RAPPORT-ENDDAY    PIC X(02).
          05 FILL09               PIC X(01).
          05 FD-RAPPORT-ENDMONTH  PIC X(02).
          05 FILL10               PIC X(01).
          05 FD-RAPPORT-ENDYEAR   PIC X(04).
          05 FILL11               PIC x(01).
          05 FD-RAPPORT-AMOUNT    PIC X(09).
          05 FILL12               PIC X(01).
          05 FD-RAPPORT-CURRENCY  PIC X(03).


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
       77 WS-ASSUR-MAX             PIC 9(03) VALUE 0.
       77 WS-EOF                   PIC X VALUE 'N'.
       77 WS-LINE-NUM              PIC 9(03) VALUE 1.
       77 WS-REC-SAVED             PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
      *Open ASSURANCES file (assurances-68259db4e2e6f768575516.csv). 
       OPEN INPUT ASSURANCES.
      
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
      *Display headrer line
      * PERFORM PARA-DISP-HDR.
      * MOVE 3 TO WS-ASSUR-IDX.
      * PERFORM PARA-DISP-REC.
      * MOVE 7 TO WS-ASSUR-IDX.
      * PERFORM PARA-DISP-REC.

      *Open RAPPORT-ASSURANCES file
       OPEN OUTPUT RAPPORT-ASSURANCES.

      *Initialize FILLxx variables
       MOVE '*' TO FILL01.
       MOVE '*' TO FILL02.
       MOVE '*' TO FILL03.
       MOVE '*' TO FILL04.
       MOVE '*' TO FILL05.
       MOVE '/' TO FILL06.
       MOVE '/' TO FILL07.
       MOVE '*' TO FILL08.
       MOVE '/' TO FILL09.
       MOVE '/' TO FILL10.
       MOVE '*' TO FILL11.
       MOVE '*' TO FILL12.


      *Set WS-LINE-NUM to 1
       MOVE 1 TO WS-LINE-NUM.

      *Start process to write to rapport-assurances file  
       PERFORM UNTIL WS-LINE-NUM = 0
         PERFORM PARA-DISP-HDR
      *Display assurances file   
         PERFORM VARYING WS-ASSUR-IDX FROM 1 BY 1 UNTIL WS-ASSUR-IDX >
                                                            WS-ASSUR-MAX
           PERFORM PARA-DISP-REC
         END-PERFORM
         DISPLAY ' '
         DISPLAY 'Select line number to save ("0" to exit): ' WITH NO
                                                               ADVANCING
         ACCEPT WS-LINE-NUM
      *   
         IF WS-LINE-NUM <> 0 THEN
      *Check if line is empty
            IF WS-ASSUR-CODE(WS-LINE-NUM) = ' ' THEN
               DISPLAY 'WARNING!!! invalid selection, retry. !!!WARNING'
               DISPLAY ' '
            ELSE 
               PERFORM PARA-SAVE-REC
               DISPLAY 'Line number ' WS-LINE-NUM ' saved.'
      *update number of records saved in the repport-assurances file
               ADD 1 TO WS-REC-SAVED
               DISPLAY ' '
            END-IF
       END-PERFORM.

      *Close rapport-assurances file
       CLOSE RAPPORT-ASSURANCES.

      *Display number of records saved and exit 
       DISPLAY 'Process ended. ' WS-REC-SAVED ' records saved'.

       STOP RUN.
      *
      ******************************************************************
      *
       PARA-SAVE-REC.
       MOVE WS-ASSUR-CODE(WS-LINE-NUM) TO FD-RAPPORT-CODE.
       MOVE WS-ASSUR-CONTRACT(WS-LINE-NUM) TO FD-RAPPORT-CONTRACT.
       MOVE WS-ASSUR-PRODUCT(WS-LINE-NUM) TO FD-RAPPORT-PRODUCT.
       MOVE WS-ASSUR-CLIENT(WS-LINE-NUM) TO FD-RAPPORT-CLIENT.
       MOVE WS-ASSUR-STATUS(WS-LINE-NUM) TO FD-RAPPORT-STATUS.
       MOVE WS-ASSUR-STDATE(WS-LINE-NUM)(7:2) TO FD-RAPPORT-STDAY.
       MOVE WS-ASSUR-STDATE(WS-LINE-NUM)(5:2) TO FD-RAPPORT-STMONTH.
       MOVE WS-ASSUR-STDATE(WS-LINE-NUM)(1:4) TO FD-RAPPORT-STYEAR.
       MOVE WS-ASSUR-ENDDATE(WS-LINE-NUM)(7:2) TO FD-RAPPORT-ENDDAY.
       MOVE WS-ASSUR-ENDDATE(WS-LINE-NUM)(5:2) TO FD-RAPPORT-ENDMONTH.
       MOVE WS-ASSUR-ENDDATE(WS-LINE-NUM)(1:4) TO FD-RAPPORT-ENDYEAR.
       MOVE WS-ASSUR-AMOUNT(WS-LINE-NUM) TO FD-RAPPORT-AMOUNT.
       MOVE WS-ASSUR-CURRENCY(WS-LINE-NUM) TO FD-RAPPORT-CURRENCY.
       WRITE FD-RAPPORT-ASSUR-REC.
       
      *
      ******************************************************************
      *
       PARA-DISP-REC.
       DISPLAY WS-ASSUR-IDX SPACE WITH NO ADVANCING.
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
       DISPLAY 'No  CODE    *CONTRACT      *PRODUCT       *'WITH NO
                                                            ADVANCING.
       DISPLAY 'CLIENT                                   *STATUS  *' 
                                                     WITH NO ADVANCING. 
       DISPLAY 'ST DATE   *END DATE  *AMOUNT     *CURRENCY'.
       DISPLAY '--- ---------------------------------------'WITH NO 
                                                            ADVANCING.
       DISPLAY '-----------------------------------------' WITH NO
                                                              ADVANCING.
       DISPLAY '----------------------------------------------------'.
