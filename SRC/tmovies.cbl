       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          MOVIES.
       AUTHOR.              ISAAC GARCIA PEVERI
       DATE-WRITTEN.        2025 JULY 4

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
          COPY 'CPVIDFCV.CPY'.
          COPY 'CPVIDGES.CPY'.

       DATA                 DIVISION.
       FILE                 SECTION.
       FD MOVIES.
          COPY 'CPVIDDAT.CPY'.
      *
       FD GENRES.
          COPY 'CPVIDGEN.CPY'.

       WORKING-STORAGE      SECTION.
               COPY "ACUGUI.DEF".
               COPY "ACUCOBOL.DEF".
               COPY "CRTVARS.DEF".
               COPY "COMUNE.DEF".

      * KEY STATUS
       77 KEY-STATUS IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
          88 ENTER-PUSHED VALUE 13.
          88 EXIT-PUSHED VALUE 27.
          88 MESSAGE-RECEIVED VALUE 95.
          88 EVENT-OCCURRED VALUE 96.
          88 SCREEN-NO-INPUT-FIELD VALUE 97.

      * PROPERTIES & USER DEFINED WORKING STOARGE
       77 FORM1-HANDLE
                  USAGE IS HANDLE OF WINDOW.
       77 FORM1-TB-1-HANDLE
                  USAGE IS HANDLE OF WINDOW.
       77 E-ESC            PIC  9 VALUE 1.
       77 E-NEW            PIC  9 VALUE 1.
       77 E-DELETE         PIC  9 VALUE 1.
       77 E-SAVE           PIC  9 VALUE 1.
       77 E-PREVIEW        PIC  9 VALUE 1.
       77 E-EDIT           PIC  9 VALUE 1.
       77 E-PRINT          PIC  9 VALUE ZERO.
       77 E-SEARCH         PIC  9 VALUE 1.
       77 FORM1-ST-1-HANDLE       USAGE IS HANDLE OF STATUS-BAR.
       77 LOGO-BMP         PIC S9(9) COMP-4.
       77 E-PICK           PIC  9 VALUE 1.
       77 TOOLBAR-BMP      PIC S9(9) COMP-4.
       77 SCREEN1-HANDLE          USAGE IS HANDLE OF WINDOW.
       77 DEFAULT-FONT            USAGE IS HANDLE OF FONT DEFAULT-FONT.
       77 SMALL-FONT              USAGE IS HANDLE OF FONT SMALL-FONT.
       78 TITLEX VALUE IS "FILMOTECA.EDUC360".
       01 SAVE-KEY.
           10 SAVE-COD     PIC  9(5).
       77 STATUS-MOVIES    PIC  X(2).
           88 VALID-STATUS-MOVIES VALUE IS "00" THRU "09".
       77 STATUS-GENRES    PIC  X(2).
           88 VALID-STATUS-GENRES VALUE IS "00" THRU "09".

       77 OLD-MOV-REC PIC X(356).
       77 DECISION PIC 9.


       LINKAGE          SECTION.

       SCREEN           SECTION.
      * FORM
       01
           FORM1,
           .

      * ENTRY FIELD
       05
           EF-CODE,
           ENTRY-FIELD,
           COL 20,00,
           LINE 2,00,
           LINES 1,31 ,
           SIZE 9,00 ,
           BOXED,
           UPPER,
           COLOR 5,
           ENABLED MOD-K,
           ID IS 5001,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MAX-TEXT 5,
           .

      * ENTRY FIELD
       05
           EF-TITLE,
           ENTRY-FIELD,
           COL 20,00,
           LINE 5,00,
           LINES 1,31 ,
           SIZE 60,00 ,
           BOXED,
           COLOR IS 2,
           ENABLED MOD,
           FONT IS SMALL-FONT,
           ID IS 5002
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MAX-TEXT 30,
           .

      * ENTRY FIELD
       05
           EF-GENRE,
           ENTRY-FIELD,
           COL 20,00,
           LINE 7,00,
           LINES 1,31 ,
           SIZE 5,00 ,
           BOXED,
           COLOR IS 2,
           ENABLED MOD,
           FONT IS SMALL-FONT,
           ID IS 5003,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MAX-TEXT 30,
           .

      * LABEL
       05
           LBL-GENRE,
           LABEL,
           COL 30,00,
           LINE 7,00,
           SIZE 33,00 ,
           ID IS 192,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "<NOT APPLICABLE>",
           COLOR 5
           .

      * ENTRY FIELD
       05
           EF-NOTE,
           ENTRY-FIELD,
           COL 20,00,
           LINE 19,00,
           LINES 1,31 ,
           SIZE 60,00 ,
           BOXED,
           COLOR IS 2,
           ENABLED MOD,
           FONT IS SMALL-FONT,
           ID IS 5013,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MAX-TEXT 30,
           .

      * LABEL
       05
           LBL-LOGO,
           LABEL,
           COL 3,00,
           LINE 21,00,
           SIZE 33,00 ,
           ID IS 192,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "LOGO",
           .

      * ENTRY FIELD
       05
           EF-LOGO,
           ENTRY-FIELD,
           COL 20,00,
           LINE 21,00,
           LINES 1,31 ,
           SIZE 59,00 ,
           BOXED,
           COLOR IS 2,
           ENABLED MOD,
           FONT IS SMALL-FONT,
           ID IS 5013,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           .


      * PUSH BUTTON
       05
           PB-LOGO,
           PUSH-BUTTON,
           COL 80,00,
           LINE 21,00,
           LINES 1,31 ,
           SIZE 3,00 ,
           EXCEPTION-VALUE 1003,
           FONT IS SMALL-FONT,
           ID IS 5005,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "...",
           .

      * LABEL
       05
           FORM1-LA-5AA,
           LABEL,
           COL 3,00,
           LINE 2,00,
           LINES 1,31 ,
           SIZE 13,00 ,
           ID IS 102,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "CODE",
           .

      * BAR
       05
           FORM1-BR-1,
           BAR,
           COL 1,00,
           LINE 3,92,
           SIZE 83,50 ,
           ID IS 4,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           COLORS (8, 8),
           SHADING (-1, 1),
           WIDTH 2,
           .

      * LABEL
       05
           FORM1-LA-1,
           LABEL,
           COL 3,00,
           LINE 5,00,
           LINES 1,31 ,
           SIZE 13,00 ,
           FONT IS SMALL-FONT,
           ID IS 1,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "TITLE",
           .

      * LABEL
       05
           FORM1-LA-3,
           LABEL,
           COL 3,00,
           LINE 7,00,
           LINES 1,31 ,
           SIZE 13,00 ,
           FONT IS SMALL-FONT,
           ID IS 8,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "GENRE",
           .

      * BITMAP CONTROLLER
       05  MOVIEBMP,
           BITMAP,
           LINE 09 COL 55
           LINES 110 SIZE 150
           BITMAP-HANDLE LOGO-BMP.

      * LABEL
       05
           FORM1-LA-4,
           LABEL,
           COL 3,00,
           LINE 19,00,
           LINES 1,31 ,
           SIZE 13,00 ,
           FONT IS SMALL-FONT,
           ID IS 10,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "NOTES",
           .

      * TOOLBAR
       01
           FORM1-TB-1,
           .

      * PUSH BUTTON
       05
           TOOL-ESC,
           PUSH-BUTTON,
           COL 1,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 1,
           FRAMED,
           SQUARE,
           ENABLED E-ESC,
           EXCEPTION-VALUE 27,
           FLAT,
           ID IS 2223,
           SELF-ACT,
           ESCAPE-BUTTON,
           TITLE "EXIT (ESC)",
           .

      * PUSH BUTTON
       05
           TOOL-NEW,
           PUSH-BUTTON,
           COL 6,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 2,
           FRAMED,
           SQUARE,
           ENABLED E-NEW,
           EXCEPTION-VALUE 2,
           FLAT,
           ID IS 208,
           SELF-ACT,
           TITLE "NEW (F2)",
           .

      * PUSH BUTTON
       05
           TOOL-DELETE,
           PUSH-BUTTON,
           COL 16,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 4,
           FRAMED,
           SQUARE,
           ENABLED E-DELETE,
           EXCEPTION-VALUE 4,
           FLAT,
           ID IS 106,
           SELF-ACT,
           TITLE "DELETE (F4)",
           .

      * PUSH BUTTON
       05
           TOOL-SAVE,
           PUSH-BUTTON,
           COL 11,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 3,
           FRAMED,
           SQUARE,
           ENABLED E-SAVE,
           EXCEPTION-VALUE 3,
           FLAT,
           ID IS 75,
           SELF-ACT,
           TITLE "SAVE (F3)",
           .

      * PUSH BUTTON
       05
           TOOL-PREVIEW,
           PUSH-BUTTON,
           COL 26,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 6,
           FRAMED,
           SQUARE,
           ENABLED E-PREVIEW,
           EXCEPTION-VALUE 6,
           FLAT,
           ID IS 76,
           SELF-ACT,
           TITLE "PREVIEW (F6)",
           .

      * CHECK BOX
       05
           TOOL-EDIT,
           CHECK-BOX,
           COL 21,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 5,
           FRAMED,
           SQUARE,
           ENABLED E-EDIT,
           EXCEPTION-VALUE 150
           FLAT,
           ID IS 77,
           SELF-ACT,
           TITLE "EDIT (F5)",
           VALUE MOD,
           .
      * PUSH BUTTON
       05
           TOOL-PRINT,
           PUSH-BUTTON,
           COL 31,00,
           LINE 1,15,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           FRAMED,
           SQUARE,
           ENABLED E-PRINT,
           EXCEPTION-VALUE 7,
           FLAT,
           ID IS 110,
           SELF-ACT,
           TITLE "PRINT (F7)",
           BITMAP-NUMBER 7
           .

      * PUSH BUTTON
       05
           TOOL-SEARCH,,
           PUSH-BUTTON,
           COL 36,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           FRAMED,
           SQUARE,
           ENABLED E-SEARCH,
           EXCEPTION-VALUE 8,
           FLAT,
           ID IS 111,
           SELF-ACT,
           TITLE "SEARCH (F8)",
           BITMAP-NUMBER 8
           .

      * PUSH BUTTON
       05
           TOOL-PICK,
           PUSH-BUTTON,
           COL 41,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 9,
           FRAMED,
           SQUARE,
           ENABLED E-PICK,
           EXCEPTION-VALUE 9,
           FLAT,
           ID IS 112,
           SELF-ACT,
           TITLE "PICK (F9)",
           .

      * PUSH BUTTON
       05
           FORM1-PB-1A,
           PUSH-BUTTON,
           COL 46,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 10,
           FRAMED,
           SQUARE,
           EXCEPTION-VALUE 1002,
           FLAT,
           ID IS 113,
           SELF-ACT,
           TITLE "&FIRST",
           .

      * PUSH BUTTON
       05
           FORM1-PB-1B,
           PUSH-BUTTON,
           COL 51,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 11,
           FRAMED,
           SQUARE,
           EXCEPTION-VALUE 67,
           FLAT,
           ID IS 114,
           SELF-ACT,
           TITLE "PREVIOUS (PGDN)",
           .

      * PUSH BUTTON
       05
           FORM1-PB-1C,
           PUSH-BUTTON,
           COL 56,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 12,
           FRAMED,
           SQUARE,
           EXCEPTION-VALUE 68,
           FLAT,
           ID IS 115,
           SELF-ACT,
           TITLE "NEXT (PGUP)",
           .

      * PUSH BUTTON
       05
           FORM1-PB-1D,
           PUSH-BUTTON,
           COL 61,00,
           LINE 1,08,
           LINES 23,00 ,
           SIZE 24,00 ,
           BITMAP-HANDLE TOOLBAR-BMP,
           BITMAP-NUMBER 13,
           FRAMED,
           SQUARE,
           EXCEPTION-VALUE 1006,
           FLAT,
           ID IS 116,
           SELF-ACT,
           TITLE "&LAST",
           .

       PROCEDURE  DIVISION.
       DECLARATIVES.
      /
      *----------------------------------------------------------------*
      * DECLARATIVES TO HABDLE COMMON FILE ERRORS                      *
      *----------------------------------------------------------------*
       MOVIES-ERR SECTION.
           USE AFTER ERROR PROCEDURE ON MOVIES.
           SET RECLOCKED TO FALSE.
           EVALUATE STATUS-MOVIES
           WHEN "35"
                DISPLAY MESSAGE "FILE [MOVIES] NOT FOUND!"
                           TITLE TITLEX
                            ICON 3
                SET ERRORI TO TRUE
           WHEN "39"
                DISPLAY MESSAGE "FILE [MOVIES] MISMATCH SIZE!"
                           TITLE TITLEX
                            ICON 3
                SET ERRORI TO TRUE
           WHEN "98"
                DISPLAY MESSAGE "[MOVIES] INDEXED FILE CORRUPT!"
                           TITLE TITLEX
                            ICON 3
                SET ERRORI TO TRUE
           WHEN "93"
                DISPLAY MESSAGE BOX "FILE ALREADY IN USE!"
                          TITLE TIT-ERR
                           ICON 3
                GOBACK
           WHEN "23"
                MOVE 1 TO MOD-K
                MOVE 0 TO MOD
                MODIFY TOOL-EDIT, VALUE = MOD
                SET STATUSVIEW  TO TRUE
                PERFORM STATUS-BAR-MSG

                IF NEW-REC
                   INITIALIZE MOV-REC OF MOVIES
                              REPLACING NUMERIC DATA BY ZEROES
                                   ALPHANUMERIC DATA BY SPACES
      *             PERFORM FORM1-FLD-TO-BUF
                   DISPLAY FORM1-TB-1
                   SET OLD-REC    TO TRUE
                END-IF
      *          PERFORM ABILITAZIONI

                DISPLAY FORM1
                MOVE "23" TO STATUS-MOVIES
           END-EVALUATE.

       INPUT-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON INPUT.
       0100-DECL.
           EXIT.
       I-O-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON I-O.
       0200-DECL.
           EXIT.
       OUTPUT-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON OUTPUT.
       0300-DECL.
           EXIT.
       TRANSACTION-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON TRANSACTION.
       0400-DECL.
           EXIT.
       END DECLARATIVES.
      /
      *----------------------------------------------------------------*
      * MAIN                                                           *
      *----------------------------------------------------------------*
       MAIN-LOGIC.
           PERFORM INITIALIZE-ROUTINE
           PERFORM FORM1-OPEN-ROUTINE
           .
      /
      *----------------------------------------------------------------*
      * INITIALIZE                                                     *
      *----------------------------------------------------------------*
       INITIALIZE-ROUTINE.
           ACCEPT SYSTEM-INFORMATION FROM SYSTEM-INFO.
           ACCEPT TERMINAL-ABILITIES FROM TERMINAL-INFO.
           PERFORM INIT-BMP.
           PERFORM OPEN-FILE-RTN.
           .
      /
      *----------------------------------------------------------------*
      * INITIALIZE BITMAPS                                             *
      *----------------------------------------------------------------*
       INIT-BMP.
           COPY RESOURCE "..\RESOURCE\TOOLBAR.BMP".
           CALL "W$BITMAP" USING WBITMAP-LOAD "..\RESOURCE\TOOLBAR.BMP",
                   GIVING TOOLBAR-BMP.
           .
      /
      *----------------------------------------------------------------*
      * MAIN OPENING FILE ROUTINE                                      *
      *----------------------------------------------------------------*
       OPEN-FILE-RTN.
           OPEN  I-O MOVIES
           IF STATUS-MOVIES = "35"
              OPEN OUTPUT MOVIES
                IF VALID-STATUS-MOVIES
                   CLOSE MOVIES
                   OPEN I-O MOVIES
                END-IF
           END-IF
      *
           IF NOT VALID-STATUS-MOVIES
      *        PERFORM  FORM1-EXTENDED-FILE-STATUS
               PERFORM EXIT-STOP-ROUTINE
           END-IF

           OPEN INPUT GENRES

           IF STATUS-GENRES = "35"
              OPEN OUTPUT GENRES
                IF VALID-STATUS-GENRES
                   CLOSE GENRES
                   OPEN I-O GENRES
                END-IF
           END-IF
      *
           IF NOT VALID-STATUS-GENRES
      *        PERFORM  FORM1-EXTENDED-FILE-STATUS
               PERFORM EXIT-STOP-ROUTINE
           END-IF
           .
      /
      *----------------------------------------------------------------*
      * MAIN CLOSING FILE ROUTINE                                      *
      *----------------------------------------------------------------*
       CLOSE-FILE-RTN.
           CLOSE MOVIES
           CLOSE GENRES
           .
      /
      *----------------------------------------------------------------*
      * SCREEN ROUTINES                                                *
      *----------------------------------------------------------------*
       FORM1-OPEN-ROUTINE.
           PERFORM FORM1-SCRN
           PERFORM FORM1-PROC
           .

       FORM1-SCRN.
           PERFORM FORM1-CREATE-WIN
      *     DISPLAY FORM1-TB-1
      *     PERFORM FORM1-DISPLAY
           MODIFY MOVIEBMP BITMAP-HANDLE LOGO-BMP
           .

       FORM1-CREATE-WIN.
           DISPLAY STANDARD GRAPHICAL WINDOW
              LINES 23,62,
              SIZE 83,50,
              COLOR 65793,
              CONTROL FONT SMALL-FONT,
              CONTROLS-UNCROPPED,
              LABEL-OFFSET 23,
              LINK TO THREAD,
              MODELESS,
              NO SCROLL,
              TITLE-BAR,
              TITLE TITLEX,
              AUTO-MINIMIZE,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              NO WRAP,
              HANDLE IS FORM1-HANDLE,

      * TOOL BAR
           DISPLAY TOOL-BAR
              LINES 2,69,
              HANDLE IN FORM1-TB-1-HANDLE
           DISPLAY FORM1-TB-1 UPON FORM1-TB-1-HANDLE

      * STATUS-BAR
            DISPLAY STATUS-BAR
               GRIP,
               PANEL-WIDTHS (42, 23, 999),
               PANEL-STYLE  (1, 1, 1),
               PANEL-TEXT   (SPACE, SPACE, SPACE),
               HANDLE IS FORM1-ST-1-HANDLE
           DISPLAY FORM1 UPON FORM1-HANDLE

           PERFORM START-MOVIES
           PERFORM FROMREC-TOSCREEN
 
           MOVE ZERO TO MOD, MOD-K
           DISPLAY FORM1

           MODIFY TOOL-EDIT,   VALUE   MOD
           MODIFY TOOL-DELETE, ENABLED MOD
           MODIFY TOOL-SEARCH  ENABLED MOD
           .

       FORM1-PROC.
           SET STATUSVIEW  TO TRUE.
           PERFORM STATUS-BAR-MSG

           PERFORM UNTIL EXIT-PUSHED
              ACCEPT FORM1
                 ON EXCEPTION
                    PERFORM FORM1-EVALUATE-FUNC
              END-ACCEPT
           END-PERFORM

           DESTROY FORM1-HANDLE
           INITIALIZE KEY-STATUS
           .

       FORM1-EVALUATE-FUNC.
           EVALUATE KEY-STATUS
           WHEN 150 *> THE EDIT FUNCTION
                INQUIRE TOOL-EDIT, ENABLED IN E-EDIT

                IF E-EDIT = 1
                   IF MOD = 0
                      MOVE 1 TO MOD
                      SET STATUSEDIT     TO TRUE
                      MOVE 1 TO E-DELETE, E-PICK
                   ELSE
                      MOVE 0 TO MOD
                      MOVE 0 TO E-DELETE, E-PICK 
                      SET STATUSVIEW     TO TRUE
                   END-IF

                   MODIFY TOOL-EDIT,   VALUE   MOD
                   MODIFY TOOL-DELETE, ENABLED MOD
                   MODIFY TOOL-SEARCH  ENABLED MOD
 
                   PERFORM STATUS-BAR-MSG
                   DISPLAY FORM1
                END-IF
           END-EVALUATE.

           .
           EVALUATE TRUE
              WHEN EXIT-PUSHED
                 PERFORM FORM1-EXIT
              WHEN EVENT-OCCURRED
                 IF EVENT-TYPE = CMD-CLOSE
                    PERFORM FORM1-EXIT
                 END-IF
      *        WHEN KEY-STATUS = 1003
      *           PERFORM PB-FOTO-LINKTO
               WHEN KEY-STATUS = 2
                  PERFORM NEW-ENTRY
      *        WHEN KEY-STATUS = 4
      *           PERFORM CANCELLA-LINKTO
               WHEN KEY-STATUS = 3
                  PERFORM SAVE-ENTRY
      *        WHEN KEY-STATUS = 6
      *           PERFORM ANTEPRIMA-LINKTO
      *        WHEN KEY-STATUS = 150
      *           PERFORM TOOL-MODIFICA-LINKTO
      *        WHEN KEY-STATUS = 7
      *           PERFORM STAMPA-LINKTO
      *        WHEN KEY-STATUS = 8
      *           PERFORM TOOL-CERCA-LINKTO
      *        WHEN KEY-STATUS = 9
      *           PERFORM TOOL-SELEZIONA-LINKTO
      *        WHEN KEY-STATUS = 1002
      *           PERFORM TOOL-PRIMO
      *        WHEN KEY-STATUS = 67
      *           PERFORM TOOL-PRECEDENTE
      *        WHEN KEY-STATUS = 68
      *           PERFORM TOOL-SUCCESSIVO
      *        WHEN KEY-STATUS = 1006
      *           PERFORM TOOL-ULTIMO
           END-EVALUATE

      * AVOID CHANGING FOCUS
           MOVE 4 TO ACCEPT-CONTROL
           .

       FORM1-CLEAR.
           PERFORM FORM1-DISPLAY
           .

       FORM1-DISPLAY.
           DISPLAY FORM1-TB-1
           DISPLAY FORM1 UPON FORM1-HANDLE
           .

       FORM1-EXIT.
           PERFORM SAVE-CHANGES-ROUTINE.

           IF ERRORI
              MOVE 26 TO KEY-STATUS
              EXIT PARAGRAPH
           END-IF.

           MOVE 27 TO KEY-STATUS
           .
      /
      *----------------------------------------------------------------*
      * ADD A NEW ENTRY TO THE FILE                                    *
      *----------------------------------------------------------------*
       NEW-ENTRY.
           PERFORM START-MOVIES

           READ MOVIES
                INVALID KEY
                        CONTINUE
                NOT INVALID KEY ADD 1 TO CODIGO
           END-READ

           MOVE SPACES TO TITULO,
                          GENERO,
                          NOTA,
                          IMAGEN

           DISPLAY FORM1
           PERFORM FROMREC-TOSCREEN

           MOVE 5002   TO CONTROL-ID
           MOVE 4      TO ACCEPT-CONTROL
           .
      /
      *----------------------------------------------------------------*
      * SAVE THE CURRENT RECORD FROM THE SCREEN TO THE FILE            *
      *----------------------------------------------------------------*
       SAVE-ENTRY.
           PERFORM FROMSCREEN-TORECORD

           WRITE MOV-REC
                 INVALID KEY
                         DISPLAY MESSAGE BOX
                            "Save changes to the current Entry?"
                            TITLE   TITLEX
                            TYPE    MB-YES-NO
                            DEFAULT MB-NO
                            GIVING  DECISION

                         IF DECISION = MB-YES
                            REWRITE MOV-REC
                               INVALID KEY
                                   DISPLAY MESSAGE BOX
                                    "Error during REWRITE"
                                    TITLE   TITLEX
                         END-IF
           END-WRITE
           .
      /
      *----------------------------------------------------------------*
      * POSITION ON THE FIRST FREE RECORD                              *
      *----------------------------------------------------------------*
       START-MOVIES.
           INITIALIZE CODIGO

           MOVE HIGH-VALUES      TO CODIGO

           START MOVIES  KEY LESS CODIGO
                 INVALID KEY
                     MOVE 1      TO CODIGO
                 NOT INVALID KEY
                     READ MOVIES PREVIOUS RECORD
           END-START
           .
      /
      *----------------------------------------------------------------*
      * PUT THE DATA FROM THE FILE INTO SCREEN FIELDS                  *
      *----------------------------------------------------------------*
       FROMREC-TOSCREEN.
           MOVE ZERO       TO MOD-K
           MOVE 1          TO MOD

           MODIFY EF-CODE  VALUE CODIGO
           MODIFY EF-TITLE VALUE TITULO
           MODIFY EF-GENRE VALUE GENERO
           MODIFY EF-NOTE  VALUE NOTA
           MODIFY EF-LOGO  VALUE IMAGEN

           CALL "W$BITMAP"
                USING WBITMAP-LOAD
                       IMAGEN
                GIVING LOGO-BMP

           MODIFY MOVIEBMP BITMAP-HANDLE LOGO-BMP
           DISPLAY FORM1

           CALL "W$BITMAP"
                USING WBITMAP-DESTROY
                      LOGO-BMP
           .
      /
      *----------------------------------------------------------------*
      * PUT THE DATA FROM THE SCRERN INTO THE FILE                    *
      *----------------------------------------------------------------*
       FROMSCREEN-TORECORD.
           INQUIRE EF-CODE    VALUE CODIGO
           INQUIRE EF-TITLE   VALUE TITULO
           INQUIRE EF-GENRE   VALUE GENERO
           INQUIRE EF-NOTE    VALUE NOTA
           INQUIRE EF-LOGO    VALUE IMAGEN
           .
      /
      *----------------------------------------------------------------*
      * SAVE CHANGES                                                   *
      *----------------------------------------------------------------*
       SAVE-CHANGES-ROUTINE.
           .
      /
      *----------------------------------------------------------------*
      * EXIT PROGRAM                                                   *
      *----------------------------------------------------------------*
       EXIT-STOP-ROUTINE.
           PERFORM CLOSE-FILE-RTN
           CALL "W$BITMAP" USING WBITMAP-DESTROY, TOOLBAR-BMP
           STOP RUN
           .

       COPYS.
           copy "status.cpy".
           .
