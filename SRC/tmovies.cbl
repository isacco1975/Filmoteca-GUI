       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          MOVIES.
       AUTHOR.              ISAAC GARCIA PEVERI
       DATE-WRITTEN.        2025 JULY 4
      /
      ******************************************************************
      *                                                                *
      *  FORMERLY FILMOTECA.EDUC360 GUI                                *
      *           GUI C. 2025 BY ISAAC GARCIA PEVERI (IGP TECH BLOG)   *
      *           OLD C. 2025 BY FABIO MARQUES                         *
      *                                                                *
      *          THIS IS THE GRAPHICAL VERSION OG THE CHARACTER BASED  *
      *          PTOGRAM, ORIGINALLY DEVELOPED BY FABIO MARQUES WITH   *
      *          GNU COBOL (FORMERLY OPEN COBOL), PORTED TO ACUSOBOL   *
      *          WITH CHARACTER BASED BY ISAAC GARCIA PEVERI.          *
      *                                                                *
      *          ALL RIGHTS RESERVED                                   *
      *                                                                *
      ******************************************************************
      *                                                                *
      *          OTHER CHANGES FROM OLD CHARACTER VERSION:             *
      *                - ENTIRELY REWRITTEN FROM SCRATCH WITH GUI      *
      *                - GENRES FILE IS ANOTHER INDEXED FILE           *
      *                                                                *
      ******************************************************************
      *                                                                *
      *          THIS SOFTWARE IS FOR EDUCATIONAL PURPOSES ONLY        *
      *          AND USES THIRD PARTY LIBRARIES (XTREME ZOOM)          *
      *                                                                *
      ******************************************************************
      /
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
               COPY "OPENSAVE.DEF".
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
       77 EF-GEN-BUF PIC 9(2) VALUE ZERO.
       77 DECISION PIC 9.

       01 OLD-MOV-REC.
          05 OLD-CODIGO                PIC 9(05).
          05 OLD-TITULO                PIC X(30).
          05 OLD-GENERO                PIC X(02).
          05 OLD-DURACAO               PIC 9(03).
          05 OLD-DISTRIB               PIC X(15).
          05 OLD-NOTA                  PIC 9(02).
          05 OLD-FILLER                PIC X(37).
          05 OLD-IMAGEN                PIC X(256).

       77  OPENSAVE-DATA-SIZE    PIC 9(4) BINARY.
       77  OPENSAVE-DATA-ADDR    POINTER.
       77  OPENSAVE-STATUS       PIC S99.
           88  OPENSAVE-OK       VALUE 1.
       77  FILE-NAME             PIC X(256).

       LINKAGE          SECTION.

       SCREEN           SECTION.
               COPY "SCNVIDLST.CPY".

       PROCEDURE  DIVISION.
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
      * SCREEN ROUTINES - WINDOW CREATION AND DISPLAY                  *
      *----------------------------------------------------------------*
       FORM1-OPEN-ROUTINE.
           PERFORM FORM1-CREATE-WINDOW
           PERFORM FORM1-PROC
           .

       FORM1-CREATE-WINDOW.
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

           INITIALIZE OLD-MOV-REC
                      REPLACING NUMERIC DATA BY ZERO
                           ALPHANUMERIC DATA BY SPACES

           DISPLAY FORM1
           PERFORM FIRST-ENTRY
           MOVE MOV-REC TO OLD-MOV-REC

           MOVE ZERO TO MOD
           MOVE 1    TO MOD-K

           MODIFY TOOL-EDIT,    VALUE   MOD
           MODIFY TOOL-NEW      ENABLED MOD
           MODIFY TOOL-DELETE,  ENABLED MOD
           MODIFY TOOL-SEARCH   ENABLED MOD
           MODIFY TOOL-SAVE     ENABLED MOD
           MODIFY PB-LOGO       ENABLED MOD
           MODIFY PB-GENRE-ZOOM ENABLED MOD
           .
      /
      *----------------------------------------------------------------*
      * ACCEPT SCREEN                                                  *
      *----------------------------------------------------------------*
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
      /
      *----------------------------------------------------------------*
      * EVALUATE KEY PRESSED ON THE SCREEN (BUTTONS)                   *
      *----------------------------------------------------------------*
       FORM1-EVALUATE-FUNC.
           EVALUATE KEY-STATUS
           WHEN 150 *> THE EDIT FUNCTION
                INQUIRE TOOL-EDIT, ENABLED IN E-EDIT

                IF E-EDIT = 1
                   IF MOD = 0
                      MOVE 1 TO MOD
                      SET STATUSEDIT     TO TRUE
                      MOVE 0 TO MOD-K
                   ELSE
                      MOVE 0 TO MOD
                      MOVE 1 TO MOD-K
                      SET STATUSVIEW     TO TRUE
                   END-IF

                   MODIFY TOOL-NEW      ENABLED MOD
                   MODIFY TOOL-EDIT,    VALUE   MOD
                   MODIFY TOOL-DELETE,  ENABLED MOD
                   MODIFY TOOL-SEARCH   ENABLED MOD
                   MODIFY TOOL-SAVE     ENABLED MOD
                   MODIFY PB-LOGO       ENABLED MOD
                   MODIFY PB-GENRE-ZOOM ENABLED MOD

                   DISPLAY FORM1
                   PERFORM STATUS-BAR-MSG
                   PERFORM READ-GENRE
                   PERFORM CHECK-CHANGES
                END-IF
           END-EVALUATE.

           EVALUATE TRUE
              WHEN EXIT-PUSHED
                 PERFORM FORM1-EXIT
              WHEN EVENT-OCCURRED
                 IF EVENT-TYPE = CMD-CLOSE
                    PERFORM FORM1-EXIT
                 END-IF
               WHEN KEY-STATUS = 2
                  PERFORM NEW-ENTRY
               WHEN KEY-STATUS = 3
                  PERFORM SAVE-ENTRY
               WHEN KEY-STATUS = 4
                  PERFORM DELETE-ENTRY
               WHEN KEY-STATUS = 8
                  PERFORM ZOOM-ENTRIES
               WHEN KEY-STATUS = 1002
                  PERFORM FIRST-ENTRY
               WHEN KEY-STATUS = 1003
                  PERFORM ZOOM-LOGO
               WHEN KEY-STATUS = 67
                  PERFORM PREV-ENTRY
               WHEN KEY-STATUS = 68
                  PERFORM NEXT-ENTRY
               WHEN KEY-STATUS = 1006
                  PERFORM LAST-ENTRY
               WHEN KEY-STATUS = 2003
                  PERFORM ZOOM-GENRES
           END-EVALUATE

      * AVOID CHANGING FOCUS
           MOVE 4 TO ACCEPT-CONTROL
           .
      /
      *----------------------------------------------------------------*
      * SCREEN RESET                                                   *
      *----------------------------------------------------------------*
       FORM1-CLEAR.
           PERFORM FORM1-DISPLAY
           .

       FORM1-DISPLAY.
           DISPLAY FORM1-TB-1
           DISPLAY FORM1 UPON FORM1-HANDLE
           .
      /
      *----------------------------------------------------------------*
      * ADD A NEW ENTRY TO THE FILE                                    *
      *----------------------------------------------------------------*
       NEW-ENTRY.
           PERFORM START-MOVIES-GREATER

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
                     PERFORM CHECK-CHANGES
                     PERFORM SAVE-CHANGES
           END-WRITE
           .
      /
      *----------------------------------------------------------------*
      * POSITION ON THE FIRST FREE RECORD                              *
      *----------------------------------------------------------------*
       START-MOVIES-LESS.
           INITIALIZE CODIGO

           MOVE LOW-VALUES      TO CODIGO

           START MOVIES  KEY >= CODIGO
                 INVALID KEY
                     MOVE 1      TO CODIGO
                 NOT INVALID KEY
                     READ MOVIES NEXT RECORD
           END-START
           .
      /
      *----------------------------------------------------------------*
      * POSITION ON THE LAST  FREE RECORD                              *
      *----------------------------------------------------------------*
       START-MOVIES-GREATER.
           INITIALIZE CODIGO

           MOVE HIGH-VALUES      TO CODIGO

           START MOVIES  KEY <= CODIGO
                 INVALID KEY
                     MOVE 1      TO CODIGO
                 NOT INVALID KEY
                     READ MOVIES PREVIOUS RECORD
           END-START
           .
      /
      *----------------------------------------------------------------*
      * SHOW FIRST RECORD                                              *
      *----------------------------------------------------------------*
       FIRST-ENTRY.
           PERFORM CHECK-CHANGES
           PERFORM START-MOVIES-LESS
           PERFORM FROMREC-TOSCREEN
           .
      /
      *----------------------------------------------------------------*
      * SHOW NEXT RECORD                                               *
      *----------------------------------------------------------------*
       NEXT-ENTRY.
           PERFORM CHECK-CHANGES

           READ MOVIES NEXT
                AT END
                   DISPLAY MESSAGE "Reached the End of File"
                   TITLE TITLEX
                NOT AT END
                   PERFORM FROMREC-TOSCREEN
           END-READ
           .
      /
      *----------------------------------------------------------------*
      * SHOW PREVIOUS RECORD                                           *
      *----------------------------------------------------------------*
       PREV-ENTRY.
           PERFORM CHECK-CHANGES

           READ MOVIES PREVIOUS
                AT END
                   DISPLAY MESSAGE "Reached the Beginning of File"
                   TITLE TITLEX
                NOT AT END
                   PERFORM FROMREC-TOSCREEN
           END-READ
           .
      /
      *----------------------------------------------------------------*
      * SHOW LAST  RECORD                                              *
      *----------------------------------------------------------------*
       LAST-ENTRY.
           PERFORM CHECK-CHANGES
           PERFORM START-MOVIES-GREATER
           PERFORM FROMREC-TOSCREEN
           .
      /
      *----------------------------------------------------------------*
      * PUT THE DATA FROM THE FILE INTO SCREEN FIELDS                  *
      *----------------------------------------------------------------*
       FROMREC-TOSCREEN.
           MOVE ZERO       TO MOD-K
           MOVE 1          TO MOD
           MOVE MOV-REC    TO OLD-MOV-REC

           MODIFY EF-CODE      VALUE CODIGO
           MODIFY EF-TITLE     VALUE TITULO
           MODIFY EF-GENRE     VALUE GENERO
           MODIFY EF-GRADE     VALUE NOTA
           MODIFY EF-LOGO      VALUE IMAGEN
           MODIFY EF-DISTRIB   VALUE DISTRIB
           MODIFY EF-DURATION  VALUE DURACAO

           INQUIRE EF-GENRE    VALUE IN CODIGO-GEN

           IF EF-GEN-BUF NOT = ZERO
              MOVE EF-GEN-BUF TO CODIGO-GEN
           END-IF

           PERFORM READ-GENRE

           CALL "W$BITMAP"
                USING  WBITMAP-LOAD
                       IMAGEN
                GIVING LOGO-BMP

           MODIFY MOVIEBMP BITMAP-HANDLE LOGO-BMP

           CALL "W$BITMAP"
                USING WBITMAP-DESTROY
                      LOGO-BMP
           .
      /
      *----------------------------------------------------------------*
      * PUT THE DATA FROM THE SCRERN INTO THE FILE                    *
      *----------------------------------------------------------------*
       FROMSCREEN-TORECORD.
           INQUIRE EF-CODE     VALUE CODIGO
           INQUIRE EF-TITLE    VALUE TITULO
           INQUIRE EF-GENRE    VALUE GENERO
           INQUIRE EF-DISTRIB  VALUE DISTRIB
           INQUIRE EF-DURATION VALUE DURACAO
           INQUIRE EF-GRADE    VALUE NOTA
           INQUIRE EF-LOGO     VALUE IMAGEN
           .
      /
      *----------------------------------------------------------------*
      * RETRIEVE THE GENRE FROM CODE                                   *
      *----------------------------------------------------------------*
       READ-GENRE.
           IF EF-GEN-BUF = ZERO
              INQUIRE EF-GENRE VALUE IN CODIGO-GEN
           END-IF

           READ GENRES
                INVALID MODIFY LBL-GENRE-DES TITLE  '<NOT APPLICABLE>'
                NOT INVALID
                        MODIFY LBL-GENRE-DES TITLE DESC-GEN
                        MODIFY EF-GENRE   VALUE    CODIGO-GEN
                        MOVE ZERO TO EF-GEN-BUF
           END-READ
           .
      /
      *----------------------------------------------------------------*
      * DELETE ENTRY                                                    *
      *----------------------------------------------------------------*
       DELETE-ENTRY.
           INQUIRE EF-CODE VALUE IN CODIGO
           READ MOVIES
                INVALID DISPLAY MESSAGE BOX
                        "This Entry does not exist!"
                NOT INVALID
                         DISPLAY MESSAGE BOX
                            "Are You SURE? DELETE current Entry?"
                            TITLE   TITLEX
                            TYPE    MB-YES-NO
                            DEFAULT MB-NO
                            GIVING  DECISION

                         IF DECISION = MB-YES
                            DELETE  MOVIES RECORD
                            DISPLAY MESSAGE "Deleted!"
                            PERFORM FIRST-ENTRY
                         END-IF
           END-READ
           .
      /
      *----------------------------------------------------------------*
      * CHECK CHANGES                                                  *
      *----------------------------------------------------------------*
       CHECK-CHANGES.
           PERFORM FROMSCREEN-TORECORD

           IF MOV-REC NOT = OLD-MOV-REC
                 PERFORM SAVE-CHANGES
           END-IF
           .
      /
      *----------------------------------------------------------------*
      * WRITES THE ENTRY TO THE FILE                                   *
      *----------------------------------------------------------------*
       SAVE-CHANGES.
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
           .
      /
      *----------------------------------------------------------------*
      * CALL TO XZOOM  FOR FILE MOVIES                                 *
      *----------------------------------------------------------------*
       ZOOM-ENTRIES.
           EVALUATE CONTROL-ID
           WHEN 5001
           WHEN 5002
                MOVE "MOVIES.DAT"  TO COMO-FILE
                INQUIRE EF-CODE, VALUE IN CODIGO
                CALL "ZOOM-GT"  USING COMO-FILE, MOV-REC
                                GIVING STATO-ZOOM
                END-CALL
                CANCEL "ZOOM-GT"

                IF STATO-ZOOM = 0
                   PERFORM FROMREC-TOSCREEN
                END-IF
           END-EVALUATE
           .
      /
      *----------------------------------------------------------------*
      * CALL TO XZOOM  FOR FILE GENRES                                 *
      *----------------------------------------------------------------*
       ZOOM-GENRES.
           MOVE "GENRE.DAT"  TO COMO-FILE
           INQUIRE EF-GENRE VALUE IN CODIGO-GEN
           CALL "ZOOM-GT"  USING COMO-FILE, REG-GEN
                          GIVING STATO-ZOOM
           END-CALL
           CANCEL "ZOOM-GT"
           MOVE ZERO TO EF-GEN-BUF

           IF STATO-ZOOM = 0
              MOVE CODIGO-GEN TO EF-GEN-BUF
              PERFORM FROMREC-TOSCREEN
           END-IF
           .
      /
      *----------------------------------------------------------------*
      * OPEN AN IMAGE FILE                                             *
      *----------------------------------------------------------------*
       ZOOM-LOGO.
           INITIALIZE                 OPENSAVE-DATA

           MOVE "PICK..."                 TO OPNSAV-TITLE
           MOVE "*.JPG"                   TO OPNSAV-FILTERS
                                              OPNSAV-DEFAULT-FILTER
      *
           CALL "C$OPENSAVEBOX" USING OPENSAVE-OPEN-BOX
                                      OPENSAVE-DATA
                               GIVING OPENSAVE-STATUS

           IF OPENSAVE-STATUS NOT = 1
              INITIALIZE               OPENSAVE-DATA
              MODIFY EF-LOGO     VALUE SPACES
           ELSE
              MODIFY EF-LOGO     VALUE OPNSAV-FILENAME
           END-IF

           MOVE OPNSAV-FILENAME TO IMAGEN
           PERFORM FROMREC-TOSCREEN
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
      /
      *----------------------------------------------------------------*
      * EXITING PROGRAM                                                *
      *----------------------------------------------------------------*
       FORM1-EXIT.
           PERFORM SAVE-CHANGES

           IF ERRORI
              MOVE 26 TO KEY-STATUS
              EXIT PARAGRAPH
           END-IF.

           MOVE 27 TO KEY-STATUS
           .
      /
      *----------------------------------------------------------------*
      * COPY PROCEDURES                                                *
      *----------------------------------------------------------------*
       COPYS.
           copy "status.cpy".
           .
