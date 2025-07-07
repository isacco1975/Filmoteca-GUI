       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          SPLASH.
       AUTHOR.              ISAAC GARCIA PEVERI
       DATE-WRITTEN.        2025 JULY 4

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.

       DATA                 DIVISION.
       FILE                 SECTION.
      *

       WORKING-STORAGE      SECTION.
               COPY "ACUGUI.DEF".
               COPY "ACUCOBOL.DEF".
               COPY "CRTVARS.DEF".

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
       77 LOGO-BMP         PIC S9(9) COMP-4.

       LINKAGE          SECTION.

       SCREEN           SECTION.
      * FORM
       01
           FORM1,
           .

      * PUSH BUTTON
       05
           PB-OK,
           PUSH-BUTTON,
           COL 40,50,
           LINE 19,00,
           LINES 3 ,
           SIZE 5,50 ,
           EXCEPTION-VALUE 10000,
           ID IS 10000
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "OK",
           .

      * BITMAP CONTROLLER
       05  MOVIEBMP,
           BITMAP,
           LINE 1 COL 1
           LINES 1000 SIZE 1500
           BITMAP-HANDLE LOGO-BMP
           .


       PROCEDURE  DIVISION.
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
           .
      /
      *----------------------------------------------------------------*
      * INITIALIZE BITMAPS                                             *
      *----------------------------------------------------------------*
       INIT-BMP.
           COPY RESOURCE "..\RESOURCE\EDUC.BMP".
           CALL "W$BITMAP" USING WBITMAP-LOAD
            "..\RESOURCE\EDUC.BMP",
                   GIVING LOGO-BMP.
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
              CONTROLS-UNCROPPED,
              LABEL-OFFSET 23,
              LINK TO THREAD,
              MODELESS,
              NO SCROLL,
              TITLE-BAR,
              TITLE "FILMOTECA EDUC.360",
              AUTO-MINIMIZE,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              NO WRAP,
              HANDLE IS FORM1-HANDLE,
              
           DISPLAY FORM1   
           .
      /
      *----------------------------------------------------------------*
      * ACCEPT SCREEN                                                  *
      *----------------------------------------------------------------*
       FORM1-PROC.
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
           EVALUATE TRUE
              WHEN EXIT-PUSHED
                 PERFORM FORM1-EXIT
              WHEN EVENT-OCCURRED
                 IF EVENT-TYPE = CMD-CLOSE
                    PERFORM FORM1-EXIT
                 END-IF
               WHEN KEY-STATUS = 10000
                  PERFORM FORM1-EXIT
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
           DISPLAY FORM1 UPON FORM1-HANDLE
           .
      /
      *----------------------------------------------------------------*
      * EXIT PROGRAM                                                   *
      *----------------------------------------------------------------*
       EXIT-STOP-ROUTINE.
           CALL "W$BITMAP" USING WBITMAP-DESTROY, LOGO-BMP
           STOP RUN
           .
      /
      *----------------------------------------------------------------*
      * EXITING PROGRAM                                                *
      *----------------------------------------------------------------*
       FORM1-EXIT.
           DESTROY FORM1
           CALL 'TMOVIES.ACU'
           MOVE 27 TO KEY-STATUS
           .
