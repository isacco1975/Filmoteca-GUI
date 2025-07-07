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
           FONT SMALL-FONT
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
           MAX-TEXT 2,
           AFTER PROCEDURE READ-GENRE
           .

      * ENTRY FIELD
       05
           EF-DURATION,
           ENTRY-FIELD,
           COL 20,00,
           LINE 9,00,
           LINES 1,31 ,
           SIZE 25,00 ,
           BOXED,
           COLOR IS 2,
           ENABLED MOD,
           FONT IS SMALL-FONT,
           ID IS 5003,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MAX-TEXT 3,
           .

      * ENTRY FIELD
       05
           EF-DISTRIB,
           ENTRY-FIELD,
           COL 20,00,
           LINE 11,00,
           LINES 1,31 ,
           SIZE 25,00 ,
           BOXED,
           COLOR IS 2,
           ENABLED MOD,
           FONT IS SMALL-FONT,
           ID IS 5003,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           MAX-TEXT 15,
           .

      * LABEL
       05
           LBL-GENRE-DES,
           LABEL,
           COL 30,00,
           LINE 7,00,
           SIZE 33,00 ,
           ID IS 192,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "<NOT APPLICABLE>",
           FONT DEFAULT-FONT
           COLOR 5
           .

      * PUSH BUTTON
       05
           PB-GENRE-ZOOM,
           PUSH-BUTTON,
           COL 25,50,
           LINE 7,00,
           LINES 1,31 ,
           SIZE 3,50 ,
           EXCEPTION-VALUE 2003,
           FONT IS SMALL-FONT,
           ID IS 6005,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "...",
           .

      * LABEL
       05
           LBL-DISTR,
           LABEL,
           COL 3,00,
           LINE 11,00,
           SIZE 15,00 ,
           ID IS 192,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "DISTRIBUTION",
           FONT DEFAULT-FONT
           .

      * LABEL
       05
           LBL-DUR,
           LABEL,
           COL 3,00,
           LINE 09,00,
           SIZE 15,00 ,
           ID IS 192,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TITLE "DURATION",
           FONT DEFAULT-FONT
           .

      * ENTRY FIELD
       05
           EF-GRADE,
           ENTRY-FIELD,
           COL 75,00,
           LINE 7,00,
           LINES 1,31 ,
           SIZE 5,00 ,
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
           LBL-CODE,
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
           LBL-TITLE,
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
           FONT DEFAULT-FONT
           .

      * LABEL
       05
           LBL-GENRE,
           LABEL,
           COL 3,00,
           LINE 7,00,
           LINES 1,31 ,
           SIZE 13,00 ,
           ID IS 8,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "GENRE",
           FONT DEFAULT-FONT
           .

      * BITMAP CONTROLLER
       05  MOVIEBMP,
           BITMAP,
           LINE 09 COL 55
           LINES 110 SIZE 150
           BITMAP-HANDLE LOGO-BMP
           .

      * LABEL
       05
           LBL-GRADE,
           LABEL,
           COL 65,00,
           LINE 7,00,
           LINES 1,31 ,
           SIZE 13,00 ,
           FONT DEFAULT-FONT,
           ID IS 10,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           TRANSPARENT,
           TITLE "GRADE",
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
           ENABLED MOD,
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
           ENABLED MOD,
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
