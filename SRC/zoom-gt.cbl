       program-id.     Zoom-GT.
       REMARKS. NUMERO MASSIMO DI CAMPI IN GRID 10.

      *************************
       working-storage section.
      *************************
       copy "acugui.def".
       copy "acucobol.def".
       copy "crtvars.def".

       78 78-walt              value 1.
       78 78-win-box           value 2.
       78 78-about             value 3.
       78 78-walt-file         value 4.
       78 78-walt-delim        value 5.
       78 78-quit              value 27.

           copy "xzoom-common.def".
           copy "xzoom3.def".
           copy "externals.def".

       77  param-size pic 9(5).
       77  nome-file  pic X(256).

       77  stato-zoom signed-long.

       77  idx pic 9(3).

       77  filler  pic 9 value 0.
         88 TuttiIRec value 1, false 0.

       77  filler  pic 9 value 0.
         88 destinif value 1, false 0.

       LINKAGE SECTION.
       77  como-file   pic x(20).
       77  como-record pic x(32000).

      ************************************************************************
       procedure division using como-file, como-record.
      ************************************************************************
       MAIN-LOGIC.
           set environment "XZOOM_HIDE_CLOCK"     to 1.
           set environment "XZOOM_WINDOW_DELAYED" to 1.
           set environment "XZOOM_NULL_TEXT_NUMERIC" to "#Blank#".
           set environment "XZOOM_NULL_TEXT_ALPHA"   to "#Blank#".

           call "C$PARAMSIZE" 
                using 2, 
                giving param-size.

           set environment "XZOOM_LAYOUT" to "Grid".

           evaluate como-file         
           when "MOVIES.DAT" 
                perform PREPARA-MOVIES             |CERCA
           when other
                display message box "guarda che non è ancora stato fatto
      -                             "IL PARAGRAFO DI PREPARAZIONE PER QU
      -                             "ESTO FILE"
                exit program
           end-evaluate.

           call "XZOOM" using xzoom-linkage 
                                 COMO-RECORD(1:PARAM-SIZE)
                                 giving stato-zoom.

           cancel "XZOOM".          

           goback stato-zoom.

      ***---
       PREPARA-MOVIES.
           initialize xzoom-linkage xzoom-ext-info(1).

           move zero to idx.
           move  zero                    to xzoom-row.
           move  zero                    to xzoom-cln.
           move  16                      to xzoom-lw.
           move  80                      to xzoom-sw.
           move "MOVIES.DAT"             to xzoom-file-name(1).
           move  0                       to xzoom-file-key.
           move  4                       to xzoom-fields.

      * CAMPO 1
           add 1 to idx
           move  5                       to xzoom-field-length(idx).
           move  0                       to xzoom-field-offset(idx).
           move  13                      to xzoom-field-column(idx).
           move "CODE"                   to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 2
           add 1 to idx
           move  20                      to xzoom-field-length(idx).
           move  5                       to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "TITLE"                  to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

      * CAMPO 3
           add 1 to idx
           move  15                      to xzoom-field-length(idx).
           move  40                      to xzoom-field-offset(idx).
           move  25                      to xzoom-field-column(idx).
           move "DISTRIB"                to xzoom-field-name(idx).  
           set  xzoom-ft-alpha(idx)      to true. 

           move  -1                     to xzoom-delimiter-offset.
           move  5                      to xzoom-delimiter-length.
           move "000"                   to xzoom-from-value.
           move "000"                   to xzoom-to-value.

