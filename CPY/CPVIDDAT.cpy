      **
      * COPY BOOK MOVIES DATA FILE
      **
       01 MOV-REC.                  *> LRECL 286
           05 CODIGO                PIC 9(05).
           05 TITULO                PIC X(30).
           05 GENERO                PIC X(02).
           05 DURACAO               PIC 9(03).
           05 DISTRIB               PIC X(15).
           05 NOTA                  PIC 9(02).
           05 FILLER                PIC X(37).
           05 IMAGEN                PIC X(256).