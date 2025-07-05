      **
      * COPY BOOK DESCRIPTION DATA FILES
      **
           SELECT MOVIES
              ASSIGN       TO 'MOVIES.dat'
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              FILE STATUS  IS STATUS-MOVIES
              RECORD KEY   IS CODIGO.
