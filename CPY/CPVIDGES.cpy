      **
      * COPY BOOK DESCRIPTION DATA FILES GENRES
      **
           SELECT GENRES
              ASSIGN       TO 'GENRE.dat'
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS CODIGO-GEN WITH NO DUPLICATES
              FILE STATUS  IS STATUS-GENRES.
