       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          IMPGEN.
       AUTHOR.              ISAAC GARCIA PEVERI
       DATE-WRITTEN.        2025 JULY 4
      /
      ******************************************************************
      *                                                                *
      *  UTILITY TO IMPORT GENRES INTO AN INDEXED FORMAT               *
      *                                                                *
      ******************************************************************
      /
       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
          SELECT IFILEO1 ASSIGN TO "GENRES.TXT"
                         ORGANIZATION LINE SEQUENTIAL.
          COPY 'CPVIDGES.CPY'.

       DATA                 DIVISION.
       FILE                 SECTION.
       FD IFILEO1.
       01 REC-IFILEO1.
          10 IFILE01-COD PIC X(2).
          10 IFILE01-DES PIC X(8).
             
       FD GENRES.
          COPY 'CPVIDGEN.CPY'.
       
      *
       WORKING-STORAGE      SECTION.
       77 STATUS-GENRES PIC XX.
       
       LINKAGE          SECTION.

       SCREEN           SECTION.

       PROCEDURE  DIVISION.
      /
      *----------------------------------------------------------------*
      * MAIN                                                           *
      *----------------------------------------------------------------*
       MAIN-LOGIC.
           OPEN INPUT IFILEO1
           OPEN OUTPUT GENRES
           
           READ IFILEO1 AT END MOVE HIGH-VALUES TO REC-IFILEO1 
           END-READ
           
           PERFORM UNTIL REC-IFILEO1 = HIGH-VALUES
              MOVE IFILE01-COD TO CODIGO-GEN CONVERT
              MOVE IFILE01-DES TO DESC-GEN
              WRITE REG-GEN
              
              READ IFILEO1 AT END MOVE HIGH-VALUES TO REC-IFILEO1 
              END-READ
           END-PERFORM
           
           CLOSE IFILEO1
           CLOSE GENRES
           
           STOP RUN
           .
