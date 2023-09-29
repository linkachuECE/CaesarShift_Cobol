      *    ///////////////////// MAIN FUNCTION /////////////////////////
           IDENTIFICATION DIVISION.
             PROGRAM-ID. 'MAIN'.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 I-TEST-SUITE-RESULT PIC 9 VALUE 0.

           PROCEDURE DIVISION.

            DISPLAY "ENTERED PROGRAM"
            
            CALL "TEST-ENCRYPT" RETURNING I-TEST-SUITE-RESULT.
               
           END PROGRAM 'MAIN'.

      *    ///////////////// END OF MAIN FUNCTION //////////////////////


      *     DISPLAY "UPPERCASE:"

      *     PERFORM UNTIL ITER > LENGTH OF TEST-STRING1
      *      MOVE TEST-STRING1(ITER:1) TO CURR-CHAR
      *      DISPLAY CURR-CHAR ": " WITH NO ADVANCING

      *      COMPUTE RESULT = FUNCTION ORD(CURR-CHAR)
      *      DISPLAY RESULT
      *        
      *      ADD 1 TO ITER
      *     END-PERFORM.
      *     
      *     MOVE 1 TO ITER.
      *     DISPLAY " ".
      *     
      *     DISPLAY "LOWERCASE:"

      *     PERFORM UNTIL ITER > LENGTH OF TEST-STRING2
      *      MOVE TEST-STRING2(ITER:1) TO CURR-CHAR
      *      DISPLAY "- " CURR-CHAR ": " WITH NO ADVANCING

      *      COMPUTE RESULT = FUNCTION ORD(CURR-CHAR)
      *      DISPLAY RESULT
      *        
      *      ADD 1 TO ITER
      *     END-PERFORM.

      *     GOBACK.
      *    END PROGRAM 'CAESAR-CIPHER'.
