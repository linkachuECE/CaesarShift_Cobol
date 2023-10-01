      *    ///////////////////// MAIN FUNCTION /////////////////////////
           IDENTIFICATION DIVISION.
             PROGRAM-ID. 'MAIN'.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 I-TEST-SUITE-RESULT PIC 9 VALUE 0.

           PROCEDURE DIVISION.

      *     DISPLAY "ENTERED PROGRAM"
            
            CALL "TEST-ENCRYPT" RETURNING I-TEST-SUITE-RESULT.
               
           END PROGRAM 'MAIN'.

      *    ///////////////// END OF MAIN FUNCTION //////////////////////
