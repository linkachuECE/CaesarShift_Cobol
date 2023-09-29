           IDENTIFICATION DIVISION.
           PROGRAM-ID. DISPLAY-TEST-RESULTS.  

           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables
            01 I-ITER      PIC 999 VALUE 0.

            LINKAGE SECTION.
      *    Input variables
             01 STR-TEST-NAME      PIC X(100).
             01 I-TEST-NAME-LEN    PIC 99.
             01 I-PASS-FAIL        PIC 9.

      *    Return variable
             01 I-RET-VAL       PIC 9 VALUE 1.
           
           PROCEDURE DIVISION
            USING STR-TEST-NAME I-TEST-NAME-LEN, I-PASS-FAIL.
           
            IF I-PASS-FAIL = 1
             DISPLAY "- ", STR-TEST-NAME(1:I-TEST-NAME-LEN), " PASSED"
            ELSE
             DISPLAY "- ", STR-TEST-NAME(1:I-TEST-NAME-LEN), " FAILED"
            END-IF              

            EXIT PROGRAM.
           END PROGRAM DISPLAY-TEST-RESULTS.

      *    // Check whether two strings ARE EQUAL OR NOT
           IDENTIFICATION DIVISION.
           PROGRAM-ID. ASSERT-STR-EQUALS. 
            
           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables
            01 I-ITER      PIC 999 VALUE 1.

            01 I-STR-LEN     PIC 999 VALUE 0.
            LINKAGE SECTION.
      *    Input variables
             01 STR-INPUT1   PIC X(100).
             01 STR-INPUT2   PIC X(100).

      *    Return variable
             01 I-RET-VAL   PIC 9 VALUE 1.
           
           PROCEDURE DIVISION
            USING STR-INPUT1, STR-INPUT2
            RETURNING I-RET-VAL.
      *
            DISPLAY "COMPARING '", STR-INPUT1, "'"
            DISPLAY "AND '", STR-INPUT2 "'"

            IF STR-INPUT1 = STR-INPUT2
             MOVE 1 TO I-RET-VAL
            ELSE
             MOVE 0 TO I-RET-VAL
            END-IF.

            EXIT PROGRAM RETURNING I-RET-VAL.
           END PROGRAM ASSERT-STR-EQUALS.

      *    /////////////////// TESTING FUNCTIONS ///////////////////////

      *    // Encrypt TEST
           IDENTIFICATION DIVISION.
           PROGRAM-ID. TEST-ENCRYPT.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables
             01 I-ENCRYPT-RESULT           PIC 9 VALUE 1.
             01 I-CURR-TEST-RESULT         PIC 9 VALUE 1.
             01 I-DISPLAY-RESULT-OUTPUT    PIC 9 VALUE 1.
             01 I-CURR-TEST-NAME-LEN       PIC 99.
             01 STR-CURR-TEST-INPUT        PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-EXPECT       PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-OUTPUT       PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-NAME         PIC X(100) VALUES SPACES.


            LINKAGE SECTION.
      *    Return variable
             01 I-RET-VAL              PIC 9 VALUE 0.
           
           PROCEDURE DIVISION
            RETURNING I-RET-VAL.
            
            DISPLAY "ENTERED TEST-ENCRYPT"

      *    // An empty STRING
            
            MOVE "Yee" TO STR-CURR-TEST-INPUT.
            
            CALL "ENCRYPT" USING
             STR-CURR-TEST-INPUT,
             BY VALUE 000,
             BY VALUE 01,
             STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "" TO STR-CURR-TEST-EXPECT.
            
            DISPLAY "REACHED POST-ENCRYPT"

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT.
           
            MOVE "Empty String" TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            CALL "DISPLAY-TEST-RESULTS" USING
             STR-CURR-TEST-NAME,
             I-CURR-TEST-NAME-LEN,
             I-CURR-TEST-RESULT.             

           MOVE SPACES TO STR-CURR-TEST-INPUT.
           MOVE SPACES TO STR-CURR-TEST-EXPECT.

      *    // A string of all the same values
           MOVE "zzz" TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
              STR-CURR-TEST-INPUT,
              003,
              01,
              STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "aaa" TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "String of all same alphabetic values" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.
            
            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.

      *     // An uppercase alphabetic string
            MOVE "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
              STR-CURR-TEST-INPUT,
              026,
              01,
              STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "BCDEFGHIJKLMNOPQRSTUVWXYZA" TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Uppercase alphabet string" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.

      *    // A lowercase alphabetic string
           
           MOVE "abcdefghijklmnopqrstuvwxyz" TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
              STR-CURR-TEST-INPUT,
              026,
              01,
              STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "bcdefghijklmnopqrstuvwxyza" TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Lowercase alphabet string" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.
            

            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.

      *    // A number string
           MOVE "1234567" TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
             STR-CURR-TEST-INPUT,
             007,
             01,
             STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "1234567" TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Number string" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.
            
            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.

      *    // A string of random non-alphanumeric characters
            MOVE "!@#$%^&*()/." TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
             STR-CURR-TEST-INPUT,
             012,
             01,
             STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "!@#$%^&*()/." TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Punctuation string" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            CALL "DISPLAY-TEST-RESULTS" USING
             STR-CURR-TEST-NAME,
             I-CURR-TEST-NAME-LEN,
             I-CURR-TEST-RESULT.

      *    // Random sentence 1
           MOVE "Real eyes realize real i's" TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
             STR-CURR-TEST-INPUT,
             026,
             07,
             STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "Ylhs lflz ylhspgl ylhs p'z" TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Random sentence 1" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.
      
      *    // Random sentence 2
           MOVE "Your mom so fat she eats" TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
             STR-CURR-TEST-INPUT,
             024,
             19,
             STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "Rhnk fhf lh ytm lax xtml" TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Random sentence 2" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.

      *    // Random sentence 3
           MOVE "Everybody wants to rule the world"
           TO STR-CURR-TEST-INPUT.
 
            CALL "ENCRYPT" USING
             STR-CURR-TEST-INPUT,
             033,
             24,
             STR-CURR-TEST-OUTPUT RETURNING I-ENCRYPT-RESULT.

            MOVE "Ctcpwzmbw uylrq rm psjc rfc umpjb"
            TO STR-CURR-TEST-EXPECT.

            CALL "ASSERT-STR-EQUALS" USING
             STR-CURR-TEST-EXPECT,
             STR-CURR-TEST-OUTPUT RETURNING I-CURR-TEST-RESULT.
           
            MOVE "Random sentence 3" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            CALL "DISPLAY-TEST-RESULTS" USING
                STR-CURR-TEST-NAME,
                I-CURR-TEST-NAME-LEN,
                I-CURR-TEST-RESULT.

            EXIT PROGRAM.
           END PROGRAM TEST-ENCRYPT.

      *    // Decrypt TEST
           IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST-DECRYPT.
      *      
           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables
             01 I-CURR-TEST-RESULT     PIC 9 VALUE 1.
             01 STR-CURR-TEST-OUTPUT   PIC X(100) VALUE "1".

            LINKAGE SECTION.
      *    Return variable
             01 I-RET-VAL          PIC 9 VALUE 0.
           
           PROCEDURE DIVISION               
               RETURNING I-RET-VAL.
                          
      *    // An empty string
      
      *    // A string of all the same values
           
      *    // An uppercase alphabetic string
      
      *    // A lowercase alphabetic string
      
      *    // A number string
      
      *    // A string of random non-alphanumeric characters
      
      *    // Random sentence 1
      
      *    // Random sentence 2

      *    // Random sentence 3
       
            EXIT PROGRAM RETURNING I-RET-VAL.
           END PROGRAM TEST-DECRYPT.
      *    //////////////// END OF TESTING FUNCTIONS ///////////////////