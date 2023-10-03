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
             01 STR-TEST-INPUT     PIC X(100).
             01 STR-TEST-OUTPUT    PIC X(100).
             01 STR-TEST-EXPECT    PIC X(100).
             01 I-PASS-FAIL        PIC 9.

           PROCEDURE DIVISION
            USING  STR-TEST-NAME, 
                   I-TEST-NAME-LEN,
                   STR-TEST-INPUT,
                   STR-TEST-OUTPUT,
                   STR-TEST-EXPECT,
                   I-PASS-FAIL.

            IF I-PASS-FAIL = 1
             DISPLAY "  - ", 
                     STR-TEST-NAME(1:I-TEST-NAME-LEN),
                     ": PASSED"
            ELSE
             DISPLAY "  - ",
                     STR-TEST-NAME(1:I-TEST-NAME-LEN),
                     ": FAILED"
            END-IF

            DISPLAY "    - IN:       '", STR-TEST-INPUT, "'"    
            DISPLAY "    - OUT:      '", STR-TEST-OUTPUT, "'"    
            DISPLAY "    - EXPECTED: '", STR-TEST-EXPECT, "'"    

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
            USING STR-INPUT1, STR-INPUT2, I-RET-VAL
            RETURNING I-RET-VAL.
      *
      *     DISPLAY "ENTERED ASSERT-STR-EQUALS"
      *     DISPLAY "COMPARING '", STR-INPUT1, "'"
      *     DISPLAY "AND '", STR-INPUT2 "'"

            IF STR-INPUT1 = STR-INPUT2
             COMPUTE I-RET-VAL = 1
            ELSE
             COMPUTE I-RET-VAL = 0
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
             01 I-CURR-STR-LEN             PIC 999.
             01 I-CURR-SHIFT-AMOUNT        PIC 99.
             01 STR-CURR-TEST-INPUT        PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-EXPECT       PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-OUTPUT       PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-NAME         PIC X(100) VALUES SPACES.


            LINKAGE SECTION.
      *    Return variable
           PROCEDURE DIVISION.
            
      *    // An empty STRING
            
            DISPLAY "TESTING 'ENCRYPT'"

            MOVE " " TO STR-CURR-TEST-INPUT.
            MOVE " " TO STR-CURR-TEST-EXPECT.
            MOVE 000 TO I-CURR-STR-LEN.
            MOVE 1 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Empty String" TO STR-CURR-TEST-NAME.
            MOVE 12 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // A string of all the same values
            MOVE "aaa" TO STR-CURR-TEST-INPUT.
            MOVE "bbb" TO STR-CURR-TEST-EXPECT.
            MOVE 003 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "String of all same alphabetic values" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // An uppercase alphabetic string
            MOVE "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO STR-CURR-TEST-INPUT.
            MOVE "BCDEFGHIJKLMNOPQRSTUVWXYZA" TO STR-CURR-TEST-EXPECT.
            MOVE 026 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "An uppercase alphabetic string" 
               TO STR-CURR-TEST-NAME.
            MOVE 30 TO I-CURR-TEST-NAME-LEN.
            
            PERFORM TEST-RUN.

      *    // A lowercase alphabetic string
            MOVE "abcdefghijklmnopqrstuvwxyz" TO STR-CURR-TEST-INPUT.
            MOVE "bcdefghijklmnopqrstuvwxyza" TO STR-CURR-TEST-EXPECT.
            MOVE 026 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Lowercase alphabetic string" 
               TO STR-CURR-TEST-NAME.
            MOVE 27 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // A number string
            MOVE "1234567" TO STR-CURR-TEST-INPUT.
            MOVE "1234567" TO STR-CURR-TEST-EXPECT.
            MOVE 007 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Number string" 
               TO STR-CURR-TEST-NAME.
            MOVE 13 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // A string of random punctuation
            MOVE "!@#$%^&*()/." TO STR-CURR-TEST-INPUT.
            MOVE "!@#$%^&*()/." TO STR-CURR-TEST-EXPECT.
            MOVE 012 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "String of random punctuation" 
               TO STR-CURR-TEST-NAME.
            MOVE 28 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // Random sentence 1
            MOVE "Real eyes realize real i's" TO STR-CURR-TEST-INPUT.
            MOVE "Ylhs lflz ylhspgl ylhs p'z" TO STR-CURR-TEST-EXPECT.
            MOVE 026 TO I-CURR-STR-LEN.
            MOVE 07 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Random sentence 1" 
               TO STR-CURR-TEST-NAME.
            MOVE 17 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // Random sentence 2
            MOVE "Supercalifragilisticexpialidocious"
               TO STR-CURR-TEST-INPUT.
            MOVE "Lnixkvtebyktzbeblmbvxqibtebwhvbhnl"
               TO STR-CURR-TEST-EXPECT.
            MOVE 034 TO I-CURR-STR-LEN.
            MOVE 19 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Random sentence 2" 
               TO STR-CURR-TEST-NAME.
            MOVE 17 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // Random sentence 3
            MOVE "Everybody wants to rule the world"
               TO STR-CURR-TEST-INPUT.
            MOVE "Ctcpwzmbw uylrq rm psjc rfc umpjb"
               TO STR-CURR-TEST-EXPECT.
            MOVE 033 TO I-CURR-STR-LEN.
            MOVE 24 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Random sentence 3" 
               TO STR-CURR-TEST-NAME.
            MOVE 17 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

            EXIT PROGRAM.

           TEST-RUN.
      *     DISPLAY "ENTERED TEST-RUN PARAGRAPH"
      *     DISPLAY "ABOUT TO CALL ENCRYPT"
            CALL "ENCRYPT" USING
             BY REFERENCE STR-CURR-TEST-INPUT,
             BY CONTENT I-CURR-STR-LEN,
             BY CONTENT I-CURR-SHIFT-AMOUNT,
             BY REFERENCE STR-CURR-TEST-OUTPUT.
            
      *     DISPLAY "ABOUT TO CALL ASSERT-STR-EQUALS"
            CALL "ASSERT-STR-EQUALS" USING
             BY REFERENCE STR-CURR-TEST-EXPECT,
             BY REFERENCE STR-CURR-TEST-OUTPUT,
             BY REFERENCE I-CURR-TEST-RESULT,
             RETURNING I-CURR-TEST-RESULT.
            
      *     DISPLAY "ABOUT TO CALL DISPLAY-TEST-RESULTS WITH:"
      *     DISPLAY "- STR-CURR-TEST-NAME: '", STR-CURR-TEST-NAME, "'"
      *     DISPLAY "- I-CURR-TEST-NAME-LEN: ", I-CURR-TEST-NAME-LEN
      *     DISPLAY "- I-CURR-TEST-RESULT: ", I-CURR-TEST-RESULT

            CALL "DISPLAY-TEST-RESULTS" USING
             BY REFERENCE STR-CURR-TEST-NAME,
             BY CONTENT I-CURR-TEST-NAME-LEN,
             BY REFERENCE STR-CURR-TEST-INPUT,
             BY REFERENCE STR-CURR-TEST-OUTPUT,
             BY REFERENCE STR-CURR-TEST-EXPECT,
             BY CONTENT I-CURR-TEST-RESULT.
           
           EXIT.

           END PROGRAM TEST-ENCRYPT.





      *    // Decrypt TEST
           IDENTIFICATION DIVISION.
           PROGRAM-ID. TEST-DECRYPT.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables
             01 I-ENCRYPT-RESULT           PIC 9 VALUE 1.
             01 I-CURR-TEST-RESULT         PIC 9 VALUE 1.
             01 I-DISPLAY-RESULT-OUTPUT    PIC 9 VALUE 1.
             01 I-CURR-TEST-NAME-LEN       PIC 99.
             01 I-CURR-STR-LEN             PIC 999.
             01 I-CURR-SHIFT-AMOUNT        PIC 99.
             01 STR-CURR-TEST-INPUT        PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-EXPECT       PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-OUTPUT       PIC X(100) VALUES SPACES.
             01 STR-CURR-TEST-NAME         PIC X(100) VALUES SPACES.


            LINKAGE SECTION.
      *    Return variable
           PROCEDURE DIVISION.
            
            DISPLAY "TESTING 'DECRYPT'"

      *    // An empty STRING
            MOVE " " TO STR-CURR-TEST-INPUT.
            MOVE " " TO STR-CURR-TEST-EXPECT.
            MOVE 000 TO I-CURR-STR-LEN.
            MOVE 1 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Empty String" TO STR-CURR-TEST-NAME.
            MOVE 12 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // A string of all the same values
            MOVE "aaa" TO STR-CURR-TEST-INPUT.
            MOVE "zzz" TO STR-CURR-TEST-EXPECT.
            MOVE 003 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "String of all same alphabetic values" 
               TO STR-CURR-TEST-NAME.
            MOVE 36 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // An uppercase alphabetic string
            MOVE "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO STR-CURR-TEST-INPUT.
            MOVE "ZABCDEFGHIJKLMNOPQRSTUVWXY" TO STR-CURR-TEST-EXPECT.
            MOVE 026 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "An uppercase alphabetic string" 
               TO STR-CURR-TEST-NAME.
            MOVE 30 TO I-CURR-TEST-NAME-LEN.
            
            PERFORM TEST-RUN.

      *    // A lowercase alphabetic string
            MOVE "abcdefghijklmnopqrstuvwxyz" TO STR-CURR-TEST-INPUT.
            MOVE "zabcdefghijklmnopqrstuvwxy" TO STR-CURR-TEST-EXPECT.
            MOVE 026 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Lowercase alphabetic string" 
               TO STR-CURR-TEST-NAME.
            MOVE 27 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // A number string
            MOVE "1234567" TO STR-CURR-TEST-INPUT.
            MOVE "1234567" TO STR-CURR-TEST-EXPECT.
            MOVE 007 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Number string" 
               TO STR-CURR-TEST-NAME.
            MOVE 13 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // A string of random punctuation
            MOVE "!@#$%^&*()/." TO STR-CURR-TEST-INPUT.
            MOVE "!@#$%^&*()/." TO STR-CURR-TEST-EXPECT.
            MOVE 012 TO I-CURR-STR-LEN.
            MOVE 01 TO I-CURR-SHIFT-AMOUNT.
            MOVE "String of random punctuation" 
               TO STR-CURR-TEST-NAME.
            MOVE 28 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // Random sentence 1
            MOVE "Ylhs lflz ylhspgl ylhs p'z" TO STR-CURR-TEST-INPUT.
            MOVE "Real eyes realize real i's" TO STR-CURR-TEST-EXPECT.
            MOVE 026 TO I-CURR-STR-LEN.
            MOVE 07 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Random sentence 1" 
               TO STR-CURR-TEST-NAME.
            MOVE 17 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // Random sentence 2
            MOVE "Lnixkvtebyktzbeblmbvxqibtebwhvbhnl"
               TO STR-CURR-TEST-INPUT.
            MOVE "Supercalifragilisticexpialidocious"
               TO STR-CURR-TEST-EXPECT.
            MOVE 034 TO I-CURR-STR-LEN.
            MOVE 19 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Random sentence 2" 
               TO STR-CURR-TEST-NAME.
            MOVE 17 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

      *    // Random sentence 3
            MOVE "Ctcpwzmbw uylrq rm psjc rfc umpjb"
               TO STR-CURR-TEST-INPUT.
            MOVE "Everybody wants to rule the world"
               TO STR-CURR-TEST-EXPECT.
            MOVE 033 TO I-CURR-STR-LEN.
            MOVE 24 TO I-CURR-SHIFT-AMOUNT.
            MOVE "Random sentence 3" 
               TO STR-CURR-TEST-NAME.
            MOVE 17 TO I-CURR-TEST-NAME-LEN.

            PERFORM TEST-RUN.

            EXIT PROGRAM.

           TEST-RUN.
            CALL "DECRYPT" USING
             BY REFERENCE STR-CURR-TEST-INPUT,
             BY CONTENT I-CURR-STR-LEN,
             BY CONTENT I-CURR-SHIFT-AMOUNT,
             BY REFERENCE STR-CURR-TEST-OUTPUT.
            
      *     DISPLAY "ABOUT TO CALL ASSERT-STR-EQUALS"
            CALL "ASSERT-STR-EQUALS" USING
             BY REFERENCE STR-CURR-TEST-EXPECT,
             BY REFERENCE STR-CURR-TEST-OUTPUT,
             BY REFERENCE I-CURR-TEST-RESULT,
             RETURNING I-CURR-TEST-RESULT.
            
            CALL "DISPLAY-TEST-RESULTS" USING
             BY REFERENCE STR-CURR-TEST-NAME,
             BY CONTENT I-CURR-TEST-NAME-LEN,
             BY REFERENCE STR-CURR-TEST-INPUT,
             BY REFERENCE STR-CURR-TEST-OUTPUT,
             BY REFERENCE STR-CURR-TEST-EXPECT,
             BY CONTENT I-CURR-TEST-RESULT.
           
            EXIT.
           END PROGRAM TEST-DECRYPT.


           IDENTIFICATION DIVISION.
            PROGRAM-ID. CAESAR-CIPHER-TEST-SUITE.
           
           PROCEDURE DIVISION.

           CALL "TEST-ENCRYPT".
           DISPLAY " ".
           CALL "TEST-DECRYPT".
           DISPLAY " ".
      *    CALL "TEST-SHOW"

           END PROGRAM CAESAR-CIPHER-TEST-SUITE.

      *    //////////////// END OF TESTING FUNCTIONS ///////////////////
