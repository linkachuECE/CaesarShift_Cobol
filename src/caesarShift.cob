      *    // Encrypt
           IDENTIFICATION DIVISION.
            PROGRAM-ID. CAESAR-SHIFT.
      *      
           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables

             01 I-ITER      PIC 999.
            
             01 I-NUM-CONSECUTIVE-SPACES PIC 9 VALUE 0.
             01 I-PREVIOUS-CHAR-WAS-SPACE PIC 9 VALUE 0.
             01 I-MAX-CONSECUTIVE-SPACES PIC 9 VALUE 10.

             01 I-CAPITAL-A-ASCII PIC 999 VALUE 66.
             01 I-CAPITAL-Z-ASCII PIC 999 VALUE 91.
             01 I-LOWER-A-ASCII   PIC 999 VALUE 98.
             01 I-LOWER-Z-ASCII   PIC 999 VALUE 123.
             01 I-SPACE-ASCII     PIC 999 VALUE 033.

             01 I-CURR-VAL-ASCII  PIC 999.
             01 I-NEW-VAL-ASCII   PIC 999.
             
             01 C-CURR-CHAR       PIC X(1).
             01 C-NEW-CHAR        PIC X(1).

            LINKAGE SECTION.
      *    Input arguments
             01 STR-INPUT      PIC X(100).
             01 I-INPUT-LEN    PIC 999.
             01 I-SHIFT-AMOUNT PIC 99.

      *    Output
             01 STR-OUTPUT     PIC X(100).

      *    Return variable
           
           PROCEDURE DIVISION
            USING STR-INPUT I-INPUT-LEN I-SHIFT-AMOUNT STR-OUTPUT.
            
      *     DISPLAY "ENTERED CAESAR-SHIFT"

      *     DISPLAY "- CURRENTLY SHIFTING: '", STR-INPUT, "'"
      *     DISPLAY "- I-SHIFT-AMOUNT: " I-SHIFT-AMOUNT
      *     DISPLAY "- I-INPUT-LEN: ", I-INPUT-LEN
            
      *     INITIALIZE the OUTPUT STRING TO have only SPACES
      *     DISPLAY "ABOUT TO MOVE SPACES TO OUTPUT"
            MOVE SPACES TO STR-OUTPUT.
      *     DISPLAY "MOVED SPACES TO OUTPUT"

      *     Loop through each character in the the string

      *    ********** THIS IS WHERE THE PROBLEM IS **************

            PERFORM VARYING I-ITER FROM 1 BY 1 
                    UNTIL I-ITER >= I-INPUT-LEN

      *    OCCASIONALLY, THIS PERFORM LOOP WILL CONTINUE LONG AFTER
      *    I-ITER HAS SURPASSED THE VALUE OF I-INPUT-LEN.



      *      Grab the current CHARACTER
      *      DISPLAY "ABOUT TO MOVE A CHARACTER FROM",
      *                "INPUT STRING TO A VAR"
             MOVE STR-INPUT(I-ITER:1) TO C-CURR-CHAR
      *      DISPLAY "MOVED CHARACTER FROM INPUT STRING TO A VAR"
             
      *      Get the ASCII value (+1) of the current character
             COMPUTE I-CURR-VAL-ASCII =
                       FUNCTION ORD(STR-INPUT(I-ITER:1))

      *      Debug
             DISPLAY "I-CURR-VAL-ASCII: ", I-CURR-VAL-ASCII
             DISPLAY "C-CURR-CHAR: ", C-CURR-CHAR
             DISPLAY "I-ITER: ", I-ITER
             DISPLAY "I-INPUT-LEN: ", I-INPUT-LEN
             DISPLAY " "

      *      Initialize the output ASCII val and char to the current val
             MOVE FUNCTION CHAR(I-CURR-VAL-ASCII) TO C-NEW-CHAR
             MOVE I-CURR-VAL-ASCII TO I-NEW-VAL-ASCII
             
      *      If the current character is a space
             IF I-CURR-VAL-ASCII = I-SPACE-ASCII
      *       DISPLAY "space         " WITH NO ADVANCING
              CONTINUE
             END-IF

      *      If the current char is an uppercase letter
             IF C-CURR-CHAR IS ALPHABETIC-UPPER
                AND I-CURR-VAL-ASCII NOT = I-SPACE-ASCII

      *       Debug
      *       DISPLAY "uppercase     " WITH NO ADVANCING
              
      *       Add the shifting amount to the ascii value
              ADD I-SHIFT-AMOUNT
                  TO I-CURR-VAL-ASCII 
                  GIVING I-NEW-VAL-ASCII

      *       If the ASCII value is no longer an uppercase letter
              IF I-NEW-VAL-ASCII > I-CAPITAL-Z-ASCII

      *        Wrap around to a value at the beginning of the alphabet
               COMPUTE I-NEW-VAL-ASCII = I-CAPITAL-A-ASCII
                           + (I-NEW-VAL-ASCII - I-CAPITAL-Z-ASCII - 1)
              END-IF

             END-IF

      *      If the current value is a lowercase letter
             IF C-CURR-CHAR IS ALPHABETIC-LOWER
                AND I-CURR-VAL-ASCII NOT = I-SPACE-ASCII
      *       Debug
      *       DISPLAY "lowercase     " WITH NO ADVANCING
              
      *       Add the shift amount to the ASCII value
              ADD I-SHIFT-AMOUNT
                  TO I-CURR-VAL-ASCII 
                  GIVING I-NEW-VAL-ASCII

      *       If the new value went past 'z'
              IF I-NEW-VAL-ASCII > I-LOWER-Z-ASCII

      *        Wrap around to the beginning of the alphabet
               COMPUTE I-NEW-VAL-ASCII = I-LOWER-A-ASCII
                           + (I-NEW-VAL-ASCII - I-LOWER-Z-ASCII - 1)
              END-IF

             END-IF

      *      If the current character is non-alphabetic
             IF C-CURR-CHAR IS NOT ALPHABETIC
      *       Debug
      *       DISPLAY "non-alphabetic" WITH NO ADVANCING
              CONTINUE
             END-IF
             
      *      Debug
      *      DISPLAY " | New val: ", I-NEW-VAL-ASCII WITH NO ADVANCING
             
      *      Get the new character
             MOVE FUNCTION CHAR(I-NEW-VAL-ASCII) TO C-NEW-CHAR

      *      Debug
      *      DISPLAY ", as char: '", C-NEW-CHAR, "'"
             
      *      Move the new character back into the string
             MOVE C-NEW-CHAR TO STR-OUTPUT(I-ITER:1) 

            END-PERFORM
            
      *     Debug
      *     DISPLAY "RESULT: '", STR-OUTPUT, "'"
            
      *     DISPLAY "REACHED END OF CAESAR SHIFT"
            EXIT PROGRAM.

           END PROGRAM CAESAR-SHIFT.


      *    Encrypt
           IDENTIFICATION DIVISION.
            PROGRAM-ID. ENCRYPT.

           ENVIRONMENT DIVISION.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
      
            LINKAGE SECTION.
      *    Input arguments
             01 STR-INPUT      PIC X(100).
             01 I-INPUT-LEN    PIC 999.
             01 I-SHIFT-AMOUNT PIC 99.

      *    Output
             01 STR-OUTPUT     PIC X(100).

      *    Return variable
           PROCEDURE DIVISION
            USING STR-INPUT, I-INPUT-LEN, I-SHIFT-AMOUNT, STR-OUTPUT.

      *     DISPLAY "ENTERED ENCRYPT"

            CALL "CAESAR-SHIFT" USING
             BY REFERENCE STR-INPUT,
             BY REFERENCE I-INPUT-LEN,
             BY REFERENCE I-SHIFT-AMOUNT,
             BY REFERENCE STR-OUTPUT.

      *      DISPLAY "REACHED END OF ENCRYPT"

            EXIT PROGRAM.
           END PROGRAM ENCRYPT.

      *    // Decrypt
      *    IDENTIFICATION DIVISION.
      *     FUNCTION-ID. DECRYPT.

      *    DATA DIVISION. C C
      *     WORKING-STORAGE SECTION.
      
      *     LINKAGE SECTION.
      *      
      *    PROCEDURE DIVISION
      *        

      *     GOBACK.
      *    END FUNCTION DECRYPT.



      *    // Solve
      *    IDENTIFICATION DIVISION.
      *     FUNCTION-ID. SOLVE.

      *    DATA DIVISION.
      *     WORKING-STORAGE SECTION.
      *     LINKAGE SECTION.
      *      
      *    PROCEDURE DIVISION
      *        

      *     GOBACK.
      *    END FUNCTION SOLVE.
