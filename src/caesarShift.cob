      *    // Encrypt
           IDENTIFICATION DIVISION.
            PROGRAM-ID. CAESAR-SHIFT.
      *      
           DATA DIVISION.
            WORKING-STORAGE SECTION.
      *    Internal variables

             01 I-ITER      PIC 999.
            
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
             01 I-SHIFT-AMOUNT PIC S99.

      *    Output
             01 STR-OUTPUT     PIC X(100).

      *    Return variable
           
           PROCEDURE DIVISION
            USING STR-INPUT I-INPUT-LEN I-SHIFT-AMOUNT STR-OUTPUT.

      *     INITIALIZE the OUTPUT STRING TO have only SPACES
            MOVE SPACES TO STR-OUTPUT.

      *     Loop through each character in the the string

            PERFORM VARYING I-ITER FROM 1 BY 1 
                    UNTIL I-ITER > I-INPUT-LEN

      *      Grab the current CHARACTER
             MOVE STR-INPUT(I-ITER:1) TO C-CURR-CHAR
             
      *      Get the ASCII value (+1) of the current character
             COMPUTE I-CURR-VAL-ASCII =
                       FUNCTION ORD(STR-INPUT(I-ITER:1))

      *      Initialize the output ASCII val and char to the current val
             MOVE FUNCTION CHAR(I-CURR-VAL-ASCII) TO C-NEW-CHAR
             MOVE I-CURR-VAL-ASCII TO I-NEW-VAL-ASCII

      *      Check what kind of character the current char is
             EVALUATE TRUE
      *       If this char is a space
              WHEN I-CURR-VAL-ASCII = I-SPACE-ASCII
      *        Move on
               CONTINUE
                 
      *       If this char is an uppercase letter
              WHEN C-CURR-CHAR IS ALPHABETIC-UPPER
      *        Shift the character by the specified amount
               ADD  I-SHIFT-AMOUNT
                    TO I-CURR-VAL-ASCII 
                    GIVING I-NEW-VAL-ASCII
               
      *        Alphabet bounds checking
               EVALUATE TRUE

      *         If the new ascii val went past 'Z'
                WHEN I-NEW-VAL-ASCII > I-CAPITAL-Z-ASCII
      *         Wrap around to the beginning of the alphabet
                 COMPUTE I-NEW-VAL-ASCII = I-CAPITAL-A-ASCII
                    + (I-NEW-VAL-ASCII - I-CAPITAL-Z-ASCII - 1)

      *         If the new ascii val went before 'A'
                WHEN I-NEW-VAL-ASCII < I-CAPITAL-A-ASCII
      *          Wrap around to the end of the alphabet
                 COMPUTE I-NEW-VAL-ASCII = I-CAPITAL-Z-ASCII
                       - (I-CAPITAL-A-ASCII - I-NEW-VAL-ASCII - 1)

      *         If the new ascii value is in range
                WHEN OTHER
      *           Move on
                  CONTINUE

               END-EVALUATE

      *       If this is a lowercase letter
              WHEN C-CURR-CHAR IS ALPHABETIC-LOWER

      *        Add the the shift value
               ADD  I-SHIFT-AMOUNT
                    TO I-CURR-VAL-ASCII 
                    GIVING I-NEW-VAL-ASCII

      *        Alphabet bounds checking
               EVALUATE TRUE

      *         If the new ascii value is past 'z'
                WHEN I-NEW-VAL-ASCII > I-LOWER-Z-ASCII
      *          Wrap around to the beginning of the alphabet
                 COMPUTE I-NEW-VAL-ASCII = I-LOWER-A-ASCII
                    + (I-NEW-VAL-ASCII - I-LOWER-Z-ASCII - 1)

      *         If the new ascii value is before 'a'
                WHEN I-NEW-VAL-ASCII < I-LOWER-A-ASCII
      *          Wrap around to the end of the alphabet
                 COMPUTE I-NEW-VAL-ASCII = I-LOWER-Z-ASCII
                    - (I-LOWER-A-ASCII - I-NEW-VAL-ASCII - 1)

                WHEN OTHER
                  CONTINUE
                     
               END-EVALUATE

              WHEN OTHER
                CONTINUE

             END-EVALUATE

      *      Get the new character
             MOVE FUNCTION CHAR(I-NEW-VAL-ASCII) TO C-NEW-CHAR

      *      Move the new character back into the string
             MOVE C-NEW-CHAR TO STR-OUTPUT(I-ITER:1) 

            END-PERFORM
            
            EXIT PROGRAM.

           END PROGRAM CAESAR-SHIFT.


      *    Encrypt
           IDENTIFICATION DIVISION.
            PROGRAM-ID. ENCRYPT.

           ENVIRONMENT DIVISION.

           DATA DIVISION.
            WORKING-STORAGE SECTION.

             01 I-SHIFT-AMOUNT-SIGNED PIC S99.
      
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
            
             COMPUTE I-SHIFT-AMOUNT-SIGNED = -I-SHIFT-AMOUNT.

            CALL "CAESAR-SHIFT" USING
             BY REFERENCE STR-INPUT,
             BY CONTENT I-INPUT-LEN,
             BY CONTENT I-SHIFT-AMOUNT,
             BY REFERENCE STR-OUTPUT.
              
            EXIT PROGRAM.
           END PROGRAM ENCRYPT.

      *    Encrypt
           IDENTIFICATION DIVISION.
            PROGRAM-ID. DECRYPT.

           ENVIRONMENT DIVISION.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
             01 I-DECRYPT-SHIFT-AMOUNT PIC S99.
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
             COMPUTE I-DECRYPT-SHIFT-AMOUNT = -I-SHIFT-AMOUNT.
             
            CALL "CAESAR-SHIFT" USING
             BY REFERENCE STR-INPUT,
             BY CONTENT I-INPUT-LEN,
             BY CONTENT I-DECRYPT-SHIFT-AMOUNT,
             BY REFERENCE STR-OUTPUT.
              
            EXIT PROGRAM.
           END PROGRAM DECRYPT.


      *    Solve
           IDENTIFICATION DIVISION.
            PROGRAM-ID. SOLVE.

           DATA DIVISION.
            WORKING-STORAGE SECTION.
             01 I-ITER         PIC 99.
             01 STR-OUTPUT     PIC X(100).

            LINKAGE SECTION.
             01 STR-INPUT       PIC X(100).
             01 I-INPUT-LEN     PIC 99.
             01 I-MAX-SHIFT-VAL PIC 999.
             
           PROCEDURE DIVISION 
               USING STR-INPUT, I-INPUT-LEN, I-MAX-SHIFT-VAL.
               
               DISPLAY "- IN: " STR-INPUT
               DISPLAY "- Outputs:"

            PERFORM VARYING I-ITER
                    FROM 1 BY 1 UNTIL
                    I-ITER > I-MAX-SHIFT-VAL
               
               CALL "ENCRYPT"
                   USING STR-INPUT, I-INPUT-LEN, I-ITER, STR-OUTPUT
               
               DISPLAY "  - Caesar ", I-ITER, ": '", 
                               STR-OUTPUT(1:I-INPUT-LEN), "'"
            END-PERFORM.

            EXIT PROGRAM.

           END PROGRAM SOLVE.
