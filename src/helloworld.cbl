           IDENTIFICATION DIVISION.
           PROGRAM-ID. "HELLOWORLD".
      *    AUTHOR.     ETHAN BRAUN.

           ENVIRONMENT DIVISION.
           
           INPUT-OUTPUT SECTION.

           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WEIGHT PIC 999.
           01 HEIGHT_INCHES PIC 999.
           01 BMI  PIC 999V999.

           PROCEDURE DIVISION.
           0100-START-HERE.

               DISPLAY "Enter your height in inches: "
               ACCEPT HEIGHT_INCHES

               DISPLAY "Enter your weight in lbs: "
               ACCEPT WEIGHT

               COMPUTE BMI = WEIGHT * 703 / (HEIGHT_INCHES ** 2)

               DISPLAY "Your BMI is : ", BMI, "%"
           
           STOP RUN.
           END PROGRAM HELLOWORLD.
