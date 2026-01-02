       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEGACY-COBOL.
      * Legacy COBOL code with old patterns and bad practices
      * No structured programming, GOTO statements, hardcoded values
      * Poor data validation, monolithic code

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO "users.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD USER-FILE.
       01 USER-RECORD.
           05 USER-ID PIC 9(5).
           05 USER-NAME PIC X(20).
           05 USER-PASS PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-USER-ID PIC 9(5) VALUE 0.
       01 WS-USER-NAME PIC X(20).
       01 WS-USER-PASS PIC X(10).
       01 WS-EOF PIC X VALUE 'N'.
       01 WS-COUNT PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT USER-FILE.
           PERFORM READ-USER UNTIL WS-EOF = 'Y'.
           CLOSE USER-FILE.
           DISPLAY "Processed " WS-COUNT " users".
           STOP RUN.

       READ-USER.
           READ USER-FILE INTO USER-RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END PERFORM PROCESS-USER.
           ADD 1 TO WS-COUNT.

       PROCESS-USER.
      * Bad practice: no input validation
           IF USER-ID = 12345 THEN
               DISPLAY "Admin user: " USER-NAME
           ELSE
               DISPLAY "Regular user: " USER-NAME.
      * GOTO bad practice
           GO TO READ-USER.

      * No modularity, everything in one place
      * Hardcoded values everywhere