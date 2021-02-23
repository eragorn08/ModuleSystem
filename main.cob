       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODULESYSTEM.
       AUTHOR. GROUP1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FD-TEACHER ASSIGN TO 'TEACHER.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS F-USERNAME
           FILE STATUS IS WS-FILESTATUS.

           SELECT FD-STUDENT ASSIGN TO 'STUDENT.dat'
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS F-STUDNUMBER
           FILE STATUS IS WS-FILESTATUS2.

           SELECT FD-SUMMARY ASSIGN TO 'SUMMARY.dat'
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  FD-TEACHER.
       01  F-TEACHERINFO.
           05 F-USERNAME PIC X(10).
           05 F-PASSWORD PIC X(10).
           05 F-TEACHERNAME PIC X(25).
           05 F-SECTION PIC X(6).

       FD  FD-STUDENT.
       01  F-STUDENTINFO.
           05 F-STUDNUMBER PIC 9(10).
           05 F-STUDNAME PIC X(25).
           05 F-STUDSECT PIC X(6).
           05 F-MODULENUMB PIC 9(4).
           05 F-GRADE PIC 9(3).
           05 F-MODULESTATUS PIC X(9).

       FD FD-SUMMARY.
       01 F-SUMMARYINFO.
          05 FD-PASS PIC 9(2).
          05 FD-FAIL PIC 9(2).
          05 FD-SUBMITTED PIC 9(2).
          05 FD-NSUBMITTED PIC 9(2).
          05 FD-STUDENTS PIC 9(2).

       WORKING-STORAGE SECTION.
       01  WS-MENU        PIC A.
           88 A           VALUE 'A', 'a'.
           88 B           VALUE 'B', 'b'.
           88 C           VALUE 'C', 'c'.
           88 D           VALUE 'D', 'd'.
           88 E           VALUE 'E', 'e'.
           88 G           VALUE 'G', 'g'.
           88 X           VALUE 'X', 'x'.

       01  QUIT            PIC 9  VALUE 0.
       01  WS-BLANK        PIC X(25) VALUE SPACES.

      *FILE STATUS
       01  WS-FILESTATUS PIC 9(2).
       01  WS-FILESTATUS2 PIC 9(2).
       01  WS-FS PIC 9(2).

      *TRIGGER
       01  WS-FLAG PIC 9.
       01  WS-FLAG2 PIC 9.

      *ADMIN ACCOUNT
       01  WS-ADMINUSERNAME PIC X(10).
       01  WS-ADMINPASSWORD PIC X(10).
       01  WS-PASSWORD-TEMP PIC X(10).

      *TEACHERS DATABASE
       01  WS-TEACHERINFO.
           05 WS-USERNAME PIC X(10).
           05 WS-PASSWORD PIC X(10).
           05 WS-TEACHERNAME PIC X(25).
           05 WS-SECTION PIC X(6).

      *STUDENTS DATABASE
       01  WS-STUDINFO.
           05 WS-STUDNUMBER PIC 9(10).
           05 WS-STUDNAME PIC X(25).
           05 WS-STUDSECT PIC X(6).
           05 WS-MODULENUMB PIC 9(4).
           05 WS-GRADE PIC 9(3).
           05 WS-MODULESTATUS PIC X(9).

       01  WS-EOF PIC A(1).
           88 EOF VALUE 'F'.
       01  WS-MOD1 PIC 9.
       01  WS-NUM PIC 9(2).
       01  WS-MODULE PIC 9(4).

      *SUMMARIZED DATABASE
       01 SUMMARYINFO.
          05 PASS PIC 9(2).
          05 FAIL PIC 9(2).
          05 SUBMITTED PIC 9(2).
          05 NSUBMITTED PIC 9(2).
          05 STUDENTS PIC 9(2).


       PROCEDURE DIVISION.
       MAIN.
           PERFORM PARA-MENU WITH TEST BEFORE UNTIL QUIT = 1.
           PERFORM PARA-SUMMARY.
           STOP RUN.

       PARA-MENU.
           MOVE 0 TO QUIT.
           DISPLAY WS-BLANK.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*            MAIN MENU               *'.
           DISPLAY '*   COBOL EDUCATION MODULE SYSTEM    *'
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   ADMIN LOGIN              *'.
           DISPLAY '*  => [B]   TEACHER LOGIN            *'.
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' .
           ACCEPT WS-MENU.

           IF A
              GO TO PARA-ADMIN
           ELSE IF B
              GO TO PARA-TEACHER
           ELSE
               ADD 1 TO QUIT
           END-IF.


       PARA-ADMIN.
           INITIALIZE WS-ADMINUSERNAME, WS-ADMINPASSWORD.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*        ADMINISTRATOR LOGIN         *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  USERNAME: ' .
           ACCEPT WS-ADMINUSERNAME.
           DISPLAY '*  PASSWORD: ' . 
           ACCEPT WS-ADMINPASSWORD.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.

           IF WS-ADMINUSERNAME="ADMIN" AND WS-ADMINPASSWORD="ADMIN"
               GO TO PARA-ADMIN-DASHBOARD
           ELSE
               DISPLAY "ACCOUNT DOES NOT EXIST."
               GO TO MAIN
           END-IF.


       PARA-ADMIN-DASHBOARD.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*      ADMINISTRATOR DASHBOARD       *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   CREATE TEACHER ACCOUNT   *'.
           DISPLAY '*  => [B]   EDIT TEACHER DATA        *'.
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' .
           ACCEPT WS-MENU.

           IF A
              GO TO CREATE-TEACHER
           ELSE IF B
              GO TO EDIT-TEACHER
           ELSE
              GO TO PARA-MENU
           END-IF.


       CREATE-TEACHER.
           DISPLAY WS-BLANK.
           DISPLAY WS-BLANK.
           DISPLAY "USERNAME: "
           ACCEPT F-USERNAME.
           DISPLAY "PASSWORD: "
           ACCEPT F-PASSWORD.
           DISPLAY "FIRST & LAST NAME: "
           ACCEPT F-TEACHERNAME.
           DISPLAY "SECTION: "
           ACCEPT F-SECTION.

           OPEN I-O FD-TEACHER
           IF WS-FILESTATUS = 35 THEN
               OPEN OUTPUT FD-TEACHER
           END-IF
           WRITE F-TEACHERINFO
           CLOSE FD-TEACHER.

           DISPLAY "ACCOUNT CREATION SUCCESSFUL."
           GO TO PARA-ADMIN-DASHBOARD.


       EDIT-TEACHER.
           INITIALIZE WS-TEACHERINFO, F-TEACHERINFO

           DISPLAY WS-BLANK
           DISPLAY WS-BLANK
           DISPLAY "ENTER TEACHER'S USERNAME: "
           ACCEPT F-USERNAME

           OPEN I-O FD-TEACHER
           IF WS-FILESTATUS NOT EQUAL TO 35
               READ FD-TEACHER INTO WS-TEACHERINFO
                   KEY IS F-USERNAME
           INVALID KEY DISPLAY "NOT FOUND." GO TO PARA-ADMIN-DASHBOARD
               END-READ
           ELSE
               DISPLAY "ACCOUNT DATABASE IS EMPTY."
               GO TO PARA-ADMIN-DASHBOARD
           END-IF.

           DISPLAY "[A] => NAME: "  WS-TEACHERNAME
           DISPLAY "[B] => USERNAME: " WS-USERNAME
           DISPLAY "[C] => PASSWORD: " WS-PASSWORD
           DISPLAY "[D] => SECTION: " WS-SECTION
           DISPLAY "[E] => DELETE ACCOUNT"
           DISPLAY "[ANY] => EXIT"
           DISPLAY "ENTER OPERATION: "
           ACCEPT WS-MENU

           MOVE WS-TEACHERINFO TO F-TEACHERINFO

           DISPLAY WS-BLANK
           DISPLAY WS-BLANK

           IF A
               DISPLAY "NEW NAME: "
               ACCEPT F-TEACHERNAME
           ELSE IF B
               DISPLAY "NEW USERNAME: "
               ACCEPT F-USERNAME
           ELSE IF C
               DISPLAY "NEW PASSWORD: "
               ACCEPT F-PASSWORD
           ELSE IF D
               DISPLAY "NEW SECTION: "
               ACCEPT F-SECTION
           ELSE IF E
               DISPLAY "ARE YOU SURE?"
               DISPLAY "[A] => YES"
               DISPLAY "[ANY] => EXIT"
               DISPLAY "ENTER OPERATION: "
               ACCEPT WS-MENU

               IF A
                   DELETE FD-TEACHER RECORD
                       NOT INVALID KEY DISPLAY "ACCOUNT DELETED."
                   END-DELETE
               ELSE
                   GO TO PARA-ADMIN-DASHBOARD
               END-IF
           ELSE
               GO TO PARA-ADMIN-DASHBOARD
           END-IF.

           MOVE F-TEACHERINFO TO WS-TEACHERINFO

           REWRITE F-TEACHERINFO FROM WS-TEACHERINFO
               NOT INVALID KEY DISPLAY "ACCOUNT UPDATED."
           END-REWRITE

           GO TO PARA-ADMIN-DASHBOARD.

           CLOSE FD-TEACHER.


       PARA-TEACHER.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*        TEACHER LOGIN PORTAL        *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  USERNAME: ' .
           ACCEPT F-USERNAME.
           DISPLAY '*  PASSWORD: ' .
           ACCEPT WS-PASSWORD-TEMP.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.

           OPEN INPUT FD-TEACHER
           IF WS-FILESTATUS NOT EQUAL TO 35
               READ FD-TEACHER INTO WS-TEACHERINFO
                   KEY IS F-USERNAME
                   NOT INVALID KEY MOVE 1 TO WS-FLAG
               END-READ
           ELSE
               DISPLAY "ACCOUNT DATABASE IS EMPTY."
               GO TO MAIN
           END-IF.

           IF WS-FLAG = 1
               IF WS-PASSWORD-TEMP = WS-PASSWORD
                   DISPLAY "LOGGED IN"
                   GO TO MENU-TEACHER
               ELSE
                   DISPLAY "ACCOUNT NOT FOUND"
                   GO TO MAIN
               END-IF
           ELSE
               DISPLAY "ACCOUNT NOT FOUND"
               GO TO MAIN
           END-IF.

           CLOSE FD-TEACHER.


       MENU-TEACHER.
           DISPLAY WS-BLANK.
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY '*          TEACHERS MENU             *'.
           DISPLAY '*                                    *'.
           DISPLAY '*  => [A]   INPUT STUDENT DATA       *'.
           DISPLAY '*  => [B]   SEARCH STUDENT           *'.
           DISPLAY '*  => [C]   STUDENT LIST             *'.
           DISPLAY '*  => [D]   SUMMARY                  *'
           DISPLAY '*  => [ANY] EXIT                     *'.
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '.
           DISPLAY '       CHOOSE AN OPERATION: ' .
           ACCEPT WS-MENU.

           IF A
               GO TO STUDENT-DATA
           ELSE IF B
               GO TO SEARCH-PARA
           ELSE IF C
               GO TO STUDENT-LIST
           ELSE
               GO TO MAIN
           END-IF.


       STUDENT-DATA.
           INITIALIZE F-STUDENTINFO
           
           DISPLAY WS-BLANK
           DISPLAY WS-BLANK
           DISPLAY '**************************************'
           DISPLAY '*                                    *'
           DISPLAY "ENTER STUDENT NUMBER".
           ACCEPT F-STUDNUMBER.
           DISPLAY "ENTER STUDENT NAME".
           ACCEPT F-STUDNAME.
           DISPLAY "ENTER STUDENT SECTION".
           ACCEPT F-STUDSECT.
           DISPLAY '*  SUBMIT A MODULE?                  *'
           DISPLAY '*  [A] NO                            *'
           DISPLAY '*  [B] YES                         *'
           DISPLAY '*                                    *'
           DISPLAY '**************************************'
           DISPLAY '                                      '
           DISPLAY '       CHOOSE AN OPERATION: '
           ACCEPT WS-MENU

           IF A
               MOVE 'NOT YET' TO F-MODULESTATUS
               MOVE 0 TO F-MODULENUMB
               MOVE 0 TO F-GRADE

               OPEN I-O FD-STUDENT
               IF WS-FILESTATUS2 = 35 THEN
                   OPEN OUTPUT FD-STUDENT
               END-IF
               WRITE F-STUDENTINFO
               CLOSE FD-STUDENT

               GO TO MENU-TEACHER
           ELSE IF B
               GO TO MODULE-PARA
           END-IF.

           OPEN I-O FD-STUDENT
           IF WS-FILESTATUS2 = 35 THEN
               OPEN OUTPUT FD-STUDENT
           END-IF
           WRITE F-STUDENTINFO
           CLOSE FD-STUDENT.


       MODULE-PARA.
           DISPLAY "ENTER MODULE NUMBER".
           ACCEPT F-MODULENUMB.
           DISPLAY "ENTER MODULE GRADE".
           ACCEPT F-GRADE.

           MOVE 'SUBMITTED' TO F-MODULESTATUS

           OPEN I-O FD-STUDENT
               WRITE F-STUDENTINFO
           CLOSE FD-STUDENT.

           DISPLAY "STUDENT DATA HAS BEEN RECORDED"
           GO TO MENU-TEACHER.


       SEARCH-PARA.
           INITIALIZE F-STUDENTINFO
           DISPLAY WS-BLANK
           DISPLAY "ENTER STUDENT NUMBER: ".
           ACCEPT F-STUDNUMBER
           OPEN I-O FD-STUDENT
           IF WS-FILESTATUS2 NOT EQUAL TO 35
               READ FD-STUDENT INTO WS-STUDINFO
                   KEY IS F-STUDNUMBER
           INVALID KEY DISPLAY "NOT FOUND." GO TO MENU-TEACHER
               END-READ
           ELSE
               DISPLAY "ACCOUNT DATABASE IS EMPTY."
               GO TO MENU-TEACHER
           END-IF.
           DISPLAY WS-BLANK
           DISPLAY '**************************************'.
           DISPLAY '*                                    *'.
           DISPLAY "*  STUDENT INFO: "
           DISPLAY "*  STUDENT NUMBER: " WS-STUDNUMBER
           DISPLAY "*  STUDENT NAME: " WS-STUDNAME
           DISPLAY "*  STUDENT SECTION: " WS-STUDSECT
           DISPLAY '*                                    *'
           DISPLAY '*  MODULE STATUS: ' WS-MODULESTATUS
           DISPLAY '*  MODULE NUMBER: ' WS-MODULENUMB
           DISPLAY '*  MODULE GRADE: ' WS-GRADE
           DISPLAY '*                                    *'
           DISPLAY '*  EDIT DATA OF STUDENT?             *'
           DISPLAY '*  [A] YES                           *'
           DISPLAY "*  [ANY] TO EXIT"
           DISPLAY '*                                    *'.
           DISPLAY '**************************************'.
           DISPLAY '                                      '
           DISPLAY '       CHOOSE AN OPERATION: '
           ACCEPT WS-MENU
           IF A
               GO TO EDIT-STUDENT
           ELSE
               GO TO MENU-TEACHER
           END-IF.
           CLOSE FD-STUDENT.


       EDIT-STUDENT.
           INITIALIZE WS-STUDINFO, F-STUDENTINFO

           DISPLAY WS-BLANK
           DISPLAY WS-BLANK
           DISPLAY "ENTER STUDENT NUMBER: "
           ACCEPT F-STUDNUMBER

           OPEN I-O FD-STUDENT
           IF WS-FILESTATUS NOT EQUAL TO 35
               READ FD-STUDENT INTO WS-STUDINFO
                   KEY IS F-STUDNUMBER
           INVALID KEY DISPLAY "NOT FOUND." GO TO MENU-TEACHER
               END-READ
           ELSE
               DISPLAY "ACCOUNT DATABASE IS EMPTY."
               GO TO MENU-TEACHER
           END-IF.

           DISPLAY "[A] => STUDENT NUMBER: "  WS-STUDNUMBER
           DISPLAY "[B] => STUDENT NAME: " WS-STUDNAME
           DISPLAY "[C] => STUDENT SECTION:  " WS-STUDSECT
           DISPLAY "[D] => MODULE STATUS: " WS-MODULESTATUS
           DISPLAY "[E] => MODULE NUMBER:  " WS-MODULENUMB
           DISPLAY "[G] => GRADE: " WS-GRADE
           DISPLAY "[X] => DELETE ACCOUNT"
           DISPLAY "[ANY] => EXIT"
           DISPLAY "ENTER OPERATION: "
           ACCEPT WS-MENU

           MOVE WS-STUDINFO TO F-STUDENTINFO

           DISPLAY WS-BLANK
           DISPLAY WS-BLANK
           IF A
               DISPLAY "NEW STUDENT NUMBER: "
               ACCEPT F-STUDNUMBER
           ELSE IF B
               DISPLAY "NEW STUDENT NAME: "
               ACCEPT F-STUDNAME
           ELSE IF C
               DISPLAY "NEW STUDENT SECTION: "
               ACCEPT F-STUDSECT
           ELSE IF D
               DISPLAY "NEW MODULE STATUS:  "
               ACCEPT F-MODULESTATUS
           ELSE IF E
               DISPLAY "NEW MODULE NUMBER: "
               ACCEPT F-MODULENUMB
           ELSE IF G
               DISPLAY "NEW GRADE: "
               ACCEPT F-GRADE
           ELSE IF X
               DISPLAY "ARE YOU SURE?"
               DISPLAY "[A] => YES"
               DISPLAY "[ANY] => EXIT"
               DISPLAY "ENTER OPERATION: "
               ACCEPT WS-MENU

               IF A
                   DELETE FD-STUDENT RECORD
                       NOT INVALID KEY DISPLAY "STUDENT DATA DELETED."
                   END-DELETE
               ELSE	
                   GO TO MENU-TEACHER
               END-IF
           ELSE
               GO TO MENU-TEACHER
           END-IF.
           MOVE F-STUDENTINFO TO WS-STUDINFO
           REWRITE F-STUDENTINFO FROM WS-STUDINFO
               NOT INVALID KEY DISPLAY "DATA UPDATED."
           END-REWRITE

           GO TO REPEAT-EDIT
           CLOSE FD-STUDENT.


       REPEAT-EDIT.
           DISPLAY "WANT TO EDIT DATA AGAIN?"
           DISPLAY "[A] NO "
           DISPLAY "[ANY] YES"
           DISPLAY '                                      '
           DISPLAY 'CHOOSE AN OPERATION: '
           ACCEPT WS-MENU
           IF A
               GO TO MENU-TEACHER
           ELSE
               GO TO SEARCH-PARA
           END-IF.


       STUDENT-LIST.
           
      *LIST BY SECTION (DEFAULT=SECTION OF TEACHER)
       
           INITIALIZE WS-STUDINFO, F-STUDENTINFO

           DISPLAY WS-BLANK
           DISPLAY "SECTION: " F-SECTION
           DISPLAY "TEACHER: " F-TEACHERNAME.

           MOVE F-SECTION TO F-STUDSECT
           MOVE "T" TO WS-EOF
           MOVE 1 TO WS-NUM

           OPEN INPUT FD-STUDENT
           PERFORM UNTIL WS-EOF = "F"
               READ FD-STUDENT NEXT RECORD INTO WS-STUDINFO
                   AT END MOVE "F" TO WS-EOF
               END-READ
               
               IF WS-STUDSECT = F-SECTION
                   DISPLAY "[" WS-NUM "]" WS-STUDINFO
                   ADD 1 TO WS-NUM
               END-IF

           END-PERFORM
           CLOSE FD-STUDENT.
           
           
      *LIST BY MODULE NUMBER
          
           DISPLAY WS-BLANK
           DISPLAY WS-BLANK
           
           DISPLAY "MODULE NUMBER: "
           ACCEPT WS-MODULE
           
           DISPLAY "MODULE: " WS-MODULE

           MOVE WS-MODULE TO F-MODULENUMB
           MOVE 1 TO WS-NUM
           MOVE "T" TO WS-EOF
           
           OPEN INPUT FD-STUDENT
           PERFORM UNTIL WS-EOF = "F"
               READ FD-STUDENT NEXT RECORD INTO WS-STUDINFO
                   AT END MOVE "F" TO WS-EOF
               END-READ
               
               IF F-MODULENUMB = WS-MODULE
                   DISPLAY "[" WS-NUM "]" WS-STUDINFO
                   ADD 1 TO WS-NUM
               END-IF
               
               
           END-PERFORM
           CLOSE FD-STUDENT.
           
           GO TO MENU-TEACHER.
           
       PARA-SUMMARY.
           MOVE "T" TO WS-EOF.
           OPEN I-O FD-STUDENT
               IF WS-FILESTATUS NOT EQUAL TO 35
               
                PERFORM UNTIL WS-EOF = "F"
                READ FD-STUDENT NEXT RECORD INTO WS-STUDINFO
                  AT END MOVE "F" TO WS-EOF
                  NOT AT END 
                   ADD 1 TO STUDENTS

                   IF WS-GRADE > 74
                       ADD 1 TO PASS
                   else
                       ADD 1 TO FAIL

                   IF WS-MODULESTATUS EQUALS "SUBMITTED"
                       ADD 1 TO SUBMITTED
                   ELSE 
                       ADD 1 TO NSUBMITTED
                 END-PERFORM
                 
                ELSE
                   DISPLAY "ACCOUNT DATABASE IS EMPTY."
                 END-IF
           CLOSE FD-STUDENT.

           OPEN OUTPUT FD-SUMMARY.
               MOVE SUMMARYINFO TO F-SUMMARYINFO
               WRITE F-SUMMARYINFO
           CLOSE FD-SUMMARY.

           DISPLAY 'STUDENTS: ' STUDENTS.
           DISPLAY 'PASS: ' PASS.
           DISPLAY 'FAIL: ' FAIL.
           DISPLAY 'SUBMITTED: ' SUBMITTED.
           DISPLAY 'NOT YET PA: ' NSUBMITTED.
           
