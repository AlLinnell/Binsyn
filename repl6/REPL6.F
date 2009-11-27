      PROGRAM REPL6
C	USE MSFLIB
C     Program to copy file NOITA.DAT to file NOIT.DAT and delete 
C     NOITA.DAT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION FL1(2)
	CHARACTER * 24 FL1
	INTEGER * 4 RESULT
  100 FORMAT (A24)
  101 FORMAT (I5)
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C**********************************************************************
	OPEN (UNIT = 3,  FILE = 'C:\LGTSYN\REPL6\REPL6.ILS',
     $STATUS = 'OLD')
	READ (3,100)     (FL1(J),J=1,2)
	OPEN (UNIT = 1, FILE = FL1(1), STATUS = 'OLD')
	OPEN (UNIT = 2, FILE = FL1(2), STATUS = 'OLD')
C**********************************************************************
C     Input update data
      READ (1, 101) NOIT
C**********************************************************************
C     Output new file
      WRITE (2, 101) NOIT
	CLOSE (UNIT = 1, STATUS = 'KEEP')
	CLOSE (UNIT = 2, STATUS = 'KEEP')
	STOP
	END PROGRAM REPL6
