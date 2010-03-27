      PROGRAM NOITCT
C     Program to increment iteration counter
	USE MSFLIB
	CHARACTER * 24 FL1
	INTEGER(4)  RESULT
  100 FORMAT (I2)
  101 FORMAT (A24)
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C*************************************************
	OPEN (UNIT=2,FILE='C:\LGTSYN\NOITCT\NOITCT.ILS',
     $ STATUS = 'OLD')
	READ (2,101) FL1
	OPEN ( UNIT=2, FILE=FL1, STATUS = 'OLD')
C*******************************************************
      READ (2, 100) NOIT
	REWIND 2
	NOIT=NOIT+1
	WRITE (2, 100) NOIT
	CLOSE( UNIT = 2, STATUS = 'KEEP')
	CLOSE( UNIT = 1, STATUS = 'KEEP')
	STOP
	END
