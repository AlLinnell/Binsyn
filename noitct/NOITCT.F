      PROGRAM NOITCT
C     Program to increment iteration counter
	CHARACTER * 24 FL1
	INTEGER(4)  RESULT
  100 FORMAT (I2)
  101 FORMAT (A24)
C*************************************************
	OPEN (UNIT=2,FILE='NOITCT.ILS',
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
	STOP 'NOITCT: Completed successfully.'
	END PROGRAM
