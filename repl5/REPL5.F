      PROGRAM REPL5
	USE MSFLIB
C     Program to copy file RFMGA.DAT to file RFMG.DAT and delete RFMGA.DAT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FL1(3),RFMGA(5)
      CHARACTER * 36 FL1
	INTEGER(4) RESULT
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  100 FORMAT (A36)
  101 FORMAT (I5)
  102 FORMAT (5F10.4)
  103 FORMAT (I2)
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C**************************************************************
C 
      OPEN (UNIT=4,  FILE='C:\LGTSYN\REPL5\REPL5.ILS',  STATUS= 'OLD')
      READ (4,100)   (FL1(J),J=1,3)
      OPEN ( UNIT=1, FILE= FL1(1), STATUS= 'OLD')
      WRITE ( 1, 103) KEYBAD
      REWIND 1
      OPEN ( UNIT=2, FILE= FL1(2), STATUS= 'OLD')
      OPEN ( UNIT=3, FILE= FL1(3), STATUS= 'REPLACE')
C*************************************************************
C     Input update data
      READ (2, 101) NWL
      READ (2, 102) (RFMGA(J),J=1,NWL)
C*************************************************************
C     Output new file 
      WRITE (3, 101) NWL
      WRITE (3, 102) (RFMGA(J),J=1,NWL)
      CLOSE ( UNIT= 2, STATUS= 'DELETE')
      CLOSE ( UNIT= 3, STATUS= 'KEEP')
      WRITE ( 1, 103) KEYGOOD
      CLOSE ( UNIT= 1, STATUS= 'KEEP')
      STOP
      END PROGRAM REPL5
