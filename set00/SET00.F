
      PROGRAM SET00
C     Program to copy output of SPT determination of RFMG to
C     the stored RFMG value
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RFMG(5),RFMGA(5),FL1(4)
      CHARACTER * 24 FL1
  100 FORMAT (I5)
  101 FORMAT (8F10.5)
  102 FORMAT (A24)
  103 FORMAT (I2)
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C*******************************************************************
      OPEN ( UNIT= 5, FILE='SET00.ILS', STATUS= 'OLD')
      READ ( 5, 102)  (FL1(J),J=1,4)
      OPEN ( UNIT= 1, FILE= FL1(1), STATUS= 'OLD')
      WRITE( 1, 103) KEYBAD
      REWIND 1
      OPEN ( UNIT= 2, FILE= FL1(2), STATUS= 'OLD')
      OPEN ( UNIT= 3, FILE= FL1(3), STATUS= 'OLD')
      OPEN ( UNIT= 4, FILE= FL1(4), STATUS= 'REPLACE')
C*******************************************************************
C      Read existing values    
       READ (2, 100) NWL
       READ (2, 101) (RFMG(K),K=1,NWL)
C      Read corrected values
       READ (3, 100) NWLA
       IF (NWLA.NE.NWL) CALL PMDSTOP
       READ (3, 101) (RFMGA(K),K=1,NWL)
C      Write corrected output values
       WRITE (4, 100) NWL
       WRITE (4, 101) (RFMGA(K),K=1,NWL)
C********************************************************************
       CLOSE ( UNIT= 2, STATUS= 'DELETE')
       CLOSE ( UNIT= 3, STATUS= 'KEEP')
       CLOSE ( UNIT= 4, STATUS= 'KEEP')
       WRITE ( 1, 103) KEYGOOD
       CLOSE ( UNIT= 1, STATUS= 'KEEP')
C*********************************************************************
       STOP
       END PROGRAM SET00
