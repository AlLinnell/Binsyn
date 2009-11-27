      PROGRAM SET0
C	USE MSFLIB
C     Program to copy output of DIFCORR determination of RFMG to
C     the stored RFMG value
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RFMG(5),RFMGA(5),VL31A(5),TZ1A(5),
     $TZ2A(5),FL1(4),A(17)
      CHARACTER * 36 FL1
	INTEGER(4) RESULT
  100 FORMAT (I5)
  101 FORMAT (8F10.5)
  102 FORMAT (6D20.13)
  103 FORMAT (A36)
  104 FORMAT (I2)
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
	RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C*******************************************************************
C 
      OPEN ( UNIT= 5, FILE='C:\LGTSYN\SET0\SET0.ILS', STATUS= 'OLD')
      READ ( 5, 103)  (FL1(J),J=1,4)
      OPEN ( UNIT= 1, FILE= FL1(1), STATUS= 'OLD')
      WRITE ( 1, 104) KEYBAD
      REWIND 1
      OPEN ( UNIT= 2, FILE= FL1(2), STATUS= 'OLD')
      OPEN ( UNIT= 3, FILE= FL1(3), STATUS= 'OLD')
      OPEN ( UNIT= 4, FILE= FL1(4), STATUS= 'REPLACE')
C*******************************************************************
C      Read existing values    
       READ (2, 100) NWL
       READ (2, 101) (RFMG(K),K=1,NWL)
C      Read corrections to current values from DIFCORR output
       READ (3, 102) (A(K),K=1,17)
       READ (3, 102) (VL31A(K),K=1,NWL)
       READ (3, 102) (TZ1A(K),K=1,NWL)
       READ (3, 102) (TZ2A(K),K=1,NWL)
       READ (3, 102) (RFMGA(K),K=1,NWL)
C      Correct current values
       DO K=1,NWL
		RFMG(K)=RFMG(K)+RFMGA(K)
	 END DO
C      Write corrected output values
       WRITE (4, 100) NWL
       WRITE (4, 101) (RFMG(K),K=1,NWL)
C********************************************************************
       CLOSE ( UNIT= 2, STATUS= 'KEEP')
       CLOSE ( UNIT= 3, STATUS= 'KEEP')
       CLOSE ( UNIT= 4, STATUS= 'KEEP')
       WRITE (1, 104) KEYGOOD
       CLOSE ( UNIT= 1, STATUS= 'KEEP')
C*********************************************************************
       STOP
       END PROGRAM SET0
