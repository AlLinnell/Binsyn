      PROGRAM SUMMARY
C	USE MSFLIB
C     Program to log binary star parameters as fn.of iteration number
C     Note that an initial line must be included in the output files
C     that gives the assumed starting parameters, and that has an
C     assigned iteration number of 0.
      DOUBLE PRECISION VI,ECC,OMD,POT1,POT2,QS,BA1,BA2,
     $GD1,GD2,TE1,TE2,US1,US2,VL3,SGMA,SGMAP 
      DIMENSION FL1(8),VIV(300),QSV(300),ECCV(300),OMDV(300),
     $POT1V(300),POT2V(300),TE1V(300),TE2V(300),BA1V(300),BA2V(300),
     $GD1V(300),GD2V(300),VL3V(300),SGMAV(300),SGMAPV(300),RFM(300)
	DIMENSION RFMG(5)
      COMMON /NUNIT/ NU,NOUT,IFOUT
      CHARACTER * 36 FL1
	INTEGER(4) RESULT
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
      DATA IN1,IN2,IN3,IN4,IN5,IN6/1,2,3,4,5,6/
      DATA NO1,NO2/7,8/
  100 FORMAT (I3,2F10.5,F9.4,F10.4,2F9.5,2F9.4)
  101 FORMAT (10I5)
  102 FORMAT (6D20.13)
  103 FORMAT (A80)
  104 FORMAT (A36)
  105 FORMAT (I2)
  107 FORMAT (I3,4F9.5,F10.6,1X,F10.5,2F10.6)
  106 FORMAT (I3,3F12.5)
  108 FORMAT ('Parameter optimization',/,
     $' IT    VI          QS       ECC      OMD      POT1     POT2   
     $TE1     TE2',/,
     $'*****************************************************************
     $**************')
  109 FORMAT ('Parameter optimization',/,
     $' IT    BA1      BA2       GD1      GD2     VL3(1)     RFMG  
     $SGMA     SGMAP',/,
     $'*****************************************************************
     $****************')
C********************************************************************
C      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT=  9, FILE='SUMMARY.ILS', 
     $STATUS= 'OLD')
      READ ( 9, 104)   (FL1(J),J=1,8)
      OPEN ( UNIT= IN1, FILE= FL1(1), STATUS= 'OLD')
      WRITE ( IN1, 105) KEYBAD
      REWIND 1
      OPEN ( UNIT=  IN2, FILE= FL1(2), STATUS= 'OLD')    !NOIT
      OPEN ( UNIT=  IN3, FILE= FL1(3), STATUS= 'OLD')    !PMINCR.DAT
	OPEN ( UNIT=  IN4, FILE= FL1(4), STATUS= 'OLD')    !RFMG.DAT
      OPEN ( UNIT=  IN5, FILE= FL1(5), STATUS= 'OLD')    !SUM1.DAT
      OPEN ( UNIT=  IN6, FILE= FL1(6), STATUS= 'OLD')    !SUM2.DAT
      OPEN ( UNIT=  NO1, FILE= FL1(7), STATUS= 'REPLACE')    !SUM1.UPD
      OPEN ( UNIT=  NO2, FILE= FL1(8), STATUS= 'REPLACE')    !SUM2.UPD
C********************************************************************
      READ (IN2,101,END=98,ERR=198) NOIT
      READ (IN3,102,END=98,ERR=198) VI,ECC,OMD,POT1,POT2,
     $QS,BA1,BA2,GD1,GD2,TE1,TE2,US1,US2,VL3,SGMA,SGMAP
	READ (IN4,101) NWL
	READ (IN4,110) (RFMG(K),K=1,NWL)
  110 FORMAT (5F10.5)
      J=1
      IF (NOIT.EQ.0) GO TO 1
      NU=5
      CALL DUMMY (3)
      DO J=1,NOIT
      READ (IN5,100) NOITA,VIV(J),QSV(J),ECCV(J),OMDV(J),POT1V(J),
     $POT2V(J),TE1V(J),TE2V(J)
      END DO
      NU=6
      CALL DUMMY(3)
      DO J=1,NOIT
      READ (IN6,107) NOITA,BA1V(J),BA2V(J),GD1V(J),GD2V(J),VL3V(J),
     $RFM(J),SGMAV(J),SGMAPV(J)
      END DO
C
    1 WRITE (NO1, 108)
      IF (NOIT.EQ.0) GO TO 2
      DO J=1,NOIT
      WRITE (NO1,100) J-1,VIV(J),QSV(J),ECCV(J),OMDV(J),POT1V(J),
     $POT2V(J),TE1V(J),TE2V(J)
      END DO
    2 WRITE (NO1,100) J-1,VI,QS,ECC,OMD,POT1,POT2,TE1,TE2
C
      WRITE (NO2, 109)
	IF (NOIT.EQ.0) GO TO 3
      DO J=1,NOIT
      WRITE (NO2,107) J-1,BA1V(J),BA2V(J),GD1V(J),GD2V(J),VL3V(J),
     $RFM(J),SGMAV(J),SGMAPV(J)
      END DO
    3 WRITE (NO2,107) J-1,BA1,BA2,GD1,GD2,VL3,RFMG(1),SGMA,SGMAP
C
C*******************************************************************
      CLOSE ( UNIT=  IN2, STATUS= 'KEEP')
      CLOSE ( UNIT=  IN3, STATUS= 'KEEP')
      CLOSE ( UNIT=  IN4, STATUS= 'KEEP')
	CLOSE ( UNIT=  IN5, STATUS= 'KEEP')
      CLOSE ( UNIT=  IN6, STATUS= 'KEEP')
      CLOSE ( UNIT=  NO1, STATUS= 'KEEP')
      CLOSE ( UNIT=  NO2, STATUS= 'KEEP')
      WRITE ( IN1, 105) KEYGOOD
      CLOSE ( UNIT=  IN1, STATUS= 'KEEP')
C*******************************************************************
      STOP
   98 CALL PMDSTOP
  198 CALL PMDSTOP
      END PROGRAM SUMMARY
