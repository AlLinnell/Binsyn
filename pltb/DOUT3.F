      SUBROUTINE DOUT3(AGLMT1,I,IEC,IFC,ISV,AGLMT2,
     $IRC,NKP,XG,YG,ICOUNT,LCT,XCM,YCM,
     $AGLSV2,CSPJ,NPT,ISEQ,SNPJ,KS,NOUT1,NOUT8,NOUT11)
	INCLUDE 'COMA.FOR'
      DIMENSION LCT(16)
      DATA TWPI/6.2831853071796D0/
      DATA ANG/6.29D0/
	DATA ERLT/1.D-13/
C*********************************************************************
C     DRAW OUTLINE      
C*********************************************************************
C  
C    IF I=ISV,WE HAVE THE STAR AT THE ORIGIN OF COODS.     
C  
C
  193 FORMAT (2F12.5)
      IF (LCT(4).NE.1) GO TO 424
C  
C     NOW DO SECOND SEGMENT IF NECESSARY     
C
C     ICOUNT.EQ.1 gives the eclipsing star. In this case there never
C     is a second segment.
      IF (ICOUNT.EQ.1) GO TO 424
      IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(DABS(AGLMT2+ANG).LE.ERLT)) 
     $GO TO 424
C     If there is a second segment there must be two roots. In that
C     case neither AGLMT1 nor AGLMT2 may = DABS(ANG)
      IF ((DABS(AGLMT1-DABS(ANG)).LE.ERLT).OR.
     $(DABS(AGLMT2-DABS(ANG)).LE.ERLT)) GO TO 424
C     If one root is on one branch and the other root is on the other
C     branch, there is no second segment.
      IF ((AGLMT2.LE.0.D0).AND.(AGLMT1.GE.0.D0)) GO TO 424
      IF ((AGLMT2.GE.0.D0).AND.(AGLMT1.LE.0.D0)) GO TO 424 
C     If the range is very small, do not plot
      IF (DABS(AGLMT2-AGLMT1).LT.0.005D0) GO TO 424      
C     We have eliminated the cases where there is not a second segment.
C     If AGLSV2 is positive, we have the small theta branch.
C     If AGLSV2 is negative, we have the large theta branch.
      IF (AGLSV2.GE.0.D0) GO TO 100
C     We have the small theta branch and AGLSV2 is negative.
C     ALP(1,2) and ALP(IRC,2) are .LE. 0.D0
      K=0 
      AGL2=ALP(IRC,2)
      AGL1=AGLSV2  
      IF (DABS(AGL1-AGL2).LT.0.001D0) GO TO 424
      CFF=NPT-1
      RG=(AGL2-AGL1)/CFF
      JST=1
  314 DO 228 J=JST,NPT
      CFF=J-1
      ANGLE=AGL1+CFF*RG
      IF ((ANGLE.GT.ALP(1,2)).OR.(ANGLE.LT.ALP(IRC,2))) CALL PMDSTOP
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $GO TO 213
      IF (ISV.EQ.IEC) GO TO 330
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 340
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 341
      CALL PMDSTOP      
  340 IF (ANGLE.LT.(AGLMT2+TWPI)) GO TO 213
      GO TO 228  
  341 IF (ANGLE.LT.AGLMT2) GO TO 213  
      GO TO 228
  330 IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 333
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 332
      CALL PMDSTOP      
  332 IF (ANGLE.LE.AGLMT2) GO TO 213
      GO TO 228
  333 IF (ANGLE.GT.AGLMT2) GO TO 213
      GO TO 228
  213 CONTINUE
      CALL RHO(ANGLE,ARYA,ARYB,ARYC,ALP,RS,2,IRC,IFC,NKP,VRHO)
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
      IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
      XP=B
      IF (I.EQ.ISV) GO TO 227  
      IF (ISV.NE.IEC) GO TO 247
      YP=YP-YG   
      XP=XP-XG   
      GO TO 227  
  247 YP=YP+YG   
      XP=XP+XG   
  227 CONTINUE   
      K=K+1      
      XV(K)=XP   
      YV(K)=YP   
      IF (I.NE.ISV) GO TO 94   
   94 IF (LCT(15).EQ.1) GO TO 228     
      YV(K)=XP*CSPJ-YP*SNPJ    
      XV(K)=XP*SNPJ+YP*CSPJ    
  228 CONTINUE
C     Draw outline
      IF (ICOUNT.EQ.1) THEN
		DO L=1,K
		X=XV(L)
		Y=YV(L)
		WRITE (NOUT8, 193) X-XCM, -Y+YCM
		END DO
	END IF
      IF (ICOUNT.EQ.2) THEN
		DO L=1,K
		X=XV(L)
		Y=YV(L)
		WRITE (NOUT11, 193) X-XCM, -Y+YCM
		END DO
	END IF
      GO TO 424
C     We have the large theta branch and AGLSV2 is positive
C     ALP(1,1) and ALP(IFC,1) are .GE. 0.D0
  100 K=0
      AGL2=ALP(IFC,1)
      AGL1=AGLSV2
      IF (DABS(AGL2-AGL1).LT.0.001D0) GO TO 424
      CFF=NPT-1
      RG=(AGL2-AGL1)/CFF
      JST=1
      DO 113 J=JST,NPT
      CFF=J-1
      ANGLE=AGL1+CFF*RG
      IF ((ANGLE.LT.(ALP(1,1)-1.D-10)).OR.(ANGLE.GT.
     $(ALP(IFC,1)+1.D-10)))THEN 
         CALL PMDSTOP
      END IF   
      IF (ISV.EQ.IEC) GO TO 107
      IF ((AGLMT1.GE.0.D0).AND.(AGLMT2.GE.0.D0)) GO TO 102
      IF ((AGLMT1.LT.0.D0).AND.(AGLMT2.LT.0.D0)) GO TO 101
      CALL PMDSTOP
  101 IF (ANGLE.GT.(AGLMT2-TWPI)) THEN
      ANGLE=ANGLE-TWPI
      GO TO 110
      END IF
      GO TO 113
  102 IF (ANGLE.GT.AGLMT2) GO TO 110
      GO TO 113
  107 IF ((AGLMT1.GE.0.D0).AND.(AGLMT2.GE.0.D0)) GO TO 109
      IF ((AGLMT1.LT.0.D0).AND.(AGLMT2.LT.0.D0)) GO TO 108
      CALL PMDSTOP
  108 IF (ANGLE.GT.(AGLMT2-TWPI)) THEN
      ANGLE=ANGLE-TWPI
      GO TO 110
      END IF
      GO TO 113
  109 IF (ANGLE.GT.AGLMT2) GO TO 113
      GO TO 110
  110 CONTINUE
      CALL RHO(ANGLE,ARYA,ARYB,ARYC,ALP,RS,1,IRC,IFC,NKP,VRHO)
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
      XP=B
      IF (I.EQ.ISV) GO TO 112
      IF (ISV.NE.IEC) GO TO 111
      YP=YP-YG
      XP=XP-XG
      GO TO 112
  111 YP=YP+YG
      XP=XP+XG
  112 CONTINUE
      K=K+1
      XV(K)=XP
      YV(K)=YP
      IF (LCT(15).EQ.1) GO TO 113
      YV(K)=XP*CSPJ-YP*SNPJ
      XV(K)=XP*SNPJ+YP*CSPJ
  113 CONTINUE
      IF (KS.GT.1) GO TO 424 
      IF (ICOUNT.EQ.1) THEN
		DO L=1,K
		X=XV(L)
		Y=YV(L)
		WRITE (NOUT8, 193) X-XCM, -Y+YCM
		END DO
	END IF
      IF (ICOUNT.EQ.2) THEN
		DO L=1,K
		X=XV(L)
		Y=YV(L)
		WRITE (NOUT11, 193) X-XCM, -Y+YCM
		END DO
	END IF
  424 RETURN
      END
