      SUBROUTINE DOUT1(AGLMT1,I,IEC,IFC,ISV,AGLMT2,
     $IRC,NKP,XG,YG,ICOUNT,LCT,XCM,YCM,
     $CSPJ,ISEQ,SNPJ,KS,NOUT1,NOUT4,NOUT9)
	INCLUDE 'COMA.FOR'
      DIMENSION LCT(16),AR2(3),AR3(3)
      DATA PIO2/1.5707963267949D0/
      DATA TWPI/6.2831853071796D0/
      DATA ANG/6.29D0/
	DATA ERLT/1.D-13/
C*********************************************************************
C     DRAW OUTLINE      
C*********************************************************************
C  
C    IF I=ISV,WE HAVE THE STAR AT THE ORIGIN OF COODS.     
C  
C    DO LARGE THETA BRANCH     
  193 FORMAT (2F12.5)
C
      K=0
C     Take alternate route if there is no eclipse
C     If in total eclipse phase, do not plot large theta points
C     In case of total phase, AGLMT1 has been set to -ANG	 
      IF ((DABS(AGLMT1+ANG).LE.ERLT).AND.(I.NE.IEC)) GO TO 221
C
C
C     Scan ALP(J,1) for maximum entry.
      IFCM=0
	AGL1=-10.D0
	DO J=1,IFC
		IF (ALP(J,1).GT.AGL1) THEN
			IFCM=J
			AGL1=ALP(J,1)
		END IF
	END DO
      DO 220 J=1,IFC-1
      AGL1=ALP(J,1)
      RG=(ALP(J+1,1)-ALP(J,1))/16.0D0
      JLM=16
      JKL=0
      IF (K.GT.0) GO TO 232
      JLM=17
      JKL=1
  232 IF ((AGLMT1.GT.ALP(J,1)).AND.(AGLMT1.LT.ALP(J+1,1))) GO TO 244
      GO TO 246
  244 JLM=16
      JKL=0
  246 DO 422 JJ=1,JLM
      CFF=JJ-JKL
      ANGLE=AGL1+CFF*RG
C	IF (ANGLE.LT.ALP(1,1)) THEN
C		ANGLE=ALP(1,1)
C	END IF
C	IF (ANGLE.GT.ALP(IFCM,1)) THEN
C		IF (DABS(ANGLE-ALP(IFCM,1)).GT.1.0D-5) CALL PMDSTOP
C	END IF
C     IF I.EQ.IEC WE HAVE THE ECLIPSING STAR.PLOT ALL OF ITS POINTS.
      IF (I.EQ.IEC) GO TO 222    
C    DO LARGE THETA BRANCH CASE WHEN THE LIMITING ANGLES ARE ON THE      
C     SMALL THETA BRANCH
      IF ((DABS(AGLMT1-ANG).LE.ERLT).AND.(DABS(AGLMT2-ANG).LE.ERLT)) 
     $GO TO 222
      IF (ISV.EQ.IEC) GO TO 261
C     If the following statement is true, both roots are on the large
C     theta branch
      IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 230
C     If the following statement is true, both roots are on the small
C     theta branch
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 231
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 250
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 242
      CALL PMDSTOP      
  230 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222
      GO TO 220
  231 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.(AGLMT2+TWPI)))GO TO 222
      GO TO 220  
  250 IF (AGLMT2.LT.-PIO2) GO TO 260  
      IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 220
      GO TO 222
  260 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.(AGLMT2+TWPI)))GO TO 220
      GO TO 222  
  242 IF (AGLMT1.GT.-PIO2) GO TO 249  
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  249 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220
C     If the following statement is true, both roots are on the large
C     theta branch.
  261 IF ((AGLMT1.GE.0.0D0).AND.(AGLMT2.GE.0.0D0)) GO TO 262
C     If the following statement is true, both roots are on the small
C     theta branch.
      IF ((AGLMT1.LT.0.0D0).AND.(AGLMT2.LT.0.0D0)) GO TO 263
      IF ((AGLMT2.LT.0.0D0).AND.(AGLMT1.GE.0.0D0)) GO TO 264
      IF ((AGLMT2.GE.0.0D0).AND.(AGLMT1.LT.0.0D0)) GO TO 265
      CALL PMDSTOP      
  262 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  263 IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.(AGLMT2+TWPI))) GO TO 222      
      GO TO 220  
  264 IF (AGLMT2.LT.-PIO2) GO TO 266  
      IF ((ANGLE.GE.AGLMT2).AND.(ANGLE.LE.AGLMT1)) GO TO 222
      GO TO 220
  266 IF ((ANGLE.LE.AGLMT1).AND.(ANGLE.GE.AGLMT2)) GO TO 222
      GO TO 220
  265 IF (AGLMT1.LT.-PIO2) GO TO 267  
      IF ((ANGLE.GE.AGLMT1).AND.(ANGLE.LE.AGLMT2)) GO TO 222      
      GO TO 220  
  267 IF ((ANGLE.LT.AGLMT1).OR.(ANGLE.GT.AGLMT2)) GO TO 220      
      GO TO 222
  222 CONTINUE
      CALL RHO(ANGLE,ARYA,ARYB,ARYC,ALP,RS,1,IRC,IFC,NKP,VRHO)
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
      IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
      XP=B
      IF (I.EQ.ISV) GO TO 224  
      IF (ISV.NE.IEC) GO TO 243
      YP=YP-YG   
      XP=XP-XG   
      GO TO 224  
  243 YP=YP+YG   
      XP=XP+XG   
  224 CONTINUE   
      K=K+1      
      XV(K)=XP   
      YV(K)=YP   
   89 IF (LCT(15).EQ.1) GO TO 220
      YV(K)=XP*CSPJ-YP*SNPJ    
      XV(K)=XP*SNPJ+YP*CSPJ    
  422 CONTINUE
  220 CONTINUE
  223 IF ((ICOUNT.EQ.2).AND.(LCT(4).NE.1)) GO TO 221
      IF ((ICOUNT.EQ.1).AND.(LCT(12).NE.1)) GO TO 221
	IF (KS.GT.1) GO TO 221
C     Draw outline
      IF (ICOUNT.EQ.1) THEN
		DO L=1,K
		X=XV(L)
		Y=YV(L)
		WRITE (NOUT4, 193) X-XCM, -Y+YCM
		END DO
	END IF
      IF (ICOUNT.EQ.2) THEN
		DO L=1,K
		X=XV(L)
		Y=YV(L)
		WRITE (NOUT9, 193) X-XCM, -Y+YCM
		END DO
      END IF
  221 RETURN
  100 DO 110 J=1,IFC-1
      AR3(1)=ALP(J,1)
	AR3(2)=ALP(J+1,1)
	AR2(1)=RS(J,1)
	AR2(2)=RS(J+1,1)
	CALL STLINE(AR3,AR2,0,IER)
	IF (IER.EQ.129) CALL PMDSTOP
      AGL1=ALP(J,1)
	RG=(ALP(J+1,1)-ALP(J,1))/16.D0
	JLM=16
	JKL=0
	IF (K.GT.0) GO TO 101
	JLM=17
	JKL=1
  101 DO 120 JJ=1,JLM
      CFF=JJ-JKL
	ANGLE=AGL1+CFF*RG
	VRHO=AR2(1)+AR2(2)*ANGLE
      A=VRHO*DCOS(ANGLE)
      B=VRHO*DSIN(ANGLE)
      YP=A
      IF ((I.NE.ISV).AND.(ISEQ.LT.3)) YP=-YP
      IF ((I.EQ.ISV).AND.(ISEQ.GT.2)) YP=-YP
      XP=B
      IF (I.EQ.ISV) GO TO 102  
      IF (ISV.NE.IEC) GO TO 103
      YP=YP-YG   
      XP=XP-XG
	GO TO 102
  103 YP=YP+YG
      XP=XP+XG
  102 CONTINUE
      K=K+1
	XV(K)=XP
	YV(K)=YP
	IF (LCT(15).EQ.1) GO TO 110
	YV(K)=XP*CSPJ-YP*SNPJ
	XV(K)=XP*SNPJ+YP*CSPJ
  120 CONTINUE
  110 CONTINUE
      GO TO 223			   
      END
