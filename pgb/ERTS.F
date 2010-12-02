      SUBROUTINE ERTS(I,ITH,PHV,ER,    
     $TARY,IRC,IFC,NKP,XC,YC,AGLD,RDS,VRHO,ISV,JJ,IRD)
C     SRT, given the mesh point coordinates ITH and PHV on the
C     eclipsed star, to calculate the position angle AGLD of the
C     vector from the plane of the sky center of the eclipsing
C     star to the mesh point on the eclipsed star, the radius of
C     the eclipsing star at that position angle, RDS, and the distance
C     VRHO from the center of the eclipsing star to the mesh point
C     on the eclipsed star. 
	INCLUDE 'COMA.FOR'
      DIMENSION TARY(MTR,MTR)      
      DATA TWPI/6.2831853071795864769D0/
      CALL SGLTE(I,ITH,PHV,ER,SNP,CSP,RVLD)   
      SN=SNT(I,ITH)     
      ANV=ANU(I,ITH,1)     
C     Produce rectangular coods. relative to center of star.
C     This is the eclipsed star.
      X=RVLD*SN*CSP      
      Y=RVLD*SN*SNP      
      Z=RVLD*ANV   
      CALL PROJ(TARY,X,Y,Z,XP,YP,ZP)  
      A=XP
      B=YP
C     Translate coods. so they are relative to center of the eclipsing
C     star.
      IF (I.EQ.ISV) GO TO 260
      A=XC+XP    
      B=YC+YP    
  260 IF (I.NE.ISV) GO TO 247  
      A=XP-XC    
      B=YP-YC    
C     VRHO is the projected radius,on the plane of the sky,
C     of point X,Y,Z,measured from the center of the eclipsing
C     star.X,Y,Z is a point on the eclipsed star.XP,YP is the
C     location of the center of the eclipsing star relative to
C     the center of the eclipsed star, whose coods. are (0,0).
  247 CONTINUE
      CALL RCTPOL(IRD,A,B,VRHO1,AGLD1,1)
	CALL RCTPOL(IRD,A,B,VRHO2,AGLD2,2)
	IF ((AGLD1.GE.ALP(1,1)).AND.(AGLD1.LE.ALP(IFC,1))) THEN
		AGLD=AGLD1
		VRHO=VRHO1
	END IF
	IF ((AGLD1.LE.ALP(1,1)).AND.(AGLD1.GE.ALP(IFC,1))) THEN
		AGLD=AGLD1
		VRHO=VRHO1
	END IF
	IF ((AGLD1.GE.ALP(1,2)).AND.(AGLD1.LE.ALP(IRC,2))) THEN
		AGLD=AGLD1
		VRHO=VRHO1
	END IF
	IF ((AGLD1.LE.ALP(1,2)).AND.(AGLD1.GE.ALP(IRC,2))) THEN
		AGLD=AGLD1
		VRHO=VRHO1
	END IF
	IF ((AGLD2.GE.ALP(1,2)).AND.(AGLD2.LE.ALP(IRC,2))) THEN
		AGLD=AGLD2
		VRHO=VRHO2
	END IF
	IF ((AGLD2.LE.ALP(1,2)).AND.(AGLD2.GE.ALP(IRC,2))) THEN
		AGLD=AGLD2
		VRHO=VRHO2
	END IF
	IF ((AGLD2.GE.ALP(1,1)).AND.(AGLD2.LE.ALP(IFC,1))) THEN
		AGLD=AGLD2
		VRHO=VRHO2
	END IF
	IF ((AGLD2.LE.ALP(1,1)).AND.(AGLD2.GE.ALP(IFC,1))) THEN
		AGLD=AGLD2
		VRHO=VRHO2
	END IF
C      IF (A.LT.0.0) GO TO 30
C     LARGE THETA BRANCH
C      CALL RCTPOL(IRD,A,B,VRHO,AGLD,2)
C      GO TO 36
C     SMALL THETA BRANCH
C   30 CALL RCTPOL(IRD,A,B,VRHO,AGLD,1)
   36 JJ=4   
      IF ((AGLD.GE.ALP(1,1)).AND.(AGLD.LE.ALP(IFC,1))) JJ=1
	IF ((AGLD.LE.ALP(1,1)).AND.(AGLD.GE.ALP(IFC,1))) JJ=1
         IF (JJ.EQ.1) GO TO 16
      IF ((AGLD.LE.ALP(1,2)).AND.(AGLD.GE.ALP(IRC,2))) JJ=2
	IF ((AGLD.GE.ALP(1,2)).AND.(AGLD.LE.ALP(IRC,2))) JJ=2
         IF (JJ.EQ.2) GO TO 16
      IF (NKP.GT.1) THEN
         IF ((AGLD.GE.ALP(1,3)).AND.(AGLD.LE.ALP(NKP,3))) JJ=3
            IF (JJ.EQ.3) GO TO 16
         IF ((AGLD.LE.ALP(1,3)).AND.(AGLD.GE.ALP(NKP,3))) JJ=3
            IF (JJ.EQ.3) GO TO 16
      END IF
      ANGLE=AGLD+TWPI
         IF ((ANGLE.GE.ALP(1,1)).AND.(ANGLE.LE.ALP(IFC,1))) JJ=1
            IF (JJ.EQ.1) THEN
               AGLD=ANGLE
               GO TO 16
            END IF
         IF ((ANGLE.LE.ALP(1,2)).AND.(ANGLE.GE.ALP(IRC,2))) JJ=2
            IF (JJ.EQ.2) THEN
               AGLD=ANGLE
               GO TO 16
            END IF
         IF (NKP.GT.2) THEN
            IF ((ANGLE.GE.ALP(1,3)).AND.(ANGLE.LE.ALP(NKP,3))) JJ=3 
               IF (JJ.EQ.3) THEN
                  AGLD=ANGLE
                  GO TO 16
               END IF
            IF ((ANGLE.LE.ALP(1,3)).AND.(ANGLE.GE.ALP(NKP,3))) JJ=3
               IF (JJ.EQ.3) THEN
                  AGLD=ANGLE
                  GO TO 16
               END IF
         END IF
      ANGLE=AGLD-TWPI
         IF ((ANGLE.GE.ALP(1,1)).AND.(ANGLE.LE.ALP(IFC,1))) JJ=1
            IF (JJ.EQ.1) THEN
               AGLD=ANGLE
               GO TO 16
            END IF
         IF ((ANGLE.LE.ALP(1,2)).AND.(ANGLE.GE.ALP(IRC,2))) JJ=2
            IF (JJ.EQ.2) THEN
               AGLD=ANGLE
               GO TO 16
            END IF
         IF (NKP.GT.1) THEN
            IF ((ANGLE.GE.ALP(1,3)).AND.(ANGLE.LE.ALP(NKP,3))) JJ=3
               IF (JJ.EQ.3) THEN
                  AGLD=ANGLE
                  GO TO 16
               END IF
            IF ((ANGLE.LE.ALP(1,3)).AND.(ANGLE.GE.ALP(NKP,3))) JJ=3
               IF (JJ.EQ.3) THEN
                  AGLD=ANGLE
                  GO TO 16
               END IF
         END IF
      IF (DABS(AGLD-ALP(1,1)).LT.1.D-9) THEN
         RDS=RS(1,1)
         GO TO 60
      END IF   
      IF (DABS(AGLD-ALP(IFC,1)).LT.1.D-9) THEN
         RDS=RS(IFC,1)
         GO TO 60
      END IF   
      IF (DABS(AGLD-ALP(IRC,2)).LT.1.D-9) THEN
         RDS=RS(IRC,2)
         GO TO 60
      END IF   
      IF (NKP.GT.2) THEN
         IF (DABS(AGLD-ALP(1,3)).LT.1.D-5) THEN
            RDS=RS(1,3)
            GO TO 60
         END IF
         IF (DABS(AGLD-ALP(NKP,3)).LT.1.D-5) THEN
            RDS=RS(NKP,3)
            GO TO 60
         END IF
      END IF
   16 GO TO (11,12,13,14) JJ
C     Given the position angle AGLD on the eclipsing star and
C     the parabola arrays ARYA, ARYB, ARYC, SRT RHO returns
C     the radius of the eclipsing star at that position angle.
   11 CALL RHO(AGLD,ARYA,ARYB,ARYC,ALP,RS,1,IRC,IFC,NKP,RDS)
      GO TO 60
   12 CALL RHO(AGLD,ARYA,ARYB,ARYC,ALP,RS,2,IRC,IFC,NKP,RDS)
      GO TO 60
   13 CALL RHO(AGLD,ARYA,ARYB,ARYC,ALP,RS,3,IRC,IFC,NKP,RDS)
      GO TO 60                                                                                  
   14 CALL PMDSTOP
   60 RETURN     
      END 
