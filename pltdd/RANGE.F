      SUBROUTINE RANGE(ANGLEP,J,IRC,IFC,NKP, 
     $YG,XG,IRCP,IFCP,NKPP,ANGLE,ERR,IRD)
C     SUBROUTINE USED BY PROGRAM PLOTB
C    AGLM1 IS THE INITIAL LIMITING ANGLE ON THE LARGE THETA BRANCH
C    AGLM2 IS THE FINAL LIMITING ANGLE ON THE LARGE THETA BRANCH  
C     ISV=STAR WHOSE CENTER IS AT THE ORIGIN OF COODS.
C     ISV ALSO = THE STAR ECLIPSED AT PHASE 0.0
C     IEC=THE ECLIPSING STAR
C     THE STAR NOT AT THE ORIGIN HAS ITS CENTER AT (YG,XG) (PROJECTED)
C     ANGLEP ALWAYS IS MEASURED ON THE ECLIPSED STAR
C     ANGLE ALWAYS IS MEASURED ON THE ECLIPSING STAR
C     ARYA,ARYB,ARYC,ALP,RS GO WITH THE ECLIPSED COMPONENT
C     ARYAS,ARYBS,ARYCS,ALPS,RSS GO WITH THE ECLIPSING COMPONENT
C     J=1 FOR THE LARGE THETA BRANCH ON THE STAR AT (YG,XG)
C     J=2 FOR THE SMALL THETA BRANCH ON THE STAR AT (YG,XG)
C     J=3 FOR THE NECK REGION ON THE STAR AT (YG,XG)
C     IF ISEQ=1, WE ARE IN THE FIRST HALF OF THE LIGHT CURVE, PSI.LT.PI
C     IF ISEQ=2, WE ARE IN THE SECOND HALF OF THE LIGHT CURVE, PSI.GE.PI
C     IF IRD=-1, YR AND YF ENTRIES ON ECLIPSING STAR ARE SORTED INTO
C          INCREASING SEQUENCE
C     IF IRD=+1, YR AND YF ENTRIES ON ECLIPSING STAR ARE SORTED INTO
C          DECREASING SEQUENCE
	INCLUDE 'COMA.FOR'
      DATA PI/3.1415926535898D0/
      DATA TWPI/6.2831853071796D0/
      DATA PIO2/1.5707963267949D0/
	DATA ERLT/1.D-13/
C     CALCULATE PROJECTED RADIUS FOR ECLIPSED COMPONENT
      IF (J.EQ.1) JJ=IFC
      IF (J.EQ.2) JJ=IRC
      IF (J.EQ.3) JJ=NKP
C     RHOV calculates VRHOC, the distance from the center of the eclipsed
C     star to the eclipsed star horizon point at position angle ANGLEP
    6 CALL RHOV(ANGLEP,ARYA,ARYB,ARYC,ALP,RS,J,IRC,IFC,NKP,VRHOC)  
      A=VRHOC*DCOS(ANGLEP)      
      IF (IRD.EQ.(-1)) A=-A
      B=VRHOC*DSIN(ANGLEP)      
C     Displace origin to center of eclipsing component, for present calculation
      YP=A-YG    
      XP=B-XG    
C     We now have the coods. of the point on the projected boundary
C     of the eclipsed star,relative to the center of the eclipsing star
C     Now calculate the radial distance from the center of the eclipsing
C     star and compare that distance with the
C     projected boundary point of the eclipsing star,for the same position
C     angle.
    5 VRHOV=DSQRT(XP**2+YP**2)  
C     VHROV=dist.from the center of the eclipsing star to the point on the 
C     proj.boundary of the eclipsed star
C     Calculate the position angle of the point,relative to the 
C     center of the eclipsing star
C     ANGLE=position angle of point in question,rel.to center of eclipsing star
      ANGLE=0.0D0
      IF ((XP.GE.0.0D0).AND.(DABS(YP).LE.ERLT)) ANGLE=PIO2
      IF (DABS(ANGLE-PIO2).LE.ERLT) GO TO 36
      IF ((XP.LT.0.0D0).AND.(DABS(YP).LE.ERLT)) ANGLE=-PIO2
      IF (DABS(ANGLE+PIO2).LE.ERLT) GO TO 36
      ANGLE=DATAN(XP/DABS(YP))
C     Allow for order of points on horizon of eclipsing star
      IF ((YP.GT.0.0D0).AND.(IRD.EQ.(-1))) GO TO 35
      IF ((YP.LT.0.0D0).AND.(IRD.EQ.1)) GO TO 35
      IF ((YP.LT.0.0D0).AND.(IRD.EQ.(-1))) GO TO 36
      IF ((YP.GT.0.0D0).AND.(IRD.EQ.1)) GO TO 36
   35 ANGLE=-PI-ANGLE
C     Now calculate the proj.radius of the eclipsing star
C     for position angle ANGLE
C     Determine what branch to use on this star
C     Choose the most remote branch from the center of the star
   36 JJ=4
      IF (ANGLE.LT.0.D0) THEN
         TS=ANGLE+TWPI
         IF ((TS.GE.ALPS(1,1)).AND.(TS.LE.ALPS(IFCP,1))) THEN
            ANGLE=TS
         END IF
      ELSE
         TS=ANGLE-TWPI
         IF ((TS.LE.ALPS(1,2)).AND.(TS.GE.ALPS(IRCP,2))) THEN
            ANGLE=TS
         END IF
      END IF
      IF (DABS(ALPS(1,1)-ANGLE).LT.1.D-10) THEN
         VRHO=RSS(1,1)
         GO TO 3
      END IF
      IF (DABS(ALPS(IFCP,1)-ANGLE).LT.1.D-10) THEN
         VRHO=RSS(IFCP,1)
         GO TO 3
      END IF
      IF (DABS(ALPS(1,2)-ANGLE).LT.1.D-10) THEN
         VRHO=RSS(1,2)
         GO TO 3
      END IF
      IF (DABS(ALPS(IRCP,2)-ANGLE).LT.1.D-10) THEN
         VRHO=RSS(IRCP,2)
         GO TO 3
      END IF
      AGLV=0.0D0
      IF ((ANGLE.GE.ALPS(1,1)).AND.(ANGLE.LE.ALPS(IFCP,1)))
     $JJ=1
      IF ((ANGLE.LE.ALPS(1,1)).AND.(ANGLE.GE.ALPS(IFCP,1)))
     $JJ=1
      IF (JJ.EQ.1) GO TO 7
      IF ((ANGLE.GE.ALPS(1,2)).AND.(ANGLE.LE.ALPS(IRCP,2)))
     $JJ=2
      IF ((ANGLE.LE.ALPS(1,2)).AND.(ANGLE.GE.ALPS(IRCP,2)))
     $JJ=2
      IF (JJ.EQ.2) GO TO 7
      AGLV=ANGLE+TWPI
      TS=ALPS(IFCP,1)-ALPS(IRCP,2)
      IF (DABS(TWPI-TS).LT.0.00001D0) JJ=2
      IF (JJ.EQ.2) ANGLE=AGLV
      IF (JJ.EQ.2) GO TO 7
      IF ((AGLV.GE.ALPS(1,1)).AND.(AGLV.LE.ALPS(IFCP,1))) JJ=1
      IF ((AGLV.LE.ALPS(1,1)).AND.(AGLV.GE.ALPS(IFCP,1))) JJ=1
      IF ((AGLV.GE.ALPS(1,2)).AND.(AGLV.LE.ALPS(IRCP,2))) JJ=2
      IF ((AGLV.LE.ALPS(1,2)).AND.(AGLV.GE.ALPS(IRCP,2))) JJ=2
      IF (JJ.EQ.1) ANGLE=AGLV
      IF (JJ.EQ.1) GO TO 7
      IF (JJ.EQ.2) ANGLE=AGLV
      IF (JJ.EQ.2) GO TO 7
      AGLV=ANGLE-TWPI
      IF ((AGLV.LE.ALPS(1,2)).AND.(AGLV.GE.ALPS(IRCP,2))) JJ=2
      IF ((AGLV.GE.ALPS(1,2)).AND.(AGLV.LE.ALPS(IRCP,2))) JJ=2
      IF (JJ.EQ.2) ANGLE=AGLV
      IF (JJ.EQ.2) GO TO 7
      IF ((ANGLE.LE.ALPS(1,3)).AND.(ANGLE.GE.ALPS(NKPP,3))) JJ=3
      IF (JJ.EQ.3) GO TO 7
      IF ((AGLV.LE.ALPS(1,3)).AND.(AGLV.GE.ALPS(NKPP,3))) JJ=4
      IF (JJ.EQ.4) GO TO 7
      IF ((ANGLE.LE.ALPS(IRCP,2)).AND.(ANGLE.GE.ALPS(1,3))) JJ=3
      IF (JJ.EQ.3) GO TO 7
      AGLV=TWPI+ALPS(NKPP,3)
      IF ((ANGLE.GE.ALPS(IFCP,1)).AND.(ANGLE.LE.AGLV)) JJ=2
      IF (JJ.EQ.2) GO TO 7
      IF ((ANGLE.LE.ALPS(IRCP,2)).AND.(ANGLE.GE.ALPS(1,3))) JJ=1
      IF (JJ.EQ.1) GO TO 7
      IF (NKPP.GE.1) JJ=3
      IF (JJ.EQ.3) GO TO 7
      AGLV=TWPI-ALPS(IFCP,1)+ALPS(IRCP,2)
      IF (AGLV.GT.0.30D0) CALL PMDSTOP
      IF (ANGLE.LT.0.0D0) JJ=2
      IF (ANGLE.GE.0.0D0) JJ=1
    7 GO TO (11,12,13,14) JJ
C     USE LARGE THETA BRANCH   
   11 CALL RHOV(ANGLE,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCP,IFCP,NKPP,
     $VRHO)    
      GO TO 3    
C    USE SMALL THETA BRANCH    
   12 CALL RHOV(ANGLE,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCP,IFCP,NKPP,VRHO)
      GO TO 3
   13 CALL RHOV(ANGLE,ARYAS,ARYBS,ARYCS,ALPS,RSS,3,IRCP,IFCP,NKPP,VRHO)
      GO TO 3
   14 CALL RHOV(AGLV,ARYAS,ARYBS,ARYCS,ALPS,RSS,3,IRCP,IFCP,NKPP,VRHO)
    3 ERR=VRHOV-VRHO
   60 RETURN     
      END 
