      SUBROUTINE CONTACT(VI,RS,RG,SMA,OMRAD,ECC,VCONJ,VSEC,THFCP,
     $THSCP,THTCP,THLCP,THFCS,THSCS,THTCS,THLCS)
C     SRT TO CALCULATE ANGLES(PHASE) OF FIRST THROUGH FOURTH CONTACT
C     FOR PRIMARY AND SECONDARY ECLIPSE
C     THIS USES A SPHERICAL STAR APPROXIMATION      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PIO2/1.5707963267948966192D0/
      DATA TWPI/6.2831853071795864769D0/
      DATA DLTTH/0.01D0/
      DATA XACC/0.0001D0/
	DATA ERLT/1.D-13/
	THFCP=0.D0
	THSCP=0.D0
	THTCP=0.D0
	THLCP=0.D0
	THFCS=0.D0
	THSCS=0.D0
	THTCS=0.D0
	THLCS=0.D0
      SVI=DSIN(VI)**2
      OMESQ=1.D0-ECC**2
      ALPHAE=(RS+RG)/SMA/OMESQ
      ALPHAI=(RG-RS)/SMA/OMESQ
      CPHIZE=DSQRT((1.D0-ALPHAE**2)/SVI)
      CPHIZI=DSQRT((1.D0-ALPHAI**2)/SVI)
      IF (DABS(CPHIZE).LE.1.D0) THEN
		PHIZE=DACOS(CPHIZE)
	ELSE
		PHIZE=0.D0
	END IF
	IF (DABS(PHIZE).LE.ERLT) GO TO 16
      PHIZI=0.D0
      IF((-1.D0.LE.CPHIZI).AND.(CPHIZI.LE.1.D0)) PHIZI=DACOS(CPHIZI)
      PHIZIST=PHIZI
      IF (DABS(PHIZI).LE.ERLT) THEN
         CALL FNDLT(0.D0,OMRAD,ECC,SVI,ALPHAI,FN1)
         IF (FN1.GT.0.D0) THEN
            DO J=1,500
            PHIZI=PHIZI+DLTTH/10.D0
            CALL FNDLT(PHIZI,OMRAD,ECC,SVI,ALPHAI,FN1)
               IF (FN1.LT.0.D0) THEN
                  PHIZI=PHIZI-DLTTH/10.D0
                  GO TO 20
               END IF
            END DO
            CALL PMDSTOP
         END IF
      END IF
   20 T1E=DSIN(PHIZE)
      T1I=DSIN(PHIZI)
      T3E=(T1E**2+1.D0/DTAN(VI)**2)*ECC/T1E
      IF(T1I.GT.1.D-33) T3I=(T1I**2+1.D0/DTAN(VI)**2)*ECC/T1I
      T21E=DSIN(OMRAD+PHIZE)
      T21I=DSIN(OMRAD+PHIZI)
      T22E=DSIN(OMRAD-PHIZE)
      T22I=DSIN(OMRAD-PHIZI)
C     Primary minimum
C     First, last contact
      SPHIFCP=T1E+T21E*T3E
      SPHILCP=T1E+T22E*T3E
      PHIFCP=DASIN(SPHIFCP)
      PHILCP=DASIN(SPHILCP)
      VFCP=VCONJ-PHIFCP
      VLCP=VCONJ+PHILCP
      THFCP=VFCP+OMRAD-PIO2
      IF(THFCP.LT.0.D0) THFCP=THFCP+TWPI
      IF(THFCP.GE.TWPI) THFCP=THFCP-TWPI
C     Refine value for THFCP
      THFCPSV=THFCP
      DTH=DLTTH
      TH1=THFCP+DTH
      TH2=THFCP
      TH3=THFCP-DTH
      LPCT=0
    1 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAE,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THFCP=TH2
         GO TO 2
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THFCP=TH1
         GO TO 2
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THFCP=TH3
         GO TO 2
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH2 and TH3. Approach from outside contact,
C     starting at TH3 and moving toward increasing position angle.
         DTH=DABS(TH2-TH3)/50.D0
         DO J=1,52
            TH3=TH3+DTH
            CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN3)
            IF (FN3.GT.0.D0) THEN
               TH1=TH3
               TH3=TH3-DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 1
               THFCP=THFCPSV
               GO TO 2
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 1
            THFCP=THFCPSV
            GO TO 2
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 1
            THFCP=THFCPSV
            GO TO 2
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.200) GO TO 1
            THFCP=THFCPSV
            GO TO 2
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THFCP=TH1
            GO TO 2
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THFCP=TH2
            GO TO 2
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THFCP=TH3
            GO TO 2
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAE,XACC,THFCP)
         IF (THFCP.LT.0.D0) THFCP=THFCP+TWPI
         IF (THFCP.GE.TWPI) THFCP=THFCP-TWPI
      END IF
    2 THLCP=VLCP+OMRAD-PIO2
      IF(THLCP.LT.0.D0) THLCP=THLCP+TWPI
      IF(THLCP.GT.TWPI) THLCP=THLCP-TWPI
C     Refine value of THLCP
      DTH=DLTTH
      LPCT=0
      TH1=THLCP+DTH
      TH2=THLCP
      TH3=THLCP-DTH
      THLCPSV=THLCP
    3 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAE,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN2)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THLCP=TH2
         GO TO 4
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THLCP=TH1
         GO TO 4
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THLCP=TH3
         GO TO 4
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH1 and TH2. Approach from outside contact,
C     starting at TH1 and moving toward decreasing position angle.
         DTH=DABS(TH1-TH2)/50.D0
         DO J=1,52
            TH1=TH1-DTH
            CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAE,FN1)
            IF (FN1.GT.0.D0) THEN
               TH3=TH1
               TH1=TH1+DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 3
               THLCP=THLCPSV
               GO TO 4
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 3
            THLCP=THLCPSV
            GO TO 4
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 3
            THLCP=THLCPSV
            GO TO 4
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 3
            THLCP=THLCPSV
            GO TO 4
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THLCP=TH1
            GO TO 4
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THLCP=TH2
            GO TO 4
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THLCP=TH3
            GO TO 4
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAE,XACC,THLCP)
         IF (THLCP.LT.0.D0) THLCP=THLCP+TWPI
         IF (THLCP.GE.TWPI) THLCP=THLCP-TWPI
      END IF
C     Second, third contact, primary minimum (complete eclipse)
    4 SPHISCP=T1I+T21I*T3I
      SPHITCP=T1I+T22I*T3I
      PHISCP=DASIN(SPHISCP)
      PHITCP=DASIN(SPHITCP)
      THSCP=VCONJ-PHISCP+OMRAD-PIO2
      IF(THSCP.LT.0.D0) THSCP=THSCP+TWPI
      IF(THSCP.GE.TWPI) THSCP=THSCP-TWPI
      IF (DABS(PHIZI).LE.ERLT) THEN
         THSCP=VCONJ+OMRAD-PIO2
         IF (THSCP.LT.0.D0) THSCP=THSCP+TWPI
         IF (THSCP.GE.TWPI) THSCP=THSCP-TWPI
         GO TO 6
      END IF
C     Refine THSCP
      DTH=DLTTH
      THSCPSV=THSCP
      TH1=THSCP+DTH
      TH2=THSCP
      TH3=THSCP-DTH
    5 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAI,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAI,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAI,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THSCP=TH2
         GO TO 6
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THSCP=TH1
         GO TO 6
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THSCP=TH3
         GO TO 6
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH2 and TH3. Approach from inside contact,
C     starting at TH2 and moving toward decreasing position angle.
         DTH=DABS(TH2-TH3)/50.D0
         DO J=1,52
            TH2=TH2-DTH
            CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
            IF (FN2.LT.0.D0) THEN
               TH3=TH2
               TH1=TH2+DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 5
               THSCP=THSCPSV
               GO TO 6
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 5
            THSCP=THSCPSV
            GO TO 6
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 5
            THSCP=THSCPSV
            GO TO 6
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 5
            THSCP=THSCPSV
            GO TO 6
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THSCP=TH1
            GO TO 6
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THSCP=TH2
            GO TO 6
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THSCP=TH3
            GO TO 6
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAI,XACC,THSCP)
         IF (THSCP.LT.0.D0) THSCP=THSCP+TWPI
         IF (THSCP.GE.TWPI) THSCP=THSCP-TWPI
      END IF
    6 THTCP=VCONJ+PHITCP+OMRAD-PIO2
      IF(THTCP.LT.0.D0) THTCP=THTCP+TWPI
      IF(THTCP.GE.TWPI) THTCP=THTCP-TWPI
      IF (DABS(PHIZI).LE.ERLT) THEN
         THTCP=VCONJ+OMRAD-PIO2
         IF (THTCP.LT.0.D0) THTCP=THTCP+TWPI
         IF (THTCP.GE.TWPI) THTCP=THTCP-TWPI
         GO TO 8
      END IF
C     Refine THTCP
      THTCPSV=THTCP
      LPCT=0
      DTH=DLTTH
      TH1=THTCP+DTH
      TH2=THTCP
      TH3=THTCP-DTH
    7 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAI,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAI,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAI,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THTCP=TH2
         GO TO 8
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THTCP=TH1
         GO TO 8
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THTCP=TH3
         GO TO 8
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH1 and TH2. Approach from inside contact,
C     starting at TH2 and moving toward increasing position angle.
         DTH=DABS(TH1-TH2)/50.D0
         DO J=1,52
            TH2=TH2+DTH
            CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
            IF (FN2.LT.0.D0) THEN
               TH3=TH2-DTH
               TH1=TH2
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 7
               THTCP=THTCPSV
               GO TO 8
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 7
            THTCP=THTCPSV
            GO TO 8
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 7
            THTCP=THTCPSV
            GO TO 8
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 7
            THTCP=THTCPSV
            GO TO 8
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THTCP=TH1
            GO TO 8
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THTCP=TH2
            GO TO 8
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THTCP=TH3
            GO TO 8
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAI,XACC,THTCP)
         IF (THTCP.LT.0.D0) THTCP=THTCP+TWPI
         IF (THTCP.GE.TWPI) THTCP=THTCP-TWPI
      END IF
C     Secondary minimum
C     First, last contact
    8 SPHIFCS=T1E-T21E*T3E
      SPHILCS=T1E-T22E*T3E
      PHIFCS=DASIN(SPHIFCS)
      PHILCS=DASIN(SPHILCS)
      VFCS=VSEC-PHIFCS
      THFCS=VFCS+OMRAD-PIO2
C     Refine THFCS
      THFCSSV=THSCS
      LPCT=0
      DTH=DLTTH
      TH1=THFCS+DTH
      TH2=THFCS
      TH3=THFCS-DTH
    9 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAE,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THFCS=TH2
         GO TO 10
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THFCS=TH1
         GO TO 10
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THFCS=TH3
         GO TO 10
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH2 and TH3. Approach from outside contact,
C     starting at TH3 and moving toward increasing position angle.
         DTH=DABS(TH2-TH3)/50.D0
         DO J=1,52
            TH3=TH3+DTH
            CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN3)
            IF (FN3.GT.0.D0) THEN
               TH1=TH3
               TH3=TH3-DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 9
               THFCS=THFCSSV
               GO TO 10
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 9
            THFCS=THFCSSV
            GO TO 10
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 9
            THFCS=THFCSSV
            GO TO 10
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 9
            THFCS=THFCSSV
            GO TO 10
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THFCS=TH1
            GO TO 10
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THFCS=TH2
            GO TO 10
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THFCS=TH3
            GO TO 10
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAE,XACC,THFCS)
         IF (THFCS.LT.0.D0) THFCS=THFCS+TWPI
         IF (THFCS.GE.TWPI) THFCS=THFCS-TWPI
      END IF
   10 VLCS=VSEC+PHILCS
      THLCS=VLCS+OMRAD-PIO2
C     Refine THLCS
      THLCSSV=THLCS
      LPCT=0
      DTH=DLTTH
      TH1=THLCS+DTH
      TH2=THLCS
      TH3=THLCS-DTH
   11 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAE,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THLCS=TH2
         GO TO 12
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THLCS=TH1
         GO TO 12
      END IF
      IF (DABS(FN3).LT.0.0001) THEN
         THLCS=TH3
         GO TO 12
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH1 and TH2. Approach from outside contact,
C     starting at TH1 and moving toward decreasing position angle.
         DTH=DABS(TH1-TH2)/50.D0
         DO J=1,52
            TH1=TH1-DTH
            CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAE,FN1)
            IF (FN1.GT.0.D0) THEN
               TH3=TH1
               TH1=TH1+DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 11
               THLCS=THLCSSV
               GO TO 12
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 11
            THLCS=THLCSSV
            GO TO 12
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 11
            THLCS=THLCSSV
            GO TO 12
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 11
            THLCS=THLCSSV
            GO TO 12
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THLCS=TH1
            GO TO 12
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THLCS=TH2
            GO TO 12
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THLCS=THLCSSV
            GO TO 12
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAE,XACC,THLCS)
         IF (THLCS.LT.0.D0) THLCS=THLCS+TWPI
         IF (THLCS.GE.TWPI) THLCS=THLCS-TWPI
      END IF
C     Second, third contact
   12 PHIZI=PHIZIST
      SPHISCS=T1I-T21I*T3I
      SPHITCS=T1I-T22I*T3I
      PHISCS=DASIN(SPHISCS)
      PHITCS=DASIN(SPHITCS)
      THSCS=VSEC-PHISCS+OMRAD-PIO2
      IF (THSCS.LT.0.D0) THSCS=THSCS+TWPI
      IF (THSCS.GE.TWPI) THSCS=THSCS-TWPI
      IF (DABS(PHIZI).LE.ERLT) THEN
         THSCS=VSEC+OMRAD-PIO2
         IF (THSCS.LT.0.D0) THSCS=THSCS+TWPI
         IF (THSCS.GE.TWPI) THSCS=THSCS-TWPI
         CALL FNDLT(THSCS,OMRAD,ECC,SVI,ALPHAI,FN1)
C     If FN1 is positive, a second contact exists
         IF (FN1.GT.0.D0) THEN
            DO J=1,500
            THSCS=THSCS-DLTTH/10.D0
            CALL FNDLT(THSCS,OMRAD,ECC,SVI,ALPHAI,FN1)
               IF (FN1.LT.0.D0) THEN
                  THSCS=THSCS+DLTTH/10.D0
                  GO TO 21
               END IF
            END DO
            CALL PMDSTOP
         END IF
      END IF
      GO TO 14
C     Refine THSCS
   21 DTH=DLTTH
      THSCSSV=THSCS
      LPCT=0
      TH1=THSCS+DTH
      TH2=THSCS
      TH3=THSCS-DTH
   13 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAI,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAI,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAI,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THSCS=TH2
         GO TO 14
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THSCS=TH1
         GO TO 14
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THSCS=TH3
         GO TO 14
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH2 and TH3. Approach from inside contact,
C     starting at TH2 and moving toward decreasing position angle.
         DTH=DABS(TH2-TH3)/50.D0
         DO J=1,52
            TH2=TH2-DTH
            CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
            IF (FN2.LT.0.D0) THEN
               TH3=TH2
               TH1=TH2+DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 13
               THSCS=THSCSSV
               GO TO 14
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 13
            THSCS=THSCSSV
            GO TO 14
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 13
            THSCS=THSCSSV
            GO TO 14
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 13
            THSCS=THSCSSV
            GO TO 14
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THSCS=TH1
            GO TO 14
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THSCS=TH2
            GO TO 14
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THSCS=TH3
            GO TO 14
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAI,XACC,THSCS)
         IF (THSCS.LT.0.D0) THSCS=THSCS+TWPI
         IF (THSCS.GE.TWPI) THSCS=THSCS-TWPI
      END IF
   14 THTCS=VSEC+PHITCS+OMRAD-PIO2
      IF (THTCS.LT.0.D0) THTCS=THTCS+TWPI
      IF (THTCS.GE.TWPI) THTCS=THTCS-TWPI
      IF (DABS(PHIZI).LE.ERLT) THEN
         THTCS=VSEC+OMRAD-PIO2
         IF (THTCS.LT.0.D0) THTCS=THTCS+TWPI
         IF (THTCS.GE.TWPI) THTCS=THTCS-TWPI
         CALL FNDLT(THTCS,OMRAD,ECC,SVI,ALPHAI,FN1)
C     If FN1 is positive, a fourth contact exists
         IF (FN1.GT.0.D0) THEN
            DO J=1,500
            THTCS=THTCS+DLTTH/10.D0
            CALL FNDLT(THTCS,OMRAD,ECC,SVI,ALPHAI,FN1)
               IF (FN1.LT.0.D0) THEN
                  THTCS=THTCS-DLTTH/10.D0
                  GO TO 22
               END IF
            END DO
            CALL PMDSTOP
         END IF
      END IF
      GO TO 16
C     Refine THTCS
   22 DTH=DLTTH
      THTCSSV=THTCS
      LPCT=0
      TH1=THTCS+DTH
      TH2=THTCS
      TH3=THTCS-DTH
   15 CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHAI,FN1)
      CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAI,FN2)
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAI,FN3)
C     Watch for case of two roots between TH1 and TH3
      IF (DABS(FN2).LT.0.0001D0) THEN
         THTCS=TH2
         GO TO 16
      END IF
      IF (DABS(FN1).LT.0.0001D0) THEN
         THTCS=TH1
         GO TO 16
      END IF
      IF (DABS(FN3).LT.0.0001D0) THEN
         THTCS=TH3
         GO TO 16
      END IF
C     If FN1,FN2,or FN3 is .GT. 0.D0,the stellar disks overlap.
C     If FN1,FN2,or FN3 is .LT. 0.D0,the stellar disks do not overlap.
      IF ((FN2.GT.0.D0).AND.(FN1.LT.0.D0).AND.(FN3.LT.0.D0)) THEN
C     The root lies between TH1 and TH2. Approach from inside contact,
C     starting at TH2 and moving toward increasing position angle.
         DTH=DABS(TH1-TH2)/50.D0
         DO J=1,52
            TH2=TH2+DTH
            CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAE,FN2)
            IF (FN2.LT.0.D0) THEN
               TH1=TH2
               TH3=TH2-DTH
               TH2=0.5D0*(TH1+TH3)
               LPCT=LPCT+1
               IF (LPCT.LT.50) GO TO 15
               THTCS=THTCSSV
               GO TO 16
            END IF
         END DO
         CALL PMDSTOP
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
         IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
            DTH=0.4D0*(DABS(TH1-TH3))
            TH2=0.5D0*(TH1+TH3)
            TH1=TH2+DTH
            TH3=TH2-DTH
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 15
            THTCS=THTCSSV
            GO TO 16
         END IF
         IF (DABS(FN1).LT.DABS(FN3)) THEN
            TH3=TH2
            TH1=TH1+DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 15
            THTCS=THTCSSV
            GO TO 16
         END IF
         IF (DABS(FN1).GE.DABS(FN3)) THEN
            TH1=TH2
            TH3=TH3-DTH
            TH2=0.5D0*(TH1+TH3)
            LPCT=LPCT+1
            IF (LPCT.LT.50) GO TO 15
            THTCS=THTCSSV
            GO TO 16
         END IF
      ELSE
         IF (DABS(FN1).LT.0.0001D0) THEN
            THTCS=TH1
            GO TO 16
         END IF
         IF (DABS(FN2).LT.0.0001D0) THEN
            THTCS=TH2
            GO TO 16
         END IF
         IF (DABS(FN3).LT.0.0001D0) THEN
            THTCS=TH3
            GO TO 16
         END IF
         CALL RTFNDLT(TH1,TH3,OMRAD,ECC,SVI,ALPHAI,XACC,THTCS)
         IF (THTCS.LT.0.D0) THTCS=THTCS+TWPI
         IF (THTCS.GE.TWPI) THTCS=THTCS-TWPI
      END IF
   16 CONTINUE
      RETURN
      END
