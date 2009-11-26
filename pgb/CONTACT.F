      SUBROUTINE CONTACT(VI,RS,RG,SMA,OMRAD,ECC,VCONJ,VSEC,THFCP,
     $THSCP,THTCP,THLCP,THFCS,THSCS,THTCS,THLCS)
C     SRT TO CALCULATE ANGLES(PHASE) OF FIRST THROUGH FOURTH CONTACT
C     FOR PRIMARY AND SECONDARY ECLIPSE
C     THIS USES A SPHERICAL STAR APPROXIMATION 
C     Reference: Kopal, CLOSE BINARY SYSTEMS, p. 383ff.
C     RS=radius of smaller star
C     RG=radius of larger star
C     SMA=orbital semi-major axis
C     OMRAD=longitude of periastron, measured from the ascending node,
C     in the direction of motion.
C     ECC=orbital eccentricity
C     VCONJ=true anomaly of mid-pri. minimum.
C     VSEC=true anomaly of mid-secondary min.
C     NOTE: The above quantities are set at input to this SRT.
C     THFCP=position angle of pri. min. first contact, relative to
C     mid-pri. minimum.
C     THSCP=position angle of pri. min. second contact, relative to
C     mid-pri. minimum.
C     THTCP=position angle of pri. min. third contact, relative to
C     mid-pri. minimum.
C     THLCP=position angle of pri. min. last contact, relative to
C     mid-pri. minimum.
C     THFCS=position angle of sec. min. first contact, relative to
C     mid-pri. minimum.
C     THSCS=position angle of sec. min. second contact, relative to
C     mid-pri. minimum.
C     THTCS=position angle of sec. min. third contact, relative to
C     mid-pri. minimum.
C     THLCS+position angle of sec. min. last contact, relative to
C     mid-pri. min.    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PIO2/1.5707963267948966192D0/
      DATA TWPI/6.2831853071795864769D0/
      DATA DLTTH/0.01D0/
      DATA XACC/1.D-11/
	DATA ERLT/1.D-13/
	THFCP=0.D0
	THSCP=0.D0
	THTCP=0.D0
	THLCP=0.D0
	THFCS=VSEC+OMRAD-PIO2
	THSCS=THFCS
	THTCS=THFCS
	THLCS=THFCS
      SVI=DSIN(VI)**2
      OMESQ=1.D0-ECC**2
	OME=DSQRT(OMESQ)
C     ALPHAE is the quantity alpha (for external contact), Kopal,
C     eq. 9-27
      ALPHAE=(RS+RG)/SMA/OME
C     ALPHAI is the corresponding alpha for internal contact.
      ALPHAI=(RG-RS)/SMA/OME
C     CPHIZE is cos(phi(zero)), Kopal eq. 9-27, for external contact
      CPHIZE=DSQRT((1.D0-ALPHAE**2)/SVI)
C     CPHIZI is the corresponding quantity for internal contact
      CPHIZI=DSQRT((1.D0-ALPHAI**2)/SVI)
C     PHIZE is the angle phi(zero) in Kopal, eq. 9-27.
      IF (DABS(CPHIZE).LE.1.D0) THEN
		PHIZE=DACOS(CPHIZE)
	ELSE
		PHIZE=0.D0
	END IF
	IF (DABS(PHIZE).LE.ERLT) GO TO 16
C     PHIZI is the corresponding angle for internal contact.
      PHIZI=0.D0
      IF((-1.D0.LE.CPHIZI).AND.(CPHIZI.LE.1.D0)) PHIZI=DACOS(CPHIZI)
      PHIZIST=PHIZI
      IF ((DABS(PHIZI).LE.ERLT).AND.(DABS(CPHIZI).LE.1.D0)) THEN
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
	IF (DABS(CPHIZI).LE.1.D0) T3I=(T1I**2+1.D0/DTAN(VI)**2)*ECC/T1I
      T21E=DSIN(OMRAD+PHIZE)
      IF (DABS(CPHIZI).LE.1.D0) T21I=DSIN(OMRAD+PHIZI)
      T22E=DSIN(OMRAD-PHIZE)
      IF (DABS(CPHIZI).LE.1.D0) T22I=DSIN(OMRAD-PHIZI)
C     Primary minimum
C     First, last contact
C     Kopal, eqs. 9-28
      SPHIFCP=T1E+T21E*T3E
      SPHILCP=T1E+T22E*T3E
C     Angles phi, pri. min., first and last contact.
C     Exact first order values rather than the approximation in 
C     Kopal, eqs. 9-29.
      PHIFCP=DASIN(SPHIFCP)
      PHILCP=DASIN(SPHILCP)
C     VFCP=true anomaly of first contact, pri. min.
      VFCP=VCONJ-PHIFCP
      VLCP=VCONJ+PHILCP
C     Position angle of first contact, Kopal eq. 9-2
      THFCP=VFCP+OMRAD-PIO2
      IF(THFCP.LT.0.D0) THFCP=THFCP+TWPI
      IF(THFCP.GE.TWPI) THFCP=THFCP-TWPI
C**********************************************************************
C     NOTE: In Kopal, CBS, the condition of external contact (contacts
C     1 and 4) corresponds to delta (eq. 9-18) = RS+RG
C     The problem is to find the value of theta for which this
C     condition is satisfied. There are two values that do this,
C     corresponding to contacts 1 and 4. Similarly, the condition for
C     contacts 2 and 3 is that delta (eq. 9-18) = RG-RS.
C     The condition for minimum light is to find the two values of
C     theta which satisfy Kopal, CBS, eq. 9-19.
C     The above equations provide the first approximations, taken from
C     the indicated equations in Kopal.
C***********************************************************************
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
C***********************************************************************
C     End of first contact, primary minimum.
C***********************************************************************
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
      CALL FNDLT(TH3,OMRAD,ECC,SVI,ALPHAE,FN3)
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
C***********************************************************************
C     End of last contact, primary minimum.
C***********************************************************************
C     Second, third contact, primary minimum (complete eclipse)
C     Test for case that no second or third contact exists
    4	IF (DABS(CPHIZI).GT.1.D0) THEN
		THSCP=VCONJ+OMRAD-PIO2
		THTCP=THSCP
		GO TO 8
      END IF
C     Calculate second and third contact
      SPHISCP=T1I+T21I*T3I
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
            CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAI,FN2)
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
C***********************************************************************
C     End of second contact, primary minimum.
C***********************************************************************
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
            CALL FNDLT(TH2,OMRAD,ECC,SVI,ALPHAI,FN2)
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
C***********************************************************************
C     End of third contact, primary minimum.
C***********************************************************************
C
C
C
C***********************************************************************
C***********************************************************************
C     Secondary minimum
C***********************************************************************
C***********************************************************************
C
C
C     First, last contact
C     Check whether there is an eclipse at secondary minimum.
    8	TH1=VSEC+OMRAD-PIO2
      ALPHTS=(RS+RG)/SMA/DSQRT(1.D0-ECC**2)
	CALL FNDLT(TH1,OMRAD,ECC,SVI,ALPHTS,FN1)
C     If FN1 is <0.0, there is no eclipse at "secondary minimum"
	IF (FN1.LT.0.D0) GO TO 16
      SPHIFCS=T1E-T21E*T3E
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
C***********************************************************************
C     End of first contact, secondary minimum.
C***********************************************************************
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
C***********************************************************************
C     End of last contact, secondary minimum.
C***********************************************************************
C     Second, third contact
   12 PHIZI=PHIZIST
C     Test for case second and third contacts do not exist
      IF (DABS(CPHIZI).GT.1.D0) THEN
		THSCS=VSEC+OMRAD-PIO2
		THTCS=THSCS
		GO TO 16
	END IF
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
C***********************************************************************
C     End of second contact, secondary mnimum.
C***********************************************************************
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
C***********************************************************************
C     End of third contact, secondary minimum.
C***********************************************************************
   16 CONTINUE
      RETURN
      END
