      PROGRAM INTEGSP
C     Program to multiply a spectrum by a second spectrum and integrate the product
C     spectrum
C     The program reads in the first spectrum to OWL1,OFL1
C     The program reads in the second spectrum to OWL2,OFL2
C     It reads NSD, an integer specifying the number of subdivisions over which
C     integration will take place.
C     It then interpolates from the first spectrum to NSD+1 points in OINTW1, OINTF1
C     It interpolates from the second spectrum to NSD+1 points in OINTW2, OINTF2
C     It calculates a product function and corresponding wavelengths in CWL, CONV
C     Finally, it integrates the product function in CONV and writes the output to
C        PROD.DAT
C     The control file is INTEGSP.INF in directory /ia
C     The output file is PROD.DAT in directory /oa
C     If KEY=0, integrate the product of the spectrum and the transmission profile
C         divided by the integral of the transmission profile.
C     If KEY=1, include the product with wavelength, numerator and denominator.
C     If KEY=2, calculate AB magnitudes.
C     If KEY=3, integrate the product of sin(x)*cos(x), 0 to pi,
C         and separately integrate sin(x), 0 to pi.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION OFL1(200000),OWL1(200000),OFL2(2000000),
     $ OWL2(2000000),
     $ OINTW1(2000000),OINTF1(2000000),CWL(2000000),CONV(2000000),
     $OINTW2(2000000),OINTF2(2000000),OFT1(2000000),OFT2(2000000),
     $XARY(3),YARY(3)
      DIMENSION FL1(10)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 48 FL1
      CHARACTER * 7 LABEL
      CHARACTER * 80 FMT1,FMT2
      DATA IN1,IN2,IN3,NOUT1,NOUT2,NOUT3,NOUT4/1,2,3,4,55,6,7/
      DATA NOUT5/8/
      DATA ER/1.D-8/
      DATA VC/2.997925E10/
  100 FORMAT (A48)
  101 FORMAT ('Integral of sin(x)*cos(x)=',F15.12,/,
     $'Integral of sin(x)=',F15.12)
  102 FORMAT (F15.8,2X,E18.8)
  103 FORMAT (F15.8,3E15.5)
  104 FORMAT ('Value of (mag)=',3X,F15.10,/,'Value of SM1=',8X,E15.9,/,
     $'Value of SM2=',8X,E15.9,/,
     $'Ratio, SM1/SM2=',6X,E15.9,/,'Value of VMGC=',4X,F15.10)
  105 FORMAT ('Value of (mag)=',3X,F15.10,/,'Value of SM1=',8X,E15.9,/,
     $'Value of SM2=',8X,E15.9,/,
     $'Value of SM3=',8X,E15.9,/,
     $'Ratio, SM1/SM2=',6X,E15.9,/,'Value of AB(nu)=',7X,F15.10,/,
     $'Ratio, SM1/SM3=',6X,E15.9,/,'Value of AB(lambda)=',3X,F15.10)
  106 FORMAT ('Value of lambda0=',6X,F15.1,1X,'Angstroms')
  107 FORMAT ('Value of lambda(pivot)=',F15.1,1X,'Angstroms')
  108 FORMAT ('Value of lambda(eff)=',2X,F15.1,1X,'Angstroms')
C*********************************************************************
      OPEN (UNIT = 10,     FILE ='INTEGSP.ILS',
     $STATUS = 'OLD')
      READ ( 10, 100)       (FL1(J), J=1,8)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT3, FILE = FL1(6),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT4, FILE = FL1(7),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT5, FILE = FL1(8),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '**INTSP'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read first data file format
      CALL CREAD ( FMT1)
      CALL DUMMY(1)
C     Read second data file format
      CALL CREAD ( FMT2)
      CALL DUMMY(1)
C     Read starting wavelength
      CALL DREAD (WST,  'F8.1' )
C     Convert to cgs units.
      WST=WST*1.0D-8
      CALL DUMMY(1)
C     Read ending wavelength
      CALL DREAD (WND,  'F8.1' )
C     Convert to cgs units.
      WND=WND*1.0D-8
      CALL DUMMY(1)
C     Read number of subintervals
      CALL DREAD (WSD, 'F10.1')
      NSD=WSD
      CALL DUMMY(1)
C     Read control for type integration weighting
      CALL IREAD (KEY)
      CALL DUMMY(4)
C     Read control for wavelengths in Angstroms, microns, or namometers
C     (Filter)
      CALL IREAD (IKY1)
      IF (IKY1.EQ.0) FMP1=1.D0
      IF (IKY1.EQ.1) FMP1=1.D4
      IF (IKY1.EQ.2) FMP1=10.D0
      CALL DUMMY(3)
C     Read control for wavelengths in Angstroms, microns, or namometers
C     (Spectrum)
      CALL IREAD (IKY2)
      IF (IKY2.EQ.0) FMP2=1.D0
      IF (IKY2.EQ.1) FMP2=1.D4
      IF (IKY2.EQ.2) FMP2=10.D0
      CALL DUMMY(3)
C     Read control for type interpolation, numerator
      CALL IREAD (JKY1)
C     Don't use spline interpolation on sine, cosine
      IF ((JKY1.EQ.3).AND.(KEY.EQ.3)) THEN
         JKY1=2
      END IF      
      CALL DUMMY(4)
C     Read control for type interpolation, denominator
      CALL IREAD (JKY2)
C     Don't use spline interpolation on sine, cosine
      IF ((JKY2.EQ.3).AND.(KEY.EQ.3)) THEN
         JKY2=2
      END IF      
      CALL DUMMY(4)
C     Read scaling factor to produce spectrum flux in correct units.
      CALL DREAD(SCL,'E8.1')
      CALL DUMMY(1)
C     Read control for integration rule
      CALL IREAD(ITG)
C     Be sure NSD is even number
      NSDA=NSD/2
      NSD=2*NSDA
C     Set ER
      ER=ER*1.0E-8*FMP2
C     Initialize storage
C     Determine wavelength range
      RG=WND-WST
C     Set K equal to number of subdivisions
      K=NSD
C     Set limit on number of entries to read in (20000000).
      JND=2000000
      KCT=0
      PI=3.1415926535898D0
      IF (KEY.EQ.3) THEN
C     Set up test of integration accuracy.
        WST=0.D0
        WND=PI
        RG=PI
        JND=7501
        IF (NSD+1.LT.JND) JND=NSD+1
      END IF
C     ******************************
C     Read data for first spectrum--> the filter
C     ******************************
      DO J=1,JND
          IF (KEY.LT.3) THEN
C     Read wavelength in Angstroms. This is the filter transmission function.
          READ ( IN2, FMT1, END=1, ERR=90) X,Y
C     Convert wavelength to cgs units.
C     Allow for wavelengths in Angstroms or microns (FMP)
          X=X*1.0D-8*FMP1
C     Restrict wavelength range to specified interval.
          IF ((X.GE.WST).AND.(X.LE.WND)) THEN
             KCT=KCT+1
             OWL1(KCT)=X
             OFL1(KCT)=Y
             zzz=0.d0
          END IF
         END IF
          IF (KEY.EQ.3) THEN
              KCT=KCT+1
              X=RG*(J-1)/(JND-1)
              OWL1(KCT)=X
                Y=DSIN(X)
                OFL1(KCT)=Y
                zzz=0.D0
          END IF
      END DO
      IF (J.GT.199999) CALL PMDSTOP
C     NOWL1=Number of wavelengths
    1 NOWL1=KCT
C     ***************************
C     End of input for first spectrum
C     ***************************
C     If JKY1=3 set up spline fit.
      IF ((JKY1.EQ.3).AND.(KEY.NE.3)) THEN
         CALL SPLINE(OWL1,OFL1,NOWL1,1.0E31,1.0E31,OFT1)
      END IF
C     ****************************
C     Read in data for second spectrum
C     ****************************
      KCT=0
C     Set up test for number of input data points
      JTEST=0
      DO J=1,JND
          IF (KEY.LT.3) THEN
          READ ( IN3, FMT2, END=2, ERR=90) X,Y
C     NOTE: if input is Eddington flux, SCL should =3.
          Y=Y*SCL
C     Convert wavelength to cgs units
          X=X*1.0D-8*FMP2
C     Restrict wavelength range to specified interval.
          IF ((X.GE.WST).AND.(X.LE.WND)) THEN
             KCT=KCT+1
             OWL2(KCT)=X
             OFL2(KCT)=Y
             zzz=0.d0
C      WRITE (NOUT3,102) OWL2(KCT)*1.0E8,OFL2(KCT)
          END IF
          END IF
          IF (KEY.EQ.3) THEN
              KCT=KCT+1
              X=RG*(J-1)/(JND-1)
                OWL2(KCT)=X
                Y=DCOS(X)
                OFL2(KCT)=Y
                zzz=0.D0
          END IF
      END DO
      IF (J.GT.19999999) CALL PMDSTOP
    2 NOWL2=KCT
C     Test for high resolution input spectrum
      IF (NOWL2.GT.NSD/2) JTEST=1
C     ***********************
C     End of input, second spectrum
C     ***********************
C     If JKY2=3 set up spline fit.
C     Do not do spline fit if we have high resolution spectrum.
      IF ((JKY2.EQ.3).AND.(KEY.NE.3).AND.(JTEST.NE.1)) THEN
         CALL SPLINE(OWL2,OFL2,NOWL2,1.0E31,1.0E31,OFT2)
      END IF
C
C     Interpolate to spacings in wavelength set by NSD
C     **************************
C     first spectrum
C     **************************
C     The number of subdivisions must be as large as the
C     number of input wavelengths.
      IF (KEY.EQ.3) THEN
C     (Integration accuracy test)
        K=NSD
          IF (K.LT.NOWL2) K=NOWL2
      END IF
      IF (K.LT.NOWL1-1) CALL PMDSTOP
C     NOTE: K is the specified number of subintervals.
C     Set up specified wavelengths in OINTWL1().
      IF (JTEST.EQ.1) THEN
      DO L=1,NOWL2
         A=OWL2(L)
         OINTW1(L)=A
         zzz=0.d0
      END DO
      END IF
      IF (JTEST.NE.1) THEN
      DO L=1,K+1
         A=(L-1)*RG/K+WST
         OINTW1(L)=A
         zzz=0.d0
      END DO
      END IF
      KKST=1
C     Interpolate a point into OINTFL1() for each point
C     in OINTWL1().
      DO L=1,NSD
        A=OINTW1(L)
C     Find points in OWL1() that bracket the OINTWL1() point.
       DO KK=KKST,NOWL1-1
      RWL1=OWL1(KK)
      RWL2=OWL1(KK+1)
C     If the two OWL1() points precede A, continue scanning.
        IF ((A.LT.RWL1).AND.(A.LT.RWL2)) THEN
           OINTF1(L)=0.D0
           GO TO 10
        END IF
C     If the first OWL1() point coincides with A, set OINTF1(L)
C     equal to the corresponding OFL1() point.
        IF (DABS(A-RWL1).LE.ER) THEN
           OINTF1(L)=OFL1(KK)
           GO TO 10
        END IF
        IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
C           GO TO 10
C      If the two OWL1() points bracket A, interpolate linearly between the
C      corresponding OFL1() points.
        IF (JKY1.EQ.0) THEN
           DF=RWL2-RWL1
           DFA=OINTW1(L)-RWL1
           H=DFA/DF
           C=OFL1(KK)
           D=OFL1(KK+1)
           DLT=D-C
           DLTA=DLT*H
           OINTF1(L)=C+DLTA
           AA=0.D0
           GO TO 10
        END IF
c        END IF
C    Start of section for parabolic interpolation
      IF (JKY1.EQ.1) THEN
          IF (KK.EQ.1) THEN
            X1=RWL1
            X2=RWL2
            X3=OWL1(KK+2)
            Y1=OFL1(KK)
            Y2=OFL1(KK+1)
            Y3=OFL1(KK+2)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF1(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL1(KK)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL1(KK+1)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL1(KK+2)
            AA=0.D0
            GO TO 10
          END IF
          IF (KK.EQ.NOWL1-1) THEN
            X1=OWL1(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=OFL1(KK-1)
            Y2=OFL1(KK)
            Y3=OFL1(KK+1)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF1(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL1(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL1(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL1(KK+1)
            AA=0.D0
            GO TO 10
          END IF
          IF ((KK.GT.1).AND.(KK.LT.NOWL1-1)) THEN
            X1=OWL1(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=OFL1(KK-1)
            Y2=OFL1(KK)
            Y3=OFL1(KK+1)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF1(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL1(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL1(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL1(KK+1)
            AA=0.D0
            GO TO 10
          END IF
       END IF
C     Start of section for interpolation by lagrange interpolation
       IF (JKY1.EQ.2) THEN
          IF (KK.LE.3) THEN
          XARY(1)=OWL1(1)
          XARY(2)=OWL1(2)
          XARY(3)=OWL1(3)
          YARY(1)=OFL1(1)
          YARY(2)=OFL1(2)
          YARY(3)=OFL1(3)
           CALL POLINT(XARY,YARY,3,A,YVL,DY)
           OINTF1(L)=YVL
          AA=0.D0
          GO TO 10
          END IF
          IF ((KK.GT.3).AND.(KK.LT.NOWL1-1)) THEN
          XARY(1)=OWL1(KK-1)
          XARY(2)=OWL1(KK)
          XARY(3)=OWL1(KK+1)
          YARY(1)=OFL1(KK-1)
          YARY(2)=OFL1(KK)
          YARY(3)=OFL1(KK+1)
           CALL POLINT(XARY,YARY,3,A,YVL,DY)
           OINTF1(L)=YVL
          AA=0.D0
          GO TO 10
          END IF
          IF (KK.GE.NOWL1-1) THEN
          XARY(1)=OWL1(NOWL1-2)
          XARY(2)=OWL1(NOWL1-1)
          XARY(3)=OWL1(NOWL1)
          YARY(1)=OFL1(NOWL1-2)
          YARY(2)=OFL1(NOWL1-1)
          YARY(3)=OFL1(NOWL1)
           CALL POLINT(XARY,YARY,3,A,YVL,DY)
           OINTF1(L)=YVL
          AA=0.D0
          GO TO 10
          END IF
       END IF
C     Set up interpolation by spline fit
       IF (JKY1.EQ.3) THEN
          CALL SPLINT(OWL1,OFL1,OFT1,NOWL1,A,OINTF1(L))
          GO TO 10
       END IF
C      If the second OWL1() point coincides with A, set OINTF1(L)
C      equal to the corresponding OFL1() point.
        IF (DABS(A-RWL2).LE.ER) THEN
           OINTF1(L)=OFL1(KK+1)
           GO TO 10
        END IF
      END IF
      END DO
   10 CONTINUE
C     Normal output case.
      IF (KEY.LT.3) THEN
      WRITE (NOUT2,102) OINTW1(L)*1.0D8,OINTF1(L)
      END IF
C     Test of integration accuracy case.
      IF (KEY.EQ.3) THEN
      WRITE (NOUT2,102) OINTW1(L),OINTF1(L)
      END IF
      zzz=0.d0
C     Set scan to start just before last scanned point.
      IF (KK.GT.2) KKST=KK-1
      END DO
C    *************************
C    End of interpolation section, first spectrum
C    *************************
C    *************************
C    Interpolate second function to equal wavelength spacings,
C    **************************
C    second spectrum
C    **************************
C     NOTE: K is the specified number of subintervals.
C     Set up equally spaced wavelengths in OINTW2() corresponding to
C     same points in OINTW1()
      DO L=1,K+1
         A=RG*(L-1)/K+WST
         OINTW2(L)=A
         zzz=0.d0
      END DO
      KKST=1
C     Interpolate a point into OINTFL2() for each point
C     in OINTWL2().
      DO L=1,K+1
        A=OINTW2(L)
       DO KK=KKST,NOWL2-1
C     Find points in OWL2() that bracket the OINTWL1() point.
      RWL1=OWL2(KK)
      RWL2=OWL2(KK+1)
C     If the two OWL2() points precede A, continue scanning.
        IF ((A.LT.RWL1).AND.(A.LT.RWL2)) THEN
           OINTF2(L)=0.D0
           GO TO 20
        END IF
C     If the first OWL2() point coincides with A, set OINTF2(L)
C     equal to the corresponding OFL2() point.
        IF (DABS(A-RWL1).LE.ER) THEN
           OINTF2(L)=OFL2(KK)
           GO TO 20
        END IF
C      If the two OWL2() points bracket A, interpolate linearly between the
C      corresponding OFL2() points.
        IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
         IF ((JKY2.EQ.0).OR.(JTEST.EQ.1)) THEN
           DF=RWL2-RWL1
           DFA=OINTW2(L)-RWL1
           H=DFA/DF
           C=OFL2(KK)
           D=OFL2(KK+1)
           DLT=D-C
           DLTA=DLT*H
           OINTF2(L)=C+DLTA
           AA=0.D0
           GO TO 20
        END IF
C        END IF
C    Start of section for parabolic interpolation
      IF ((JKY2.EQ.1).AND.(JTEST.NE.1)) THEN        
          IF (KK.EQ.1) THEN
            X1=RWL1
            X2=RWL2
            X3=OWL2(KK+2)
            Y1=OFL2(KK)
            Y2=OFL2(KK+1)
            Y3=OFL2(KK+2)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF2(L)=YVL
            IF (YVL.GT.7.0E-10) THEN
            ZZZ=0.D0
            END IF
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL2(KK)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL2(KK+1)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL2(KK+2)
            AA=0.D0
            IF ((DABS(ER1).GT.1.0E-22).OR.(DABS(ER2).GT.1.0E-22).OR.
     $(DABS(ER3).GT.1.0E-22)) THEN
            ZZZ=0.D0
            END IF
            GO TO 20
          END IF
          IF (KK.EQ.NOWL2-1) THEN
            X1=OWL2(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=OFL2(KK-1)
            Y2=OFL2(KK)
            Y3=OFL2(KK+1)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF2(L)=YVL
            IF (YVL.GT.7.0E-10) THEN
            ZZZ=0.D0
            END IF
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL2(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL2(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL2(KK+1)
            AA=0.D0
            IF ((DABS(ER1).GT.1.0E-22).OR.(DABS(ER2).GT.1.0E-22).OR.
     $(DABS(ER3).GT.1.0E-22)) THEN
            ZZZ=0.D0
            END IF
            GO TO 20
          END IF
          IF ((KK.GT.1).AND.(KK.LT.NOWL2-1)) THEN
            X1=OWL2(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=OFL2(KK-1)
            Y2=OFL2(KK)
            Y3=OFL2(KK+1)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF2(L)=YVL
            IF (YVL.GT.7.0E-10) THEN
            ZZZ=0.D0
            END IF
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL2(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL2(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL2(KK+1)
            AA=0.D0
            IF ((DABS(ER1).GT.1.0E-22).OR.(DABS(ER2).GT.1.0E-22).OR.
     $(DABS(ER3).GT.1.0E-22)) THEN
            ZZZ=0.D0
            END IF
            GO TO 20
          END IF
        END IF
C     Start of section for interpolation by lagrange interpolation
        IF ((JKY2.EQ.2).AND.(JTEST.NE.1)) THEN
          IF (KK.LE.3) THEN
          XARY(1)=OWL2(1)
          XARY(2)=OWL2(2)
          XARY(3)=OWL2(3)
          YARY(1)=OFL2(1)
          YARY(2)=OFL2(2)
          YARY(3)=OFL2(3)
           CALL POLINT(XARY,YARY,3,A,YVL,DY)
           OINTF2(L)=YVL
          AA=0.D0
          GO TO 20
          END IF
          IF ((KK.GT.3).AND.(KK.LT.NOWL2-1)) THEN
          XARY(1)=OWL2(KK-1)
          XARY(2)=OWL2(KK)
          XARY(3)=OWL2(KK+1)
          YARY(1)=OFL2(KK-1)
          YARY(2)=OFL2(KK)
          YARY(3)=OFL2(KK+1)
            CALL POLINT(XARY,YARY,3,A,YVL,DY)
           OINTF2(L)=YVL
          AA=0.D0
          GO TO 20
          END IF
          IF (KK.GE.NOWL2-1) THEN
          XARY(1)=OWL2(NOWL2-2)
          XARY(2)=OWL2(NOWL2-1)
          XARY(3)=OWL2(NOWL2)
          YARY(1)=OFL2(NOWL2-2)
          YARY(2)=OFL2(NOWL2-1)
          YARY(3)=OFL2(NOWL2)
           CALL POLINT(XARY,YARY,3,A,YVL,DY)
           OINTF2(L)=YVL
          AA=0.D0
          GO TO 20
          END IF
        END IF
C     Set up for interpolation by spline
        IF ((JKY2.EQ.3).AND.(JTEST.NE.1)) THEN
           CALL SPLINT(OWL2,OFL2,OFT2,NOWL2,A,OINTF2(L))
           GO TO 20
        END IF
       END IF
C      If the second OWL2() point coincides with A, set OINTF2(L)
C      equal to the corresponding OFL2() point.
        IF (DABS(A-RWL2).LE.ER) THEN
           OINTF2(L)=OFL2(KK+1)
           GO TO 20
        END IF
      END DO
   20 CONTINUE
      IF (KEY.LT.3) THEN
      WRITE (NOUT3,102) OINTW2(L)*1.0D8,OINTF2(L)
      END IF
      IF (KEY.EQ.3) THEN
      WRITE (NOUT3,102) OINTW2(L),OINTF2(L)
      END IF
      zzz=0.d0
C     Set scan to start just before last scanned point.
C      IF (KK.GT.2)KKST=KK-1 
      END DO
C     **************************
C     End of interpolation section, second spectrum
C     **************************
C
C     Form product function
C
      DO N=1,NSD+1
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
C     CC is the second spectrum (star spectrum) flux in cgs units.
          CC=OINTF2(N)
          CWL(N)=AA
          IF ((KEY.EQ.0).OR.(KEY.EQ.3)) THEN
C     Form product function.
          CONV(N)=BB*CC
          END IF
C     Form product function for photonic magnitudes
          IF (KEY.EQ.1) THEN
          CONV(N)=AA*BB*CC*1.0D8
          END IF
C     Form product for production of AB magnitudes.
C     Express wavelengths in cgs units.
          IF (KEY.EQ.2) THEN
          CONV(N)=AA*BB*CC
          END IF
          IF (KEY.NE.3) THEN  
               WRITE (NOUT4, 103) CWL(N)*1.0D8,BB,CC,CONV(N)
          END IF
          IF (KEY.EQ.3) THEN  
               WRITE (NOUT4, 102) CWL(N),CONV(N)
          END IF
      END DO
C     Now have product function stored in array CONV(N)
C     *********
C     Evaluate numerator. This is the product of the filter response
C     function and the stellar flux (KEY=0).
C     *********
      IF (ITG.EQ.0) THEN
C     Integrate product function by trapezoidal rule.
C     Note that there are NOWL2 intervals, NOWL2+1 points.
C     
      SM1=0.D0
      SM6=0.D0
C     
      DO L=2,NSD
           H=OINTW1(L)-OINTW1(L-1)
           ADD=(CONV(L)+CONV(L-1))/2.D0
           IF ((KEY.EQ.0).OR.(KEY.EQ.3)) THEN
           SM1=SM1+ADD*H
           AA=0.D0
           ELSE
C      
           SM1=SM1+ADD*H
           SM6=SM6+(CONV(L)*OINTW1(L)+CONV(L-1)*OINTW1(L-1))*H/2.D0
           AA=0.D0
           END IF
      END DO
      END IF
C
C    
      IF (ITG.EQ.1) THEN     
C     Integrate product function by Simpson's rule
      SM1=0.D0
      SM6=0.D0
      H=(CWL(K+1)-CWL(1))/K
      CF=H/3.D0
      LM=K/2
      JPT=1
      DO L=1,LM
            A=CONV(JPT)
            B=CONV(JPT+1)
            C=CONV(JPT+2)
            SM1=SM1+A+4.D0*B+C
            SM6=SM6+A*OINTW1(JPT)+4.D0*B*OINTW1(JPT+1)+C*OINTW1(JPT+2)
            JPT=JPT+2
            AA=0.D0
      END DO
      SM1=SM1*CF
      SM6=SM6*CF
      END IF
C     *******************
C     Evaluate denominator.
C     This is the integral over the filter response function (KEY=0). 
C     *******************
C     *******************
C     Start of integration section.
C     *******************
      IF (ITG.EQ.0) THEN
C     Integrate filter passband by trapezoidal rule.
      SM2=0.D0
C     Add ordinates
      DO L=2,NSD
          WRITE (NOUT5,103) OINTW1(L-1),OINTF1(L-1),
     $OINTF1(L-1)/OINTW1(L-1)
          H=OINTW1(L)-OINTW1(L-1)
          IF ((KEY.EQ.0).OR.(KEY.EQ.3)) THEN
              SM2=SM2+(OINTF1(L)+OINTF1(L-1))*H/2.D0
              AA=0.D0
          END IF
          IF (KEY.EQ.1) THEN
              CC=OINTW1(L-1)
              DD=OINTW1(L)
              H=DD-CC
C      Express flux in Angstroms
              BB=(OINTF1(L)*DD+OINTF1(L-1)*CC)
              SM2=SM2+BB*H*1.0D8/2.D0
              AA=0.D0
           END IF
          IF (KEY.EQ.2) THEN
              CC=OINTW1(L-1)
              DD=OINTW1(L)
              H=DD-CC
C      Express flux in cgs units
              BB=(OINTF1(L)/DD+OINTF1(L-1)/CC)*1.0D-8
              SM2=SM2+BB*H*VC/2.D0
              AA=0.D0
          END IF
      END DO
C     
      END IF
C     End of integration by trapezoidal rule.
C     *******************
      IF (ITG.EQ.1) THEN
C     Integrate filter passband by Simpson's rule
      SM2=0.D0
      H=(OINTW1(K+1)-OINTW1(1))/K
      CF=H/3.D0
      LM=K/2
      JPT=1
      DO L=1,LM
          WRITE (NOUT5,103) OINTW1(L),OINTF1(L),
     $OINTF1(L)/OINTW1(L)
C      Express flux in cgs units
            A=OINTF1(JPT)
            B=OINTF1(JPT+1)
            C=OINTF1(JPT+2)
            W1=OINTW1(JPT)
            W2=OINTW1(JPT+1)
            W3=OINTW1(JPT+2)
            IF ((KEY.EQ.0).OR.(KEY.EQ.3)) THEN
                SM2=SM2+A+4.D0*B+C
            END IF
            IF (KEY.EQ.1) THEN
                SM2=SM2+(A*W1+4.D0*B*W2+C*W3)*1.0D8
            END IF
            IF (KEY.EQ.2) THEN
                 SM2=SM2+(A/W1+4.D0*B/W2+C/W3)*VC*1.0D-8
            END IF
            JPT=JPT+2
            zzz=0.d0
      END DO
      SM2=SM2*CF
      END IF
C     *****************
      IF (ITG.EQ.0) THEN
C     Integrate filter passband by trapezoidal rule.
      SM3=0.D0
      SM4=0.D0
      SM5=0.D0
C     Add ordinates
      DO L=2,NSD
C          WRITE (NOUT5,103) OINTW1(L-1),OINTF1(L-1),
C     $OINTF1(L-1)/OINTW1(L-1)
          H=OINTW1(L)-OINTW1(L-1)
          IF (KEY.EQ.2) THEN
              CC=OINTW1(L-1)
              DD=OINTW1(L)
              H=DD-CC
C      Express flux in cgs units
              BB=(OINTF1(L)*DD+OINTF1(L-1)*CC)
              SM3=SM3+BB*H/2.D0
              SM4=SM4+(OINTF1(L)+OINTF1(L-1))*H/2.0D0
              SM5=SM5+(OINTF1(L)/DD+OINTF1(L-1)/CC)*H/2.0D0
              AA=0.D0
          END IF
      END DO
C     
      END IF
C     End of integration by trapezoidal rule.
C     *******************
      IF (ITG.EQ.1) THEN
C     Integrate filter passband by Simpson's rule
      SM3=0.D0
      SM4=0.D0
      SM5=0.D0
      H=(OINTW1(K+1)-OINTW1(1))/K
      CF=H/3.D0
      LM=K/2
      JPT=1
      DO L=1,LM
C          WRITE (NOUT5,103) OINTW1(L),OINTF1(L),
C     $OINTF1(L)/OINTW1(L)
C      Express flux in cgs units
            A=OINTF1(JPT)
            B=OINTF1(JPT+1)
            C=OINTF1(JPT+2)
            W1=OINTW1(JPT)
            W2=OINTW1(JPT+1)
            W3=OINTW1(JPT+2)
C            IF ((KEY.EQ.0).OR.(KEY.EQ.3)) THEN
C                SM2=SM2+A+4.D0*B+C
C            END IF
            IF (KEY.EQ.2) THEN
                 SM3=SM3+(A*W1+4.D0*B*W2+C*W3)*1.0D-8
                 SM4=SM4+(A+4.D0*B+C)*1.0D-8
                 SM5=SM5+(A/W1+4.D0*B/W2+C/W3)*1.0D-8
            END IF
            JPT=JPT+2
            zzz=0.d0
      END DO
      SM3=SM3*CF*1.0D8
      SM4=SM4*CF*1.0D8
      SM5=SM5*CF*1.0D8
      END IF
C     *****************
C     End of integration
C     *****************
      IF (KEY.LT.3) THEN
      SM=SM1/SM2
      A=SM1
      B=SM2
      C=SM
      VMG=-2.5*DLOG10(SM)
      D=VMG
C     For following equation see Bessell, 1998, A&A, 333, 231, p.242
C     Note that KEY=0 is used for both flux-based filters and photonic.
C     Flux-based are designated UFLT90.DAT, BFLT90.DAT, etc.
C     Photonic are designated UFLT.DAT, BFLT.DAT, etc.
      IF (KEY.EQ.0) THEN
      VMGC=VMG-21.1D0
      END IF
      IF (KEY.EQ.1) THEN
      VMGC=VMG-21.1D0
      END IF
C     If KEY=2, see Bessell & Murphy, 2012, PASP, 124, xxxx, equation 2.
      IF (KEY.EQ.2) THEN
      VMGC=VMG-48.60D0
C     Produce a listing comparable to the KEY=0,1 output.
      D=VMG
      END IF
      E=VMGC
      IF (KEY.EQ.0) THEN
          WRITE (NOUT1,104) D,A,B,C,E
      END IF
      IF (KEY.EQ.1) THEN
          WRITE (NOUT1,104) D,A,B,C,E
      END IF
      IF (KEY.EQ.2) THEN
          SMA=SM1/SM3
          VMGA=-2.5*DLOG10(SMA)
          VMGCA=VMGA-21.1D0
          VLAM0=SM3/SM4*1.0D8
          VLAMP=DSQRT(SM3/SM5)*1.0D8
          VLEFF=SM6/SM1*1.0E8
          WRITE (NOUT1,105) D,A,B,SM3,C,E,SMA,VMGCA
          WRITE (NOUT1,106) VLAM0
          WRITE (NOUT1,107) VLAMP
          WRITE (NOUT1,108) VLEFF
      END IF
      END IF
      IF (KEY.EQ.3) THEN
      WRITE (NOUT1,101) SM1,SM2
      END IF   
C*********************************************************************
      CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN2,      STATUS = 'KEEP')
      CLOSE ( UNIT = IN3,      STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT1,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT2,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT3,    STATUS = 'KEEP')
      CLOSE ( UNIT = NOUT4,    STATUS = 'KEEP')
C*********************************************************************
      STOP 'INTEGSP completed successfully'
   90 CALL PMDSTOP
      END
