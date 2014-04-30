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
      DIMENSION OFL1(20000),OWL1(20000),OFL2(2000000),
     $ OWL2(2000000),
     $ OINTW1(20000),OINTF1(20000),CWL(20000),CONV(20000),
     $OINTW2(20000),OINTF2(20000),OFT1(20000),OFT2(20000),
     $XARY(3),YARY(3)
      DIMENSION FL1(10)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 48 FL1
      CHARACTER * 7 LABEL
      CHARACTER * 80 FMT1,FMT2
      DATA IN1,IN2,IN3,NOUT1,NOUT2,NOUT3,NOUT4/1,2,3,4,55,6,7/
      DATA ER/1.D-8/
      DATA VC/2.997925E10/
  100 FORMAT (A48)
  101 FORMAT ('Integral of sin(x) from 0 to pi=',F15.12)
  102 FORMAT (F15.8,2X,E18.8)
  104 FORMAT ('Value of (mag)=',3X,F15.10,/,'Value of SM1=',8X,E15.9,/,
     $'Value of SM2=',8X,E15.9,/,
     $'Ratio, SM1/SM2=',6X,E15.9,/,'Value of VMGC=',4X,F15.10)
  105 FORMAT ('Value of (mag)=',3X,F15.10,/,'Value of SM1=',8X,E15.9,/,
     $'Value of SM2=',8X,E15.9,/,
     $'Value of SM3=',8X,E15.9,/,
     $'Value of SM4=',8X,E15.9)
  106 FORMAT ('Value of AB(nu)=',10X,E15.9,/,
     $'Value of <f_nu>=',10X,E15.9,/,
     $'Value of AB(lambda)=',6X,E15.9,/,
     $'Value of <f_lambda>=',6X,E15.9,/,
     $'Value of lambda(pivot)=',9X,F7.1,1X,'Angstroms'/,
     $'Value of lambda0=',15X,F7.1,1X,'Angstroms'/,
     $'Value of lambda(eff)=',11X,F7.1,1X,'Angstroms')
C*********************************************************************
      OPEN (UNIT = 10,     FILE ='INTEGSP.ILS',
     $STATUS = 'OLD')
      READ ( 10, 100)       (FL1(J), J=1,7)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT2, FILE = FL1(5),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT3, FILE = FL1(6),  STATUS = 'REPLACE')
      OPEN ( UNIT = NOUT4, FILE = FL1(7),  STATUS = 'REPLACE')
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
      CALL DUMMY(3)
C     Read control for integration rule
      CALL IREAD(ITG)
C     Be sure NSD is odd number
      NSDA=NSD/2
      NSD=2*NSDA+1
      CALL DUMMY(2)
C     Read control for which integrand to list
      CALL IREAD (ITKY)
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
C     Set up default scaling coefficient to compensate integral over filter for
C     conversion of wavelength to cgs units.
      SCLFLT=1.0D-8*FMP1
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
C      JTEST=0
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
      WRITE (NOUT3,102) OWL2(KCT)*1.0E8,OFL2(KCT)
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
C     Set up default conversion factor to compensate for conversion of
C     input spectrum to cgs units
      SCLSPECT=1.0D-8*FMP2*SCL
C     Test for high resolution input spectrum
C      IF (NOWL2.GT.NSD/2) JTEST=1
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
      REWIND NOUT2
      IF (KEY.EQ.3) THEN
C     (Integration accuracy test)
        K=NSD
          IF (K.LT.NOWL2) K=NOWL2
      END IF
      IF (K.LT.NOWL1-1) CALL PMDSTOP
C     NOTE: K is the specified number of subintervals.
C     Set up specified wavelengths in OINTWL1().
C      IF (JTEST.EQ.1) THEN
C      DO L=1,NOWL2
C         A=OWL2(L)
C         OINTW1(L)=A
C         zzz=0.d0
C      END DO
C      END IF
C      IF (JTEST.NE.1) THEN
      DO L=1,K
         A=(L-1)*RG/K+WST
         OINTW1(L)=A
         zzz=0.d0
      END DO
C      END IF
      KKST=1
C     Interpolate a point into OINTFL1() for each point
C     in OINTWL1().
      DO L=1,NSD
        A=OINTW1(L)
        IF (L.GE.NSD-5) THEN
            ZZZ=0.D0
        END IF
        YVL=0.D0
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
C      IF (KEY.EQ.3) THEN
C      WRITE (NOUT2,102) OINTW1(L),OINTF1(L)
C      END IF
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
      REWIND NOUT3
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
        YVL=0.D0
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
         IF (JKY2.EQ.0) THEN
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
C    Start of section for parabolic interpolation
      IF (JKY2.EQ.1) THEN        
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
            BTM=TM3/(X2-X1)
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            OINTF2(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL2(KK)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL2(KK+1)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL2(KK+2)
            AA=0.D0
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
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL2(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL2(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL2(KK+1)
            AA=0.D0
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
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-OFL2(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-OFL2(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-OFL2(KK+1)
            AA=0.D0
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
C      If the second OWL2() point coincides with A, set OINTF2(L)
C      equal to the corresponding OFL2() point.
        IF (DABS(A-RWL2).LE.ER) THEN
           OINTF2(L)=OFL2(KK+1)
           GO TO 20
        END IF
        END IF
      END DO
   20 CONTINUE
      IF (KEY.LT.3) THEN
      if (L.ge.K-5) then
          zzz=0.d0
      end if
      WRITE (NOUT3,102) OINTW2(L)*1.0D8,OINTF2(L)
      END IF
      IF (KEY.EQ.3) THEN
      WRITE (NOUT3,102) OINTW2(L),OINTF2(L)
      END IF
      zzz=0.d0
C     Set scan to start just before last scanned point.
      IF (KK.GT.2)KKST=KK-1 
      END DO
C     **************************
C     End of interpolation section, second spectrum
C     **************************
C     **************************
C     Beginning of integration section
C     **************************
C
C     Form product function, SM2
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
          CWL(N)=AA
          CONV(N)=BB
          IF ((KEY.NE.3).AND.(ITKY.EQ.2)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          IF (KEY.EQ.3) THEN  
               WRITE (NOUT4, 102) CWL(N),CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM2 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM2=SM
C     *********
C     Form product function, SM1
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
          CC=OINTF2(N)
          CONV(N)=BB*CC
          IF ((KEY.NE.3).AND.(ITKY.EQ.1)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM1 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM1=SM
C     *********
C     Form product function, SM3
C     *********
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
C     CC is the second spectrum in absolute flux units.
          CC=OINTF2(N)
          CONV(N)=AA*BB*CC
          IF ((KEY.NE.3).AND.(ITKY.EQ.3)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM3 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM3=SM
C     *********
C     Form product function, SM4
C     *********
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
          CONV(N)=AA*BB
          IF ((KEY.NE.3).AND.(ITKY.EQ.4)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM4 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM4=SM
C     *********
C     Form product function, SM5
C     *********
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
          IF (AA.GE.0.D0) THEN
              CONV(N)=BB/AA
          ELSE
              CONV(N)=0.D0
          END IF
          IF ((KEY.NE.3).AND.(ITKY.EQ.5)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM5 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM5=SM
C     *********
C     Form product function, SM6
C     *********
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
C     CC is the second spectrum in absolute flux units.
          CC=OINTF2(N)
          CONV(N)=AA*AA*BB*CC
          IF ((KEY.NE.3).AND.(ITKY.EQ.6)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM6 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM6=SM
C     *********
C     Form product function, SM7
C     *********
C
      DO N=1,NSD
C     AA is the wavelength in cgs units.
          AA=OINTW1(N)
C     BB is the first spectrum (filter) equivalent flux (dimensionless).
          BB=OINTF1(N)
          IF (AA.GE.0.D0) THEN
              CONV(N)=BB/AA*VC/1.0D8
          ELSE
              CONV(N)=0.D0
          END IF
          IF ((KEY.NE.3).AND.(ITKY.EQ.7)) THEN  
               WRITE (NOUT4, 102) CWL(N)*1.0D8,CONV(N)
          END IF
          ZZ=0.D0
      END DO
C     Now have SM7 integrand stored in array CONV(N)
C      CALL ITGFN(OINTW1,CONV,KEY,ITG,ITKY,NSD,SM)
      CALL ITGFN(OINTW1,CONV,ITG,NSD,SM)
      SM7=SM
C     *****************
C     Test integration
C     *****************
      IF (KEY.EQ.3) THEN
C      CALL ITGFN(OWL1,OFL1,KEY,ITG,ITKY,JND,SM)
      CALL ITGFN(OWL1,OFL1,ITG,JND,SM)
      SM1=SM
      END IF
C     *****************
C     End of integration
C     *****************
      IF (KEY.LT.3) THEN
C      SM1=SM1*SCLSPECT
C      SM2=SM2*SCLFLT
      SM=SM1/SM2
      A=SM1
      B=SM2
      C=SM
      VMG=-2.5D0*DLOG10(SM)
      D=VMG
C     For following equation see Bessell, 1998, A&A, 333, 231, p.242
C     Note that KEY=0 requires use of flux-based filters.
C     These are designated UFLT90.DAT, BFLT90.DAT, etc.
      IF (KEY.EQ.0) THEN
      VMGC=VMG-21.1D0
      GO TO 50
      END IF
C     If KEY=1, calculate wavelength-weighted quantities.
      IF (KEY.EQ.1) THEN
      SM=SM3/SM4
      A=SM3
      B=SM4
      C=SM
      VMG=-2.5D0*DLOG10(SM)
      D=VMG
      VMGC=VMG-21.1D0
      GO TO 50
      END IF
C     If KEY=2, see Bessell & Murphy, 2012, PASP, 124, 140.
      IF (KEY.EQ.2) THEN
      SM=SM3/SM7
      RATIO=SM3/SM4
      VMG=-2.5*DLOG10(SM)
      VMGC=VMG-48.60D0
      ABlam=-2.5*DLOG10(SM3/SM4)-21.1
      VLAM0=SM4/SM2*1.0D8
      VLAMEFF=SM6/SM3*1.0D8
      VLAMP=DSQRT(SM4/SM5)*1.0D8
      WRITE (NOUT1,105) VMG,A,B,SM3,SM4
      WRITE (NOUT1,106) VMGC,SM,ABlam,RATIO,VLAMP,VLAM0,VLAMEFF
      GO TO 51 
C     Produce a listing comparable to the KEY=0,1 output.
      END IF
   50 CONTINUE
      E=VMGC
      WRITE (NOUT1,104) D,A,B,C,E
      END IF
   51 CONTINUE
      IF (KEY.EQ.3) THEN
      WRITE (NOUT1,101) SM1
      END IF   
C*********************************************************************
   52 CLOSE ( UNIT = IN1,      STATUS = 'KEEP')
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
