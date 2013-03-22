      PROGRAM INTPOL
C     Program to input a filter response function and interpolate response points
C     at steps of 1 Angstrom.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WL(1000),RS(1000),WLTP(100000),RSTP(100000),FL1(3)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 48 FL1
      CHARACTER * 7 LABEL
      CHARACTER * 80 FMT1
      DATA IN1,IN2,NOUT1/1,2,3/
  100 FORMAT (A48)
C*********************************************************************
      OPEN (UNIT = 4,     FILE ='INTPOL.ILS',
     $STATUS = 'OLD')
      READ ( 4, 100)       (FL1(J), J=1,3)
      OPEN ( UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN ( UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = NOUT1, FILE = FL1(3),  STATUS = 'REPLACE')
C*********************************************************************
      LABEL = '*INTPOL'
      LBLERR = 0
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read first data file format
      CALL CREAD ( FMT1)
      DO J=1,1000
        READ (IN2,FMT1,ERR=99,END=1) WL(J),RS(J)
      END DO
    1 N=J-1
      KRG=WL(N-1)-WL(1)
      DO L=1,KRG+1
         WLTP(L)=WL(1)+L-1
C
      KKST=1
C     Interpolate a point into RSTP() for each point
C     in WLTP().
      DO LL=1,N
        A=WLTP(L)
C     Find points in WL() that bracket the WLTP() point.
       DO KK=KKST,KRG-1
      RWL1=WL(KK)
      RWL2=WL(KK+1)
C     If the two WL() points precede A, continue scanning.
        IF ((A.LT.RWL1).AND.(A.LT.RWL2)) THEN
           RSTP(LL)=0.D0
           GO TO 10
        END IF
C     If the first WL() point coincides with A, set RSTP(LL)
C     equal to the corresponding WL() point.
        IF (DABS(A-RWL1).LE.ER) THEN
           RSTP(L)=RS(KK)
           GO TO 10
        END IF
        IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
C    Start of section for parabolic interpolation
C      IF (JKY1.EQ.1) THEN
          IF (KK.EQ.1) THEN
            X1=RWL1
            X2=RWL2
            X3=WL(KK+2)
            Y1=RS(KK)
            Y2=RS(KK+1)
            Y3=RS(KK+2)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            RSTP(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-RS(KK)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-RS(KK+1)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-RS(KK+2)
            AA=0.D0
            GO TO 10
          END IF
          IF (KK.EQ.N-1) THEN
            X1=WL(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=RS(KK-1)
            Y2=RS(KK)
            Y3=RS(KK+1)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            RSTP(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-RS(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-RS(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-RS(KK+1)
            AA=0.D0
            GO TO 10
          END IF
          IF ((KK.GT.1).AND.(KK.LT.N-1)) THEN
            X1=WL(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=RS(KK-1)
            Y2=RS(KK)
            Y3=RS(KK+1)
            TM1=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1/TM2
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            RSTP(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-RS(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-RS(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-RS(KK+1)
            AA=0.D0
            GO TO 10
          END IF
C       END IF
C      If the second WL() point coincides with A, set RSTP(L)
C      equal to the corresponding RS() point.
        IF (DABS(A-RWL2).LE.ER) THEN
           RSTP(L)=RS(KK+1)
           GO TO 10
        END IF
      END IF
      END DO
      END DO
   10 CONTINUE
         IF (RSTP(L).GT.1.0D0) RSTP(L)=1.0D0
         IF (RSTP(L).LT.0.0D0) RSTP(L)=0.0D0
         WRITE (NOUT1,FMT1) WLTP(L),RSTP(L)
         ZZ=0.d0
C
      END DO
      STOP 'INTPOL completed successfully'
   99 CALL PMDSTOP
      END PROGRAM INTPOL
