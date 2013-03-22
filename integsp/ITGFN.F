      SUBROUTINE ITGFN(AR1,AR2,KY,N,SM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AR1(10000),AR2(10000)
      SM=0.D0
      H=AR1(2)-AR1(1)
      IF (KY.EQ.0) THEN
C     Trapezoidal rule
      DO L=2,N
         ORD=(AR2(L)+AR2(L-1))/2.D0
         SM=SM+ORD*H
         IF (L.GE.N-5) THEN
         ZZZ=0.D0
         END IF
      END DO
      END IF
      IF (KY.EQ.1) THEN
C     Simpson's Rule
      CF=H/3.D0
      LM=N/2
      JPT=1
      DO L=1,LM
            A=AR2(JPT)
            B=AR2(JPT+1)
            C=AR2(JPT+2)
            SM=SM+A+4.D0*B+C
            JPT=JPT+2
      END DO
      SM=SM*CF
      END IF
      RETURN
      END SUBROUTINE ITGFN
