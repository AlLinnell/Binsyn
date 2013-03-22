      SUBROUTINE GCF(GAMMCF,A,X,GLN)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=DFLOAT(N)
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(DABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      CALL PMDSTOP
C       'A too large, ITMAX too small'
1     GAMMCF=DEXP(-X+A*DLOG(X)-GLN)*G
      Z1=DLOG(X)
	Z2=A*Z1
	Z3=-X+Z2
	Z4=Z3-GLN
	Z5=DEXP(Z4)
      RETURN
      END
