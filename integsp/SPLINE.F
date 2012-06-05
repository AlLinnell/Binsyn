      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
C     From Numerical Recipes, P. 88
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NMAX=60000)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.D0
        U(1)=0.D0
      ELSE
        Y2(1)=-0.5D0
        U(1)=(3.D0/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.D0
        Y2(I)=(SIG-1.D0)/P
        A1=X(I+1)
        A2=X(I)
        A3=X(I-1)
        B1=Y(I+1)
        B2=Y(I)
        B3=Y(I-1)
        C1=Y(I+1)-Y(I)
        C2=X(I+1)-X(I)
        C3=X(I+1)-X(I-1)
        C4=Y(I)-Y(I-1)
        C5=X(I)-X(I-1)
        U(I)=(6.D0*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.D0
        UN=0.D0
      ELSE
        QN=0.5D0
        UN=(3.D0/(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.D0)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END
