      SUBROUTINE SETPOLAR(X,Y,XSAV,YSAV,VRHO,VTHETA,KTH,I)
      INCLUDE 'COMA.FOR'
	DATA PIO2/1.5707963267948966192D0/
	DATA PI/3.141592653589793238463D0/
	DATA TPIO2/4.712388980384689858D0/
	DIMENSION VRHO(2,MTHM),VTHETA(2,MTHM),XSAV(2,MTHM),YSAV(2,MTHM)
	KTH=KTH+1
	XSAV(I,KTH)=X
	YSAV(I,KTH)=Y
	VRHO(I,KTH)=DSQRT(X**2+Y**2)
C     First quadrant. (X is negative in the upward direction)
	IF ((X.LE.0.D0).AND.(Y.GT.0.D0)) THEN
	VTHETA(I,KTH)=TPIO2+DACOS(Y/VRHO(I,KTH))
	END IF
C     Upward X axis. Polar angle is zero.
	IF ((X.LT.0.D0).AND.(Y.EQ.0.D0)) THEN
	VTHETA(I,KTH)=0.D0
	END IF
C     Fourth quadrant, but this is the positive direction for theta
	IF ((X.LE.0.D0).AND.(Y.LT.0.D0)) THEN
	VTHETA(I,KTH)=-DASIN(Y/VRHO(I,KTH))
	END IF
C     Third quadrant
	IF ((X.GE.0.D0).AND.(Y.LT.0.D0)) THEN
	VTHETA(I,KTH)=PIO2+DASIN(X/VRHO(I,KTH))
	END IF
C     Downward X axis. Polar angle = PI
	IF ((X.GT.0.D0).AND.(Y.EQ.0.D0)) THEN
	VTHETA(I,KTH)=PI
	END IF
C     Second quadrant
	IF ((X.GE.0.D0).AND.(Y.GT.0.D0)) THEN
	VTHETA(I,KTH)=TPIO2-ACOS(Y/VRHO(I,KTH))
	END IF
	RETURN
	END