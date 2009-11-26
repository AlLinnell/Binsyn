      SUBROUTINE RTSAFE(X1,X2,P1,P2,P3,XACC,RTS)
C     Procedure modified from Numerical Recipes
C     The desired root lies between X1 and X2
C     The accuracy limit on the root is +/- XACC
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXIT=100)
      CALL FTHETA(P1,P2,X1,P3,FL)
      CALL FTHETA(P1,P2,X2,P3,FH)
      CALL FTHETAP(P1,P2,(X1+X2)/2.D0,P3,DF)
      IF (FL*FH.GE.0.D0) CALL PMDSTOP
      IF (FL.LT.0.D0) THEN
         XL=X1
         XH=X2
      ELSE
         XH=X1
         XL=X2
      END IF
      RTS=(X1+X2)/2.D0
      DXOLD=DABS(X2-X1)
      DX=DXOLD
      CALL FTHETA(P1,P2,RTS,P3,F)
      CALL FTHETAP(P1,P2,RTS,P3,DF)
      DO 11 J=1,MAXIT
         IF((((RTS-XH)*DF-F)*((RTS-XL)*DF-F).GE.0.D0)
     $   .OR.(DABS(2.D0*F).GT.DABS(DXOLD*DF))) THEN
         DXOLD=DX
         DX=0.5*(XH-XL)
         RTS=XL+DX
         IF (XL.EQ.RTS) RETURN
      ELSE
         DXOLD=DX
         DX=F/DF
         TEMP=RTS
         RTS=RTS-DX
         IF (TEMP.EQ.RTS) RETURN
      END IF
      IF (DABS(DX).LT.XACC) RETURN
      CALL FTHETA(P1,P2,RTS,P3,F)
      CALL FTHETAP(P1,P2,RTS,P3,DF)
      IF (F.LT.0.D0) THEN
         XL=RTS
      ELSE
         XH=RTS
      END IF
   11 CONTINUE
      CALL PMDSTOP
      END
