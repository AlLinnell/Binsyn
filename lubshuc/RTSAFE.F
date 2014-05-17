      FUNCTION RTSAFE(FUNCD,VMU,X1,X2,XACC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MAXIT=100)
      CALL FUNCD(VMU,X1,FL,DF)
      CALL FUNCD(VMU,X2,FH,DF)
      IF (FL*FH.GE.0.D0) PAUSE 'root must be bracketed'
      IF (FL.LT.0.D0) THEN
		XL=X1
		XH=X2
      ELSE
		XH=X1
		XL=X2
		SWAP=FL
		FL=FH
		FH=SWAP
      ENDIF
      RTSAFE=0.5D0*(X1+X2)
      DXOLD=DABS(X2-X1)
      DX=DXOLD
      CALL FUNCD(VMU,RTSAFE,F,DF)
      DO 11 J=1,MAXIT
         IF(((RTSAFE-XH)*DF-F)*((RTSAFE-XL)*DF-F).GE.0.D0
     $	.OR.DABS(2.D0*F).GT.DABS(DXOLD*DF)) THEN
		 DXOLD=DX
		 DX=0.5D0*(XH-XL)
		 RTSAFE=XL+DX
		 IF (XL.EQ.RTSAFE) RETURN
	   ELSE
		 DXOLD=DX
           DX=F/DF
           TEMP=RTSAFE
           RTSAFE=RTSAFE-DX
           IF(TEMP.EQ.RTSAFE) RETURN
	   ENDIF
         IF (DABS(DX).LT.XACC) RETURN
         CALL FUNCD(VMU,RTSAFE,F,DF)
         IF (F.LT.0.D0) THEN
		 XL=RTSAFE
		 FL=F
         ELSE
	     XH=RTSAFE
		 FH=F
         ENDIF
   11 CONTINUE
      PAUSE 'RTSAFE exceeding maximum iterations'
	RETURN
      END
