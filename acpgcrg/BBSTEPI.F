      SUBROUTINE BBSTEPI(VKL,VNE,T,WVL,AVI,SMT,VLI,DVIDL,NV,ZCURR,HTRY,
     $EPS,YSCAL,HDID,HNEXT,SEPCM)
C     Subroutine for numerical solution of equation of transfer for
C     bremsstrahlung radiation by the Burlisch-Stoer method.
C     See Numerical Recipes, by Press et al., p.563.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER(NMAX=10,IMAX=11,NUSE=7,ONE=1.D0,SHRINK=0.95D0,
     $GROW=1.2D0)
	DIMENSION VLI(NV),DVIDL(NV),YSCAL(NV),YERR(NMAX),
     $YSAV(NMAX),DYSAV(NMAX),YSEQ(NMAX),NSEQ(IMAX)
	DATA NSEQ/2,4,6,8,12,16,24,32,48,64,96/
	H=HTRY
	ZSAV=ZCURR*SEPCM
	Z=ZSAV
	DO 11 I=1,NV
		YSAV(I)=VLI(I)
		DYSAV(I)=DVIDL(I)
   11 CONTINUE
    1 DO 10 I=1,IMAX
		CALL MMIDI(VKL,VNE,T,WVL,AVI,SMT,YSAV,DYSAV,NV,Z/SEPCM,H,
     $		NSEQ(I),YSEQ,SEPCM)
		ZEST=(H/NSEQ(I))**2
		CALL RZEXTR(I,ZEST,YSEQ,VLI,YERR,NV,NUSE)
		ERRMAX=0.D0
		DO 12 J=1,NV
			ERRMAX=MAX(ERRMAX,DABS(YERR(J)/YSCAL(J)))
   12		CONTINUE
		ERRMAX=ERRMAX/EPS
		IF (ERRMAX.LT.ONE) THEN
C     The program normally steps the value of the independent variable 
C     at this point.
C     However, to permit Lambda iteration of the mean intensity, we 
C     defer stepping Z to the proper place in the calling program.
C			Z=Z+H
			HDID=H
			IF (I.EQ.NUSE) THEN
				HNEXT=H*SHRINK
			ELSE IF(I.EQ.NUSE-1) THEN
				HNEXT=H*GROW
			ELSE
				HNEXT=(H*NSEQ(NUSE-1))/NSEQ(I)
			ENDIF
			ZCURR=Z/SEPCM
			RETURN
		ENDIF
   10 CONTINUE
      H=0.25D0*H/2**((IMAX-NUSE)/2)
	IF (Z+H.EQ.Z) CALL PMDSTOP
	GO TO 1
	END