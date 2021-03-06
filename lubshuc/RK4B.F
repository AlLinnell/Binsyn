      SUBROUTINE RK4B(X,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YP,DYDX,
     $N,H,YOUT)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (NMAX=20)
	DIMENSION YP(N),DYDX(N),YOUT(N),YT(NMAX),DYT(NMAX),DYM(NMAX)
	HH=H*0.5D0
	H6=H/6.D0
	DO I=1,N
		YT(I)=YP(I)+HH*DYDX(I)
	END DO
	X=YT(1)
	Y=YT(2)
	UX=YT(3)
	UY=YT(4)
	US=YT(5)
	ALPHAV=YT(6)
	BETAV=YT(7)
	GAMMAV=YT(8)
	VLI=YT(9)
	CHIV=YT(10)
	VNUV=YT(11)
	RHO20V=YT(12)
	RDCV=YT(13)
	CALL DVSB(X,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YP,DYT)
	DO I=1,N
		YT(I)=YP(I)+HH*DYT(I)
	END DO
	X=YT(1)
	Y=YT(2)
	UX=YT(3)
	UY=YT(4)
	US=YT(5)
	ALPHAV=YT(6)
	BETAV=YT(7)
	GAMMAV=YT(8)
	VLI=YT(9)
	CHIV=YT(10)
	VNUV=YT(11)
	RHO20V=YT(12)
	RDCV=YT(13)
	CALL DVSB(X,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YT,DYM)
	DO I=1,N
		YT(I)=YP(I)+H*DYM(I)
		DYM(I)=DYT(I)+DYM(I)
	END DO
	X=YT(1)
	Y=YT(2)
	UX=YT(3)
	UY=YT(4)
	US=YT(5)
	ALPHAV=YT(6)
	BETAV=YT(7)
	GAMMAV=YT(8)
	VLI=YT(9)
	CHIV=YT(10)
	VNUV=YT(11)
	RHO20V=YT(12)
	RDCV=YT(13)
	CALL DVSB(X,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YT,DYT)
	DO I=1,N
		YOUT(I)=YP(I)+H6*(DYDX(I)+DYT(I)+2.D0*DYM(I))
	END DO
	RETURN
	END