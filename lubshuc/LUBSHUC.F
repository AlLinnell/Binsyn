      PROGRAM LUBSHUC											
C     Program to calculate gas dynamics for semidetached binaries
C     See the paper by Lubow and Shu, 1975, ApJ, 198, 383.
C     Also see the paper, same authors, 1976, ApJ, 207, L53.
C      USE MSFLIB
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	COMMON /NUNIT/ NU, NUOUT, IFOUT
      EXTERNAL XL1,PPHIPXYZ,XLMT,PHIR,POTFN,POTCM,COODS,XL2,POTCMRF
	DIMENSION FL1(13)
	DIMENSION XRY(10000),YRY(10000),XRCH(10000),YRCH(10000)
      DIMENSION PHIRY(10000),PHIRYCH(10000)
	DIMENSION RIMX(10000),RIMY(10000),CSTARX(10000),CSTARY(10000)
	DIMENSION XX(20000),YY(20000),VXX(20000),VYY(20000),R1(20000)
	DIMENSION DLT(20000),ACCEL(20000)
	DIMENSION VJAC(20000),UV(20000)
	DIMENSION S(20000),RDC(20000),VI(20000)
      DIMENSION DUXDT(20000),DUYDT(20000),ALPHA(20000),BETA(20000),
     $GAMMA(20000)
	DIMENSION VNU(20000),CHI(20000),RHO20(20000),OMEGZ(20000)
	DIMENSION YP(4),DYDX(4),YOUT(4),YPA(13),DYDXA(13),YOUTA(13)
	CHARACTER * 7 LABEL
	CHARACTER * 36 FL1
	DATA IN1,NOUT1,NOUT2,NOUT3,NOUT4,NOUT5,NOUT6/1,2,3,4,5,6,7/
      DATA NOUT7,NOUT8/8,9/
      DATA NOUT9,NOUT10,NOUT11,NOUT12/10,11,12,13/
	DATA SNRAD/6.9599D10/
	DATA SOLMS/1.9891D33/
	DATA YRSEC/31556925.9747/
	DATA BOLTZ/1.38062D-16/
	DATA GRAVCST/6.6704D-8/
	DATA UNATWT/1.660531D-24/
	DATA PI,TWPI/3.14159265359D0,6.28318530718D0/
	DATA ICON/1/
  100 FORMAT (A36)
  101 FORMAT (3F10.5)
  102 FORMAT (8F10.5)
  103 FORMAT (6E12.5)
  104 FORMAT (1X,'Azimuth of center of impact stream=',13X,F12.3,1X,
     $'degrees')
  106 FORMAT (1X,'Time of flight from L1 point to rim=',8X,F14.1,
     $2X,'seconds')
  107 FORMAT (1X,'Number of steps to reach rim=',15X,I12)
  108 FORMAT (1X,'Stream velocity along X direction at impact=',11X,1P,
     $D9.3,1X,'cm/sec')
  109 FORMAT (1X,'Stream velocity along Y direction at impact=',10X,1P,
     $D10.3,1X,'cm/sec')
  110 FORMAT (1X,'Stream velocity at rim impact=',25X,1P,D9.3,1X,
     $'cm/sec')
  111 FORMAT (1X,'Center of mass location of L1 point=',15X,F12.6)
  112 FORMAT (1X,'Center of mass location of L2 point=',15X,F12.6)
  113 FORMAT (1X,'Center of mass location of L3 point=',15X,F12.6)
  114 FORMAT (1X,'Mass ratio, MD/MC=',33X,F12.6)
  115 FORMAT (1X,'Value of mu=',39X,F12.6)
  116 FORMAT (1X,'Value of theta star=',28X,F12.3,1X,'degrees')
  117 FORMAT (1X,'Value of theta(s)=',30X,F12.3,1X,'degrees')
  118 FORMAT (1X,'Angle between stream and X axis at rim impact=',
     $7X,F7.3,1X,'degrees')
  119 FORMAT (1X,'Angle between stream and rim at impact=',14X,F7.3,
     $1X,'degrees')
  120 FORMAT (1X,'Mass transfer rate, in gm/sec, =',23X,1P,D9.3)
  121 FORMAT (1X,'Mass residing in stream=',31X,1P,D9.3,1X,'gm')
  122 FORMAT (1X,'Orbital period of rim point=',24X,1P,D12.3,1X,
     $'seconds')
  123 FORMAT (1X,'Orbital speed of rim point=',28X,1P,D10.4,1X,
     $'cm/sec')
  124 FORMAT (1X,'Value of perturbation parameter epsilon=',13X,F10.6)
  125 FORMAT (1X,'Value of lambda1=',36X,F12.8)
  126 FORMAT (1X,'Value of m1=',41X,F12.8)
  130 FORMAT (1X,'Mass, contact component (solar mass units)=',10X,F10.6
     $)
  131 FORMAT (1X,'Mass, detached component (solar mass units)=',9X,F10.6
     $) 
  132 FORMAT (1X,'Radius, accretion disk (solar radii)=',16X,F10.6)
  133 FORMAT (1X,'Radius, detached component (solar radii)=',12X,F10.6)
  134 FORMAT (1X,'Separation of components (solar radii)=',14X,F10.6)
  135 FORMAT (1X,'Orbital period (days)=',31X,F10.6)
  137 FORMAT (1X,'Error limit per integration step=',18X,1P,E12.2)
  138 FORMAT (1X,'Integration step value, time variable=',12X,F12.5)
  139 FORMAT (1X,'Radius, accretion disk (units of component separation)
     $=',F8.6)
  140 FORMAT (1X,'Radius, detached component ( /component separation)=',
     $F11.6)
  141 FORMAT (1X,'Effective temperature, contact component=',7X,F10.1,
     $2X,'K')
  142 FORMAT (1X,'Effective temperature, detached component=',6X,F10.1,
     $2X,'K')
  143 FORMAT (1X,'Effective temperature, ambient for stream=',6X,F10.1,
     $2X,'K')
  144 FORMAT (30X,'AVL=',21X,F10.7)
  145 FORMAT (30X,'USQTHS=',18X,F10.7)
  146 FORMAT (30X,'UTHS=',20X,F10.7)
  147 FORMAT (30X,'CTHS=',20X,F10.7)
  148 FORMAT (30X,'BVL=',21X,F10.7)
  149 FORMAT (30X,'CVL=',21X,F10.7)
  150 FORMAT (30X,'R0=',22X,F10.7)
  151 FORMAT (30X,'C1=',22X,F10.7)
  152 FORMAT (30X,'C2=',22X,F10.7)
  153 FORMAT (30X,'C3=',21X,F11.7)
  154 FORMAT (30X,'C4=',22X,F10.7)
  155 FORMAT (30X,'C5=',22X,F10.7)
  156 FORMAT (30X,'C6=',22X,F10.7)
  157 FORMAT (30X,'B1=',22X,F10.7)
  158 FORMAT (30X,'B2=',22X,F10.7)
  159 FORMAT (30X,'GAM0='20X,F10.7)
  160 FORMAT (30X,'BETA0P=',18X,F10.7)
  161 FORMAT (30X,'GAM0P=',18X,F11.7)
  162 FORMAT (30X,'CHI0=',20X,F10.7)
  163 FORMAT (30X,'VNU0P=',19X,F10.7)
  164 FORMAT (30X,'CHI0P=',19X,F10.7)
  165 FORMAT (1X,'Coefficient to transform to cgs length=',15X,1P,E14.7)
  166 FORMAT (1X,'Coefficient to transform to cgs velocity=',13X,1P,
     $E14.7)
  167 FORMAT (1X,'Coefficient to transform to cgs density=',14X,1P,
     $E14.7)
  168 FORMAT (1X,'Coefficient to transform to cgs time=',17X,1P,E14.7)
  169 FORMAT (1X,'Path length, L1 point to rim=',22X,F12.6)
  170 FORMAT (1X,'Value of alpha at rim=',29X,F12.6)
  171 FORMAT (1X,'Value of beta at rim=',30X,F12.6)
  172 FORMAT (1X,'Value of gamma at rim=',29X,F12.6)
  173 FORMAT (1X,'Value of omega at rim=',29X,F12.6)
  174 FORMAT (1X,'Value of nu at rim=',32X,F12.6)
  175 FORMAT (1X,'Value of chi at rim=',31X,F12.6)
  176 FORMAT (1X,'Value of X cood. at rim=',27X,F12.6)
  177 FORMAT (1X,'Value of Y cood. at rim=',27X,F12.6)
  178 FORMAT (1X,'Value of UX at rim=',32X,F12.6)
  179 FORMAT (1X,'Value of UY at rim=',32X,F12.6)
  180 FORMAT (1X,'Radius of curvature at rim=',24X,F12.6)
  181 FORMAT (1X,'Value of (epsilon)^2*rho at rim=',19X,F12.6)
  182 FORMAT (1X,'Initial value, radius of curvature=',16X,F12.6)
  183 FORMAT (1X,'Initial value, alpha=',30X,F12.6)
  184 FORMAT (1X,'Initial value, beta=',31X,F12.6)
  185 FORMAT (1X,'Initial value, gamma=',30X,F12.6)
  186 FORMAT (1X,'Initial value, omega=',30X,F12.6)
  187 FORMAT (1X,'Initial value, nu=',33X,F12.6)
  188 FORMAT (1X,'Initial value, chi=',32X,F12.6)
  200 FORMAT (' ')
C*********************************************************************
      OPEN ( UNIT = 14,   FILE = 'LUBSHUC.ILS', 
     $	STATUS='OLD')
      READ ( 14, 100)     (FL1(J),J=1,13)
	OPEN ( UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	OPEN ( UNIT =NOUT1,FILE = FL1(2), STATUS = 'REPLACE')
	OPEN ( UNIT =NOUT2,FILE = FL1(3), STATUS = 'REPLACE')
      OPEN ( UNIT =NOUT3,FILE = FL1(4), STATUS = 'REPLACE')
	OPEN ( UNIT =NOUT4,FILE = FL1(5), STATUS = 'REPLACE')
	OPEN ( UNIT =NOUT5,FILE = FL1(6), STATUS = 'REPLACE')
	OPEN ( UNIT =NOUT6,FILE = FL1(7), STATUS = 'REPLACE')
      OPEN ( UNIT =NOUT7,FILE = FL1(8), STATUS = 'REPLACE')
	OPEN ( UNIT =NOUT8,FILE = FL1(9), STATUS = 'REPLACE')
      OPEN ( UNIT =NOUT9,FILE = FL1(10),STATUS = 'REPLACE')
	OPEN ( UNIT=NOUT10,FILE = FL1(11),STATUS = 'REPLACE')
	OPEN ( UNIT=NOUT11,FILE = FL1(12),STATUS = 'REPLACE')
	OPEN ( UNIT=NOUT12,FILE = FL1(13),STATUS = 'REPLACE')
C**********************************************************************
      LABEL = 'LUBSHUC'
	NU= IN1
	LBLERR=0
	CALL GTLBL(LABEL, LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL DREAD (VMC, 'F10.4')
	VMC=VMC*SOLMS
	CALL DUMMY (1)
	CALL DREAD (VMD, 'F10.4')
	CALL DUMMY (1)
	VMD=VMD*SOLMS
	CALL DREAD (RA, 'F10.4')
	RA=RA*SNRAD
	CALL DUMMY (1)
	CALL DREAD (RB, 'F10.4')
	RB=RB*SNRAD
	CALL DUMMY (1)
	CALL DREAD (D, 'F12.4')
	D=D*SNRAD
	CALL DUMMY(1)
	CALL DREAD (PER, 'F12.6')
	PER=PER*86400.D0
	OMEGA=TWPI/PER
	CALL DUMMY (1)
	CALL DREAD (VMDOT, 'F12.4')
	VMDOT=VMDOT*SOLMS/YRSEC/1.D6
	CALL DUMMY(1)
	CALL IREAD (KYTRM)
	CALL DUMMY(1)
	CALL DREAD (TEFFC, 'F12.2')
	CALL DUMMY(1)
	CALL DREAD (TEFFD, 'F12.2')
	CALL DUMMY (1)
	CALL DREAD (VMNMASS,'F12.5')
	CALL DUMMY (1)
	CALL DREAD (ERJC, 'D12.1')
	CALL DUMMY (1)
	CALL DREAD (DLTIN, 'D12.1')
	CALL DUMMY (1)
	CALL IREAD (NPOINTS)
	IF (NPOINTS.GT.20000) NPOINTS=20000
	CALL DUMMY (1)
	CALL IREAD (KEYSOL)
	IF ((KEYSOL.LT.1).OR.(KEYSOL.GT.2)) CALL PMDSTOP
	CALL DUMMY (1)
	CALL DREAD (THETA, 'F12.5')
	THETA=THETA/57.2957795131D0
	DT=DLTIN
	ISW=0
	ERLMT=ERJC
C**********************************************************************
C     Coefficient for converting to cgs length units
	COEFLGTH=D
C     Coefficient for converting to cgs velocity units
	COEFVEL=OMEGA*D
C     Coefficient for converting to cgs density units
	COEFDEN=VMDOT/OMEGA/D**3
C     Calculate the dilute ambient radiation Teff
C     An empirical calculation. Take a point close to the L1
C     point, and assume the detached component is partly
C     hidden by the accretion disk.
      TEFFAM=DSQRT(0.9D0*TEFFC**2+0.1D0*TEFFD**2)
C     Calculate epsilon squared
      EPSQ=BOLTZ*TEFFAM/UNATWT/VMNMASS/COEFVEL**2
C     Calculate epsilon
      EPSILON=DSQRT(EPSQ)
C**********************************************************************
C
	A=VMC+VMD
	VMU=VMD/A
	VMOMU=VMC/A
	RIMRCGS=RA
	RIMR=RA/D
	RAC=RIMR
	RDSTAR=RB/D
C**********************************************************************
C     Calculate array for plotting accretion disk rim
      DO K=1,101
		PHI=(K-1)*TWPI/100.D0
		RIMX(K)=RIMR*DCOS(PHI)+VMOMU
		RIMY(K)=RIMR*DSIN(PHI)
		PHIRY(K)=PHI
	END DO
	DO K=1,101
		WRITE (NOUT3,101) RIMX(K),RIMY(K),PHIRY(K)
	END DO
	CLOSE ( UNIT =NOUT3,  STATUS = 'KEEP')
C**********************************************************************
C     Calculate array for plotting central star  (detached component)
      DO K=1,101
		PHI=(K-1)*TWPI/100.D0
		CSTARX(K)=RDSTAR*DCOS(PHI)+VMOMU
		CSTARY(K)=RDSTAR*DSIN(PHI)
		PHIRY(K)=PHI
	END DO
	DO K=1,101
		WRITE (NOUT4,101) CSTARX(K),CSTARY(K),PHIRY(K)
	END DO
C**********************************************************************
C     Write coods of contact star center
      WRITE (NOUT5,101) -VMU,0.D0,0.D0
C     Write coods. of central star center
      WRITE (NOUT6,101) VMOMU,0.D0,0.D0
C
C**********************************************************************
C     Calculate value of L1 point X coordinate
	IF (VMU.EQ.0.D0) VXL1=1.D0
	IF (VMU.EQ.1.D0) VXL1=-1.D0
	IF ((VMU.GT.0.D0).AND.(VMU.LT.1.D0)) THEN
		VXL1=RTSAFE(XL1,VMU,1.D0,-1.D0,1.D-6)
	END IF
	DO K=1,100
	X=-VMU-K/100.D0
	CALL POTCM(X,0.D0,0.D0,VMU,VMOMU,POT)
	END DO
	V1=VXL1-1.D0+VMU
	V2=V1+1.D0
C**********************************************************************
C     Calculate critical potential through L1 point
      CALL POTCM(VXL1,0.D0,0.D0,VMU,VMOMU,PHICR)
C**********************************************************************
C     Calculate intersections of critical potential and X axis.
C     First, locate the L2 point
C     Start just beyond the center of the detached component.
      XSTRT=VMOMU+0.01D0
C     POTERR1 will be negative.
	CALL POTCMRF(XSTRT,0.D0,0.D0,VMU,VMOMU,PHICR,POTERR1)
	DO K=1,100
		X=XSTRT+K/100.D0
		CALL POTCMRF(X,0.D0,0.D0,VMU,VMOMU,PHICR,POTERR)
C     Eventually POTERR will become positive
		IF (POTERR1/POTERR.LT.0.D0) GO TO 2
	END DO
	CALL PMDSTOP
    2 X2=X
C     The last X before POTERR became positive gave a negative POTERR.
      X1=X-0.01D0
C     XLM2 marks the L2 point
      XLM2=RTBISB(POTCMRF,VMU,VMOMU,PHICR,X1,X2,1.D-6,IER)
	IF (IER.NE.0) CALL PMDSTOP
C**********************************************************************
C
C     Now locate the L3 point. Start just beyond the center of the
C     contact component.
      XSTRT=-VMU-0.0001D0
C     POTERR1 will be negative.
	CALL POTCMRF(XSTRT,0.D0,0.D0,VMU,VMOMU,PHICR,POTERR1)
	DO K=1,200
	X=XSTRT-K/200.D0
	CALL POTCMRF(X,0.D0,0.D0,VMU,VMOMU,PHICR,POTERR)
C     Eventually POTERR will become positive.
	IF (POTERR1/POTERR.LT.0.D0) GO TO 3
	END DO
	CALL PMDSTOP
    3 X2=X
C     Two steps back from the X that produced a positive POTERR,
C     the value of POTERR was negative.
      X1=X+0.01D0
	XLM3=RTBISB(POTCMRF,VMU,VMOMU,PHICR,X1,X2,1.D-6,IER)
C     XLM3 designates the L3 point.
	IF (IER.NE.0) CALL PMDSTOP
C**********************************************************************
C     Calculate points on contact component equator
      XMX=VXL1+VMU
	XMN=0.5*XMX
	ISTAR=1
	KLMT=100
	VLMT=KLMT
	DO K=1,KLMT+1
		IF ((K.GT.1).AND.(K.LT.(KLMT+1))) THEN
			PHI=(K-1)*TWPI/VLMT
			R=RTBISA(POTFN,ISTAR,ICON,VMU,VMOMU,PHI,PHICR,
     $		XMN,XMX,1.D-4,IER)
			IF(IER.NE.0) THEN
				CALL PMDSTOP
			END IF
			X=R*DCOS(PHI)-VMU
			Y=R*DSIN(PHI)
		END IF
		IF ((K.EQ.1).OR.(K.EQ.(KLMT+1)))THEN
			X=VXL1
			Y=0.D0
			IF (K.EQ.1) PHI=0.D0
			IF (K.EQ.KLMT+1) PHI=TWPI
		END IF
			XRY(K)=X
			YRY(K)=Y
			PHIRY(K)=PHI
	END DO
	DO K=1,KLMT+1
		WRITE (NOUT1,101) XRY(K),YRY(K),PHIRY(K)
	END DO
C**********************************************************************
C     Calculate points on Roche lobe outline.
      XMX=-VXL1+VMOMU
	XMN=0.5*XMX
	ISTAR=2
	KLMT=100
	VLMT=KLMT
	DO K=1,KLMT+1
		ICT=0
		IF ((K.GT.1).AND.(K.LT.(KLMT+1))) THEN
			PHI=(K-1)*TWPI/VLMT-PI
    1			R=RTBISA(POTFN,ISTAR,ICON,VMU,VMOMU,PHI,PHICR,
     $		XMN,XMX,1.D-4,IER)
			IF (IER.NE.0) THEN
				IF (XMX.LT.XLM2+VMOMU) XMX=XLM2+VMOMU+1.D-6
				ICT=ICT+1
				IF (ICT.LE.3) THEN
					XMX=XMX+0.02D0
					GO TO 1
				ELSE
					CALL PMDSTOP
				END IF
			END IF
			X=R*DCOS(PHI)+VMOMU
			Y=R*DSIN(PHI)
			XMXER=XMX-R
			IF (XMXER.LT.0.02D0) THEN
				XMX=XMX+0.02D0
			END IF
			IF (XMXER.GT.0.05D0) THEN
				XMX=R+0.05D0
			END IF
			XMNER=R-XMN
			IF (XMNER.LT.0.02D0) THEN
				XMN=XMN-0.02D0
			END IF
			IF (XMNER.GT.0.05D0) THEN
				XMN=R-0.05D0
			END IF
		END IF
		IF ((K.EQ.1).OR.(K.EQ.(KLMT+1)))THEN
			X=VXL1
			Y=0.D0
			IF (K.EQ.1) PHI=-PI
			IF (K.EQ.KLMT+1) PHI=PI
		END IF
			XRCH(K)=X
			YRCH(K)=Y
			PHIRYCH(K)=PHI
	END DO
	DO K=1,KLMT+1
		WRITE (NOUT2,101) XRCH(K),YRCH(K),PHIRYCH(K)
	END DO
C**********************************************************************
C**********************************************************************
C     Now calculate mass transfer stream
C
C
C
      AVL=VMU/(DABS(VXL1-1.D0+VMU))**3+VMOMU/(DABS(VXL1+VMU))**3
C     Calculate THETAS
C
      FN1=-4.D0/3.D0/AVL+DSQRT(1.D0-8.D0/9.D0/AVL)
	FN2=-DSQRT((8.D0/9.D0/AVL)*(1.D0-2.D0/AVL+3.D0*
     $DSQRT(1.D0-8.D0/9.D0/AVL)))
      TWTHETAS=DASIN(FN2)
C	CHECK=FN1-DCOS(TWTHETAS)
	IF (DABS(CHECK).GT.0.0001D0) CALL PMDSTOP
C     Calculate THETAS in degrees
	THETAS=TWTHETAS/2.D0*57.2957795131
C     Calculate cos(thas)
      COSTHAS=DCOS(0.5D0*TWTHETAS)
C     Calculate sin(thas)
      SINTHAS=DSIN(0.5D0*TWTHETAS)
C     Calculate THSTAR
      FNA=-(2.D0+AVL)/3.D0/AVL
	FNB=2.D0*DSQRT((2.D0*AVL+1.D0)*(AVL-1.D0))/3.D0/AVL
	TWTHSTAR=DACOS(FNA)
C	CHECK=FNB-DSIN(TWTHSTAR)
	IF (DABS(CHECK).GT.0.00001D0) CALL PMDSTOP
C     Calculate THSTAR in degrees
      THSTAR=TWTHSTAR/2.D0*57.2957795131
C     Calculate U^2(THETAS)
      USQTHS=1.D0+AVL/2.D0*(3.D0*FN1+1.D0)
C     Calculate U(THETAS)
      UTHS=-0.75D0*AVL*FN2
C     Calculate C(THETAS)
      CTHS=DSQRT(AVL*(USQTHS-AVL+2.D0))/TWPI/UTHS
C     Calculate the leading bracket in the expression for CAPTHETA
      FN3=USQTHS-AVL+2.D0
C     Calculate CAPTHETA
      CAPTHETA=0.5D0*FN3*(THETA-THETAS)**2
C     Calculate UVL
      UVL=UTHS+2.D0*(THETA-THETAS)
C     Calculate VVL
      VVL=-UTHS*(THETA-THETAS)
C
C     Choose order of solution.
      IF (KEYSOL.EQ.2) GO TO 201
C
C	First order solution
C     Now calculate stream in vicinity of L1
C     Start at LI
C     The initial motion is on a straight line.
      J=1
	VR1=0.0001D0
	X=VR1*COSTHAS
	Y=VR1*SINTHAS
      XX(J)=X+VXL1
	YY(J)=Y
	UV(J)=VR1*UTHS
	UX=UV(J)*COSTHAS
	UY=UV(J)*SINTHAS
	VXX(J)=UX
	VYY(J)=UY
	DUXDT(J)=(2.D0*AVL+1.D0)*X+2.D0*UY
	DUYDT(J)=-(AVL-1.D0)*Y-2.D0*UX
	ACCEL(J)=DSQRT(DUXDT(J)**2+DUYDT(J)**2)
	DLT(J)=DT
	S(J)=VR1
	R1(J)=VR1
	DO J=2,5
		DR1=UR1*DT
		VR1=VR1+DR1
		UR1=VR1*UTHS
		X=VR1*COSTHAS
		Y=VR1*SINTHAS
		S(J)=VR1
		XX(J)=X+VXL1
		YY(J)=Y
		VXX(J)=UR1*COSTHAS
		VYY(J)=UR1*SINTHAS
		UX=VXX(J)
		UY=VYY(J)
		UV(J)=UR1
		DUXDT(J)=(UV(J)-UV(J-1))/DT*COSTHAS
		DUYDT(J)=(UV(J)-UV(J-1))/DT*SINTHAS
		ACCEL(J)=DSQRT(DUXDT(J)**2+DUYDT(J)**2)
		DLT(J)=DLT(J-1)+DT
		R1(J)=VR1
		VI(J)=DATAN(UY/UX)
C     Check Jacobi integral
		CALL POTCM(X+VXL1,Y,Z,VMU,VMOMU,VPOT)
		VJAC(J)=0.5D0*UV(J)**2+VPOT-PHICR
	END DO
C	DO J=1,150
C	WRITE (NOUT12,3001) J,XX(J),YY(J),VXX(J),VYY(J),UV(J)
C	WRITE (NOUT12,3002)	ACCEL(J),DUXDT(J),DUYDT(J),RDC(J),VI(J)
C	WRITE (NOUT12,3002) S(J),ALPHA(J),BETA(J), GAMMA(J),VJAC(J)
C	WRITE (NOUT12,3003)
C	END DO
C	IF (J.NE.0) STOP
C
C     ****************************************************************
C     ****************************************************************
C
C     Start with point near L1
      J=J-1
	XV=XX(J)
	X=XX(J)-VXL1
	Y=YY(J)
	UX=VXX(J)
	UY=VYY(J)
	YP(1)=XV
	YP(2)=Y
	YP(3)=UX
	YP(4)=UY	
C     Set initial time step
	DT=DLTIN
C
   50 CALL DERIVS(XV,Y,Z,VMU,VMOMU,UX,UY,YP,DYDX)
	CALL RK4(XV,Y,Z,VMU,VMOMU,YP,DYDX,4,DT,YOUT)
	DLT(J+1)=DLT(J)+DT
	YP(1)=YOUT(1)
	YP(2)=YOUT(2)
	YP(3)=YOUT(3)
	YP(4)=YOUT(4)
	XV=YP(1)
	X=XV-VXL1
	Y=YP(2)
	UX=YP(3)
	UY=YP(4)
	XX(J+1)=XV
	YY(J+1)=Y
	VXX(J+1)=UX
	VYY(J+1)=UY
      CALL DERIVS(XV,Y,Z,VMU,VMOMU,UX,UY,YP,DYDX)
	DUXDT(J+1)=DYDX(3)
	DUYDT(J+1)=DYDX(4)
	ACCEL(J+1)=DSQRT(DUXDT(J+1)**2+DUYDT(J+1)**2)
	R1(J+1)=DSQRT((XV-VXL1)**2+Y**2)
	S(J+1)=S(J)+DSQRT((XX(J+1)-XX(J))**2+(YY(J+1)-YY(J))**2)
	UV(J+1)=DSQRT(UX**2+UY**2)
	RDC(J+1)=UV(J+1)**3/(UY*DUXDT(J+1)-UX*DUYDT(J+1))
	VI(J+1)=DATAN(UY/UX)
	ALPHA(J+1)=2.D0-UV(J+1)/RDC(J+1)
C     Check Jacobi integral
	CALL POTCM(XV,Y,Z,VMU,VMOMU,VPOT)
	VJAC(J+1)=0.5D0*UV(J+1)**2+VPOT-PHICR
    5 J=J+1
      DIST=DSQRT((XV-VMOMU)**2+Y**2)
	IF (DIST.LE.RAC) THEN
		IF (ISW.EQ.0) THEN
			JS=J
			ISW=1
		END IF
		IF (KYTRM.EQ.1) GO TO 10
	END IF
      IF (J.LT.NPOINTS) GO TO 50
	IF (KEYSOL.EQ.1) GO TO 10
  201	IF (KEYSOL.NE.2) CALL PMDSTOP	
C     
C     Calculate stream flow.
C     Calculate lambda1
      VLAMBDA1=DSQRT(0.5D0*((AVL-2.D0)+(9.D0*AVL**2-8.D0*AVL)**0.5D0))
C     Calculate M1
      VM1=(VLAMBDA1**2-(2.D0*AVL+1.D0))/2.D0/VLAMBDA1
      BVL=VMU/(1.D0-VMU+VXL1)**4-VMOMU/(VXL1-VMU)**4
	CVL=-3.D0*VMU*(1.D0-VMU+VXL1)/DABS((1.D0-VMU+VXL1)**5)-
     $3.D0*VMOMU*(VXL1-VMU)/DABS((VXL1-VMU)**5)
      C5=CVL
	C6=1.5D0*BVL
	TERMA=C5+C6*VM1**2
	DENOMV=(4.D0*VLAMBDA1**2+AVL-1.D0)*(2.D0*AVL+1.D0-4.D0*
     $VLAMBDA1**2)-16.D0*VLAMBDA1**2
	B1=-(8.D0*C6*VM1*VLAMBDA1+TERMA*(AVL-1.D0+4.D0*VLAMBDA1**2))
     $/DENOMV
	B2=(TERMA*4.D0*VLAMBDA1+(2.D0*AVL+1.D0-4.D0*VLAMBDA1**2)*2.D0*C6
     $*VM1)/DENOMV
	R0=(1.D0+VM1**2)**1.5D0/2.D0/(VM1*B1-B2)
	C1=-2.D0*AVL*SINTHAS**2+AVL*COSTHAS**2-1.D0
	C2=AVL
	C3=3.D0*AVL*DSIN(TWTHETAS)/R0+3.D0*BVL*(4.D0-5.D0*COSTHAS**2)*
     $COSTHAS
	C4=CVL*COSTHAS
	GAM0=4.D0+C1
	BETA0P=(6.D0*VLAMBDA1/R0-C3)*VLAMBDA1/(VLAMBDA1**2+2.D0*GAM0)
	GAM0P=-2.D0*BETA0P*GAM0/VLAMBDA1
	CHI0=C2
	VNU0P=-C4*VLAMBDA1/(VLAMBDA1**2+2.D0*CHI0)
	CHI0P=-2.D0*VNU0P*CHI0/VLAMBDA1
C
C
C     Vicinity of L1 region
C     Initialize variables
      J=1
	VR1=0.0001D0
	X=VR1*COSTHAS
	Y=VR1*SINTHAS
      XX(J)=X+VXL1
	YY(J)=Y
	VXX(J)=VLAMBDA1*X*(1.D0+B1*X)
	VYY(J)=VLAMBDA1*Y*(1.D0+B2*Y/VM1**2)
	UX=VXX(J)
	UY=VYY(J)
	DUXDT(J)=VLAMBDA1**2*X*(1.D0+3.D0*B1*X)
	DUYDT(J)=VLAMBDA1**2*Y*(1.D0+3.D0*B2*Y/VM1**2)
	ACCEL(J)=DSQRT(DUXDT(J)**2+DUYDT(J)**2)
	DLT(J)=0.D0
	R1(J)=VR1
	S(J)=VR1
	UV(J)=DSQRT(VXX(J)**2+VYY(J)**2)
	ALPHA(J)=2.D0-VLAMBDA1*S(J)/R0
	BETA(J)=BETA0P*S(J)
	GAMMA(J)=GAM0-GAM0P*S(J)
	CHI(J)=CHI0+CHI0P*S(J)
	VNU(J)=VNU0P*S(J)
	RHO20(J)=(GAMMA(J)*CHI(J))**0.5D0/TWPI/UV(J)
	RDC(J)=R0
	VI(J)=DATAN(UY/UX)
	CALL PP2CMPZ(XX(J),Y,Z,VMU,VMOMU,PP2CMTZ)
	OMEGZ(J)=DSQRT(PP2CMTZ)
C     Check Jacobi integral
	CALL POTCM(X+VXL1,Y,Z,VMU,VMOMU,VPOT)
	VJAC(J)=0.5D0*UV(J)**2+VPOT-PHICR
C
C     End of initializing section
C
  202	XX(J)=X+VXL1
	YY(J)=Y
	VXX(J)=VLAMBDA1*X*(1.D0+B1*X)
	VYY(J)=VLAMBDA1*Y*(1.D0+B2*Y/VM1**2)
	UX=VXX(J)
	UY=VYY(J)
	UV(J)=DSQRT(VXX(J)**2+VYY(J)**2)
	DUXDT(J)=VLAMBDA1**2*X*(1.D0+3.D0*B1*X)
	DUYDT(J)=VLAMBDA1**2*Y*(1.D0+3.D0*B2*Y/VM1**2)
	ACCEL(J)=DSQRT(DUXDT(J)**2+DUYDT(J)**2)
	DLT(J)=DLT(J-1)+DT
	R1(J)=DSQRT((XX(J)-VXL1)**2+YY(J)**2)
	YP(1)=X
	YP(2)=Y
	YP(3)=UX
	YP(4)=UY
	CALL PP2CMPZ(XX(J),Y,Z,VMU,VMOMU,PP2CMTZ)
	OMEGZ(J)=DSQRT(PP2CMTZ)
	CALL DVSA(X,Y,Z,VLAMBDA1,VM1,B1,B2,UX,UY,YP,DYDX)
	CALL RK4A(X,Y,Z,VLAMBDA1,VM1,B1,B2,UX,UY,YP,DYDX,4,DT,YOUT)
	YP(1)=YOUT(1)
	YP(2)=YOUT(2)
	YP(3)=YOUT(3)
	YP(4)=YOUT(4)
	X=YP(1)
	Y=YP(2)
	UX=YP(3)
	UY=YP(4)
	XX(J+1)=X+VXL1
	YY(J+1)=Y
	VXX(J+1)=UX
	VYY(J+1)=UY
	CALL DVSA(X,Y,Z,VLAMBDA1,VM1,B1,B2,UX,UY,YP,DYDX)
	DUXDT(J+1)=DYDX(3)
	DUYDT(J+1)=DYDX(4)
	ACCEL(J+1)=DSQRT(DUXDT(J+1)**2+DUYDT(J+1)**2)
	R1(J+1)=DSQRT((XX(J+1)-VXL1)**2+Y**2)
	S(J+1)=S(J)+DSQRT((XX(J+1)-XX(J))**2+(YY(J+1)-YY(J))**2)
	UV(J+1)=DSQRT(VXX(J+1)**2+VYY(J+1)**2)
	ALPHA(J+1)=2.D0-VLAMBDA1*S(J+1)/R0
	BETA(J+1)=BETA0P*S(J+1)
	GAMMA(J+1)=GAM0-GAM0P*S(J+1)
	CHI(J+1)=CHI0+CHI0P*S(J+1)
	VNU(J+1)=VNU0P*S(J+1)
	RHO20(J+1)=(GAMMA(J+1)*CHI(J+1))**0.5D0/TWPI/UV(J+1)
C
C     Calculate radius of curvature
	DENOM=2.D0*(VM1*B1-B2)
	UVSQ=1.D0+VM1**2
	RDCA=UVSQ**1.5D0/DENOM
	RDC(J+1)=RDCA
C     Calculate the variable VI
      VI(J+1)=DATAN(VYY(J+1)/VXX(J+1))
C     Accumulate time of flight
	DLT(J+1)=DLT(J)+DT
	CALL PP2CMPZ(XX(J),Y,Z,VMU,VMOMU,PP2CMTZ)
	OMEGZ(J+1)=DSQRT(PP2CMTZ)
C     Check Jacobi integral
	CALL POTCM(X+VXL1,Y,Z,VMU,VMOMU,VPOT)
	VJAC(J+1)=0.5D0*UV(J+1)**2+VPOT-PHICR
	J=J+1
	IF (J.LT.5) GO TO 202
C	DO J=1,150
C	WRITE (NOUT12,3001) J,XX(J),YY(J),VXX(J),VYY(J),UV(J)
C	WRITE (NOUT12,3002)	ACCEL(J),DUXDT(J),DUYDT(J),RDC(J),VI(J)
C	WRITE (NOUT12,3002) S(J),ALPHA(J),BETA(J), GAMMA(J),VJAC(J)
C	WRITE (NOUT12,3003)
C	END DO
C	DO J=1,20
C	WRITE (NOUT12,3000) J,S(J),UV(J),ALPHA(J),BETA(J),GAMMA(J),
C     $CHI(J),VNU(J),RHO20(J),RDC(J),VI(J)
C	END DO
C	DO J=2,20
C	WRITE (NOUT12,3000) J,(S(J)-S(J-1))/DT,(UV(J)-UV(J-1))/DT,
C     $(ALPHA(J)-ALPHA(J-1))/DT,(BETA(J)-BETA(J-1))/DT,
C     $(GAMMA(J)-GAMMA(J-1))/DT,(CHI(J)-CHI(J-1))/DT,(VNU(J)-VNU(J-1))/DT
C     $,(RHO20(J)-RHO20(J-1))/DT,(RDC(J)-RDC(J-1))/DT,(VI(J)-VI(J-1))/DT
C	END DO
C      IF (J.NE.0) STOP
C      IF (J.NE.0) GO TO 10
 3001 FORMAT (I3,5D15.7)
 3002 FORMAT (3X,5D15.7)
 3003 FORMAT (' ')
C
C     End of L1 vicinity section
C
    9 XX(J)=X+VXL1
      YY(J)=Y
	UX=VXX(J)
	UY=VYY(J)
	UV(J)=DSQRT(UX**2+UY**2)
	XV=XX(J)
	VLI=VI(J)
	AX=DUXDT(J)
	AY=DUYDT(J)
	AS=ACCEL(J)
	ALPHAV=ALPHA(J)
	BETAV=BETA(J)
	GAMMAV=GAMMA(J)
	CHIV=CHI(J)
	VNUV=VNU(J)
	RHO20V=RHO20(J)
	RDCV=RDC(J)
	CALL DVSB(XV,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YPA,DYDXA)
	CALL RK4B(XV,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YPA,DYDXA,13,DT,
     $YOUTA)
	DO I=1,13
		YPA(I)=YOUTA(I)
	END DO
	XX(J+1)=YPA(1)
	YY(J+1)=YPA(2)
	XV=YPA(1)
	X=XV-VXL1
	Y=YPA(2)
	UX=YPA(3)
	UY=YPA(4)
	UV(J+1)=YPA(5)
	DS=UV(J+1)*DT
	S(J+1)=S(J)+DS
	ALPHA(J+1)=YPA(6)
	BETA(J+1)=YPA(7)
	GAMMA(J+1)=YPA(8)
	VI(J+1)=YPA(9)
	CHI(J+1)=YPA(10)
	VNU(J+1)=YPA(11)
	RHO20(J+1)=YPA(12)
	RDC(J+1)=YPA(13)
	CALL DVSB(XV,Y,Z,VMU,VMOMU,UX,UY,VLI,AX,AY,AS,
     $ALPHAV,BETAV,GAMMAV,CHIV,VNUV,RHO20V,RDCV,YPA,DYDXA)
	VXX(J+1)=DYDXA(1)
	VYY(J+1)=DYDXA(2)
	DUXDT(J+1)=DYDXA(3)
	DUYDT(J+1)=DYDXA(4)
	ACCEL(J+1)=DSQRT(DUXDT(J+1)**2+DUYDT(J+1)**2)
	R1(J+1)=DSQRT((XV-VXL1)**2+Y**2)
	DLT(J+1)=DLT(J)+DT
	CALL PP2CMPZ(XV,Y,Z,VMU,VMOMU,PP2CMTZ)
	DVLI=DCOS(VI(J+1))**2*(UX*DUYDT(J+1)-UY*DUXDT(J+1))/UX**2
	CALL P2CMPNPS(XV,Y,Z,VMU,VMOMU,VI(J+1),DVLI,UX,UY,UV(J+1),
     $DUXDT(J+1),DUYDT(J+1),ACCEL(J+1),P2CMTNTS)
	CK1=2.D0*VLAMBDA1*(1.D0-VLAMBDA1/RDC(J+1)*S(J+1))
	CALL P2CMPN(XV,Y,Z,VMU,VMOMU,VI(J+1),DVLI,UX,UY,P2CMTN)
	CK2=GAM0-4.D0-S(J+1)*(GAM0P-6.D0*VLAMBDA1/RDC(J+1)+
     $BETA0P*VLAMBDA1)
	OMEGZ(J+1)=DSQRT(PP2CMTZ)
C     Check Jacobi integral
	CALL POTCM(X+VXL1,Y,Z,VMU,VMOMU,VPOT)
	VJAC(J+1)=0.5D0*UV(J+1)**2+VPOT-PHICR
	J=J+1 
	WRITE (NOUT12,3001) J,XX(J),YY(J),VXX(J),VYY(J),UV(J)
	WRITE (NOUT12,3002)	ACCEL(J),DUXDT(J),DUYDT(J),RDC(J),VI(J)
	WRITE (NOUT12,3002) DVLI,P2CMTN,P2CMTNTS,DYDXA(6),DYDXA(7)
	WRITE (NOUT12,3002) R1(J),S(J),ALPHA(J),BETA(J),VJAC(J)
	WRITE (NOUT12,3002) PP2CMTZ,CK1,CK2
	WRITE (NOUT12,3003)
C     If distance is less than radius of accretion disk, terminate run
C     under control of KYTRM.
  211	IF (DIST.LE.RAC) THEN
			IF (ISW.EQ.0) THEN
				JS=J
				ISW=1
			END IF
			IF (KYTRM.EQ.1) GO TO 10
	END IF
	IF (J.LT.30) GO TO 9
	IF (J.NE.0) STOP
C      IF (J.GT.168) THEN
C		ZZZ=0.D0
C	END IF
C	IF (J.LT.NPOINTS) GO TO 9
 2000	FORMAT (1X,I3,1X,6E12.5)
 2001	FORMAT (10I5)
 2002 FORMAT (1X,I5,1X,4F18.15)
 2003 FORMAT (1X,4F18.15)
C     ***************************************************************
C     Calculate azimuth of stream impact
   10 AZ=DATAN(YY(JS)/(XX(JS)-VMOMU))*57.2957795131
      TOTTM=DLT(JS)
C     List important parameters
      WRITE (NOUT12,130) VMC/SOLMS
	WRITE (NOUT12,131) VMD/SOLMS
	WRITE (NOUT12,132) RA/SNRAD
	WRITE (NOUT12,139) RIMR
	WRITE (NOUT12,140) RDSTAR
	WRITE (NOUT12,133) RB/SNRAD
	WRITE (NOUT12,134) D/SNRAD
	WRITE (NOUT12,135) PER/86400.D0
	WRITE (NOUT12,137) ERJC
	WRITE (NOUT12,138) DLTIN
	WRITE (NOUT12,114) VMD/VMC
	WRITE (NOUT12,115) VMU

      WRITE (NOUT12,111) VXL1
	WRITE (NOUT12,112) XLM2
	WRITE (NOUT12,113) XLM3
	WRITE (NOUT12,141) TEFFC
	WRITE (NOUT12,142) TEFFD
	WRITE (NOUT12,143) TEFFAM
	WRITE (NOUT12,124) EPSILON

      WRITE (NOUT12,144) AVL
	WRITE (NOUT12,116) THSTAR
	WRITE (NOUT12,117) THETAS
	WRITE (NOUT12,145) USQTHS
	WRITE (NOUT12,146) UTHS
	WRITE (NOUT12,147) CTHS
	WRITE (NOUT12,125) VLAMBDA1
	WRITE (NOUT12,126) VM1
	WRITE (NOUT12,148) BVL
	WRITE (NOUT12,149) CVL
	WRITE (NOUT12,150) R0
	WRITE (NOUT12,151) C1
	WRITE (NOUT12,152) C2
	WRITE (NOUT12,153) C3
	WRITE (NOUT12,154) C4
	WRITE (NOUT12,155) C5
	WRITE (NOUT12,156) C6
	WRITE (NOUT12,157) B1
	WRITE (NOUT12,158) B2
	WRITE (NOUT12,159) GAM0
	WRITE (NOUT12,160) BETA0P
	WRITE (NOUT12,161) GAM0P
	WRITE (NOUT12,162) CHI0
	WRITE (NOUT12,163) VNU0P
	WRITE (NOUT12,164) CHI0P
	WRITE (NOUT12,182) RDC(1)
	WRITE (NOUT12,183) ALPHA(1)
	WRITE (NOUT12,184) BETA(1)
	WRITE (NOUT12,185) GAMMA(1)
	WRITE (NOUT12,186) OMEGZ(1)
	WRITE (NOUT12,187) VNU(1)
	WRITE (NOUT12,188) CHI(1)
	WRITE (NOUT12,169) S(JS)
	WRITE (NOUT12,176) XX(JS)
	WRITE (NOUT12,177) YY(JS)
	WRITE (NOUT12,178) VXX(JS)
	WRITE (NOUT12,179) VYY(JS)
	WRITE (NOUT12,180) RDC(JS)
	WRITE (NOUT12,181) RHO20(JS)
	WRITE (NOUT12,170) ALPHA(JS)
	WRITE (NOUT12,171) BETA(JS)
	WRITE (NOUT12,172) GAMMA(JS)
	WRITE (NOUT12,173) OMEGZ(JS)
	WRITE (NOUT12,174) VNU(JS)
	WRITE (NOUT12,175) CHI(JS)
	WRITE (NOUT12,165) COEFLGTH
	WRITE (NOUT12,166) COEFVEL
	WRITE (NOUT12,167) COEFDEN
	COEFTM=COEFLGTH/COEFVEL
	WRITE (NOUT12,168) COEFTM

      WRITE (NOUT12,104) AZ
C     The time scale is transformed by COEFLGTH/COEFVEL
      TOTTM=TOTTM*COEFTM
	WRITE (NOUT12,106) TOTTM
	WRITE (NOUT12,107) JS
	AGLU=DATAN(VYY(JS)/VXX(JS))*57.2957795131
	WRITE (NOUT12,118) AGLU
	AGCOL=90.D0-DABS(AGLU)-DABS(AZ)
	WRITE (NOUT12,119) AGCOL
	VKEPPER=DSQRT(TWPI**2/GRAVCST/VMD*RIMRCGS**3.D0)
	VKEPVEL=TWPI*RIMRCGS/VKEPPER
	WRITE (NOUT12,122) VKEPPER
	WRITE (NOUT12,108) VXX(JS)*COEFVEL
	WRITE (NOUT12,109) VYY(JS)*COEFVEL
	WRITE (NOUT12,110) UV(JS)*COEFVEL
	WRITE (NOUT12,123) VKEPVEL
	WRITE (NOUT12,120) VMDOT
	STRMASS=VMDOT*TOTTM
	WRITE (NOUT12,121) STRMASS
C     Write output, produce cgs data, and write it to output.
   11 ILIM=J
      DO I=1,ILIM
		R1(I)=DSQRT(XX(I)**2+YY(I)**2)
	  	WRITE (NOUT7,101) XX(I),YY(I)
		WRITE (NOUT8,101) VXX(I),VYY(I),UV(J)
		WRITE (NOUT10,103) ALPHA(I),BETA(I),GAMMA(I),CHI(I),VNU(I)
	END DO
C     List values of Jacobi integral
      WRITE (NOUT11,102) (VJAC(K),K=1,J)
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	DO I=1,ILIM
	WRITE (NOUT9,103) S(I)
	END DO
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	DO I=1,ILIM
	WRITE (NOUT9,103)  S(I)
	END DO
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	DO I=1,ILIM
	WRITE (NOUT9,103) S(I)
	END DO
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
      DO I=1,ILIM
C	WRITE (NOUT9,103) S(I),RDC(I),VNUM,DENOM,VYY(I)*DUXDT(I),RDCP
	WRITE (NOUT9,103) S(I),ALPHA(I),BETA(I),GAMMA(I),CHI(I),VNU(I)
	END DO
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
	WRITE (NOUT9,200)
C
      CLOSE ( UNIT = IN1,   STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT1,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT2,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT4,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT5,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT6,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT7,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT8,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT9,  STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT10, STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT11, STATUS = 'KEEP')
	CLOSE ( UNIT =NOUT12, STATUS = 'KEEP')
      STOP
	END PROGRAM LUBSHUC		      
