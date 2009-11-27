      PROGRAM ACPGCRG											  
C	USE MSFLIB												 
C     Program for defining accretion disk corona
C     It includes a capability to calculate bremsstrahlung.
C     The bremsstrahlung procedure is the following:
C		(1) Erect a screen behind the accretion disk.
C		(2) Establish boundaries of the projection of the accretion
C				disk on the screen.
C		(3) Divide the enclosed region into a large number of area
C				segments.
C		(4) Produce a vector directed toward the observer, 
C				perpendicular to each area segment.
C		(5) Trace each vector through the plasma, following the
C				procedure of control key 4. Initially step from the
C				screen until we reach the start of the plasma on
C				that vector.
C		(6) Having reached the plasma, calculate the change in 
C				light intensity for each wavelength in an array of
C				wavelengths, for a step of length DS, using an
C				expression of the form
C				dI(lambda)=[K(lambda)N^2B(T)-K(lambda)I(lambda)N^2
C					-6.65*10^(-25)NI(lambda)]*DS
C				Note that N and T are local values for each voxel.
C				See Rybicki and Lightman, p.159ff for values of K.
C				see Jameson & Longmore, 1976,MNRAS, 174, 217
C		(7) Accumulate the intensity toward the observer for each
C				wavelength and each area segment as each vector
C				passes through the plasma. Extend each vector until
C				it emerges from the plasma. At this point we have the
C				data to calculate the net uneclipsed light at each
C				wavelength. Use a program like ACPGDRG. 
C
	INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
	INCLUDE 'COMM.FOR'
	INCLUDE 'COMR.FOR'
	INCLUDE 'COMN.FOR'
	INCLUDE 'COMPH.FOR'
	DIMENSION FL1(22)
	DIMENSION TARY(3,3),TARYP(3,3),WL(90),
     $UC(2),C1(2),C2(2),T0(2),A(2),BCF(2),UA(2)
	DIMENSION VFLX(52,91,39,2),VFLXA(52,92,39,2)
	DIMENSION VFLXB(52,91,39,2)
	DIMENSION XAN(52,91,39,2),YAN(52,91,39,2),ZAN(52,91,39,2),
     $CX(52,91,39,2),CY(52,91,39,2),CZ(52,91,39,2),
     $DS(52,91,39,2),VLC(52,91,39,2),VMC(52,91,39,2),VNC(52,91,39,2)
	DIMENSION XANP(52,91,39,2),YANP(52,91,39,2),ZANP(52,91,39,2)
	DIMENSION VOXEL(52,91,39,2),VNMEL(52,91,39,2),DENSEL(52,91,39,2)
	DIMENSION CXP(52,91,39,2),CYP(52,91,39,2),CZP(52,91,39,2)
	DIMENSION DPART(52,91,39,2),TCOR(52,91,39,2)
	DIMENSION DENSN2(52,91,39,2)
	DIMENSION PJAR(52,91,39,2)
	DIMENSION VLX(40401),VLY(40401),VLZ(40401),KIDX(40401)
	DIMENSION TMPRFA(52,501),SMV(52,501),RMIFLX(25,501),
     $TMPRF(52,501),TMPRR(25,501)
      DIMENSION CRHT(52),NVDAN(52),RDDENS(52),VCOEFF(52)
	DIMENSION CSCTR(52,91,39,2),VSBKY(52,91,39,2)
	DIMENSION PXR(25,501),PYR(25,501),PZR(25,501),PXF(52,501),
     $PYF(52,501),PZF(52,501),PRFH(52,501),PRSH(2,201,2),STX(2,40401),
     $STY(2,40401),STZ(2,40401),STVS(2,40401),CSGR(25,501),CSGF(52,501),
     $XHZ(501),YHZ(501),RXHZL(20),RYHZL(20),RXHZG(20),RYHZG(20),
     $PRVFG(20),PRVFL(20),ANG(501),RD(501),
     $VXP(2,201,2),VYP(2,201,2),PRRH(25,501)
      DIMENSION VTAUSV(1500),XECPJ(40401),YECPJ(40401),ZECPJ(40401)
	DIMENSION FXBRE(100,100,100,2),VSBBRE(100,100,2),BREX(100,2),
     $BREY(100,2)
	DIMENSION WVLST(100),SFXBRE(100),TFXBRE(100)
	DIMENSION FQLST(100)
	DIMENSION VLIZ(100,10000)
C
	COMMON /NUNIT/ NU,NUOUT,IFOUT
	CHARACTER * 7 LABEL
	CHARACTER * 24 FL1
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 4 RESULT
C     Program to calculate geometry for coronal structure addition
C     to accretion disk.
C
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
	DATA IN10,IN11,IN12,IN13,IN14,IN15,IN16/10,11,12,13,14,15,16/
	DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5,NOUT6/17,18,19,20,21,22/
C     Number of division points in integration over irradiating star.
	DATA NTHETA/11/
	DATA EPS/1.D-5/
  100 FORMAT (A24)
  101 FORMAT (8D10.3)
  103 FORMAT (1H1)
  104 FORMAT (1X,'Program ACPGCRG',25X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/,13X,
     $'**OUT**',/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'*')
  105 FORMAT (10F8.5)
  106 FORMAT (10I5)
  107 FORMAT (' ')
  108 FORMAT (4F18.13)
  109 FORMAT (1X,'Total scattered light toward observer=',20X,1P,D12.5
     $,2X,'cgs')
  110 FORMAT (1X,'Scattered light not intercepted by accretion disk=',
     $8X,1P,D12.5,2X,'cgs')
  111 FORMAT (1X,'Scattered light eclipsed by companion=',20X,1P,D12.5
     $,2X,'cgs',/,5X,'excluding light intercepted by accretion disk')
  112 FORMAT (1X,'Scattered light not blocked by any object=',16X,1P,
     $D12.5,2X,'cgs')
  113 FORMAT (1X,'Wavelength number',40X,I5,/,1X,'Wavelength',45X,
     $F12.4,2X,'microns')
  114 FORMAT (5D15.8)
  115 FORMAT (1X,'Total projected area of voxels=',27X,1P,D12.5,2X,
     $'cgs')
  116 FORMAT (1X,'Projected area of voxels blocked by accret. disk=',
     $9X,1P,D12.5,2X,'cgs')
  117 FORMAT (1X,'Projected area of voxels eclipsed by companion=',
     $11X,1P,D12.5,2X,'cgs',/,5X,
     $'excluding projected area blocked by accret. disk')
  118 FORMAT (1X,'Projected area of voxels not blocked by accretion disk
     $=',3X,1P,D12.5,2X,'cgs')
  119 FORMAT (10F8.4)
  120 FORMAT (1X,'Bremsstrahlung at specified wavelength, observed',
     $10X,1P,D12.5,2X,'cgs')
  121 FORMAT (1X,'Sum of observed bremsstrahlung and scattered light'
     $,8X,1P,D12.5,2X,'cgs')
  122 FORMAT (1X,'Total eclipsed bremsstrahlung and scattered light',
     $9X,1P,D12.5,2X,'cgs')
  123 FORMAT (1X,2F8.4,1PD13.5,2I5)
  124 FORMAT (1P8D10.2)
  125 FORMAT (I5,1PD14.5,2I5)
  193 FORMAT (3F12.5,I5)
  194 FORMAT (3D15.5)
  205 FORMAT (1X,'Program running time=',F12.4,2X,'minutes')
C
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C     Avoid warnings on assembly
      SGTMZ=SGTM
	AMUMSZ=AMUMS
	VLIGHTZ=VLIGHT
	SGMAZ=SGMA
	COEFZ=COEF
	YRSECZ=YRSEC
	SOLZ=SOL
	RADZ=RAD
	CFZ=CF
	TPIO2Z=TPIO2
	PIO2Z=PIO2
	GRAVZ=GRAV
	SOLMSZ=SOLMS
	BOLTZZ=BOLTZ

C**********************************************************************
      OPEN (UNIT=23,  FILE='E:\LGTSYN\ACDISK\ACPGCRG.ILS',STATUS='OLD')
	READ (23, 100)     (FL1(J),J=1,22)
	OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS = 'OLD')
	OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS = 'OLD')
	OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS = 'OLD')
	OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS = 'OLD')
	OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS = 'OLD')
	OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS = 'OLD')
	OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS = 'OLD')
	OPEN (UNIT=IN8,   FILE=FL1(8),  STATUS = 'OLD')
	OPEN (UNIT=IN9,   FILE=FL1(9),  STATUS = 'OLD')
	OPEN (UNIT=IN10,  FILE=FL1(10), STATUS = 'OLD')
	OPEN (UNIT=IN11,  FILE=FL1(11), STATUS = 'OLD')
	OPEN (UNIT=IN12,  FILE=FL1(12), STATUS = 'OLD')
	OPEN (UNIT=IN13,  FILE=FL1(13), STATUS = 'OLD')
	OPEN (UNIT=IN14,  FILE=FL1(14), STATUS = 'OLD')
	OPEN (UNIT=IN15,  FILE=FL1(15), STATUS = 'OLD')
	OPEN (UNIT=IN16,  FILE=FL1(16), STATUS = 'OLD')
	OPEN (UNIT=NOUT1, FILE=FL1(17), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT2, FILE=FL1(18), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT3, FILE=FL1(19), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT4, FILE=FL1(20), STATUS = 'REPLACE')
	OPEN (UNIT=NOUT5, FILE=FL1(21), STATUS = 'REPLACE')
	OPEN (UNIT=25, FILE='D:\OA2\ACPGCRG4.DAT',STATUS='REPLACE')
	OPEN (UNIT=26, FILE='D:\OA2\ACPGCRG5.DAT',STATUS='REPLACE')
      OPEN (UNIT=40,FILE='SCRATCH',STATUS='REPLACE')
	OPEN (UNIT=NOUT6, FILE=FL1(22), STATUS = 'REPLACE')
C**********************************************************************
C      CALL GETDAT(IYR,IMON,IDAY)
C	CALL GETTIM(IHR,IMIN,ISEC,I100TH)
	TM=3600*IHR+60*IMIN+ISEC
	AD=I100TH
	TM=(100.D0*TM+AD)/100.D0
	AD=IDAY
	DA=AD+TM/86400.D0
	CALL CVJD(DA,IMON,IYR,FVL1)
	WRITE (NOUT1,103)
	WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC
C
      LBLERR=0
	LABEL = 'ACPGCRG'
	NU=IN1
	CALL GTLBL(LABEL, LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read NSGAN, the number of segments, azimuthally, on a given 
C     coronal annulus.
	CALL IREAD (NSGAN)
C     Calculate the azimuthal width of each coronal segment.
	DPH=TWPI/(NSGAN-1)
	CALL DUMMY(5)
C     Input data from PGA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN10)
C     Input data from ACPGA.
C     This includes full data concerning the accretion disk itself.
C     It specifically	inputs the value of NRING.
      CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,AD,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN2)
C

C     Read NVDAN(), the number of vertical divisions on the coronal
C     segments.
C***********************
      DO K=1,NRING+1
		CALL IREAD (NVDAN(K))
	END DO
C***********************
	CALL DUMMY(3)
C     Read in height of each annulus above its bottom face.
C     The height is in solar radii.
C     These data are in the input file ACPGCRG.INF.
C***********************
      DO K=1,NRING+1
		CALL DREAD(CRHT(K),'F12.6')
	END DO
C**********************
	CALL DUMMY(1)
C     Read PARTDENS
      CALL DREAD(PARTDENS,'D9.3')
	CALL DUMMY(1)
C     Read COEFFD
      CALL DREAD(COEFFD,'F12.6')
	CALL DUMMY(2)
C     Read values of RDDENS
C***********************
      DO K=1,NRING+1
		CALL DREAD(RDDENS(K),'D9.3')
	END DO
C***********************
	CALL DUMMY(2)
C     Read values of VCOEFF
C***********************
      DO K=1,NRING+1
		CALL DREAD(VCOEFF(K),'F12.6')
	END DO
C***********************
C     Read value of TREF (in kK)
C***********************
      CALL DUMMY(1)
     	CALL DREAD (TREF,'F12.6')
C***********************
C     Read value of T1XP
C***********************
      CALL DUMMY(1)
	CALL DREAD (T1XP,'F12.6')
C***********************
C     Read value of T2XP
C***********************
      CALL DUMMY(1)
	CALL DREAD (T2XP,'F12.6')
C***********************	      
C
C     Read KEY1. If=1, apply scattering of light originating from
C     central star.
      CALL DUMMY (1)
	CALL IREAD (KEY1)
C
C     Read KEY2. If=1, apply scattering of light originating from
C     faces of accretion disk.
      CALL DUMMY (1)
	CALL IREAD (KEY2)
C
C     Read KEY3. If=1, apply extinction of light originating from
C     mass loser and passing through light scattering regions.
      CALL DUMMY (1)
	CALL IREAD (KEY3)
C
C     Read KEY4. If=1, apply scattering of light originating from
C     mass loser and incident on scattering regions.
      CALL DUMMY (1)
	CALL IREAD (KEY4)			     
C
C     Read KEY5. If=1, calculate bremsstrahlung
      CALL DUMMY (1)
	CALL IREAD (KEY5)
C
C     Read KEY5A. If=1, read wavelengths from external table of
C     wavelengths. If=0, use single wavelength from PGC.
      CALL DUMMY (1)
	CALL IREAD (KEY5A)
C     Write KEY5 and KEY5A to NOUT4, for use by ACPGDRG.
      WRITE (NOUT4, 106) KEY5, KEY5A
C
C     Read NHOR. This is the number of horizontal division points
C     on the screen behind the accretion disk, used to divide the
C     projected area into segments for calculation of bremsstrahlung.
      CALL DUMMY (1)
	CALL IREAD (NHOR)
C
C     Read NVER.  This is the number of vertical division points
C     on the screen behind the accretion disk, used to divide the
C     projected area into segments for calculation of bremsstrahlung.
      CALL DUMMY (1)
	CALL IREAD (NVER)
C
C     Number of steps in Z direction for solution of equation of transfer.
      CALL DUMMY (3)
	CALL IREAD (NSTEP)
	IF (NSTEP.GT.10000) CALL PMDSTOP
C 
C     Y direction plane of sky index for test line of sight.
      CALL DUMMY (1)
	CALL IREAD (JINDX)
C
C     X direction plane of sky index for test line of sight.
C     Note: If the test is intended for the lower coronal region,
C     the input value is in the range 100->100+basic index value.
      CALL DUMMY (1)
	CALL IREAD (KINDX)
C
C     Read control KTST. If KTST=1, do calculations and write	test
C     output for the specified line of sight only. If KTST=0, do
C     not write test output but range over entire object.
      CALL DUMMY (1)
	CALL IREAD (KTST)
C
C     Read ACPGA.INF to determine rim height in solar radii
      LBLERR=0
	LABEL = ' *ACPGA'
	NU=IN11
	CALL GTLBL(LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL DUMMY(12)
	CALL DREAD (HVP,'F12.6')
C
C***********************
      DO K=1,NRING+1
C     Note that we read in NRING+1 values of CRHT(K). This is
C     because the "innermost annulus" is a point on the Z axis.
C     Convert heights of coronal annuli to units of component 
C     separation.
C     The value of the rim height read in from ACPGA.INF permits
C     the conversion to be calculated. CRHT(K) are in solar radii.
C     HVP is the outer rim height in solar radii. HV is the value
C     of one solar radius, in units of the component separation.
C     HV is read in from ACRD1.
		CRHT(K)=CRHT(K)/HVP*HV
	END DO
C***********************
C     Determine scale factor, component separation in cm.
      SEPCM=HVP/HV*SNRAD
	SEPCM3=SEPCM**3
C     Set coordinates of points defining corona
C     Note that there may be a different number of coronal azimuthal
C     segments than the number of azimuthal accretion disk segments.
C     There are the same number of annuli in the corona as annuli
C     in the accretion disk. That number is NRING.
C
C     Extract the theta coordinate of the top of the accretion disk 
C     rim, in THR(1).
      VTH=THR(1)
	SNTH=DSIN(VTH)
	COSTH=DCOS(VTH)
C     Calculate the ratio of the vertical height of an accretion disk
C     annulus to its inner radius. This number will be used to set the
C     vertical position of the lower face of a given coronal annulus.
	COT=COSTH/SNTH
C     Set the vertical position of the "innermost annulus" of the corona
C     equal to the radius of the central star.
C     Beyond the innermost annulus, the voxels will
C     start at the top surface of the accretion disk at the radius of
C     that annulus.
C     The radius of the central star is RB.
C     The array indices are:
C		First index,   annulus number (i.e., radial coordinate)
C		Second index,  azimuthal segment number
C		Third index,   vertical segment number
C		Fourth index,  upper or lower face (1=upper face, 2=lower)
C     Note that these coordinates relate to the inner and lower 
C     boundary, and the smaller of the bounding azimuths.
C***********************
	XAN(1,1,1,1)=0.D0
	YAN(1,1,1,1)=0.D0
	ZAN(1,1,1,1)=RB
C     Set corresponding indices for lower face, "innermost annulus".
	XAN(1,1,1,2)=0.D0
	YAN(1,1,1,2)=0.D0
	ZAN(1,1,1,2)=-RB
C     Set all azimuth indices.
	DO L=1,NSGAN
		XAN(1,L,1,1)=XAN(1,1,1,1)
		YAN(1,L,1,1)=YAN(1,1,1,1)
		ZAN(1,L,1,1)=ZAN(1,1,1,1)
		XAN(1,L,1,2)=XAN(1,1,1,2)
		YAN(1,L,1,2)=YAN(1,1,1,2)
		ZAN(1,L,1,2)=ZAN(1,1,1,2)
	END DO
C     Calculate the heights of the voxels for the "innermost annulus".
	VDAN=NVDAN(1)-1
	DH=(CRHT(1)-RB)/VDAN
C     Set the vertical boundary of each vertical segment for the
C     "innermost annulus". This sets the innermost face of each voxel.
      DO LH=2,NVDAN(1)
		AZZ=(LH-1)*DH+ZAN(1,1,1,1)
		IF (AZZ.LE.CRHT(1)) THEN
C     Set upper face.
			ZAN(1,1,LH,1)=AZZ
C     Set lower face.
			ZAN(1,1,LH,2)=-AZZ
		ELSE
			ZAN(1,1,LH,1)=CRHT(1)
			ZAN(1,1,LH,2)=-ZAN(1,1,LH,1)
		END IF
		DO L=1,NSGAN
C     Note that the following X and Y coods. place the point on the Z 
C     axis.
			XAN(1,L,LH,1)=0.D0
			YAN(1,L,LH,1)=0.D0
			ZAN(1,L,LH,1)=ZAN(1,1,LH,1)
C     Set corresponding indices for lower face, "innermost annulus".
			XAN(1,L,LH,2)=0.D0
			YAN(1,L,LH,2)=0.D0
			ZAN(1,L,LH,2)=ZAN(1,1,LH,2)
		END DO
	END DO
C     Now set the outer face of the voxels on the innermost annulus.
      DO L=1,NSGAN
		VPHV=(L-1)*DPH
		SNPH=DSIN(VPHV)
		COSPH=DCOS(VPHV)
		XAN(2,L,1,1)=RB*COSPH/2.D0
		YAN(2,L,1,1)=RB*SNPH/2.D0
		DO LH=2,NVDAN(1)
			AZZ=(LH-1)*DH+ZAN(1,1,1,1)
			IF (AZZ.LE.CRHT(1)) THEN
				ZAN(2,L,LH,1)=AZZ
				ZAN(2,L,LH,2)=-AZZ
			ELSE
				ZAN(2,L,LH,1)=CRHT(1)
				ZAN(2,L,LH,2)=-ZAN(2,L,LH,1)
			END IF
		END DO
	END DO
C***********************
C     Now calculate coordinates defining initial point for all voxels.
C     There are NRING+1 coronal annuli. This counts NRING annuli to the
C     inner edge of the toroidal rim and an additional annulus extending
C     to the orbital plane radius of the toroidal rim. This final 
C     annulus has the same height as the penultimate annulus.	Since we
C     start with points on the polar axis of the central star, there are
C     NRING+2 sets of radial index values.
C
C     The above description is true IF the rim is toroidal. The parameter
C     NTYPE determines the shape of the rim. If NTYPE=2 the rim is
C     toroidal. If NTYPE=1, it is cylindrical.
C     Note, in the following program text, that we go to a K limit K+1
C     beyond NRINGLMT. This is in preparation for calculation of the
C     locations of the centers of voxels, where we take the geometric 
C     mean of the eight corners of a given voxel. By going to K+1 we
C     permit calculation of NRING voxel centers in the radial direction
C
C***********************
	IF (NTYPE.EQ.2) NRINGLMT=NRING+1
	IF (NTYPE.EQ.1) NRINGLMT=NRING
      DO K=1,NRINGLMT
C     Successive annuli step "upward" on their lower faces to match the
C     flaring of the accretion disk.
	IF (K.LE.NRING) THEN
		ZAN(K+1,1,1,1)=RGRD(K)*COT
	ELSE
		ZAN(K+1,1,1,1)=RGRD(K-1)*COT
	END IF
C     Set all azimuth indices, bottom faces
	DO L=1,NSGAN
		ZAN(K+1,L,1,1)=ZAN(K+1,1,1,1)
		ZAN(K+1,L,1,2)=-ZAN(K+1,1,1,1)
	END DO
C     Calculate coordinates for the vertical segments.
	IF (K.LE.NRING) THEN
		LMT=NVDAN(K+1)
		DH=(CRHT(K+1)-ZAN(K+1,1,1,1))/(LMT-1)
	ELSE
		LMT=NVDAN(K)
		DH=(CRHT(K)-ZAN(K+1,1,1,1))/(LMT-1)
	END IF
	IF (DH.LT.0.D0) DH=0.D0
		DO LH=2,LMT
			XAN(K+1,1,LH,1)=XAN(K+1,1,1,1)
			YAN(K+1,1,LH,1)=YAN(K+1,1,1,1)
			AZZ=(LH-1)*DH+ZAN(K+1,1,1,1)
			IF (K.LE.NRING) THEN
				IF (DABS(AZZ).LE.DABS(CRHT(K+1))) THEN
					ZAN(K+1,1,LH,1)=AZZ
				ELSE
					ZAN(K+1,1,LH,1)=CRHT(K+1)
				END IF
			ELSE
				IF (DABS(AZZ).LE.DABS(CRHT(K))) THEN
					ZAN(K+1,1,LH,1)=AZZ
				ELSE
					ZAN(K+1,1,LH,1)=CRHT(K)
				END IF
			END IF
C     Set corresponding coordinates for the lower face.
			XAN(K+1,1,LH,2)=XAN(K+1,1,LH,1)
			YAN(K+1,1,LH,2)=YAN(K+1,1,LH,1)
			ZAN(K+1,1,LH,2)=-ZAN(K+1,1,LH,1)
C     L labels the azimuthal segments
			DO L=1,NSGAN
				VPHV=(L-1)*DPH
				SNPH=DSIN(VPHV)
				COSPH=DCOS(VPHV)
C     Note that there are NRING+2 values for the first index.
				IF (K.LE.NRING) THEN
					IF (K.EQ.1) THEN
						XAN(K+1,L,LH,1)=RB*COSPH/2.D0
						YAN(K+1,L,LH,1)=RB*SNPH/2.D0
					ELSE
						XAN(K+1,L,LH,1)=RGRD(K)*COSPH
						YAN(K+1,L,LH,1)=RGRD(K)*SNPH
					END IF
				ELSE
					XAN(K+1,L,LH,1)=RA*COSPH
					YAN(K+1,L,LH,1)=RA*SNPH
				END IF
				ZAN(K+1,L,LH,1)=ZAN(K+1,1,LH,1)
C     Set lower face.
				XAN(K+1,L,LH,2)=XAN(K+1,L,LH,1)
				YAN(K+1,L,LH,2)=YAN(K+1,L,LH,1)
				ZAN(K+1,L,LH,2)=-ZAN(K+1,L,LH,1)
			END DO
		END DO
	END DO
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				IF (K.EQ.1) THEN
					CX(K,L,LH,1)=(XAN(K+1,L,LH,1)+XAN(K+1,L,LH+1,1)+
     $							XAN(K+1,L+1,LH,1)+XAN(K+1,L+1,LH+1,1))
     $							/4.D0
					CY(K,L,LH,1)=(YAN(K+1,L,LH,1)+YAN(K+1,L,LH+1,1)+
     $							YAN(K+1,L+1,LH,1)+YAN(K+1,L+1,LH+1,1))
     $							/4.D0
					CZ(K,L,LH,1)=(ZAN(K+1,L,LH,1)+ZAN(K+1,L,LH+1,1)+
     $							ZAN(K+1,L+1,LH,1)+ZAN(K+1,L+1,LH+1,1))
     $							/4.D0
					CX(K,L,LH,2)=(XAN(K+1,L,LH,2)+XAN(K+1,L,LH+1,2)+
     $							XAN(K+1,L+1,LH,2)+XAN(K+1,L+1,LH+1,2))
     $							/4.D0
					CY(K,L,LH,2)=(YAN(K+1,L,LH,2)+YAN(K+1,L,LH+1,2)+
     $							YAN(K+1,L+1,LH,2)+YAN(K+1,L+1,LH+1,2))
     $							/4.D0
					CZ(K,L,LH,2)=(ZAN(K+1,L,LH,2)+ZAN(K+1,L,LH+1,2)+
     $							ZAN(K+1,L+1,LH,2)+ZAN(K+1,L+1,LH+1,2))
     $							/4.D0
				ELSE														
				CX(K,L,LH,1)=(XAN(K,L,LH,1)+XAN(K,L,LH+1,1)+
     $							XAN(K,L+1,LH,1)+XAN(K,L+1,LH+1,1)+
     $							XAN(K+1,L,LH,1)+XAN(K+1,L,LH+1,1)+
     $							XAN(K+1,L+1,LH,1)+XAN(K+1,L+1,LH+1,1))
     $	                        /8.D0				
				CY(K,L,LH,1)=(YAN(K,L,LH,1)+YAN(K,L,LH+1,1)+
     $							YAN(K,L+1,LH,1)+YAN(K,L+1,LH+1,1)+
     $							YAN(K+1,L,LH,1)+YAN(K+1,L,LH+1,1)+
     $							YAN(K+1,L+1,LH,1)+YAN(K+1,L+1,LH+1,1))
     $                        	/8.D0
				CZ(K,L,LH,1)=(ZAN(K,L,LH,1)+ZAN(K,L,LH+1,1)+
     $							ZAN(K,L+1,LH,1)+ZAN(K,L+1,LH+1,1)+
     $							ZAN(K+1,L,LH,1)+ZAN(K+1,L,LH+1,1)+
     $							ZAN(K+1,L+1,LH,1)+ZAN(K+1,L+1,LH+1,1))
     $                            /8.D0
				CX(K,L,LH,2)=(XAN(K,L,LH,2)+XAN(K,L,LH+1,2)+
     $							XAN(K,L+1,LH,2)+XAN(K,L+1,LH+1,2)+
     $							XAN(K+1,L,LH,2)+XAN(K+1,L,LH+1,2)+
     $							XAN(K+1,L+1,LH,2)+XAN(K+1,L+1,LH+1,2))
     $                            /8.D0				
				CY(K,L,LH,2)=(YAN(K,L,LH,2)+YAN(K,L,LH+1,2)+
     $							YAN(K,L+1,LH,2)+YAN(K,L+1,LH+1,2)+
     $							YAN(K+1,L,LH,2)+YAN(K+1,L,LH+1,2)+
     $							YAN(K+1,L+1,LH,2)+YAN(K+1,L+1,LH+1,2))
     $                        	/8.D0
				CZ(K,L,LH,2)=(ZAN(K,L,LH,2)+ZAN(K,L,LH+1,2)+
     $							ZAN(K,L+1,LH,2)+ZAN(K,L+1,LH+1,2)+
     $							ZAN(K+1,L,LH,2)+ZAN(K+1,L,LH+1,2)+
     $							ZAN(K+1,L+1,LH,2)+ZAN(K+1,L+1,LH+1,2))
     $                            /8.D0
				END IF
C     ****************************************
C     Note that, at this point, the coods. CX(), CY(), CZ() are relative
C     to the center of the accretion disk.
C     ****************************************
			END DO
     		END DO
	END DO
C***********************
C
C     Calculate distances of voxel central points from origin.
C     The origin is the center of the accretion disk.
C
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				DS(K,L,LH,1)=DSQRT(CX(K,L,LH,1)**2+CY(K,L,LH,1)**2+
     $				CZ(K,L,LH,1)**2)
				DS(K,L,LH,2)=DSQRT(CX(K,L,LH,2)**2+CY(K,L,LH,2)**2+
     $				CZ(K,L,LH,2)**2)
			END DO
		END DO
	END DO
C***********************
C
C     Calculate voxel volumes, cgs units.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)
			ELSE
				LMT=NVDAN(K-1)
			END IF
			DO LH=2,LMT
				IF (K.LE.NRING) THEN
				IF (K.EQ.1) THEN
					AVAREA=DPH*RGRD(K)*(ZAN(K,L,LH,1)-ZAN(K,L,LH-1,1))
					VOXEL(K,L,LH-1,1)=RGRD(K)*AVAREA*SEPCM3/2.D0
				ELSE
					AREA1=DPH*RGRD(K-1)*(ZAN(K-1,L,LH,1)-
     $						ZAN(K-1,L,LH-1,1))
					AREA2=DPH*RGRD(K)*(ZAN(K,L,LH,1)-ZAN(K,L,LH-1,1))
					AVAREA=(AREA1+AREA2)/2.D0
					VOXEL(K,L,LH-1,1)=(RGRD(K)-RGRD(K-1))*AVAREA*
     $				SEPCM3
				END IF
				ELSE
					AREA1=DPH*RGRD(K-1)*(ZAN(K-1,L,LH,1)-
     $						ZAN(K-1,L,LH-1,1))
					AREA2=DPH*RA*(ZAN(K,L,LH,1)-ZAN(K,L,LH-1,1))
					AVAREA=(AREA1+AREA2)/2.D0
					VOXEL(K,L,LH-1,1)=(RA-RGRD(K-1))*AVAREA*SEPCM3
				END IF
				VOXEL(K,L,LH-1,2)=VOXEL(K,L,LH-1,1)								
			END DO
		END DO
	END DO
C***********************
C
C     Input data from PGC
      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),
     $UA(3-I),IN8)     
C     Set particle density in corona.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				TERMA=PARTDENS*((DS(K,L,LH,1)/DS(1,1,1,1))**(-COEFFD))
				TERMB=RDDENS(K)*((CZ(K,L,LH,1)/CZ(1,1,1,1))
     $				**(-VCOEFF(K)))
				DPART(K,L,LH,1)=TERMA+TERMB
				DPART(K,L,LH,2)=DPART(K,L,LH,1)
			END DO
		END DO
	END DO
C***********************
C     Set temperature in corona
C
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				TCOR(K,L,LH,1)=TREF*1000.D0*(DS(K,L,LH,1)
     $							**T1XP)*(CZ(K,L,LH,1)**T2XP)
				TCOR(K,L,LH,2)=TCOR(K,L,LH,1)
			END DO
		END DO
	END DO
C***********************
C     Adopt electron number density equal to the particle
C     number density, as an ititial approximation.
C***********************
	DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				T=TCOR(K,L,LH,2)
				DENSEL(K,L,LH,1)=DPART(K,L,LH,1)
				DENSEL(K,L,LH,2)=DPART(K,L,LH,2)
				HN2=8.D0*2.0706D-16/T/DSQRT(T)*
     $				DEXP(39451.D0/T)*DENSEL(K,L,LH,2)**2
				DENSN2(K,L,LH,2)=HN2
				DENSN2(K,L,LH,1)=HN2
			END DO
		END DO
	END DO
C***********************
C
C     Calculate number of scattering electrons for each voxel.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				VNMEL(K,L,LH,1)=VOXEL(K,L,LH,1)*DENSEL(K,L,LH,1)
				VNMEL(K,L,LH,2)=VOXEL(K,L,LH,2)*DENSEL(K,L,LH,2)
			END DO
		END DO
	END DO
C***********************
C
C     Calculate direction cosines of voxel central points
C     from the origin.
C     Note that the following direction cosines would need to be 
C     replaced by others whose terminal points range over the surface
C     of the central star rather than be referenced to the origin.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				VLC(K,L,LH,1)=CX(K,L,LH,1)/DS(K,L,LH,1)
				VMC(K,L,LH,1)=CY(K,L,LH,1)/DS(K,L,LH,1)
				VNC(K,L,LH,1)=CZ(K,L,LH,1)/DS(K,L,LH,1)
				VLC(K,L,LH,2)=CX(K,L,LH,2)/DS(K,L,LH,2)
				VMC(K,L,LH,2)=CY(K,L,LH,2)/DS(K,L,LH,2)
				VNC(K,L,LH,2)=CZ(K,L,LH,2)/DS(K,L,LH,2)
			END DO
		END DO
	END DO
C***********************
C
C     Read number of orbital longitudes produced by PGB.
	READ (IN12, 106) NPSI
C
C     Input intensity data.
	CALL RDWT6(I,IWL,NT,IN9)
C***********************
C     Input data for accretion disk face.
      CALL ACRD2(NWL,NSG,NRM,NRING,IRCT,WLV,
     $TMPRF,TMPRFA,SMV,TMPRR,RMIFLX,IN13)     
C
C     ********************************************
C     Loop for successive wavelengths
C     ********************************************
C
      DO IWL=1,NWL
C
C     Principal loop for successive values of orbital longitude
C     ***********************************************************
C
      DO 60 KS=1,NPSI
C     ***********************************************************
C     Initialize PLTB storage areas
	DO KK=1,40401
		VLX(KK)=0.D0
		VLY(KK)=0.D0
		VLZ(KK)=0.D0
		KIDX(KK)=0
	END DO
C     Initialize sums for KEY4 program segment.
      SMA4=0.D0
	SMB4=0.D0
	SMC4=0.D0
	SMD4=0.D0
C
C     Input data from PLTB
	READ (IN15,106) KLM
      DO KK=1,KLM
		READ (IN15, 193) VLX(KK), VLY(KK), VLZ(KK), KIDX(KK)
	END DO
C
  236 KLG=KLM
C     Read photospheric grid data for eclipsing star, to determine
C     light scattering of light from eclipsing star scattered from
C     scattering region, under control of KEY4.
	READ (IN14,106) KLM
      DO KK=1,KLM
		READ (IN14, 193) XECPJ(KK),YECPJ(KK),ZECPJ(KK),KECL
	END DO
C
      CALL RDWT8(KSPP,IEC,ISV,IRC,IFC,NKP,IRCS,IFCS,NKPS,
     $AGLMT1,AGLMT2,XG,YG,XCM,YCM,ISEQ,IN7)
C
C     Read data from PGB needed for projection onto plane of sky.
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN5)
	MTH=2*NTH-1
      CSI=-DCOS(VI)
	SNI=DSIN(VI)
	CM=TWPI-PSI
	IF (CM.LT.0.D0) CM=CM+TWPI

C     Calculate projection matrix	TARY
C     Note that this calculation precedes separate program segments
C     controlled by separate control keys, KEY1-KEY5.
C
	CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
C
C     Input projection data concerning accretion disk	from ACPGB.
      CALL ACRD3(NSG,NRM,NRING,CSGR,CSGF,PXR,PYR,PZR,PXF,PYF,PZF,XHZ,
     $YHZ,PRFH,PRSH,PRRH,XRMC,YRMC,RBP,STX,STY,STZ,STVS,RXHZL,RYHZL,
     $RXHZG,RYHZG,PRVFG,PRVFL,XUL,XLL,YUL,YLL,RVL,ANG,RD,AGL,VXP,VYP,
     $IFC,IRC,IFCS,IRCS,IGAIN,IP,IEC,IFLAG,MTH,IN3)
C     Read additional data from ACPGB
	READ (IN4,108) DX,DY,DZ,DDX,DDY,DDZ,XG,YG,ZG,XLSL
	READ (IN4,106) ISV,ISEQ
C     The subsequent sections of the program require the coronal grid 
C     coordinates to be relative to the center of the accretion disk.
C     The coods of XUL, XLL, YUL, YLL from ACRD3 are in center of
C     mass coordinates. Transform to coordinates relative to the center
C     of the accretion disk.
	IF (ISTAR.EQ.ISV) THEN
			XLL=XLL-DX
			XUL=XUL-DX
			YLL=YLL+DY
			YUL=YUL+DY
	ELSE
			XLL=XLL-DDX
			XUL=XUL-DDX
			YLL=YLL+DDY
			YUL=YUL+DDY
	END IF
C
C     Initialize visibility keys
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				VSBKY(K,L,LH,1)=0.D0
				VSBKY(K,L,LH,2)=0.D0
			END DO
		END DO
	END DO
C***********************
C
C     Project grid of voxel centers onto plane of sky.
C     Note that the points are located relative to the projected
C     center of the gainer.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				CALL PROJ(TARY,CX(K,L,LH,1),CY(K,L,LH,1),
     $				CZ(K,L,LH,1),CXP(K,L,LH,1),CYP(K,L,LH,1),
     $				CZP(K,L,LH,1))
				CALL PROJ(TARY,CX(K,L,LH,2),CY(K,L,LH,2),
     $				CZ(K,L,LH,2),CXP(K,L,LH,2),CYP(K,L,LH,2),
     $				CZP(K,L,LH,2))
C     Set correct sense for Z axis.
					CZP(K,L,LH,1)=-CZP(K,L,LH,1)
					CZP(K,L,LH,2)=-CZP(K,L,LH,2)
			END DO
		END DO
	END DO
C***********************
C
C     Project grid of voxel corners onto plane of sky. Note that, in 
C     this case, LMT is one greater than for voxel centers.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)
			ELSE
				LMT=NVDAN(K-1)
			END IF
			DO LH=1,LMT
				CALL PROJ(TARY,XAN(K,L,LH,1),YAN(K,L,LH,1),
     $				ZAN(K,L,LH,1),XANP(K,L,LH,1),YANP(K,L,LH,1),
     $				ZANP(K,L,LH,1))
				CALL PROJ(TARY,XAN(K,L,LH,2),YAN(K,L,LH,2),
     $				ZAN(K,L,LH,2),XANP(K,L,LH,2),YANP(K,L,LH,2),
     $				ZANP(K,L,LH,2))
C     Set correct sense for Z axis
					ZANP(K,L,LH,1)=-ZANP(K,L,LH,1)
					ZANP(K,L,LH,2)=-ZANP(K,L,LH,2)
			END DO
		END DO
	END DO
C***********************
C
C     Note that XANP(), YANP(), ZANP(), at this point, are relative to
C     the center of the accretion disk.
C
C***********************
C
C     Calculate voxel projected areas
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			LMT=NVDAN(K)-1
			DO LH=1,LMT
				AB=DSQRT((YANP(K+1,L+1,LH,1)-YANP(K+1,L,LH,1))**2+
     $				(XANP(K+1,L+1,LH,1)-XANP(K+1,L,LH,1))**2)
				BF=DSQRT((YANP(K+1,L,LH,1)-YANP(K,L,LH,1))**2+
     $				(XANP(K+1,L,LH,1)-XANP(K,L,LH,1))**2)
				BC=DSQRT((YANP(K+1,L,LH+1,1)-YANP(K+1,L,LH,1))**2+
     $				(XANP(K+1,L,LH+1,1)-XANP(K+1,L,LH,1))**2)
				PJAR(K,L,LH,1)=(AB+BF)*BC*SEPCM**2
				AB=DSQRT((YANP(K+1,L+1,LH,2)-YANP(K+1,L,LH,2))**2+
     $				(XANP(K+1,L+1,LH,2)-XANP(K+1,L,LH,2))**2)
				BF=DSQRT((YANP(K+1,L,LH,2)-YANP(K,L,LH,2))**2+
     $				(XANP(K+1,L,LH,2)-XANP(K,L,LH,2))**2)
				BC=DSQRT((YANP(K+1,L,LH+1,2)-YANP(K+1,L,LH,2))**2+
     $				(XANP(K+1,L,LH+1,2)-XANP(K+1,L,LH,2))**2)
				PJAR(K,L,LH,2)=(AB+BF)*BC*SEPCM**2
			END DO
		END DO
	END DO
C***********************
C
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Start of program section for calculating light scattering due to
C     light originating on central star.
C     Application of this program section is under control of KEY1
C     If KEY1=1, apply this section. Otherwise omit it.
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C    ******************
C    ******************
      IF (KEY1.EQ.1) THEN
C    ******************
C    ******************
C
C     At this point, the coronal grid points are relative to the
C     center of the accretion disk.
C     Calculate cosine of scattering angles for all voxel central
C     points, for rays originating at the origin of coods.
C     The direction cosines VLO, VMO, VNO are the direction cosines
C     of the line of sight to the observer, relative to the cood.
C     system with X axis directed from the central star toward the
C     center of the companion, the Y axis lies in the orbital plane
C     90 degrees in advance of the X axis, and the Z axis forms a
C     right hand system with X and Y.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				CSCTR(K,L,LH,1)=VLC(K,L,LH,1)*VL0+VMC(K,L,LH,1)*VM0+
     $				VNC(K,L,LH,1)*VN0
				CSCTR(K,L,LH,2)=VLC(K,L,LH,2)*VL0+VMC(K,L,LH,2)*VM0+
     $				VNC(K,L,LH,2)*VN0
			END DO
		END DO
	END DO
C***********************
C
C
C     Calculate geometric factors for light scattering, for light
C     originating at central star.
C     First determine radius of central star.
      RDA=VR(ISTAR,1)
C     Calculate radiation	properties of central star.
C     Intensity data have already been calculated by PGC and have been
C     input via unit IN9, SRT RDWT6. The intensity data are in array 
C     BDI. Approximate, using the polar intensity, at BDI(ISTAR,1). The
C     corresponding limb darkening coefficients are in U1(ISTAR,1) and
C     U2(ISTAR,1). Then the flux from the central star at the wavelength
C     in question is 
C     pi*BDI(ISTAR,1)*(6-2*U1(ISTAR,1)-3*U2(ISTAR,1))/6*pi*RDA^2*SEPCM^2
C     The radiation intercepted by 1cm^2 at the voxel center is the
C     above quantity divided by 2*pi*DS(K,L,LH,1)^2*SEPCM^2, in the
C     approximation in which the central star is a point source.
C     Calculate flux from central star.
C     Note in particular that star I is the star eclipsed at orbital
C     phase 0.0. The star with the accretion disk is star ISTAR.
      FLUX=PI*BDI(ISTAR,1)*(6.D0-2.D0*U1(ISTAR,1)-3.D0*U2(ISTAR,1))/
     $6.D0*PI*RDA**2*SEPCM**2
C     Choose an increment for distance along pencil.
	DLS=(RGRD(2)-RGRD(1))/3.D0
	DLSSV=DLS
C
C***********************
	DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
C     Initialize VTAU
			VTAU=0.D0
C     Determine test distance for voxel under consideration.
C     This distance is the AVG. distance from the voxel center to 
C     the remote two corners on the lower voxel face.
C     It is used to determine whether the pencil of radiation under
C     study has reached the voxel in question.
C
				TSDS1=DSQRT((XAN(K+1,L,LH,1)-CX(K,L,LH,1))**2
     $					+(YAN(K+1,L,LH,1)-CY(K,L,LH,1))**2
     $					*(ZAN(K+1,L,LH,1)-CZ(K,L,LH,1))**2)
				TSDS2=DSQRT((XAN(K+1,L+1,LH,1)-CX(K,L,LH,1))**2
     $					+(YAN(K+1,L+1,LH,1)-CY(K,L,LH,1))**2
     $					*(ZAN(K+1,L+1,LH,1)-CZ(K,L,LH,1))**2)
				TSDS=(TSDS1+TSDS2)/2.D0
				IF (TSDS.LT.1.D-3) TSDS=1.D-3
C     Calculate coods. of starting point.
C     Direction cosine for orbital plane projection of point on pencil
C     directed from cood. center toward voxel in question. 					
				VRCVAL=DSQRT(VLC(K,L,LH,1)**2+VMC(K,L,LH,1)**2)
C     Direction cosines for pencil directed toward voxel in question.
				VLCVAL=VLC(K,L,LH,1)
				VMCVAL=VMC(K,L,LH,1)
				VNCVAL=VNC(K,L,LH,1)
C     Z value of starting point.
				VHLOC=RDA*VNCVAL
C     X cood. of starting point.
				VXLOC=RDA*VLCVAL
C     Y cood. of starting point.
				VYLOC=RDA*VMCVAL
C     Radial cood. of starting point.
				VRLOC=RDA*VRCVAL
C     Distance of starting point from origin.
				VSLOC=RDA
C     Initialize pointers for radius, azimuth, and height
				KTS=K
				LTS=L
				LHTS=LH
C     Distance of voxel center from origin.
				DSVAL=DS(K,L,LH,1)
C     Z value of voxel center.
				ZVAL=CZ(K,L,LH,1)
C     Radial distance of voxel center from origin.
				RVAL=DSQRT(CX(K,L,LH,1)**2+CY(K,L,LH,1)**2)
C     Determine whether current end of pencil is within test distance
C     of center of voxel.
C     Note that VXLOC, VYLOC are relative to center of accretion disk.
				KLOOP=0
   32				KLOOP=KLOOP+1
   				DELTAS=DSVAL-VSLOC
   				IF (DELTAS.LT.0.D0) THEN
					IF (KLOOP.EQ.1) THEN
						VTAU=1.D6
						GO TO 26
					END IF 
					VSLOC=VSLOC-DLS
					VRLOC=VRLOC-DLS*VRCVAL
					VXLOC=VXLOC-DLS*VLCVAL
					VYLOC=VYLOC-DLS*VMCVAL
					VHLOC=VHLOC-DLS*VNCVAL
					DLTAU=SGTM*DENSEL(KTS,LTS,LHTS,1)*SEPCM*DLS
					VTAU=VTAU-DLTAU
					DLS=DLS/2.D0
					IF (KLOOP.LT.100) GO TO 32
					CALL PMDSTOP
				END IF			
				TEST=DELTAS/TSDS
				IF (TEST.LT.1.0D0) THEN
C     Current end of pencil is within test distance of center of voxel.
C     Calculate distance from current end of pencil to center of voxel
C     and increment VTAU appropriately.
					DLTAU=SGTM*DENSEL(K,L,LH,1)*SEPCM*DELTAS
					VTAU=VTAU+DLTAU
C     We have reached the voxel center. Calculate scattered light for
C     that voxel.
					GO TO 26
				END IF     ! IF (TEST.LT.1.D0) THEN
C     Test case in which TSDS is very small
				IF ((TSDS.LT.1.D-3).AND.(TEST.LT.2.D0)) THEN
					DLTAU=SGTM*DENSEL(K,L,LH,1)*SEPCM*DELTAS
					VTAU=VTAU+DLTAU
					GO TO 26
				END IF
C     If we arrive here, the current end of the pencil has not reached
C     the voxel in question. Must step through intervening voxels.
C     Increment location of end of pencil and increment VTAU.
				VSLOC=VSLOC+DLS
				VRLOC=VRLOC+DLS*VRCVAL
				VXLOC=VXLOC+DLS*VLCVAL
				VYLOC=VYLOC+DLS*VMCVAL
				VHLOC=VHLOC+DLS*VNCVAL
				DLTAU=SGTM*DENSEL(KTS,LTS,LHTS,1)*SEPCM*DLS
				VTAU=VTAU+DLTAU
C     Test for exit
				IF (DABS(DSVAL-VSLOC).LT.TSDS) GO TO 26
C     Locate radial index
				IF (VRLOC.LT.RGRD(1)) THEN
					KK=1
					GO TO 27
				END IF
				DO KK=2,NRING
					IF (VRLOC.GE.RGRD(KK-1).AND.(VRLOC.LT.RGRD(KK))) 
     $				GO TO 27
				END DO
				KK=NRING
   27				KTS=KK
C     Locate height index
				IF (VHLOC.LT.CZ(KTS,1,1,1)) THEN
					LLH=1
					GO TO 28
				END IF
				DO LLH=1,LMT
					IF (VHLOC.GE.CZ(KTS,1,LLH,1).AND.
     $				(VHLOC.LT.CZ(KTS,1,LLH+1,1))) GO TO 28
				END DO
				LLH=LMT
   28             LHTS=LLH
C     Note that it is unnecessary to determine the azimuthal index since
C     the electron density is azimuthally constant within a given
C     annulus.
C     Test whether we have reached the voxel in question.
				GO TO 32
C     
C     
 4000 FORMAT (10F8.5)
 4001 FORMAT (' ')
 4002 FORMAT (10I5)
 4003 FORMAT (8E10.3)
C     We have reached the voxel center. Now calculate the scattered 
C     light. FLUX is the monochromatic radiation emitted by the central
C     star, considered as a point source. DIV is the surface area of a
C     hemisphere whose radius equals the distance of the voxel center from
C     the origin (the location of the central star point source). Note that
C     the radiation from the central star, with a cross-section equal to 
C     the polar radius of the central star, is into a hemisphere.
   26				DIV=2.D0*PI*DS(K,L,LH,1)**2*SEPCM**2
C     TERM1 is the monochromatic energy per unit cross-sectional area
C     reaching the voxel center.
				IF (VTAU.LT.697.69D0) THEN
					EXPTRM=DEXP(VTAU)
				ELSE
					EXPTRM=1.D303
				END IF
				TERM1=FLUX/DIV/EXPTRM
C     Calculate phase function
				PHFN=0.75D0*(1.D0+CSCTR(K,L,LH,1)**2)
C     The number of scattering electrons in the voxel is in 
C     VNMEL(K,L,LH,1). Consider all electrons as located in unit vol.
C     with unit cross-section, located at the voxel center.
				TERM2=SGTM*PHFN*VNMEL(K,L,LH,1)
C     Calculate light scattered toward observer. Neglect the radiation
C     scattering OUT of the pencil toward the observer after the
C     first scattering. Some radiation will be scattered into the line
C     of sight and will, in part, compensate for this omission. Then
C     VFLX() is the monochromatic radiation scattered toward the
C     observer, per unit solid angle, from the voxel center designated
C     by the indices K,L,LH,1.
				VFLX(K,L,LH,1)=TERM1*TERM2
C     The geometry is symmetric about the orbital plane.
				PHFN=0.75D0*(1.D0+CSCTR(K,L,LH,2)**2)
				TERM2=SGTM*PHFN*VNMEL(K,L,LH,2)
				VFLX(K,L,LH,2)=TERM1*TERM2/TWPI/2.D0
C     Reset DLS to default value
				DLS=DLSSV
			END DO		  !DO LH=1,LMT
C     Completes other scattering region, for LH values.
		END DO		!DO L=1,NSGAN-1
	END DO	 !DO K=1,NRINGLMT
C***********************
      END IF	 !Test for KEY1
C     ******************
C     ******************
C     !!!!!!!!!!!!!!!!!!!!!!!!
C     End of program section under control of KEY1
C     Note that this program section has not modified the coods. of
C     any of the tabulated quantities. At this stage, all stored
C     coods. are relative to the center of the accretion disk.
C     !!!!!!!!!!!!!!!!!!!!!!!!
C     Start of program section under control of KEY2
C     If KEY2=1, apply scattering of radiation from scattering regions
C     that originates on the accretion disk faces.
C     !!!!!!!!!!!!!!!!!!!!!!!!
C     ******************
C     ******************
      IF (KEY2.EQ.1) THEN
C     ******************
C     ******************
C     Calculate azimuthal separation of voxels.
	DPH=TWPI/(NSGAN-1)
C     Calculate azimuthal separation of face segments
      DELPH=PHIR(2)-PHIR(1)
C
C
C     ***********************************
C     Set the starting and ending indices
C     ***********************************
C
      KST=1
C	KND=1		 !Test value
	KND=NRING
	LST=1
C	LND=1		 !Test value
	LND=NSGAN-1
      KFST=2		 !Note! This is the correct value.
C	KFND=5		 !Test value
	KFND=NRING
      LFST=1
C	LFND=5		 !Test value
      LFND=NSG-1
C     ***************************
      DO K=KST,KND
		DO L=LST,LND
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
C     Calculate azimuth of voxel being illuminated.
			VOXPHI=(L-1)*DPH+DPH/2.D0
			DO LH=1,LMT
C     Initialize VFLXA(K,L,LH,1), etc.
				VFLXA(K,L,LH,1)=0.D0
				VFLXA(K,L,LH,2)=0.D0
C     Choose an increment for distance along pencil.
				DLS=(RGRD(2)-RGRD(1))
				DLSSV=DLS
C     Determine test distance for voxel under consideration.
C     This is the avg. distance from the voxel center to the remote
C     two corners on the lower voxel face.
C     It is used to determine whether the pencil of radiation under
C     consideration has reached the voxel in question.
C     Note that the coods. XAN(), YAN(), ZAN() are relative to the
C     center of the accretion disk.
      TSDS1=DSQRT((XAN(K+1,L,LH,1)-CX(K,L,LH,1))**2
     $		+(YAN(K+1,L,LH,1)-CY(K,L,LH,1))**2
     $		*(ZAN(K+1,L,LH,1)-CZ(K,L,LH,1))**2)
	TSDS2=DSQRT((XAN(K+1,L+1,LH,1)-CX(K,L,LH,1))**2
     $		+(YAN(K+1,L+1,LH,1)-CY(K,L,LH,1))**2
     $		*(ZAN(K+1,L+1,LH,1)-CZ(K,L,LH,1))**2)
	TSDS=(TSDS1+TSDS2)/2.D0
	IF (TSDS.LT.1.D-3) TSDS=1.D-3
C     Produce contributions from all face annuli, for a given voxel.
				DO KF=KFST,KFND
C     The temperature is constant on a given face annulus. Extract
C     limb darkening and normal intensity for that face annulus.
					U1FC=U1F(KF,1)
					U2FC=U2F(KF,1)
					BDIFC=BDIF(KF,1)
C     Calculate area of a face segment.
C     Area=pi*((outer radius*SEPCM)**2-(inner radius*SEPCM)**2)*
C     DELPH/twpi
					AREASG=(RGRD(KF)**2-RGRD(KF-1)**2)*SEPCM**2*DELPH/
     $						2.D0
C     Calculate orbital plane distance to center of face segment.
					RNG=(RGRD(KF)+RGRD(KF-1))/2.D0
C     Consider a given azimuthal segment on a given face annulus.
					DO LF=LFST,LFND
C     Initialize VTAU
						VTAU=0.D0
C     Calculate azimuth of face segment
						FCPHI=(PHIR(LF)+PHIR(LF+1))/2.D0
C     Calculate projection of RNG on X axis.
						X0=RNG*DCOS(FCPHI)
						SEPX=CX(K,L,LH,1)-X0
C     Calculate projection of RNG on Y axis.
						Y0=RNG*DSIN(FCPHI)
						SEPY=CY(K,L,LH,1)-Y0
C     Calculate projection of RNG on Z axis. Note that VZAF() is defined 
C     for the lower (visible) face.
						CZABS=DABS(CZ(K,L,LH,1))
						Z0=DABS(VZAF(KF,LF))
						SEPZ=CZ(K,L,LH,1)/CZABS*(CZABS-Z0)							
C     Calculate separation of face segment and voxel.
						SEPSQ=SEPX**2+SEPY**2+SEPZ**2
						SEP=DSQRT(SEPSQ)
C     Calculate direction cosines of vector from face segment to voxel.
						VLSEP=SEPX/SEP
						VMSEP=SEPY/SEP
						VNSEP=SEPZ/SEP
C     Calculate area of face segment projected perpendicular to above
C     vector.
						PROJAR=AREASG*VNSEP
						IF (PROJAR.LT.0.D0) PROJAR=0.D0
C     Calculate light intensity from face segment.
						IF (VNSEP.GE.0.D0) THEN
							VLTINT=BDIFC*(1.D0-U1FC-U2FC+U1FC*VNSEP+
     $						U2FC*VNSEP**2)*PROJAR
						ELSE
							VLTINT=0.D0
						END IF
C     We have already set X0, Y0, Z0, the starting coods on the pencil.
C     The coods. of the illuminated point are CX(K,L,LH,1), 
C     CY(K,L,LH,1), CZ(K,L,LH,1).
C     Now locate the voxel which contains the starting point.
						KTS=0
						LTS=0
						LHTS=0
    3						KTS=KTS+1
C     Find value of KTS.
	IF ((RNG.GE.RGRD(KTS)).AND.(RNG.LE.RGRD(KTS+1))) THEN
							GO TO 4
						ELSE
							IF (KTS.LT.NRING) GO TO 3
							CALL PMDSTOP
						END IF
C     Find value of LTS.
    4						LTS=LTS+1
						VXPH=(LTS-1)*DPH
	IF ((FCPHI.GE.VXPH).AND.(FCPHI.LE.VXPH+DPH)) THEN
							GO TO 5
						ELSE
							IF (LTS.LT.LND) GO TO 4
						CALL PMDSTOP
						END IF
C     Check whether Z0 lies within initial voxel.
C     Increment LHTS.
    5						LHTS=LHTS+1
      IF (LHTS.EQ.1) THEN
C     !Note! The index for RGRD begins at the innermost edge of the
C     smallest face annulus. The index for XAN, YAN, ZAN begins at the
C     origin, so the ZAN index equals the RGRD index+1.
		IF (Z0.LT.ZAN(KTS+1,LTS,LHTS,1)) GO TO 6
	END IF		
      IF  ((Z0.GE.ZAN(KTS+1,LTS,LHTS,1)).AND.
     $					(Z0.LE.ZAN(KTS+1,LTS,LHTS+1,1))) THEN
C     We have located the initial voxel.      
							GO TO 6
						ELSE
							LMT=NVDAN(KTS)-1
							IF (LHTS.LE.LMT) GO TO 5
							CALL PMDSTOP
						END IF
    6 CONTINUE
C
C     Set current coods. of end point of pencil.
						VHLOC=Z0
						VXLOC=X0
						VYLOC=Y0
C     Initialize VSLOC, the distance of the end point of the pencil from 
C     its start
						VSLOC=0.D0
C     Determine whether current end of pencil is within test distance 
C     of center of voxel.
C     Note that VXLOC, VYLOC are relative to the center of the accretion 
C     disk.
						KLOOP=0
   42						KLOOP=KLOOP+1
						DELTAS=SEP-VSLOC
						IF (DABS(DELTAS).LT.1.D-3) GO TO 36
						IF (DELTAS.LT.0.D0) THEN
							VSLOC=VSLOC-DLS
							VXLOC=VXLOC-DLS*VLSEP
							VYLOC=VYLOC-DLS*VMSEP
							VHLOC=VHLOC-DLS*VNSEP
						DLTAU=SGTM*DENSEL(KTS,LTS,LHTS,1)*SEPCM*DLS
							VTAU=VTAU-DLTAU
							DLS=DLS/2.D0
							IF (KLOOP.LT.100) GO TO 42
							CALL PMDSTOP
						END IF
						TEST=DELTAS/TSDS
						IF (TEST.LT.1.D0) THEN
C     Current end of pencil is within test distance of center of voxel.
C     Calculate distance from current end of pencil to center of voxel
C     and increment VTAU appropriately.
							DLTAU=SGTM*DENSEL(K,L,LH,1)*SEPCM*DELTAS
							VTAU=VTAU+DLTAU
C     We have reached the voxel center. Calculate scattered light for 
C     that face segment.
							GO TO 36
						END IF
C     If we arrive here, the current end of the pencil has not reached
C     the voxel in question. Must step through intervening voxels.
C     Increment location of end of pencil and increment VTAU.
						VSLOC=VSLOC+DLS
						VXLOC=VXLOC+DLS*VLSEP
						VYLOC=VYLOC+DLS*VMSEP
						VRLOC=DSQRT(VXLOC**2+VYLOC**2)
C     Calculate position angle of end point.
						ANGVL=DACOS(VXLOC/VRLOC)
						IF (ANGVL.LT.0.D0) ANGVL=ANGVL+TWPI
						VHLOC=VHLOC+DLS*VNSEP
						DLTAU=SGTM*DENSEL(KTS,LTS,LHTS,1)*SEPCM*DLS
						VTAU=VTAU+DLTAU
C     Locate voxel containing current end point
						IF (VRLOC.LT.RGRD(1)) THEN
							KTS=1
							GO TO 37
						END IF
						DO KTS=2,KND
							IF ((VRLOC.GE.RGRD(KTS-1)).AND.
     $							(VRLOC.LE.RGRD(KTS))) GO TO 37
						END DO
						CALL PMDSTOP
   37						DO LTS=1,LND
							ANGTST=(LTS-1)*DPH
							IF ((ANGVL.GE.ANGTST).AND.
     $							(ANGVL.LE.ANGTST+DPH)) GO TO 38
						END DO
						CALL PMDSTOP
   38						DO LHTS=1,LMT
							IF (LHTS.EQ.1) THEN
								IF (VHLOC.LT.CZ(KTS,LTS,LHTS,1)) 
     $								GO TO 39
							END IF									
							IF ((VHLOC.GE.CZ(KTS,LTS,LHTS,1)).AND.
     $							(VHLOC.LE.CZ(KTS,LTS,LHTS+1,1)))
     $							GO TO 39
						END DO
						IF ((SEP-VSLOC).LT.0.D0) GO TO 42
						IF ((VHLOC-DLS*VNSEP).LT.CZ(KTS,LTS,LHTS-1,1)) 
     $						GO TO 42
						VSLOC=VSLOC-DLS
						VXLOC=VXLOC-DLS*VLSEP
						VYLOC=VYLOC-DLS*VMSEP
						VHLOC=VHLOC-DLS*VNSEP
						DLS=DLS/2.D0 
C     Test whether we have reached the voxel in question.
   39						GO TO 42	   
C     We have reached the voxel center. Calculate the scattered light.
C	First calculate the incident light.	Note that the emission from
C     the segment of the face photosphere is into a hemisphere. This is
C     the reason for the 2.D0*PI factor.
   36						DIV=2.D0*PI*(SEP*SEPCM)**2
   						TERM1=VLTINT/DIV/DEXP(VTAU)
C     Calculate the cosine of the scattering angle.
						CSANG=VL0*VLSEP+VM0*VMSEP+VN0*VNSEP
C     Calculate phase function.
						PHFN=0.75D0*(1.D0+CSANG**2)
C     Calculate TERM2
						TERM2=SGTM*PHFN*VNMEL(K,L,LH,1)
						SCAT=TERM1*TERM2/TWPI/2.D0
C     Add this additional scattered light to the current contents of
C     the scattered light for this voxel.
						VFLXA(K,L,LH,1)=VFLXA(K,L,LH,1)+SCAT
C     Do the corresponding case for the other coronal region.
C     Note that, for this case, VNSEP becomes -VNSEP.
						CSANG=VL0*VLSEP+VM0*VMSEP-VN0*VNSEP
						PHFN=0.75D0*(1.D0+CSANG**2)
						TERM2=SGTM*PHFN*VNMEL(K,L,LH,2)
						SCAT=TERM1*TERM2/TWPI/2.D0
						VFLXA(K,L,LH,2)=VFLXA(K,L,LH,2)+SCAT
						DLS=DLSSV
C     Complete all segments of a given face annulus. Illumination of a
C     given voxel.
					END DO
C     Complete all face annuli. Illumination of a given voxel.
				END DO
C
			END DO	 !LH loop
		END DO	 !L loop
	END DO	 !K loop
C     ******************
C     ******************
      END IF	 !Test for KEY2     
C      
C     ******************
C     ******************
C     
C     End of program section under control of KEY2.
C     Note that this program section has not modified the stored values
C     of any coods. At this point, all stored coods. are relative to
C     the center of the accretion disk.
C
C     ***************************************
C     Note that the following program section runs for all values of
C     the control keys.
C     ***************************************     


C
C     Set visibility keys for blocking of coronal voxels by the
C     accretion disk..
C     The visibility keys are controlled by the boundary of the
C     accretion disk.
C     First do the visibility keys for the upper coronal region.
C     The radial distances and position angles of top rim points,
C     relative to the projected center of the central star.
C     These two quantities are, respectively, RVL() and ANG().
C     Note that the positive
C     X direction is vertically downward.
C     Any coronal point whose projected distance from that center is
C     less than the distance to the rim, at the same position angle,
C     is hidden.
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
C     Note that CXP() and CYP() are plane-of-sky projected 
C     X-coordinate and Y-coordinate distances
C     of voxel centers from the projected center of the gainer.
				XVL=CXP(K,L,LH,2)
				YVL=CYP(K,L,LH,2)
				DIST=DSQRT(XVL**2+YVL**2)
				PA=DASIN(DABS(YVL/DIST))
				DO KK=1,NSG-1
					IF ((ANG(KK).LE.PA).AND.(ANG(KK+1).GT.PA)) THEN
						COMP=0.5D0*(RVL(KK)+RVL(KK+1))
						IF (COMP.GE.DIST) THEN
C     Use visibility key that is distinctive for blocking by accretion 
C     disk
							VSBKY(K,L,LH,2)=-2.D0
						END IF
					END IF
					IF ((ANG(KK).GE.PA).AND.(ANG(KK+1).LT.PA)) THEN
						COMP=0.5D0*(RVL(KK)+RVL(KK+1))
						IF (COMP.GE.DIST) THEN 
							VSBKY(K,L,LH,2)=-2.D0
						END IF
					END IF
					IF (DABS(ANG(KK)-PA).LT.1.D-10) THEN
						COMP=RVL(KK)
						IF (COMP.GE.DIST) THEN
							VSBKY(K,L,LH,2)=-2.D0
						END IF
					END IF
					IF (DABS(ANG(KK+1)-PA).LT.1.D-10) THEN
						COMP=RVL(KK+1)
						IF (COMP.GE.DIST) THEN
							VSBKY(K,L,LH,2)=-2.D0
						END IF
					END IF
				END DO
			END DO
		END DO
	END DO
C***********************
C		 	     
C     Now do the visibility keys for the lower coronal region.
C     Note that the angles in array AGL are all greater than 0.0.
C	Follow the visibility key procedure in SRT VSB.
C     However, note that the positive X direction for the corona
C     is opposite to that of the accretion disk, and that it was
C     necessary to reverse the signs of CXP and CYP to make them
C     compatible with the accretion disk.
C     The distance from the projected center of the gainer to the
C     major axis of the projected bottom ring of the rim is XRMC.
C
C*********************** 
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				XVL=CXP(K,L,LH,1)+XRMC
				YVL=CYP(K,L,LH,1)+YRMC
				DIST=DSQRT(XVL**2+YVL**2)
				IF (DABS(YVL).GT.1.D-12) GO TO 1
				IF (XVL.GT.0.D0) PA=0.D0
				IF (XVL.LT.0.D0) PA=PI
				GO TO 2
    1				PA=DASIN(DABS(YVL)/DIST)
				IF ((YVL.GT.0.D0).AND.(XVL.GT.0.D0)) GO TO 2
				IF ((YVL.GT.0.D0).AND.(XVL.LE.0.D0)) PA=PI-PA
				IF ((YVL.LE.0.D0).AND.(XVL.LE.0.D0)) PA=PI+PA
				IF ((YVL.LE.0.D0).AND.(XVL.GT.0.D0)) PA=TWPI-PA
    2				CONTINUE
				DO KK=1,NSG-1
					IF ((AGL(KK).LE.PA).AND.(AGL(KK+1).GE.PA)) THEN
						COMP=0.5D0*(RD(KK)+RD(KK+1))
C     Note that the current origin of coods. is at the center of the 
C     gainer.
						IF ((COMP.LT.DIST).AND.(XVL.GT.0.D0)) THEN
							VSBKY(K,L,LH,1)=-2.D0
						END IF
					END IF
					IF (AGL(KK).GT.AGL(KK+1)) THEN
						IF ((AGL(KK).LE.PA).AND.(AGL(KK+1).LE.PA))THEN
							COMP=0.5D0*(RD(KK)+RD(KK+1))
							IF ((COMP.LT.DIST).AND.(XVL.GT.0.D0)) THEN
								VSBKY(K,L,LH,1)=-2.D0
							END IF
						END IF
					END IF
C     XLSL is the smallest X value on the bottom rim circle,
C     measured relative to the projected center of the gainer.
C     CXP(K,L,LH,2) is measured relative to the same point.
C     Note that more negative CXP values are further from the origin.
					IF (-CXP(K,L,LH,1).LT.XLSL) THEN
						VSBKY(K,L,LH,1)=-2.D0
					END IF
				END DO
			END DO
		END DO
	END DO
C***********************
 1000 FORMAT (3I3,7F9.4)
 1001 FORMAT (3I3,9F7.4)
C     Start of program section under control of KEY3
C     This program section calculates the light loss from the primary
C     component due to light scattering out of the pencils from the
C     primary component, directed toward the observer, because of the
C     interposition of the light scattering regions.
C     ******************
C     ******************
	IF (KEY3.EQ.1) THEN
C**************************************************************
C     At entry to this program section, the coods. of the coronal
C     grid are relative to the center of the accretion disk.
C     XUL, XLL, YUL, YLL  are relative to the center of the accretion
C     disk. They were transformed after input from ACPGB.
C     Transfer the coods. of the coronal grid to the center of mass.
C**************************************************************
C
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				IF (ISTAR.EQ.ISV) THEN
					CXP(K,L,LH,1)=CXP(K,L,LH,1)+DX
					CYP(K,L,LH,1)=CYP(K,L,LH,1)-DY
					CXP(K,L,LH,2)=CXP(K,L,LH,2)+DX
					CYP(K,L,LH,2)=CYP(K,L,LH,2)-DY
					CZP(K,L,LH,1)=CZP(K,L,LH,1)-DZ
					CZP(K,L,LH,2)=CZP(K,L,LH,2)-DZ
					XANP(K,L,LH,1)=XANP(K,L,LH,1)+DX
					YANP(K,L,LH,1)=YANP(K,L,LH,1)-DY
					XANP(K,L,LH,2)=XANP(K,L,LH,2)+DX
					YANP(K,L,LH,2)=YANP(K,L,LH,2)-DY
					ZANP(K,L,LH,1)=ZANP(K,L,LH,1)-DZ
					ZANP(K,L,LH,2)=ZANP(K,L,LH,2)-DZ
				ELSE
					CXP(K,L,LH,1)=CXP(K,L,LH,1)+DDX
					CYP(K,L,LH,1)=CYP(K,L,LH,1)-DDY
					CXP(K,L,LH,2)=CXP(K,L,LH,2)+DDX
					CYP(K,L,LH,2)=CYP(K,L,LH,2)-DDY
					CZP(K,L,LH,1)=CZP(K,L,LH,1)-DDZ
					CZP(K,L,LH,2)=CZP(K,L,LH,2)-DDZ
					XANP(K,L,LH,1)=XANP(K,L,LH,1)+DDX
					YANP(K,L,LH,1)=YANP(K,L,LH,1)-DDY
					XANP(K,L,LH,2)=XANP(K,L,LH,2)+DDX
					YANP(K,L,LH,2)=YANP(K,L,LH,2)-DDY
					ZANP(K,L,LH,1)=ZANP(K,L,LH,1)-DDZ
					ZANP(K,L,LH,2)=ZANP(K,L,LH,2)-DDZ
				END IF
			END	DO
		END DO
	END DO
C
C     Note that the coods. XANP(), YANP(), ZANP(), CXP(), CYP(), and
C     CZP() now are relative to the system center of mass.
C     Restrict orbital phase range to region of primary minimum.
		IF ((PSI.LT.1.57D0).OR.(PSI.GT.4.73D0)) THEN
C     Test for overlap of the projection of the mass loser and the
C     accretion disk.
C     Scan for extreme values of X and Y coods. of loser grid points,
C     projected on the plane of the sky. The projected points are in
C     the arrays VLX() and VLY().
C
C     NOTE. The "plane of the sky coods." refers to a cood. system
C     with the origin at the system center of mass, with the X- and 
C     Y-axes in a plane perpendicular to the line of sight, and the
C     Z-axis directed along the line of sight toward the observer.
C
C     The grid points have been read in from PLTB output data.
		XMX=-1.D0
		XMNV=1.D0
		YMX=-1.D0
		YMN=1.D0
		CALL PROJ(TARY,VLX(1),VLY(1),VLZ(1),PJX,PJY,PJZ)
		IF (XMX.LT.PJX) XMX=PJX
		IF (XMNV.GT.PJX) XMNV=PJX
		CALL PROJ(TARY,VLX(KLG),VLY(KLG),VLZ(KLG),PJX2,PJY2,PJZ2)
		IF (XMX.LT.PJX2) XMX=PJX2
		IF (XMNV.GT.PJX2) XMNV=PJX2
		CALL IDW(I,NTH,KST)
		N=MPH(I,NTH)
		KND=KST+N-1
		DO KK=1,KLG
			IF ((KIDX(KK).GE.KST).AND.(KIDX(KK).LE.KND)) THEN
				CALL PROJ(TARY,VLX(KK),VLY(KK),VLZ(KK),PJX,PJY,PJZ)
				IF (YMX.LT.PJY) YMX=PJY
				IF (YMN.GT.PJY) YMN=PJY
			END IF
		END DO
C     The maximum and minimum X-axis projections of the mass loser
C     are in XMX and XMNV, and the corresponding Y-axis projections
C     are in YMX and YMN.
C     Do any of the four extreme pencils intersect the light scatt.
C     regions?
C     Find the plane of the sky limits for the two light scattering 
C     regions.
		XMXAD1=-1.D0
		XMNAD1=1.D0
		YMXAD1=-1.D0
		YMNAD1=1.D0
		XMXAD2=-1.D0
		XMNAD2=1.D0
		YMXAD2=-1.D0
		YMNAD2=1.D0
		LMT=NVDAN(1)-1
		DO KK=1,NSGAN-1
		IF (XMXAD1.LT.CXP(NRINGLMT,KK,1,1)) XMXAD1=CXP(NRINGLMT,KK,1,1)
		IF (XMNAD1.GT.CXP(NRINGLMT,KK,1,1)) XMNAD1=CXP(NRINGLMT,KK,1,1)
		IF (XMXAD1.LT.CXP(1,KK,LMT,1)) XMXAD1=CXP(1,KK,LMT,1)
		IF (XMNAD1.GT.CXP(1,KK,LMT,1)) XMNAD1=CXP(1,KK,LMT,1)
		IF (YMXAD1.LT.CYP(NRINGLMT,KK,1,1)) YMXAD1=CYP(NRINGLMT,KK,1,1)
		IF (YMNAD1.GT.CYP(NRINGLMT,KK,1,1)) YMNAD1=CYP(NRINGLMT,KK,1,1)
		IF (YMXAD1.LT.CYP(1,KK,LMT,1)) YMXAD1=CYP(1,KK,LMT,1)
		IF (YMNAD1.GT.CYP(1,KK,LMT,1)) YMNAD1=CYP(1,KK,LMT,1)
		IF (XMXAD2.LT.CXP(NRINGLMT,KK,1,2)) XMXAD2=CXP(NRINGLMT,KK,1,2)
		IF (XMNAD2.GT.CXP(NRINGLMT,KK,1,2)) XMNAD2=CXP(NRINGLMT,KK,1,2)
		IF (XMXAD2.LT.CXP(1,KK,LMT,2)) XMXAD2=CXP(1,KK,LMT,2)
		IF (XMNAD2.GT.CXP(1,KK,LMT,2)) XMNAD2=CXP(1,KK,LMT,2)
		IF (YMXAD2.LT.CYP(NRINGLMT,KK,1,2)) YMXAD2=CYP(NRINGLMT,KK,1,2)
		IF (YMNAD2.GT.CYP(NRINGLMT,KK,1,2)) YMNAD2=CYP(NRINGLMT,KK,1,2)
		IF (YMXAD2.LT.CYP(1,KK,LMT,2)) YMXAD2=CYP(1,KK,LMT,2)
		IF (YMNAD2.GT.CYP(1,KK,LMT,2)) YMNAD2=CYP(1,KK,LMT,2)
		END DO	!KK=1,NSGAN-1
C     If YMX or YMN is bracketed by YMXAD1,2 and YMNAD1,2, and XMX or 
C     XMN is
C     bracketed by XMXAD1,2 and XMNAD1,2, then calculate optical depth 
C     through light scattering region.
		IF ((YMX.GT.YMNAD1).AND.(YMX.LT.YMXAD1)) THEN
			IF ((XMNAD1.GT.XMNV).AND.(XMNAD1.LT.XMX)) GO TO 237
			IF ((XMXAD1.GT.XMNV).AND.(XMXAD1.LT.XMX)) GO TO 237
		END IF
		IF ((YMN.GT.YMNAD1).AND.(YMN.LT.YMXAD1)) THEN
			IF ((XMNAD1.GT.XMNV).AND.(XMNAD1.LT.XMX)) GO TO 237
			IF ((XMXAD1.GT.XMNV).AND.(XMXAD1.LT.XMX)) GO TO 237
		END IF
		IF ((YMX.GT.YMNAD2).AND.(YMX.LT.YMXAD2)) THEN
			IF ((XMNAD2.GT.XMNV).AND.(XMNAD2.LT.XMX)) GO TO 237
			IF ((XMXAD2.GT.XMNV).AND.(XMXAD2.LT.XMX)) GO TO 237
		END IF
		IF ((YMN.GT.YMNAD2).AND.(YMN.LT.YMXAD2)) THEN
			IF ((XMNAD2.GT.XMNV).AND.(XMNAD2.LT.XMX)) GO TO 237
			IF ((XMXAD2.GT.XMNV).AND.(XMXAD2.LT.XMX)) GO TO 237
		END IF
C     Conditions not met for calculation of optical depth for that
C     orbital longitude. Skip to next orbital longitude.
		GO TO 248
C     Conditions met for calculation of optical depth of each pencil
C     from eclipsed star toward observer.
C     The center of mass grid points on the eclipsed star, expressed
C     in the primitive cood. system, with X-axis directed from star
C     ISV (=the donor in the case of beta Lyrae) toward the other star,
C     are in VLX(), VLY(), VLZ(). These entries involve only grid points
C     that are "visible" to the observer, and include points hidden by
C     the companion and the accretion disk proper.
C
C     We will consider successively each point in the VLX(),... arrays
C     and follow a pencil of radiation starting at a given one of the 
C     points and directed toward the observer. The direction cosines of
C     the pencil are VL0, VM0, VN0.
C
  237 CONTINUE
C     Choose an initial coarse step length.
      DSTP=0.1D0
C     Initialize pointer for light scattering region.
      IDXRF=0
C     Calculate optical depths for all pencils from eclipsed star.
C     This loop includes all pencils.
		DO KK=1,KLG
C     Initialize the (starting) end point on the given pensil.
C     Note that VXLOC, VYLOC are relative to system center of mass.
			VXLOC=VLX(KK)
			VYLOC=VLY(KK)
			VHLOC=VLZ(KK)
			VSLOC=0.D0
C     Initialize loop counter.
			ILOOP=0
C     Initialize VTAU
			VTAU=0.D0
C     Project the current pencil point to the plane of sky coods.
			CALL PROJ(TARY,VXLOC,VYLOC,VHLOC,XFV,YFV,ZFV)
C     Check whether the pencil projects within the light scattering
C     regions and identify which light scattering region is involved.
			IF ((XFV.GE.XMNAD1).AND.(XFV.LE.XMXAD1)) THEN
				IF ((YFV.GE.YMNAD1).AND.(YFV.LE.YMXAD1)) THEN
					IDXRF=1
					GO TO 260
				END IF
			END IF
			IF ((XFV.GE.XMNAD2).AND.(XFV.LE.XMXAD2)) THEN
				IF ((YFV.GE.YMNAD2).AND.(YFV.LE.YMXAD2)) THEN
					IDXRF=2
					GO TO 260
				END IF 
			END IF
C     The pencil projection on the plane of the sky does not fall within
C     either of the projected light scattering regions. Test the next 
C     point.
			GO TO 467
C     Start of loop for calculating VTAU for a given pencil.
C     XFV,YFV,ZFV are the projected coods. of a current point along a given 
C     pencil.
  260			VXLOC=VXLOC+VL0*DSTP
			VYLOC=VYLOC+VM0*DSTP
			VHLOC=VHLOC+VN0*DSTP
			VSLOC=VSLOC+DSTP
C     Calculate distance from origin, in orbital plane.
			VRVL=DSQRT(VXLOC**2+VYLOC**2)
C     Compare pencil point location with outer radius of accretion disk 
C     rim.
C     Continue stepping until we reach vicinity of accretion disk.
			IF (VRVL.GT.RGRD(NRING))  THEN
				IF (VRVL.LT.2.D0) GO TO 260
				CALL PMDSTOP
			END IF
			IF (VRVL.EQ.RGRD(NRING)) GO TO 261
	IF (DABS(VRVL-RGRD(NRING)).LT.0.2D0*(RGRD(NRING-RGRD(NRING-1))))
     $			GO TO 261
C     Step back one step and choose smaller increment.
			ILOOP=ILOOP+1
			VXLOC=VXLOC-VL0*DSTP
			VYLOC=VYLOC-VM0*DSTP
			VHLOC=VHLOC-VN0*DSTP
			VSLOC=VSLOC-DSTP
			VRVL=DSQRT(VXLOC**2+VYLOC**2)
			DSTP=(RGRD(NRING)-RGRD(NRING-1))/(ILOOP+1)
			IF (ILOOP.LT.10) GO TO 260
C     On arriving here we have reached the boundary of the light 
C     scattering region. Step through the region, determining the
C     nearest voxel center at each step and calculating the increment
C     in optical depth for each step. First choose a step length.
  261			DSTP=(RGRD(NRING)-RGRD(NRING-1))/3.D0
C     Recalculate distance from origin, in orbital plane.
			VRVL=DSQRT(VXLOC**2+VYLOC**2)
C     Locate indices for the enclosing voxel. Note that the electron 
C     density	is constant within annuli, so it is unnecessary to
C     locate the azimuthal index.
  266			DO KKK=2,NRINGLMT
				IF (VRVL.LE.RGRD(1)) THEN
					KRF=1
					GO TO 262
				END IF
				IF ((VRVL.GT.RGRD(KKK-1)).AND.(VRVL.LE.RGRD(KKK)))
     $				GO TO 263
			END DO
  262			KKK=KRF
  263			IF (KKK.LE.NRING) THEN
				LMT=NVDAN(KKK)-1
			ELSE
				LMT=NVDAN(KKK-1)-1
			END IF
			DO LHH=1,LMT
				IF (IDXRF.EQ.1) THEN
					IF (VHLOC.LE.CZ(KKK,1,1,IDXRF)) THEN
						DLTAU=400.D0
						GO TO 466
					END IF
					IF ((VHLOC.GT.CZ(KKK,1,LHH,IDXRF)).AND.
     $					(VHLOC.LE.CZ(KKK,1,LHH+1,IDXRF))) GO TO 465
				END IF
				IF (IDXRF.EQ.2) THEN
					IF (VHLOC.GE.CZ(KKK,1,1,IDXRF)) THEN
						DLTAU=400.D0
						GO TO 466
					END IF
					IF ((VHLOC.LT.CZ(KKK,1,LHH,IDXRF)).AND.
     $					(VHLOC.GE.CZ(KKK,1,LHH+1,IDXRF))) GO TO 465
				END IF
			END DO
C     Set the length for the current step along the pencil.
  465				DLGTH=DABS(DSTP)
C     Calculate DLTAU for DLGTH within that voxel.
				DLTAU=SGTM*DENSEL(KKK,1,LHH,IDXRF)*SEPCM*DLGTH
				VTAU=VTAU+DLTAU
C     We have completed the test for the current point along the pencil.
C     Test whether we have passed through the light scattering region.
			IF (IDXRF.EQ.1) THEN
				VRVL=DSQRT(VXLOC**2+VYLOC**2)
				IF (VRVL.GT.RGRD(NRING)) GO TO 466
C     Have not reached outer boundary of light scattering region.
C     Increment location of pencil point and test again.
				VXLOC=VXLOC+DSTP*VL0
				VYLOC=VYLOC+DSTP*VM0
				VHLOC=VHLOC+DSTP*VN0
				VSLOC=VSLOC+DSTP
				VRVL=DSQRT(VXLOC**2+VYLOC**2)
				GO TO 266
			END IF
			IF (IDXRF.EQ.2) THEN
				VRVL=DSQRT(VXLOC**2+VYLOC**2)
				IF (VRVL.GT.RGRD(NRING)) GO TO 466
				VXLOC=VXLOC+DSTP*VL0
				VYLOC=VYLOC+DSTP*VM0
				VHLOC=VHLOC+DSTP*VN0
				VSLOC=VSLOC+DSTP
				VRVL=DSQRT(VXLOC**2+VYLOC**2)
				GO TO 266
			END IF
C     We have completed the calculation of VTAU for that pencil.
C     Store the value of VTAU for use by ACPGDRG.	Index KK successively
C     points to all the projected points on the mass loser above the
C     horizon of the observer.
  466			VTAUSV(KK)=VTAU
C     If we arrive here we are finished with a given pencil.
C     Go to next pencil.
  467			CONTINUE
		END DO	!KK=1,KLG
C
  248 CONTINUE
		END IF	!IF ((PSI.LT.1.57D0).OR.(PSI.GT.4.73D0))
C***********************
C     Now transform back to origin at center of gainer.
C***********************      
      DO K=1,NRINGLMT
	DO L=1,NSGAN-1
		IF (K.LE.NRING) THEN
			LMT=NVDAN(K)-1
		ELSE
			LMT=NVDAN(K-1)-1
		END IF
	DO LH=1,LMT
		IF (ISTAR.EQ.ISV) THEN
			CXP(K,L,LH,1)=CXP(K,L,LH,1)-DX
			CYP(K,L,LH,1)=CYP(K,L,LH,1)+DY
			CXP(K,L,LH,2)=CXP(K,L,LH,2)-DX
			CYP(K,L,LH,2)=CYP(K,L,LH,2)+DY
			CZP(K,L,LH,1)=CZP(K,L,LH,1)+DZ
			CZP(K,L,LH,2)=CZP(K,L,LH,2)+DZ
			XANP(K,L,LH,1)=XANP(K,L,LH,1)-DX
			YANP(K,L,LH,1)=YANP(K,L,LH,1)+DY
			XANP(K,L,LH,2)=XANP(K,L,LH,2)-DX
			YANP(K,L,LH,2)=YANP(K,L,LH,2)+DY
		ELSE
			CXP(K,L,LH,1)=CXP(K,L,LH,1)-DDX
			CYP(K,L,LH,1)=CYP(K,L,LH,1)+DDY
			CXP(K,L,LH,2)=CXP(K,L,LH,2)-DDX
			CYP(K,L,LH,2)=CYP(K,L,LH,2)+DDY
			CZP(K,L,LH,1)=CZP(K,L,LH,1)+DDZ
			CZP(K,L,LH,2)=CZP(K,L,LH,2)+DDZ
			XANP(K,L,LH,1)=XANP(K,L,LH,1)-DDX
			YANP(K,L,LH,1)=YANP(K,L,LH,1)+DDY
			XANP(K,L,LH,2)=XANP(K,L,LH,2)-DDX
			YANP(K,L,LH,2)=YANP(K,L,LH,2)+DDY
		END IF
	END DO
	END DO
	END DO
C
      END IF  !IF (KEY3.EQ.1)
C     ******************
C     ******************
C
C     End of program section under control of KEY3
C
C     ******************
C     ******************
C
C     Note that the following program section runs for all values of
C     the control keys, KEY1-KEY5.
C
C
C     Note that this program section has not changed the coods. of any
C     stored quantity, and that, at this point, CXP(), CYP(), CZP()
C     are relative to the center of the companion.
C     XANP(), YANP(), ZANP() are relative to the system center of mass.
C     CX(), CY(), CZ(), XAN(), YAN(), ZAN() are relative to the center
C     of the accretion disk.
C
C     ******************
C     Calculate visibility keys for eclipse of coronal grid by 
C     companion.
C     Follow precepts of SRT VSBSD
C     If the eclipsing star is the gainer, no coronal points are
C     hidden. (Neglect the coronal points behind the gainer.)
C
C     Determine maximum and minimum projected radii of the eclipsing
C     component.
      RMN=2.D0
	RMX=0.D0
C***********************
	IF (IEC.EQ.ISV) THEN
		DO J=1,IFCS
			IF (RSS(J,1).LT.RMN) RMN=RSS(J,1)
		END DO
		DO J=1,IRCS
			IF (RSS(J,2).LT.RMN) RMN=RSS(J,2)
		END DO
C***********************
C     RMN now contains the smallest projected radius of the eclipsing
C     star at the current orbital longitude.
C***********************
		DO J=1,IFCS
			IF (RSS(J,1).GT.RMX) RMX=RSS(J,1)
		END DO
		DO J=1,IRCS
			IF (RSS(J,2).GT.RMX) RMX=RSS(J,2)
		END DO
	ELSE
		DO J=1,IFC
			IF (RS(J,1).LT.RMN) RMN=RS(J,1)
		END DO
		DO J=1,IRC
			IF (RS(J,2).LT.RMN) RMN=RS(J,2)
		END DO
C***********************
C     RMN now contains the smallest projected radius of the eclipsing
C     star at the current orbital longitude.
C***********************
		DO J=1,IFC
			IF (RS(J,1).GT.RMX) RMX=RS(J,1)
		END DO
		DO J=1,IRC
			IF (RS(J,2).GT.RMX) RMX=RS(J,2)
		END DO
	END IF
C***********************
      IF (IEC.EQ.ISTAR) GO TO 50
C
C     RMX now contains the largest projected radius of the eclipsing
C     star at the current orbital longitude.
C     Note that RMN and RMX are measured from the center of the 
C     eclipsing star.
C     The coronal points currently have their origin at the system 
C     center of mass. We wish to determine the distance of a given
C     coronal point from the center of the eclipsing star.
C     First transform back to origin at center of gainer.
C***********************      
      DO K=1,NRINGLMT
	DO L=1,NSGAN-1
		IF (K.LE.NRING) THEN
			LMT=NVDAN(K)-1
		ELSE
			LMT=NVDAN(K-1)-1
		END IF
	DO LH=1,LMT
C     Now determine coods. of point relative to center of companion.
		X=CXP(K,L,LH,1)-XG
		Y=CYP(K,L,LH,1)+YG
		RELX=X
		RELY=Y
C     Now restore coods. relative to center of mass.
		IF (ISTAR.EQ.ISV) THEN
			CXP(K,L,LH,1)=CXP(K,L,LH,1)+DX
			CYP(K,L,LH,1)=CYP(K,L,LH,1)-DY
			CXP(K,L,LH,2)=CXP(K,L,LH,2)+DX
			CYP(K,L,LH,2)=CYP(K,L,LH,2)-DY
			CZP(K,L,LH,1)=CZP(K,L,LH,1)-DZ
			CZP(K,L,LH,2)=CZP(K,L,LH,2)-DZ
		ELSE
			CXP(K,L,LH,1)=CXP(K,L,LH,1)+DDX
			CYP(K,L,LH,1)=CYP(K,L,LH,1)-DDY
			CXP(K,L,LH,2)=CXP(K,L,LH,2)+DDX
			CYP(K,L,LH,2)=CYP(K,L,LH,2)-DDY
			CZP(K,L,LH,1)=CZP(K,L,LH,1)-DDZ
			CZP(K,L,LH,2)=CZP(K,L,LH,2)-DDZ
		END IF
C     Calculate distance of coronal point from center of eclipsing star.
			RV=DSQRT(X**2+Y**2)
C     If distance is greater than maximum projected radius of eclipsing
C     component, then the point is not eclipsed. But do not reset VSBKY
C     values already set for eclipse by accretion disk itself.
C     If distance is less than the minimum projected radius of eclipsing
C     component, then the point is surely eclipsed.
		IF ((RV.LT.RMN).AND.(VSBKY(K,L,LH,1).NE.-2.D0))
     $		 VSBKY(K,L,LH,1)=-1.D0
C     If distance is intermediate, must check in greated retail.
		IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 10
C     Now must do detailed check using position angle data.
		ANGL=DASIN(DABS(RELX/RV))
C     Set correct quadrant.
		IF (RELY.LT.0.D0) ANGL=PI-ANGL
		IF (RELX.LT.0.D0) ANGL=-ANGL
		IF (ANGL.LT.0.D0) GO TO 11
C     In range covered by IFCS
		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,
     $	RW)
		GO TO 12
C     In range covered by IRCS
   11		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,
     $	RW)
C     The projected distance of the coronal point from the center of
C     the eclipsing component is RV, and the position angle is ANG.
C     The projected radius of the eclipsing component at the same
C     position angle is RW.
   12 ER=RV-RW
C     Set visibility key, if it has not already been set.
      IF ((ER.LT.0.D0).AND.(VSBKY(K,L,LH,1).NE.-2.D0))
     $VSBKY(K,L,LH,1)=ER
   10 CONTINUE
C     Now determine coods. of point relative to center of companion.
		X=CXP(K,L,LH,2)-XG
		Y=CYP(K,L,LH,2)+YG
		RELX=X
		RELY=Y
C     Now restore coods. relative to center of mass.
		IF (ISTAR.EQ.ISV) THEN
			CXP(K,L,LH,1)=CXP(K,L,LH,1)+DX
			CYP(K,L,LH,1)=CYP(K,L,LH,1)-DY
			CXP(K,L,LH,2)=CXP(K,L,LH,2)+DX
			CYP(K,L,LH,2)=CYP(K,L,LH,2)-DY
			CZP(K,L,LH,1)=CZP(K,L,LH,1)-DZ
			CZP(K,L,LH,2)=CZP(K,L,LH,2)-DZ
			XANP(K,L,LH,1)=XANP(K,L,LH,1)+DX
			YANP(K,L,LH,1)=YANP(K,L,LH,1)-DY
			XANP(K,L,LH,2)=XANP(K,L,LH,2)+DX
			YANP(K,L,LH,2)=YANP(K,L,LH,2)-DY
		ELSE
			CXP(K,L,LH,1)=CXP(K,L,LH,1)+DDX
			CYP(K,L,LH,1)=CYP(K,L,LH,1)-DDY
			CXP(K,L,LH,2)=CXP(K,L,LH,2)+DDX
			CYP(K,L,LH,2)=CYP(K,L,LH,2)-DDY
			CZP(K,L,LH,1)=CZP(K,L,LH,1)-DDZ
			CZP(K,L,LH,2)=CZP(K,L,LH,2)-DDZ
			XANP(K,L,LH,1)=XANP(K,L,LH,1)+DDX
			YANP(K,L,LH,1)=YANP(K,L,LH,1)-DDY
			XANP(K,L,LH,2)=XANP(K,L,LH,2)+DDX
			YANP(K,L,LH,2)=YANP(K,L,LH,2)-DDY
		END IF
C     Calculate distance of coronal point from center of eclipsing star.
			RV=DSQRT(RELY**2+RELX**2)
C     If distance is greater than maximum projected radius of eclipsing
C     component, then the point is not eclipsed.
C     If distance is less than the minimum projected radius of eclipsing
C     component, then the point is surely eclipsed.
		IF ((RV.LT.RMN).AND.(VSBKY(K,L,LH,2).NE.-2.D0))
     $		 VSBKY(K,L,LH,2)=-1.D0
C     If distance is intermediate, must check in greated detail.
		IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 20
C     Now must do detailed check using position angle data.
		ANGL=DASIN(DABS(RELX/RV))
C     Set correct quadrant.
		IF (DABS(RELY).LT.0.D0) ANGL=PI-ANGL
		IF (RELX.LT.0.D0) ANGL=-ANGL
		IF (ANGL.LT.0.D0) GO TO 21
C     In range covered by IFCS
		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RS,1,IRCS,IFCS,NKPS,
     $	RW)
		GO TO 22
C     In range covered by IRCS
   21		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,
     $	RW)
C     The projected distance of the coronal point from the center of
C     the eclipsing component is RV, and the position angle is ANG.
C     The projected radius of the eclipsing component at the same
C     position angle is RW.
   22 ER=RV-RW
      IF ((ER.LT.0.D0).AND.(VSBKY(K,L,LH,2).NE.-2.D0)) 
     $VSBKY(K,L,LH,2)=ER
   20 CONTINUE
	END DO
	END DO
	END DO
C
C
C     Start of program section under control of KEY4
C     At this point, CXP(), CYP(), CZP(), XANP(), YANP(), ZANP() are
C     relative to the center of mass.
C     ******************
C     ******************
      IF (KEY4.EQ.1) THEN
C     *******************
C     Determine coods. of center of eclipsing star.
C     The first and last points that were input are at the poles of 
C     the star.
      XCENEC=(XECPJ(1)+XECPJ(KECL))/2.D0
	YCENEC=(YECPJ(1)+YECPJ(KECL))/2.D0
	ZCENEC=(ZECPJ(1)+ZECPJ(KECL))/2.D0
C     Cycle over all of the voxels in the scattering regions.
C     For each voxel, cycle over all of the points on the photosphere
C     of the eclipsing star, calculating the radiation incident on the
C     voxel from each surface area element on the eclipsing star.
C     There is a problem to be solved. For a given voxel, only part of
C     the photospheric points on the illuminating star are visible.
C     To solve this problem, do the following:
C     Test each starting pencil, at the eclipsing star photosphere, by
C     stepping a very short distance toward the target voxel and seeing
C     whether the end point is closer to or farther from the center of
C     the eclipsing star. If the end point is closer, the pencil passes
C     inward through the star. Terminate that pencil. The non-terminated 
C     pencils are visible at the voxel in question.
C
C     Begin with the first scattering region.
	DO KKK=1,NRINGLMT
		DO LL=1,NSGAN-1
			LMTLH=NVDAN(KKK)-1
			DO LHH=1,LMTLH
C     Consider visibility at center of voxel.
C     At this point, CX(), CY(), CZ() are relative to the center of
C     the accretion disk.	They are the spatial coods. of the voxel
C     centers.
				X2=CX(KKK,LL,LHH,1)
				Y2=CY(KKK,LL,LHH,1)
				Z2=CZ(KKK,LL,LHH,1)
C     Initialize buffer to accumulate scattered radiation.
				SUM=0.D0
C     Range over all points on photosphere of eclipsing star.
				DO ITH=1,MTH
					CALL IDW(I,ITH,KST)
					N=MPH(I,ITH)
					KND=KST+N-1
					DO J=KST,KND
C     Determine direction cosines of vector from given photospheric
C     point on eclipsing star to the particular voxel center.
						X1=XECPJ(J)
						Y1=YECPJ(J)
						Z1=ZECPJ(J)
C     DIST is the distance from the photospheric point to the voxel 
C     center.
						DIST=DSQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
C     VLVEC, VMVEC, VNVEC are the direction cosines of the pencil.
						VLVEC=(X2-X1)/DIST
						VMVEC=(Y2-Y1)/DIST
						VNVEC=(Z2-Z1)/DIST
C     Determine distance of starting point from center of eclipsing star.
						CENDS=DSQRT((X1-XCENEC)**2+(Y1-YCENEC)**2+
     $							(Z1-ZCENEC)**2)
C     Calculate the
C     incident flux from the segment of the photosphere.
C     Note that this calculation neglects the case in which the 
C     accretion disk rim blocks illumination of the voxel by the given
C     point on the eclipsing star. We suppress any contribution from
C     such a point to the OBSERVED light by use of the visibility keys
C     associated with the rim. Also note that there is no allowance for
C     scattering OUT of the pencil directed toward the voxel, or
C     scattering out of the pencil directed toward the observer. In
C     each case, there would also be some scattering into the pencil,
C     and that also is neglected, so there is some degree of 
C     compensation.
C     Will need indices for case of data stored for 1/8 of the complete
C     surface of the object.
						CALL SYOBK(I,ITH,J,MTH,KBK,KTH)
C     Determine the value of VPH1 for the given ITH value.
						CALL SYO(I,ITH,KST,NTH,JP,ITHP,ISG,VPH1)
						IF (J.GT.KST) THEN
C     The phi increment is constant for a given ITH. Consequently,
C     determine the value of VPH2 for the index KST+1, with the 
C     exception of the starting point on a given ITH. In this latter
C     case, set VPH2 so that the increment DPHI will be 0.0.
							CALL SYO(I,ITH,KST+1,NTH,JP,ITHP,ISG,
     $								VPH2)
						ELSE
							CALL SYO(I,ITH,KST,NTH,JP,ITHP,ISG,
     $								VPH2)
						END IF
C     Calculate the phi increment, DPHI.
						DPHI=VPH2-VPH1
C     Determine the theta value for ITH.
						CALL SYP(I,ITH,NTH,VTH2)
C     Determine the theta value for ITH-1, if ITH.GT.1.
						IF (ITH.GT.1) THEN
							CALL SYP(I,ITH-1,NTH,VTH1)
						ELSE
							CALL SYP(I,ITH,NTH,VTH1)
						END IF
C     Calculate the theta increment.
						DTH=VTH2-VTH1
						SNTH=DSIN(VTH2)
C     Extract the normal intensity. Note that we require an index for
C     the case of data storage for 1/8 of the complete object.
C     Note that, for beta Lyrae, I designates the primary start, the
C     star eclipsed at orbital phase 0.0, and the mass donor.
						VIVL=BDI(I,KBK)
C     Get the first and second order limb darkening coeffs.
						VU1=U1(I,KBK)
						VU2=U2(I,KBK)
C     Calculate the cosine of the angle between the surface normal 
C     and the vector toward the voxel.
						CSGM=ZL(I,KBK)*VLVEC+ZM(I,KBK)*VMVEC+
     $							ZN(I,KBK)*VNVEC
C     Calculate the intensity of radiation in the pencil.
C     Suppress any case in which CSGM is negative.
						IF (CSGM.GE.0.D0) THEN
						VIILUM=VIVL*(1.D0-VU1-VU2+VU1*CSGM+
     $							VU2*CSGM**2)*CSGM*SNTH*DTH*DPHI*
     $							CENDS**2/DIST**2/4.D0/PI
						END IF
C     Calculate product of VIILUM, electron density, and Thomson
C     scattering coefficient.
						VOXFLX=VIILUM*SGTM*VNMEL(KKK,LL,LHH,1)
C     Now calculate cosine of scattering angle.
						COSSCAT=VL0*VLVEC+VM0*VMVEC+VN0*VNVEC
C     Calculate phase function
						PHFN=(1.D0+COSSCAT**2)
C     Calculate and accumulate radiation scattered
C     into the line of sight (per unit solid angle) from the particular
C     photospheric segments and from the specific voxel.
C     NOTE:Normally, the second term in the following equation would be 
C     divided by 4*pi, but the other contributions to light received by
C     the observer do not contain that term. To allow for the solid
C     angle into which the radiation flows, the final received system
C     light would be multiplied by pi*Rec^2/(4*pi*D^2), where Rec is the
C     radius of the receiver and D is the distance to the system, taken
C     outside the earth's atmosphere. If there is interstellar 
C     extinction, a correction would need to be applied. 
						SUM=SUM+VOXFLX*PHFN
					END DO	  !DO J=KST,KND
				END DO	 !DO ITH=1,MTH
C     Enter total scattered radiation from star, for that voxel,
C     into storage array.
				VFLXB(KKK,LL,LHH,1)=SUM
			END DO	  !DO LHH=1,LMTLH
		END DO	  !DO LL=1,NSGAN-1
	END DO	!DO KKK=1,NRINGLMT
C
C     Now do the second scattering region.
C
	DO KKK=1,NRINGLMT
		DO LL=1,NSGAN-1
			LMTLH=NVDAN(KKK)-1
			DO LHH=1,LMTLH
				X2=CX(KKK,LL,LHH,2)
				Y2=CY(KKK,LL,LHH,2)
				Z2=CZ(KKK,LL,LHH,2)
C     Initialize buffer to accumulate scattered radiation.
				SUM=0.D0
C     Range over all points on photosphere of eclipsing star.
				DO ITH=1,MTH
					CALL IDW(I,ITH,KST)
					N=MPH(I,ITH)
					KND=KST+N-1
					DO J=KST,KND
C     Determine direction cosines of vector from given photospheric
C     point on eclipsing star to the particular voxel center.
						X1=XECPJ(J)
						Y1=YECPJ(J)
						Z1=ZECPJ(J)
						DIST=DSQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
						VLVEC=(X2-X1)/DIST
						VMVEC=(Y2-Y1)/DIST
						VNVEC=(Z2-Z1)/DIST
C     Determine distance of starting point from center of eclipsing star.
						CENDS=DSQRT((X1-XCENEC)**2+(Y1-YCENEC)**2+
     $							(Z1-ZCENEC)**2)
C     Calculate the
C     incident flux from the segment of the photosphere.
						CALL SYOBK(I,ITH,J,MTH,KBK,KTH)
						CALL SYO(I,ITH,KST,NTH,JP,ITHP,ISG,VPH1)
						IF (J.GT.KST) THEN
							CALL SYO(I,ITH,KST+1,NTH,JP,ITHP,ISG,
     $								VPH2)
						ELSE
							CALL SYO(I,ITH,KST,NTH,JP,ITHP,ISG,
     $								VPH2)
						END IF
						DPHI=VPH2-VPH1
						CALL SYP(I,ITH,NTH,VTH2)
						IF (ITH.GT.1) THEN
							CALL SYP(I,ITH-1,NTH,VTH1)
						ELSE
							CALL SYP(I,ITH,NTH,VTH1)
						END IF
						DTH=VTH2-VTH1
						SNTH=DSIN(VTH2)
C     Extract the normal intensity
						VIVL=BDI(I,KBK)
C     Get the first and second order limb darkening coeffs.
						VU1=U1(I,KBK)
						VU2=U2(I,KBK)
C     Calculate the cosine of the angle between the surface normal 
C     and the vector toward the voxel.
						CSGM=ZL(I,KBK)*VLVEC+ZM(I,KBK)*VMVEC+
     $							ZN(I,KBK)*VNVEC
C     Calculate the intensity of radiation in the pencil.
C     Suppress any case in which CSGM is negative.
						IF (CSGM.GE.0.D0) THEN
						VIILUM=VIVL*(1.D0-VU1-VU2+VU1*CSGM+
     $							VU2*CSGM**2)*CSGM*SNTH*DTH*DPHI*
     $							CENDS**2/DIST**2/4.D0/PI
						END IF
C     Calculate product of VIILUM, number of electrons in voxel, and 
C     Thomson scattering coefficient.
						VOXFLX=VIILUM*SGTM*VNMEL(KKK,LL,LHH,2)
C     Now calculate cosine of scattering angle.
						COSSCAT=VL0*VLVEC+VM0*VMVEC+VN0*VNVEC
C     Calculate phase function
						PHFN=(1.D0+COSSCAT**2)
C     Calculate and accumulate radiation scattered
C     into the line of sight (per unit solid angle) from the particular
C     photospheric segments and from the specific voxel.
						SUM=SUM+VOXFLX*PHFN 																		          							
					END DO	 !DO J=KST,KND
				END DO	!DO ITH=1,MTH
C     Enter total scattered radiation from star, for that voxel,
C     into storage array.
				VFLXB(KKK,LL,LHH,2)=SUM
			END DO	 !DO LHH=1,LMTLH
		END DO	 !DO LL=1,NSGAN-1
	END DO	!DO KKK=1,NRINGLMT
C     Now sum the scattered light contributions from all voxels.
C     SMA4A is the total scattered light for scatt.region 1
C     SMA4B is the corresponding scattered light for region 2
C     SMB4A is the scattered light, region 1, not blocked by the
C     accretion disk.
C     SMB4B is the corresponding quantity for region 2.
C     SMC4A is the scattered light, region 1, blocked by the companion.
C     SMC4B is the corresponding quantity for region 2.
C     SMD4A is the scattered light visible to the observer, region 1
C     SMD4B is the corresponding quantity for region 2.
      SMA4A=0.D0
	SMA4B=0.D0
	SMB4A=0.D0
	SMB4B=0.D0
	SMC4A=0.D0
	SMC4B=0.D0
	SMD4A=0.D0
	SMD4B=0.D0
	DO KKK=1,NRINGLMT
		DO LL=1,NSGAN-1
			IF(KKK.LE.NRING) THEN
				LMTLH=NVDAN(KKK)-1
			ELSE
				LMTLH=NVDAN(KKK-1)-1
			END IF
			DO LHH=1,LMTLH
C     Calculate total scattered light, separately for the two scattering
C     regions.
				SMA4A=SMA4A+VFLXB(KKK,LL,LHH,1)
				SMA4B=SMA4B+VFLXB(KKK,LL,LHH,2)
C     Calculate scattered light not blocked by accretion disk.
C     If the accretion disk blocks a given voxel, the visisibility key
C     is set to -2.0. Consequently, a visibility key .GE. -1.0 tags a
C     voxel as not blocked by the accretion disk. It may or may not be
C     blocked by the companion.
				IF (VSBKY(KKK,LL,LHH,1).GE.-1.D0) THEN
					SMB4A=SMB4A+VFLXB(KKK,LL,LHH,1)
				END IF
				IF (VSBKY(KKK,LL,LHH,2).GE.-1.D0) THEN
					SMB4B=SMB4B+VFLXB(KKK,LL,LH,2)
				END IF
C     Calculate the scattered light blocked by the companion.
C     If the visibility key is negative, it is blocked from view. If the
C     visibility key is .GE.-1.0, it is not blocked by the accretion 
C     disk, so it must be blocked by the companion.
				IF ((VSBKY(KKK,LL,LHH,1).GE.-1.D0).AND.
     $				(VSBKY(KKK,LL,LHH,1).LT.0.D0)) THEN
					SMC4A=SMC4A+VFLXB(KKK,LL,LHH,1)
				END IF
				IF ((VSBKY(KKK,LL,LHH,2).GE.-1.D0).AND.
     $				(VSBKY(KKK,LL,LHH,2).LT.0.D0)) THEN
					SMC4B=SMC4B+VFLXB(KKK,LL,LHH,2)
				END IF
C     Calculate scattered light visible to observer.
C     If the visibility key .GE.0.0, the voxel is visible.
				IF (VSBKY(KKK,LL,LHH,1).GE.0.D0) THEN
					SMD4A=SMD4A+VFLXB(KKK,LL,LHH,1)
				END IF
				IF (VSBKY(KKK,LL,LHH,2).GE.0.D0) THEN
					SMD4B=SMD4B+VFLXB(KKK,LL,LHH,2)
				END IF
			END DO
		END DO
	END DO
C     Calculate the total scattered light from the two scattering
C     regions. These quantities will be added to other sources of
C     scattered radiation at the end of the program.
	SMA4=SMA4A+SMA4B
	SMB4=SMB4A+SMB4B
	SMC4=SMC4A+SMC4B
	SMD4=SMD4A+SMD4B	
      END IF
C     ******************
C     ******************
C
C     End of program section under control of KEY4
C     This program section has made no changes in the stored coods.
C     of any quantity.						      
C	
C
C     ******************
C     Start of KEY5 section
C     ******************
C
C     Note that, at the start of this section, CXP(), CYP(), CZP(), 
c     XANP(), YANP(), ZANP() are relative to the system center of mass.
C
C     ******************
   50 IF (KEY5.NE.0) THEN
C     ******************
C     Determine next procedure from value of KEY5A.
      IF (KEY5A.EQ.1) THEN
C     Input the list of wavelengths. The wavelengths are in microns.
C     These are the wavelengths that are fiducial values for 
C     calculation of bremsstrahlung.
		READ (IN16,106) NWLA
C     Don't exceed array limit.
		IF (NWLA.GT.100) CALL PMDSTOP
		READ (IN16, 119) (WVLST(JWL), JWL=1,NWLA)
	DO JWL=1,NWLA
C     WVLST is in microns.
	FQLST(JWL)=2.997925D14/WVLST(JWL)
	END DO
	END IF
	IF (KEY5A.NE.1) THEN
		NWLA=1
		WVLST(1)=WL(IWL)
	END IF
C     Initialize visibility keys and flux storage arrays.
      DO JVER=1,NVER
		DO JHOR=1,NHOR+1
			VSBBRE(JVER,JHOR,1)=0.D0
			VSBBRE(JVER,JHOR,2)=0.D0
			DO JWL=1,NWLA
				FXBRE(JWL,JVER,JHOR,1)=0.D0
				FXBRE(JWL,JVER,JHOR,2)=0.D0
			END DO
		END DO
	END DO	  
C
C     Determine inverse of projection array, to locate starting points
C     in cood. system tied to center of accretion disk.
      DO JJ=1,3
		DO KK=1,3
			TARYP(KK,JJ)=TARY(JJ,KK)
		END DO
	END DO
C     Determine boundaries of screen on plane of sky that extends to
C     projected limits of coronal regions. The plane of the sky
C     is represented by the X-Y coods.
C     
      XMXV=-2.D0
	XMNV=2.D0
	YMXV=-2.D0
	YMNV=2.D0
	DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
				DO LH=1,LMT
					IF (CXP(K,L,LH,2).GT.XMXV) XMXV=CXP(K,L,LH,2)
					IF (CXP(K,L,LH,1).LT.XMNV) XMNV=CXP(K,L,LH,1)
					IF (CYP(K,L,LH,2).GT.YMXV) YMXV=CYP(K,L,LH,2)
					IF (CYP(K,L,LH,1).LT.YMNV) YMNV=CYP(K,L,LH,1)
				END DO
		END DO
	END DO
C     Determine boundaries for vectors, in Z direction. The Z direction
C     is toward the observer.
      ZMNV=2.D0
	ZMXV=-2.D0
	DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			DO LH=1,LMT
				IF (CZP(K,L,LH,2).GT.ZMXV) ZMXV=CZP(K,L,LH,2)
				IF (CZP(K,L,LH,2).LT.ZMNV) ZMNV=CZP(K,L,LH,2)
				IF (CZP(K,L,LH,1).GT.ZMXV) ZMXV=CZP(K,L,LH,1)
				IF (CZP(K,L,LH,1).LT.ZMNV) ZMNV=CZP(K,L,LH,1)
			END DO
		END DO
	END DO
C     Set step length in Z direction.	Note that the step length is
C     in units of the component separation.
      ZSTP=1.05*(ZMXV-ZMNV)/NSTEP
	ZMNV=1.01*ZMNV
C     Calculate step length in cgs units.
      ZSTPCGS=ZSTP*SEPCM
C     Determine corresponding boundaries of accretion disk rim.
C     The region 2 X limits are XMNV and XLL, already available.
C     The region 2 Y limits are YLL and YUL, already determined.
C     The region 1 X limits are XUL and XMXV, already available.
C     The region 1 Y limits are the same as for region 2.
C     Note that these coordinates are center-of-mass coods. at 
C     this point.
C
C     Set increment in X direction for starting points of vectors
C     directed toward observer, through the corona, from the screen.
C     ******NOTE******
C     We are dealing with two coordinate systems in the following.
C     One is the "plane of the sky" coordinates. In this system,
C     the X and Y coods. are on the plane of the sky, and the Z cood.
C     is perpendicular to the plane of the sky and is directed toward
C     the observer. The X cood. is measured positive DOWNWARD, as you
C     view the plane of the sky. The Y cood. is measured horizontally.
C     The other cood. system is tied to the components. The x direction
C     is along the line of centers. The z direction is perpendicular to
C     the orbital plane, and the y direction is in the orbital plane,
C     and makes a right-hand cood. system with the x and z axes.
C
C     Because of the symmetry of the coronal regions relative to the
C     accretion disk, the coronal region divides into two regions, one
C     above and the other beloe the accretion disk.
C     Do region 2 first. Region 2 is the region with positive X coods.
C     It is the region "below" the accretion disk, and is the one
C     adjacent to the visible face of the accretion disk, as seen by
C     the observer.
C
C     NOTE: In the cood. system tied to the components, region 2
C     corresponds with the positive side of the z axis.
C
      XRG=XLL-XMNV
	DLX=XRG/(NVER-1)
C     Set increment in Y direction for starting points, as above.
      YRG=YUL-YLL
	DLY=YRG/(NHOR-1)
C     Set starting point of scan over region 2.
      XST=XMNV-DLX/2.D0
	YST=YLL-DLY/2.D0
C     Calculate area of projected segment.
	DA1=DLX*DLY*SEPCM**2
C     Set index ranges
      IF (KTST.EQ.0) THEN
		JVERST=1
		JVERND=NVER
		JHORST=1
		JHORND=NHOR+1
	ELSE
		JHORST=JINDX
		JHORND=JINDX
		IF (KINDX.LT.100) THEN
			JVERST=KINDX
			JVERND=KINDX
		ELSE
			JVERST=KINDX-100
			JVERND=KINDX-100
		END IF
	END IF
C     **********************************************************
C     Initialize array containing line-of-sight intensities.
	DO JJJ=1,NSTEP
		DO KKK=1,NWLA
			VLIZ(KKK,JJJ)=0.D0
		END DO
	END DO
C     Range over vertical starting points
      DO JVER=JVERST,JVERND
		XCURR=XST+(JVER-1)*DLX
C     Save coordinates.
		BREX(JVER,2)=XCURR
C     Range over horizontal starting points, for each vertical
C     starting point.	This scan takes us by equal steps over the
C     "plane of the sky" region that covers the projection of the
C     coronal regions on the plane of the sky. Note that the coods.
C     of the starting points for the vectors are X and Y values.
          DO 305 JHOR=JHORST,JHORND
  			TAUV=0.D0
			YCURR=YST+(JHOR-1)*DLY
C     Save coordinates.
			BREY(JHOR,2)=YCURR
C     For each starting point, begin behind screen, located at ZMNV.
			ZCURR=ZMNV-2.D0*ZSTP
C     Determine corresponding point in cood. system tied to center
C     of accretion disk.
			CALL PROJ(TARYP,XCURR,YCURR,ZCURR,VXLOC,VYLOC,VHLOC)
C     Do a complete integration of the equation of transfer through
C     the coronal regions.
  			DO JZDIR=1,NSTEP
C     Step through full range, from ZMNV to ZMXV, for each starting
C     point on screen.
				VXLOC=VXLOC+VL0*ZSTP
				VYLOC=VYLOC+VM0*ZSTP
				VHLOC=VHLOC+VN0*ZSTP
				ZCURR=ZCURR+ZSTP
				IF (ZCURR.GT.ZMXV) GO TO 3001
C     Calculate distance from origin at center of accretion disk,and
C     in orbital plane.
				VRVL=DSQRT(VXLOC**2+VYLOC**2)
C     Compare this distance with outer radius of accretion disk rim.
C     Continue stepping until the current location reaches outer
C     radius of accretion disk rim.
				IF (VRVL.GT.RGRD(NRING)) GO TO 300
C     We are within the coronal region radial limits. Note that the 
C     temperature and 
C     electron density are constant within annuli, azimuthally, so it 
C     is unnecessary
C     to determine the azimuthal index.
C     Determine value of local temperature and electron density.
				DO KKK=2,NRINGLMT
					IF (VRVL.LE.RGRD(1)) THEN
						KRF=1
						GO TO 301
					END IF
					IF ((VRVL.GT.RGRD(KKK-1)).AND.(VRVL.LE.RGRD(KKK)))
     $					GO TO 302
   				END DO      
  301				KKK=KRF
  302				IF (KKK.LE.NRING) THEN
					LMT=NVDAN(KKK)-1
				ELSE
					LMT=NVDAN(KKK-1)-1
				END IF
C     We have located the enclosing annulus. Now must locate the plate
C     within the annulus.	Temperature and electron density can vary
C     from one plate to another. Note that the coods. VXLOC, VYLOC,
C     VHLOC, are in the cood. system tied to the components. This means
C     that, in the comparison with CZ(), which is also in the cood.
C     system tied to the components, we must use 1 in the fourth
C     CZ parameter.
				DO LHH=1,LMT-1
					IF (VHLOC.LE.CZ(KKK,1,1,1)) GO TO 300
					IF ((VHLOC.GT.CZ(KKK,1,LHH,1)).AND.
     $				(VHLOC.LE.CZ(KKK,1,LHH+1,1))) GO TO 303
				END DO
				GO TO 300
C     If we exit the DO loop, the point lies outside the vertical extent
C     of the corona.
C     If we reach label 303, we are within the corona. Solve equation 
C     of transfer.
  303				TMPLOC=TCOR(KKK,1,LHH,2)
				ELDLOC=DENSEL(KKK,1,LHH,2)
C     Range over all frequencies.	
  				DO JWL=1, NWLA
C     Calculate Gaunt factor
				IF (FQLST(JWL).EQ.0.D0) 
     $				FQLST(JWL)=2.997925D14/WVLST(JWL)
				IF (FQLST(JWL).GT.1.D13) THEN
					GAUNT=1.D0
				ELSE
					GAUNT=10.6+1.9D0*LOG10(TMPLOC)
     $					-1.26D0*LOG10(FQLST(JWL))
				END IF
C     The WVLST(JWL) entries are in microns.
				WVL=WVLST(JWL)*1.D-4
				EXPARG=-1.43883/TMPLOC/WVL
				ARG2=1.D0-DEXP(EXPARG)
C     Calculate terms in equation of transfer. The current intensity
C     at the tabular frequencies is in array FXBRE(JFQ,JVER,JHOR,2)
C     Calculate the free-free absorption coefficient.
				VKLAM=1.37D-23*ARG2*WVL**3*ELDLOC**2
     $				/DSQRT(TMPLOC)*GAUNT
C     Add bf absorption, for Balmer continuum.
				VKLAM=VKLAM+3.2649D-4*ARG2*WVL**3*DENSN2(KKK,1,LHH,1)
C     Calculate total absorption coeff.
C                 chi=k+sigma(e)*n(e)
C
				VCHI=VKLAM+SGTM*ELDLOC
C     Calculate B(T). The entries in	WVLST(JWL) are in microns, as
C     required by SRT BBDY. The temperature must be in thousands of 
C     degrees
				CALL BBDY(TMPLOC/1.D3,WVLST(JWL),VIBB)
C     The output of BBDY is for a wavelength interval of one micron.
C     Reexpress for a wavelength of one cm.
      			VIBB=VIBB*1.D4
C     Approximate mean intensity as mean of intens. at entrance to 
C     current step and end of current step.
C     Iterate 4 times
				DO JJJ=1,4
				VJ=(VLIZ(JWL,JZDIR)+VLIZ(JWL,JZDIR+1))/2.D0
C     Calculate the contribution to the intensity.
C                 eta=k*B(T)+sigma(e)*n(e)*J
C
				VETA=VKLAM*VIBB+SGTM*ELDLOC*VJ
C     Calculate intensity increment.
C                 dI=(eta-chi*I)*dl
C
				DVLIZ=(VETA-VCHI*VLIZ(JWL,JZDIR))*ZSTPCGS
C     Calculate projected intensity at end of current step.
C                 I(J+1)=I(J)+dI(J)
				VLIZ(JWL,JZDIR+1)=VLIZ(JWL,JZDIR)+DVLIZ
C     End of iteration loop.
				END DO
C     Accumulate intensity increments in FXBRE.
C     Express for spectral interval of one angstrom.
				FXBRE(JWL,JVER,JHOR,2)=FXBRE(JWL,JVER,JHOR,2)+
     $				DVLIZ/1.D8
				END DO  ! End of DO JWL... loop
				JWL=JWL-1
C     Write output for plot of intensity as fn. of distance into corona.
				IF ((JWL.EQ.1).AND.(KTST.EQ.1)) THEN
				IF (KINDX.LT.100) THEN
					TAUV=TAUV+ZSTPCGS*VKLAM
					TERM1=VETA*ZSTPCGS
					TERM2=SGTM*ELDLOC*VJ*ZSTPCGS
					TERM3=VCHI*VLIZ(JWL,JZDIR)*ZSTPCGS
					TERM4=SGTM*ELDLOC*VLIZ(JWL,JZDIR)*ZSTPCGS
					WRITE (NOUT6,124) ZCURR,VLIZ(JWL,JZDIR)/1.D8,
     $				TERM1/1.D8,TERM2/1.D8,TERM3/1.D8,
     $				TERM4/1.D8,DVLIZ/1.D8,TAUV,ELDLOC,TMPLOC
				END IF
				END IF
C     Save current JZDIR as possible exit from corona indicator.
				JZDIRSV=JZDIR+1
C     Now step the value of ZCURR by the step width just completed
				ZCURR=ZCURR+ZSTP
				IF (ZCURR.GE.ZMXV) THEN
					GO TO 3001
				END IF
  300			END DO !End of DO JZDIR .... loop.
 3001		CONTINUE
  305		END DO	! End of JHOR loop
	END DO		! End of JVER loop
C
C     ***********************************
C     End of region 2 calculation
C     ***********************************
C
C     Now do region 1. Region 1 is the region with negative X coods.
C     It is the region "above" the accretion disk, and is the one
C     opposed to the visible face of the accretion disk.
C
C     NOTE: In the cood. system tied to the components, region 1
C     corresponds with the negative side of the z axis.
C
      XRG=XMXV-XUL
	DLX=XRG/(NVER-1)
C     Set increment in Y direction for starting points, as above.
      YRG=YUL-YLL
	DLY=YRG/(NHOR-1)
C     Set starting point of scan over region 1.
C     Note that the starting point here is below the accretion disk
C     and should not be at XUL-DLX/2.D0.
      XST=XUL+DLX/2.D0
	YST=YLL-DLY/2.D0
C     Calculate area of projected segment.
	DA2=DLX*DLY*SEPCM**2
C     Initialize array containing line-of-sight intensities.
	DO JJJ=1,NSTEP
		DO KKK=1,NWLA
			VLIZ(KKK,JJJ)=0.D0
		END DO
	END DO
C     Range over vertical starting points
      DO JVER=JVERST,JVERND
		XCURR=XST+(JVER-1)*DLX
C     Save coordinates.
		BREX(JVER,1)=XCURR
C     Range over horizontal starting points, for each vertical
C     starting point.	This scan takes us by equal steps over the
C     "plane of the sky" region that covers the projection of the
C     coronal regions on the plane of the sky. Note that the coods.
C     of the starting points for the vectors are X and Y values.
		DO JHOR=JHORST,JHORND
			TAUV=0.D0
			YCURR=YST+(JHOR-1)*DLY
C     Save coordinates.
			BREY(JHOR,1)=YCURR
C     For each starting point, begin behind screen, located at ZMNV.
			ZCURR=ZMNV-2.D0*ZSTP
C     Determine corresponding point in cood. system tied to center
C     of accretion disk.
			CALL PROJ(TARYP,XCURR,YCURR,ZCURR,VXLOC,VYLOC,VHLOC)
C     Do a complete integration of the equation of transfer through
C     the coronal regions.
     			DO JZDIR=1,NSTEP
C     Step through full range, from ZMNV to ZMXV, for each starting
C     point on screen.
				VXLOC=VXLOC+VL0*ZSTP
				VYLOC=VYLOC+VM0*ZSTP
				VHLOC=VHLOC+VN0*ZSTP
				ZCURR=ZCURR+ZSTP
				IF (ZCURR.GT.ZMXV) GO TO 3010
C     Calculate distance from origin at center of accretion disk,
C     in orbital plane.
				VRVL=DSQRT(VXLOC**2+VYLOC**2)
C     Compare this distance with outer radius of accretion disk rim.
C     Continue stepping until the current location reaches outer
C     radius of accretion disk rim.
				IF (VRVL.GT.RGRD(NRING)) GO TO 310
C     We are within the coronal region radial limits. Note that the 
C     temperature and 
C     electron density are constant within annuli, azimuthally, so it 
C     is unnecessary
C     to determine the azimuthal index.
C     Determine value of local temperature and electron density.
				DO KKK=2,NRINGLMT
					IF (VRVL.LE.RGRD(1)) THEN
						KRF=1
						GO TO 311
					END IF
					IF ((VRVL.GT.RGRD(KKK-1)).AND.(VRVL.LE.RGRD(KKK)))
     $					GO TO 312
   				END DO      
  311				KKK=KRF
  312				IF (KKK.LE.NRING) THEN
					LMT=NVDAN(KKK)-1
				ELSE
					LMT=NVDAN(KKK-1)-1
				END IF
C     We have located the enclosing annulus. Now must locate the plate
C     within the annulus.	Temperature and electron density can vary
C     from one plate to another. Note that the coods. VXLOC, VYLOC,
C     VHLOC, are in the cood. system tied to the components. This means
C     that, in the comparison with CZ(), which is also in the cood.
C     system tied to the components, we must use 2 in the fourth
C     CZ parameter. Note that the values of CZ() in the following test
C     are negative.
				DO LHH=1,LMT-1
					IF (VHLOC.GE.CZ(KKK,1,1,2)) GO TO 310
					IF ((VHLOC.LT.CZ(KKK,1,LHH,2)).AND.
     $				(VHLOC.GE.CZ(KKK,1,LHH+1,2))) GO TO 313
				END DO
				GO TO 310
C     If we exit the DO loop, the point lies outside the vertical extent
C     of the corona.
C     If we reach label 313, we are within the corona. Solve equation 
C     of transfer.
  313				TMPLOC=TCOR(KKK,1,LHH,1)
				ELDLOC=DENSEL(KKK,1,LHH,1)
C     Range over all frequencies
				DO JWL=1,NWLA
C     Calculate Gaunt factor
				IF (FQLST(JWL).EQ.0.D0) 
     $				FQLST(JWL)=2.997925D14/WVLST(JWL)
				IF (FQLST(JWL).GT.1.D13) THEN
					GAUNT=1.D0
				ELSE
					GAUNT=10.6+1.9D0*LOG10(TMPLOC)
     $					-1.26D0*LOG10(FQLST(JWL))
				END IF
C     Calculate terms in equation of transfer. The current intensity
C     at the tabular frequencies is in array FXBRE(JFQ,JVER,JHOR,1)
C     The entries in WVLST(JWL) are in microns.
				WVL=WVLST(JWL)*1.D-4
				EXPARG=-1.43883/TMPLOC/WVL
				ARG2=1.D0-DEXP(EXPARG)
C     Calculate the free-free absorption coefficient.
				VKLAM=1.37D-23*ARG2*WVL**3*ELDLOC**2
     $					/DSQRT(TMPLOC)*GAUNT
C     Add bf absorption, for Balmer continuum.
				VKLAM=VKLAM+3.2649D-4*ARG2*WVL**3*DENSN2(KKK,1,LHH,2)
C     Calculate total absorption coeff.
C                 chi=k+sigma(e)*n(e)
C
				VCHI=VKLAM+SGTM*ELDLOC
C     Calculate B(T)
				CALL BBDY(TMPLOC/1.D3,WVLST(JWL),VIBB)
C     The output of BBDY is for a wavelength interval of one micron.
C     Reexpress for a wavelength of one cm.
      			VIBB=VIBB*1.D4
C     Approximate mean intensity as mean of intens. at entrance to 
C     current step and end of current step.
C     Iterate 4 times
				DO JJJ=1,4
				VJ=(VLIZ(JWL,JZDIR)+VLIZ(JWL,JZDIR+1))/2.D0
C     Calculate the contribution to the intensity.
C                 eta=k*B(T)+sigma(e)*n(e)*J
C
				VETA=VKLAM*VIBB+SGTM*ELDLOC*VJ
C     Calculate intensity increment.
C                 dI=(eta-chi*I)*dl
C
				DVLIZ=(VETA-VCHI*VLIZ(JWL,JZDIR))*ZSTPCGS
C     Calculate projected intensity at end of current step.
C                 I(J+1)=I(J)+dI(J)
				VLIZ(JWL,JZDIR+1)=VLIZ(JWL,JZDIR)+DVLIZ
C     End of iteration loop.
				END DO
C     Accumulate intensity increments in FXBRE
				FXBRE(JWL,JVER,JHOR,1)=FXBRE(JWL,JVER,JHOR,1)+
     $				DVLIZ/1.D8
				END DO	 ! End of DO JWL... loop
				JWL=JWL-1
C     Write output for plot of intensity as fn. of distance into corona.
				IF ((JWL.EQ.1).AND.(KTST.EQ.1)) THEN
				IF (KINDX.GT.100) THEN
					TAUV=TAUV+ZSTPCGS*VKLAM
					TERM1=VETA*ZSTPCGS
					TERM2=SGTM*ELDLOC*VJ*ZSTPCGS
					TERM3=VCHI*VLIZ(JWL,JZDIR)*ZSTPCGS
					TERM4=SGTM*ELDLOC*VLIZ(JWL,JZDIR)*ZSTPCGS
					WRITE (NOUT6,124) ZCURR,VLIZ(JWL,JZDIR)/1.D8,
     $				TERM1/1.D8,TERM2/1.D8,TERM3/1.D8,
     $				TERM4/1.D8,DVLIZ/1.D8,TAUV,ELDLOC,TMPLOC
				END IF
				END IF
C     Save current JZDIR as possible exit value from corona.
				JZDIRSV=JZDIR+1
C     Now step the value of ZCURR by the step width just completed.
				ZCURR=ZCURR+ZSTP
				IF (ZCURR.GE.ZMXV) THEN
					GO TO 3010
				END IF
  310			END DO !End of DO JZDIR .... loop.
 3010			CONTINUE
		END DO
	END DO
C     ***************
C     End of region 1
C     ***************
C
C     Set visibility keys for the area segments on the plane of the sky.
C	The coods. in BREX() and BREY() are relative to the center of the
C     gainer.
C
C     Do region 2.
C
      DO JVER=1,NVER
C     Determine coods. relative to the center of the companion.
		X=BREX(JVER,2)-XG
		DO JHOR=1,NHOR+1
			Y=BREY(JHOR,2)+YG
C     If the eclipsing star is the object with the accretion disk, then
C     none of the corona is eclipsed.
			IF (IEC.EQ.ISTAR) GO TO 320
C     Calculate projected dist. of coronal point from projected center
C     of eclipsing star.
			RV=DSQRT(X**2+Y**2)
C     If distance is greater than maximum projected radius of eclipsing
C     star, the point is not eclipsed. If distance is less than minimum
C     projected radius of eclipsing star, the point is surely eclipsed.
			IF ((RV.LT.RMN).AND.(VSBBRE(JVER,JHOR,2).NE.-2.D0))	THEN
      			VSBBRE(JVER,JHOR,2)=-1.D0
				GO TO 320
			END IF
C     If distance is intermediate, must check in greater detail.
			IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 320
C     Do detailed check, using position angle data.
			ANGL=DASIN(DABS(X/RV))
C     Set correct quadrant.
			IF (Y.LT.0.D0) ANGL=PI-ANGL
			IF (X.LT.0.D0) ANGL=-ANGL
			IF (ANGL.LT.0.D0) GO TO 315
C     In range covered by IFCS
		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,
     $	RW)
		GO TO 316
C     In range covered by IRCS
  315		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,
     $	RW)
C     The projected distance of the coronal point from the center of the
C     eclipsing component is RV, and the position angle is ANG.
C     The projected radius of the eclipsing component at the same
C     position angle is RW.
  316		ER=RV-RW
C     Set visibility key.
		IF ((ER.LT.0.D0).AND.(VSBBRE(JVER,JHOR,2).NE.-2.D0))
     $		VSBBRE(JVER,JHOR,2)=ER  	         
  320		CONTINUE
		END DO
	END DO 
C
C     ****************
C     End of region 2
C     ****************
C     
C     Do region 1.
C
      DO JVER=1,NVER
C     Determine coods. relative to the center of the companion.
		X=BREX(JVER,1)-XG
		DO JHOR=1,NHOR+1
			Y=BREY(JHOR,1)+YG
C     If the eclipsing star is the object with the accretion disk, then
C     none of the corona is eclipsed.
			IF (IEC.EQ.ISTAR) GO TO 330
C     Calculate projected dist. of coronal point from projected center
C     of eclipsing star.
			RV=DSQRT(X**2+Y**2)
C     If distance is greater than maximum projected radius of eclipsing
C     star, the point is not eclipsed. If distance is less than minimum
C     projected radius of eclipsing star, the point is surely eclipsed.
			IF ((RV.LT.RMN).AND.(VSBBRE(JVER,JHOR,1).NE.-2.D0))	THEN
      			VSBBRE(JVER,JHOR,1)=-1.D0
				GO TO 330
			END IF
C     If distance is intermediate, must check in greater detail.
			IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 330
C     Do detailed check, using position angle data.
			ANGL=DASIN(DABS(X/RV))
C     Set correct quadrant.
			IF (Y.LT.0.D0) ANGL=PI-ANGL
			IF (X.LT.0.D0) ANGL=-ANGL
			IF (ANGL.LT.0.D0) GO TO 325
C     In range covered by IFCS
		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,
     $	RW)
		GO TO 326
C     In range covered by IRCS
  325		CALL RHOV(ANGL,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,
     $	RW)
C     The projected distance of the coronal point from the center of the
C     eclipsing component is RV, and the position angle is ANG.
C     The projected radius of the eclipsing component at the same
C     position angle is RW.
  326		ER=RV-RW
C     Set visibility key.
		IF ((ER.LT.0.D0).AND.(VSBBRE(JVER,JHOR,1).NE.-2.D0))
     $		VSBBRE(JVER,JHOR,1)=ER  	         
  330		CONTINUE
		END DO
	END DO
C	
C     ***************
C     End of region 1	 
C     ***************
C  
C
      END IF
C
C     ***********************
C     End of KEY5 section
C     ***********************	
C
C     Write output for plotting.
C     Transform to center of mass coordinates.
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				IF (ISTAR.EQ.ISV) THEN
					CXP(K,L,LH,1)=CXP(K,L,LH,1)+DX
					CYP(K,L,LH,1)=CYP(K,L,LH,1)-DY
					CXP(K,L,LH,2)=CXP(K,L,LH,2)+DX
					CYP(K,L,LH,2)=CYP(K,L,LH,2)-DY
					CZP(K,L,LH,1)=CZP(K,L,LH,1)-DZ
					CZP(K,L,LH,2)=CZP(K,L,LH,2)-DZ
					XANP(K,L,LH,1)=XANP(K,L,LH,1)+DX
					YANP(K,L,LH,1)=YANP(K,L,LH,1)-DY
					XANP(K,L,LH,2)=XANP(K,L,LH,2)+DX
					YANP(K,L,LH,2)=YANP(K,L,LH,2)-DY
					ZANP(K,L,LH,1)=ZANP(K,L,LH,1)-DZ
					ZANP(K,L,LH,2)=ZANP(K,L,LH,2)-DZ
				ELSE
					CXP(K,L,LH,1)=CXP(K,L,LH,1)+DDX
					CYP(K,L,LH,1)=CYP(K,L,LH,1)-DDY
					CXP(K,L,LH,2)=CXP(K,L,LH,2)+DDX
					CYP(K,L,LH,2)=CYP(K,L,LH,2)-DDY
					CZP(K,L,LH,1)=CZP(K,L,LH,1)-DDZ
					CZP(K,L,LH,2)=CZP(K,L,LH,2)-DDZ
					XANP(K,L,LH,1)=XANP(K,L,LH,1)+DDX
					YANP(K,L,LH,1)=YANP(K,L,LH,1)-DDY
					XANP(K,L,LH,2)=XANP(K,L,LH,2)+DDX
					YANP(K,L,LH,2)=YANP(K,L,LH,2)-DDY
					ZANP(K,L,LH,1)=ZANP(K,L,LH,1)-DDZ
					ZANP(K,L,LH,2)=ZANP(K,L,LH,2)-DDZ
				END IF
			END	DO
		END DO
	END DO
C
      IF (NPSI.EQ.1) THEN
   	WRITE (NOUT3,106) NRINGLMT,NSGAN-1
	WRITE (26,106) NRINGLMT,NSGAN-1
C***********************
	DO K=1,NRINGLMT
		IF (K.LE.NRING) THEN
			WRITE (NOUT3,106) NVDAN(K)-1
			LMT=NVDAN(K)-1
	WRITE (26,106) LMT
		ELSE
			WRITE (NOUT3,106) NVDAN(K-1)-1
			LMT=NVDAN(K-1)-1
	WRITE (26,106) LMT
		END IF
		DO L=1,NSGAN-1
			WRITE (NOUT3,105) (CXP(K,L,LH,1),LH=1,LMT)
			WRITE (NOUT3,105) (CYP(K,L,LH,1),LH=1,LMT)
			WRITE (NOUT3,105) (CXP(K,L,LH,2),LH=1,LMT)
			WRITE (NOUT3,105) (CYP(K,L,LH,2),LH=1,LMT)
			WRITE (NOUT3,105) (VSBKY(K,L,LH,1),LH=1,LMT)
			WRITE (NOUT3,105) (VSBKY(K,L,LH,2),LH=1,LMT)
	WRITE (26,105) (CXP(K,L,LH,1),LH=1,LMT)
	WRITE (26,105) (CYP(K,L,LH,1),LH=1,LMT)
	WRITE (26,105) (CZP(K,L,LH,1),LH=1,LMT)
	WRITE (26,105) (CXP(K,L,LH,2),LH=1,LMT)
	WRITE (26,105) (CYP(K,L,LH,2),LH=1,LMT)
	WRITE (26,105) (CZP(K,L,LH,2),LH=1,LMT)
	WRITE (26,105) (VSBKY(K,L,LH,1),LH=1,LMT)
	WRITE (26,105) (VSBKY(K,L,LH,2),LH=1,LMT)
		END DO
	END DO
	DO K=1,NRINGLMT
		IF (K.LE.NRING) THEN
			WRITE (NOUT3,106) NVDAN(K)-1
			LMT=NVDAN(K)-1
		ELSE
			WRITE (NOUT3,106) NVDAN(K-1)-1
			LMT=NVDAN(K-1)-1
		END IF
		DO L=1,NSGAN-1
			WRITE (NOUT3,105) (XANP(K,L,LH,1),LH=1,LMT)
			WRITE (NOUT3,105) (YANP(K,L,LH,1),LH=1,LMT)
			WRITE (NOUT3,105) (XANP(K,L,LH,2),LH=1,LMT)
			WRITE (NOUT3,105) (YANP(K,L,LH,2),LH=1,LMT)
		END DO
	END DO
C***********************
	END IF
C     Calculate light curve data.
C     SMA is the total scattered light toward the observer.
C     SMB is the scattered light not blocked by the accretion disk.
C     SMC is the scattered light eclipsed by the companion and not
C     blocked by the accretion disk.
C     SMD is the scattered light not blocked by any object.
C
C     Calculate projected area data
C     SME is the total projected area of voxels
C     SMF is the total projected area of voxels not blocked by
C     the accretion disk.
C     SMG is the total projected area of voxels eclipsed by the
C     companion and not blocked by the accretion disk.
      SMA=0.D0
	SMAA=0.D0
	SMAB=0.D0
	SMB=0.D0
	SMBA=0.D0
	SMBB=0.D0
	SMC=0.D0
	SMCA=0.D0
	SMCB=0.D0
	SMD=0.D0
	SMDA=0.D0
	SMDB=0.D0
	SME=0.D0
	SMEA=0.D0
	SMEB=0.D0
	SMF=0.D0
	SMFA=0.D0
	SMFB=0.D0
	SMG=0.D0
	SMGA=0.D0
	SMGB=0.D0
C***********************
      DO K=1,NRINGLMT
		DO L=1,NSGAN-1
			IF (K.LE.NRING) THEN
				LMT=NVDAN(K)-1
			ELSE
				LMT=NVDAN(K-1)-1
			END IF
			DO LH=1,LMT
				SMAA=SMAA+VFLX(K,L,LH,1)+VFLXA(K,L,LH,1)
				SMAB=SMAB+VFLX(K,L,LH,2)+VFLXA(K,L,LH,2)
				SMEA=SMEA+PJAR(K,L,LH,1)
				SMEB=SMEB+PJAR(K,L,LH,2)
C     Note: If the accretion disk blocks a given voxel, the visibility 
C     key is set to -2.0. Consequently, a visibility key .GE. -1.0 tags
C     a voxel as not blocked by the accretion disk. It may or may not 
C     be blocked by the companion.
				IF (VSBKY(K,L,LH,1).GE.-1.D0) THEN
					SMBA=SMBA+VFLX(K,L,LH,1)+VFLXA(K,L,LH,1)
					SMFA=SMFA+PJAR(K,L,LH,1)
				END IF
				IF (VSBKY(K,L,LH,2).GE.-1.D0) THEN
					SMBB=SMBB+VFLX(K,L,LH,2)+VFLXA(K,L,LH,2)
					SMFB=SMFB+PJAR(K,L,LH,2)
				END IF
C     If the visibility key is negative, it is blocked from view. If the
C     visibility key is .GE. -1.0, it is not blocked by the accretion 
C     disk, so must be blocked by the companion.
				IF ((VSBKY(K,L,LH,1).GE.-1.D0).AND.
     $				(VSBKY(K,L,LH,1).LT.0.D0)) THEN
					SMCA=SMCA+VFLX(K,L,LH,1)+VFLXA(K,L,LH,1)
					SMGA=SMGA+PJAR(K,L,LH,1)
				END IF
				IF ((VSBKY(K,L,LH,2).GE.-1.D0).AND.
     $                (VSBKY(K,L,LH,2).LT.0.D0)) THEN
					SMCB=SMCB+VFLX(K,L,LH,2)+VFLXA(K,L,LH,2)
					SMGB=SMGB+PJAR(K,L,LH,2)
				END IF
C     If the visibility key is .GE. 0.0, it is visible.					
				IF (VSBKY(K,L,LH,1).GE.0.D0) THEN
					SMDA=SMDA+VFLX(K,L,LH,1)+VFLXA(K,L,LH,1)
				END IF
				IF (VSBKY(K,L,LH,2).GE.0.D0) THEN
					SMDB=SMDB+VFLX(K,L,LH,2)+VFLXA(K,L,LH,2)
				END IF
			END DO
		END DO
	END DO
	SMA=SMAA+SMAB
	SMB=SMBA+SMBB
	SMC=SMCA+SMCB
	SMD=SMDA+SMDB
	SMA=SMA+SMA4
	SMB=SMB+SMB4
	SMC=SMC+SMC4
	SMD=SMD+SMD4
	SME=SMEA+SMEB
	SMF=SMFA+SMFB
	SMG=SMGA+SMGB
C***********************
      WRITE (NOUT2,125) IWL, WL(IWL), JINDX, KINDX
	WRITE (NOUT2,114) SMA,SMB,SMC,SMD
	WRITE (NOUT2,106) KLG
	WRITE (NOUT2,101) (VTAUSV(KK),KK=1,KLG)
	WRITE (NOUT2,106) (KIDX(KK),KK=1,KLG)
	WRITE (NOUT1,113) IWL, WL(IWL)
	WRITE (NOUT1,109) SMA
	WRITE (NOUT1,110) SMB
	WRITE (NOUT1,111) SMC
	WRITE (NOUT1,112) SMD
	WRITE (NOUT1,115) SME
	WRITE (NOUT1,118) SMF
	WRITE (NOUT1,116) SME-SMF
	WRITE (NOUT1,117) SMG
	WRITE (NOUT1,107)
 3000	FORMAT (20F4.1)
 2001 FORMAT (8E10.3)
C     Calculate total flux of bremsstrahlung, at wavelengths
C     specified.
      IF (KEY5.EQ.1) THEN
		DO JWL=1,NWLA
			SFXBRE(JWL)=0.D0
			TFXBRE(JWL)=0.D0
			DO JVER=1,NVER
				DO JHOR=1,NHOR+1
C     Accumulate total flux from bremsstrahlung. Convert back to
C     wavelength interval of 1 micron.
					TFXBRE(JWL)=TFXBRE(JWL)+DA1*FXBRE(JWL,JVER,JHOR,2)
     $					*1.D4
C     Accumulate flux not blocked by companion.
					IF (VSBBRE(JVER,JHOR,2).GE.0.D0) THEN
					SFXBRE(JWL)=SFXBRE(JWL)+DA1*FXBRE(JWL,JVER,JHOR,2)
     $					*1.D4
					END IF
					TFXBRE(JWL)=TFXBRE(JWL)+DA2*FXBRE(JWL,JVER,JHOR,1)
     $					*1.D4
					IF (VSBBRE(JVER,JHOR,1).GE.0.D0) THEN
					SFXBRE(JWL)=SFXBRE(JWL)+DA2*FXBRE(JWL,JVER,JHOR,1)
     $					*1.D4
					END IF
				END DO
			END DO
		END DO
C     Write wavelengths in Angstroms
		WRITE (NOUT4,194) (WVLST(JWL)*1.D4,SFXBRE(JWL),TFXBRE(JWL),
     $	JWL=1,NWLA)
C     Write data for plotting flux contours
      	DO JVER=1,NVER
			XVL=BREX(JVER,2)
			DO JHOR=1,NHOR+1
				YVL=BREY(JHOR,2)
				WRITE (NOUT5,123) YVL,XVL,DA1*FXBRE(1,JVER,JHOR,2)
     $				/1.D4,JHOR,JVER
			END DO
		END DO
      	DO JVER=1,NVER
			XVL=BREX(JVER,1)
			DO JHOR=1,NHOR+1
				YVL=BREY(JHOR,1)
				WRITE (NOUT5,123) YVL,XVL,DA2*FXBRE(1,JVER,JHOR,1)
     $				/1.D4,JHOR,JVER
			END DO
		END DO
	END IF
	IF (KEY5A.EQ.0) THEN
		WRITE (NOUT1,120) SFXBRE(1)
		WRITE (NOUT1,121) SMD+SFXBRE(1)
		WRITE (NOUT1,122) SMC+TFXBRE(1)-SFXBRE(1)
	END IF
   60 CONTINUE
      REWIND IN3
	REWIND IN4									
	REWIND IN5
	REWIND IN6
	REWIND IN7
	REWIND IN14
	END DO
    99 CONTINUE
C   99 CALL GETDAT(IYR,IMON,IDAY)
C	CALL GETTIM(IHR,IMIN,ISEC,I100TH)
	TM=3600*IHR+60*IMIN+ISEC
	AD=I100TH
	TM=(100.D0*TM+AD)/100.D0
	AD=IDAY
	DA=AD+TM/86400.D0
	CALL CVJD(DA,IMON,IYR,FVL2)
	TM=(FVL2-FVL1)*1440.D0
	WRITE (NOUT1, 205) TM
C**********************************************************************
      CLOSE (UNIT = 18,    STATUS = 'KEEP')
	CLOSE (UNIT = IN1,   STATUS = 'KEEP')
	CLOSE (UNIT = IN2,   STATUS = 'KEEP')
	CLOSE (UNIT = IN3,   STATUS = 'KEEP')
	CLOSE (UNIT = IN4,   STATUS = 'KEEP')
	CLOSE (UNIT = IN5,   STATUS = 'KEEP')
	CLOSE (UNIT = IN6,   STATUS = 'KEEP')
	CLOSE (UNIT = IN7,   STATUS = 'KEEP')
	CLOSE (UNIT = IN8,   STATUS = 'KEEP')
	CLOSE (UNIT = IN9,   STATUS = 'KEEP')
	CLOSE (UNIT = IN10,  STATUS = 'KEEP')
	CLOSE (UNIT = IN11,  STATUS = 'KEEP')
	CLOSE (UNIT = IN12,  STATUS = 'KEEP')
	CLOSE (UNIT = IN13,  STATUS = 'KEEP')
	CLOSE (UNIT = IN14,  STATUS = 'KEEP')
	CLOSE (UNIT = IN15,  STATUS = 'KEEP')
	CLOSE (UNIT = IN16,  STATUS = 'KEEP')
	CLOSE (UNIT = NOUT1, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT2, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT3, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT4, STATUS = 'KEEP')
	CLOSE (UNIT = NOUT5, STATUS = 'KEEP')
	CLOSE (UNIT = 25, STATUS = 'KEEP')
	CLOSE (UNIT = 26, STATUS = 'KEEP')
	CLOSE (UNIT = 40, STATUS = 'DELETE')
	CLOSE (UNIT = NOUT6, STATUS = 'KEEP')
	STOP
	END PROGRAM ACPGCRG
