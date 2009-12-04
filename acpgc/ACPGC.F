      PROGRAM ACPGC																					   vmnmass
C	USE MSFLIB
C     PROGRAM TO CALCULATE TEMPERATURE DISTRIBUTION AND NORMAL
C     INTENSITY DISTRIBUTION OVER THE SURFACES OF AN ACCRETION DISK
C      
C     ******************
C     Version ACPGC2
C     ******************
C	 
      INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMN.FOR'
      INCLUDE 'COMPH.FOR'
      DIMENSION WL(MWL),TMPRF(MRING,MSG),TMPRR(MRM,MSG),SMV(MRING,MSG),
     $FLXSMSV(MRING),FLX1SV(MRING),FLX2SV(MRING),VINRM(MRING),
     $FRCTRAD(MRING),
     $TMPRFA(MRING,MSG),RMIFLX(MRM,MSG),TMPRFB(MRING,MSG)
	DIMENSION DUMARY(MRING),vradvl(MRING),VMASS(MRING),
     $VLUMIN(MRING),ALPHA(MRING),
     $D(MRING),VC(MRM),FACETMP(MRING,MSG),TMPRFNR(MRING)
      DIMENSION UC(MTW),VC1(MTW),VC2(MTW),T0(MTW),AV(MTW),BCF(MTW),
     $UA(MTW)
	DIMENSION RMTMP(MRM,MSG)
	DIMENSION VINITANG(MRING),VINENDANG(MRING),VNDITANG(MRING)
	DIMENSION VNDENDANG(MRING),TRTMP(MRING)
	DIMENSION KINSTART(MRING),KINEND(MRING),KNDSTART(MRING)
	DIMENSION KNDEND(MRING)
	DIMENSION VELSND(MRING),VLH(MRING),RHO(MRING),PRESS(MRING),
     $VISC(MRING),SGMAV(MRING),TC(MRING),TAU(MRING),TCROSS(MRING)
	DIMENSION U1Q(10,22,14),U2Q(10,22,14),U1SV(10,22,14),
     $U2SV(10,22,14)
      DIMENSION FL1(18)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 28 FL1
      CHARACTER * 7 LABEL
      INTEGER(4) RESULT
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD, KEYBAD
C     CF=6.0/PI,NORMALIZATION FACTOR FOR FIRST AND SECOND ORDER
C     LIMB DARKENING
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
      DATA IN10,IN11,IN12,IN13/10,11,12,13/
      DATA NOUT1,NOUT2,NOUT3,NOUT4/14,15,16,17/
	DATA IN14/18/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
	DATA CKRAMER/6.6D22/		!Coefficient in Kramer opacity equation
      DATA ETA/0.1D0/             !Accretion efficiency for critical accretion
	DATA SUNM/1.9891D33/
  100 FORMAT (A28)
  101 FORMAT (I2)
  102 FORMAT (8F10.2)
  103 FORMAT (' ')
  104 FORMAT (1X,'PROGRAM ACPGC',27X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2)
  105 FORMAT (1X,7E10.3)
  106 FORMAT (14F5.0)
  107 FORMAT (1X,'NUMBER OF WAVELENGTHS OF OBSERVATION=',I5,/,1X,'MODEL  
     $ATMOSPHERE CONTROL=',I5,5X,/,1X,'IF THIS INTEGER=0,USE BLACK BODY
     $APPROXIMATION,OTHERWISE USE MODEL ATMOSPHERE',/,
     $1X,'NUMBER OF REFERENCE WAVELENGTHS FOR DETERMINATION OF LIMB DARK
     $ENING COEFFS.',/,' AND MODEL ATMOSPHERE=',I5,/,
     $1X,'NUMBER OF VALUES OF SURFACE GRAVITY ASSOCIATED WITH MODEL ATMO
     $SPHERE=',I5,/,1X,'NUMBER OF VALUES OF SURFACE TEMPERATURE ASSOCIAT
     $ED WITH MODEL ATMOSPHERE=',I5,/)
  109 FORMAT (8F10.5)
  110 FORMAT (1X,'WAVELENGTHS OF OBSERVATION,IN MICRONS,=',/,1X,8F12.5)
  111 FORMAT (1X,'ARRAY OF FIRST ORDER LIMB DARKENING COEFFS., DISK FACE
     $')
  112 FORMAT (1X,'ARRAY OF SECOND ORDER LIMB DARKENING COEFFS., DISK
     $ FACE')
  113 FORMAT (1X,'RIM TEMPERATURE',20X,'=',F10.5)
  115 FORMAT (1X,'FOLLOWING FACE TEMPERATURES INCLUDE IRRADIATION EFFECT
     $',/,1X,'IF CONTROL KEY KFC HAS BEEN SET, THE FACE TEMPERATURES HAV
     $E BEEN RESET BY AN EXTERNAL FILE')
  116 FORMAT (1X,'DISK FACE TEMPERATURE ARRAY, WITHOUT IRRADIATION')
  117 FORMAT (1X,'INTENSITY NORMAL TO SURFACE OF RIM',/,1X,
     $'IN ERGS/CM.**2/SEC/MICRON/SR AT WAVELENGTH=',F12.5,2X,'MICRONS'
     $)
  118 FORMAT (1X,'ARRAY OF FIRST ORDER LIMB DARKENING COEFFS., RIM')
  119 FORMAT (1X,'ARRAY OF SECOND ORDER LIMB DARKENING COEFFS., RIM')
  122 FORMAT (1X,'RIM TEMPERATURE ARRAY, BY LONGITUDE')
  125 FORMAT (1X,'ARRAY OF NORMAL INTENSITY VALUES FOR DISK FACE ARISING
     $ FROM ENERGY DISSIPATION',/,1X,
     $'AND IRRADIATION, IN ERGS/CM**2/SEC/MICRON/SR AT WAVELENGTH=',
     $F12.5,2X,'MICRONS')
  127 FORMAT (1X,'ARRAY OF GRAVITY VALUES FOR DISK FACE, PERPENDICULAR T
     $O DISK PLANE (CGS)')
  128 FORMAT (1X,'ARRAY OF RIM GRAVITY VALUES PERPENDICULAR TO RIM SURFA
     $CE, (CGS)')
  129 FORMAT (1X,'ARRAY OF INCIDENT FLUX VALUES, DISK FACE (CGS)'/,
     $10X,'(ONLY APPLIES IF USED WITH TLUSDISK)')
  130 FORMAT (1X,'EMERGENT ENERGY FLUX FROM ENERGY DISSIPATION (FACE) (C
     $GS)')
  131 FORMAT (1X,'EMERGENT FLUX FROM IRRADIATION, (FACE) WEIGHTED BY BAL
     $F (CGS)'
     $)
  132 FORMAT (1X,'TOTAL EMERGENT FLUX (FACE) (CGS)')
  133 FORMAT (1X,'TOTAL NORMAL INTENSITY, DISK FACE.,IN ERGS/CM**2/SEC/A
     $NGSTROM/SR')
  134 FORMAT (1X,'(SOLAR SURFACE FLUX = 6.27E10 ERGS/CM**2/SEC)')
  135 FORMAT (1X,'MASS TRANSFER RATE IN M(SUN)E-6/YR=',8X,F14.7)
  136 FORMAT (1X,'EFFECTIVE TEMPERATURE, CENTRAL STAR=',6X,F10.2)
  137 FORMAT (1X,'BOLOMETRIC LIMB DARKENING, CENTRAL STAR=',3X,F10.3)
  138 FORMAT (1X,'BOLOMETRIC ALBEDO, ACCRETION DISK FACE=',4X,F10.3)
  139 FORMAT (1X,'BOLOMETRIC LIMB DARKENING, ACCRETION DISK FACE=',1X,
     $F5.3)
  140 FORMAT (1X,'RADIUS OF CENTRAL STAR=',21X,E15.5,2X,'CM')
  141 FORMAT (1X,'OUTER RADIUS OF ACCRETION DISK=',13X,E15.5,2X,'CM')
  142 FORMAT (1X,'RADII OF DISK RINGS, UNITS OF CENTRAL STAR RADIUS',
     $/,6(1X,8F9.4,/))
  143 FORMAT (1X,'RADII OF DISK RINGS, UNITS OF SOLAR RADIUS',
     $/,6(1X,8F9.4,/))
  144 FORMAT (1X,'RADIUS OF CENTRAL STAR=',22X,F10.5,2X,'SOLAR RADII')
  145 FORMAT (1X,'RADIUS OF ACCRETION DISK OUTER RIM=',10X,F10.5,2X,
     $'SOLAR RADII')
  147 FORMAT (1X,'LOG10 OF GRAVITY VALUES FOR DISK FACE')
  148 FORMAT (1X,'LOG10 OF RIM GRAVITY VALUES PERPENDICULAR TO RIM SURFA
     $CE')         
  154 FORMAT (1X,'Viscosity coefficient',16X,'ALPHAV=',5X,F10.3,/)
  151 FORMAT (1X,'Calculated properties of face rings',/,
     $'ID',' H(sn.rd.)',' TC   ','    RHO   ','   PRESS     ',
     $'SGMAV    ',
     $'   TAU   ','  VELSND  ','   VISC')
  152 FORMAT (I2,F7.3,F9.0,1P,6D10.2)
  153 FORMAT (' ')
  155 FORMAT (1X,'Maximum possible luminosity of accretion disk=',
     $1X,1P,D10.3,1X,'ergs/sec')
  156 FORMAT (1X,'Tidal cutoff radius of accretion disk=',
     $9X,1P,D10.3,1X,'solar radii')
  157 FORMAT (1X,'Total luminosity of accretion disk=',10X,1P,
     $D11.5,2X,'ergs/sec',/,5X,'(Includes rim luminosity and, if KEYBL.E
     $Q.1, boundary layer)')
  158 FORMAT (1X,'Additional properties of face rings',/,
     $'ID','  Drift v.','    Mass ','    Luminos.  ','Cross.T.', 
     $' Alpha/Re')
  159 FORMAT(I2,1P,5D10.2)
  160 FORMAT (1X,'Eddington luminosity=',25X,1P,D10.2,1X,'ergs/sec',
     $/,1X,'Critical accretion rate=',22X,D10.2,1X,'sol.ms./yr')
  162 FORMAT (1X,'Rim temperature array, including irradiation, orbital
     $plane')
  163 FORMAT (1X,'Bolometric albedo, accretion disk rim=',5X,f10.3)
  164 FORMAT (1X,'Bolometric limb darkening, companion star=',4X,
     $F6.2)
  165 FORMAT (1X,'Value of KEY=',35X,I2,/,
     $5X,'If =1, apply irradiation correction, face')
  166 FORMAT (1X,'Value of KYRM=',34X,I2,/,
     $5X,'If =1, apply irradiation correction, rim')
  167 FORMAT (1X,'Value of IHSP=',34X,I2,/,
     $5X,'If =1, there is a hot spot')
  168 FORMAT (1X,'Hot spot central azimuth=',19X,F8.2,2X,'degrees')
  169 FORMAT (1X,'Hot spot Teff=',30X,F8.2)
  170 FORMAT (1X,'Exponent for temp. profile of accret. disk face=',
     $F4.2)
  171 FORMAT (1X,'Hot spot angular width (central portion)=',3X,F8.2,
     $2X,'degrees')
  172 FORMAT (1X,'Semi-height of boundary layer (solar radii)=',4X,
     $F10.7,/,1X,'Thickness of boundary layer (solar radii)=', 6X,F10.7,
     $/,1X,'Radiating area of boundary layer=',15X,1P,
     $D12.6,2X,'cm**2')
  173 FORMAT (1X,'Effective temperature, boundary layer=', 9X,1P,D12.5,
     $2X,'degrees',/,1X,'Luminosity of boundary layer=',18X,1P,
     $D12.5,2X,'ergs/sec')
  174 FORMAT (1X,'Radius of toroidal cross-section=',10X,F10.3,
     $2X,'solar radii')
  175 FORMAT (1X,'Key to include boundary layer luminosity=',7X,I2)
  176 FORMAT (1X,'Luminosity of accretion disk face=',10X,1P,D12.5,
     $2X,'ergs/sec')
  177 FORMAT (1X,'Luminosity of accretion disk rim=',11X,1P,D12.5,
     $2X,'ergs/sec')
  178 FORMAT (1X,'Luminosity of boundary layer=',15X,1P,D12.5,
     $2X,'ergs/sec')
  179 FORMAT (1X,'Hot spot upstream azimuthal boundary=',7X,F8.2,
     $3X,'degrees')
  180 FORMAT (1X,'Hot spot upstream boundary Teff=',8X,F12.2)
  181 FORMAT (1X,'Hot spot downstream azimuthal boundary=',5X,F8.2,
     $2X,'degrees')
  182 FORMAT( 1X,'Hot spot downstream boundary Teff=',6X,F12.2)
  183	FORMAT (1X,'FIRST ORDER STD. MODEL HEIGHTS OF FACE RINGS',/,
     $10X,'UNITS OF COMPONENT SEPARATION',
     $/,6(1X,8F9.4,/))
  184 FORMAT (1X,'FIRST ORDER STD. MODEL HEIGHTS OF FACE RINGS',/, 
     $10X,'UNITS OF SOLAR RADIUS',
     $/,6(1X,8F9.4,/))
  185 FORMAT (1X,'HEIGHTS OF FACE RINGS SPECIFIED BY ACPGA',/,
     $10X,'UNITS OF SOLAR RADIUS',
     $/,6(1X,8F9.4,/))
  186 FORMAT (1X,'Total mass of accretion disk =',18X,D10.4,2X,'solar ma
     $sses')
  187 FORMAT (1X,'Value of KEYBL, control to include boundary layer=',
     $I2)
  189 FORMAT (1X,'Time for parcel to drift across entire acc. disk=',
     $E10.4,2X,'seconds',/)
  190 FORMAT (1X,'Viscosity coefficient=',23X,F12.3)
  200 FORMAT (1P,8D10.2)
  205 FORMAT (1X,'Program running time=',F12.4,2X,'minutes')
C*****************************************************
C     RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=19,    FILE='C:\LGTSYN\ACPGC\ACPGC.ILS',STATUS='OLD')
      READ (19, 100)    (FL1(J),J=1,18)
      OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')
      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')
      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')
      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')
      OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS='OLD')
      OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS='OLD')
      OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS='OLD')
      OPEN (UNIT=IN8,   FILE=FL1(8),  STATUS='OLD')
      OPEN (UNIT=IN9,   FILE=FL1(9),  STATUS='OLD')
	OPEN (UNIT=IN10,  FILE=FL1(10), STATUS='OLD')
      OPEN (UNIT=IN11,  FILE=FL1(11), STATUS='OLD')
	OPEN (UNIT=IN12,  FILE=FL1(12), STATUS='OLD')
	OPEN (UNIT=IN13,  FILE=FL1(13), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(15), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(16), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(17), STATUS='REPLACE')
	OPEN (UNIT=IN14,  FILE=FL1(18), STATUS='OLD')
C*****************************************************
      WRITE (IN14,101) KEYBAD
	CLOSE (UNIT=IN14, STATUS = 'KEEP')
C      CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F1)
C     Read input data
      LABEL = ' *ACPGC'
      NU=IN1
      CALL GTLBL ( LABEL,LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD ( NWL)
      CALL IREAD ( MDAT)
      CALL DUMMY (2)
	CALL IREAD ( IRCT)
	CALL DUMMY (2)
      CALL IREAD ( NRF)
      CALL IREAD ( NGR)
      CALL IREAD ( NTMP)
      DO I=1,NWL
      CALL DREAD ( WL(I),'F10.8')
      END DO
      CALL DUMMY (3)
      CALL DREAD (VMDOT,'F15.12')
      CALL DUMMY (1)
      CALL DREAD (TECS,'F10.7')
      TECS=TECS*1000.D0
      CALL DUMMY (1)
      CALL DREAD (VMU,'F10.8')    !Bolometric limb dark.,central star
      CALL DUMMY (1)
      CALL DREAD (BALF,'F10.8')   !Bolometric albedo, accretion disk
C    	face
      CALL DUMMY (1)
      CALL DREAD (VMUF,'F10.8')   !Bolometric limb dark.,accret. disk
C     face
      CALL DUMMY (1)
	CALL DREAD (BALB,'F10.8')   !Bolometric albedo, accretion disk
C     rim
      CALL DUMMY (1)
	CALL DREAD (VMUD,'F10.8')   !Bolometric limb darkening, accret.
C     disk rim
      CALL DUMMY (1)
      CALL IREAD ( KEY)           !Key to apply irradiation correction
C     to accretion disk face
	CALL DUMMY (7)
	CALL IREAD (KYRM)           !Key to apply irradiation correction
C     to accretion disk rim
      CALL DUMMY (1)
	CALL IREAD ( IHSP)			!Key to indicate presence of hot spot
C     Read azimuth of center of hot spot
	CALL DREAD (HSAZ,'F8.1')    !Azimuth of hot spot
	CALL DUMMY (1)
C     Read azimuthal width of direct impact region
	CALL DREAD (HSWD,'F8.1')    !Azimuthal width of hot spot
      HSAZ=HSAZ/RAD
	HSWD=HSWD/RAD
	CALL DUMMY (1)
C     Read Teff of direct impact region, in kK
	CALL DREAD (HSTEMP,'F8.2')  !Teff of hot spot
	HSTEMP=HSTEMP*1000.0D0
	CALL DUMMY (1)
C     Read downstream extent of hot spot, measured from boundary of
C     direct impact region, in degrees.
      CALL DREAD (HSDWD, 'F8.2')
	HSDWD=HSDWD/RAD
	CALL DUMMY (1)
C     Read Teff of downstream boundary, in kK
C     If less than TRIM, set = TRIM
      CALL DREAD (TDWND, 'F8.2')
	TDWND=TDWND*1000.0D0
	CALL DUMMY (1)
C     Read upstream extent of hot spot, measured from boundary of
C     direct impact region, in degrees
      CALL DREAD(HSUP, 'F8.2')
	HSUP=HSUP/RAD
	CALL DUMMY (1)
C     Read Teff of upstream boundary, in kK
C     If less than TRIM, set = TRIM
      CALL DREAD (TUPD, 'F8.2')
	TUPD=TUPD*1000.0D0
	CALL DUMMY (1)
C     Read exponent for calculating temperature distribution on face
C     of accretion disk
      CALL DREAD (EXPON, 'F8.3')
	CALL DREAD (SCL,'F8.3')
	CALL DREAD (ALPHAV, 'F8.4')   !Viscosity coefficient
	CALL DREAD (VMNMASS,'F8.3')  !Mean molecular mass
      CALL DUMMY(1)
	CALL IREAD (KEYBL)           !Key to apply BL temp.
	CALL DUMMY (1)
	CALL DREAD (TBL,'F8.2')      !Boundary layer Teff, in kK
C     Convert to degrees K
      TBL=TBL*1.D3
	CALL DUMMY(1)
	CALL IREAD (KFC) 			 !Key to assign face temperatures
	CALL DUMMY (1)
	CALL IREAD (KRMT)            !Key to assign rim temperature
C     Set constant CS2
      CS2=4.D0/3.D0*SGMA
C***********************************************
C     Convert from effective to boundary temp.
      WRITE (NOUT1,103)
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC
      WRITE (NOUT1,107) NWL,MDAT,NRF,NGR,NTMP
      WRITE (NOUT1,110) (WL(K),K=1,NWL)   
C     LIST MASS TRANSFER RATE
      WRITE (NOUT1,135) VMDOT
C     LIST EFFECTIVE TEMP., CENTRAL STAR
      WRITE (NOUT1,136) TECS
C     LIST BOLOMETRIC LIMB DARK., CENTRAL STAR
      WRITE (NOUT1,137) VMU
C     LIST BOLOMETRIC ALBEDO, ACCRETION DISK FACE
      WRITE (NOUT1,138) BALF
C     LIST BOLOMETRIC LIMB DARK., ACCRETION DISK FACE
      WRITE (NOUT1,139) VMUF
C     LIST BOLOMETRIC ALBEDO, ACCRETION DISK RIM
      WRITE (NOUT1,163) BALB
C     LIST BOLOMETRIC LIMB DARK., COMPANION STAR
      WRITE (NOUT1,164) VMUD
C     LIST VALUE OF KEY
      WRITE (NOUT1,165) KEY
C     LIST VALUE OF KYRM
      WRITE (NOUT1,166) KYRM
C     LIST VALUE OF IHSP
      WRITE (NOUT1,167) IHSP
C     LIST VALUE OF HOT SPOT AZIMUTH
      WRITE (NOUT1,168) HSAZ*RAD
C     LIST VALUE OF HOT SPOT WIDTH
      WRITE (NOUT1,171) HSWD*RAD
C     LIST VALUE OF HOT SPOT EFFECTIVE TEMPERATURE
      WRITE (NOUT1,169) HSTEMP
C     INPUT PERMANENT FILE DATA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN2)
      VMSRATIO=QS/(QS+1.D0)
C     Calculate mass transfer rate in cgs units
      VMD=SUNM*VMDOT/YRSEC/1.0E6 !mass transfer rate,gm/sec
C
      CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,A,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN3)
C     If required by control key KFC, input array of face Teff values
      IF (KFC.GT.0) THEN
		READ (IN12,102) (FACETMP(K,1),K=1,NRING)
	END IF
C     If KFC=1, set temperature profile azimuthally symmetric in accordance
C     with the radial profile just read in.
      T1RF=0.D0
	IF (KFC.GT.0) THEN
		T1RF=FACETMP(NRING,1)
		DO K=1,NRING
			DO J=2,NSG
				FACETMP(K,J)=FACETMP(K,1)
			END DO
		END DO
	END IF
C     If KFC=2, set temperature profile that is not azimuthally symmetric
	IF (KFC.EQ.2) THEN
		OPEN (UNIT = 20, FILE = 'D:\IA2\TRANG1.DAT', STATUS = 'OLD')
		OPEN (UNIT = 21, FILE = 'D:\IA2\TRANG2.DAT', STATUS = 'OLD')
		OPEN (UNIT = 22, FILE = 'D:\IA2\TRANG3.DAT', STATUS = 'OLD')
		OPEN (UNIT = 23, FILE = 'D:\IA2\TRANG4.DAT', STATUS = 'OLD')
		OPEN (UNIT = 24, FILE = 'D:\IA2\TRTMP.DAT', STATUS = 'OLD')
		READ (20,106) (VINITANG(K),K=1,NRING)
		READ (21,106) (VINENDANG(K),K=1,NRING)
		READ (22,106) (VNDITANG(K),K=1,NRING)
		READ (23,106) (VNDENDANG(K),K=1,NRING)
		READ (24,102) (TRTMP(K),K=1,NRING)
C     Calculate ordinal number of segment starting the initial
C     temperature ramp regions.
      DO K=1,NRING
		KINSTART(K)=VINITANG(K)/360.*NSG
	END DO
C     Calculate ordinal number of segment ending the initial
C     temperature ramp regions.
      DO K=1,NRING
		KINEND(K)=VINENDANG(K)/360.*NSG
	END DO
C     Calculate ordinal number of segment starting the terminating
C     temperature ramp regions.
      DO K=1,NRING
		KNDSTART(K)=VNDITANG(K)/360.*NSG
	END DO
C     Calculate ordinal number of segment ending the terminating
C     temperature ramp regions.
      DO K=1,NRING
		KNDEND(K)=VNDENDANG(K)/360.*NSG
	END DO
C     Set the temperatures within the hot spot area
	DO K=1, NRING
				AA=KINEND(K)-KINSTART(K)
				IF (AA.LT.0.D0) CALL PMDSTOP
				IF (AA.EQ.0.D0) AA=1.D0
				BB=KNDEND(K)-KNDSTART(K)
				IF (BB.LT.0.D0) CALL PMDSTOP
				IF (BB.EQ.0.D0) BB=1.D0
		DO J=1,NSG
			IF ((J.GE.KINSTART(K)).AND.(J.LT.KINEND(K))) THEN
				FACETMP(K,J)=(J-KINSTART(K))/AA*(TRTMP(K)
     $			-FACETMP(K,J))+FACETMP(K,J)
			END IF
			IF ((J.GE.KINEND(K)).AND.(J.LE.KNDSTART(K))) THEN
				FACETMP(K,J)=TRTMP(K)
			END IF
			IF ((J.GT.KNDSTART(K)).AND.(J.LE.KNDEND(K))) THEN
				FACETMP(K,J)=(KNDEND(K)-J)/BB*(TRTMP(K)
     $				-FACETMP(K,J))+FACETMP(K,J)
			END IF
		END DO
	END DO
		CLOSE (UNIT =20, STATUS = 'KEEP')
		CLOSE (UNIT =21, STATUS = 'KEEP')
		CLOSE (UNIT =22, STATUS = 'KEEP')
		CLOSE (UNIT =23, STATUS = 'KEEP')
		CLOSE (UNIT =24, STATUS = 'KEEP')
	END IF
C     If required by control key KRMT, input rim Teff values
      IF (KRMT.EQ.1) THEN
		DO K=1,NRM
		READ (IN13,102) (RMTMP(K,J),J=1,NSG)
		END DO
	END IF
C
      IF (NRM.EQ.0) GO TO 18
C     Initialize incident flux array for outer rim of accretion disk
      DO K=1,NRM
		DO J=1,NSG
			RMIFLX(K,J)=0.D0
		END DO
	END DO
C     Set mass of central star equal to mass of correct component.
      IF (ISTAR.EQ.1) THEN
          VMC=VMI
		VMCOM=VMI*QS
      ELSE
          VMC=VMI*QS
		VMCOM=VMI
      END IF
C     Set radius of central star, in cm., equal to separation of
C     components in cm. times equatorial radius of correct component.
C     Set the distance D1L1 to the distance from the central star to 
C     the L1 point. The distance D2L1 is the distance from the companion
C     star to the L1 point.
      IF (ISTAR.EQ.1) THEN
          VRC=RI*VR(I,NTH/2+1,1)
		D1L1=VL(I,1)*RI
		D2L1=VL(3-I,1)*RI
      ELSE
          VRC=RI*VR(3-I,NTH/2+1,1)
		D1L1=VL(3-I,1)*RI
		D2L1=VL(I,1)*RI
      END IF
C     Calculate rate of delivery of potential energy to central
C     star in the absence of an accretion disk.
      DPE1=GRAV*VMD*VMC*(1.D0/VRC-1.D0/D1L1)
	DPE2=GRAV*VMD*VMCOM*(1.D0/D2L1-1.D0/(RI-VRC))
	EDEL=DPE1-DPE2
	TOTLUM=0.5D0*EDEL
C     Calculate Eddington luminosity
C     See eq. 1.2a, p.84 of FKR
      VLEDD=4.D0*PI*GRAV*VMC*AMUMS*VLIGHT/SGTM
C     Calculate critical accretion rate
C     See eq. 5.55, p.84 of FKR
      VMCRIT=VLEDD*VRC/2.D0/ETA/GRAV/VMC
C     The value of VMCRIT is in cgs units. Convert to solar mass/yr.
	VMCRIT=VMCRIT/SOLMS*YRSEC
C     Calculate maximum radius of accretion disk
C     See Warner, p.57, eq. 2.61
      IF (QS.LE.1.D0) THEN
		RMX=RI*0.6D0/(1.D0+QS)/SNRAD
	ELSE
		RMX=RI*0.6D0*QS/(1.D0+QS)/SNRAD
	END IF
C     List outer radius of accretion disk
      ROUT=RGRD(NRING)*RI
C     CVFACT converts from units of component separation to
C     units of solar radii. RGRD(K) are in units of component 
C     separation      
      CVFACT=RI/SNRAD
C
C     Calculate normal intensity for central star
      T4=TECS**4
      VINORM=SGMA/PI*T4*3.D0/(3.D0-VMU)
C
      NU=IN9
      CALL DUMMY(2)
      READ (IN9,109,END=399,ERR=299) (RWL(K),K=1,NRF)
      NU=IN10
      CALL DUMMY(2)
      READ (IN10,109,END=399,ERR=299) (RTMP(K),K=1,NTMP)
      NU=IN11
      CALL DUMMY(2)
      READ (IN11,109,END=399,ERR=299) (RGRAV(K),K=1,NGR)
C  
C  
C     Calculate effective temperatures on disk face
C     See eq. 2.2 of Hubeny and Plavec paper on beta Lyrae or
C     Pringle(1981,ARA&A,19,137), eq. at top of p.146.
C     Calculate TSTAR
   17 TSTAR=(COEF*VMC*VMD/VRC/VRC/VRC)**0.25
      FRCTRAD(1)=RGRD(1)*RI/VRC
      DO 1 K=2,NRING
	if (k.eq.10) then
	zzz=0.d0
	end if
C
      DIST=RGRD(K)*RI
C     **********************************************************
C     Approximate solution from eqs. 5.45 of FKR
      FVAL=(1.D0-(VRC/DIST)**0.50)**0.25
	VMD16=VMD/1.D16
	VM1=VMC/SUNM
	R10=DIST/1.D10
	IF (ALPHAV.LE.5.D0) THEN
		ALPHH=ALPHAV
	ELSE
		ALPHH=1.D-1
	END IF
C     Value of sigma, mass density, in gm/cm^2
	VSGMA=5.2D0/ALPHH**0.8D0*VMD16**0.7D0*VM1**0.25/R10**0.75D0*
     $		FVAL**2.8D0
C     Value of H, annulus height, in cm
	VHH=1.7D8/ALPHH**0.1D0*VMD16**1.5D-1/VM1**0.375D0*R10**1.125D0*
     $		FVAL**0.6D0
C     Value of rho, mass density, in gm/cm^3
	VRHO=3.1D-8/ALPHH**0.7D0*VMD16**5.5D-1*VM1**6.25D-1/R10**1.875D0*
     $		FVAL**2.2D0
C     Value of T_c, central plane temperature, in Kelvins
	VTC=1.4D4/ALPHH**0.2D0*VMD16**0.3D0*VM1**0.25D0/R10**0.75D0*
     $		FVAL**1.2D0
C     Value of tau, the optical depth at the central plane
	VTAU=33.D0/ALPHH**0.8D0*VMD16**0.2D0*FVAL**0.8D0
C     Value of nu, the kinematic viscosity, in cm^2/sec
	VNU=1.8D14*ALPHH**0.8D0*VMD16**0.3D0/VM1**0.25D0*R10**0.75D0*
     $		FVAL**1.2D0
C     Value of v_R, the radial drift velocity, in cm/sec
	VRDVL=2.7D4*ALPHH**0.8D0*VMD16**0.3D0/VM1**0.25D0/R10**0.25D0/
     $		FVAL**2.8D0
C     Value of ratio of height to ring radius
	VHOR=1.7D-2/ALPHH**0.1D0*VMD16**1.5D-1/VM1**0.375D0*R10**0.125D0*
     $		FVAL**0.6D0
C
C
C     **********************************************************
C     Set speed of radial drift
      VRADVL(K)=VRDVL
	DIST2=RGRD(K-1)*RI
C     Viscosity from TLUSDISK User's Guide
C     This is the depth-averaged kinematic viscosity, given by
C     Hubeny 1990, eq. 2.5 (corrected)
C     VISC(K)=(GM(star)R)**0.5/Re
C     Calculate viscosity
      FRCTRAD(K)=DIST/VRC
      RATIO=VRC/DIST
      F=1.D0-DSQRT(RATIO)
	IF (F.LT.0.D0) F=0.D0
      CALL REALPH(ALPHAV,RE,ALPHAQ,VRC,VMD,VMC,FRCTRAD(K))
      VISC(K)=(GRAV*VMC*DIST)**0.5D0/RE
	IF (ALPHAV.LE.5.D0) THEN
		ALPHA(K)=RE
	ELSE
		ALPHA(K)=ALPHAQ
	END IF
C     Mass per unit area from eq. 5.39(7), FKR
      SGMAV(K)=VMD/3.D0/PI*F/VISC(K)
C     D=dissipation per unit disk face area, eq. 5.18, FKR
      D(K)=3.D0*GRAV*VMD/8.D0/PI/DIST**3*VMC*F
C     Use eq. 5.44, FKR, for Ross. mean opacity, and eq. 5.39(6)
C     for optical depth. Then use eq. 5.39(5) to solve for T^8
      CS1=DSQRT(BOLTZ/VMNMASS/AMUMS)
	CS3=CKRAMER*DSQRT(GRAV*VMC)*D(K)/DIST**1.5D0
	T8=CS3/CS2/CS1*SGMAV(K)**2
C     Calculate temperature at the central plane
      IF (T8.GT.0.D0) THEN
		TC(K)=T8**0.125D0
	ELSE
		TC(K)=0.D0
	END IF
C     Calculate velocity of sound, neglect second term in
C     eq. 5.39(4) and use P/rho in eq. 5.39(3), FKR.
      VELSND(K)=DSQRT(TC(K)*CS1*CS1)
C     First approx. to H, eq. 5.39(2), FKR
      VLH(K)=VELSND(K)*DIST**1.5D0/(GRAV*VMC)**0.5D0
C     First approx. to density, eq. 5.39(1), FKR
      IF (VLH(K).GT.0.D0) THEN
		RHO(K)=SGMAV(K)/VLH(K)
	ELSE
		RHO(K)=0.D0
	END IF
C     First approx. to pressure, eq. 5.39(4), FKR
      PRESS(K)=RHO(K)*CS1*CS1*TC(K)+CS2/VLIGHT*TC(K)**4
C     Recalculate vel. of sound
      IF (RHO(K).GT.0.D0) THEN
		VELSND(K)=DSQRT(PRESS(K)/RHO(K))
	ELSE
		VELSND(K)=0.D0
	END IF
C     Recalculate H, eq. 5.39(2)
C     Note that VLH(K) is in cm.
      VLH(K)=VELSND(K)*DIST**1.5D0/(GRAV*VMC)**0.5D0
C     Recalculate density, eq. 5.39(1)
      IF (VLH(K).GT.0.D0) THEN
		RHO(K)=SGMAV(K)/VLH(K)
	ELSE
		RHO(K)=0.D0
	END IF
C     Recalculate pressure, eq. 5.39(4)
      PRESS(K)=RHO(K)*CS1*CS1*TC(K)+CS2/VLIGHT*TC(K)**4
C     Use Kramer's law again for Rosseland mean opacity, eq. 5.44, FKR
      IF (TC(K).GT.0.D0) THEN
		VKAPPA=CKRAMER*RHO(K)/TC(K)**3.5D0
	ELSE
		VKAPPA=0.D0
	END IF
C     Calculate optical depth to central plane, eq. 5.39(6), FKR
      TAU(K)=VKAPPA*SGMAV(K)
C     Calculate std. model Teff of ring
C
      CC=F*RATIO**3
	IF (CC.GT.0.D0) THEN    
		T1=SCL*TSTAR*CC**EXPON
	ELSE
		T1=0.D0
	END IF
        DO J=1,NSG
C     Std. model Teff, without irradiation
        TMPRFB(K,J)=T1
C     Initial setting of array including irradiation
        TMPRFA(K,J)=T1
        END DO
	  DO J=1,NSG
		TMPRF(K,J)=TMPRFB(K,J)
	  END DO
    1 CONTINUE
      IF (T1RF.LT.T1) T1RF=T1
      T1RF=T1
      DO J=1,NSG
		TMPRFB(1,J)=TMPRFB(2,J)
		TMPRFA(1,J)=TMPRFA(2,J)
		TMPRF(1,J)=TMPRF(2,J)
	END DO
C     If specified by KFC,
C     replace calculated face temperatures by values from external file
      IF (KFC.GT.0) THEN
		DO K=1,NRING
			DO J=1,NSG
				TMPRF(K,J)=FACETMP(K,J)
				TMPRFA(K,J)=FACETMP(K,J)
				TMPRFB(K,J)=FACETMP(K,J)
			END DO
		END DO
	   END IF
C     Convert calculated ring heights to solar radii
    5 DO K=1,NRING
		VLH(K)=VLH(K)/SNRAD
	END DO
C     Calculate the boundary layer total radiating area
	BLAREA=PI*RI*RI*(RGRD(2)**2-RGRD(1)**2)
C     Calculate luminosity of the boundary layer
      BLLUM=BLAREA*SGMA*TBL**4
C     Decide whether to include boundary layer
      IF (KEYBL.NE.0) THEN
		DO J=1,NSG
			TMPRF(1,J)=TBL
		END DO
	END IF
	DO K=1,NRING
		TMPRFNR(K)=TMPRF(K,1)
	END DO
C     Allow for irradiation of disk face by central star.
C     Do not allow irradiation if temperatures are assigned.
      IF (KFC.EQ.0) THEN
      DO 70 K=2,NRING
	IF (RB/A*SNRAD.LE.RGRD(K)) THEN
         TMP=TMPRF(K,1)
         RDV=RGRD(K)*RI
         IF (KEY.EQ.1) THEN
         CALL FIFFC(TMP,RDV,VRC,VMU,BALF,VINORM,TMPO,SM,FLXSM,FLX1,
     $   FLX2,NOUT1)
         ELSE
         CALL FIFFC(TMP,RDV,VRC,VMU,0.D0,VINORM,TMPO,SM,FLXSM,FLX1,
     $   FLX2,NOUT1)
         END IF    
         FLXSMSV(K)=FLXSM !Emergent flux, accret.disk, allow. for BALF
         FLX1SV(K)=FLX1   !Emergent flux from energy dissipation
         FLX2SV(K)=FLX2   !Emergent flux from incid., weighted by BALF
         VINRM(K)= FLXSM/PI*3.D0/(3.D0-VMUD)  !Normal intensity, disk
         DO J=1,NSG
		  TMPRFA(K,J)=TMPO
			IF (IRCT.EQ.0) THEN
	           TMPRF(K,J)=TMP
		    ELSE
			   TMPRF(K,J)=TMPO     !TMPRF(K,J) is an effective T
		    END IF
			IF (IRCT.EQ.0) THEN
	           SMV(K,J)=0.D0
		    ELSE
	           SMV(K,J)=SM         !Incident flux from central star
		    END IF
         END DO
	END IF
   70 CONTINUE
      END IF
C     Initialize accumulators for total disk luminosity and mass
      TOTLUM=0.D0
	AREA=0.D0
	TOTMASS=0.D0
      DO K=2,NRING
C     Calculate mass of ring and luminosity
		AREA=PI*RI*RI*(RGRD(K)**2-RGRD(K-1)**2)
		VMASS(K)=AREA*(SGMAV(K)+SGMAV(K-1))/2.D0
C     If KFC.eq.1, the face temperatures have been read in from an external file.
		IF (KFC.NE.1) THEN
C			IF (DABS(EXPON-0.25D0).LT.1.D-10) THEN
C     Note that this expression would be divided by 2.0 to use a 
C     dissipation appropriate to the average annulus radius, but we
C     must allow for the radiation from both sides of the accretion disk
C				VLUMIN(K)=AREA*(D(K)+D(K-1))
C			ELSE
				VLUMIN(K)=AREA*SGMA*(TMPRF(K,1)**4+TMPRF(K-1,1)**4)
C			END IF
		END IF
		IF (KFC.EQ.1) THEN
C     In this case the face temperatures have been read in from an external
C     data file. Must allow for the radiation from both sides of the 
C     accretion disk.
			VLUMIN(K)=2.D0*AREA*SGMA*(0.5D0*FACETMP(K,1)+0.5D0*
     $			FACETMP(K-1,1))**4.D0
		END IF
C     Accumulate contributions to total accretion disk luminosity
          TOTLUM=TOTLUM+VLUMIN(K)
C     Accumulate total accretion disk mass
		TOTMASS=TOTMASS+VMASS(K)
      END DO
C     Calculate effective temperatures on outer rim
C     See eq. for q(h) in Hubeny and Plavec paper on beta Lyrae, and
C     eqs. 4.16, 4.17, and eq. for D(bar)/D0.
   34 SMH=HV/RA
      C1=1.0+SMH/2.0
      C2=1.0+SMH*2.0
      C3=1.0+SMH*2.0/PI
      VQ=4.0/PI*C1/C2/C3
      C4=(8.0*VQ/9.0)**0.25
      TRIM=C4*T1RF
	DO K=1,NRM
		DO J=1,NSG
C     If rim temperature is assigned, do so.
			IF (KRMT.EQ.1) THEN
				TMPRR(K,J)=RMTMP(K,J)
			ELSE
				TMPRR(K,J)=TRIM
			END IF
		END DO
	END DO
C     Test TDWND and TUPD
      IF (TDWND.LT.TRIM) TDWND=TRIM
	IF (TUPD.LT.TRIM) TUPD=TRIM
C     Set nominal azimuthal boundaries for direct impact region.
C     Downstream means in the direction of increasing longitude.
C     Downstream boundary
      BDYDN=HSAZ+0.5D0*HSWD
C     Upstream boundary
	BDYUP=HSAZ-0.5D0*HSWD
	IF (BDYUP.LT.0.D0) BDYUP=BDYUP+TWPI
C     Set azimuthal boundary for terminus of downstream extension
      ENDDWN=BDYDN+HSDWD
C     Set azimuthal boundary for terminus of upstream extension.
      ENDUP=BDYUP-HSUP
	IF (ENDUP.LT.0.D0) ENDUP=ENDUP+TWPI   
C     Calculate downstream temperature gradient.
C     Degrees per radian
      DLTMP=HSTEMP-TDWND
	IF (HSDWD.GT.0.D0) THEN
		DTDN=DLTMP/HSDWD
	ELSE
		DTDN=0.D0
	END IF
C     Calculate upstream temperature gradient.
C     Degrees per radian
      DLTMP=HSTEMP-TUPD
	IF (HSUP.GT.0.D0) THEN
		DTUP=DLTMP/HSUP
	ELSE
		DTUP=0.D0
	END IF
C     List some output quantities
C
	  WRITE (NOUT1,179) ENDUP*RAD
	  WRITE (NOUT1,180) TUPD
	  WRITE (NOUT1,181) ENDDWN*RAD	 
	  WRITE (NOUT1,182) TDWND
C     LIST VALUE OF EXPONENT FOR FACE TEMPERATURE PROFILE
      WRITE (NOUT1,170) EXPON
C     List value of KEYBL, the control to include a boundary layer.
      WRITE (NOUT1, 187) KEYBL
C     List central star radius
      WRITE (NOUT1,140) VRC
      VRCA=VRC/SNRAD
      WRITE (NOUT1,144) VRCA
C     List Edd. luminos. and crit. accret. rate
      WRITE (NOUT1,160) VLEDD,VMCRIT
C     List maxumum possible accretion disk luminosity
      WRITE (NOUT1, 155) EDEL
C     List largest radius of accretion disk
      WRITE (NOUT1,156) RMX	      
      WRITE (NOUT1,141) RA*SNRAD*CVFACT
      WRITE (NOUT1,145) RA*CVFACT
C     List radius of toroidal cross-section
      WRITE (NOUT1,174) HV*CVFACT
C     List total mass of accretion disk
      WRITE (NOUT1,186) TOTMASS/SUNM
C     List Viscosity coefficient
      WRITE (NOUT1,190) ALPHAV
C     List fractional radii of face rings
      WRITE (NOUT1,142) (FRCTRAD(K),K=1,NRING)
C     List radii of face rings, units of solar radius.
      WRITE (NOUT1,143) (RGRD(K)*CVFACT,K=1,NRING)
C     List heights of face rings, units of component separation.
      WRITE (NOUT1,183)  (VLH(K)/CVFACT,K=1,NRING)
C     List heights of face rings, units of solar radius.
      WRITE (NOUT1,184) (VLH(K),K=1,NRING)
C     List heights of face rings, specified by ACPGA,
C     units of solar radius.
      WRITE (NOUT1,185) (VLH(K)/VLH(NRING)*HV*CVFACT,K=1,NRING)
C     List Viscosity coefficient.
      WRITE (NOUT1,154) ALPHAV
C     List boundary layer properties
      WRITE (NOUT1,175) KEYBL
	  WRITE (NOUT1,172) HVBL,THBL,BLAREA
	  WRITE (NOUT1,173) TBL,BLLUM
C     Calculate values of Shakura-Sunyaev alpha, if the input was Re
C      DO K=2,NRING
C		HVAL=VLH(K)*SNRAD*CVFACT
C		ALPHA(K)=VISC(K)/VELSND(K)/HVAL
C	END DO
C     List properties of accretion disk
	  WRITE (NOUT1,151)
	  DO K=1,NRING
		WRITE (NOUT1,152) K,VLH(K),TC(K),RHO(K),PRESS(K),SGMAV(K),
     $    TAU(K),VELSND(K),VISC(K)
	  END DO
	  WRITE (NOUT1,153)
	  WRITE (NOUT1,153)
	  WRITE (NOUT1,153)
	  WRITE (NOUT1,153)
	  WRITE (NOUT1,153)
	  WRITE (NOUT1,158)
C     Calculate ring crossing times for parcel drift
        DO K=2,NRING
		AA1=(RGRD(K)-RGRD(K-1))*RI
	    AA2=(VRADVL(K)+VRADVL(K-1))/2.D0
		TCROSS(K)=AA1/AA2
	  END DO
	  TSUM=0.D0
	  DO K=1,NRING
		WRITE (NOUT1,159) K,VRADVL(K),VMASS(K),VLUMIN(K),TCROSS(K),
     $		ALPHA(K)
		TSUM=TSUM+TCROSS(K)
	  END DO
	WRITE (NOUT1,153)
C     List time to cross entire accretion disk
      WRITE (NOUT1,189) TSUM
C     Now assign Teff values to all the rim points.
      DO K=1,NRM
C     Initialize counter for number of grid points within hot spot.
C     Note that the scan first goes over a meridian perpendicular	to
C     the equatorial plane.
	   IHSCT=0
	   DPHR=PHIR(2)-PHIR(1)
         DO J=1,NSG
C     ************************************************
C     ************************************************
	   IF (IHSP.NE.0) THEN
C	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C     If there is a hot spot, test for proper azimuthal location
C     This section treats points that may fall within the spot proper
C     ************************************************
		   AZIM=PHIR(J)
		   IF ((AZIM.GE.(BDYUP)).AND.
     $	   (AZIM.LE.(BDYDN))) THEN
				IHSCT=IHSCT+1
				TMPRR(K,J)=HSTEMP
			END IF
C     Allow for case in which hot spot goes past phase zero.
			IF (BDYUP.GT.BDYDN) THEN
				IF (AZIM.GE.BDYUP) THEN
					IHSCT=IHSCT+1
					TMPRR(K,J)=HSTEMP
				END IF
			IF ((AZIM.GE.0.D0).AND.(AZIM.LE.BDYDN)) THEN
					IHSCT=IHSCT+1
					TMPRR(K,J)=HSTEMP
			END IF
			END IF
C	Guarantee that the hot spot will be at least one
C     division segment wide.
			IF ((HSWD.LE.DPHR).AND.((AZIM-BDYDN).LE.DPHR)) THEN
      			TMPRR(K,J)=HSTEMP
	            IHSCT=IHSCT+1
	        END IF
C     End of spot proper
C     *************************************************
C     Now allow for grid points in the downstream extension
C     The downstream extension is at larger longitudes than HSAZ
		IF ((AZIM.GE.BDYDN).AND.(AZIM.LE.ENDDWN)) THEN
			IHSCT=IHSCT+1
			TMPPT=HSTEMP-DTDN*(AZIM-BDYDN)
			TMPRR(K,J)=TMPPT
		END IF
		IF (BDYDN.GT.ENDDWN) THEN
			IF (AZIM.GE.BDYDN) THEN
				IHSCT=IHSCT+1
				TMPPT=HSTEMP-DTDN*(AZIM-BDYDN)
				TMPRR(K,J)=TMPPT
				IF (TMPRR(K,J).GT.HSTEMP) TMPRR(K,J)=HSTEMP
			END IF
              IF (AZIM.LE.ENDDWN) THEN
				IHSCT=IHSCT+1
				TMPPT=HSTEMP-DTDN*(AZIM+TWPI-BDYDN)
				TMPRR(K,J)=TMPPT
				IF (TMPRR(K,J).GT.HSTEMP) TMPRR(K,J)=HSTEMP
			END IF
		END IF
C     Allow for case in which the spot extends past phase zero
C
C
C     Now allow for grid points in the upstream extension
C     The upstream extension is at smaller longitudes than HSAZ
C     **************************************************
		IF ((AZIM.GE.ENDUP).AND.(ENDUP.GE.0.D0).AND.
     $		(AZIM.LE.BDYUP)) THEN
			IHSCT=IHSCT+1
			TMPPT=HSTEMP-DTUP*(BDYUP-AZIM)
			TMPRR(K,J)=TMPPT
			IF (TMPRR(K,J).GT.HSTEMP) TMPRR(K,J)=HSTEMP
		END IF
C     Allow for case in which hot spot extends past phase zero.
		IF (ENDUP.GT.BDYUP) THEN
			IF ((BDYUP.GE.0.D0).AND.(AZIM.GE.0.D0).AND.
     $				(AZIM.LE.BDYUP)) THEN
				IHSCT=IHSCT+1
				TMPPT=HSTEMP-DTUP*(BDYUP-AZIM)
				TMPRR(K,J)=TMPPT
				IF (TMPRR(K,J).GT.HSTEMP) TMPRR(K,J)=HSTEMP
			END IF
			IF ((BDYUP.GE.0.D0).AND.(AZIM.LE.TWPI).AND.
     $				(AZIM.GE.ENDUP)) THEN
				IHSCT=IHSCT+1
				TMPPT=HSTEMP-DTUP*(BDYUP-AZIM+TWPI)
				TMPRR(K,J)=TMPPT
				IF (TMPRR(K,J).GT.HSTEMP) TMPRR(K,J)=HSTEMP
			END IF
		END IF
C     *****************************************************
C     There should be no case where BDYUP is negative.
		IF (BDYUP.LT.0.D0) CALL PMDSTOP
C     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C
	   END IF
C     *****************************************************
C     *****************************************************
         END DO
      END DO
      WRITE (NOUT1,116)
      WRITE (NOUT1,102) (TMPRFNR(K),K=1,NRING)
      WRITE (NOUT1,103)
      WRITE (NOUT1,122)
      WRITE (NOUT1,102) (TMPRR((NRM+1)/2,K),K=1,NSG)
      WRITE (NOUT1,103)
C    	Add luminosity of rim to accretion disk total
      DO K=2,NRM
C     Set rim height increment. If NTYPE=2, rim is toroidal.
C     If NTYPE is 1, rim is cylindrical.
		IF (NTYPE.EQ.2) THEN
			DH=HV*(VC(K)-VC(K-1))*RI
		ELSE
			IF (NTYPE.NE.1) CALL PMDSTOP
C     The value of HV is the semi-height of the rim, in units of the
C     component separation.
			DH=HV/(NRM-1)*2.D0*RI
		END IF
		DO J=2,NSG
			DRPH=RI*(PHIR(J)-PHIR(J-1))
C     Select procedure to calculate DLARM
			IF (NTYPE.EQ.2) THEN
				DLARM=DH*DRPH*(RMR(K)+RMR(K-1))/2.D0
			ELSE
				DLARM=DH*TWPI*A*RA/(NSG-1)
			END IF
			TMP=(TMPRR(K-1,J-1)+TMPRR(K-1,J)+TMPRR(K,J-1)
     $			+TMPRR(K,J))/4.D0
			BOLRM=BOLRM+DLARM*SGMA*(TMP**4)
		END DO
	END DO
C     List luminosity of face
      WRITE (NOUT1,176) TOTLUM
C     List luminosity of rim
      WRITE (NOUT1,177) BOLRM
C     List luminosity of boundary layer
      WRITE (NOUT1,178) BLLUM
	TOTLUM=TOTLUM+BOLRM
	IF (KEYBL.EQ.1) TOTLUM=TOTLUM+BLLUM
C     List total luminosity of accretion disk
      WRITE (NOUT1,157) TOTLUM
C     Allow for irradiation of rim by companion, under control of KYRM
C     If KYRM.NE.1, DO NOT APPLY CORRECTION FOR IRRADIATION
      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),VC1(I),VC2(I),
     $VC1(3-I),VC2(3-I),T0(I),T0(3-I),AV(I),AV(3-I),BCF(I),BCF(3-I),
     $UA(I),UA(3-I),IN4)
	CALL RDWT6(I,IWL,NTH,IN5)
C
      IF (KYRM.EQ.1) THEN
		CALL FIRM(TMPRR,VMSRATIO,3-ISTAR,NTH,BALB,VMUD,
     $		NRM,NSG,NOUT1)
	END IF
C     List rim effective temperature, including irradiation
      WRITE (NOUT1,162)
	WRITE (NOUT1,102) (TMPRR((NRM+1)/2,K),K=1,NSG)
	WRITE (NOUT1,103)
C

C     LIST INCIDENT FLUX VALUES FROM CENTRAL STAR
      WRITE (NOUT1,103)
      WRITE (NOUT1,129)
      WRITE (NOUT1,105) (SMV(K,1),K=1,NRING)
C     LIST EMERGENT FLUX FROM ENERGY DISSIPATION
      WRITE (NOUT1,103)
      WRITE (NOUT1,130)
      WRITE (NOUT1,105) (FLX1SV(K),K=1,NRING)
C     LIST EMERGENT FLUX FROM INCIDENT FLUX, WEIGHTED BY BALF
      WRITE (NOUT1,103)
      WRITE (NOUT1,131)
      WRITE (NOUT1,105) (FLX2SV(K),K=1,NRING)
C     LIST TOTAL EMERGENT FLUX
      WRITE (NOUT1,103)
      WRITE (NOUT1,132)
      WRITE (NOUT1,105) (FLXSMSV(K),K=1,NRING)
      WRITE (NOUT1,103)
C     LIST FACE TEMPERATURES, INCLUDING IRRADIATION
      WRITE (NOUT1,115)
      WRITE (NOUT1,102) (TMPRFA(K,1),K=1,NRING)
C     Read limb darkening data, accretion disk face
   12 CALL RDWT4(NGR,NTMP,NRF,U1R,U2R,IN6)
C     Read limb darkening data, accretion disk rim
      CALL RDWT4A(NGR,NTMP,NRF,U1Q,U2Q,IN7)
      IF (NRF.GT.13) GO TO 31
      LM1=NGR-1
      LM2=NTMP-1
      DO 23 J=1,LM1
      DO 22 K=1,LM2
      DO 21 L=NRF,13
      U1R(J,K,L+1)=U1R(J,K,L)
      U2R(J,K,L+1)=U2R(J,K,L)
	U1Q(J,K,L+1)=U1Q(J,K,L)
	U2Q(J,K,L+1)=U2Q(J,K,L)
   21 CONTINUE
   22 CONTINUE     
   23 CONTINUE   
   31 IF (NTMP.GT.21) GO TO 32 
      LM1=NGR-1
      LM2=NTMP-1
      DO 26 J=1,LM1     
      DO 25 K=LM2 ,21   
      DO 24 L=1,14      
      U1R(J,K+1,L)=U1R(J,K,L)  
      U2R(J,K+1,L)=U2R(J,K,L)
	U1Q(J,K+1,L)=U1Q(J,K,L)
	U2Q(J,K+1,L)=U2Q(J,K,L)
   24 CONTINUE
   25 CONTINUE   
   26 CONTINUE   
   32 IF (NGR.GT.9) GO TO 33   
      LM1=NGR-1
      DO 29 J=LM1,9     
      DO 28 K=1,22      
      DO 27 L=1,14      
      U1R(J+1,K,L)=U1R(J,K,L)  
      U2R(J+1,K,L)=U2R(J,K,L)
	U1Q(J+1,K,L)=U1Q(J,K,L)
	U2Q(J+1,K,L)=U2Q(J,K,L)
   27 CONTINUE
   28 CONTINUE   
   29 CONTINUE
   33 IF (MDAT.EQ.0) GO TO 18
C     Read model atmosphere flux values
C     PRINCIPAL LOOP    
C  
   18 DO 50 IWL=1,NWL   
      WLV=WL(IWL)
      IF (NRM.EQ.0) GO TO 150
      DO 8 K=2,NRING
      DO 7 J=1,NSG
C     DETERMINE LIMB DARKENING COEFFICIENTS, FACE OF DISK
      TMP=TMPRFA(K,J)              !Tables assume temp. in 
C		kilokelvins
      TAA=TMP/1.D3
	BLMK=0.D0
	IF (TMP.GT.10.D0) THEN
      CALL DKCF(WLV,NGR,NTMP,NRF,TAA,GF(K),U1F(K,J),U2F(K,J))
C      IF (MDAT.NE.0) GO TO 15  
      CALL BBDY(TMP/1.D3,WLV,BLMK)
	END IF  
C      GO TO 16   
C   15 CALL MDLAT(WLV,NGR,NTMP,NRF,TMP,GF(K),BLMK)
C     DETERMINE OUTWARD INTENSITY,NORMAL TO SURFACE, ON FACE OF DISK
C     IN WAVELENGTH UNITS OF MICRONS
C     Note that BLMK is in intensity units  
   16 BDIF(K,J)=BLMK
    7 CONTINUE   
    8 CONTINUE
C     Store current limb darkening tables
      DO II=1,10
		DO K=1,22
			DO J=1,14
				U1SV(II,K,J)=U1R(II,J,K)
				U2SV(II,K,J)=U2R(II,J,K)
			END DO
		END DO
	END DO
C     Replace current limb darkening tables with tables for rim.
	DO II=1,10
		DO K=1,22
			DO J=1,14
				U1R(II,K,J)=U1Q(II,J,K)
				U2R(II,K,J)=U2Q(II,J,K)
			END DO
		END DO
	  END DO       
C     Determine limb darkening coeffs. and normal intens. for rim
      DO 10 K=1,NRM
         DO 9 J=1,NSG
         TMP=TMPRR(K,J)/1.23244D0
	   TAA=TMP/1.D3
		BLMK=0.D0
		IF (TMP.GT.10.D0) THEN
         CALL DKCF(WLV,NGR,NTMP,NRF,TAA,GR(K),U1RM(K,J),
     $   U2RM(K,J))
C         IF (MDAT.NE.0) GO TO 11
         CALL BBDY(TMP/1.D3,WLV,BLMK)
		END IF
C     Note that BLMK is in intensity units.
   14 BDIR(K,J)=BLMK
    9 CONTINUE
   10 CONTINUE
C     Restore original limb darkening tables
	DO II=1,10
		DO K=1,22
			DO J=1,14
				U1R(II,K,J)=U1SV(II,J,K)
				U2R(II,K,J)=U2SV(II,J,K)
			END DO
		END DO
	  END DO       
C     LIST NORMAL INTENSITY VALUES
      WRITE (NOUT1,103)
      WRITE (NOUT1,125) WLV
      WRITE (NOUT1,134)
      WRITE (NOUT1,105) (BDIF(K,1),K=1,NRING)
C     LIST NORMAL INTENSITY VALUES, TOTAL
      WRITE (NOUT1,103)
      WRITE (NOUT1,133)
      WRITE (NOUT1,105) (VINRM(K),K=1,NRING)
      WRITE (NOUT1,103)
      WRITE (NOUT1,117) WLV
      WRITE (NOUT1,134)
      WRITE (NOUT1,105) (BDIR(K,1),K=1,NRM)
C     LIST GRAVITY VALUES
      WRITE (NOUT1,103)
      WRITE (NOUT1,127)
      WRITE (NOUT1,105) (GF(K),K=1,NRING)
      WRITE (NOUT1,103)
	WRITE (NOUT1,147)
	DO K=1,NRING
		IF (GF(K).GT.1.D0) THEN
			DUMARY(K)=DLOG10(DABS(GF(K)))
		ELSE
			DUMARY(K)=0.D0
		END IF
	END DO
	WRITE (NOUT1,105) (DUMARY(K),K=1,NRING)
	WRITE (NOUT1,103)
      WRITE (NOUT1,128)
	DO K=1,NRM
		IF (GR(K).NE.0.D0) THEN
			DUMARY(K)=DLOG10(DABS(GR(K)))
		END IF
		IF (GR(K).EQ.0.D0) DUMARY(K)=-1000.D0
	END DO
      WRITE (NOUT1,105) (GR(K),K=1,NRM)
      WRITE (NOUT1,103)
	WRITE (NOUT1,148)
	WRITE (NOUT1,105) (DUMARY(K),K=1,NRM)
	WRITE (NOUT1,103)
C
  150 CALL CWTO(NWL,NSG,NRM,NRING,IRCT,WLV,TMPRF,TMPRFA,SMV,TMPRR,
     $RMIFLX,NOUT2)
   50 CONTINUE
   99 CONTINUE
C   99 CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F2)
      TM=(F2-F1)*1440.D0
      WRITE (NOUT1, 205) TM
      CLOSE (15,         STATUS='KEEP')
      CLOSE (UNIT=IN1,   STATUS='KEEP')
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=IN3,   STATUS='KEEP')
      CLOSE (UNIT=IN4,   STATUS='KEEP')
      CLOSE (UNIT=IN5,   STATUS='KEEP')
      CLOSE (UNIT=IN6,   STATUS='KEEP')
      CLOSE (UNIT=IN7,   STATUS='KEEP')
      CLOSE (UNIT=IN8,   STATUS='KEEP')
	CLOSE (UNIT=IN9,   STATUS='KEEP')
	CLOSE (UNIT=IN10,  STATUS='KEEP')
	CLOSE (UNIT=IN11,  STATUS='KEEP')
	CLOSE (UNIT=IN12,  STATUS='KEEP')
	CLOSE (UNIT=IN13,  STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
	  OPEN (UNIT = IN14, FILE=FL1(18), STATUS = 'OLD')
	  WRITE (IN14,101) KEYGOOD
	  CLOSE (UNIT=IN14, STATUS = 'KEEP')
      STOP
  399 CALL PMDSTOP
  299 CALL PMDSTOP
      END 
C
      SUBROUTINE FIFFC(TMPI,RDV,VRC,VMU,BALB,VINORM,TMP,SM,A,FLX1,FLX2,
     $NOUT1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION ORDIN(25)
      DATA SGMA/5.6696D-5/
      FRACT=VRC/RDV
      COEF=2.D0*((FRACT)**3)*VINORM
      VLL=FRACT
      VUL=1.D0
      DLT=(VUL-VLL)/24.D0
      DO J=1,25
         ABSC=(J-1)*DLT+VLL
         A=ABSC-FRACT
         D=1.D0+FRACT**2-2.D0*FRACT*ABSC
         B=(1.D0-VMU+VMU*A/D**0.5D0)
         C=A/D**2
         ORDIN(J)=B*C*(1.D0-ABSC**2)
      END DO
      CALL ITF(ORDIN,1,25,DLT,YM1,YNP1,0,ER,SM,25,KRT)
      SM=SM*COEF
      FLX1=SGMA*TMPI**4
      FLX2=BALB*SM
      A=FLX1+FLX2
      TMP=(A/SGMA)**0.25D0
      RETURN
      END
C
	SUBROUTINE REALPH(ALPHAV,RE,ALPHAQ,VRC,VMD,VMC,FRCT)
	IMPLICIT DOUBLE PRECISION(A-H,O-Z)
	DATA PARAM/3.7136909728D-15/
C     If ALPHAV is .LE. 5.0, it is a normal ALPHA, otherwise it is a Reynolds number
	IF (ALPHAV.LE.5.0D0) THEN
		QR=1.D0/(FRCT*(1.D0-1.D0/DSQRT(FRCT))**0.25)
		RE=PARAM*(VMC**0.75)/(VRC**0.25)/(VMD**0.25)*QR/ALPHAV
		ALPHAQ=0.D0
	ELSE
		RE=ALPHAV
		QR=1.D0/(FRCT*(1.D0-1.D0/DSQRT(FRCT))**0.25)
		ALPHAQ=PARAM*(VMC**0.75)/(VRC**0.25)/(VMD**0.25)*QR/RE
	END IF
	RETURN
	END PROGRAM ACPGC
