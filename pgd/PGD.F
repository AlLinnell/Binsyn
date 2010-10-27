      PROGRAM PGD
C     Program to integrate over system binary components and determine
C     received light
C
C     ******************
C     Version PGD
C     ******************
C
C     Important parameters
C     ---------------------
C     CF=RI*RI, where RI=component separation in cgs units.
C
C     Stellar components
C     ------------------
C
C     ARYF()	Surface area, one segment
C     ARYG()	Projected surface area, one segment
C     ARYH()	Bolometric luminosity, one segment
C     ARYI()	Light, one segment, not counting eclipse (curr. wavel.)
C     ARYJ()	Eclipsed area, one segment
C     ARYK()	Light lost by eclipse, one segment,current wavelength
C     VITG1()	Surface area, one colatitude circle
C     VITG2()	Projected surface area, one colatitude circle
C     VITG3()	Bolometric luminosity, one colatitude circle
C     VITG4()	Light, one colatitude circle, not counting eclipses
C     VITG5()	Eclipsed area, one colatitude circle
C     VITG6()	Light lost by eclipse, one colatitude circle
C     SURF1	Surface area, first component
C     SM10A	Projected surface area, first component
C     BOL1	Bolometric luminosity, first component (4*pi luminosity)
C     SMCO	Light toward observer, not counting eclipse,
C			current wavelength, first component
C     SMDO	Light toward observer lost by eclipse,
C			current wavelength, first component
C     SM20A	Eclipsed area, first component (projected)
C     FLUX1   Flux (4*PI), first component, current wavelength
C     SURF2	Surface area, second component
C     SM10B	Projected surface area, second component
C     BOL2	Bolometric luminosity, second component	(4*pi luminosity)
C     SMEO	Light toward observer, not counting eclipse,
C			current wavelength, second component
C     SMFO	Light toward observer lost by eclipse,
C			current wavelength, second component
C     SM20B	Eclipsed area, second component	(projected)
C     FLUX2   Flux (4*PI), second component, current wavelength
C    -------------------------
C		Details about integrations
C     ITH     Index indicating current latitude on star under 
C			consideration.
C			Runs from 1 at upper pole to NTH at lower pole.
C			Typically, NTH=49.
C     VTH		Value of colatitude for index ITH.
C     VTHA	Value for colatitude for index ITH-1.
C     CSTH1	cos(VTHA)
C     CSTH2	cos(VTH)
C     DLCS	(CSTH1-CSTH2)
C     DLPH	Increment in phi, the azimuthal angle on a given 
C			latitude circle.
C     CSB		cos(beta), where beta is the angle between the radius
C			vector to the surface element in question and the
C			surface normal to the same surface element.
C     DSGA	VR*VR/CSB, where VR is the radial distance from the
C			component center to a surface mesh point, measured
C			in units of the orbital semi-major axis. When multiplied
C			by DLCS and DLPH, gives element of surface area.
C     VL1A	DSGA*cos(gamma), where gamma is the angle between the
C			local surface normal and the line of sight to the
C			observer. This is the projection of the surface area
C			element on the plane of the sky, when multiplied by
C			DLCS and DLPH.
C     AA		Visibility value, set=1.0 for those surface area elements
C			that are visible to the observer.
C     BB		Eclipsed value, set=1.0 or a lesser amount to represent
C			fractional eclipsed parts of a given surface element.
C     CFA		(1-mu1*(1-cos(gamma))-mu2*(1-cos(gamma))**2)*CF
C			mu1=first order limb darkening coefficient
C			mu2=second order limb darkening coefficient
C             This is for the Wade and Rucinski expression for intensity.
C     BDI		Normal light intensity at the surface element in question,
C			at the specified wavelength, in ergs/cm**2/sec/micron/sr
C     VL5A	VL1A*BB  This is a projected element of eclipsed area
C     VL2A	BDI*CFA*AA*VL1A   This is the local surface element 
C			contribution to the visible light
C     VL4A	BDI*CFA*BB*VL1A   This is the local surface element
C			contribution to the eclipsed light.
C     VL3A	SGMA*Teff**4*DSGA*CF  This is the local surface element
C			contribution to the bolometric luminosity (4*pi).
C     ---------surface area variables-----------------
C     Y1		DSGA(ITH,k), where k designates the local phi index.
C     Y2		DSGA(ITH,k+1)
C     Y3		DSGA(ITH+1,k)
C     Y4		DSGA(ITH+1,k+1)
C     ---------projected area variables----------------
C     Y5		DSGA(ITH,k)*cos(gamma(ITH,k))
C     Y6		DSGA(ITH,k+1)*cos(gamma(ITH,k+1))
C     Y7		DSGA(ITH+1,k)*cos(gamma(ITH+1,k))
C     Y8		DSGA(ITH+1,k+1)*cos(gamma(ITH+1,k+1))
C     ---------bolometric luminosity variables----------
C     Y9		VL3A(ITH,k)
C     Y10		VL3A(ITH,k+1)
C     Y11		VL3A(ITH+1,k)
C     Y12		VL3A(ITH+1,k+1)
C     ---------visible light variables------------------
C     Y13		VL2A(ITH,k)
C     Y14		VL2A(ITH,k+1)
C     Y15		VL2A(ITH+1,k)
C     Y16		VL2A(ITH+1,k+1)
C     ---------eclipsed area variables------------------
C     Y17		VL5A(ITH,k)
C     Y18		VL5A(ITH,k+1)
C     Y19		VL5A(ITH+1,k)
C     Y20		VL5A(ITH+1,k+1)
C     ---------eclipsed light variables-----------------
C     Y21		VL4A(ITH,k)
C     Y22		VL4A(ITH,k+1)
C     Y23		VL4A(ITH+1,k)
C     Y24		VL4A(ITH+1,k+1)
C     ---------integrands for latitude strip------------------
C     DV		Factor, typically 4.0 except in end ranges
C     ARYF(k)	(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH	        surface area
C     ARYG(k)	(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH          projected area
C     ARYH(k)	(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH 		bolom. luminos.
C     ARYI(k) (Y13+Y14+Y15+Y16)/DV*DLCS*DLPH      visible light
C     ARYJ(k) (Y17+Y18+Y19+Y20)/DV*DLCS*DLPH      eclipsed area
C     ARYK(k) (Y21+Y22+Y23+Y24)/DV*DLCS*DLPH      eclipsed light
C     ----------sums over latitude strip segments-------------
C     VITG1(ITH)	sum over ARYF
C     VITG2(ITH)	sum over ARYG
C     VITG3(ITH)	sum over ARYH
C     VITG4(ITH)	sum over ARYI
C     VITG5(ITH)	sum over ARYJ
C     VITG6(ITH)	sum over ARYK
C    -----------sums over all latitude strips, first component--
C     SURF1		sum over VITG1(ITH)		surface area
C     SM10A		sum over VITG2(ITH)		projected area
C     BOL1		sum over VITG3(ITH)		bolometric luminos.
C     SMCO		sum over VITG4(ITH)		visible light
C     SMDO		sum over VITG5(ITH)		eclipsed area
C     SM20A		sum over VITG6(ITH)		eclipsed light
C     
C	comparable expressions for second component
c
C
      INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
	INCLUDE 'COMN.FOR'
C
C
      DIMENSION VL1A(MTW,MTHM,MPHM),VL2A(MTW,MTHM,MPHM),
     $VL3A(MTW,MTHM,MPHM)
      DIMENSION ARYH(MPHM),ARYI(MPHM),VITG3(MTW,MPHM),VITG4(MTW,MPHM)
      DIMENSION ARYG(MPHM)
      DIMENSION VL4A(MTW,MTHM,MPHM),VL5A(MTW,MTHM,MPHM),ARYJ(MPHM),
     $ARYK(MPHM),
     $VITG5(MTW,MPHM),VITG6(MTW,MPHM),AR3(MTW,MTHM,MPHM),
     $VITG1(MTW,MPHM),VITG1A(MTW,MPHM),
     $VITG2(MTW,MPHM),
     $DSGA(MTW,MTHM,MPHM),DSGAF(MTW,MTHM,MPHM)
C
      DIMENSION STVS(MTW,MTHM,MPHM),ARYF(MTHM),ARYFA(MTHM)
	DIMENSION XVL(MTW,MTHM,MPHM),YVL(MTW,MTHM,MPHM)
	DIMENSION SHADXST(MTHM,3),SHADYST(MTHM,3),SHADXND(MTHM,3),
     $SHADYND(MTHM,3)
	DIMENSION XHORZU(2,MTHM),YHORZU(2,MTHM),XHORZL(2,MTHM),
     $YHORZL(2,MTHM)
	DIMENSION XTP(MTHM),YTP(MTHM),SPL1U(MTHM),SPL2U(MTHM),
     $SPL1L(MTHM),SPL2L(MTHM)
	DIMENSION XRF(MTHM),YRF(MTHM)
C
	DIMENSION XHZBI(10000),YHZBI(10000),XHZEI(10000),YHZEI(10000)
	DIMENSION XHZB3MI(10000),YHZB3MI(10000),XHZE3MI(10000),
     $YHZE3MI(10000)      
C
C
      DIMENSION UC(MTW),C1(MTW),C2(MTW),T0(MTW),A(MTW),BCF(MTW),
     $UA(MTW),
     $WL(MWL),
     $VLPSI(MPHS),LV(13)
      DIMENSION FL1(17)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 36 FL1,BFR
      CHARACTER * 7 LABEL
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD, KEYBAD
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,66,7,8,9/
      DATA NOUT1,NOUT2,NOUT3,IN13/12,13,14,15/
	DATA NOUT4,NOUT5,NOUT6,NOUT7,NOUT8/16,17,18,19,20/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/ 
C     Solar bolometric output in ergs/sec.
      DATA SOLF/3.8268D33/
C     Factor for producing bolometric abs. mag.
      DATA CVMG/4.75D0/ 
C     Radiation constant
      DATA SGMA/5.6696D-5/	                         
C  100 FORMAT (A36)
C  101 FORMAT (I2)
C  102 FORMAT (1H )
C  104 FORMAT (1X,'PROGRAM PGD',30X,'DATE=',I2,':',I2,':',I4,2X,
C     $'TIME=',I2,':',I2,':',I2,/)
C  105 FORMAT (1X,'Number of values of orbital longitude=',I5,/,1X, 
C     $'Ordinal value of starting orbital longitude=',I11,/,1X,
C     $'Ending value of orbital longitude=',I5)     
C  106 FORMAT (10I5)
C  107 FORMAT (6E20.12)     
C  108 FORMAT (6F13.8)   
C  109 FORMAT (2F18.13)
C  110 FORMAT (3F18.13)   
C  128 FORMAT (7F18.13)  
C  111 FORMAT (1X,'Calculated surface area,component',I10,1X,'=',
C     $E14.6,/,43X,'+/-',E14.6,'cm**2')     
C  112 FORMAT (1X,'Calculated bolom. luminosity,component',I3,3X,  
C     $'=',E12.4,1X,'(4 pi)',/,43X,'+/-',E12.4,/)
C  113 FORMAT (1X,'Depth of eclipse',23X,
C     $'=',F9.5,/,1X,'Projected area,component',I5,10X,'=',
C     $E14.6,1X,     
C     $'+/-',E14.6,'cm**2',/,1X,'Eclipsed area',26X,'=',E14.6,1X,'+/-',
C     $E14.6,'cm**2',/,   
C     $1X,'Value of fractional light loss',9X,'=',F9.5,2X,
C     $'(component)')  
C  114 FORMAT (2X,'Component'
C     $,I5,1X,'First order limb darkening(pole)=',F7.3,/,
C     $2X,'Second order limb darkening(pole)',14X,'=',F7.3,/,2X,
C     $'Light without eclipse',26X,'=',E13.5,/,48X,
C     $'+/-',E12.5,'ergs/sec/micron',/,2X,'Light lost by eclipse',
C     $26X,'=',E13.5,/,48X,'+/-',
C     $E12.5,'ergs/sec/micron',/,2X,'Value of fractional light loss'
C     $,17X,'=',F9.5,2X,'(component)',/)
C  115 FORMAT (2X,'Component',I5,1X,'First order limb darkening(pole)=', 
C     $F7.3,/,2X,'Second order limb darkening(pole)',14X,'=',F7.3,/,
C     $2X,'Light without eclipse',26X,'=',E13.5,/,48X,'+/-',E12.5,
C     $'ergs/sec/micron',/,2X,
C     $'Light lost by eclipse',26X,'=',E13.5,/,48X,'+/-',E12.5,
C     $'ergs/sec/micron',/,2X,'Value of fractional light loss',17X,
C     $'=',F9.5,2X,'(component)',/)
C  116 FORMAT (1X,'Depth of eclipse',23X,'=',F9.5,/,
C     $1X,'Projected area,component',I5,10X,'=',E14.6,1X,'+/-',E14.6,
C     $'cm**2',/,1X,'Eclipsed area',
C     $26X,'=',E14.6,1X,'+/-',E14.6,'cm**2',
C     $/,1X,'Value of fractional light loss',9X,'=',F9.5,2X,
C     $'(component)')     
C  117 FORMAT (1X,'Array of theta values,each component,between pole on + 
C     $Z axis and equator')     
C  118 FORMAT (1H )      
C  119 FORMAT (1X,'T(eff),component',I2,26X,'=',F8.1,2X,'K')
C  120 FORMAT (1X,'T(eff),component',I2,26X,'=',F8.1,2X,'K')
C  121 FORMAT (1X,'Monochromatic light toward observer in absence of ecli
C     $pse=',E12.5,/,1X,'Light lost by eclipse',36X,'=',E12.5,/,1X,'Net l
C     $ight toward observer',32X,'=',E12.5,/)
C  123 FORMAT ('********************************************')
C  125 FORMAT (1X,'System T(eff)',31X,'=',F8.1,2X,'K')
C  127 FORMAT (1X,'Orbital longitude=',F12.7,/,1X,'Time from mid-pri.=',
C     $F11.7,3X,'(One orbital period = two pi)',
C     $/,1X,'Ordinal orbital longitude=',I5)
C  129 FORMAT (1X,'Bolometric absolute magnitude, component',I2,2X,'=',
C     $F10.4)
C  130 FORMAT (1X,'Bolometric absolute magnitude, component',I2,2X,'=',
C     $F10.4)
C  145 FORMAT (1X,'Orbital inclination',20X,'=',F12.8,/,1X,
C     $'Ordinal value of orbital longitude',5X,'=',I3,/,1X,
C     $'Orbital longitude',22X,'=',F12.8)
C  146 FORMAT (1X,'Wavelength(microns)',20X,'=',F9.5)
C  147 FORMAT (1X,'Bolometric luminosity, system',15X,'=',E12.4,
C     $1X,'(4 pi)')
C  148 FORMAT (1X,'Bolometric absolute magnitude, system',7X,
C     $'=',F10.4,/,10X,
C     $'This is 4*pi value, omitting eclipse effects',/,10X,
C     $'Assuming isotropic radiation by system components',/,10X, 
C     $'This is the quantity measured by observer.')
C  151 FORMAT (1X,'In the following listing, component 1 always refers to
C     $ the component eclipsed at primary minimum')
C  152 FORMAT (1X,'Fractional light loss, system',20X,'=',E13.4)
C  262 FORMAT (1X,'Flux (4*PI), first component, at wavelength',1X,
C     $'=',E12.4,'ergs/sec/micron')
C  263 FORMAT (1X,'Flux (4*PI), second component, at wavelength',
C     $'=',E12.4,'ergs/sec/micron')
C  205 FORMAT (1X,'Program running time=',F12.4,2X,'seconds')
C***********************************************************
C      OPEN (UNIT=22,    FILE='PGDA.ILS',STATUS='OLD')
C      READ (22, 100)    BFR
C      OPEN (UNIT=21,     FILE=BFR,  STATUS='OLD')
C      READ ( 21, 100)    (FL1(J),J=1,17)
C      OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')
C      WRITE (IN1,101) KEYBAD
C      REWIND IN1
C      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')
C      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')
C      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')
C      OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS='OLD')
C      OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS='OLD')
C      OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS='OLD')
C      OPEN (UNIT=IN8,   FILE=FL1(8),  STATUS='OLD')
C      OPEN (UNIT=IN9,   FILE=FL1(9),  STATUS='OLD')
C      OPEN (UNIT=NOUT1, FILE=FL1(10), STATUS='REPLACE')
C      OPEN (UNIT=NOUT2, FILE=FL1(11), STATUS='REPLACE')
C      OPEN (UNIT=NOUT3, FILE=FL1(12), STATUS='REPLACE')
C      OPEN (UNIT=NOUT4, FILE=FL1(13), STATUS='REPLACE')
C      OPEN (UNIT=NOUT5, FILE=FL1(14), STATUS='REPLACE')
C      OPEN (UNIT=NOUT6, FILE=FL1(15), STATUS='REPLACE')
C      OPEN (UNIT=NOUT7, FILE=FL1(16), STATUS='REPLACE')
C      OPEN (UNIT=NOUT8, FILE=FL1(17), STATUS='REPLACE')
C***********************************************************
C     Read input data
C      LABEL = ' ***PGD'
C      NU=IN2
C      CALL GTLBL( LABEL, LBLERR)
C      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C      CALL IREAD (NPSI)	 !Number of orbital phases
C      CALL DUMMY (1)
C      CALL IREAD (NPST)	 !Ordinal of starting phase
C      CALL DUMMY (1)
C      CALL IREAD (NPSN)	 !Ordinal value of ending phase
C      CALL DUMMY (1)
C      CALL IREAD (KDERIV)	 !Key to calculate derivatives
C	CALL DUMMY (2)
C	CALL IREAD (KDARK)   !Index to choose limb darkening law
C      CALL SYSTEM_CLOCK(CLOCK1)
C      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
C      IYR=TIMEVALUES(1)
C      IMON=TIMEVALUES(2)
C      IDAY=TIMEVALUES(3)
C      IHR=TIMEVALUES(5)
C      IMIN=TIMEVALUES(6)
C      ISEC=TIMEVALUES(7)
C	CLOSE (UNIT = IN13, STATUS = 'KEEP')
C      WRITE (NOUT1,104)IMON,IDAY,IYR,IHR,IMIN,ISEC
C  
C     THE FOLLOWING ARE INSTALLATION PARAMETERS,BUT CAN BE CHANGED
C     AT INSTALLATION TIME     
C  
C      KEC=1      
C      KY=1
C      KLT=MPHS    
C  
C  
C      IF (NPST.EQ.0) NPST=1    
C      IF (NPSN.EQ.0) NPSN=NPSI 
C      IF (NPSI.NE.0) GO TO 10  
C      READ (IN6,106) KCTRL,NPSI
C      IF (NPSN.EQ.0) NPSN=NPSI  
C   10 WRITE (NOUT1,105) NPSI,NPST,NPSN
C     INPUT PERMANENT FILE DATA
C      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN4)
C      RI2=RI*RI
C      READ (IN7,106) NPSI,N1,N2,N3,N4,N5,N6
C      READ (IN7,128) (VLPSI(K),K=1,NPSI)
C      READ (IN7,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
C     $NRY10,NRY11,NRY12
C      READ (IN7,106) (LV(K),K=1,13)
C      READ (IN7,128) OMRAD,ECC,SMAJ,TH0,VCONJ,VMN0,PHZERO,THFCP,THSCP,
C     $THTCP,THLCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,THTCS,
C     $THLCS,THFOUR
C      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),    
C     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),   
C     $UA(3-I),IN8)
CC      WRITE (NOUT8,106) NPSI,NPST,NPSN,NWL
C      WRITE (NOUT1,117)     
C      WRITE (NOUT1,108) (TH(I,KK),KK=1,NTH)      
C  
C     *******************************                          
C     Loop for successive wavelengths
C     *******************************
C     This is the outermost loop
C
C      DO 50 IWL=1,NWL   
C      CALL RDWT6(I,IWL,NTH,IN9)
C      WLV=WL(IWL)
C	WRITE (NOUT5,108) WLV
C      CALL WTOUT4(I,OM,TMPR(I,1,1),TMPR(3-I,1,1),QS,QN,RI,VMI,VVI,VVJ,
C     $IWL,WLV,U1(I,1,1),U2(I,1,1),U1(3-I,1,1),U2(3-I,1,1),NOUT2)
C      IF ((THTHREE.EQ.0.D0).AND.(THFOUR.EQ.0.D0)) THEN
C         THTS1=PIO2
C         THTS2=TPIO2
C      ELSE
C         THTS1=THTHREE
C         THTS2=THFOUR
C      END IF
C      IF (PHSEC.EQ.0.D0) THEN
C         THTS3=PI
C         THTS4=0.D0
C      ELSE
C         THTS3=PHSEC
C         THTS4=PHZERO
C      END IF
C
C     PRINCIPAL LOOP,FOR SUCCESSIVE VALUES OF ORBITAL LONGITUDE   
C  
C      WRITE (NOUT1,118)
C      DO 60 KS=1,NPSN
C           
C      DO J=1,2
C		DO ITH=1,NTH
C		VITG1(J,ITH)=0.D0
C		VITG1A(J,ITH)=0.D0
C		VITG3(J,ITH)=0.D0
C		VITG4(J,ITH)=0.D0
C		VITG5(J,ITH)=0.D0
C		N=NPH(I,ITH)
C			DO K=1,N
C     Visibility keys for photosphere points, stellar components
C				STVS(J,ITH,K)=0.D0
C			END DO
C		END DO
C      END DO                                    
C     Input data from PGB
C      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
C     $PSIP,KCTRL,KFORM,IN5)
C      viv=vi*57.2957795131
C      CONTINUE 
C      WRITE (NOUT1,127) PSI,PSIP,KS
C      IF (KS.LT.NPST) GO TO 60
C     LOOP TO CALCULATE ARRAYS OF SURFACE AREA ELEMENTS AND 
C     RELATED QUANTITIES
C 
C      CSI=-DCOS(VI)
C      SNI=DSIN(VI)
C     Set filling factor from PGB
C	FILLF=FL
C     SET TO LOOK AT OPPOSED HEMISPHERE OF ECLIPSING STAR
C      PSI=PSIP  
C	CM=TWPI-PSI
C      AG=0.D0
C      IF (ISV.EQ.IEC) AG=PI
C     Set control for quadrant in which PSIP is located
C      IF ((PSIP.GE.PHTS4).AND.(PSIP.LE.PHTS1)) ISEQ=1
C      IF ((PSIP.GT.PHTS1).AND.(PSIP.LE.PHTS3)) ISEQ=2
C      IF ((PSIP.GT.PHTS3).AND.(PSIP.LE.PHTS2)) ISEQ=3
C      IF ((PSIP.GT.PHTS2).AND.(PSIP.LT.PHTS4+TWPI)) ISEQ=4
C
C      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
C      CALL PROJ(TARY,VRD,0.0D0,0.0D0,XG,YG,ZG)
C      XC=QS/(1.D0+QS)
C      IF (IEC.EQ.ISV) XC=QN/(1.D0+QN)
C
C      CALL PROJ(TARY,XC*VRD,0.D0,0.D0,XCMS,YCMS,ZCM)
C      IF (IEC.EQ.ISV) XCMS=XCMS-XG
C      IF (IEC.EQ.ISV) YCMS=YCMS-YG
C      XCM=YCMS
C      YCM=XCMS
C
C      IF (I.NE.ISV) THEN
C		XCMV=XC*VRD
C	ELSE
C		XCMV=-(1.D0-XC)*VRD
C	END IF
C      ANGLO=0.D0
C      CSPJ=DCOS(ANGLO)
C      SNPJ=DSIN(ANGLO)
C	I=1
C	DO ITH=1,NTH
C	NPHV=NPH(I,ITH)
C	DO K=1,NPHV
C	WRITE(NOUT1,2005)ITH,K,CSG(I,ITH,K)  
C	END DO
C	END DO
C 2005 FORMAT (2I5,2F15.10)
C
C     Calculate projection grid of photospheric points
C	DO II=1,2
C     Go from upper pole to equator
C	KCT=0
C      DO ITH=1,NTH/2+1
C		NPHV=NPH(II,ITH)
C     Initialize KCTI at each ITH value
C		DO K=1,NPHV
C				CSGM=CSG(II,ITH,K)
C				X=VR(II,ITH,K)*ALM(II,ITH,K)
C				Y=VR(II,ITH,K)*AMU(II,ITH,K)
C				Z=VR(II,ITH,K)*ANU(II,ITH,K)
C				IF (I.NE.ISV) X=X-VRD
C				CALL PROJ(TARY,X,Y,Z,PJX,PJY,PJZ)
C				Y=PJX*CSPJ-PJY*SNPJ
C				X=PJX*SNPJ+PJY*CSPJ
C				Z=PJZ
C				X=X-XCM
C				Y=-Y+YCM
C				Z=Z-ZCM
C				XVL(II,ITH,K)=X
C				YVL(II,ITH,K)=Y
C     Tabulate horizon points
C				IF (DABS(CSGM).LT.1.0D-8) THEN
C					KCT=KCT+1
C					XHORZU(II,KCT)=X
C					YHORZU(II,KCT)=Y
C	WRITE (NOUT1,2005) ITH,K,X,Y
C				END IF
C		END DO
C	END DO
C	IF (II.EQ.3-IEC) THEN
C	KCTA1=KCT
C	KCT=0
C	END IF
C	IF (II.EQ.IEC) THEN
C	KCTA2=KCT
C	KCT=0
C	END IF
C     Go from equator to lower pole
C      DO ITH=NTH/2+1,NTH
C		NPHV=NPH(I,ITH)
C		DO K=1,NPHV
C				CSGM=CSG(II,ITH,K)
C				X=VR(II,ITH,K)*ALM(II,ITH,K)
C				Y=VR(II,ITH,K)*AMU(II,ITH,K)
C				Z=VR(II,ITH,K)*ANU(II,ITH,K)
C				IF (I.NE.ISV) X=X-VRD
C				CALL PROJ(TARY,X,Y,Z,PJX,PJY,PJZ)
C				Y=PJX*CSPJ-PJY*SNPJ
C				X=PJX*SNPJ+PJY*CSPJ
C				Z=PJZ
C				X=X-XCM
C				Y=-Y+YCM
C				Z=Z-ZCM
C				XVL(II,ITH,K)=X
C				YVL(II,ITH,K)=Y
C     Tabulate horizon points
C				IF (DABS(CSGM).LT.1.0D-8) THEN
C					KCT=KCT+1
C					XHORZL(II,KCT)=X
C					YHORZL(II,KCT)=Y
C				END IF
C		END DO
C	END DO
C	IF (II.EQ.3-IEC) THEN
C	KCTB1=KCT
C	KCT=0
C	END IF
C	IF (II.EQ.IEC) THEN
C	KCTB2=KCT
C	KCT=0
C	END IF
C      END DO
C     Sort upper horizon points
C      II=3-IEC
C	DO L1=1,KCTA1-1
C		DO L2=1,KCTA1-1
C			IF (XHORZU(II,L2).GT.XHORZU(II,L2+1)) THEN
C				SV=XHORZU(II,L2+1)
C				XHORZU(II,L2+1)=XHORZU(II,L2)
C				XHORZU(II,L2)=SV
C				SV=YHORZU(II,L2+1)
C				YHORZU(II,L2+1)=YHORZU(II,L2)
C				YHORZU(II,L2)=SV
C			END IF	
C		END DO
C	END DO
C      II=IEC
C	DO L1=1,KCTA2-1
C		DO L2=1,KCTA2-1
C			IF (XHORZU(II,L2).GT.XHORZU(II,L2+1)) THEN
C				SV=XHORZU(II,L2+1)
C				XHORZU(II,L2+1)=XHORZU(II,L2)
C				XHORZU(II,L2)=SV
C				SV=YHORZU(II,L2+1)
C				YHORZU(II,L2+1)=YHORZU(II,L2)
C				YHORZU(II,L2)=SV
C			END IF	
C		END DO
C	END DO
C	DO ITH=1,KCTA1
C	WRITE (NOUT1,2001)XHORZU(3-IEC,ITH),YHORZU(3-IEC,ITH) 
C	END DO
C	WRITE (NOUT1,2002)
C	DO ITH=1,KCTA2
C	WRITE (NOUT1,2001)XHORZU(IEC,ITH),YHORZU(IEC,ITH) 
C	END DO
C	WRITE (NOUT1,2002)
C	WRITE (NOUT1,2002)
C     Sort lower horizon points
C      II=3-IEC
C	DO L1=1,KCTB1-1
C		DO L2=1,KCTB1-1
C			IF (XHORZL(II,L2).GT.XHORZL(II,L2+1)) THEN
C				SV=XHORZL(II,L2+1)
C				XHORZL(II,L2+1)=XHORZL(II,L2)
C				XHORZL(II,L2)=SV
C				SV=YHORZL(II,L2+1)
C				YHORZL(II,L2+1)=YHORZL(II,L2)
C				YHORZL(II,L2)=SV
C			END IF	
C		END DO
C	END DO
C      II=IEC
C	DO L1=1,KCTB2-1
C		DO L2=1,KCTB2-1
C			IF (XHORZL(II,L2).GT.XHORZL(II,L2+1)) THEN
C				SV=XHORZL(II,L2+1)
C				XHORZL(II,L2+1)=XHORZL(II,L2)
C				XHORZL(II,L2)=SV
C				SV=YHORZL(II,L2+1)
C				YHORZL(II,L2+1)=YHORZL(II,L2)
C				YHORZL(II,L2)=SV
C			END IF	
C		END DO
C	END DO
C	DO ITH=1,KCTB1
C	WRITE (NOUT1,2001)XHORZL(3-IEC,ITH),YHORZL(3-IEC,ITH) 
C	END DO
C	WRITE (NOUT1,2002)
C	DO ITH=1,KCTB2
C	WRITE (NOUT1,2001)XHORZL(IEC,ITH),YHORZL(IEC,ITH) 
C	END DO
C	WRITE (NOUT1,2002)
C     Following calculation only if an eclipse is present
C      IF (SP.GT.1.D0) GO TO 49
C     Calculate SPLINE arrays
C	DO K=1,KCTA1
C		XTP(K)=XHORZU(3-IEC,K)
C		YTP(K)=YHORZU(3-IEC,K)
C	END DO
C	YP1=1.0E10
C	YPN=-1.0E10
C      CALL SPLINE(XTP,YTP,KCTA1,YP1,YPN,SPL1U)
C
C	DO K=1,KCTA2
C		XTP(K)=XHORZU(IEC,K)
C		YTP(K)=YHORZU(IEC,K)
C	END DO
C	YP1=1.0E10
C	YPN=-1.0E10
C	CALL SPLINE(XTP,YTP,KCTA2,YP1,YPN,SPL2U)
C
C	DO K=1,KCTB1
C		XTP(K)=XHORZL(3-IEC,K)
C		YTP(K)=YHORZL(3-IEC,K)
C	END DO
C	YP1=-1.0E10
C	YPN=1.0E10
C	CALL SPLINE(XTP,YTP,KCTB1,YP1,YPN,SPL1L)
C
C	DO K=1,KCTB2
C		XTP(K)=XHORZL(IEC,K)
C		YTP(K)=YHORZL(IEC,K)
C	END DO
C	YP1=-1.0E10
C	YPN=1.0E10
C	CALL SPLINE(XTP,YTP,KCTB2,YP1,YPN,SPL2L)
C   49 CONTINUE
C
C     Read ALP(),RS() values
C      READ (IN3,106) ICNT,IFC,IRC,NKP
C	READ (IN3,109) (ALP(J,1),RS(J,1),J=1,IFC)
C	READ (IN3,109) (ALP(J,2),RS(J,2),J=1,IRC)
C	IF (NKP.GT.0) THEN
C	READ (IN3,109) (ALP(J,3),RS(J,3),J=1,NKP)
C	END IF
C	READ (IN3,110) (ARYA(J,1),ARYA(J,2),ARYA(J,3),J=1,IFC)
C	READ (IN3,110) (ARYB(J,1),ARYB(J,2),ARYB(J,3),J=1,IRC)
C	IF (NKP.GT.0) THEN
C	READ (IN3,110) (ARYC(J,1),ARYC(J,2),ARYC(J,3),J=1,NKP)
C	END IF
C	IF (I.EQ.IEC) THEN
C		ICOUNT=1
C	ELSE
C		ICOUNT=2
C	END IF
C	IF (ICOUNT.EQ.1) THEN
C	CALL LGTHETA(I,IEC,IFC,ISV,IRC,NKP,ICOUNT,LCT,XG,YG,XCM,YCM,SNPJ,
C     $CSPJ,ISEQ,NPBA,KS,XHZBI,YHZBI,NPBB,XHZB3MI,YHZB3MI)
C     Read large theta branch, eclipsing star
C      NPB=NPBA
C	IF (NPB.GT.10000) CALL PMDSTOP
C	END IF
C
C     Extend polar values to full grid size
C	DO II=1,2
C		NPHV=NPH(I,NTH/2+1)
C		DO K=2,NPHV
C			XVL(II,1,K)=XVL(II,1,1)
C			YVL(II,1,K)=YVL(II,1,1)
C			XVL(II,NTH,K)=XVL(II,NTH,1)
C			YVL(II,NTH,K)=YVL(II,NTH,1)
C		END DO
C	END DO
C     Calculate projected points for start and end of eclipse shadows
C      I=3-IEC
C      DO 41 ITH=1,NTH
C	    ANG1=0.D0
C		ANG2=0.D0
C		ANG3=0.D0
C		ANG4=0.D0
C		ANG5=0.D0
C		ANG6=0.D0
C     Test for no shadow boundary on that ITH value
C		ANG1=SHS(ITH,1)
C		IF (DABS(ANG1-6.29D0).LT.1.D-13) GO TO 41
C                IF (DABS(ANG1-6.29D0).GT.1.D-13) THEN
C		ANG2=SHN(ITH,1)
C		ANG3=SHS(ITH,2)
C		ANG4=SHN(ITH,2)
C		ANG5=SHS(ITH,3)
C		ANG6=SHN(ITH,3)
C		VSNTH=DSIN(TH(I,ITH))
C		VCSTH=DCOS(TH(I,ITH))
C		VSNPH1=DSIN(ANG1)
C		VCSPH1=DCOS(ANG1)
C		VSNPH2=DSIN(ANG2)
C		VCSPH2=DCOS(ANG2)
C		IF (DABS(ANG3).LT.6.29D0) THEN
C			VSNPH3=DSIN(ANG3)
C			VCSPH3=DCOS(ANG3)
C		END IF
C		IF (DABS(ANG4).LT.6.29D0) THEN
C			VSNPH4=DSIN(ANG4)
C			VCSPH4=DCOS(ANG4)
C		END IF
C		IF (DABS(ANG5).LT.6.29D0) THEN
C			VSNPH5=DSIN(ANG5)
C			VCSPH5=DCOS(ANG5)
C		END IF
C		IF (DABS(ANG6).LT.6.29D0) THEN
C			VSNPH6=DSIN(ANG6)
C			VCSPH6=DCOS(ANG6)
C		END IF
C     Locate K index corresponding to ANG1
C		NPHV=NPH(I,ITH)
C		DO K=1,NPHV-1
C			IF ((ANG1.GE.PH(I,ITH,K)).AND.(ANG1.LT.PH(I,ITH,K+1)))
C     $			THEN
C				KVL=K
C				GO TO 30
C			END IF
C			IF (ANG1.EQ.PH(I,ITH,NPHV)) THEN
C				KVL=NPHV
C				GO TO 30
C			END IF
C		END DO
C   30		VRAD1=VR(I,ITH,KVL)
C		XTMP=VRAD1*VSNTH*VCSPH1
C		YTMP=VRAD1*VSNTH*VSNPH1
C		ZTMP=VRAD1*VCSTH
C		IF (I.NE.ISV) XTMP=XTMP-VRD
C		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
C		YCU=PJX*CSPJ-PJY*SNPJ
C		XCU=PJX*SNPJ+PJY*CSPJ
C		ZCU=PJZ
C		XCU=XCU-XCM
C		YCU=-YCU+YCM
C     Projected coods.corresponding to ANG1
C		SHADXST(ITH,1)=XCU
C		SHADYST(ITH,1)=YCU
C
C     Locate K index corresponding to ANG2
C		DO K=1,NPHV-1
C			IF ((ANG2.GE.PH(I,ITH,K)).AND.(ANG2.LT.PH(I,ITH,K+1)))
C     $			THEN
C				KVL=K
C				GO TO 31
C			END IF
C			IF (ANG2.EQ.PH(I,ITH,NPHV)) THEN
C				KVL=NPHV
C				GO TO 31
C			END IF			
C		END DO		  
C   31		VRAD2=VR(I,ITH,KVL)
C		XTMP=VRAD2*VSNTH*VCSPH2
C		YTMP=VRAD2*VSNTH*VSNPH2
C		ZTMP=VRAD2*VCSTH
C		IF (I.NE.ISV) XTMP=XTMP-VRD
C		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
C		YCU=PJX*CSPJ-PJY*SNPJ
C		XCU=PJX*SNPJ+PJY*CSPJ
C		ZCU=PJZ
C		XCU=XCU-XCM
C		YCU=-YCU+YCM
C     Projected coods. corresponding to ANG2
C		SHADXND(ITH,1)=XCU
C		SHADYND(ITH,1)=YCU
C
C     Locate K index corresponding to ANG3
C		DO K=1,NPHV-1
C			IF ((ANG3.GE.PH(I,ITH,K)).AND.(ANG3.LT.PH(I,ITH,K+1)))
C     $			THEN
C				KVL=K
C				GO TO 43
C			END IF
C			IF (ANG3.EQ.PH(I,ITH,NPHV)) THEN
C				KVL=NPHV
C				GO TO 43
C			END IF			
C		END DO		  
C   43		VRAD3=VR(I,ITH,KVL)
C		XTMP=VRAD3*VSNTH*VCSPH3
C		YTMP=VRAD3*VSNTH*VSNPH3
C		ZTMP=VRAD3*VCSTH
C		IF (I.NE.ISV) XTMP=XTMP-VRD
C		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
C		YCU=PJX*CSPJ-PJY*SNPJ
C		XCU=PJX*SNPJ+PJY*CSPJ
C		ZCU=PJZ
C		XCU=XCU-XCM
C		YCU=-YCU+YCM
C     Projected coods. corresponding to ANG3
C		SHADXST(ITH,2)=XCU
C		SHADYST(ITH,2)=YCU
C     Locate K index corresponding to ANG4
C		DO K=1,NPHV-1
C			IF ((ANG4.GE.PH(I,ITH,K)).AND.(ANG4.LT.PH(I,ITH,K+1)))
C     $			THEN
C				PHVVL=PH(I,ITH,K)
C				PHVVVL=PH(I,ITH,K+1)
C				KVL=K
C				GO TO 44
C			END IF
C		END DO		  
C		IF (DABS(ANG4-PH(I,ITH,NPHV)).LT.1.0D-13) THEN
C			KVL=NPHV
C			GO TO 44
C		END IF
C		CALL PMDSTOP			
C   44		VRAD4=VR(I,ITH,KVL)
C		XTMP=VRAD4*VSNTH*VCSPH4
C		YTMP=VRAD4*VSNTH*VSNPH4
C		ZTMP=VRAD4*VCSTH
C		IF (I.NE.ISV) XTMP=XTMP-VRD
C		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
C		YCU=PJX*CSPJ-PJY*SNPJ
C		XCU=PJX*SNPJ+PJY*CSPJ
C		ZCU=PJZ
C		XCU=XCU-XCM
C		YCU=-YCU+YCM
C     Projected coods. corresponding to ANG4
C		SHADXND(ITH,2)=XCU
C		SHADYND(ITH,2)=YCU
C        END IF
C   41	END DO
C     Calculate projected areas of both stars
C     Initialize projected areas
C      SM10A=0.D0
C	SM10B=0.D0
C
C
C
C     ----------------
C     First Component
C     ----------------
C      
C      SURF1=0.D0
C      SURF2=0.D0
C      SM10A=0.D0
C      SM20A=0.D0
C      BOL1=0.D0
C      SMCO=0.D0
C      SMDO=0.D0
C      BOL2=0.D0
C      SM10B=0.D0
C      SM20B=0.D0
C      SMEO=0.D0
C      SMFO=0.D0            
C      CF=RI*RI
C      VTHST=0.D0
C	FLUX1=0.D0
C	FLUX2=0.D0
C     -----------------
C      DO 22 ITH=1,NTH
C      VITG1(I,ITH)=0.D0
C	VITG1A(I,ITH)=0.D0
C      VITG2(I,ITH)=0.D0
C      VITG3(I,ITH)=0.D0
C      VITG4(I,ITH)=0.D0
C      VITG5(I,ITH)=0.D0
C      VITG6(I,ITH)=0.D0
C      IF (ITH.EQ.1) THEN
C         VTHA=VTHST
C      ELSE
C         VTHA=VTH
C      END IF
C	VTH=TH(I,ITH)
C      CSTH1=DCOS(VTHA)
C      CSTH2=DCOS(VTH)
C      DLCS=CSTH1-CSTH2
C      N=NPH(I,ITH)      
C      IF (N.LE.0) CALL PMDSTOP
C      IF (ITH.EQ.1) GO TO 22
C      IF (ITH.LT.NTH) THEN
C
C         DO 20 K=1,N  
C     Set visibility keys for eclipsing component.
C     First set keys negative for points below horizon.
C         IF (CSG(I,ITH,K).LT.0.D0) STVS(I,ITH,K)=CSG(I,ITH,K)
C	   VPHV=PH(I,ITH,K)
C     Initialize all points between start and end of visible
C     limits to visibility true condition. 
C         IF ((VPHV.GE.VST(I,ITH,1)).AND.(VPHV.LE.VND(I,ITH,1)))
C     $   STVS(I,ITH,K)=1.D0
C         IF ((VPHV.GE.VST(I,ITH,2)).AND.(VPHV.LE.VND(I,ITH,2)))
C     $   STVS(I,ITH,K)=1.D0
C         IF ((VPHV.GE.VST(I,ITH,3)).AND.(VPHV.LE.VND(I,ITH,3)))
C     $   STVS(I,ITH,K)=1.D0
C     Set visibility keys to allow for eclipse effects.
C         IF (I.NE.IEC) THEN
C			IF ((VPHV.GE.SHS(ITH,1)).AND.(VPHV.LE.SHN(ITH,1)))
C     $		THEN 
C      		IF (DABS(SHS(ITH,1)-SHN(ITH,1)).GT.1.0E-12)
C     $			STVS(I,ITH,K)=-1.D0
C			END IF
C			IF ((VPHV.GE.SHS(ITH,2)).AND.(VPHV.LE.SHN(ITH,2)))
C     $        THEN
C              IF (DABS(SHS(ITH,2)-SHN(ITH,2)).GT.1.0E-12) 
C     $			STVS(I,ITH,K)=-1.D0
C			END IF
C			IF ((VPHV.GE.SHS(ITH,3)).AND.(VPHV.LE.SHN(ITH,3)))
C     $		THEN
C			IF (DABS(SHS(ITH,3)-SHN(ITH,3)).GT.1.0E-12)
C     $			STVS(I,ITH,K)=-1.0
C			END IF
C	   END IF
C     DSGA REPRESENTS AN ORDINATE OF FN. TO BE INTEGRATED FOR SURFACE
C     AREA
C     AR3() contains the longitudes on the star of the grid points 
C	   VPHV=PH(I,ITH,K) 
C         AR3(I,ITH,K)=VPHV
C         DSGA(I,ITH,K)=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)
C	   DSGAF(I,ITH,K)=DSGA(I,ITH,K)*BDI(I,ITH,K)*PI
C         VL1A(I,ITH,K)=DABS(CSG(I,ITH,K))*DSGA(I,ITH,K)
C     AA is a variable to include segments that are visible.
C     BB is a variable that ultimately will include areas that
C     are eclipsed.
C         AA=1.0
C         BB=0.D0
C     If the point is below the horizon, set AA=0.
C         IF (CSG(I,ITH,K).LT.0.D0) AA=0.D0
C         IF (CSG(I,ITH,K).GE.0.D0) BB=1.D0
C     Initially, retain those areas that are above horizon.
C         VL5A(I,ITH,K)=BB*VL1A(I,ITH,K)         
C         VL1A(I,ITH,K)=AA*VL1A(I,ITH,K)
C	   CSGABS=DABS(CSG(I,ITH,K))
C		IF (KDARK.EQ.1) THEN
C			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
C     $		-U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
C		END IF
C		IF (KDARK.EQ.2) THEN
C			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
C     $		-U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
C		END IF
C		IF (KDARK.EQ.3) THEN
C			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
C     $		-U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
C		END IF
C         VL2A(I,ITH,K)=BDI(I,ITH,K)*CFA*AA*VL1A(I,ITH,K)
C         VL4A(I,ITH,K)=BDI(I,ITH,K)*CFA*BB*VL1A(I,ITH,K)
C         VL3A(I,ITH,K)=SGMA*(1.0D3*TMPR(I,ITH,K))**4
C     $    *DSGA(I,ITH,K)*CF    
C         IF (K.EQ.1) GO TO 20   
C     Accumulate integrals for area and related calculations
C         Y1=DSGA(I,ITH,K-1)
C         Y2=DSGA(I,ITH,K)
C	   Y1A=DSGAF(I,ITH,K-1)
C	   Y2A=DSGAF(I,ITH,K)
C         Y5=VL1A(I,ITH,K-1)
C         Y6=VL1A(I,ITH,K)
C         Y9=VL3A(I,ITH,K-1)
C         Y10=VL3A(I,ITH,K)
C         Y13=VL2A(I,ITH,K-1)
C         Y14=VL2A(I,ITH,K)
C         Y17=VL5A(I,ITH,K-1)
C         Y18=VL5A(I,ITH,K)
C         Y21=VL4A(I,ITH,K-1)
C         Y22=VL4A(I,ITH,K)
C     Guarantee that corresponding longitudes on circles ITH and ITH-1
C     are the same. This means that each colatitued circle has the same 
C     number of grid points.
C         IF (ITH.GT.2) THEN
C            IF (DABS(AR3(I,ITH,K)-AR3(I,ITH-1,K)).GT.1.D-9)
C     $       CALL PMDSTOP 
C            Y3=DSGA(I,ITH-1,K-1)
C            Y4=DSGA(I,ITH-1,K)
C		  Y3A=DSGAF(I,ITH-1,K-1)
C		  Y4A=DSGAF(I,ITH-1,K)
C            Y7=VL1A(I,ITH-1,K-1)
C            Y8=VL1A(I,ITH-1,K)
C            Y11=VL3A(I,ITH-1,K-1)
C            Y12=VL3A(I,ITH-1,K)
C            Y15=VL2A(I,ITH-1,K-1)
C            Y16=VL2A(I,ITH-1,K)
C            Y19=VL5A(I,ITH-1,K-1)
C            Y20=VL5A(I,ITH-1,K)
C            Y23=VL4A(I,ITH-1,K-1)
C            Y24=VL4A(I,ITH-1,K)
C            DV=4.D0
C     Case ITH.LE.2
C         ELSE
C            Y3=0.D0
C            Y4=0.D0
C		  Y3A=0.D0
C		  Y4A=0.D0
C            Y7=0.D0
C            Y8=0.D0
C            Y11=0.D0
C            Y12=0.D0
C            Y15=0.D0
C            Y16=0.D0
C            Y19=0.D0
C            Y20=0.D0
C            Y23=0.D0
C            Y24=0.D0
C            DV=2.D0
C         END IF
C         DLPH=AR3(I,ITH,K)-AR3(I,ITH,K-1)      
C         ARYF(K)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH
C	   ARYFA(K)=(Y1A+Y2A+Y3A+Y4A)/DV*DLCS*DLPH
C         DVSV=DV
C		IF (ITH.LE.2) THEN
C         IF (((Y5+Y6).NE.0.D0).AND.((Y7+Y8).EQ.0.D0)) THEN
C			DV=2.D0
C	   ELSE		  
C			DV=DVSV
C	   END IF
C		END IF
C         ARYG(K)=(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH
C	   VL1(I,ITH,K)=ARYG(K)
C         ARYH(K)=(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH
C         ARYI(K)=(Y13+Y14+Y15+Y16)/DV*DLCS*DLPH
C     Do approximation to fraction of segment eclipsed when shadow
C     crosses the segment
C	   BB=0.D0
C         IF ((K.GT.1).AND.(ITH.GE.2)) THEN
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,K) references the lower 
C     right corner of the area segment.
C			CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1)+CSG(I,ITH-1,K-1)+
C     $				CSG(I,ITH-1,K))/4.D0
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
C			IF (CSGM.GT.0.D0) THEN
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $					.LT.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $						(STVS(I,ITH-1,K).LT.0.D0)) THEN
C						BB=1.D0
C						GO TO 33
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $					.LT.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) 
C     $				THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $				.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
C     $					CSG(I,ITH-1,K-1)).GT.1.D-05)) THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K))
C      				BB=BB/(BB+STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $				.LT.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH,K)
C     $					-CSG(I,ITH,K)).GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
C     $					.GT.1.D-05)) THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $					STVS(I,ITH-1,K-1))
C      				BB=BB/(BB+STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $				.LT.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $					.GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
C     $					THEN
C      					BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $						STVS(I,ITH-1,K))
C      					BB=BB/(BB+STVS(I,ITH-1,K-1))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $				.GE.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $					.GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
C     $					.GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
C     $			 THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
C     $					STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $				.GE.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF (DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $					.GT.1.D-05)THEN
C     					BB=DABS(STVS(I,ITH,K-1))
C					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
C     $					STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $				.GE.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $					.GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
C     $					.GT.1.D-05)) THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
C					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
C     $				.GE.0.D0))THEN 
C				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.(STVS(I,ITH-1,K)
C     $				.LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
C     $					.GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05)) 
C     $				THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K)
C     $				.LT.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $					.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
C     $					.GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05)) 
C     $				THEN
C      				BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
C     $				STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K-1))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF (DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)	
C     $				THEN
C						BB=DABS(STVS(I,ITH,K))
C						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)
C     $						+STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $					.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
C     $					.GT.1.D-05))THEN
C      					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
C						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K)
C     $				.LT.0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
C     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)
C     $					-CSG(I,ITH-1,K)).GT.1.D-05))THEN
C      					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K))
C						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K)
C     $				.GE.0.D0))THEN 
C				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.(STVS(I,ITH-1,K)
C     $				.LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
C     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
C     $						CSG(I,ITH-1,K)).GT.1.D-05)) THEN
C      					BB=DABS(STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
C						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
C						BB=0.D0
C						GO TO 33
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
C     $					1.D-05) THEN
C      					BB=DABS(STVS(I,ITH-1,K-1))

C						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $						STVS(I,ITH-1,K))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF (DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
C     $					1.D-05)THEN
C      					BB=DABS(STVS(I,ITH-1,K))
C						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $						STVS(I,ITH-1,K-1))
C						GO TO 33
C					END IF
C					END IF
C				END IF
C			END IF
C		END IF
C   33    CONTINUE
C         ARYJ(K)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
C         ARYK(K)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
C         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
C	   VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
C         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
C         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
C         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
C         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
C         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
C   20    CONTINUE
C     ITH=NTH in following section.
C      ELSE
C         N=NPH(I,ITH-1)
C         DO K=2,N
C            Y1=DSGA(I,ITH-1,K-1)
C            Y2=DSGA(I,ITH-1,K)
C	      Y1A=Y1*BDI(I,ITH-1,K-1)
C	      Y2A=Y2*BDI(I,ITH-1,K)
C            Y5=VL1A(I,ITH-1,K-1)
C            Y6=VL1A(I,ITH-1,K)
C            Y9=VL3A(I,ITH-1,K-1)
C            Y10=VL3A(I,ITH-1,K)
C            Y13=VL2A(I,ITH-1,K-1)
C            Y14=VL2A(I,ITH-1,K)
C		  CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1))/2.D0
C		  BB=0.D0
C		  IF (CSGM.GT.0.D0) THEN
C			IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.0.D0)) 
C     $			BB=1.D0
C		  END IF 
C            Y17=VL5A(I,ITH-1,K-1)*BB
C            Y18=VL5A(I,ITH-1,K)*BB
C            Y21=VL4A(I,ITH-1,K-1)*BB
C            Y22=VL4A(I,ITH-1,K)*BB
C            DLPH=AR3(I,ITH-1,K)-AR3(I,ITH-1,K-1)
C            DV=2.D0
C            ARYF(K)=(Y1+Y2)/DV*DLCS*DLPH
C	      ARYFA(K)=(Y1A+Y2A)/DV*DLCS*DLPH
C            ARYG(K)=(Y5+Y6)/DV*DLCS*DLPH
C	      VL1(I,ITH,K)=ARYG(K)
C            ARYH(K)=(Y9+Y10)/DV*DLCS*DLPH
C            ARYI(K)=(Y13+Y14)/DV*DLCS*DLPH
C            ARYJ(K)=(Y17+Y18)/DV*DLCS*DLPH
C            ARYK(K)=(Y21+Y22)/DV*DLCS*DLPH
C            VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
C	      VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
C            VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
C            VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
C            VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
C            VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
C            VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
C         END DO
C      END IF         
C   22 CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE (NOUT1,1000) (VITG2(I,ITH)*RI2,ITH=1,NTH)
C	write (nout1,1001)
C	IF (I.LT.1000) STOP 'PGD: I < 1000'
C 1000 FORMAT (6E12.5)
C 1001 FORMAT (' ')
C     ----------------------------
C     Sum contributions from all latitude strips, from 
C     upper pole to lower pole.    
C      DO ITH=1,NTH
C         SURF1=SURF1+VITG1(I,ITH)
C	   FLUX1=FLUX1+VITG1A(I,ITH)
C         SM10A=SM10A+VITG2(I,ITH)
C         BOL1=BOL1+VITG3(I,ITH)
C         SMCO=SMCO+VITG4(I,ITH)
C         SMDO=SMDO+VITG6(I,ITH)
C         SM20A=SM20A+VITG5(I,ITH)
C      END DO
C
C
C     Set component pointer to other star.
C     ------------------------------------  
C      I=3-I 
C  
C     CORRESPONDING LOOP FOR SECOND COMPONENT
C
C      VTHST=0.D0
C	  
C
C      DO 42 ITH=1,NTH
C      VITG1(I,ITH)=0.D0
C	VITG1A(I,ITH)=0.D0
C      VITG2(I,ITH)=0.D0
C      VITG3(I,ITH)=0.D0
C      VITG4(I,ITH)=0.D0
C      VITG5(I,ITH)=0.D0
C      VITG6(I,ITH)=0.D0
C      IF (ITH.EQ.1) THEN
C         VTHA=VTHST
C      ELSE
C         VTHA=VTH
C      END IF
C	VTH=TH(I,ITH)
C      CSTH1=DCOS(VTHA)
C      CSTH2=DCOS(VTH)
C      DLCS=CSTH1-CSTH2      
C      N=NPH(I,ITH)      
C      IF (N.LE.0) CALL PMDSTOP 
C      IF (ITH.EQ.1) GO TO 42
C      IF (ITH.LT.NTH) THEN
C
C         DO 40 K=1,N
C     Set visibility keys for eclipsing component.
C     First set keys negative for points below horizon.
C         IF (CSG(I,ITH,K).LT.0.D0) STVS(I,ITH,K)=CSG(I,ITH,K)
C	   VPHV=PH(I,ITH,K)
C     Initialize all points between start and end of visible
C     limits to visibility true condition. 
C         IF ((VPHV.GE.VST(I,ITH,1)).AND.(VPHV.LE.VND(I,ITH,1)))
C     $   STVS(I,ITH,K)=1.D0
C         IF ((VPHV.GE.VST(I,ITH,2)).AND.(VPHV.LE.VND(I,ITH,2)))
C     $   STVS(I,ITH,K)=1.D0
C         IF ((VPHV.GE.VST(I,ITH,3)).AND.(VPHV.LE.VND(I,ITH,3)))
C     $   STVS(I,ITH,K)=1.D0
C     Set visibility keys to allow for eclipse effects.
C     Note that these are eclipses due to companion star.
C         IF (I.NE.IEC) THEN
C			IF ((VPHV.GE.SHS(ITH,1)).AND.(VPHV.LE.SHN(ITH,1)))
C     $		THEN 
C      		IF (DABS(SHS(ITH,1)-SHN(ITH,1)).GT.1.0E-12)
C     $			STVS(I,ITH,K)=-1.D0
C			END IF
C			IF ((VPHV.GE.SHS(ITH,2)).AND.(VPHV.LE.SHN(ITH,2)))
C     $        THEN
C              IF (DABS(SHS(ITH,2)-SHN(ITH,2)).GT.1.0E-12) 
C     $			STVS(I,ITH,K)=-1.D0
C			END IF
C			IF ((VPHV.GE.SHS(ITH,3)).AND.(VPHV.LE.SHN(ITH,3)))
C     $		THEN
C			IF (DABS(SHS(ITH,3)-SHN(ITH,3)).GT.1.0E-12)
C     $			STVS(I,ITH,K)=-1.0
C			END IF
C	   END IF
C     DSGA represents an ordinate of fn. to be integrated for surface
C     area.
C     AR3() contains the longitudes on the star of the grid points.	      
C		VPHV=PH(I,ITH,K)
C         AR3(I,ITH,K)=VPHV
C         DSGA(I,ITH,K)=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)
C	   DSGAF(I,ITH,K)=DSGA(I,ITH,K)*BDI(I,ITH,K)*PI
C         VL1A(I,ITH,K)=DABS(CSG(I,ITH,K))*DSGA(I,ITH,K)
C     AA is a variable to include segments that are visible.
C     BB is a variable that ultimately will include areas that
C     are eclipsed.
C         AA=1.0D0
C         BB=0.D0
C     If the point is below the horizon, set AA=0.
C         IF (CSG(I,ITH,K).LT.0.D0) AA=0.D0
C         IF (CSG(I,ITH,K).GE.0.D0) BB=1.D0
C     Initially, retain those areas that are above horizon.
C         VL5A(I,ITH,K)=BB*VL1A(I,ITH,K)          
C         VL1A(I,ITH,K)=AA*VL1A(I,ITH,K)
C	   CSGABS=DABS(CSG(I,ITH,K))
C		IF (KDARK.EQ.1) THEN
C			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
C     $		 -U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
C		END IF
C		IF (KDARK.EQ.2) THEN
C			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
C     $		 -U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
C		END IF
C		IF (KDARK.EQ.3) THEN
C			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
C     $		 -U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
C		END IF
C         VL2A(I,ITH,K)=BDI(I,ITH,K)*CFA*AA*VL1A(I,ITH,K)
C         VL4A(I,ITH,K)=BDI(I,ITH,K)*CFA*BB*VL1A(I,ITH,K)
C         VL3A(I,ITH,K)=SGMA*(1.0D3*TMPR(I,ITH,K))**4
C     $    *DSGA(I,ITH,K)*CF    
C         IF (K.EQ.1) GO TO 40
C     Accumulate integrals for area and related calculations.
C         Y1=DSGA(I,ITH,K-1)
C         Y2=DSGA(I,ITH,K)
C	   Y1A=DSGAF(I,ITH,K-1)
C	   Y2A=DSGAF(I,ITH,K)
C         Y5=VL1A(I,ITH,K-1)
C         Y6=VL1A(I,ITH,K)
C         Y9=VL3A(I,ITH,K-1)
C         Y10=VL3A(I,ITH,K)
C         Y13=VL2A(I,ITH,K-1)
C         Y14=VL2A(I,ITH,K)
C         Y17=VL5A(I,ITH,K-1)
C         Y18=VL5A(I,ITH,K)
C         Y21=VL4A(I,ITH,K-1)
C         Y22=VL4A(I,ITH,K)
C     Guarantee that corresponding longitudes on circles ITH and ITH-1
C     are the same. This means that each colatitued circle has the same 
C     number of grid points.
C         IF (I.GT.2) THEN
C            IF (DABS(AR3(I,ITH,K)-AR3(I,ITH-1,K)).GT.1.D-9)
C     $         CALL PMDSTOP
C            Y3=DSGA(I,ITH-1,K-1)
C            Y4=DSGA(I,ITH-1,K)
C	      Y3A=DSGAF(I,ITH-1,K-1)
C	      Y4A=DSGAF(I,ITH-1,K)
C            Y7=VL1A(I,ITH-1,K-1)
C            Y8=VL1A(I,ITH-1,K)
C            Y11=VL3A(I,ITH-1,K-1)
C            Y12=VL3A(I,ITH-1,K)
C            Y15=VL2A(I,ITH-1,K-1)
C            Y16=VL2A(I,ITH-1,K)
C            Y19=VL5A(I,ITH-1,K-1)
C            Y20=VL5A(I,ITH-1,K)
C            Y23=VL4A(I,ITH-1,K-1)
C            Y24=VL4A(I,ITH-1,K)
C            DV=4.D0
C     Case ITH.LE.2
C         ELSE
C            Y3=0.D0
C            Y4=0.D0
C	      Y3A=0.D0
C	      Y4A=0.D0
C            Y7=0.D0
C            Y8=0.D0
C            Y11=0.D0
C            Y12=0.D0
C            Y15=0.D0
C            Y16=0.D0
C            Y19=0.D0
C            Y20=0.D0
C            Y23=0.D0
C            Y24=0.D0
C            DV=2.D0
C         END IF
C         DLPH=AR3(I,ITH,K)-AR3(I,ITH,K-1)
C         ARYF(K)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH
C	   ARYFA(K)=(Y1A+Y2A+Y3A+Y4A)/DV*DLCS*DLPH
C	   DVSV=DV
C		IF (ITH.LE.2) THEN
C         IF (((Y5+Y6).NE.0.D0).AND.((Y7+Y8).EQ.0.D0)) THEN
C			DV=2.D0
C	   ELSE
C			DV=DVSV
C	   END IF
C		END IF
C         ARYG(K)=(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH
C	   VL1(I,ITH,K)=ARYG(K)
C         ARYH(K)=(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH
C         ARYI(K)=(Y13+Y14+Y15+Y16)/DV*DLCS*DLPH
C     Do approximation to fraction of segment eclipsed when shadow
C     crosses the segment
C	   BB=0.D0
C         IF ((K.GT.1).AND.(ITH.GE.2)) THEN
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,ITH,K) references the lower 
C     right corner of the area segment.
C			CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1)+CSG(I,ITH-1,K-1)+
C     $			CSG(I,ITH-1,K))/4.D0
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
C			IF (CSGM.GT.0.D0) THEN
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0)) THEN
C						BB=1.D0
C						GO TO 32
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) 
C     $				THEN
C					IF ((DABS(CSG(I,ITH,K-1)-STVS(I,ITH,K-1)).GT.
C     $					1.D-05).AND.
C     $				(DABS(CSG(I,ITH-1,K-1)-STVS(I,ITH-1,K-1)).GT.
C     $					1.D-05))THEN 
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K))
C					BB=BB/(BB+STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
C     $					1.D-5).AND.(DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
C     $					.GT.1.D-5).AND.
C     $		        (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
C     $					1.D-05)) THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $					STVS(I,ITH-1,K-1))
C					BB=BB/(BB+STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
C     $					1.D-05).AND.(DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
C     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
C     $					CSG(I,ITH-1,K)).GT.1.D-05))THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $					STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH-1,K-1))
C						GO TO 32
C					END IF
C					END IF
C
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
C     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
C     $					CSG(I,ITH-1,K-1)).GT.1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
C     $					1.D-05))THEN
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
C     $					STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
C     $					1.D-5)) THEN
C					BB=DABS(STVS(I,ITH,K-1))
C					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
C     $					STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
C     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
C     $					CSG(I,ITH-1,K-1)).GT.1.D-05))THEN 
C      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
C					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
C     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
C     $					CSG(I,ITH-1,K)).GT.1.D-05)) THEN
C					BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $					.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
C     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
C     $					CSG(I,ITH-1,K)).GT.1.D-05))THEN
C      					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
C     $						STVS(I,ITH-1,K))
C						BB=BB/(BB+STVS(I,ITH,K-1))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $					0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF (DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $					THEN 
C					BB=DABS(STVS(I,ITH,K))
C					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
C     $					STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $					.AND.(DABS(STVS(I,ITH-1,K-1)-
C     $					CSG(I,ITH-1,K-1)).GT.1.D-05))THEN 
C      				BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
C					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $					.AND.(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
C     $					1.D-05))THEN 
C					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF ((DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
C     $					1.D-05).AND.
C     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
C     $					THEN 
C					BB=DABS(STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
C						BB=0.D0
C						GO TO 32
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
C     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
C					IF (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
C     $				1.D-05) THEN
C      				BB=DABS(STVS(I,ITH-1,K-1))
C					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $				STVS(I,ITH-1,K))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
C     $				0.D0))THEN
C					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
C					IF (DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
C     $					1.D-05)THEN 
C      				BB=DABS(STVS(I,ITH-1,K))
C					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
C     $					STVS(I,ITH-1,K-1))
C						GO TO 32
C					END IF
C					END IF
C				END IF
C			END IF
C		END IF
C   32    CONTINUE		 	
C         ARYJ(K)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
C         ARYK(K)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
C         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
C	   VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
C         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
C         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
C         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
C         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
C         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)   
C   40    CONTINUE
C     ITH=NTH in following section.
C      ELSE
C         N=NPH(I,ITH-1)
C         DO K=2,N
C            Y1=DSGA(I,ITH-1,K-1)
C            Y2=DSGA(I,ITH-1,K)
C	      Y1A=Y1*BDI(I,ITH-1,K-1)
C	      Y2A=Y2*BDI(I,ITH-1,K)
C            Y5=VL1A(I,ITH-1,K-1)
C            Y6=VL1A(I,ITH-1,K)
C            Y9=VL3A(I,ITH-1,K-1)
C            Y10=VL3A(I,ITH-1,K)
C            Y13=VL2A(I,ITH-1,K-1)
C            Y14=VL2A(I,ITH-1,K)
C		  CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1))/2.D0
C		  BB=0.D0
C		  IF (CSGM.GT.0.D0) THEN
C			IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.0.D0)) 
C     $			BB=1.D0
C		  END IF
C            Y17=VL5A(I,ITH-1,K-1)*BB
C            Y18=VL5A(I,ITH-1,K)*BB
C            Y21=VL4A(I,ITH-1,K-1)*BB
C            Y22=VL4A(I,ITH-1,K)*BB
C            DLPH=AR3(I,ITH-1,K)-AR3(I,ITH-1,K-1)
C	      DV=2.D0
C            ARYF(K)=(Y1+Y2)/DV*DLCS*DLPH
C	      ARYFA(K)=(Y1A+Y2A)/DV*DLCS*DLPH
C            ARYG(K)=(Y5+Y6)/DV*DLCS*DLPH
C	      VL1(I,ITH,K)=ARYG(K)
C            ARYH(K)=(Y9+Y10)/DV*DLCS*DLPH
C            ARYI(K)=(Y13+Y14)/DV*DLCS*DLPH
C            ARYJ(K)=(Y17+Y18)/DV*DLCS*DLPH
C            ARYK(K)=(Y21+Y22)/DV*DLCS*DLPH
C            VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
C	      VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
C            VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
C            VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
C            VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
C            VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)  
C            VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
C         END DO
C      END IF      
C   42 CONTINUE
C     
C  
C     Sum contributions from all latitude strips, from
C     upper pole to lower pole.
C      DO ITH=1,NTH
C         SURF2=SURF2+VITG1(I,ITH)
C	   FLUX2=FLUX2+VITG1A(I,ITH)
C         SM10B=SM10B+VITG2(I,ITH)
C         BOL2=BOL2+VITG3(I,ITH)
C         SMEO=SMEO+VITG4(I,ITH)
C         SMFO=SMFO+VITG6(I,ITH)
C         SM20B=SM20B+VITG5(I,ITH)
C      END DO
C     Reset component designations to initial values
C      I=3-I      
C      SURF1=SURF1*RI2
C      ER1A=0.001D0*SURF1
C      SM10A=SM10A*RI2
C      ER10A=0.001D0*SM10A
C      BOLER1=0.001D0*BOL1       
C      SMCOER=0.001D0*SMCO
C      SMDOER=0.001D0*SMDO
C      SM20A=SM20A*RI2
C      ER20A=0.001D0*SM20A
C	FLUX1=FLUX1*RI2    
C      SURF2=SURF2*RI2 
C      ER2A=0.001D0*SURF2
C      SM10B=SM10B*RI2
C      ER10B=0.001D0*SM10B
C      BOLER2=0.001D0*BOL2
C      SMEOER=0.001D0*SMEO
C      SMFOER=0.001D0*SMFO
C      SM20B=SM20B*RI2
C      ER20B=0.001D0*SM20B
C	FLUX2=FLUX2*RI2
C	   WRITE (NOUT1,151)
C         WRITE (NOUT1,111) I,SURF1,ER1A
C         WRITE (NOUT1,111) 3-I,SURF2,ER2A
C         WRITE (NOUT1,112) I,BOL1,BOLER1
C         WRITE (NOUT1,112) 3-I,BOL2,BOLER2
C	WRITE (NOUT1,262) FLUX1
C	WRITE (NOUT1,263) FLUX2   
C      TPRI=(BOL1/SURF1/SGMA)**0.25
C      TPSC=(BOL2/SURF2/SGMA)**0.25
C      VMG1=CVMG-2.5D0*DLOG10(BOL1/SOLF)
C      VMG2=CVMG-2.5D0*DLOG10(BOL2/SOLF)
C         WRITE (NOUT1,119) I,TPRI
C         WRITE (NOUT1,129) I,VMG1
C         WRITE (NOUT1,120) 3-I,TPSC
C         WRITE (NOUT1,130) 3-I,VMG2
C      SURFSM=SURF1+SURF2
C     Allow for both faces of accretion disk
C     Note that this calculation excludes irradiation, to permit
C     comparison with model of energy produced by mass transfer.
C      BOLSM=BOL1+BOL2
C      TAV=(BOLSM/SURFSM/SGMA)**0.25
C     Note that the following abs. mag. is as seen by the observer!
C     Only one face of accret. disk is visible, and includes 
C     irradiation      
C      VMGSM=CVMG-2.5D0*DLOG10((BOL1+BOL2)/SOLF)
C      VLTSM=SMCO+SMEO
C      VLTSB=SMDO+SMFO
C		IF (SMCO.NE.0.D0) THEN
C			VLSA=SMDO/SMCO
C		END IF
C	  IF (SMEO.NE.0.D0) THEN
C		VLSB=SMFO/SMEO
C	  END IF
C	  IF (SMEO.EQ.0.D0) VLSB=1.D99
C         WRITE (NOUT1,125) TAV
C         WRITE (NOUT1,147) BOLSM
C         WRITE (NOUT1,148) VMGSM
C
C      WRITE (NOUT1,145) VIV,KS,PSIP
C      WRITE (NOUT1,113) SP,I,SM10A,ER10A,SM20A,ER20A,VLSA
C      WRITE (NOUT1,116) SP,3-I,SM10B,ER10B,SM20B,ER20B,VLSB
C      WRITE (NOUT1,146) WLV
C      WRITE (NOUT1,114) I,U1(I,1,1),U2(I,1,1),SMCO,SMCOER,SMDO,
C     $   SMDOER,VLSA
C      WRITE (NOUT1,115) 3-I,U1(3-I,1,1),U2(3-I,1,1),SMEO,SMEOER,SMFO,
C     $   SMFOER,VLSB
C      VLT=VLTSM-VLTSB
C	IF (VLTSM.NE.0.D0) THEN
C		FLOSS=VLTSB/VLTSM
C	END IF
C      WRITE (NOUT1,121)VLTSM,VLTSB,VLT 
C      WRITE (NOUT1,152) FLOSS
C	WRITE (NOUT1,102)
C     Write VL2A() values for program RDVEL2
C      DO IPP=1,2
C		DO ITH=1,NTH
C		NN=NPH(IPP,ITH)
C			WRITE (NOUT7,106) IWL,KS,IPP,ITH,NN
C			WRITE (NOUT7,107) (VL2A(IPP,ITH,KK),KK=1,NN)
C		END DO
C	END DO
C     Write VL1() values for RDVEL2
C     Note first that the VL1 values must be summed for ITH=NTH
C      DO IPP=1,2
C		VL1(IPP,NTH,1)=0.D0
C		NN=NPH(IPP,NTH/2+1)
C		DO KK=2,NN
C			VL1(IPP,NTH,1)=VL1(IPP,NTH,1)+VL1(IPP,NTH,KK)
C		END DO
C	END DO
C      DO IPP=1,2
C		DO ITH=1,NTH
C		NN=NPH(IPP,ITH)
C			WRITE (NOUT7,106) IPP,ITH,NN
C			WRITE (NOUT7,107) (VL1(IPP,ITH,KK)*RI2,KK=1,NN)
C		END DO
C	END DO
C     Write STVS values for external check
C      DO IPP=1,2
C		DO ITH=1,NTH
C		NN=NPH(IPP,ITH)
C			WRITE (NOUT4,106) IWL,KS,IPP,ITH,NN
C			WRITE (NOUT4,162) (STVS(IPP,ITH,KK),KK=1,NN)
C		END DO
C	END DO		          
C    Write output
C      CALL WTOUT5(I,IWL,VI,PSIP,SP,SMCO,SMCOER,SMDO,SMDOER,SMEO,
C     $SMEOER,SMFO,SMFOER,VLTSM,VLTSB,1.D-3*VLTSM,VLT,1.D-3*VLT,THSCP,
C     $THFCP,SURF1,SURF2,KCTRL,NOUT3)
C      WRITE (NOUT6,107) PSIP,VI,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0
C   60 CONTINUE
C      WRITE (NOUT1,123)
C      REWIND IN4
C      REWIND IN5
C	REWIND IN8
C  160 FORMAT(8E10.3)
C  161 FORMAT (4E17.8)
C  162 FORMAT (10F8.3)
C   50 CONTINUE   
C      CONTINUE   
C      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
C      AAA=CLOCK1
C      BBB=CLOCK2
C      CCC=COUNT_RATE
C      TM=(BBB-AAA)/CCC
C      WRITE (NOUT1, 205) TM
C      CLOSE (UNIT=IN2,   STATUS='KEEP')
C      CLOSE (UNIT=IN3,   STATUS='KEEP')
C      CLOSE (UNIT=IN4,   STATUS='KEEP')
C      CLOSE (UNIT=IN5,   STATUS='KEEP')
C      CLOSE (UNIT=IN6,   STATUS='KEEP')
C      CLOSE (UNIT=IN7,   STATUS='KEEP')
C      CLOSE (UNIT=IN8,   STATUS='KEEP')
C      CLOSE (UNIT=IN9,   STATUS='KEEP')
C      CLOSE (UNIT=NOUT1, STATUS='KEEP')
C      CLOSE (UNIT=NOUT2, STATUS='KEEP')
C      CLOSE (UNIT=NOUT3, STATUS='KEEP')
C      CLOSE (UNIT=NOUT4, STATUS='KEEP')
C      CLOSE (UNIT=NOUT5, STATUS='KEEP')
C      CLOSE (UNIT=NOUT6, STATUS='KEEP')
C      CLOSE (UNIT=NOUT7, STATUS='KEEP')
C      CLOSE (UNIT=NOUT8, STATUS='KEEP')
C      CLOSE (UNIT=21,    STATUS='KEEP')
C      CLOSE (UNIT=22,    STATUS='KEEP')
C      WRITE (IN1,101) KEYGOOD
C      CLOSE (UNIT=IN1,   STATUS='KEEP')
      STOP 'PGD: Completed successfully.'
      END PROGRAM PGD
