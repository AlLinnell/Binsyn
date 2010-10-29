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
      DIMENSION FL1(18)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 36 FL1,BFR
      CHARACTER * 7 LABEL
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD, KEYBAD
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,66,7,8,9/
      DATA IN10/10/
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
  100 FORMAT (A36)
  101 FORMAT (I2)
  102 FORMAT (1H )
  104 FORMAT (1X,'PROGRAM PGD',30X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/)
  105 FORMAT (1X,'Number of values of orbital longitude=',I5,/,1X, 
     $'Ordinal value of starting orbital longitude=',I11,/,1X,
     $'Ending value of orbital longitude=',I5)     
  106 FORMAT (10I5)
  107 FORMAT (6E20.12)     
  108 FORMAT (6F13.8)   
  109 FORMAT (2F12.8)
  110 FORMAT (3F18.13)   
  128 FORMAT (7F18.13)  
  111 FORMAT (1X,'Calculated surface area,component',I10,1X,'=',
     $E14.6,/,43X,'+/-',E14.6,'cm**2')     
  112 FORMAT (1X,'Calculated bolom. luminosity,component',I3,3X,  
     $'=',E12.4,1X,'(4 pi)',/,43X,'+/-',E12.4,/)
  113 FORMAT (1X,'Depth of eclipse',23X,
     $'=',F9.5,/,1X,'Projected area,component',I5,10X,'=',
     $E14.6,1X,     
     $'+/-',E14.6,'cm**2',/,1X,'Eclipsed area',26X,'=',E14.6,1X,'+/-',
     $E14.6,'cm**2',/,   
     $1X,'Value of fractional light loss',9X,'=',F9.5,2X,
     $'(component)')  
  114 FORMAT (2X,'Component'
     $,I5,1X,'First order limb darkening(pole)=',F7.3,/,
     $2X,'Second order limb darkening(pole)',14X,'=',F7.3,/,2X,
     $'Light without eclipse',26X,'=',E13.5,/,48X,
     $'+/-',E12.5,'ergs/sec/micron',/,2X,'Light lost by eclipse',
     $26X,'=',E13.5,/,48X,'+/-',
     $E12.5,'ergs/sec/micron',/,2X,'Value of fractional light loss'
     $,17X,'=',F9.5,2X,'(component)',/)
  115 FORMAT (2X,'Component',I5,1X,'First order limb darkening(pole)=', 
     $F7.3,/,2X,'Second order limb darkening(pole)',14X,'=',F7.3,/,
     $2X,'Light without eclipse',26X,'=',E13.5,/,48X,'+/-',E12.5,
     $'ergs/sec/micron',/,2X,
     $'Light lost by eclipse',26X,'=',E13.5,/,48X,'+/-',E12.5,
     $'ergs/sec/micron',/,2X,'Value of fractional light loss',17X,
     $'=',F9.5,2X,'(component)',/)
  116 FORMAT (1X,'Depth of eclipse',23X,'=',F9.5,/,
     $1X,'Projected area,component',I5,10X,'=',E14.6,1X,'+/-',E14.6,
     $'cm**2',/,1X,'Eclipsed area',
     $26X,'=',E14.6,1X,'+/-',E14.6,'cm**2',
     $/,1X,'Value of fractional light loss',9X,'=',F9.5,2X,
     $'(component)')     
  117 FORMAT (1X,'Array of theta values,each component,between pole on + 
     $Z axis and equator')     
  118 FORMAT (1H )      
  119 FORMAT (1X,'T(eff),component',I2,26X,'=',F8.1,2X,'K')
  120 FORMAT (1X,'T(eff),component',I2,26X,'=',F8.1,2X,'K')
  121 FORMAT (1X,'Monochromatic light toward observer in absence of ecli
     $pse=',E12.5,/,1X,'Light lost by eclipse',36X,'=',E12.5,/,1X,'Net l
     $ight toward observer',32X,'=',E12.5,/)
  123 FORMAT ('********************************************')
  125 FORMAT (1X,'System T(eff)',31X,'=',F8.1,2X,'K')
  127 FORMAT (1X,'Orbital longitude=',F12.7,/,1X,'Time from mid-pri.=',
     $F11.7,3X,'(One orbital period = two pi)',
     $/,1X,'Ordinal orbital longitude=',I5)
  129 FORMAT (1X,'Bolometric absolute magnitude, component',I2,2X,'=',
     $F10.4)
  130 FORMAT (1X,'Bolometric absolute magnitude, component',I2,2X,'=',
     $F10.4)
  145 FORMAT (1X,'Orbital inclination',20X,'=',F12.8,/,1X,
     $'Ordinal value of orbital longitude',5X,'=',I3,/,1X,
     $'Orbital longitude',22X,'=',F12.8)
  146 FORMAT (1X,'Wavelength(microns)',20X,'=',F9.5)
  147 FORMAT (1X,'Bolometric luminosity, system',15X,'=',E12.4,
     $1X,'(4 pi)')
  148 FORMAT (1X,'Bolometric absolute magnitude, system',7X,
     $'=',F10.4,/,10X,
     $'This is 4*pi value, omitting eclipse effects',/,10X,
     $'Assuming isotropic radiation by system components',/,10X, 
     $'This is the quantity measured by observer.')
  151 FORMAT (1X,'In the following listing, component 1 always refers to
     $ the component eclipsed at primary minimum')
  152 FORMAT (1X,'Fractional light loss, system',20X,'=',E13.4)
  262 FORMAT (1X,'Flux (4*PI), first component, at wavelength',1X,
     $'=',E12.4,'ergs/sec/micron')
  263 FORMAT (1X,'Flux (4*PI), second component, at wavelength',
     $'=',E12.4,'ergs/sec/micron')
  205 FORMAT (1X,'Program running time=',F12.4,2X,'seconds')
C***********************************************************
      OPEN (UNIT=22,    FILE='PGDA.ILS',STATUS='OLD')
      READ (22, 100)    BFR
      OPEN (UNIT=21,     FILE=BFR,  STATUS='OLD')
      READ ( 21, 100)    (FL1(J),J=1,18)
      OPEN (UNIT=IN1,   FILE=FL1(1),  STATUS='OLD')
      WRITE (IN1,101) KEYBAD
      REWIND IN1
      OPEN (UNIT=IN2,   FILE=FL1(2),  STATUS='OLD')
      OPEN (UNIT=IN3,   FILE=FL1(3),  STATUS='OLD')
      OPEN (UNIT=IN4,   FILE=FL1(4),  STATUS='OLD')
      OPEN (UNIT=IN5,   FILE=FL1(5),  STATUS='OLD')
      OPEN (UNIT=IN6,   FILE=FL1(6),  STATUS='OLD')
      OPEN (UNIT=IN7,   FILE=FL1(7),  STATUS='OLD')
      OPEN (UNIT=IN8,   FILE=FL1(8),  STATUS='OLD')
      OPEN (UNIT=IN9,   FILE=FL1(9),  STATUS='OLD')
      OPEN (UNIT=IN10,  FILE=FL1(10), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(11), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(12), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(13), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(15), STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(16), STATUS='REPLACE')
      OPEN (UNIT=NOUT7, FILE=FL1(17), STATUS='REPLACE')
      OPEN (UNIT=NOUT8, FILE=FL1(18), STATUS='REPLACE')
C***********************************************************
C     Read input data
      LABEL = ' ***PGD'
      NU=IN2
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NPSI)	 !Number of orbital phases
      CALL DUMMY (1)
      CALL IREAD (NPST)	 !Ordinal of starting phase
      CALL DUMMY (1)
      CALL IREAD (NPSN)	 !Ordinal value of ending phase
      CALL DUMMY (1)
      CALL IREAD (KDERIV)	 !Key to calculate derivatives
	CALL DUMMY (2)
	CALL IREAD (KDARK)   !Index to choose limb darkening law
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
	CLOSE (UNIT = IN13, STATUS = 'KEEP')
      WRITE (NOUT1,104)IMON,IDAY,IYR,IHR,IMIN,ISEC
C  
C     THE FOLLOWING ARE INSTALLATION PARAMETERS,BUT CAN BE CHANGED
C     AT INSTALLATION TIME     
C  
      KEC=1      
      KY=1
      KLT=MPHS    
C  
C  
      IF (NPST.EQ.0) NPST=1    
      IF (NPSN.EQ.0) NPSN=NPSI 
      IF (NPSI.NE.0) GO TO 10  
      READ (IN7,106) KCTRL,NPSI
      IF (NPSN.EQ.0) NPSN=NPSI  
   10 WRITE (NOUT1,105) NPSI,NPST,NPSN
C     INPUT PERMANENT FILE DATA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN5)
      RI2=RI*RI
      READ (IN8,106) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN8,128) (VLPSI(K),K=1,NPSI)
      READ (IN8,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      READ (IN8,106) (LV(K),K=1,13)
      READ (IN8,128) OMRAD,ECC,SMAJ,TH0,VCONJ,VMN0,PHZERO,THFCP,THSCP,
     $THTCP,THLCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,THTCS,
     $THLCS,THFOUR
      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),    
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),   
     $UA(3-I),IN9)
C      WRITE (NOUT8,106) NPSI,NPST,NPSN,NWL
      WRITE (NOUT1,117)     
      WRITE (NOUT1,108) (TH(I,KK),KK=1,NTH)      
C  
C     *******************************                          
C     Loop for successive wavelengths
C     *******************************
C     This is the outermost loop
C
      DO 50 IWL=1,NWL   
      CALL RDWT6(I,IWL,NTH,IN10)
      WLV=WL(IWL)
	WRITE (NOUT5,108) WLV
      CALL WTOUT4(I,OM,TMPR(I,1,1),TMPR(3-I,1,1),QS,QN,RI,VMI,VVI,VVJ,
     $IWL,WLV,U1(I,1,1),U2(I,1,1),U1(3-I,1,1),U2(3-I,1,1),NOUT2)
      IF ((THTHREE.EQ.0.D0).AND.(THFOUR.EQ.0.D0)) THEN
         THTS1=PIO2
         THTS2=TPIO2
      ELSE
         THTS1=THTHREE
         THTS2=THFOUR
      END IF
      IF (PHSEC.EQ.0.D0) THEN
         THTS3=PI
         THTS4=0.D0
      ELSE
         THTS3=PHSEC
         THTS4=PHZERO
      END IF
C
C     PRINCIPAL LOOP,FOR SUCCESSIVE VALUES OF ORBITAL LONGITUDE   
C  
      WRITE (NOUT1,118)
      DO 60 KS=1,NPSN
C           
      DO J=1,2
		DO ITH=1,NTH
		VITG1(J,ITH)=0.D0
		VITG1A(J,ITH)=0.D0
		VITG3(J,ITH)=0.D0
		VITG4(J,ITH)=0.D0
		VITG5(J,ITH)=0.D0
		N=NPH(I,ITH)
			DO K=1,N
C     Visibility keys for photosphere points, stellar components
				STVS(J,ITH,K)=0.D0
			END DO
		END DO
      END DO                                    
C     Input data from PGB
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN6)
      viv=vi*57.2957795131
C      CONTINUE 
      WRITE (NOUT1,127) PSI,PSIP,KS
      IF (KS.LT.NPST) GO TO 60
C     LOOP TO CALCULATE ARRAYS OF SURFACE AREA ELEMENTS AND 
C     RELATED QUANTITIES
C 
      CSI=-DCOS(VI)
      SNI=DSIN(VI)
C     Set filling factor from PGB
	FILLF=FL
C     SET TO LOOK AT OPPOSED HEMISPHERE OF ECLIPSING STAR
      PSI=PSIP  
	CM=TWPI-PSI
      AG=0.D0
      IF (ISV.EQ.IEC) AG=PI
C     Set control for quadrant in which PSIP is located
      IF ((PSIP.GE.PHTS4).AND.(PSIP.LE.PHTS1)) ISEQ=1
      IF ((PSIP.GT.PHTS1).AND.(PSIP.LE.PHTS3)) ISEQ=2
      IF ((PSIP.GT.PHTS3).AND.(PSIP.LE.PHTS2)) ISEQ=3
      IF ((PSIP.GT.PHTS2).AND.(PSIP.LT.PHTS4+TWPI)) ISEQ=4
C
      CALL LSGT(CSI,SNI,CM-AG,VL0,VM0,VN0,SPSI,CPSI,TARY)
      CALL PROJ(TARY,VRD,0.0D0,0.0D0,XG,YG,ZG)
      XC=QS/(1.D0+QS)
      IF (IEC.EQ.ISV) XC=QN/(1.D0+QN)
C
      CALL PROJ(TARY,XC*VRD,0.D0,0.D0,XCMS,YCMS,ZCM)
      IF (IEC.EQ.ISV) XCMS=XCMS-XG
      IF (IEC.EQ.ISV) YCMS=YCMS-YG
      XCM=YCMS
      YCM=XCMS
C
      IF (I.NE.ISV) THEN
		XCMV=XC*VRD
	ELSE
		XCMV=-(1.D0-XC)*VRD
	END IF
      ANGLO=0.D0
      CSPJ=DCOS(ANGLO)
      SNPJ=DSIN(ANGLO)
	I=1
 2005 FORMAT (2I5,2F15.10)
C
C     Calculate projection grid of photospheric points
	DO II=1,2
C     Go from upper pole to equator
	KCT=0
      DO ITH=1,NTH/2+1
		NPHV=NPH(II,ITH)
C     Initialize KCTI at each ITH value
		DO K=1,NPHV
				CSGM=CSG(II,ITH,K)
				X=VR(II,ITH,K)*ALM(II,ITH,K)
				Y=VR(II,ITH,K)*AMU(II,ITH,K)
				Z=VR(II,ITH,K)*ANU(II,ITH,K)
				IF (I.NE.ISV) X=X-VRD
				CALL PROJ(TARY,X,Y,Z,PJX,PJY,PJZ)
				Y=PJX*CSPJ-PJY*SNPJ
				X=PJX*SNPJ+PJY*CSPJ
				Z=PJZ
				X=X-XCM
				Y=-Y+YCM
				Z=Z-ZCM
				XVL(II,ITH,K)=X
				YVL(II,ITH,K)=Y
		END DO
	END DO
C     Input horizon points
        NLMT=2*NTH
     	IF ((II.EQ.3-IEC).and.(II.EQ.1)) THEN
        DO K=1,NLMT
           READ (IN3,109) XX,YY
           IF (K.LE.NTH) THEN
           XHORZU(II,K)=XX
           YHORZU(II,K)=YY
           END IF
           IF (K.GT.NTH) THEN
           XHORZL(II,K)=XX
           YHORZL(II,K)=YY
           END IF
        END DO
	KCTA1=NTH
	KCT=0
	END IF
	IF ((II.EQ.IEC).AND.(II.EQ.1)) THEN
        DO K=1,NLMT
           READ (IN3,109) XX,YY
           IF (K.LE.NTH) THEN
           XHORZU(II,K)=XX
           YHORZU(II,K)=YY
           END IF
           IF (K.GT.NTH) THEN
           XHORZL(II,K)=XX
           YHORZL(II,K)=YY
           END IF
        END DO
	KCTA2=NTH
	KCT=0
	END IF
     	IF ((II.EQ.3-IEC).AND.(II.EQ.2)) THEN
       DO K=1,NLMT
           READ (IN4,109) XX,YY
           IF (K.LE.NTH) THEN
           XHORZU(II,K)=XX
           YHORZU(II,K)=YY
           END IF
           IF (K.GT.NTH) THEN
           XHORZL(II,K)=XX
           YHORZL(II,K)=YY
           END IF
        END DO
	KCTA1=NTH
	KCT=0
	END IF
	IF ((II.EQ.IEC).AND.(II.EQ.2)) THEN
       DO K=1,NLMT
           READ (IN4,109) XX,YY
           IF (K.LE.NTH) THEN
           XHORZU(II,K)=XX
           YHORZU(II,K)=YY
           END IF
           IF (K.GT.NTH) THEN
           XHORZL(II,K)=XX
           YHORZL(II,K)=YY
           END IF
        END DO
	KCTA2=NTH
	KCT=0
	END IF
C     Go from equator to lower pole
      DO ITH=NTH/2+1,NTH
		NPHV=NPH(I,ITH)
		DO K=1,NPHV
				CSGM=CSG(II,ITH,K)
				X=VR(II,ITH,K)*ALM(II,ITH,K)
				Y=VR(II,ITH,K)*AMU(II,ITH,K)
				Z=VR(II,ITH,K)*ANU(II,ITH,K)
				IF (I.NE.ISV) X=X-VRD
				CALL PROJ(TARY,X,Y,Z,PJX,PJY,PJZ)
				Y=PJX*CSPJ-PJY*SNPJ
				X=PJX*SNPJ+PJY*CSPJ
				Z=PJZ
				X=X-XCM
				Y=-Y+YCM
				Z=Z-ZCM
				XVL(II,ITH,K)=X
				YVL(II,ITH,K)=Y
		END DO
	END DO
	IF (II.EQ.3-IEC) THEN
	KCTB1=KCT
	KCT=0
	END IF
	IF (II.EQ.IEC) THEN
	KCTB2=KCT
	KCT=0
	END IF
      END DO
C     Sort upper horizon points
      II=3-IEC
	DO L1=1,KCTA1-1
		DO L2=1,KCTA1-1
			IF (XHORZU(II,L2).GT.XHORZU(II,L2+1)) THEN
				SV=XHORZU(II,L2+1)
				XHORZU(II,L2+1)=XHORZU(II,L2)
				XHORZU(II,L2)=SV
				SV=YHORZU(II,L2+1)
				YHORZU(II,L2+1)=YHORZU(II,L2)
				YHORZU(II,L2)=SV
			END IF	
		END DO
	END DO
      II=IEC
	DO L1=1,KCTA2-1
		DO L2=1,KCTA2-1
			IF (XHORZU(II,L2).GT.XHORZU(II,L2+1)) THEN
				SV=XHORZU(II,L2+1)
				XHORZU(II,L2+1)=XHORZU(II,L2)
				XHORZU(II,L2)=SV
				SV=YHORZU(II,L2+1)
				YHORZU(II,L2+1)=YHORZU(II,L2)
				YHORZU(II,L2)=SV
			END IF	
		END DO
	END DO
C	DO ITH=1,KCTA1
C	WRITE (NOUT1,2001)XHORZU(3-IEC,ITH),YHORZU(3-IEC,ITH) 
C	END DO
C	WRITE (NOUT1,2002)
C	DO ITH=1,KCTA2
C	WRITE (NOUT1,2001)XHORZU(IEC,ITH),YHORZU(IEC,ITH) 
C	END DO
C	WRITE (NOUT1,2002)
C	WRITE (NOUT1,2002)
C 2001   FORMAT(2F20.13)
C 2002   FORMAT(' ')
C     Sort lower horizon points
      II=3-IEC
	DO L1=1,KCTB1-1
		DO L2=1,KCTB1-1
			IF (XHORZL(II,L2).GT.XHORZL(II,L2+1)) THEN
				SV=XHORZL(II,L2+1)
				XHORZL(II,L2+1)=XHORZL(II,L2)
				XHORZL(II,L2)=SV
				SV=YHORZL(II,L2+1)
				YHORZL(II,L2+1)=YHORZL(II,L2)
				YHORZL(II,L2)=SV
			END IF	
		END DO
	END DO
      II=IEC
	DO L1=1,KCTB2-1
		DO L2=1,KCTB2-1
			IF (XHORZL(II,L2).GT.XHORZL(II,L2+1)) THEN
				SV=XHORZL(II,L2+1)
				XHORZL(II,L2+1)=XHORZL(II,L2)
				XHORZL(II,L2)=SV
				SV=YHORZL(II,L2+1)
				YHORZL(II,L2+1)=YHORZL(II,L2)
				YHORZL(II,L2)=SV
			END IF	
		END DO
	END DO
	DO ITH=1,KCTB1
	WRITE (NOUT1,2001)XHORZL(3-IEC,ITH),YHORZL(3-IEC,ITH) 
	END DO
	WRITE (NOUT1,2002)
	DO ITH=1,KCTB2
	WRITE (NOUT1,2001)XHORZL(IEC,ITH),YHORZL(IEC,ITH) 
	END DO
	WRITE (NOUT1,2002)
C     Following calculation only if an eclipse is present
      IF (SP.GT.1.D0) GO TO 49
C     Calculate SPLINE arrays
	DO K=1,KCTA1
		XTP(K)=XHORZU(3-IEC,K)
		YTP(K)=YHORZU(3-IEC,K)
	END DO
	YP1=1.0E10
	YPN=-1.0E10
      CALL SPLINE(XTP,YTP,KCTA1,YP1,YPN,SPL1U)
C
	DO K=1,KCTA2
		XTP(K)=XHORZU(IEC,K)
		YTP(K)=YHORZU(IEC,K)
	END DO
	YP1=1.0E10
	YPN=-1.0E10
	CALL SPLINE(XTP,YTP,KCTA2,YP1,YPN,SPL2U)
C
	DO K=1,KCTB1
		XTP(K)=XHORZL(3-IEC,K)
		YTP(K)=YHORZL(3-IEC,K)
	END DO
	YP1=-1.0E10
	YPN=1.0E10
	CALL SPLINE(XTP,YTP,KCTB1,YP1,YPN,SPL1L)
C
	DO K=1,KCTB2
		XTP(K)=XHORZL(IEC,K)
		YTP(K)=YHORZL(IEC,K)
	END DO
	YP1=-1.0E10
	YPN=1.0E10
	CALL SPLINE(XTP,YTP,KCTB2,YP1,YPN,SPL2L)
   49 CONTINUE
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
	DO II=1,2
		NPHV=NPH(I,NTH/2+1)
		DO K=2,NPHV
			XVL(II,1,K)=XVL(II,1,1)
			YVL(II,1,K)=YVL(II,1,1)
			XVL(II,NTH,K)=XVL(II,NTH,1)
			YVL(II,NTH,K)=YVL(II,NTH,1)
		END DO
	END DO
C     Calculate projected points for start and end of eclipse shadows
      I=3-IEC
      DO 41 ITH=1,NTH
	    ANG1=0.D0
		ANG2=0.D0
		ANG3=0.D0
		ANG4=0.D0
		ANG5=0.D0
		ANG6=0.D0
C     Test for no shadow boundary on that ITH value
		ANG1=SHS(ITH,1)
		IF (DABS(ANG1-6.29D0).LT.1.D-13) GO TO 41
                IF (DABS(ANG1-6.29D0).GT.1.D-13) THEN
		ANG2=SHN(ITH,1)
		ANG3=SHS(ITH,2)
		ANG4=SHN(ITH,2)
		ANG5=SHS(ITH,3)
		ANG6=SHN(ITH,3)
		VSNTH=DSIN(TH(I,ITH))
		VCSTH=DCOS(TH(I,ITH))
		VSNPH1=DSIN(ANG1)
		VCSPH1=DCOS(ANG1)
		VSNPH2=DSIN(ANG2)
		VCSPH2=DCOS(ANG2)
		IF (DABS(ANG3).LT.6.29D0) THEN
			VSNPH3=DSIN(ANG3)
			VCSPH3=DCOS(ANG3)
		END IF
		IF (DABS(ANG4).LT.6.29D0) THEN
			VSNPH4=DSIN(ANG4)
			VCSPH4=DCOS(ANG4)
		END IF
		IF (DABS(ANG5).LT.6.29D0) THEN
			VSNPH5=DSIN(ANG5)
			VCSPH5=DCOS(ANG5)
		END IF
		IF (DABS(ANG6).LT.6.29D0) THEN
			VSNPH6=DSIN(ANG6)
			VCSPH6=DCOS(ANG6)
		END IF
C     Locate K index corresponding to ANG1
		NPHV=NPH(I,ITH)
		DO K=1,NPHV-1
			IF ((ANG1.GE.PH(I,ITH,K)).AND.(ANG1.LT.PH(I,ITH,K+1)))
     $			THEN
				KVL=K
				GO TO 30
			END IF
			IF (ANG1.EQ.PH(I,ITH,NPHV)) THEN
				KVL=NPHV
				GO TO 30
			END IF
		END DO
   30		VRAD1=VR(I,ITH,KVL)
		XTMP=VRAD1*VSNTH*VCSPH1
		YTMP=VRAD1*VSNTH*VSNPH1
		ZTMP=VRAD1*VCSTH
		IF (I.NE.ISV) XTMP=XTMP-VRD
		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
		YCU=PJX*CSPJ-PJY*SNPJ
		XCU=PJX*SNPJ+PJY*CSPJ
		ZCU=PJZ
		XCU=XCU-XCM
		YCU=-YCU+YCM
C     Projected coods.corresponding to ANG1
		SHADXST(ITH,1)=XCU
		SHADYST(ITH,1)=YCU
C
C     Locate K index corresponding to ANG2
		DO K=1,NPHV-1
			IF ((ANG2.GE.PH(I,ITH,K)).AND.(ANG2.LT.PH(I,ITH,K+1)))
     $			THEN
				KVL=K
				GO TO 31
			END IF
			IF (ANG2.EQ.PH(I,ITH,NPHV)) THEN
				KVL=NPHV
				GO TO 31
			END IF			
		END DO		  
   31		VRAD2=VR(I,ITH,KVL)
		XTMP=VRAD2*VSNTH*VCSPH2
		YTMP=VRAD2*VSNTH*VSNPH2
		ZTMP=VRAD2*VCSTH
		IF (I.NE.ISV) XTMP=XTMP-VRD
		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
		YCU=PJX*CSPJ-PJY*SNPJ
		XCU=PJX*SNPJ+PJY*CSPJ
		ZCU=PJZ
		XCU=XCU-XCM
		YCU=-YCU+YCM
C     Projected coods. corresponding to ANG2
		SHADXND(ITH,1)=XCU
		SHADYND(ITH,1)=YCU
C
C     Locate K index corresponding to ANG3
		DO K=1,NPHV-1
			IF ((ANG3.GE.PH(I,ITH,K)).AND.(ANG3.LT.PH(I,ITH,K+1)))
     $			THEN
				KVL=K
				GO TO 43
			END IF
			IF (ANG3.EQ.PH(I,ITH,NPHV)) THEN
				KVL=NPHV
				GO TO 43
			END IF			
		END DO		  
   43		VRAD3=VR(I,ITH,KVL)
		XTMP=VRAD3*VSNTH*VCSPH3
		YTMP=VRAD3*VSNTH*VSNPH3
		ZTMP=VRAD3*VCSTH
		IF (I.NE.ISV) XTMP=XTMP-VRD
		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
		YCU=PJX*CSPJ-PJY*SNPJ
		XCU=PJX*SNPJ+PJY*CSPJ
		ZCU=PJZ
		XCU=XCU-XCM
		YCU=-YCU+YCM
C     Projected coods. corresponding to ANG3
		SHADXST(ITH,2)=XCU
		SHADYST(ITH,2)=YCU
C     Locate K index corresponding to ANG4
		DO K=1,NPHV-1
			IF ((ANG4.GE.PH(I,ITH,K)).AND.(ANG4.LT.PH(I,ITH,K+1)))
     $			THEN
				PHVVL=PH(I,ITH,K)
				PHVVVL=PH(I,ITH,K+1)
				KVL=K
				GO TO 44
			END IF
		END DO		  
		IF (DABS(ANG4-PH(I,ITH,NPHV)).LT.1.0D-13) THEN
			KVL=NPHV
			GO TO 44
		END IF
		CALL PMDSTOP			
   44		VRAD4=VR(I,ITH,KVL)
		XTMP=VRAD4*VSNTH*VCSPH4
		YTMP=VRAD4*VSNTH*VSNPH4
		ZTMP=VRAD4*VCSTH
		IF (I.NE.ISV) XTMP=XTMP-VRD
		CALL PROJ(TARY,XTMP,YTMP,ZTMP,PJX,PJY,PJZ)
		YCU=PJX*CSPJ-PJY*SNPJ
		XCU=PJX*SNPJ+PJY*CSPJ
		ZCU=PJZ
		XCU=XCU-XCM
		YCU=-YCU+YCM
C     Projected coods. corresponding to ANG4
		SHADXND(ITH,2)=XCU
		SHADYND(ITH,2)=YCU
        END IF
   41	END DO
C     Calculate projected areas of both stars
C     Initialize projected areas
      SM10A=0.D0
	SM10B=0.D0
C
*******************************
C     Start of section for star 3-IEC
C     *******************************
	II=3-IEC
C     Start of outer DO ITH loop for component 3-IEC
	DO ITH=3,NTH-1
	SM=0.D0
		NPHV=NPH(II,ITH)
C     Counter for number of cells on given ITH
	KCNT=0
C     Start of inner DO K loop
		DO K=2,NPHV
			A1=0.D0
			A2=0.D0
			A3=0.D0
			A4=0.D0
			A5=0.D0
			A6=0.D0
			A7=0.D0
			AR=0.D0
			X1=XVL(II,ITH-1,K-1)
			X2=XVL(II,ITH-1,K)
			X3=XVL(II,ITH,K-1)
			X4=XVL(II,ITH,K)
			Y1=YVL(II,ITH-1,K-1)
			Y2=YVL(II,ITH-1,K)
			Y3=YVL(II,ITH,K-1)
			Y4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
C     Make sure point (X2,Y2) is always in advance of point (X1,Y1)
			IF (DABS(X2).LT.DABS(X1))THEN
				SV=X2
				X2=X1
				X1=SV
				SV=Y2
				Y2=Y1
				Y1=SV
				SV=X4
				X4=X3
				X3=SV
				SV=Y4
				Y4=Y3
				Y3=SV
			END IF
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
C     Set up limb darkening
		SMLD=(G1+G2+G3+G4)/4.D0
	    CSGABS=DABS(SMLD)
		IF (KDARK.EQ.1) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
		END IF
		IF (KDARK.EQ.2) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
		END IF
		IF (KDARK.EQ.3) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
		END IF
C     End of limb darkening setup
C
C     Set up surface brightness
		BDICF=(BDI(II,ITH-1,K-1)+BDI(II,ITH,K-1)+BDI(II,ITH,K-1)+
     $			BDI(II,ITH,K))/4.D0
C     End of surface brightness setup
			IF ((G1.LT.0.D0).AND.(G2.LT.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				GO TO 25
			END IF
			IF ((G1.GE.0.D0).AND.(G2.LT.0.D0).AND.(G3.GE.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				IF ((DABS(G2).LT.1.0D-10).AND.(DABS(G4).LT.1.D-10))
     $				GO TO 27
				GO TO 26
			END IF
			IF ((G1.GE.0.D0).AND.(G2.LT.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				GO TO 26
			END IF
			IF ((G1.LT.0.D0).AND.(G2.LT.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				GO TO 26
			END IF
			IF ((G1.LT.0.D0).AND.(G2.GE.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.GE.0.D0)) THEN
				IF ((DABS(G1).LT.1.1D-10).AND.(DABS(G3).LT.1.0D-10))
     $				GO TO 27
				GO TO 26
			END IF
			IF ((G1.GE.0.D0).AND.(G2.GE.0.D0).AND.(G3.GE.0.D0)
     $		.AND.(G4.GE.0.D0)) THEN
				GO TO 27
			END IF
C     Partial segment contribution section
   26			CONTINUE
			GO TO 25
C     Case 1
   27			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y2-Y1))
				A2=DABS((X2-X1)*(Y2-Y1))/2.D0
				A3=DABS((X4-X2)*(Y2-Y4))/2.D0
				A4=DABS((X4-X3)*(Y4-Y3))/2.D0
				A5=DABS((X1-X3)*(Y1-Y3))/2.D0
				AT=DABS((X4-X3)*(Y2-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     Case 2
			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X4-X2)*(Y1-Y4))
				A4=DABS((X4-X2)*(Y2-Y4))/2.D0
				A5=DABS((X4-X3)*(Y4-Y3))/2.D0
				AT=DABS((X4-X3)*(Y1-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1C
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				GO TO 25
			END IF
C     Case 3
			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y2-Y1))/2.D0
				A3=DABS((X4-X2)*(Y2-Y4))/2.D0
				A4=DABS((X4-X3)*(Y3-Y4))/2.D0
				A5=DABS((X3-X1)*(Y3-Y4))
				AT=DABS((X4-X1)*(Y2-Y4))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     Case 4
			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X4-X2)*(Y1-Y2))























				A4=DABS((X4-X2)*(Y2-Y4))/2.D0
				A5=DABS((X4-X3)*(Y3-Y4))/2.D0
				A6=DABS((X3-X1)*(Y3-Y4))
				AT=DABS((X4-X1)*(Y1-Y4))
				AR=AT-(A1+A2+A3+A4+A5+A6)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     Case 5
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y1-Y3))/2.D0
				A2=DABS((X1-X3)*(Y2-Y1))
				A3=DABS((X2-X1)*(Y2-Y1))/2.D0
				A4=DABS((X2-X4)*(Y2-Y4))/2.D0
				A5=DABS((X4-X3)*(Y3-Y4))/2.D0
				AT=DABS((X2-X3)*(Y2-Y4))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     Case 6
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X2-X4)*(Y1-Y4))/2.D0
				A4=DABS((X2-X4)*(Y4-Y3))
				A5=DABS((X4-X3)*(Y4-Y3))/2.D0
				AT=DABS((X2-X3)*(Y1-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     Case 7
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y2-Y1))/2.D0
				A3=DABS((X2-X4)*(Y2-Y4))/2.D0
				A4=DABS((X2-X4)*(Y4-Y3))
				A5=DABS((X4-X3)*(Y4-Y3))/2.D0
				AT=DABS((X2-X1)*(Y2-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     Case 8
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X2-X4)*(Y2-Y4))/2.D0
				A4=DABS((X4-X3)*(Y3-Y4))/2.D0
				A5=DABS((X3-X1)*(Y3-Y4))
				AT=DABS((X2-X1)*(Y1-Y4))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 25
			END IF
C     If the correct case has not been identified, stop
			CALL PMDSTOP
   25 CONTINUE
C     Set VL1()=VL1A()
      VL1(II,ITH,K)=VL1A(II,ITH,K)
     	SM=SM+VL1A(II,ITH,K)
	VITG2(II,ITH)=VITG2(II,ITH)+VL1A(II,ITH,K)
C	WRITE (NOUT1,2000) ITH-1,ITH,K-1,K
C	WRITE (NOUT1,2001) X1,Y1,X2,Y2,X3,Y3,X4,Y4
C	WRITE (NOUT1,2001) A1*1.D4,A2*1.D4,A3*1.D4,A4*1.D4,A5*1.D4,
C     $A6*1.D4,A7*1.D4,AR*1.D4
C	WRITE (NOUT1,2001) SM*1.D4,G1,G2,G3,G4,VR(II,ITH,K)
C	WRITE (NOUT1,2002)
C	IF ((ITH.EQ.20).AND.(KCNT.EQ.1)) THEN
C      IF (ITH.EQ.20) THEN
C	WRITE (NOUT8,2001) X1,Y1
C	WRITE (NOUT8,2001) X2,Y2
C	WRITE (NOUT8,2001) X4,Y4
C	WRITE (NOUT8,2001) X3,Y3
C	WRITE (NOUT8,2001) X1,Y1
C	END IF
	PHV1=PH(II,ITH,K-1)
	PHV2=PH(II,ITH,K)
C     ***********************************************************
C     Calculation of error in this approximation to the projected
C     area of strip ITH-1 to ITH
C     ***********************************************************
C     End of inner DO K loop
C	IF ((ITH.EQ.20).AND.(KCNT.EQ.1)) THEN
C	STOP
C	END IF
	END DO
	IF (II.EQ.ISV) THEN
		SM10A=SM10A+VITG2(II,ITH)
	ELSE
		SM10B=SM10B+VITG2(II,ITH)
	END IF
   28 CONTINUE
 2000 FORMAT (6I4)
 2001 FORMAT (8F10.6)
 2002 FORMAT (' ')
 2003 FORMAT (2I5,4F15.6)
	RDS=VR(II,2,1)
	VTH1=TH(II,ITH)
	VTH2=TH(II,ITH-1)
	TERM1=VTH1-DSIN(VTH1)*DCOS(VTH1)
	TERM2=VTH2-DSIN(VTH2)*DCOS(VTH2)
	AREA=RDS**2*PI*(TERM1-TERM2)
	ER=AREA-VITG2(II,ITH)
C	WRITE (NOUT1,2003)ITH-1,ITH, AREA*1.D2,ER*1.D4
C     End of outer DO ITH loop
	END DO
C     Bypass program section for other star
	GO TO 77
C     *************************************
C     Start of program section for star IEC
C     *************************************
   78	II=IEC
C     Start of outer DO ITH loop
	DO ITH=3,NTH-1
	SM=0.D0
		NPHV=NPH(II,ITH)
C     Counter for number of cells on given ITH
	KCNT=0
C     Start of inner DO K loop
		DO K=2,NPHV
			A1=0.D0
			A2=0.D0
			A3=0.D0
			A4=0.D0
			A5=0.D0
			A6=0.D0
			A7=0.D0
			AR=0.D0
			X1=XVL(II,ITH-1,K-1)
			X2=XVL(II,ITH-1,K)
			X3=XVL(II,ITH,K-1)
			X4=XVL(II,ITH,K)
			Y1=YVL(II,ITH-1,K-1)
			Y2=YVL(II,ITH-1,K)
			Y3=YVL(II,ITH,K-1)
			Y4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
C     Make sure point (X2,Y2) is always in advance of point (X1,Y1)
			IF (DABS(X2).LT.DABS(X1))THEN
				SV=X2
				X2=X1
				X1=SV
				SV=Y2
				Y2=Y1
				Y1=SV
				SV=X4
				X4=X3
				X3=SV
				SV=Y4
				Y4=Y3
				Y3=SV
			END IF
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
C     Set up limb darkening
		SMLD=(G1+G2+G3+G4)/4.D0
	    CSGABS=DABS(SMLD)
		IF (KDARK.EQ.1) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
		END IF
		IF (KDARK.EQ.2) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
		END IF
		IF (KDARK.EQ.3) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
		END IF
C     End of limb darkening setup
C
C     Set up surface brightness
		BDICF=(BDI(II,ITH-1,K-1)+BDI(II,ITH,K-1)+BDI(II,ITH,K-1)+
     $			BDI(II,ITH,K))/4.D0
C     End of surface brightness setup
			IF ((G1.LT.0.D0).AND.(G2.LT.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				GO TO 79
			END IF
			IF ((G1.GE.0.D0).AND.(G2.LT.0.D0).AND.(G3.GE.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				IF ((DABS(G2).LT.1.0D-10).AND.(DABS(G4).LT.1.D-10))
     $				GO TO 81
				GO TO 80
			END IF
			IF ((G1.GE.0.D0).AND.(G2.LT.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				GO TO 80
			END IF
			IF ((G1.LT.0.D0).AND.(G2.LT.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.LT.0.D0)) THEN
				GO TO 80
			END IF
			IF ((G1.LT.0.D0).AND.(G2.GE.0.D0).AND.(G3.LT.0.D0)
     $		.AND.(G4.GE.0.D0)) THEN
				IF ((DABS(G1).LT.1.1D-10).AND.(DABS(G3).LT.1.0D-10))
     $				GO TO 81
				GO TO 80
			END IF
			IF ((G1.GE.0.D0).AND.(G2.GE.0.D0).AND.(G3.GE.0.D0)
     $		.AND.(G4.GE.0.D0)) THEN
				GO TO 81
			END IF
C     Partial segment contribution section
   80			CONTINUE
			GO TO 79
C     Case 1
   81			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y2-Y1))
				A2=DABS((X2-X1)*(Y2-Y1))/2.D0
				A3=DABS((X4-X2)*(Y2-Y4))/2.D0
				A4=DABS((X4-X3)*(Y4-Y3))/2.D0
				A5=DABS((X1-X3)*(Y1-Y3))/2.D0
				AT=DABS((X4-X3)*(Y2-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     Case 2
			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X4-X2)*(Y1-Y4))
				A4=DABS((X4-X2)*(Y2-Y4))/2.D0
				A5=DABS((X4-X3)*(Y4-Y3))/2.D0
				AT=DABS((X4-X3)*(Y1-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1C

C	WRITE (NOUT8,3055) X2,Y2

C	WRITE (NOUT8,3055) X4,Y4



C	WRITE (NOUT8,3055) X3,Y3

C	WRITE (NOUT8,3055) X1,Y1
				GO TO 79
			END IF
C     Case 3
			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y2-Y1))/2.D0
				A3=DABS((X4-X2)*(Y2-Y4))/2.D0
				A4=DABS((X4-X3)*(Y3-Y4))/2.D0
				A5=DABS((X3-X1)*(Y3-Y4))
				AT=DABS((X4-X1)*(Y2-Y4))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     Case 4
			IF ((DABS(X4).GE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X4-X2)*(Y1-Y2))
				A4=DABS((X4-X2)*(Y2-Y4))/2.D0
				A5=DABS((X4-X3)*(Y3-Y4))/2.D0
				A6=DABS((X3-X1)*(Y3-Y4))
				AT=DABS((X4-X1)*(Y1-Y4))
				AR=AT-(A1+A2+A3+A4+A5+A6)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     Case 5
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y1-Y3))/2.D0
				A2=DABS((X1-X3)*(Y2-Y1))
				A3=DABS((X2-X1)*(Y2-Y1))/2.D0
				A4=DABS((X2-X4)*(Y2-Y4))/2.D0
				A5=DABS((X4-X3)*(Y3-Y4))/2.D0
				AT=DABS((X2-X3)*(Y2-Y4))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     Case 6
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).LE.DABS(X1)))THEN
				A1=DABS((X1-X3)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X2-X4)*(Y1-Y4))/2.D0
				A4=DABS((X2-X4)*(Y4-Y3))
				A5=DABS((X4-X3)*(Y4-Y3))/2.D0
				AT=DABS((X2-X3)*(Y1-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     Case 7
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).LE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y2-Y1))/2.D0
				A3=DABS((X2-X4)*(Y2-Y4))/2.D0
				A4=DABS((X2-X4)*(Y4-Y3))
				A5=DABS((X4-X3)*(Y4-Y3))/2.D0
				AT=DABS((X2-X1)*(Y2-Y3))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     Case 8
			IF ((DABS(X4).LE.DABS(X2)).AND.(DABS(Y1).GE.DABS(Y2))
     $			.AND.(DABS(X3).GE.DABS(X1)))THEN
				A1=DABS((X3-X1)*(Y1-Y3))/2.D0
				A2=DABS((X2-X1)*(Y1-Y2))/2.D0
				A3=DABS((X2-X4)*(Y2-Y4))/2.D0
				A4=DABS((X4-X3)*(Y3-Y4))/2.D0
				A5=DABS((X3-X1)*(Y3-Y4))
				AT=DABS((X2-X1)*(Y1-Y4))
				AR=AT-(A1+A2+A3+A4+A5)
				VL1A(II,ITH,K)=DABS(AR)
				VL2A(II,ITH,K)=BDICF*CFA*VL1A(II,ITH,K)
C	WRITE (NOUT8,3055) X1,Y1
C	WRITE (NOUT8,3055) X2,Y2
C	WRITE (NOUT8,3055) X4,Y4
C	WRITE (NOUT8,3055) X3,Y3
C	WRITE (NOUT8,3055) X1,Y1
				KCNT=KCNT+1
				GO TO 79
			END IF
C     If the correct case has not been identified, stop
			CALL PMDSTOP
   79 CONTINUE
C     Set VL1()=VL1A()
      VL1(II,ITH,K)=VL1A(II,ITH,K)
     	SM=SM+VL1A(II,ITH,K)
	VITG2(II,ITH)=VITG2(II,ITH)+VL1A(II,ITH,K)
C	WRITE (NOUT1,2000) ITH-1,ITH,K-1,K
C	WRITE (NOUT1,2001) X1,Y1,X2,Y2,X3,Y3,X4,Y4
C	WRITE (NOUT1,2001) A1*1.D4,A2*1.D4,A3*1.D4,A4*1.D4,A5*1.D4,
C     $A6*1.D4,A7*1.D4,AR*1.D4
C	WRITE (NOUT1,2001) SM*1.D4,G1,G2,G3,G4,VR(II,ITH,K)
C	WRITE (NOUT1,2002)
C	IF ((ITH.EQ.20).AND.(KCNT.EQ.1)) THEN
C      IF (ITH.EQ.20) THEN
C	WRITE (NOUT8,2001) X1,Y1
C	WRITE (NOUT8,2001) X2,Y2
C	WRITE (NOUT8,2001) X4,Y4
C	WRITE (NOUT8,2001) X3,Y3
C	WRITE (NOUT8,2001) X1,Y1
C	END IF
	PHV1=PH(II,ITH,K-1)
	PHV2=PH(II,ITH,K)
C     ***********************************************************
C     Calculation of error in this approximation to the projected
C     area of strip ITH-1 to ITH
C     ***********************************************************
C     End of inner DO K loop
C	IF ((ITH.EQ.20).AND.(KCNT.EQ.1)) THEN
C	STOP
C	END IF
	END DO
	IF (II.EQ.ISV) THEN
		SM10A=SM10A+VITG2(II,ITH)
	ELSE
		SM10B=SM10B+VITG2(II,ITH)
	END IF
   82 CONTINUE
	RDS=VR(II,2,1)
	VTH1=TH(II,ITH)
	VTH2=TH(II,ITH-1)
	TERM1=VTH1-DSIN(VTH1)*DCOS(VTH1)
	TERM2=VTH2-DSIN(VTH2)*DCOS(VTH2)
	AREA=RDS**2*PI*(TERM1-TERM2)
	ER=AREA-VITG2(II,ITH)
C	WRITE (NOUT1,2003)ITH-1,ITH, AREA*1.D2,ER*1.D4
C     End of outer DO ITH loop
	END DO
   77 CONTINUE
C     Area of top cap
      VTH=TH(II,2)
	RDS=VR(II,2,1)
	TERM1=VTH-DSIN(VTH)*DCOS(VTH)
	ARTOP=RDS**2*TERM1
	VL1A(II,1,1)=ARTOP
	VL1(II,1,1)=ARTOP
C     Area of bottom cap
      VTH=TH(II,NTH-1)
	RDS=VR(II,NTH-1,1)
	TERM=(PI-VTH)-DSIN(PI-VTH)*DCOS(PI-VTH)
	ARBOT=RDS**2*TERM
	VL1A(II,NTH,1)=ARBOT
	VL1(II,NTH,1)=ARBOT
	SM=SM+ARTOP+ARBOT
	IF (II.EQ.ISV) THEN
		SM10A=SM10A+ARTOP+ARBOT
	ELSE
		SM10B=SM10B+ARTOP+ARBOT
	END IF
C     Test of calculated projected area
      SM=0.D0
      DO ITH=1,NTH
		SM=SM+VITG2(II,ITH)
	END DO
	SM=SM+ARTOP+ARBOT
	AREA=PI*RDS**2
	ER=SM-AREA
	SM=0.D0
C     Return for calculation of other star
	IF (II.EQ.3-IEC) THEN
		GO TO 78
	END IF
C     ****************************************
C     Calculate eclipsed area on eclipsed star
C     ****************************************
	II=3-IEC
C     Initialize eclipsed areas
	SM20A=0.D0
	SM20B=0.D0
C     Skip this calculation if there is no eclipse
      IF (SP.GT.1.D0) GO TO 48
C     Set visibility keys
      DO ITH=1,NTH
		NPHV=NPH(II,ITH)
		DO K=1,NPHV
C     Set visibility keys for eclipsing component.
C     First set keys negative for points below horizon.
			IF (CSG(I,ITH,K).LT.0.D0) STVS(I,ITH,K)=CSG(I,ITH,K)
			VPHV=PH(I,ITH,K)
C     Initialize all points between start and end of visible
C     limits to visibility true condition. 
			IF ((VPHV.GE.VST(I,ITH,1)).AND.(VPHV.LE.VND(I,ITH,1)))
     $		STVS(I,ITH,K)=1.D0
			IF ((VPHV.GE.VST(I,ITH,2)).AND.(VPHV.LE.VND(I,ITH,2)))
     $		STVS(I,ITH,K)=1.D0
			IF ((VPHV.GE.VST(I,ITH,3)).AND.(VPHV.LE.VND(I,ITH,3)))
     $		STVS(I,ITH,K)=1.D0
C     Set visibility keys to allow for eclipse effects.
			IF ((VPHV.GE.SHS(ITH,1)).AND.(VPHV.LE.SHN(ITH,1)))
     $			THEN 
      			IF (DABS(SHS(ITH,1)-SHN(ITH,1)).GT.1.0E-12)
     $				STVS(I,ITH,K)=-1.D0
				END IF
			IF ((VPHV.GE.SHS(ITH,2)).AND.(VPHV.LE.SHN(ITH,2)))
     $			THEN
				IF (DABS(SHS(ITH,2)-SHN(ITH,2)).GT.1.0E-12) 
     $				STVS(I,ITH,K)=-1.D0
				END IF
			IF ((VPHV.GE.SHS(ITH,3)).AND.(VPHV.LE.SHN(ITH,3)))
     $			THEN
				IF (DABS(SHS(ITH,3)-SHN(ITH,3)).GT.1.0E-12)
     $				STVS(I,ITH,K)=-1.0
				END IF
		END DO
	END DO
C     Separate calculation of eclipsed areas on eclipsed star to
C     test accuracy of PGD calculation.
C     Do calculation by consecutive colatitude strips on the
C     eclipsed star.
C     Calculate central meridian Y values and bounding colatitudes
C     on projected eclipsing star.
C     YREFF is the radius of the eclipsing star and also the upper
C     projected point of the eclipsing star
      YREFF=YVL(IEC,1,1)
C     YREFB is the bottommost projected point of the eclipsing star.
	YREFB=YVL(IEC,NTH,1)
	KREF=0
	SMM=0.D0
      DO ITH=1,NTH
C     YZZ is the projected central meridian point of the eclipsed
C     star for colatitude circle ITH.
	YZZ=YVL(3-IEC,ITH,1)
C     DLTY is the vertical displacement of YZZ from the upper
C     projected point of the eclipsing star.
C     This value = R*cos(theta) where theta is the polar angle
C     of the colatitude circle on the eclipsing star passing
C     through DLTY and R is the radius of the eclipsing star.
	DLTY=YREFF-YZZ
	IF ((DLTY.GE.0.D0).AND.(DABS(YZZ).LE.DABS(YREFF))) THEN
C     KREF is the ITH value of the first colatitude circle on the
C     eclipsed star that crosses onto the projected eclipsing star.
		IF (KREF.EQ.0) THEN
			KREF=ITH
		END IF
C     Calculate area of top cap using KREF
		IF (ITH.EQ.KREF) THEN
			VRR=YVL(IEC,1,1)
C     ANGG is the colatitude of the circle just described.
			ANGG=TWPI/4.D0-DACOS((YREFF-YZZ)/YREFF)
C     ARR is the projected area of the eclipsing star above the
C     first colatitude circle of the eclipsed star crossing the
C     shadow of the eclipsing star. 
			ARR=VRR**2*(ANGG-DSIN(ANGG)*DCOS(ANGG))
			SMM=SMM+ARR
C			WRITE (NOUT1,3050) KREF,ARR,SMM,VRR**2
		END IF
C     **********************************************************
C     Calculate projected areas of the eclipsing star bounded by
C     consecutive colatitude circles on the eclipsed star.
C     This permits a test of the accuracy of the following routine.
C     See the printout of VITG5()
C     **********************************************************
		IF (ITH.GT.KREF) THEN
			IF (DABS(YZZ).LE.DABS(YREFB)) THEN
				IF (YZZ.GE.0.D0) THEN
				ANGG=TWPI/4.D0-DACOS((YREFF-YZZ)/YREFF)
				ELSE
				ANGG=TWPI/2.D0-DACOS((YREFF+YZZ)/YREFF)
				END IF
				TERM1=ANGG-DSIN(ANGG)*DCOS(ANGG)
				TERM1=DABS(TERM1)
				TERM2=ANGGM1-DSIN(ANGGM1)*DCOS(ANGGM1)
				TERM2=DABS(TERM2)
				ARRV=VRR**2*DABS(TERM1-TERM2)
				SMM=SMM+ARRV
			END IF
C			WRITE (NOUT1,3050) ITH,TERM1,TERM2,ARRV,SMM
		END IF
C		WRITE (NOUT1,3050) ITH,DLTY,YREFF,YZZ,ANGG
C     ***********************************************************
	END IF
C     Save current data
		ITHMN=ITH
		ANGGM1=ANGG
	END DO
 3050 FORMAT (I5,4F20.17)
C     Set keys (VKEY1,VKEY2,VKEY3.VKEY4) to determine whether a given
C     projected grid quadralateral is completely eclipsed, partially
C     eclipsed, or outside eclipse boundaries.
C     Set the keys to their visibility key values.
C     If a given visibility key is (-1), the point is eclipsed.
C     If a given visibility key is (+1), the point is outside eclipse.
C     If a given visibility key is .LT.0.D0 but not (-1), the point
C     is below the horizon.
C
C     Set a key to identify the first ITH value with points in eclipse.
      KEYECL=0
	DO ITH=2,NTH-1
C     Initialize integrator for eclipsed area
	SM=0.D0
C
		NPHV=NPH(II,ITH)
		DO K=2,NPHV
			VKEY1=STVS(II,ITH-1,K-1)
			VKEY2=STVS(II,ITH-1,K)
			VKEY3=STVS(II,ITH,K-1)
			VKEY4=STVS(II,ITH,K)
C     *********************************
C     Case 1
C     *********************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
	ICASE=1
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	IF (XX2.GT.0.D0) THEN
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C	END IF
C	IF (XX2.LT.0.D0) THEN
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX2,YY2
C	END IF
C     All corners are in shadow. Full area in eclipse.
C     Set up limb darkening
		SMLD=(G1+G2+G3+G4)/4.D0
	    CSGABS=DABS(SMLD)
		IF (KDARK.EQ.1) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
		END IF
		IF (KDARK.EQ.2) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
		END IF
		IF (KDARK.EQ.3) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
		END IF
C     End of limb darkening setup
C
C     Set up surface brightness
		BDICF=(BDI(II,ITH-1,K-1)+BDI(II,ITH,K-1)+BDI(II,ITH,K-1)+
     $			BDI(II,ITH,K))/4.D0
C     End of surface brightness setup
				SM=SM+VL1A(II,ITH,K)
				GO TO 29
			END IF
C     **********************************
C     Case 2
C     **********************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=2
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Continue with processing
C     Locate intersection point on K boundary of eclipsed star
				XXV1=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 69
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 69
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   69				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH boundary of eclipsed star 
				YYV2=YVL(3-IEC,ITH,K)
				XXV2=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 70
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 70
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 70
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 70
						END IF
					END IF
				END DO
C     Failed to locate point
C     If this is an equatorial point, exit
				IF ((ITH.EQ.NTH/2+2).AND.(K.EQ.NPHV/2+2)) GO TO 29
				CALL PMDSTOP
	END IF
   70				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)

					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5



					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XX4-XXV2)
				VLG2=DABS(YY4-YYV1)
				AREA=VLG1*VLG2/2.D0
C     Continue sum of eclipsed area
				SM=SM+VL1A(II,ITH,K)-AREA
C     Set up limb darkening
		SMLD=(G1+G2+G3+G4)/4.D0
	    CSGABS=DABS(SMLD)
		IF (KDARK.EQ.1) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
		END IF
		IF (KDARK.EQ.2) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
		END IF
		IF (KDARK.EQ.3) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
		END IF
C     End of limb darkening setup
C
C     Set up surface brightness
		BDICF=(BDI(II,ITH-1,K-1)+BDI(II,ITH,K-1)+BDI(II,ITH,K-1)+
     $			BDI(II,ITH,K))/4.D0
C     End of surface brightness setup
                  
				GO TO 29
			END IF
C     **********************************
C     Case 3
C     **********************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=3
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Continue with processing
C     Locate intersection point on K-1 boundary of eclipsed star
				XXV1=XVL(3-IEC,ITH,K-1)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 71
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 71
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   71				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH boundary of eclipsed star 
				YYV2=YVL(3-IEC,ITH,K)
				XXV2=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 72
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 72
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 72
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 72
						END IF
					END IF
				END DO
C     Failed to locate point
C     If this is an equatorial point, exit
				IF ((ITH.EQ.NTH/2+2).AND.(K.EQ.NPHV/2+2)) GO TO 29
				CALL PMDSTOP
	END IF
   72				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XX3-XXV2)
				VLG2=DABS(YY3-YYV1)
				AREA=VLG1*VLG2/2.D0
C     Continue sum of eclipsed area
				SM=SM+VL1A(II,ITH,K)-AREA
				GO TO 29
			END IF
C     **********************************
C     Case 4
c     **********************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=4
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
   	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Continue with processing
C     Locate intersection point on K boundary	of eclipsed star
				XXV1=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 57
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 57
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   57				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH-1 boundary of eclipsed star 
				YYV2=YVL(3-IEC,ITH-1,K)
				XXV2=XVL(3-IEC,ITH-1,K)
	IF (ITH.LE.NTH/2) THEN
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 58
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 58
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 58

						END IF

					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 58
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   58				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				XX2=XVL(3-IEC,ITH-1,K)
				YY2=YVL(3-IEC,ITH-1,K)
				VLG1=DABS(XX2-XXV2)
				VLG2=DABS(YY2-YYV1)
				AREA=VLG1*VLG2/2.D0
C     Continue sum of eclipsed area
				SM=SM+VL1A(II,ITH,K)-AREA
				GO TO 29
			END IF
C     *******************************
C     Case 5
C     *******************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=5
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Continue with processing
C     Locate intersection point on K boundary	of eclipsed star
				XXV1=XVL(3-IEC,ITH,K-1)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 51
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 51
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   51				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH-1 boundary of eclipsed star 
				YYV2=YVL(3-IEC,ITH-1,K)
				XXV2=XVL(3-IEC,ITH-1,K)
	IF (ITH.LE.NTH/2) THEN
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 52
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 52
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 52
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 52
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   52				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				XX2=XVL(3-IEC,ITH-1,K)
				YY2=YVL(3-IEC,ITH-1,K)
				VLG1=DABS(XX2-XXV2)
				VLG2=DABS(YY2-YYV1)
				AREA=VLG1*VLG2/2.D0
C     Continue sum of eclipsed area
				SM=SM+VL1A(II,ITH,K)-AREA
				GO TO 29
			END IF
C     ***********************************
C     Case 6
C     ***********************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=6
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
C     NOTE! DO NOT ALLOW THIS CASE
                  GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C     Continue with processing
C     Locate intersection point on K-1 boundary of eclipsed star
				XXV1=XVL(3-IEC,ITH,K-1)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 73
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 73
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   73				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C     Locate intersection point on K boundary of eclipsed star
				XXV2=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV2).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV2)) THEN
						LREF=L1
						GO TO 74
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV2).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV2)) THEN
						LREF=L1
						GO TO 74
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   74				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV2,YYV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XXV2-XXV1)
				VLG2=DABS(XX2-XX1)
				VLG3=DABS(YYV1-YY1)
				VLG4=DABS(YYV2-YY2)
				AREA=(VLG1+VLG2)*(VLG3+VLG4)/4.D0
C     Continue sum of eclipsed area
				SM=SM+AREA
				GO TO 29
			END IF
C     ***********************************
C     Case 7
C     ***********************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=7
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29

C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C     Continue with processing
C				SM=SM+VL1A(II,ITH,K)
C     Trap this case
				CALL PMDSTOP
				GO TO 29
			END IF
C     *************************************
C     Case 8
C     *************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=8
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C
   	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Continue with processing
C     Locate intersection point on K-1 boundary of eclipsed star
				XXV1=XVL(3-IEC,ITH,K-1)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 65
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 65
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   65				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C     Locate intersection point on K boundary of eclipsed star
				XXV2=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV2).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV2)) THEN
						LREF=L1
						GO TO 66
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV2).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV2)) THEN
						LREF=L1
						GO TO 66
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   66				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV2,YYV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XXV2-XXV1)
				VLG2=DABS(XX4-XX3)
				VLG3=DABS(YYV1-YY3)
				VLG4=DABS(YYV2-YY4)
				AREA=(VLG1+VLG2)*(VLG3+VLG4)/4.D0
C     Continue sum of eclipsed area
				SM=SM+AREA
				GO TO 29
			END IF
C     *************************************
C     Case 9
C     *************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
C     THIS CASE DOES NOT OCCUR
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=9
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C     Trap this case
				CALL PMDSTOP
				GO TO 29
			END IF
C     **************************************
C     Case 10
C     **************************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=10
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     Continue with processing
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Locate intersection point on K-1 boundary of eclipsed star
				XXV1=XVL(3-IEC,ITH,K-1)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 67
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 67
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   67				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH-1 boundary of eclipsed star 
				YYV2=YVL(3-IEC,ITH-1,K)
				XXV2=XVL(3-IEC,ITH-1,K)
	IF (ITH.LE.NTH/2) THEN
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 68
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 68
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 68
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 68
						END IF
					END IF
				END DO
C     Failed to locate point
C     If this is an equatorial point, exit
				IF ((ITH.EQ.NTH/2+2).AND.(K.EQ.NPHV/2+2)) GO TO 29
				CALL PMDSTOP
	END IF
   68				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XX2-XXV2)
				VLG2=DABS(YY2-YYV1)
				AREA=VLG1*VLG2/2.D0
C     Continue sum of eclipsed area

				SM=SM+VL1A(II,ITH,K)-AREA
				GO TO 29
			END IF


C     *************************************
C     Case 11
C     *************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
			ICASE=11
C		None of this segment is in eclipse
				GO TO 29
			END IF
C     **************************************
C     Case 12
C     **************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=12
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C      IF (ITH.GT.NTH/2) GO TO 29
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C	IF ((ITH.EQ.NTH/2+1).OR.(ITH.EQ.NTH/2+1)) GO TO 29
C     Continue with processing
C     Locate intersection point on ITH-1 boundary	of eclipsed star
				YYV1=YVL(3-IEC,ITH-1,K)
				XXV1=XVL(3-IEC,ITH-1,K)
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV1).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV1)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
					IF (((DABS(YHORZU(IEC,L1))-DABS(YYV1)).LT.(1.D-10)
     $					).AND.(YHORZU(IEC,L1+1).GE.YYV1)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV1).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV1)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
					IF (((DABS(YHORZU(IEC,L1))-DABS(YYV1)).LT.(1.D-10)
     $					).AND.(YHORZU(IEC,L1+1).LE.YYV1)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV1).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV1)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV1)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).GE.YYV1)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV1).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV1)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
				END DO
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV1)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).LE.YYV1)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 59
						END IF
					END IF
	END IF
C     Failed to locate point
				CALL PMDSTOP
   59				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
					IF (ITH.LE.NTH/2+1) THEN
						YRF(L1)=YHORZU(IEC,LST+L1-1)
						XRF(L1)=XHORZU(IEC,LST+L1-1)
					END IF
					IF (ITH.GT.NTH/2+1) THEN
						YRF(L1)=YHORZL(IEC,LST+L1-1)
						XRF(L1)=XHORZL(IEC,LST+L1-1)
					END IF
				END DO
				CALL POLINT(YRF,XRF,5,YYV1,XXV1,DYY)
C	Locate intersection point on ITH boundary of eclipsed star
				YYV2=YVL(3-IEC,ITH,K)
				XXV2=XVL(3-IEC,ITH,K)
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1+1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1+1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1+1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
					IF (((DABS(YHORZL(IEC+1,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 61
						END IF
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   61				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
				IF (ITH.LE.NTH/2+1) THEN
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END IF
				IF (ITH.GT.NTH/2+1) THEN
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END IF
				END DO
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XX4-XXV2)
				VLG2=DABS(XX2-XXV1)
				VHT=DABS(YY2-YY4)
				AREA=(VLG1+VLG2)/2.D0*VHT
				SM=SM+AREA
				GO TO 29
			END IF
C     **************************************
C     Case 13
C     **************************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=13
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the

C     eclipsed area between ITH-1 and ITH is done below.


                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C      IF (ITH.GT.NTH/2) GO TO 29
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
	WRITE (NOUT8,3055) XX1,YY1
	WRITE (NOUT8,3055) XX2,YY2
	WRITE (NOUT8,3055) XX4,YY4
	WRITE (NOUT8,3055) XX3,YY3
	WRITE (NOUT8,3055) XX1,YY1
C     Locate intersection point on ITH-1 boundary	of eclipsed star
				YYV1=YVL(3-IEC,ITH-1,K)
				XXV1=XVL(3-IEC,ITH-1,K)
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV1).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV1)) THEN
					TST2=DABS(XHORZU(IEC,L1)-XX4)
      					IF (TST2.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
					ABS1=DABS(YHORZU(IEC,L1))
					ABS2=DABS(YYV1)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZU(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZU(IEC,L1+1).GE.YYV1)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV1).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV1)) THEN
						TST2=DABS(XHORZU(IEC,L1)-XX4)
      					IF (TST2.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
					ABS1=DABS(YHORZU(IEC,L1))
					ABS2=DABS(YYV1)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZU(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZU(IEC,L1+1).LE.YYV1)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV1).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV1)) THEN
						TST2=DABS(XHORZL(IEC,L1)-XX4)
      					IF (TST2.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
					ABS1=DABS(YHORZL(IEC,L1))
					ABS2=DABS(YYV1)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZL(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10).AND.(YHORZL(IEC,L1+1).GE.
     $					YYV1)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV1).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV1)) THEN
						TST2=DABS(XHORZL(IEC,L1)-XX4)
      					IF (TST2.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
					ABS1=DABS(YHORZL(IEC,L1))
					ABS2=DABS(YYV1)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZL(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZL(IEC,L1+1).LE.YYV1)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 63
						END IF
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   63				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
				IF (ITH.LE.NTH/2+1) THEN
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END IF
				IF (ITH.GT.NTH/2+1) THEN
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END IF
				END DO
				CALL POLINT(YRF,XRF,5,YYV1,XXV1,DYY)
C	Locate intersection point on ITH boundary of eclipsed star
				YYV2=YVL(3-IEC,ITH,K)
				XXV2=XVL(3-IEC,ITH,K)
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
					TST2=DABS(XHORZU(IEC,L1)-XX4)
      					IF (TST2.LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					ABS1=DABS(YHORZU(IEC,L1))
					ABS2=DABS(YYV2)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZU(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZU(IEC,L1+1).GE.YYV2)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					ABS1=DABS(YHORZU(IEC,L1+1))
					ABS2=DABS(YYV2)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZU(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZU(IEC,L1).LE.YYV2)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
					TST2=DABS(XHORZU(IEC,L1)-XX4)
      					IF (TST2.LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					ABS1=DABS(YHORZU(IEC,L1))
					ABS2=DABS(YYV2)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZU(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZU(IEC,L1+1).LE.YYV2)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					ABS1=DABS(YHORZU(IEC,L1+1))
					ABS2=DABS(YYV2)
					TST1=DABS(ABS1-ABS2)
					TST3=DABS(XHORZU(IEC,L1)-XX4)
					IF ((TST1.LT.1.D-10
     $					).AND.(YHORZU(IEC,L1).GE.YYV2)) THEN
      					IF (TST3.LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1+1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					IF (((DABS(YHORZL(IEC,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1+1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
					IF (((DABS(YHORZL(IEC+1,L1))-DABS(YYV2)).LT.(1.D-10)
     $					).AND.(YHORZL(IEC,L1).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 64
						END IF
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   64				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
					IF (ITH.LE.NTH/2+1) THEN
						YRF(L1)=YHORZU(IEC,LST+L1-1)
						XRF(L1)=XHORZU(IEC,LST+L1-1)
					END IF
					IF (ITH.GT.NTH/2+1) THEN
						YRF(L1)=YHORZL(IEC,LST+L1-1)
						XRF(L1)=XHORZL(IEC,LST+L1-1)
					END IF
				END DO
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of eclipsed segment 
				VLG1=DABS(XX3-XXV2)
				VLG2=DABS(XX1-XXV1)
				HGT=DABS(YY1-YY3)
				AREA=(VLG1+VLG2)/2.D0*HGT
				SM=SM+AREA
				GO TO 29
	END IF
C     *********************************
C     Case 14
C     *********************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=14
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given

C     VKEY1,VKEY2,VKEY3,VKEY4
C      IF (ITH.GT.NTH/2) GO TO 29



	XX1=XVL(II,ITH-1,K-1)



	YY1=YVL(II,ITH-1,K-1)

	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Locate intersection point on K-1 boundary	of eclipsed star
				XXV1=XVL(3-IEC,ITH,K-1)
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 53
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
   53				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
					IF (ITH.LE.NTH/2+1) THEN
						YRF(L1)=YHORZU(IEC,LST+L1-1)
						XRF(L1)=XHORZU(IEC,LST+L1-1)
					END IF
					IF (ITH.GT.NTH/2+1) THEN
						YRF(L1)=YHORZL(IEC,LST+L1-1)
						XRF(L1)=XHORZL(IEC,LST+L1-1)
					END IF
				END DO
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH boundary of eclipsed star
				YYV2=YVL(3-IEC,ITH,K-1)
				XXV2=XVL(3-IEC,ITH,K-1)
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 54
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 54
						END IF
					END IF
				END DO
C     Failed to locate point
                  IF (ITH.EQ.NTH/2+1) GO TO 29
				CALL PMDSTOP
   54				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
					IF (ITH.LE.NTH/2+1) THEN
						YRF(L1)=YHORZU(IEC,LST+L1-1)
						XRF(L1)=XHORZU(IEC,LST+L1-1)
					END IF
					IF (ITH.GT.NTH/2+1) THEN
						YRF(L1)=YHORZL(IEC,LST+L1-1)
						XRF(L1)=XHORZL(IEC,LST+L1-1)
					END IF
				END DO
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				XX3=XVL(3-IEC,ITH,K-1)
				YY3=YVL(3-IEC,ITH,K-1)
				VLG1=DABS(XX3-XXV2)
				VLG2=DABS(YY3-YYV1)
				AREA=VLG1*VLG2/2.D0
				SM=SM+AREA
				GO TO 29
			END IF
C     ****************************************
C     Case 15
C     ****************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=15
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Locate intersection point on K boundary of eclipsed star
				XXV1=XVL(3-IEC,ITH,K)
	IF (ITH.LE.NTH/2) THEN
				DO L1=1,KCTA2-1
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 75
					END IF
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,KCTB2-1
					IF ((XHORZL(IEC,L1).LE.XXV1).AND.(XHORZL(IEC,L1+1
     $					).GE.XXV1)) THEN
						LREF=L1
						GO TO 75
					END IF
				END DO
	END IF
C     Failed to locate point
				CALL PMDSTOP
   75				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH-1 boundary of eclipsed star 
				YYV2=YVL(3-IEC,ITH-1,K)
				XXV2=XVL(3-IEC,ITH-1,K)
	IF (ITH.LE.NTH/2) THEN
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 76
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 76
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	ELSE IF (ITH.GT.NTH/2+1) THEN
				TST=DABS(XHORZL(IEC,NTH)-XHORZL(IEC,1))/5.D0
				DO L1=1,KCTB2-1
	ZZ1=DABS(XHORZL(IEC,L1)-XX4)
	ZZ2=DABS(YHORZL(IEC,L1)-YYV2)
C     Test ascending branch
					IF ((YHORZL(IEC,L1).LE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 76
						END IF
					END IF
					IF ((DABS(YHORZL(IEC,L1)-YYV2).LT.1.0E-10).AND.
     $					(YHORZL(IEC,L1+1).GE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 76
						END IF
					END IF
C     Test descending branch
					IF ((YHORZL(IEC,L1).GE.YYV2).AND.(YHORZL(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 76
						END IF
					END IF
					IF ((DABS(YHORZL(IEC,L1)-YYV2).LT.1.0E-10).AND.
     $					(YHORZL(IEC,L1+1).LE.YYV2)) THEN
      					IF ((DABS(XHORZL(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 76
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
	END IF
   76				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
	IF (ITH.LE.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZU(IEC,LST+L1-1)
					XRF(L1)=XHORZU(IEC,LST+L1-1)
				END DO
	ELSE IF (ITH.GT.NTH/2+1) THEN
				DO L1=1,5
					YRF(L1)=YHORZL(IEC,LST+L1-1)
					XRF(L1)=XHORZL(IEC,LST+L1-1)
				END DO
	END IF
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				VLG1=DABS(XX2-XXV2)
				VLG2=DABS(YY2-YYV1)
				AREA=VLG1*VLG2/2.D0
C     Continue sum of eclipsed area
				SM=SM+VL1A(II,ITH,K)-AREA
				GO TO 29
			END IF
C     ***************************************
C     Case 16
C     ***************************************
			IF ((VKEY1.EQ.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=16
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
C     Trap this case
				CALL PMDSTOP
				GO TO 29
			END IF
C     *****************************************
C     Case 17
C     *****************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.EQ.(-1.D0))) THEN
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
			ICASE=17
C     Test for this ITH.EQ.KEYECL. If yes, the integration over the
C     eclipsed area between ITH-1 and ITH is done below.
                  IF (ITH.EQ.KEYECL) GO TO 29
C     If the program continues, calculate proper case for the given
C     VKEY1,VKEY2,VKEY3,VKEY4
	XX1=XVL(II,ITH-1,K-1)
	YY1=YVL(II,ITH-1,K-1)
	XX2=XVL(II,ITH-1,K)
	YY2=YVL(II,ITH-1,K)
	XX3=XVL(II,ITH,K-1)
	YY3=YVL(II,ITH,K-1)
	XX4=XVL(II,ITH,K)
	YY4=YVL(II,ITH,K)
			G1=CSG(II,ITH-1,K-1)
			G2=CSG(II,ITH-1,K)
			G3=CSG(II,ITH,K-1)
			G4=CSG(II,ITH,K)
      IF ((G1.LT.0.D0).OR.(G2.LT.0.D0).OR.(G3.LT.0.D0).OR.
     $(G4.LT.0.D0)) THEN
		GO TO 29
	END IF
C	WRITE (NOUT8,3055) XX1,YY1
C	WRITE (NOUT8,3055) XX2,YY2
C	WRITE (NOUT8,3055) XX4,YY4
C	WRITE (NOUT8,3055) XX3,YY3
C	WRITE (NOUT8,3055) XX1,YY1
C     Locate intersection point on K boundary	of eclipsed star
				XXV1=XVL(3-IEC,ITH,K)
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((XHORZU(IEC,L1).LE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).GE.XXV1)) THEN
							LREF=L1
							GO TO 55
					END IF
C     Test descending branch branch
					IF ((XHORZU(IEC,L1).GE.XXV1).AND.(XHORZU(IEC,L1+1
     $					).LE.XXV1)) THEN
							LREF=L1
							GO TO 55
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
   55				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
					IF (ITH.LE.NTH/2+1) THEN
						YRF(L1)=YHORZU(IEC,LST+L1-1)
						XRF(L1)=XHORZU(IEC,LST+L1-1)
					END IF
					IF (ITH.GT.NTH/2+1) THEN
						YRF(L1)=YHORZL(IEC,LST+L1-1)
						XRF(L1)=XHORZL(IEC,LST+L1-1)
					END IF
				END DO
				CALL POLINT(XRF,YRF,5,XXV1,YYV1,DYY)
C	Locate intersection point on ITH boundary of eclipsed star
				YYV2=YVL(3-IEC,ITH,K)
				XXV2=XVL(3-IEC,ITH,K)
				TST=DABS(XHORZU(IEC,NTH)-XHORZU(IEC,1))/5.D0
				DO L1=1,KCTA2-1
C     Test ascending branch
					IF ((YHORZU(IEC,L1).LE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).GE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 56
						END IF
					END IF
C     Test descending branch
					IF ((YHORZU(IEC,L1).GE.YYV2).AND.(YHORZU(IEC,L1+1
     $					).LE.YYV2)) THEN
      					IF ((DABS(XHORZU(IEC,L1)-XX4)).LT.TST)	THEN
							LREF=L1
							GO TO 56
						END IF
					END IF
				END DO
C     Failed to locate point
				CALL PMDSTOP
   56				CONTINUE
				LST=LREF-3
				IF (LST.LT.1) LST=1
				DO L1=1,5
					IF (ITH.LE.NTH/2+1) THEN
						YRF(L1)=YHORZU(IEC,LST+L1-1)
						XRF(L1)=XHORZU(IEC,LST+L1-1)
					END IF
					IF (ITH.GT.NTH/2+1) THEN
						YRF(L1)=YHORZL(IEC,LST+L1-1)
						XRF(L1)=XHORZL(IEC,LST+L1-1)
					END IF
				END DO
				CALL POLINT(YRF,XRF,5,YYV2,XXV2,DYY)
C     Calculate area of segment not eclipsed
				XX4=XVL(3-IEC,ITH,K)
				YY4=YVL(3-IEC,ITH,K)
				VLG1=DABS(XX4-XXV2)
				VLG2=DABS(YY4-YYV1)
				AREA=VLG1*VLG2/2.D0
				SM=SM+AREA
				GO TO 29
			END IF
C     *************************************
C     Case 18
C     *************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.EQ.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
			ICASE=18
C     IDENTICAL WITH CASE 14
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
C     Trap this case
				CALL PMDSTOP
				GO TO 29
			END IF
C     *************************************
C     Case 19
C     *************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.EQ.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
			ICASE=19
C     IDENTICAL WITH CASE 15
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
C     Trap this case
				CALL PMDSTOP
				GO TO 29
			END IF
C     ************************************
C     Case 20
C     ************************************
			IF ((VKEY1.NE.(-1.D0)).AND.(VKEY2.NE.(-1.D0)).AND.
     $		(VKEY3.NE.(-1.D0)).AND.(VKEY4.NE.(-1.D0))) THEN
			ICASE=20
C     IDENTICAL WITH CASE 11
				IF (KEYECL.EQ.0) THEN
					KEYECL=ITH
				END IF
C     Trap this case
				CALL PMDSTOP
				GO TO 29
			END IF
C     ***************************************
			CALL PMDSTOP
C     ***************************************
   29			CONTINUE
C     The following statement is the end of the DO K=... loop
		END DO
C     **********************************
	VITG5(II,ITH)=SM
C     ***********************************
	IF (ISV.EQ.3-IEC) SM20A=SM20A+SM
	IF (ISV.EQ.IEC) SM20B=SM20B+SM
C     ***********************************
C     The following statement is the end of the long DO ITH=...loop
	END DO
C
C     **********************************
C     Test for completed calculation of eclipsed area above first ITH with
C     shadow crossings.
      IF (KEYECL.NE.-1) THEN
C	Test for presence of any eclipsed area at this orbital phase
		IF (KEYECL.GT.0) THEN
			X1=SHADXST(KEYECL,1)
			Y1=SHADYST(KEYECL,1)
			X2=SHADXND(KEYECL,1)
			Y2=SHADYND(KEYECL,1)
			X3=SHADXST(KEYECL,2)
			Y3=SHADYST(KEYECL,2)
			X4=SHADXND(KEYECL,2)
			Y4=SHADYND(KEYECL,2)
		END IF
		YRFZ=(Y1+Y4)/2.D0
C     Set boundaries of eclipsed area on colatitude profile KEYECL
		IF (DABS(X1)-DABS(X4).LT.1.D-15) THEN
			RANGE=DABS(X2)+DABS(X3)
			IF (X2.LT.X3) THEN
				XST=X2
			ELSE
				XST=X3
			END IF
			DO K=1,KCTA1
				XTP(K)=XHORZU(IEC,K)
				YTP(K)=YHORZU(IEC,K)
			END DO
			DO K=1,11
				XXX=XST+(K-1)*RANGE/10.D0
				XRF(K)=XXX
				CALL SPLINT(XTP,YTP,SPL1U,KCTA1,XXX,YYY)
				YRF(K)=YYY-YRFZ
			END DO
		ELSE
			CALL PMDSTOP
		END IF
	DO KKK=1,11
C	WRITE (NOUT8,3055) XRF(KKK),YRF(KKK)
	END DO
 3055 FORMAT (2F10.6)
C     Integrate over eclipsed area above colatitude profile KEYECL
		CALL SMP(YRF,1,11,RANGE/10.D0,0.D0,0.D0,1,ER,11,SMD)
		SMD=DABS(SMD)
C     Add this area to total
		SM20A=SM20A+SMD
C     Set KEYECL to -1 so that eclipsed area is added only once
		KEYSAVE=KEYECL
		KEYECL=-1
	END IF
C
C      DO ITH=1,NTH
C		WRITE (NOUT1,3060)ITH,SHADXST(ITH,1),SHADXND(ITH,1),
C     $	SHADXST(ITH,2),SHADXND(ITH,2)
C	END DO
C 3060 FORMAT (I5,4F15.9)
C     **********************************
C     Determine ITH with last shadow crossing
      DO ITH=KEYSAVE,NTH
		IF ((SHADXST(ITH,1).EQ.0.D0).AND.(SHADXND(ITH,1).EQ.0.D0))
     $	THEN
			KEYEND=ITH-1
		END IF
	END DO
			X1=SHADXST(KEYSAVE,1)
			Y1=SHADYST(KEYSAVE,1)
			X2=SHADXND(KEYSAVE,1)
			Y2=SHADYND(KEYSAVE,1)
			X3=SHADXST(KEYSAVE,2)
			Y3=SHADYST(KEYSAVE,2)
			X4=SHADXND(KEYSAVE,2)
			Y4=SHADYND(KEYSAVE,2)
			YRFZ=(Y1+Y4)/2.D0
C     Set boundaries of eclipsed area on colatitude profile KEYSAVE
		IF (DABS(X1)-DABS(X4).LT.1.D-15) THEN
			RANGE=DABS(X2)+DABS(X3)
			IF (X2.LT.X3) THEN
				XST=X2
			ELSE
				XST=X3
			END IF
			DO K=1,KCTB1
				XTP(K)=XHORZL(IEC,K)
				YTP(K)=YHORZL(IEC,K)
			END DO
			DO K=1,11
				XXX=XST+(K-1)*RANGE/10.D0
				XRF(K)=XXX
				CALL SPLINT(XTP,YTP,SPL1U,KCTA1,XXX,YYY)
				YRF(K)=YYY+YRFZ
			END DO
		ELSE
			CALL PMDSTOP
		END IF
C     Integrate over eclipsed area below colatitude profile KEYSAVE
	DO KKK=1,11
C	WRITE (NOUT8,3055) XRF(KKK),YRF(KKK)
	END DO
		CALL SMP(YRF,1,11,RANGE/10.D0,0.D0,0.D0,1,ER,11,SMF)
		SMF=DABS(SMF)
C     Add this area to total
		SM20A=SM20A+SMF
C     *********************************
	IF (ISV.EQ.3-IEC) SM20A=SM20A+SM
	IF (ISV.EQ.IEC) SM20B=SM20B+SM
C	**********************************
   48 CONTINUE
C     ***********************************************
C     PRINT THE VALUES IN VITG5()
C
	SMM=0.D0
	DO ITH=1,NTH
C	WRITE (NOUT8,3056) ITH,VITG5(II,ITH)
	SMM=SMM+VITG5(II,ITH)
	END DO
C	WRITE (NOUT8,3056) 0,SMM
C	SMM=SMM+SMD+SMF
C	WRITE (NOUT8,3056) -1,SMM
 3056 FORMAT(I5,F15.10)
C     ***********************************************
C
C
C     ----------------
C     First Component
C     ----------------
C      
      SURF1=0.D0
      SURF2=0.D0
      SM10A=0.D0
      SM20A=0.D0
      BOL1=0.D0
      SMCO=0.D0
      SMDO=0.D0
      BOL2=0.D0
      SM10B=0.D0
      SM20B=0.D0
      SMEO=0.D0
      SMFO=0.D0            
      CF=RI*RI
      VTHST=0.D0
	FLUX1=0.D0
	FLUX2=0.D0
C     -----------------
      DO 22 ITH=1,NTH
      VITG1(I,ITH)=0.D0
	VITG1A(I,ITH)=0.D0
      VITG2(I,ITH)=0.D0
      VITG3(I,ITH)=0.D0
      VITG4(I,ITH)=0.D0
      VITG5(I,ITH)=0.D0
      VITG6(I,ITH)=0.D0
      IF (ITH.EQ.1) THEN
         VTHA=VTHST
      ELSE
         VTHA=VTH
      END IF
	VTH=TH(I,ITH)
      CSTH1=DCOS(VTHA)
      CSTH2=DCOS(VTH)
      DLCS=CSTH1-CSTH2
      N=NPH(I,ITH)      
      IF (N.LE.0) CALL PMDSTOP
      IF (ITH.EQ.1) GO TO 22
      IF (ITH.LT.NTH) THEN
C
         DO 20 K=1,N  
C     Set visibility keys for eclipsing component.
C     First set keys negative for points below horizon.
         IF (CSG(I,ITH,K).LT.0.D0) STVS(I,ITH,K)=CSG(I,ITH,K)
	   VPHV=PH(I,ITH,K)
C     Initialize all points between start and end of visible
C     limits to visibility true condition. 
         IF ((VPHV.GE.VST(I,ITH,1)).AND.(VPHV.LE.VND(I,ITH,1)))
     $   STVS(I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(I,ITH,2)).AND.(VPHV.LE.VND(I,ITH,2)))
     $   STVS(I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(I,ITH,3)).AND.(VPHV.LE.VND(I,ITH,3)))
     $   STVS(I,ITH,K)=1.D0
C     Set visibility keys to allow for eclipse effects.
         IF (I.NE.IEC) THEN
			IF ((VPHV.GE.SHS(ITH,1)).AND.(VPHV.LE.SHN(ITH,1)))
     $		THEN 
      		IF (DABS(SHS(ITH,1)-SHN(ITH,1)).GT.1.0E-12)
     $			STVS(I,ITH,K)=-1.D0
			END IF
			IF ((VPHV.GE.SHS(ITH,2)).AND.(VPHV.LE.SHN(ITH,2)))
     $        THEN
              IF (DABS(SHS(ITH,2)-SHN(ITH,2)).GT.1.0E-12) 
     $			STVS(I,ITH,K)=-1.D0
			END IF
			IF ((VPHV.GE.SHS(ITH,3)).AND.(VPHV.LE.SHN(ITH,3)))
     $		THEN
			IF (DABS(SHS(ITH,3)-SHN(ITH,3)).GT.1.0E-12)
     $			STVS(I,ITH,K)=-1.0
			END IF
	   END IF
C     DSGA REPRESENTS AN ORDINATE OF FN. TO BE INTEGRATED FOR SURFACE
C     AREA
C     AR3() contains the longitudes on the star of the grid points 
	   VPHV=PH(I,ITH,K) 
         AR3(I,ITH,K)=VPHV
         DSGA(I,ITH,K)=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)
	   DSGAF(I,ITH,K)=DSGA(I,ITH,K)*BDI(I,ITH,K)*PI
         VL1A(I,ITH,K)=DABS(CSG(I,ITH,K))*DSGA(I,ITH,K)
C     AA is a variable to include segments that are visible.
C     BB is a variable that ultimately will include areas that
C     are eclipsed.
         AA=1.0
         BB=0.D0
C     If the point is below the horizon, set AA=0.
         IF (CSG(I,ITH,K).LT.0.D0) AA=0.D0
         IF (CSG(I,ITH,K).GE.0.D0) BB=1.D0
C     Initially, retain those areas that are above horizon.
         VL5A(I,ITH,K)=BB*VL1A(I,ITH,K)         
         VL1A(I,ITH,K)=AA*VL1A(I,ITH,K)
	   CSGABS=DABS(CSG(I,ITH,K))
		IF (KDARK.EQ.1) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
		END IF
		IF (KDARK.EQ.2) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
		END IF
		IF (KDARK.EQ.3) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		-U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
		END IF
         VL2A(I,ITH,K)=BDI(I,ITH,K)*CFA*AA*VL1A(I,ITH,K)
         VL4A(I,ITH,K)=BDI(I,ITH,K)*CFA*BB*VL1A(I,ITH,K)
         VL3A(I,ITH,K)=SGMA*(1.0D3*TMPR(I,ITH,K))**4
     $    *DSGA(I,ITH,K)*CF    
         IF (K.EQ.1) GO TO 20   
C     Accumulate integrals for area and related calculations
         Y1=DSGA(I,ITH,K-1)
         Y2=DSGA(I,ITH,K)
	   Y1A=DSGAF(I,ITH,K-1)
	   Y2A=DSGAF(I,ITH,K)
         Y5=VL1A(I,ITH,K-1)
         Y6=VL1A(I,ITH,K)
         Y9=VL3A(I,ITH,K-1)
         Y10=VL3A(I,ITH,K)
         Y13=VL2A(I,ITH,K-1)
         Y14=VL2A(I,ITH,K)
         Y17=VL5A(I,ITH,K-1)
         Y18=VL5A(I,ITH,K)
         Y21=VL4A(I,ITH,K-1)
         Y22=VL4A(I,ITH,K)
C     Guarantee that corresponding longitudes on circles ITH and ITH-1
C     are the same. This means that each colatitued circle has the same 
C     number of grid points.
         IF (ITH.GT.2) THEN
            IF (DABS(AR3(I,ITH,K)-AR3(I,ITH-1,K)).GT.1.D-9)
     $       CALL PMDSTOP 
            Y3=DSGA(I,ITH-1,K-1)
            Y4=DSGA(I,ITH-1,K)
		  Y3A=DSGAF(I,ITH-1,K-1)
		  Y4A=DSGAF(I,ITH-1,K)
            Y7=VL1A(I,ITH-1,K-1)
            Y8=VL1A(I,ITH-1,K)
            Y11=VL3A(I,ITH-1,K-1)
            Y12=VL3A(I,ITH-1,K)
            Y15=VL2A(I,ITH-1,K-1)
            Y16=VL2A(I,ITH-1,K)
            Y19=VL5A(I,ITH-1,K-1)
            Y20=VL5A(I,ITH-1,K)
            Y23=VL4A(I,ITH-1,K-1)
            Y24=VL4A(I,ITH-1,K)
            DV=4.D0
C     Case ITH.LE.2
         ELSE
            Y3=0.D0
            Y4=0.D0
		  Y3A=0.D0
		  Y4A=0.D0
            Y7=0.D0
            Y8=0.D0
            Y11=0.D0
            Y12=0.D0
            Y15=0.D0
            Y16=0.D0
            Y19=0.D0
            Y20=0.D0
            Y23=0.D0
            Y24=0.D0
            DV=2.D0
         END IF
         DLPH=AR3(I,ITH,K)-AR3(I,ITH,K-1)      
         ARYF(K)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPHC
	   ARYFA(K)=(Y1A+Y2A+Y3A+Y4A)/DV*DLCS*DLPH
         DVSV=DV
		IF (ITH.LE.2) THEN
         IF (((Y5+Y6).NE.0.D0).AND.((Y7+Y8).EQ.0.D0)) THEN
			DV=2.D0
	   ELSE		  
			DV=DVSV
	   END IF
		END IF
         ARYG(K)=(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH
	   VL1(I,ITH,K)=ARYG(K)
         ARYH(K)=(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH
         ARYI(K)=(Y13+Y14+Y15+Y16)/DV*DLCS*DLPH
C     Do approximation to fraction of segment eclipsed when shadow
C     crosses the segment
	   BB=0.D0
        IF ((K.GT.1).AND.(ITH.GE.2)) THEN
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,K) references the lower 
C     right corner of the area segment.
			CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1)+CSG(I,ITH-1,K-1)+
     $				CSG(I,ITH-1,K))/4.D0
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
			IF (CSGM.GT.0.D0) THEN
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
C     $				(DABS(STVS(I,ITH-1,K-1))-CSG(I,ITH-1,K-1))
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
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $				.GE.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $					.GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $					.GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
     $			 THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $				.GE.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF (DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $					.GT.1.D-05)THEN
     					BB=DABS(STVS(I,ITH,K-1))
					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $				.GE.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $					.GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $					.GT.1.D-05)) THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $				.GE.0.D0))THEN 
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.(STVS(I,ITH-1,K)
     $				.LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $					.GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05)) 
     $				THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K)
     $				.LT.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $					.GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05)) 
     $				THEN
      				BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
     $				STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K-1))
						GO TO 33
					END IF
					END IF
				END IF
C				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
C     $				0.D0))THEN
C                                IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
C     $                              (STVS(I,ITH-1,K).GE.0.D0)) THEN
C                                IF (DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
C     $                          THEN
C                                BB=DABS(STVS(I,ITH,K))
C                                BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K)
C     $                          +STVS(I,ITH-1,K))
C                                GO TO 33   
C					END IF
C					END IF
C				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $					.GT.1.D-05))THEN
      					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K)
     $				.LT.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)
     $					-CSG(I,ITH-1,K)).GT.1.D-05))THEN
      					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K))
						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K)
     $				.GE.0.D0))THEN 
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.(STVS(I,ITH-1,K)
     $				.LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $						CSG(I,ITH-1,K)).GT.1.D-05)) THEN
      					BB=DABS(STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
						BB=0.D0
						GO TO 33
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $					1.D-05) THEN
      					BB=DABS(STVS(I,ITH-1,K-1))
						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $						STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF (DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05)THEN
      					BB=DABS(STVS(I,ITH-1,K))
						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $						STVS(I,ITH-1,K-1))
						GO TO 33
					END IF
					END IF
				END IF
			END IF
		END IF
   33    CONTINUE
         ARYJ(K)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
         ARYK(K)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
	   VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
   20    CONTINUE
C     ITH=NTH in following section.
      ELSE
         N=NPH(I,ITH-1)
         DO K=2,N
            Y1=DSGA(I,ITH-1,K-1)
            Y2=DSGA(I,ITH-1,K)
	      Y1A=Y1*BDI(I,ITH-1,K-1)
	      Y2A=Y2*BDI(I,ITH-1,K)
            Y5=VL1A(I,ITH-1,K-1)
            Y6=VL1A(I,ITH-1,K)
            Y9=VL3A(I,ITH-1,K-1)
            Y10=VL3A(I,ITH-1,K)
            Y13=VL2A(I,ITH-1,K-1)
            Y14=VL2A(I,ITH-1,K)
		  CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1))/2.D0
		  BB=0.D0
		  IF (CSGM.GT.0.D0) THEN
			IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.0.D0)) 
     $			BB=1.D0
		  END IF 
            Y17=VL5A(I,ITH-1,K-1)*BB
            Y18=VL5A(I,ITH-1,K)*BB
            Y21=VL4A(I,ITH-1,K-1)*BB
            Y22=VL4A(I,ITH-1,K)*BB
            DLPH=AR3(I,ITH-1,K)-AR3(I,ITH-1,K-1)
            DV=2.D0
            ARYF(K)=(Y1+Y2)/DV*DLCS*DLPH
	      ARYFA(K)=(Y1A+Y2A)/DV*DLCS*DLPH
            ARYG(K)=(Y5+Y6)/DV*DLCS*DLPH
	      VL1(I,ITH,K)=ARYG(K)
            ARYH(K)=(Y9+Y10)/DV*DLCS*DLPH
            ARYI(K)=(Y13+Y14)/DV*DLCS*DLPH
            ARYJ(K)=(Y17+Y18)/DV*DLCS*DLPH
            ARYK(K)=(Y21+Y22)/DV*DLCS*DLPH
            VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
	      VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
            VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
            VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
            VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
            VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
            VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
         END DO
      END IF         
   22 CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      WRITE (NOUT1,1000) (VITG2(I,ITH)*RI2,ITH=1,NTH)
C	write (nout1,1001)
C	IF (I.LT.1000) STOP 'PGD: I < 1000'
C 1000 FORMAT (6E12.5)
C 1001 FORMAT (' ')
C     ----------------------------
C     Sum contributions from all latitude strips, from 
C     upper pole to lower pole.    
      DO ITH=1,NTH
         SURF1=SURF1+VITG1(I,ITH)
	   FLUX1=FLUX1+VITG1A(I,ITH)
         SM10A=SM10A+VITG2(I,ITH)
         BOL1=BOL1+VITG3(I,ITH)
         SMCO=SMCO+VITG4(I,ITH)
         SMDO=SMDO+VITG6(I,ITH)
         SM20A=SM20A+VITG5(I,ITH)
      END DO
C
C
C     Set component pointer to other star.
C     ------------------------------------  
      I=3-I 
C  
C     CORRESPONDING LOOP FOR SECOND COMPONENT
C
      VTHST=0.D0
C	  
C
      DO 42 ITH=1,NTH
      VITG1(I,ITH)=0.D0
	VITG1A(I,ITH)=0.D0
      VITG2(I,ITH)=0.D0
      VITG3(I,ITH)=0.D0
      VITG4(I,ITH)=0.D0
      VITG5(I,ITH)=0.D0
      VITG6(I,ITH)=0.D0
      IF (ITH.EQ.1) THEN

         VTHA=VTHST
      ELSE
         VTHA=VTH
      END IF
	VTH=TH(I,ITH)
      CSTH1=DCOS(VTHA)
      CSTH2=DCOS(VTH)
      DLCS=CSTH1-CSTH2      
      N=NPH(I,ITH)      
      IF (N.LE.0) CALL PMDSTOP 
      IF (ITH.EQ.1) GO TO 42

      IF (ITH.LT.NTH) THEN
C
        DO 40 K=1,N
C     Set visibility keys for eclipsing component.
C     First set keys negative for points below horizon.
         IF (CSG(I,ITH,K).LT.0.D0) STVS(I,ITH,K)=CSG(I,ITH,K)
	   VPHV=PH(I,ITH,K)
C     Initialize all points between start and end of visible
C     limits to visibility true condition. 
         IF ((VPHV.GE.VST(I,ITH,1)).AND.(VPHV.LE.VND(I,ITH,1)))
     $   STVS(I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(I,ITH,2)).AND.(VPHV.LE.VND(I,ITH,2)))
     $   STVS(I,ITH,K)=1.D0
         IF ((VPHV.GE.VST(I,ITH,3)).AND.(VPHV.LE.VND(I,ITH,3)))
     $   STVS(I,ITH,K)=1.D0
C     Set visibility keys to allow for eclipse effects.
C     Note that these are eclipses due to companion star.
         IF (I.NE.IEC) THEN
			IF ((VPHV.GE.SHS(ITH,1)).AND.(VPHV.LE.SHN(ITH,1)))
     $		THEN 
      		IF (DABS(SHS(ITH,1)-SHN(ITH,1)).GT.1.0E-12)
     $			STVS(I,ITH,K)=-1.D0
			END IF
			IF ((VPHV.GE.SHS(ITH,2)).AND.(VPHV.LE.SHN(ITH,2)))
     $        THEN
              IF (DABS(SHS(ITH,2)-SHN(ITH,2)).GT.1.0E-12) 
     $			STVS(I,ITH,K)=-1.D0
			END IF
			IF ((VPHV.GE.SHS(ITH,3)).AND.(VPHV.LE.SHN(ITH,3)))
     $		THEN
			IF (DABS(SHS(ITH,3)-SHN(ITH,3)).GT.1.0E-12)
     $			STVS(I,ITH,K)=-1.0
			END IF
	   END IF
C     DSGA represents an ordinate of fn. to be integrated for surface
C     area.
C     AR3() contains the longitudes on the star of the grid points.	      
		VPHV=PH(I,ITH,K)
         AR3(I,ITH,K)=VPHV
         DSGA(I,ITH,K)=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)
	   DSGAF(I,ITH,K)=DSGA(I,ITH,K)*BDI(I,ITH,K)*PI
         VL1A(I,ITH,K)=DABS(CSG(I,ITH,K))*DSGA(I,ITH,K)
C     AA is a variable to include segments that are visible.
C     BB is a variable that ultimately will include areas that
C     are eclipsed.
         AA=1.0D0
         BB=0.D0
C     If the point is below the horizon, set AA=0.
         IF (CSG(I,ITH,K).LT.0.D0) AA=0.D0
         IF (CSG(I,ITH,K).GE.0.D0) BB=1.D0
C     Initially, retain those areas that are above horizon.
         VL5A(I,ITH,K)=BB*VL1A(I,ITH,K)          
         VL1A(I,ITH,K)=AA*VL1A(I,ITH,K)
	   CSGABS=DABS(CSG(I,ITH,K))
		IF (KDARK.EQ.1) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		 -U2(I,ITH,K)*(1.D0-CSGABS)**2)*CF
		END IF
		IF (KDARK.EQ.2) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		 -U2(I,ITH,K)*(1.D0-DSQRT(CSGABS)))*CF
		END IF
		IF (KDARK.EQ.3) THEN
			CFA=(1.D0-U1(I,ITH,K)*(1.D0-CSGABS)
     $		 -U2(I,ITH,K)*CSGABS*DLOG(CSGABS))*CF
		END IF
         VL2A(I,ITH,K)=BDI(I,ITH,K)*CFA*AA*VL1A(I,ITH,K)
         VL4A(I,ITH,K)=BDI(I,ITH,K)*CFA*BB*VL1A(I,ITH,K)
         VL3A(I,ITH,K)=SGMA*(1.0D3*TMPR(I,ITH,K))**4
     $    *DSGA(I,ITH,K)*CF    
         IF (K.EQ.1) GO TO 40
C     Accumulate integrals for area and related calculations.
         Y1=DSGA(I,ITH,K-1)
         Y2=DSGA(I,ITH,K)
	   Y1A=DSGAF(I,ITH,K-1)
	   Y2A=DSGAF(I,ITH,K)
         Y5=VL1A(I,ITH,K-1)
         Y6=VL1A(I,ITH,K)
         Y9=VL3A(I,ITH,K-1)
         Y10=VL3A(I,ITH,K)
         Y13=VL2A(I,ITH,K-1)
         Y14=VL2A(I,ITH,K)
         Y17=VL5A(I,ITH,K-1)
         Y18=VL5A(I,ITH,K)
         Y21=VL4A(I,ITH,K-1)
         Y22=VL4A(I,ITH,K)
C     Guarantee that corresponding longitudes on circles ITH and ITH-1
C     are the same. This means that each colatitued circle has the same 
C     number of grid points.
         IF (I.GT.2) THEN
            IF (DABS(AR3(I,ITH,K)-AR3(I,ITH-1,K)).GT.1.D-9)
     $         CALL PMDSTOP
            Y3=DSGA(I,ITH-1,K-1)
            Y4=DSGA(I,ITH-1,K)
	      Y3A=DSGAF(I,ITH-1,K-1)
	      Y4A=DSGAF(I,ITH-1,K)
            Y7=VL1A(I,ITH-1,K-1)
            Y8=VL1A(I,ITH-1,K)
            Y11=VL3A(I,ITH-1,K-1)
            Y12=VL3A(I,ITH-1,K)
            Y15=VL2A(I,ITH-1,K-1)
            Y16=VL2A(I,ITH-1,K)
            Y19=VL5A(I,ITH-1,K-1)
            Y20=VL5A(I,ITH-1,K)
            Y23=VL4A(I,ITH-1,K-1)
            Y24=VL4A(I,ITH-1,K)
            DV=4.D0
C     Case ITH.LE.2
         ELSE
            Y3=0.D0
            Y4=0.D0
	      Y3A=0.D0
	      Y4A=0.D0
            Y7=0.D0
            Y8=0.D0
            Y11=0.D0
            Y12=0.D0
            Y15=0.D0
            Y16=0.D0
            Y19=0.D0
            Y20=0.D0
            Y23=0.D0
            Y24=0.D0
            DV=2.D0
         END IF
         DLPH=AR3(I,ITH,K)-AR3(I,ITH,K-1)
         ARYF(K)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH
	   ARYFA(K)=(Y1A+Y2A+Y3A+Y4A)/DV*DLCS*DLPH
	   DVSV=DV
		IF (ITH.LE.2) THEN
         IF (((Y5+Y6).NE.0.D0).AND.((Y7+Y8).EQ.0.D0)) THEN
			DV=2.D0
	   ELSE
			DV=DVSV
	   END IF
		END IF
         ARYG(K)=(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH
	   VL1(I,ITH,K)=ARYG(K)
         ARYH(K)=(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH
         ARYI(K)=(Y13+Y14+Y15+Y16)/DV*DLCS*DLPH
C     Do approximation to fraction of segment eclipsed when shadow
C     crosses the segment
	   BB=0.D0
         IF ((K.GT.1).AND.(ITH.GE.2)) THEN
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,ITH,K) references the lower 
C     right corner of the area segment.
			CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1)+CSG(I,ITH-1,K-1)+
     $			CSG(I,ITH-1,K))/4.D0
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
			IF (CSGM.GT.0.D0) THEN
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0)) THEN
						BB=1.D0
						GO TO 32
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) 
     $				THEN
					IF ((DABS(CSG(I,ITH,K-1)-STVS(I,ITH,K-1)).GT.
     $					1.D-05).AND.
     $				(DABS(CSG(I,ITH-1,K-1)-STVS(I,ITH-1,K-1)).GT.
     $					1.D-05))THEN 
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K))
					BB=BB/(BB+STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-5).AND.(DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
     $					.GT.1.D-5).AND.
     $		        (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $					1.D-05)) THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K-1))
					BB=BB/(BB+STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-05).AND.(DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $					CSG(I,ITH-1,K)).GT.1.D-05))THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH-1,K-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
     $					CSG(I,ITH-1,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05))THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K))

						GO TO 32

					END IF

					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-5)) THEN
					BB=DABS(STVS(I,ITH,K-1))
					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
     $					CSG(I,ITH-1,K-1)).GT.1.D-05))THEN 
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $					CSG(I,ITH-1,K)).GT.1.D-05)) THEN
					BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $					CSG(I,ITH-1,K)).GT.1.D-05))THEN
      					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
     $						STVS(I,ITH-1,K))
						BB=BB/(BB+STVS(I,ITH,K-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF (DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					THEN 
					BB=DABS(STVS(I,ITH,K))
					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					.AND.(DABS(STVS(I,ITH-1,K-1)-
     $					CSG(I,ITH-1,K-1)).GT.1.D-05))THEN 
      				BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					.AND.(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05))THEN 
					BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $					1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
     $					THEN 
					BB=DABS(STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
						BB=0.D0
						GO TO 32
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $				1.D-05) THEN
      				BB=DABS(STVS(I,ITH-1,K-1))
					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $				STVS(I,ITH-1,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF (DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05)THEN 
      				BB=DABS(STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K-1))
						GO TO 32
					END IF
					END IF
				END IF
			END IF
		END IF
   32    CONTINUE		 	
         ARYJ(K)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
         ARYK(K)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
	   VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)   
   40    CONTINUE
C     ITH=NTH in following section.
      ELSE
         N=NPH(I,ITH-1)
         DO K=2,N
            Y1=DSGA(I,ITH-1,K-1)
            Y2=DSGA(I,ITH-1,K)
	      Y1A=Y1*BDI(I,ITH-1,K-1)
	      Y2A=Y2*BDI(I,ITH-1,K)
            Y5=VL1A(I,ITH-1,K-1)
            Y6=VL1A(I,ITH-1,K)
            Y9=VL3A(I,ITH-1,K-1)
            Y10=VL3A(I,ITH-1,K)
            Y13=VL2A(I,ITH-1,K-1)
            Y14=VL2A(I,ITH-1,K)
		  CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1))/2.D0
		  BB=0.D0
		  IF (CSGM.GT.0.D0) THEN
			IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.0.D0)) 
     $			BB=1.D0
		  END IF
            Y17=VL5A(I,ITH-1,K-1)*BB
            Y18=VL5A(I,ITH-1,K)*BB
            Y21=VL4A(I,ITH-1,K-1)*BB
            Y22=VL4A(I,ITH-1,K)*BB
            DLPH=AR3(I,ITH-1,K)-AR3(I,ITH-1,K-1)
	      DV=2.D0
            ARYF(K)=(Y1+Y2)/DV*DLCS*DLPH
	      ARYFA(K)=(Y1A+Y2A)/DV*DLCS*DLPH
            ARYG(K)=(Y5+Y6)/DV*DLCS*DLPH
	      VL1(I,ITH,K)=ARYG(K)
            ARYH(K)=(Y9+Y10)/DV*DLCS*DLPH
            ARYI(K)=(Y13+Y14)/DV*DLCS*DLPH
            ARYJ(K)=(Y17+Y18)/DV*DLCS*DLPH
            ARYK(K)=(Y21+Y22)/DV*DLCS*DLPH
            VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
	      VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
            VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
            VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
            VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
            VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)  
            VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
         END DO
      END IF      
   42 CONTINUE
C     
C  
C     Sum contributions from all latitude strips, from
C     upper pole to lower pole.
      DO ITH=1,NTH
         SURF2=SURF2+VITG1(I,ITH)
	   FLUX2=FLUX2+VITG1A(I,ITH)
         SM10B=SM10B+VITG2(I,ITH)
         BOL2=BOL2+VITG3(I,ITH)
         SMEO=SMEO+VITG4(I,ITH)
         SMFO=SMFO+VITG6(I,ITH)
         SM20B=SM20B+VITG5(I,ITH)
      END DO
C     Reset component designations to initial values
      I=3-I      
      SURF1=SURF1*RI2
      ER1A=0.001D0*SURF1
      SM10A=SM10A*RI2
      ER10A=0.001D0*SM10A
      BOLER1=0.001D0*BOL1       
      SMCOER=0.001D0*SMCO
      SMDOER=0.001D0*SMDO
      SM20A=SM20A*RI2
      ER20A=0.001D0*SM20A
	FLUX1=FLUX1*RI2    
      SURF2=SURF2*RI2 
      ER2A=0.001D0*SURF2
      SM10B=SM10B*RI2
      ER10B=0.001D0*SM10B
      BOLER2=0.001D0*BOL2
      SMEOER=0.001D0*SMEO
      SMFOER=0.001D0*SMFO
      SM20B=SM20B*RI2
      ER20B=0.001D0*SM20B
	FLUX2=FLUX2*RI2
	   WRITE (NOUT1,151)
         WRITE (NOUT1,111) I,SURF1,ER1A
         WRITE (NOUT1,111) 3-I,SURF2,ER2A
         WRITE (NOUT1,112) I,BOL1,BOLER1
         WRITE (NOUT1,112) 3-I,BOL2,BOLER2
	WRITE (NOUT1,262) FLUX1
	WRITE (NOUT1,263) FLUX2   
      TPRI=(BOL1/SURF1/SGMA)**0.25
      TPSC=(BOL2/SURF2/SGMA)**0.25
      VMG1=CVMG-2.5D0*DLOG10(BOL1/SOLF)
      VMG2=CVMG-2.5D0*DLOG10(BOL2/SOLF)
         WRITE (NOUT1,119) I,TPRI
         WRITE (NOUT1,129) I,VMG1
         WRITE (NOUT1,120) 3-I,TPSC


         WRITE (NOUT1,130) 3-I,VMG2
      SURFSM=SURF1+SURF2
C     Allow for both faces of accretion disk
C     Note that this calculation excludes irradiation, to permit
C     comparison with model of energy produced by mass transfer.
      BOLSM=BOL1+BOL2
      TAV=(BOLSM/SURFSM/SGMA)**0.25
C     Note that the following abs. mag. is as seen by the observer!
C     Only one face of accret. disk is visible, and includes 
C     irradiation      
      VMGSM=CVMG-2.5D0*DLOG10((BOL1+BOL2)/SOLF)
      VLTSM=SMCO+SMEO
      VLTSB=SMDO+SMFO
		IF (SMCO.NE.0.D0) THEN
			VLSA=SMDO/SMCO
		END IF
	  IF (SMEO.NE.0.D0) THEN
		VLSB=SMFO/SMEO
	  END IF
	  IF (SMEO.EQ.0.D0) VLSB=1.D99
         WRITE (NOUT1,125) TAV
         WRITE (NOUT1,147) BOLSM
         WRITE (NOUT1,148) VMGSM
      WRITE (NOUT1,145) VIV,KS,PSIP
      WRITE (NOUT1,113) SP,I,SM10A,ER10A,SM20A,ER20A,VLSA
      WRITE (NOUT1,116) SP,3-I,SM10B,ER10B,SM20B,ER20B,VLSB
      WRITE (NOUT1,146) WLV
      WRITE (NOUT1,114) I,U1(I,1,1),U2(I,1,1),SMCO,SMCOER,SMDO,
     $   SMDOER,VLSA
      WRITE (NOUT1,115) 3-I,U1(3-I,1,1),U2(3-I,1,1),SMEO,SMEOER,SMFO,
     $   SMFOER,VLSB
      VLT=VLTSM-VLTSB
	IF (VLTSM.NE.0.D0) THEN
		FLOSS=VLTSB/VLTSM
	END IF
      WRITE (NOUT1,121)VLTSM,VLTSB,VLT 
      WRITE (NOUT1,152) FLOSS
	WRITE (NOUT1,102)
C     Write VL2A() values for program RDVEL2
      DO IPP=1,2
		DO ITH=1,NTH
		NN=NPH(IPP,ITH)
			WRITE (NOUT7,106) IWL,KS,IPP,ITH,NN
			WRITE (NOUT7,107) (VL2A(IPP,ITH,KK),KK=1,NN)
		END DO
	END DO
C     Write VL1() values for RDVEL2
C     Note first that the VL1 values must be summed for ITH=NTH
      DO IPP=1,2
		VL1(IPP,NTH,1)=0.D0
		NN=NPH(IPP,NTH/2+1)
		DO KK=2,NN
			VL1(IPP,NTH,1)=VL1(IPP,NTH,1)+VL1(IPP,NTH,KK)
		END DO
	END DO
      DO IPP=1,2
		DO ITH=1,NTH
		NN=NPH(IPP,ITH)
			WRITE (NOUT7,106) IPP,ITH,NN
			WRITE (NOUT7,107) (VL1(IPP,ITH,KK)*RI2,KK=1,NN)
		END DO
	END DO
C     Write STVS values for external check
      DO IPP=1,2
		DO ITH=1,NTH
		NN=NPH(IPP,ITH)
			WRITE (NOUT4,106) IWL,KS,IPP,ITH,NN
			WRITE (NOUT4,162) (STVS(IPP,ITH,KK),KK=1,NN)
		END DO
	END DO		          
C    Write output
      CALL WTOUT5(I,IWL,VI,PSIP,SP,SMCO,SMCOER,SMDO,SMDOER,SMEO,
     $SMEOER,SMFO,SMFOER,VLTSM,VLTSB,1.D-3*VLTSM,VLT,1.D-3*VLT,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,NOUT3)
      WRITE (NOUT6,107) PSIP,VI,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0
   60 CONTINUE
      WRITE (NOUT1,123)
      REWIND IN5
      REWIND IN6
	REWIND IN9
  160 FORMAT(8E10.3)
  161 FORMAT (4E17.8)
  162 FORMAT (10F8.3)
   50 CONTINUE   
C      CONTINUE   
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
      WRITE (NOUT1, 205) TM
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=IN3,   STATUS='KEEP')
      CLOSE (UNIT=IN4,   STATUS='KEEP')
      CLOSE (UNIT=IN5,   STATUS='KEEP')
      CLOSE (UNIT=IN6,   STATUS='KEEP')
      CLOSE (UNIT=IN7,   STATUS='KEEP')
      CLOSE (UNIT=IN8,   STATUS='KEEP')
      CLOSE (UNIT=IN9,   STATUS='KEEP')
      CLOSE (UNIT=IN10,  STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      CLOSE (UNIT=NOUT6, STATUS='KEEP')
      CLOSE (UNIT=NOUT7, STATUS='KEEP')
      CLOSE (UNIT=NOUT8, STATUS='KEEP')
      CLOSE (UNIT=21,    STATUS='KEEP')
      CLOSE (UNIT=22,    STATUS='KEEP')
      WRITE (IN1,101) KEYGOOD
      CLOSE (UNIT=IN1,   STATUS='KEEP')
      STOP 'PGD: Completed successfully.'
      END PROGRAM PGD
