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
      DIMENSION VL4A(MTW,MTHM,MPHM),VL5A(MTW,MTHM,MPHM),ARYJ(MPHM),
     $ARYK(MPHM),
     $VITG5(MTW,MPHM),VITG6(MTW,MPHM),AR3(MTW,MTHM,MPHM),
     $VITG1(MTW,MPHM),VITG1A(MTW,MPHM),
     $VITG2(MTW,MPHM),
     $DSGA(MTW,MTHM,MPHM),DSGAF(MTW,MTHM,MPHM)
C
      DIMENSION STVS(MTW,MTHM,MPHM),ARYF(MTHM),ARYG(MTHM),ARYFA(MTHM)
C
C
      DIMENSION UC(MTW),C1(MTW),C2(MTW),T0(MTW),A(MTW),BCF(MTW),
     $UA(MTW),
     $WL(MWL),
     $VLPSI(MPHS),LV(13)
      DIMENSION FL1(16)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 36 FL1,BFR
      CHARACTER * 7 LABEL
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD, KEYBAD
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8/1,2,3,4,5,66,7,8/
      DATA NOUT1,NOUT2,NOUT3,IN13/12,13,14,15/
	DATA NOUT4,NOUT5,NOUT6,NOUT7,NOUT8/16,17,18,19,20/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/ 
C     Solar bolometric output in ergs/sec.
      DATA SOLF/3.846D33/
C     Factor for producing bolometric abs. mag.
      DATA CVMG/4.75D0/ 
C     Radiation constant
      DATA SGMA/5.6696D-5/
      DATA TWPI/6.2831853071795864769D0/
      DATA PI/3.141592653589793238463D0/
      DATA PIO2/1.5707963267948966192D0/	                         
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
      READ ( 21, 100)    (FL1(J),J=1,16)
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
      OPEN (UNIT=NOUT1, FILE=FL1(9),  STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(10), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(11), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(12), STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(13), STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT7, FILE=FL1(15), STATUS='REPLACE')
      OPEN (UNIT=NOUT8, FILE=FL1(16), STATUS='REPLACE')
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
      READ (IN5,106) KCTRL,NPSI
      IF (NPSN.EQ.0) NPSN=NPSI  
   10 WRITE (NOUT1,105) NPSI,NPST,NPSN
C     INPUT PERMANENT FILE DATA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN3)
      RI2=RI*RI
      READ (IN6,106) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN6,128) (VLPSI(K),K=1,NPSI)
      READ (IN6,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      READ (IN6,106) (LV(K),K=1,13)
      READ (IN6,128) OMRAD,ECC,SMAJ,TH0,VCONJ,VMN0,PHZERO,THFCP,THSCP,
     $THTCP,THLCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,THTCS,
     $THLCS,THFOUR

      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),    
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),   
     $UA(3-I),IN7)
      WRITE (NOUT8,106) NPSI,NPST,NPSN,NWL
      WRITE (NOUT1,117)     
      WRITE (NOUT1,108) (TH(I,KK),KK=1,NTH)      

C  
C     *******************************                          
C     Loop for successive wavelengths
C     *******************************
C     This is the outermost loop
C
      DO 50 IWL=1,NWL   
      CALL RDWT6(I,IWL,NTH,IN8)
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
		N=NPH(I,ITH)
			DO K=1,N
C     Visibility keys for photosphere points, stellar components
				STVS(J,ITH,K)=0.D0
			END DO
		END DO
      END DO                                    
C     Input data from PGB
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN4)
      viv=vi*57.2957795131
   11 CONTINUE 
      WRITE (NOUT1,127) PSI,PSIP,KS
      DO ITH=1,NTH
       WRITE (NOUT1,2000) ITH,SHS(ITH,1),SHN(ITH,1),SHS(ITH,2),
     $SHN(ITH,2),SHS(ITH,3),SHN(ITH,3)
      END DO
      WRITE (NOUT1,2001)
 2000 FORMAT (I5,6F12.8)
 2001 FORMAT (' ')
      IF (KS.LT.NPST) GO TO 60
C     LOOP TO CALCULATE ARRAYS OF SURFACE AREA ELEMENTS AND 
C     RELATED QUANTITIES
C 
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
      IF (ITH.GE.24) THEN
         ZZZ=0.D0
      END IF
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
      IF ((ITH.GT.1).AND.(ITH.LT.NTH)) THEN
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
         VPHVA=PH(I,ITH-1,K-1)
         VPHVB=PH(I,ITH-1,K)
         VPHVC=PH(I,ITH,K-1)
         SHSX=SHS(ITH-1,1)
         SHSB=SHN(ITH-1,1)
         SHSC=SHS(ITH-1,2)
         SHSD=SHN(ITH-1,2)
         SHSE=SHS(ITH,1)
         SHSF=SHN(ITH,1)
         SHSG=SHS(ITH,2)
         SHSH=SHN(ITH,2)
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
C     are eclipsed. BB>0.0 means the area referenced is eclipsed.
         AA=1.0
         BB=1.D0
C     If the point is below the horizon, set AA=0.
         IF (CSG(I,ITH,K).LT.0.D0) AA=0.D0
C     Initially, retain those areas that are above horizon.
         VL5A(I,ITH,K)=AA*VL1A(I,ITH,K)         
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
C     Start processing area segments with K=2.    
         IF (K.GT.1) THEN   
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
C     are the same. This means that each colatitude circle has the same 
C     number of grid points.
         IF (ITH.GT.2) THEN
         ZZA=AR3(I,ITH,K)
         ZZB=AR3(I,ITH-1,K)
         ZZC=DABS(ZZA-ZZB)
C     Only call error if this is not a contact system
         IF ((ZZC.GT.1.D-9).AND.(KC.NE.0)) CALL PMDSTOP
C            IF (DABS(AR3(I,ITH,K)-AR3(I,ITH-1,K)).GT.1.D-9)
C     $       CALL PMDSTOP 
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
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,K) references the lower 
C     right corner of the area segment.
			CSGM=(CSG(I,ITH,K)+CSG(I,ITH,K-1)+CSG(I,ITH-1,K-1)+
     $				CSG(I,ITH-1,K))/4.D0
				AAB=STVS(I,ITH,K-1)
				AAC=STVS(I,ITH,K)
				AAD=STVS(I,ITH-1,K-1)
				AAE=STVS(I,ITH-1,K)
				BBA=CSG(I,ITH,K-1)
				BBB=CSG(I,ITH,K)
				BBC=CSG(I,ITH-1,K-1)
				BBD=CSG(I,ITH-1,K)
				VPHVA=PH(I,ITH-1,K-1)
				VPHVB=PH(I,ITH-1,K)
				VPHVC=PH(I,ITH,K-1)
				VPHV=PH(I,ITH,K)
				SHSX=SHS(ITH-1,1)
				SHSB=SHN(ITH-1,1)
				SHSC=SHS(ITH-1,2)
				SHSD=SHN(ITH-1,2)
				SHSE=SHS(ITH,1)
				SHSF=SHN(ITH,1)
				SHSG=SHS(ITH,2)
				SHSH=SHN(ITH,2)
				DDA=TH(I,ITH-1)
				DDB=TH(I,ITH)
				DDC=TH(I,ITH+1)
				DDD=TH(I,ITH-2)
				CCA=SHS(ITH+1,1)
				CCB=SHN(ITH+1,1)
				CCC=SHS(ITH+1,2)
				CCD=SHN(ITH+1,2)
				CCE=SHS(ITH-2,1)
				CCF=SHN(ITH-2,1)
				CCG=SHS(ITH-2,2)
				CCH=SHN(ITH-2,2)
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
			IF (CSGM.GT.0.D0) THEN

                        IF ((ITH.GE.13).AND.(ITH.LE.52)) THEN
                           ZZZZZ=0.D0
                        END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $					.LT.0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $						(AAE.LT.0.D0)) THEN
C    Case 16
                        CASE=16
						BB=1.D0
						AAA=0.D0
C			write (nout1,2000) ITH-1,K-1,ITH-1,K
C                        WRITE (NOUT1,2001)
C                        WRITE (NOUT1,2000) ITH,K-1,ITH,K
C                        WRITE (NOUT1,2001)
C                        WRITE (NOUT1,2001)
						GO TO 33
					END IF
				END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $					.LT.0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $				(AAE.GE.0.D0)) 
     $				THEN
C    Case 13
                        CASE=13
			IF ((VPHVC.GT.SHSE).AND.(VPHV.LE.SHSF)) THEN
C    Set VIN=VPHVC
C                                VIN=VPHVC
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C                                IF (YY1.GT.DDA) YY1=DDA
C    Set VIN=VPHV
C                                VIN=VPHV
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY2=VO
C                                IF (YY2.GT.DDA) YY2=DDAC
C				TERM1=((DDB-YY1)+(DDB-YY2))/2.D0
C				TERM2=VPHV-VPHVC
C				BB=(TERM1*TERM2)/((VPHV-VPHVC)*(DDB-DDA))
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.5D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
                                END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $				.LT.0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $					(AAE.GE.0.D0)) THEN
C    Case 14
                        CASE=14
			IF ((VPHVA.LT.SHSB).AND.(SHSB.LE.VPHVB).AND.
     $				(SHSX.LT.VPHVA)) THEN
C                                VIN=VPHV
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C                                IF (YY1.GT.DDA) YY1=DDA
C                                TRIANG=0.5D0*(YY1-DDA)*(VPHV-SHSB)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=(RECT-TRIANG)/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.75D0
						AAA=0.D0
						GO TO 33
					END IF
					END IF
				END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $				.LT.0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $					(AAE.LT.0.D0)) THEN
C    Case 15
                        CASE=15
			IF ((VPHVA.LE.SHSX).AND.(SHSX.LT.VPHVB).AND.
     $				(SHSB.GT.VPHV)) THEN
C                                VIN=VPHVC
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C				TRIANG=0.5D0*(SHSX-VPHVA)*(YY1-DDA)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=(RECT-TRIANG)/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.75D0
						AAA=0.D0
						GO TO 33
					END IF
					END IF
				END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $				.GE.0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $					(AAE.LT.0.D0)) THEN
C    Case 12
                        CASE=12
			IF ((VPHVC.LE.SHSF).AND.(SHSF.LT.VPHVB).AND.
     $				(SHSB.GT.VPHVB)) THEN
C                                VIN=VPHVB
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C				TRIANG=0.5D0*(VPHV-SHSF)*(DDB-YY1)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=(RECT-TRIANG)/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.75D0
						AAA=0.D0
						GO TO 33
					END IF
					END IF
				END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $				.GE.0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $				(AAE.GE.0.D0)) THEN
C    Case 9
                        CASE=9
C                                VIN=VPHVA
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C                                IF (YY1.GT.DDA) YY1=DDA
C				BB=0.5D0*(SHSF-VPHVA)*(DDB-YY1)
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.25D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $				.GE.0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $				(AAE.GE.0.D0)) THEN
C    Case 10
                        CASE=10
			IF ((VPHVA.LT.SHSB).AND.(SHSB.LE.VPHVB)) THEN
			IF ((VPHVC.LT.SHSF).AND.(SHSF.LE.VPHV)) THEN
C				TERM1=(SHSB-VPHVA)/(VPHVB-VPHVA)
C				TERM2=(SHSF-VPHVC)/(VPHV-VPHVC)
C				BB=(TERM1+TERM2)/2.D0
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.5D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
					END IF
				END IF
C
C
				IF ((AAB.LT.0.D0).AND.(AAC
     $				.GE.0.D0))THEN 
				IF ((AAD.GE.0.D0).AND.(AAE
     $				.LT.0.D0)) THEN
C    Case 11  can't exist
C                        CASE=11
C					CALL PMDSTOP
				END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC
     $				.LT.0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $					(AAE.LT.0.D0)) THEN
C    Case 8
                        CASE=8
		        IF ((SHSX.LT.VPHVA).AND.(SHSB.GT.VPHVB)) THEN
			IF ((SHSE.GT.VPHVC).AND.(SHSE.LT.VPHV)) THEN
C                                VIN=VPHVA
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C				TRIANG=0.5D0*(SHSE-VPHVC)*(DDB-YY1)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=(RECT-TRIANG)/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.5D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
					END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC.LT.
     $				0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $				(AAE.GE.0.D0)) THEN
C    Case 5
                        CASE=5
			IF ((SHSE.GE.VPHVC).AND.(SHSE.LT.VPHV).AND.
     $                  (SHSF.GT.VPHV)) THEN
C                                VIN=VPHV
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C				TRIANG=0.5D0*(VPHV-SHSE)*(DDB-YY1)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=TRIANG/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.25D0
						AAA=0.D0
						GO TO 33
				END IF
					END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC.LT.
     $					0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $					(AAE.GE.0.D0)) THEN
C    Case 6 can't exist
C                        CASE=6
C					CALL PMDSTOP
					END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC
     $				.LT.0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $					(AAE.LT.0.D0)) THEN
C    Case 7
                        CASE=7
			IF ((VPHVA.LE.SHSX).AND.(SHSX.LE.VPHVB).AND.
     $				(VPHVB.LT.SHSB)) THEN
			IF ((VPHVC.LE.SHSE).AND.(SHSE.LE.VPHV).AND.
     $				(VPHV.LT.SHSF)) THEN
C				TERM1=(VPHVB-SHSX)/(VPHVB-VPHVA)
C				TERM2=(VPHV-SHSE)/(VPHV-VPHVC)
C				BB=(TERM1+TERM2)/2.D0
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.5D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
					END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC
     $				.GE.0.D0))THEN 
				IF ((AAD.LT.0.D0).AND.(AAE
     $				.LT.0.D0)) THEN
C    Case 4
                        CASE=4
                        IF ((SHSX.NE.6.29D0).AND.(SHSB.NE.6.29D0)) THEN
			IF ((VPHVA.GE.SHSX).AND.(VPHVB.LT.SHSB)) THEN
C                                VIN=VPHVA
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C                                VIN=VPHVB
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY2=VO
C				TERM1=YY1-DDA
C				TERM2=YY2-DDA
C				BB=(TERM1+TERM2)/2.D0/(DDB-DDA)
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.5D0
						AAA=0.D0
						GO TO 33
                                END IF
					END IF
				END IF
					END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC.GE.
     $				0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $				(AAE.GE.0.D0)) THEN
C    Case 1
                        CASE=1
						BB=0.D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC.GE.
     $					0.D0))THEN
					IF ((AAD.LT.0.D0).AND.
     $					(AAE.GE.0.D0)) THEN
C    Case 2
                        CASE=2
			IF ((SHSB.GT.VPHVA).AND.(SHSB.LT.VPGVB).AND.
     $				(VPHVA.GT.SHSX)) THEN
C                                VIN=VPHVC
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C				TRIANG=0.5D0*(SHSB-VPHVA)*(YY1-DDA)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=TRIANG/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.25D0
						AAA=0.D0
						GO TO 33
					END IF
				END IF
				END IF
C
C
				IF ((AAB.GE.0.D0).AND.(AAC.GE.
     $					0.D0))THEN
					IF ((AAD.GE.0.D0).AND.
     $					(AAE.LT.0.D0)) THEN
C    Case 3
                        CASE=3
			IF ((VPHVA.LE.SHSX).AND.(SHSX.LT.VPHVB).AND.
     $				(SHSB.GT.VPHVB)) THEN
C                                VIN=VPHVB
C                                CALL SPLINT(YFTRY,XFTRY,Y2,KLG,VIN,VO)
C                                YY1=VO
C				TRIANG=0.5D0*(VPHV-SHSX)*(YY1-DDA)
C				RECT=(VPHV-VPHVC)*(DDB-DDA)
C				BB=TRIANG/RECT
C                                IF ((BB.LT.0.D0).OR.(BB.GT.1.D0)) THEN
C				CALL PMDSTOP
C				END IF
				BB=0.25D0
						AAA=0.D0
						GO TO 33
				END IF
					END IF
				END IF
   33    CONTINUE
C      Following END IF from IF (CSGM.GT.0.D0) THEN
         END IF
         ARYJ(K)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
         ARYK(K)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
	   VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K)
C        END IF from IF (K.GT.1) THEN
         END IF
   20    END DO
C      IF (NTH.GT.0) STOP
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
   22 END DO
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
C      IF (NTH.GT.0) STOP
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
      IF ((ITH.GT.1).AND.(ITH.LT.NTH)) THEN
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
C         IF (K.GT.1) THEN
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
C     *************************************************************
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
		IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0)) THEN
						BB=1.D0
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) 
     $				THEN
			IF ((DABS(CSG(I,ITH,K-1)-STVS(I,ITH,K-1)).GT.
     $					1.D-05).AND.
     $			(DABS(CSG(I,ITH-1,K-1)-STVS(I,ITH-1,K-1)).GT.
     $					1.D-05))THEN 
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K))
			BB=BB/(BB+STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
			IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $			1.D-5).AND.(DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
     $					.GT.1.D-5).AND.
     $		        (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $					1.D-05)) THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K-1))
					BB=BB/(BB+STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
			IF((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $			1.D-05).AND.(DABS(STVS(I,ITH,K)-CSG(I,ITH,K))
     $				.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $					CSG(I,ITH-1,K)).GT.1.D-05))THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH-1,K-1))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
			IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $				1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
     $				CSG(I,ITH-1,K-1)).GT.1.D-05).AND.
     $			(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05))THEN
      			BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
			IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $					1.D-5)) THEN
					BB=DABS(STVS(I,ITH,K-1))
			BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
			IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $				1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
     $				CSG(I,ITH-1,K-1)).GT.1.D-05))THEN 
      			BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
			BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K).GE.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
			IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1)).GT.
     $				1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $				CSG(I,ITH-1,K)).GT.1.D-05)) THEN
			BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
			BB=BB/(BB+STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
		IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $			.AND.(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $				.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K)-
     $				CSG(I,ITH-1,K)).GT.1.D-05))THEN
      			BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1)+
     $						STVS(I,ITH-1,K))
					BB=BB/(BB+STVS(I,ITH,K-1))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $					0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
			IF (DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					THEN 
					BB=DABS(STVS(I,ITH,K))
			BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)+
     $					STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
		IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $					.AND.(DABS(STVS(I,ITH-1,K-1)-
     $				CSG(I,ITH-1,K-1)).GT.1.D-05))THEN 
      				BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K-1))
			BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
			IF ((DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)
     $			.AND.(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05))THEN 
				BB=DABS(STVS(I,ITH,K)+STVS(I,ITH-1,K))
			BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
		IF ((DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $					1.D-05).AND.
     $		(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
     $					THEN 
			BB=DABS(STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
			BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
						BB=0.D0
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
		IF (DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1)).GT.
     $				1.D-05) THEN
      				BB=DABS(STVS(I,ITH-1,K-1))
			BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $				STVS(I,ITH-1,K))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
C
		IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).GE.
     $				0.D0))THEN
				IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).LT.0.D0)) THEN
			IF (DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.
     $					1.D-05)THEN 
      				BB=DABS(STVS(I,ITH-1,K))
			BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K-1))
						AAB=STVS(I,ITH,K-1)
						AAC=STVS(I,ITH,K)
						AAD=STVS(I,ITH-1,K-1)
						AAE=STVS(I,ITH-1,K)
						BBA=CSG(I,ITH,K-1)
						BBB=CSG(I,ITH,K)
						BBC=CSG(I,ITH-1,K-1)
						BBD=CSG(I,ITH-1,K)
						AAA=0.D0
						GO TO 32
					END IF
					END IF
				END IF
   32    CONTINUE
C      **********************************************************************
         END IF
          END IF		 	
         ARYJ(K)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
         ARYK(K)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K)
	   VITG1A(I,ITH)=VITG1A(I,ITH)+ARYFA(K)
         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K)
         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K)
         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K)
         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K)
         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K) 
   40    END DO
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
   42 END DO
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
C 
C     Note that this calculation excludes irradiation, to permit
C     comparison with model of energy produced by mass transfer.
      BOLSM=BOL1+BOL2
      TAV=(BOLSM/SURFSM/SGMA)**0.25
C     Note that the following abs. mag. is as seen by the observer!
C           
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
C
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
C     Note first that the VL1 values must be summed for ITH=2,NTH
      DO IPP=1,2
           DO ITH=2,NTH
		VL1(IPP,ITH,1)=0.D0
		NN=NPH(IPP,ITH/2+1)
		DO KK=2,NN
			VL1(IPP,ITH,1)=VL1(IPP,ITH,1)+VL1(IPP,ITH,KK)
		END DO
             END DO
	END DO
      DO IPP=1,2
		DO ITH=2,NTH
		NN=NPH(IPP,ITH)
			WRITE (NOUT7,106) IPP,ITH,NN
			WRITE (NOUT7,107) (VL1(IPP,ITH,KK)*RI2,KK=1,NN)
		END DO
	END DO
C     Area check
      ASM1=0.D0
      DO ITH=1,NTH
         ASM1=ASM1+VL1(1,ITH,1)*RI2
      END DO
      ASM2=0.D0
      DO LL=1,NTH
        ASM2=ASM2+VL1(2,LL,1)*RI2
      END DO
      AAA=ASM1
      BBB=ASM2
      DDD=0.D0
C     Write STVS values for external check
      DO IPP=1,2
		DO LL=1,NTH
		NN=NPH(IPP,ITH)
			WRITE (NOUT4,106) IWL,KS,IPP,LL,NN
			WRITE (NOUT4,162) (STVS(IPP,LL,KK),KK=1,NN)
		END DO
	END DO		          
C    Write output
      CALL WTOUT5(I,IWL,VI,PSIP,SP,SMCO,SMCOER,SMDO,SMDOER,SMEO,
     $SMEOER,SMFO,SMFOER,VLTSM,VLTSB,1.D-3*VLTSM,VLT,1.D-3*VLT,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,NOUT3)
      WRITE (NOUT6,107) PSIP,VI,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0
   60 CONTINUE
      WRITE (NOUT1,123)
      REWIND IN3
      REWIND IN4
	REWIND IN7
  160 FORMAT(8E10.3)
  161 FORMAT (4E17.8)
  162 FORMAT (10F8.3)
   50 CONTINUE   
  199 CONTINUE   
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
