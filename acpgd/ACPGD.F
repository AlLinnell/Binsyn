      PROGRAM ACPGD
C     Program to integrate over system components and determine
C     received light
C
C     ******************
C     Version ACPGD1
C     ******************
C
C     Important parameters
C     ---------------------
C     CF=RI*RI, where RI=component separation in cgs units.
C
C     Rim	of accretion disk
C     ---------------------
C
C     Case of cylindrical rim
C     ARM		Accumulated total rim area, cgs units
C     APROJR	Accumulated projected rim area, cgs units
C     AECR	Eclipsed rim area, cgs units
C     FLTRM	Light from rim in absence of eclipse
C     FLECRM	Rim light lost by eclipse
C     FLNECRM	Rim light
C     BOLRM	Rim bolometric luminosity (4*pi luminosity)
C
C     Face of accretion disk
C     ----------------------
C
C     AFC		Total face area
C     APROJF	Total projected face area
C     AECF	Total eclipsed face area
C     BOLFC	Bolometric luminosity, face (4*pi luminosity)
C     FLTFC	Light toward observer in absence of eclipse,
C			at the specified wavelength
C     FLECFC	Light toward observer lost by eclipse, at
C			the specified wavelength
C	FLNECFC Face light at the specified wavelength
C     FLUXFC  Face flux (4*PI), both faces, at the specified wavelength
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
C     STVS    Visibility keys
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
      DIMENSION PXR(MRM,MSG),PYR(MRM,MSG),PZR(MRM,MSG),PXF(MRING,MSG),
     $PYF(MRING,MSG),PZF(MRING,MSG),PRFH(MRING,MSG)
      DIMENSION TMPRF(MRING,MSG),TMPRR(MRM,MSG)
      DIMENSION ANNULT(MRING),ANNUBL(MRING),ANNULTA(MRING),
     $ANNUBLA(MRING)   
      DIMENSION CSGR(MRM,MSG),CSGF(MRING,MSG),
     $XHZ(MSG),YHZ(MSG),RXHZL(MSG),RYHZL(MSG),RXHZG(MSG),
     $RYHZG(MSG),
     $PRVFG(MRING),PRVFL(MRING),RVLD(MSG),ANG(MSG),RD(MSG),AGLD(MSG),
     $VC(MRM)
      DIMENSION PRRH(MRM,MSG),
     $TMPRFA(MRING,MSG),SMV(MRING,MSG),RMIFLX(MRM,MSG)
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
      DIMENSION STX(MTW,MTLM),
     $STY(MTW,MTLM),STZ(MTW,MTLM),STVS(MTW,MTHM,MPHM),PRSH(MTW,MTHM,MTW)
     $,VXP(MTW,MTHM,MTW),VYP(MTW,MTHM,MTW),ARYF(MTHM),ARYG(MTHM),
     $ARYFA(MTHM)
C
C
      DIMENSION UC(MTW),C1(MTW),C2(MTW),T0(MTW),A(MTW),BCF(MTW),
     $UA(MTW),
     $WL(MWL),
     $VLPSI(MPHS),LV(13)
      DIMENSION FL1(17)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 28 FL1
      CHARACTER * 7 LABEL
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER * 2 KEYGOOD, KEYBAD
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
      DATA IN10,IN11/10,11/
      DATA NOUT1,NOUT2,NOUT3,IN12,IN13/12,13,14,15,16/
	DATA NOUT4/17/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/ 
C     Solar bolometric output in ergs/sec.
      DATA SOLF/3.846D33/
C     Factor for producing bolometric abs. mag.
      DATA CVMG/4.75D0/ 
C     Radiation constant
      DATA SGMA/5.6696D-5/
C     Small limit
      DATA DLTA/1.0D-11/		                         
  100 FORMAT (A28)
  101 FORMAT (I2)
  102 FORMAT (1H )
  103 FORMAT (1H1)      
  104 FORMAT (1X,'PROGRAM ACPGD',30X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2)
  105 FORMAT (1X,'Number of values of orbital longitude=',I5,/,1X, 
     $'Ordinal value of starting orbital longitude=',I11,/,1X,
     $'Ending value of orbital longitude=',I5)     
  106 FORMAT (10I5)     
  108 FORMAT (6F13.8)   
  109 FORMAT (8F10.5)   
  110 FORMAT (6E20.12)
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
  122 FORMAT (8E16.8)
  123 FORMAT ('********************************************')
  124 FORMAT (1X,'Light of component',I2,1X,'at wavelength',F8.4,'=',
     $E15.7,1X,'+/-',E15.7,'ergs/sec/micron')
  125 FORMAT (1X,'System T(eff)',31X,'=',F8.1,2X,'K')
  127 FORMAT (1X,'Orbital longitude=',F12.7,/,1X,'Time from mid-pri.=',
     $F11.7,3X,'(One orbital period = two pi)',
     $/,1X,'Ordinal orbital longitude=',I5)
  129 FORMAT (1X,'Bolometric absolute magnitude, component',I2,2X,'=',
     $F10.4)
  130 FORMAT (1X,'Bolometric absolute magnitude, component',I2,2X,'=',
     $F10.4)
  131 FORMAT (1X,'Calculated surface area, accretion disk rim     =',
     $E13.4,'cm**2')
  132 FORMAT (1X,'Calculated surface area, one face of acc. disk  =',
     $E13.4,'cm**2')
  133 FORMAT (1X,'Calculated bolometric luminosity, acc. disk rim =',
     $E13.4,1X,'(4 pi)')
  134 FORMAT (1X,'Calculated bolometric luminosity, acc. disk face=',
     $E13.4,/,10X,'including irradiation, (4 pi)')
  135 FORMAT (1X,'T(eff), accretion disk rim',18X,'=',F8.1,2X,'K')
  136 FORMAT (1X,'T(eff), accretion disk face',17X,'=',F8.1,2X,'K')
  137 FORMAT (1X,'Projected area, accretion disk rim',5X,'=',E12.4,
     $'cm**2')
  138 FORMAT (1X,'Eclipsed area, accretion disk rim',6X,'=',E12.4,
     $'cm**2')
  139 FORMAT (1X,'Projected area, accretion disk face',4X,'=',E12.4,
     $'cm**2')
  140 FORMAT (1X,'Eclipsed area, accretion disk face',5X,'=',E12.4,
     $'cm**2')
  141 FORMAT (1X,'Rim light without eclipse',15X,'=',E11.4,
     $'ergs/sec/micron')
  142 FORMAT (1X,'Rim light lost by eclipse',15X,'=',E11.4,
     $'ergs/sec/micron')
  143 FORMAT (1X,'Face light without eclipse',14X,'=',E11.4,
     $'ergs/sec/micron')
  144 FORMAT (1X,'Face light lost by eclipse',14X,'=',E11.4,
     $'ergs/sec/micron')
  145 FORMAT (1X,'Orbital inclination',20X,'=',F12.8,/,1X,
     $'Ordinal value of orbital longitude',5X,'=',I3,/,1X,
     $'Orbital longitude',22X,'=',F12.8)
  146 FORMAT (1X,'Wavelength(microns)',20X,'=',F9.5)
  147 FORMAT (1X,'Bolometric luminosity, system',15X,'=',E12.4,
     $1X,'(4 pi)',/,10X,
     $'This excludes irradiation of accret.disk by central star')
  148 FORMAT (1X,'Bolometric absolute magnitude, system',7X,
     $'=',F10.4,/,10X,
     $'This is 4*pi value, omitting eclipse effects',/,10X,
     $'Includes one face of accret. disk, as affected by irradiation',
     $/,10X,'Assuming isotropic radiation by system components',/,10X, 
     $'this is the quantity measured by observer.')
  149 FORMAT (1X,'Bolometric luminosity, accretion disk',7X,'=',
     $E12.4,1X,'(4 pi)',/,10X,'omitting irradiation')
  150 FORMAT (1X,'Calculated bolometric luminosity, acc. disk face=',
     $E13.4,1X,'(4 pi)',/,10X,
     $'excluding irradiation if KEY or IRCT equal 0')
  151 FORMAT (1X,'In the following listing, component 1 always refers to
     $ the component eclipsed at primary minimum')
  152 FORMAT (1X,'Fractional light loss, system',20X,'=',E13.4)
  153 FORMAT (1X,'Projected area, ring number', I5,2X,'=',E14.6)
  154 FORMAT (1X,'Eclipsed area, ring number',  I6,2X,'=',E14.6)
  155 FORMAT (1X,'Area, ring number',        9X,I6,2X,'=',E14.6)                                                             
  156 FORMAT (1X,'Fractional contributions of face annuli to face lumino
     $soty.',/,5X,'annulus number',5X,'current wavelength',5X,'bolometri
     $c')
  157 FORMAT (11X,I2,18X,F5.3,12X,F5.3)
  158 FORMAT (1X,'Rim light',31X,'=',E11.4,'ergs/sec/micron')
  159 FORMAT (1X,'Face light',30X,'=',E11.4,'ergs/sec/micron')
  260 FORMAT (1X,'Rim flux (4*PI) at wavelength',11X,
     $'=',E11.4,'ergs/sec/micron')
  261 FORMAT (1X,'Face flux (4*PI) at wavelength',10X,
     $'=',E11.4,'ergs/sec/micron')
  262 FORMAT (1X,'Flux (4*PI), first component, at wavelength',1X,
     $'=',E12.4,'ergs/sec/micron')
  263 FORMAT (1X,'Flux (4*PI), second component, at wavelength',
     $'=',E12.4,'ergs/sec/micron')
  205 FORMAT (1X,'Program running time=',F12.4,2X,'seconds')
C***********************************************************
      OPEN (UNIT=18,    FILE='ACPGD.ILS',STATUS='OLD')
      READ (18, 100)    (FL1(J),J=1,17)
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
      OPEN (UNIT=NOUT1, FILE=FL1(12), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(13), STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(15), STATUS='REPLACE')
	OPEN (UNIT=IN12,  FILE=FL1(16), STATUS='OLD')
	OPEN (UNIT=IN13,  FILE=FL1(17), STATUS='OLD')
C***********************************************************
      WRITE (IN13, 101) KEYBAD
	CLOSE (UNIT = IN13, STATUS = 'KEEP')
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
C     Read input data
      LABEL = ' *ACPGD'
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NPSI)
      CALL DUMMY (1)
      CALL IREAD (NPST)
      CALL DUMMY (1)
      CALL IREAD (NPSN)
      CALL DUMMY (1)
      CALL IREAD (KTS)
	CALL DUMMY (2)
	CALL IREAD (KDARK)
C     Read value of NTYPE
      LABEL = ' *ACPGA'
      NU=IN12
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL DUMMY (18)
C     Read the type of outer accretion disk boundary.
C     If NTYPE=1, vertical cylinder.
C     If NTYPE=2, half torus.
	CALL IREAD (NTYPE)
C     Read orbital period
      NU=IN10
      CALL DUMMY (7)
      CALL DREAD (PER, 'F15.12')
C
      WRITE (NOUT1,103)
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC
C  
C     THE FOLLOWING ARE INSTALLATION PARAMETERS,BUT CAN BE CHANGED
C     AT INSTALLATION TIME     
C  
      KEC=1      
      KY=1
      KLT=300    
C  
C  
      IF (NPST.EQ.0) NPST=1    
      IF (NPSN.EQ.0) NPSN=NPSI 
      IF (NPSI.NE.0) GO TO 10
C     Read KCTRL and NPSI, the number of input orbital longitudes	  
      READ (IN4,106) KCTRL,NPSI
C     If the control parameter value of NPSN was 0, set to do all NPSI
      IF (NPSN.EQ.0) NPSN=NPSI  
   10 WRITE (NOUT1,105) NPSI,NPST,NPSN
C     INPUT PERMANENT FILE DATA FROM ACPGA
      CALL ACRD1(NRM,NSG,NRING,RAP,RBP,HVP,ACM,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN8)
C     Produce dimensions in cgs units
C     RAP is outer radius of rim, in solar radii
C     RBP is inner radius of accretion disk, in solar radii
C     HVP is semi-thickness of rim, in solar radii
      RA=RAP*ACM
      RB=RBP*ACM
      HV=HVP*ACM
C     Input data from PGA	     
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN2)  
C     RI is orbit semi-major axis, on cgs units
      RI2=RI*RI
      WRITE (NOUT1,117)     
      WRITE (NOUT1,108) (TH(I,KK),KK=1,NTH)      
C      WRITE (NOUT1,151)
C     Input data from PGC
      CALL RDWT3(I,NTH,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),
     $UA(3-I),IN5)
C     Input data from ACPGC
      CALL ACRD2(NWL,NSG,NRM,NRING,IRCT,WLV,
     $TMPRF,TMPRFA,SMV,TMPRR,RMIFLX,IN9)
C
C     Input additional PGB data
      READ (IN11,106) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN11,128) (VLPSI(K),K=1,NPSI)
      READ (IN11,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      READ (IN11,106) (LV(K),K=1,13)
      READ (IN11,128) OMRAD,ECC,SMAJ,TH0,VCONJ,VMN0,PHZERO,THFCP,THSCP,
     $THTCP,THCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,THTCS,
     $THCS,THFOUR
C
C     *******************************                          
C     Loop for successive wavelengths
C     *******************************
C     This is the outermost loop
C
      DO 50 IWL=1,NWL
C     Input PGC data
      CALL RDWT6(I,IWL,NTH,IN6)
C     Set current wavelength
      WLV=WL(IWL)
C     Set orbital phases for max. elongation and conjunctions
C     -------------------------------------------------------
      IF ((DABS(THTHREE).LT.DLTA).AND.(DABS(THFOUR).LT.DLTA)) THEN
         THTS1=PIO2
         THTS2=TPIO2
      ELSE
         THTS1=THTHREE
         THTS2=THFOUR
      END IF
      IF (DABS(PHSEC).LT.DLTA) THEN
         THTS3=PI
         THTS4=0.D0
      ELSE
         THTS3=PHSEC
         THTS4=PHZERO
      END IF
C     ------------------------------------------------------	            
C
C     PRINCIPAL LOOP,FOR SUCCESSIVE VALUES OF ORBITAL LONGITUDE   
C  
      WRITE (NOUT1,118)
      DO 60 KS=1,NPSN
C           
C     Input data from PGB
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN3)
C     Input data from ACPGB
      CALL ACRD3(NSG,NRM,NRING,CSGR,CSGF,PXR,PYR,PZR,PXF,PYF,PZF,XHZ,
     $YHZ,PRFH,PRSH,PRRH,XRMC,YRMC,RBP,STX,STY,STZ,STVS,RXHZL,RYHZL,
     $RXHZG,RYHZG,PRVFG,PRVFL,XUL,XLL,YUL,YLL,RVLD,ANG,RD,AGLD,VXP,VYP,
     $IFC,IRC,IFCS,IRCS,IGAIN,IP,IEC,IFLAG,NTH,IBDY,IN7)
      viv=vi*57.2957795131
   11 CONTINUE 
	DO IPP=1,2
		DO ITH=1,NTH
			NN=NPH(IPP,ITH)
			WRITE (NOUT4,106) IWL,KS,IPP,ITH,NN
			WRITE (NOUT4,162) (STVS(IPP,ITH,KK),KK=1,NN)
		END DO
	END DO
C	IF (I.LT.1000) STOP          
      WRITE (NOUT1,127) PSI,PSIP,KS
      IF (KS.LT.NPST) GO TO 60
C     Reset visibility keys on bottom edge of rim to eliminate 
C     those actually below the observers horizon. These points
C     had all been set as visible in ACPGB, for plotting.
      DO KK=1,NSG
         IF ((CSGR(NRM,KK).LT.0.D0).AND.(PRRH(NRM,KK).GE.0.D0))
     $      PRRH(NRM,KK)=CSGR(NRM,KK)    
      END DO
C  
C     LOOP TO CALCULATE ARRAYS OF SURFACE AREA ELEMENTS AND 
C     RELATED QUANTITIES
C 
C
C     **************
C     Rim
C     **************
C     
C     ARM		Accumulated total rim area, cgs units
C     APROJR	Accumulated projected rim area, cgs units
C     AECR	Eclipsed rim area, cgs units
C     FLTRM	Light from rim in absence of eclipse
C     FLECRM	Rim light lost by eclipse
C     FLNECRM Rim light
C     FLUXRM  Rim flux (4*PI) at the wavelength in question
C     BOLRM	Rim bolometric luminosity
      ARM=0.D0
      APROJR=0.D0
      AECR=0.D0
      BOLRM=0.D0
      FLTRM=0.D0
      FLECRM=0.D0
	FLUXRM=0.D0
      DO J=2,NRM
C	  Test type of rim.
      IF (NTYPE.EQ.1) THEN
C     Case of cylindrical rim
C     Set bounding values of colatitude. Calculate cos() of each.
         TH1=THR(J-1)
         TH2=THR(J)
         CS1=DCOS(TH1)
         CS2=DCOS(TH2)
C     Set increment in height of rim segments, cgs units.
         DH=RA*(CS1/SNTR(J-1)-CS2/SNTR(J))
	   GO TO 2
      END IF
	IF (NTYPE.EQ.2) THEN  
C     Case of toroidal rim
		DH=HV*(VC(J)-VC(J-1))
		GO TO 2
	END IF
C     NTYPE must be 1 or 2.
	CALL PMDSTOP
    2 CONTINUE
		DO K=2,NSG
			IF (NTYPE.EQ.1) THEN
				DLARM=RA*(PHIR(K)-PHIR(K-1))*DH
				ARM=ARM+DLARM
				IF (J.LT.NRM) THEN
C     Assign segment Teff 		               
				TMP=TMPRR(J-1,K)
C     Set cos(gamma) for that segment
				CSGRM=CSGR(J-1,K)
				IF (CSGRM.LT.0.D0) CSGRM=0.D0
				CSGABS=DABS(CSGR(J-1,K))
C     Set limb darkening factor for segment
C     NOTE: This is the Wade and Rucinski expression for intensity.
C     1985,A&A Supp.,60,471.
				IF (KDARK.EQ.1) THEN
				CFA=(1.D0-U1RM(J-1,K)*(1.D0-CSGABS)
     $				-U2RM(J-1,K)*(1.D0-CSGABS)**2)
				END IF
C     NOTE: This is the Dias-Cordova and Geminez expression for intensity.
C     1992, A&A, 269
				IF (KDARK.EQ.2) THEN
				CFA=(1.D0-U1RM(J-1,K)*(1.D0-CSGABS)
     $				-U2RM(J-1,K)*(1.D0-DSQRT(CSGABS)))
				END IF
C     NOTE: This is the Klinglesmith and Sobieski expression for intensity.
C     
				IF (KDARK.EQ.3) THEN
				CFA=(1.D0-U1RM(J-1,K)*(1.D0-CSGABS)
     $				-U2RM(J-1,K)*CSGABS*DLOG(CSGABS))
				END IF
				ELSE
				TMP=TMPRR(J,K)
				CSGRM=CSGR(J,K)
				IF (CSGRM.LT.0.D0) CSGRM=0.D0
				CSGABS=DABS(CSGR(J,K))
				IF (KDARK.EQ.1) THEN
				CFA=(1.D0-U1RM(J,K)*(1.D0-CSGABS)
     $				-U2RM(J,K)*(1.D0-CSGABS)**2)
				END IF
				IF (KDARK.EQ.2) THEN
				CFA=(1.D0-U1RM(J,K)*(1.D0-CSGABS)
     $				-U2RM(J,K)*(1.D0-DSQRT(CSGABS)))
				END IF
				IF (KDARK.EQ.3) THEN
				CFA=(1.D0-U1RM(J,K)*(1.D0-CSGABS)
     $				-U2RM(J,K)*CSGABS*DLOG(CSGABS))
				END IF
				END IF
				AA=DLARM*CSGRM
				APROJR=APROJR+AA
				BB=0.D0
				IF (CSGRM.GE.0.D0) THEN
					IF (PRRH(J-1,K).LT.0.D0) BB=1.D0
				END IF
			END IF
			IF (NTYPE.EQ.2) THEN
C     Set width of bottom edge of rim segment, cgs units.
			DRPH=ACM*(PHIR(K)-PHIR(K-1))
C     Must allow for fact that a (J,K) pair references that vertex from
C     the bounding vertices (J-1,K-1),(J-1,K),(J,K-1),and (J,K).
C     If the lower left vertex is below horizon and lower right vertex
C     is above horizon, use mean of cos(gamma) values to approximate
C     the fraction of projected area to count as above horizon.
C     Similarly for case that lower right vertex is below horizon and
C     lower left vertex is above horizon.
            CRM=0.D0
            IF ((CSGR(J,K-1).LT.0.D0).AND.(CSGR(J,K).GE.0.D0)) THEN 
			TS=0.5D0*(CSGR(J,K-1)+CSGR(J,K))
			IF (TS.GT.0.D0) CRM=TS
		  END IF
            IF ((CSGR(J,K-1).GE.0.D0).AND.(CSGR(J,K).LT.0.D0)) THEN
		    TS=0.5D0*(CSGR(J,K-1)+CSGR(J,K))
			IF (TS.GT.0.D0) CRM=TS
		  END IF
            IF ((CSGR(J,K-1).GE.0.D0).AND.(CSGR(J,K).GE.0.D0)) Then
		    CRM=1.D0
            END IF
C
C
			      
C     Assign segment Teff that is mean of Teff values at four vertices		               
            TMP=(TMPRR(J-1,K-1)+TMPRR(J-1,K)+TMPRR(J,K-1)
     $           +TMPRR(J,K))/4.D0
C     Set mean cos(gamma) for that segment
               CSGRM=(CSGR(J-1,K-1)+CSGR(J-1,K)+CSGR(J,K-1)+
     $             CSGR(J,K))/4.D0
			 CSGABS=DABS(CSGRM)
C     Set mean limb darkening factor for segment
			IF (KDARK.EQ.1) THEN
               CFA=(1.D0-U1RM(J,K)*(1.D0-CSGABS)
     $          -U2RM(J,K)*(1.D0-CSGABS)**2)
			END IF
			IF (KDARK.EQ.2) THEN
               CFA=(1.D0-U1RM(J,K)*(1.D0-CSGABS)
     $          -U2RM(J,K)*(1.D0-DSQRT(CSGABS)))
			END IF
			IF (KDARK.EQ.3) THEN
               CFA=(1.D0-U1RM(J,K)*(1.D0-CSGABS)
     $          -U2RM(J,K)*CSGABS*DLOG(CSGABS))
			END IF
C     Accumulate total rim area
C     Note that this expression is correct for a toroidal boundary.
		  DLARM=DH*DRPH*(RMR(J)+RMR(J-1))/2.D0            
            ARM=ARM+DLARM
C     Accumulate projected rim area
C     AA is value of segment projected area, including approximation
C     at horizon crossover.
            AA=DLARM*DABS(CSGRM)*CRM
            APROJR=APROJR+AA
C     BB is parameter to allow for accumulated eclipse effects
            BB=0.D0
C     Must have CSGRM positive to restrict segments to ones above
C     horizon.
	IF (CSGRM.GT.0.D0) THEN
C     Use decision tree to determine approximation for fraction of
C     segment area to include when edge of eclipse shadow crosses
C     the segment in question.
C     Consider case in which at least one of the corners is
C     not in eclipse. In that case, approximate to fraction of area to
C     include as eclipsed. Note that a negative value of PRRH indicates 
C     that the point in question is eclipsed. 	  
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=1.D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.5D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=0.25D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=0.75D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
C     Distinguish case of actual eclipse from case of crossing horizon.
				IF ((CSGR(J,K-1).GE.0.D0).AND.(CSGR(J,K).GE.0.D0)) THEN
					BB=0.5D0
				END IF
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).LT.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=0.5D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=0.75D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.5D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).LT.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
C     Distinguish case of actual eclipse from horizon crossing
				IF ((CSGR(J,K-1).GE.0.D0).AND.(CSGR(J,K).GE.0.D0)) THEN
					BB=0.5D0
				END IF
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=0.50D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).LT.0.D0).AND.(PRRH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 13
			END IF
		END IF
		IF ((PRRH(J-1,K-1).GE.0.D0).AND.(PRRH(J-1,K).GE.0.D0)) THEN
			IF ((PRRH(J,K-1).GE.0.D0).AND.(PRRH(J,K).LT.0.D0)) THEN
				BB=0.25D0
				GO TO 13
			END IF
		END IF
	END IF
   13 CONTINUE
		END IF
C     Accumulate eclipsed (projected) rim area		     
            AECR=AECR+AA*BB
C     Accumulate light from rim, neglecting eclipse
		IF (J.LT.NRM) THEN
            FLTRM=FLTRM+CFA*AA*BDIR(J-1,K)
C     Accumulate eclipsed light from rim
            FLECRM=FLECRM+CFA*AA*BDIR(J-1,K)*BB
C     Accumulate flux from rim. Note that BDIR() is normal intensity.
		  FLUXRM=FLUXRM+DLARM*BDIR(J-1,K)*PI
		ELSE
            FLTRM=FLTRM+CFA*AA*BDIR(J,K)
            FLECRM=FLECRM+CFA*AA*BDIR(J,K)*BB
		  FLUXRM=FLUXRM+DLARM*BDIR(J,K)*PI
		END IF
C     Accumulate bolometric luminosity
C     Note that the rim temperature is an effective temperature, in
C     contrast to the stellar components, where the temperatures are
C     boundary temperatures.
            BOLRM=BOLRM+DLARM*SGMA*(TMP**4)                    
         END DO
      END DO
C
C     **********************
C     Face of accretion disk
C     **********************
C     
C     AFC accumulates the total face area
      AFC=0.D0
C     APROJF accumulates the total projected face area      
      APROJF=0.D0
C     AECF accumulates the total eclipsed area of the face      
      AECF=0.D0
C     BOLFC accumulates the bolometric luminosity of the face      
      BOLFC=0.D0
      BOLFCA=0.D0
C     FLTFC accumulates the light in the observer's direction at
C     the specified wavelengths of observation.      
      FLTFC=0.D0
C     FLECFC accumulates the light in the observer's direction
C     lost by eclipse.      
      FLECFC=0.D0
C     FLNECFC is the face light at the wavelength in question
C     FLUXFC is the face flux (4*PI) at the wavelength in question
      FLUXFC=0.D0
C
C     ANNULT will contain the light in the observer's direction
C     for the individual face annuli, omitting eclipse effects, divided
C     by the total light from the face, at the current wavelength.
      DO J=1,NRING
		ANNULT(J)=0.D0
	END DO
C     ANNUBL will contain the bolometric light from individual face 
C     annuli, omitting eclipse effects, divided by the total bolometric
C     light from the face.
      DO J=1,NRING
		ANNUBL(J)=0.D0
	END DO
      DO J=2,NRING
C     AA is 1/2 the area of the circular ring, in the orbital plane,
C     between the radii RGRD(J) and RGRD(J-1).
C     ACM is the factor to convert from dimensions in units of the
C     component separation to cgs units.
C     The factor 1/2 allows for the fact that the total angular range
C     in azimuth is 2PI, while the area of a circle is PI*R^2.      
         AA=ACM**2*(RGRD(J)**2-RGRD(J-1)**2)/2.D0
C     AFCRG accumulates the area of one ring
         AFCRG=0.D0         
C     APROJRG accumulates the projected area of one ring
         APROJRG=0.D0         
C     AECFRG accumulates the eclipsed area of one ring
         AECFRG=0.D0         
         DO K=2,NSG
            DLTAREA=AA*(PHIR(K)-PHIR(K-1))
            TMP=(TMPRF(J-1,K-1)+TMPRF(J-1,K)+TMPRF(J,K-1)
     $           +TMPRF(J,K))/4.D0
            TMPA=(TMPRFA(J-1,K-1)+TMPRFA(J-1,K)+TMPRFA(J,K-1)
     $           +TMPRFA(J,K))/4.D0       
            BOLFC=BOLFC+DLTAREA*SGMA*(TMP**4)
            BOLFCA=BOLFCA+DLTAREA*SGMA*(TMPA**4)
C     CSGMN is the mean of the cos(gamma) values at the four corners of
C     the area segment.                   
            CSGMN=(CSGF(J-1,K-1)+CSGF(J-1,K)+CSGF(J,K-1)
     $             +CSGF(J,K))/4.D0
C     Save possible negative value for mean for subsequent use.
	      CSGMNS=CSGMN
            IF (CSGMN.LT.0.D0) CSGMN=0.D0
		IF (KDARK.EQ.1) THEN
            CFA=(1.D0-U1F(J,K)*(1.D0-CSGMN)
     $           -U2F(J,K)*(1.D0-CSGMN)**2)
		END IF
		IF (KDARK.EQ.2) THEN
            CFA=(1.D0-U1F(J,K)*(1.D0-CSGMN)
     $           -U2F(J,K)*(1.D0-DSQRT(CSGMN)))
		END IF
		IF (KDARK.EQ.3) THEN
            CFA=(1.D0-U1F(J,K)*(1.D0-CSGMN)
     $           -U2F(J,K)*CSGMN*DLOG(CSGMN))
		END IF
C     PJAR is the projection of the area segment on the plane of
C     the sky. CSGMN was set to 0.0 if otherwise negative to avoid
C     negative values for projected areas.           
            PJAR=DLTAREA*CSGMN
            AFCRG=AFCRG+DLTAREA           
            APROJRG=APROJRG+PJAR
            BB=0.D0
C     Must have CSGMNS positive to restrict segments to ones above
C     horizon.
	IF (CSGMNS.GT.0.D0) THEN
C     Use decision tree to determine approximation for fraction of
C     segment area to include when edge of eclipse shadow crosses
C     the segment in question.
C     Consider case in which at least one of the corners is
C     not in eclipse. In that case, approximate to fraction of area to
C     include as eclipsed. Note that a negative value of PRRH indicates 
C     that the point in question is eclipsed. 	  
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=1.D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.5D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.75D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.75D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.75D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.5D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).LT.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.5D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.75D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.5D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).LT.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.5D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.5D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).LT.0.D0).AND.(PRFH(J,K).GE.0.D0)) THEN
				BB=0.25D0
				GO TO 12
			END IF
		END IF
		IF ((PRFH(J-1,K-1).GE.0.D0).AND.(PRFH(J-1,K).GE.0.D0)) THEN
			IF ((PRFH(J,K-1).GE.0.D0).AND.(PRFH(J,K).LT.0.D0)) THEN
				BB=0.25D0
				GO TO 12
			END IF
		END IF
	END IF
   12 CONTINUE
            AECFRG=AECFRG+PJAR*BB
            FLTFC=FLTFC+CFA*PJAR*BDIF(J,K)
            FLECFC=FLECFC+CFA*PJAR*BDIF(J,K)*BB
		  FLUXFC=FLUXFC+DLTAREA*BDIF(J,K)*PI
        END DO
	ANNULT(J)=FLTFC
	ANNUBL(J)=BOLFC
      WRITE (NOUT1,155) J,AFCRG  
      WRITE (NOUT1,153) J,APROJRG
      WRITE (NOUT1,154) J,AECFRG
      AFC=AFC+AFCRG
      APROJF=APROJF+APROJRG
      AECF=AECF+AECFRG
      END DO
	DO J=2,NRING
		IF (FLTFC.GT.0.D0) THEN
		ANNULT(J)=ANNULT(J)/FLTFC
		END IF
		ANNUBL(J)=ANNUBL(J)/BOLFC
	END DO
	DO J=2,NRING
		ANNULTA(J)=ANNULT(J)-ANNULT(J-1)
		ANNUBLA(J)=ANNUBL(J)-ANNUBL(J-1)
	END DO      
C     Write output data about individual annuli luminosities
      WRITE (NOUT1,156)
      DO J=2,NRING
		WRITE (NOUT1,157) J, ANNULTA(J), ANNUBLA(J)
	END DO
	WRITE (NOUT1, 102)
C
C    
C     **********************
C     Stellar components
C     **********************
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
          IF ((ITH.GE.5).AND.(ITH.LE.10)) THEN
            ZZZ=0.D0
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
         ARYF(K)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH
	   ARYFA(K)=(Y1A+Y2A+Y3A+Y4A)/DV*DLCS*DLPH
         DVSV=DV
		IF (ITH.LE.2) THEN
         IF (((Y5+Y6).NE.0.D0).AND.(DABS(Y7+Y8).LT.DLTA)) THEN
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
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $					.LT.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $						(STVS(I,ITH-1,K).LT.0.D0)) THEN
						BB=1.D0
						GO TO 33
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $					.LT.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) 
     $				THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $				.GT.1.D-05).AND.(DABS(STVS(I,ITH-1,K-1)-
     $					CSG(I,ITH-1,K-1)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K))
      				BB=BB/(BB+STVS(I,ITH-1,K-1)+STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $				.LT.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).LT.0.D0).AND.
     $					(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $					.GT.1.D-05).AND.(DABS(STVS(I,ITH,K)
     $					-CSG(I,ITH,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K-1)-CSG(I,ITH-1,K-1))
     $					.GT.1.D-05)) THEN
      				BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $					STVS(I,ITH-1,K-1))
      				BB=BB/(BB+STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,ITH,K-1).LT.0.D0).AND.(STVS(I,ITH,K)
     $				.LT.0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $					(STVS(I,ITH-1,K).LT.0.D0)) THEN
					IF ((DABS(STVS(I,ITH,K-1)-CSG(I,ITH,K-1))
     $					.GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,ITH-1,K)-CSG(I,ITH-1,K)).GT.1.D-05))
     $					THEN
      					BB=DABS(STVS(I,ITH,K-1)+STVS(I,ITH,K)+
     $						STVS(I,ITH-1,K))
      					BB=BB/(BB+STVS(I,ITH-1,K-1))
						GO TO 33
					END IF
					END IF
				END IF
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
				IF ((STVS(I,ITH,K-1).GE.0.D0).AND.(STVS(I,ITH,K).LT.
     $				0.D0))THEN
					IF ((STVS(I,ITH-1,K-1).GE.0.D0).AND.
     $				(STVS(I,ITH-1,K).GE.0.D0)) THEN
					IF (DABS(STVS(I,ITH,K)-CSG(I,ITH,K)).GT.1.D-05)	
     $				THEN
						BB=DABS(STVS(I,ITH,K))
						BB=BB/(BB+STVS(I,ITH,K-1)+STVS(I,ITH-1,K-1)
     $						+STVS(I,ITH-1,K))
						GO TO 33
					END IF
					END IF
				END IF
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
         IF ((ITH.GE.5).AND.(ITH.LE.26)) THEN
         ZZZ=0.D0
         END IF
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
 1000 FORMAT (10F8.5)
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
         ZZZ=0.D0
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
         IF (((Y5+Y6).NE.0.D0).AND.(DABS(Y7+Y8).LT.DLTA)) THEN
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
C		WRITE (NOUT3,110) (VL2A(I,ITH,K),K=1,N)
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
C	       Write output for accretion disk 
         WRITE (NOUT1,131) ARM 
         WRITE (NOUT1,132) AFC
         WRITE (NOUT1,133) BOLRM
         WRITE (NOUT1,134) BOLFCA
         WRITE (NOUT1,150) BOLFC
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
         WRITE (NOUT1,111) I,SURF1,ER1A
         WRITE (NOUT1,111) 3-I,SURF2,ER2A
         WRITE (NOUT1,112) I,BOL1,BOLER1
         WRITE (NOUT1,112) 3-I,BOL2,BOLER2
	WRITE (NOUT1,262) FLUX1
	WRITE (NOUT1,263) FLUX2   
	TRM=0.D0
	TFC=0.D0      
      IF (ARM.GT.0.D0) TRM=(BOLRM/ARM/SGMA)**0.25
      
	IF (AFC.GT.0.D0) TFC=(BOLFC/AFC/SGMA)**0.25
         WRITE (NOUT1,135) TRM
         WRITE (NOUT1,136) TFC
      TPRI=(BOL1/SURF1/SGMA)**0.25
      TPSC=(BOL2/SURF2/SGMA)**0.25
      VMG1=CVMG-2.5D0*DLOG10(BOL1/SOLF)
      VMG2=CVMG-2.5D0*DLOG10(BOL2/SOLF)
         WRITE (NOUT1,119) I,TPRI
         WRITE (NOUT1,129) I,VMG1
         WRITE (NOUT1,120) 3-I,TPSC
         WRITE (NOUT1,130) 3-I,VMG2
      SURFSM=SURF1+SURF2+ARM+AFC
C     Allow for both faces of accretion disk
C     Note that this calculation excludes irradiation, to permit
C     comparison with model of energy produced by mass transfer.
      BOLACD=BOLRM+2.D0*BOLFCA      
      BOLSM=BOL1+BOL2+BOLACD
      TAV=(BOLSM/SURFSM/SGMA)**0.25
C     Note that the following abs. mag. is as seen by the observer!
C     Only one face of accret. disk is visible, and includes 
C     irradiation      
      VMGSM=CVMG-2.5D0*DLOG10((BOL1+BOL2+BOLRM+BOLFC)/SOLF)
      VLTSM=SMCO+SMEO+FLTRM+FLTFC
      VLTSB=SMDO+SMFO+FLECRM+FLECFC
	  FLNECRM=FLTRM-FLECRM
	  FLNECFC=FLTFC-FLECFC
		IF (SMCO.NE.0.D0) THEN
			VLSA=SMDO/SMCO
		END IF
	  IF (SMEO.NE.0.D0) THEN
		VLSB=SMFO/SMEO
	  END IF
	  IF (DABS(SMEO).LT.DLTA) VLSB=1.D99
         WRITE (NOUT1,125) TAV
         WRITE (NOUT1,149) BOLACD
         WRITE (NOUT1,147) BOLSM
         WRITE (NOUT1,148) VMGSM
C
      WRITE (NOUT1,145) VIV,KS,PSIP
      WRITE (NOUT1,137) APROJR
      WRITE (NOUT1,138) AECR
      WRITE (NOUT1,139) APROJF
      WRITE (NOUT1,140) AECF
      WRITE (NOUT1,113) SP,I,SM10A,ER10A,SM20A,ER20A,VLSA
      WRITE (NOUT1,116) SP,3-I,SM10B,ER10B,SM20B,ER20B,VLSB
      WRITE (NOUT1,146) WLV
      WRITE (NOUT1,141) FLTRM
      WRITE (NOUT1,142) FLECRM
	  WRITE (NOUT1,158) FLNECRM
	  WRITE (NOUT1,260) FLUXRM
      WRITE (NOUT1,143) FLTFC
      WRITE (NOUT1,144) FLECFC
	  WRITE (NOUT1,159) FLNECFC
C     Allow for two radiating faces
      FLUXFC=2.D0*FLUXFC
	  WRITE (NOUT1,261) FLUXFC
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
			WRITE (NOUT3,106) IWL,KS,IPP,ITH,NN
			WRITE (NOUT3,110) (VL2A(IPP,ITH,KK),KK=1,NN)
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
			WRITE (NOUT3,106) IPP,ITH,NN
			WRITE (NOUT3,110) (VL1(IPP,ITH,KK)*RI2,KK=1,NN)
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
C     Write output
      CALL WTOUT5(I,IWL,VI,PSIP,SP,SMCO,SMCOER,SMDO,SMDOER,SMEO,
     $SMEOER,SMFO,SMFOER,VLTSM,VLTSB,1.D-3*VLTSM,VLT,1.D-3*VLT,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,NOUT2)
   60 CONTINUE
      WRITE (NOUT1,123)
      REWIND IN3
	REWIND IN7
  160 FORMAT(8E10.3)
  161 FORMAT (4E17.8)
  162 FORMAT (10F8.3)
   50 CONTINUE   
   99 CONTINUE   
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
      WRITE (NOUT1, 205) TM
      CLOSE (UNIT=16,    STATUS='KEEP')
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
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
	OPEN (UNIT= IN13, FILE = FL1(17), STATUS = 'OLD')
	WRITE (IN13,101) KEYGOOD
	CLOSE (UNIT=IN13,  STATUS='KEEP')
      STOP 'ACPGD Completed successfully.'
      END PROGRAM ACPGD 
