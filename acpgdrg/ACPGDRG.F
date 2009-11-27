      PROGRAM ACPGDRG
C     Use Microsoft PowerStation Fortran for following statement
C	USE MSFLIB
C     Program to integrate over system components and determine
C     received light
C
C     ******************
C     Version ACPGDRG1
C     ******************
C
C     Important parameters
C     ---------------------
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
C     BOLRM	Rim bolometric luminosity
C
C     Face of accretion disk
C     ----------------------
C
C     AFC		Total face area
C     APROJF	Total projected face area
C     AECF	Total eclipsed face area
C     BOLFC	Bolometric luminosity, face
C     FLTFC	Light flux toward observer in absence of eclipse,
C			at the specified wavelength
C     FLECFC	Light flux toward observer lost by eclipse, at
C			the specified wavelength
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
C     BOL1	Bolometric luminosity, first component
C     SMCO	Light flux toward observer, not counting eclipse,
C			current wavelength, first component
C     SMDO	Light flux toward observer lost by eclipse,
C			current wavelength, first component
C     SM20A	Eclipsed area, first component
C     SURF2	Surface area, second component
C     SM10B	Projected surface area, second component
C     BOL2	Bolometric luminosity, second component
C     SMEO	Light flux toward observer, not counting eclipse,
C			current wavelength, second component
C     SMFO	Light flux toward observer lost by eclipse,
C			current wavelength, second component
C     SM20B	Eclipsed area, second component
C
      INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
	INCLUDE 'COMN.FOR'
      INCLUDE 'COMPH.FOR'
      DIMENSION PXR(MRM,MSG),PYR(MRM,MSG),PZR(MRM,MSG),PXF(MRM,MSG),
     $PYF(MRING,MSG),PZF(MRING,MSG),PRFH(MRING,MSG),PRSH(2,MPHM,2),
     $STX(2,MTLM),
     $STY(2,MTLM),STZ(2,MTLM),STVS(2,MTLM),CSGR(MRM,MSG),
     $CSGF(MRING,MSG),
     $XHZ(MSG),YHZ(MSG),RXHZL(MSG),RYHZL(MSG),RXHZG(MSG),RYHZG(MSG),
     $PRVFG(MSG),PRVFL(MSG),ANG(MSG),RD(MSG),
     $VXP(2,MPHM,2),VYP(2,MPHM,2),VC(MRM)
      DIMENSION PRRH(MRM,MSG),
     $TMPRFA(MRING,MSG),SMV(MRING,MSG),RMIFLX(MRM,MSG)
      DIMENSION UC(2),C1(2),C2(2),T0(2),A(2),BCF(2),UA(2),
     $WL(90),AR3(2,MTHM,MPHM),VITG1(2,MPHM),VITG2(2,MPHM),
     $VLPSI(200),LV(13),ARYF(MPHM)
      DIMENSION TMPRF(MRING,MSG),TMPRR(MRM,MSG),DSGA(2,MTHM,MPHM)
      DIMENSION VL1A(2,MTHM,MPHM),ARYG(MPHM),VL2A(2,MTHM,MPHM),
     $VL3A(2,MTHM,MPHM)
      DIMENSION ARYH(MPHM),ARYI(MPHM),VITG3(2,MPHM),VITG4(2,MPHM)
      DIMENSION VL4A(2,MTHM,MPHM),VL5A(2,MTHM,MPHM),ARYJ(MPHM),
     $ARYK(MPHM),
     $VITG5(2,MPHM),VITG6(2,MPHM)
      DIMENSION VTAUSV(1500),KIDX(MTLM)   
      DIMENSION FL1(16)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 24 FL1
      CHARACTER * 7 LABEL
	INTEGER(4) RESULT
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,5,6,7,8,9/
      DATA IN10,IN11,IN12,IN13/10,11,12,13/
      DATA NOUT1,NOUT2,NOUT3/14,15,16/ 
C     Solar bolometric output in ergs/sec.
      DATA SOLF/3.8268D33/
C     Factor for producing bolometric abs. mag.
      DATA CVMG/4.75D0/                          
  100 FORMAT (A24)
  101 FORMAT (8E10.3)
  102 FORMAT (1H )
  103 FORMAT (1H1)      
  104 FORMAT (1X,'PROGRAM ACPGDRG',30X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2)
  105 FORMAT (1X,'Number of values of orbital longitude=',I5,/,1X, 
     $'Ordinal value of starting orbital longitude=',I11,/,1X,
     $'Ending value of orbital longitude=',I5)     
  106 FORMAT (10I5)     
  108 FORMAT (6F13.8)   
  109 FORMAT (10F8.5)   
  110 FORMAT (6E20.12)
  128 FORMAT (7F18.13)  
  111 FORMAT (1X,'Calculated surface area,component',I10,1X,'=',
     $E14.6,/,43X,'+/-',E14.6,'cm**2')     
  112 FORMAT (1X,'Calculated bolom. luminosity,component',I3,3X,  
     $'=',E12.4,1X,'(4 pi)',/,43X,'+/-',E12.4,/)
  113 FORMAT (1X,'Depth of eclipse',23X,
     $'=',F10.5,/,1X,'Projected area,component',I5,10X,'=',
     $E15.6,1X,     
     $'+/-',E14.6,'cm**2',/,1X,'Eclipsed area',26X,'=',E15.6,1X,'+/-',
     $E14.6,'cm**2',/,   
     $1X,'Value of fractional light loss',9X,'=',F10.5,2X,
     $'(component)')  
  114 FORMAT (2X,'Component'
     $,I5,1X,'First order limb darkening(pole)=',F7.3,/,
     $2X,'Second order limb darkening(pole)',14X,'=',F7.3,/,2X,
     $'Light without eclipse',26X,'=',E13.5,/,48X,
     $'+/-',E12.5,'ergs/sec/micron/sr',/,2X,'Light lost by eclipse',
     $26X,'=',E13.5,/,48X,'+/-',
     $E12.5,'ergs/sec/micron/sr',/,2X,'Value of fractional light loss'
     $,17X,'=',F9.5,2X,'(component)',/)
  115 FORMAT (2X,'Component',I5,1X,'First order limb darkening(pole)=', 
     $F7.3,/,2X,'Second order limb darkening(pole)',14X,'=',F7.3,/,
     $2X,'Light without eclipse',26X,'=',E13.5,/,48X,'+/-',E12.5,
     $'ergs/sec/micron/sr',/,2X,
     $'Light lost by eclipse',26X,'=',E13.5,/,48X,'+/-',E12.5,
     $'ergs/sec/micron/sr',/,2X,'Value of fractional light loss',17X,
     $'=',F9.5,2X,'(component)',/)
  116 FORMAT (1X,'Depth of eclipse',23X,'=',F10.5,/,
     $1X,'Projected area,component',I5,10X,'=',E15.6,1X,'+/-',E14.6,
     $'cm**2',/,1X,'Eclipsed area',
     $26X,'=',E15.6,1X,'+/-',E14.6,'cm**2',
     $/,1X,'Value of fractional light loss',9X,'=',F10.5,2X,
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
  124 FORMAT (1X,'Light of component',I2,1X,'at wavelength',F12.4,'=',
     $E15.7,1X,'+/-',E15.7,'ergs/sec/micron/sr')
  125 FORMAT (1X,'System T(eff)',31X,'=',F9.1,2X,'K')
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
     $'ergs/sec/micron/sr')
  142 FORMAT (1X,'Rim light lost by eclipse',15X,'=',E11.4,
     $'ergs/sec/micron/sr')
  143 FORMAT (1X,'Face light without eclipse',14X,'=',E11.4,
     $'ergs/sec/micron/sr')
  144 FORMAT (1X,'Face light lost by eclipse',14X,'=',E11.4,
     $'ergs/sec/micron/sr')
  145 FORMAT (1X,'Orbital inclination',20X,'=',F12.8,/,1X,
     $'Ordinal value of orbital longitude',5X,'=',I3,/,1X,
     $'Orbital longitude',22X,'=',F12.8)
  146 FORMAT (1X,'Wavelength(microns)',20X,'=',F12.5)
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
     $E12.4,1X,'(4 pi)',/,10X,
     $'excluding irradiation if KEY or IRCT equal 0')
  151 FORMAT (1X,'In the following listing, component 1 always refers to
     $ the component eclipsed at primary minimum')
  152 FORMAT (1X,'Fractional light loss, system',20X,'=',E13.3)
  153 FORMAT (1X,'Projected area, ring number', I5,2X,'=',E14.6)
  154 FORMAT (1X,'Eclipsed area, ring number',  I6,2X,'=',E14.6)
  155 FORMAT (1X,'Area, ring number',        9X,I6,2X,'=',E14.6)                                                             
  156 FORMAT (5D15.8)
  157 FORMAT (1X,'Total monochromatic scattered light',13X,'=',
     $E13.5,'ergs/sec/micron/sr')
  158 FORMAT (1X,'Scattered light uneclipsed by acc. disk',9X,'=',
     $E13.5,'ergs/sec/micron/sr')
  159 FORMAT (1X,'Scattered light eclipsed by companion',11X,'=',
     $E13.5,'ergs/sec/micron/sr')
  161 FORMAT (1X,'Monochrom. scattered light received by observer',
     $1X,'=',E13.5,'ergs/sec/micron/sr',/)
  162 FORMAT (1X,'Bremsstrahlung reaching observer',16X,'=',
     $E11.3,'  ergs/sec/micron/sr')
  163 FORMAT (1X,'Bremsstrahlung eclipsed by companion',12X,'=',
     $E11.3,'  ergs/sec/micron/sr')
  194 FORMAT (3D15.5)
  205 FORMAT (1X,'Program running time=',F12.4,2X,'minutes')
C***********************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=17,    FILE='E:\LGTSYN\ACDISK\ACPGDRG.ILS',
     $STATUS='OLD')
      READ (17, 100)    (FL1(J),J=1,16)
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
C***********************************************************
C      CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F1)
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
C     Read KEY5 and KEY5A, from ACPGCRG
      READ (IN13, 106) KEY5, KEY5A
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
C
C     Set orbital phases for max. elongation and conjunctions
C     -------------------------------------------------------
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
C     ------------------------------------------------------	            
C
C     PRINCIPAL LOOP,FOR SUCCESSIVE VALUES OF ORBITAL LONGITUDE   
C  
      WRITE (NOUT1,118)
C     Set parameter for control of repetitive output listing
C     Subsequent use limits output to first three values of KS
      KKS=0
      DO 60 KS=1,NPSN
C           
C     Input data from PGB
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN3)
      MTH=2*NTH-1
C     Input data from ACPGB
      CALL ACRD3(NSG,NRM,NRING,CSGR,CSGF,PXR,PYR,PZR,PXF,PYF,PZF,XHZ,
     $YHZ,PRFH,PRSH,PRRH,XRMC,YRMC,RBP,STX,STY,STZ,STVS,RXHZL,RYHZL,
     $RXHZG,RYHZG,PRVFG,PRVFL,XUL,XLL,YUL,YLL,RVL,ANG,RD,AGL,VXP,VYP,
     $IFC,IRC,IFCS,IRCS,IGAIN,IP,IEC,IFLAG,MTH,IN7)
      viv=vi*57.2957795131
      KKS=KKS+1
   11 CONTINUE 
 1000 FORMAT (1X,'ACPGD')
 1001 FORMAT (1X,10F8.3)
 1002 FORMAT (' ')
 1003 FORMAT (5I5)
      WRITE (NOUT1,127) PSI,PSIP,KS
C
C     *****************************************************************
C     Input data from ACPGCRG
C     Read bremsstrahlung
      IF (KEY5.EQ.1) THEN
		READ (IN13,194) WLAV, BRFLX	, TBRFLX
C     Bremsstrahlung lost by eclipse
		EBRFLX=TBRFLX-BRFLX
	END IF
C     JWL is the current ordinal wavelength number.
      READ (IN12,106) JWL
	IF (JWL.NE.IWL) CALL PMDSTOP
C     SMARG is the monochromatic scattered flux toward the observer.
C		This includes radiation that may be blocked by the accretion 
C		disk itself.
C     SMBRG is the scattered flux toward the observer not blocked by
C		the accretion disk. It includes flux that the companion may
C		block.
C     SMCRG is the scattered flux that the companion blocks.
C     SMDRG is the monochromatic scattered flux received by the 
C		observer.
	READ (IN12,156) SMARG,SMBRG,SMCRG,SMDRG
C     Input the data on extinction by the accretion disk during eclipse
C     of the star without the accretion disk.
      READ (IN12,106) KLG
	READ (IN12,101) (VTAUSV(KK),KK=1,KLG)
	READ (IN12,106) (KIDX(KK),KK=1,KLG)
C*************************************************************************
C
      IF (KS.LT.NPST) GO TO 60
C      WRITE (NOUT1,1002)
C      WRITE (NOUT1,1000)
C      DO ITH=1,MTH
C      CALL IDW(I,ITH,KST)
C      N=MPH(I,ITH)
C      KND=KST+N-1
C      WRITE (NOUT1,1003) KS,I,ITH      
C      WRITE (NOUT1,1001) (STVS(I,JJ),JJ=KST,KND)
C      END DO
C      DO ITH=1,MTH
C      CALL IDW(3-I,ITH,KST)
C      N=MPH(3-I,ITH)
C      KND=KST+N-1
C      WRITE (NOUT1,1003) KS,3-I,ITH
C      WRITE (NOUT1,1001) (STVS(3-I,JJ),JJ=KST,KND)
C      END DO
C     Reset visibility keys on bottom edge of rim to eliminate 
C     those actually below the observers horizon. These points
C     had all been set as visible in ACPGB, for plotting.
      DO KK=1,NSG
         IF ((CSGR(NRM,KK).LT.0.D0).AND.(PRRH(NRM,KK).GE.0.D0))
     $      PRRH(NRM,KK)=CSGR(NRM,KK)    
      END DO            
C      DO JJ=1,NRM
C      WRITE (NOUT1,1003) JJ
C      WRITE (NOUT1,1001) (PRRH(JJ,KK),KK=1,NSG)
C	WRITE (NOUT1,1002)
C	WRITE (NOUT1,1001) (CSGR(JJ,KK),KK=1,NSG)
C	WRITE (NOUT1,1002)
C      END DO
C      WRITE (NOUT1,1002)    
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
C     BOLRM	Rim bolometric luminosity
      ARM=0.D0
      APROJR=0.D0
      AECR=0.D0
      BOLRM=0.D0
      FLTRM=0.D0
      FLECRM=0.D0
      DO J=2,NRM
C     Test type of rim.
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
C     Set cos(gamma) for that segment.
					CSGRM=CSGR(J-1,K)
					IF (CSGRM.LT.0.D0) CSGRM=0.D0
C     Set limb darkening factor for segment.
					CFA=(1.D0-U1RM(J-1,K)-U2RM(J-1,K)+U1RM(J-1,K)
     $					*DABS(CSGR(J-1,K))
     $					+U2RM(J-1,K)*CSGR(J-1,K)**2)
				ELSE
					TMP=TMPRR(J,K)
					CSGRM=CSGR(J,K)
					IF (CSGRM.LT.0.D0) CSGRM=0.D0
					CFA=(1.D0-U1RM(J,K)-U2RM(J,K)+U1RM(J,K)
     $					*DABS(CSGR(J,K))
     $					+U2RM(J,K)*CSGR(J,K)**2)
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
            IF ((CSGR(J,K-1).GE.0.D0).AND.(CSGR(J,K).GE.0.D0)) THEN
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
C     Set mean limb darkening factor for segment
               CFA=(1.D0-U1RM(J,K)-U2RM(J,K)+U1RM(J,K)*DABS(CSGR(J,K))
     $          +U2RM(J,K)*CSGR(J,K)**2)
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
C     Accumulate eclipsed rim area		     
            AECR=AECR+AA*BB
C     Accumulate light from rim, neglecting eclipse
		IF (J.LT.NRM) THEN
			FLTRM=FLTRM+CFA*AA*BDIR(J-1,K)
C     Accumulate eclipsed light from rim
			FLECRM=FLECRM+CFA*AA*BDIR(J-1,K)*BB
		ELSE
			FLTRM=FLTRM+CFA*AA*BDIR(J,K)
			FLECRM=FLECRM+CFA*AA*BDIR(J,K)*BB
		END IF
C     Accumulate bolometric luminosity
C     Note that the rim temperature is an effective temperature, in
C     contrast to the stellar components, where the temperatures are
C     boundary temperatures.
            BOLRM=BOLRM+DLARM*SGMA*(TMP**4)                    
         END DO
C         WRITE (NOUT1,222) APROJR           
      END DO
C      WRITE (NOUT1,103) 
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
C     FLTFC accumulates the light flux in the observer's direction at
C     the specified wavelengths of observation.      
      FLTFC=0.D0
C     FLECFC accumulates the light flux in the observer's direction
C     lost by eclipse.      
      FLECFC=0.D0
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
            CFA=(1.D0-U1F(J,K)-U2F(J,K)+U1F(J,K)*DABS(CSGF(J,K))
     $           +U2F(J,K)*CSGF(J,K)**2)
C     PJAR is the projection of the area segment on the plane of
C     the sky. CSGMN was set to 0.0 if otherwise negative to avoid
C     negative values for projected areas.           
            PJAR=DLTAREA*CSGMN
C            IF (J.EQ.20) WRITE (NOUT1,222) DLTAREA,CSGMN,AA,
C     $      PHIR(K),PHIR(K-1),PJAR
  222 FORMAT (1X,6E13.5)
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
        END DO
      WRITE (NOUT1,155) J,AFCRG  
      WRITE (NOUT1,153) J,APROJRG
      WRITE (NOUT1,154) J,AECFRG
      AFC=AFC+AFCRG
      APROJF=APROJF+APROJRG
      AECF=AECF+AECFRG
      END DO
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
      MTH=2*NTH-1
      CF=RI*RI
      VTHST=0.D0
C     -----------------
      DO 22 ITH=1,MTH
      VITG1(I,ITH)=0.D0
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
      CALL SYP(I,ITH,NTH,VTH)
      CSTH1=DCOS(VTHA)
      CSTH2=DCOS(VTH)
      DLCS=CSTH1-CSTH2         
      CALL IDW(I,ITH,KST)      
      N=MPH(I,ITH)      
      KND=KST+N-1
C	IF ((ITH.GT.25).AND.(ITH.LT.28)) THEN
C	WRITE (NOUT1,1003) I,ITH,KST,KND
C	WRITE (NOUT1,109) (CSG(I,KKK),KKK=KST,KND)
C	WRITE (NOUT1,1002)
C	WRITE (NOUT1,109) (STVS(I,KKK),KKK=KST,KND)
C	WRITE (NOUT1,1002)
C	END IF
      IF (N.LE.0) CALL PMDSTOP
      IF (ITH.EQ.1) GO TO 22
      IF (ITH.LT.MTH) THEN
C
         DO 20 K=KST,KND   
C     DSGA REPRESENTS AN ORDINATE OF FN. TO BE INTEGRATED FOR SURFACE
C     AREA
C     AR3() contains the longitudes on the star of the grid points 
         CALL SYO(I,ITH,K,NTH,KK,JTH,ISG,VPHV) 
         AR3(I,ITH,K-KST+1)=VPHV
         DSGA(I,ITH,K-KST+1)=VR(I,KK)*VR(I,KK)/CSB(I,KK)
         VL1A(I,ITH,K-KST+1)=DABS(CSG(I,K))*DSGA(I,ITH,K-KST+1)
C     AA is a variable to include segments that are visible.
C     BB is a variable that ultimately will include areas that
C     are eclipsed.
         AA=1.0
         BB=0.D0
C     If the point is below the horizon, set AA=0.
         IF (CSG(I,K).LT.0.D0) AA=0.D0
         IF (CSG(I,K).GE.0.D0) BB=1.D0
C     Initially, retain those areas that are above horizon.
         VL5A(I,ITH,K-KST+1)=BB*VL1A(I,ITH,K-KST+1)         
         VL1A(I,ITH,K-KST+1)=AA*VL1A(I,ITH,K-KST+1)
        CFA=(1.D0-U1(I,KK)-U2(I,KK)+U1(I,KK)*DABS(CSG(I,K))
     $   +U2(I,KK)*CSG(I,K)**2)*CF
		DIV=1.D0
		IF (I.NE.ISTAR) THEN
			IF (VTAUSV(KIDX(KK)).LT.400.D0) THEN
				DIV=DEXP(VTAUSV(KIDX(KK)))
			ELSE
				DIV=5.D173
			END IF
		END IF
         VL2A(I,ITH,K-KST+1)=BDI(I,KK)*CFA*AA*VL1A(I,ITH,K-KST+1)
	   DLGHT=VL2A(I,ITH,K-KST+1)*(1.D0-1.D0/DIV)
         VL4A(I,ITH,K-KST+1)=BDI(I,KK)*CFA*BB*VL1A(I,ITH,K-KST+1)+DLGHT
         VL3A(I,ITH,K-KST+1)=SGMA*(1.23244D3*TMPR(I,KK))**4
     $    *DSGA(I,ITH,K-KST+1)*CF    
         IF (K.EQ.KST) GO TO 20   
C     Accumulate integrals for area and related calculations
         Y1=DSGA(I,ITH,K-KST)
         Y2=DSGA(I,ITH,K-KST+1)
         Y5=VL1A(I,ITH,K-KST)
         Y6=VL1A(I,ITH,K-KST+1)
         Y9=VL3A(I,ITH,K-KST)
         Y10=VL3A(I,ITH,K-KST+1)
         Y13=VL2A(I,ITH,K-KST)
         Y14=VL2A(I,ITH,K-KST+1)
         Y17=VL5A(I,ITH,K-KST)
         Y18=VL5A(I,ITH,K-KST+1)
         Y21=VL4A(I,ITH,K-KST)
         Y22=VL4A(I,ITH,K-KST+1)
C     Guarantee that corresponding longitudes on circles ITH and ITH-1
C     are the same. This means that each colatitued circle has the same 
C     number of grid points.
         IF (ITH.GT.2) THEN
            IF (DABS(AR3(I,ITH,K-KST+1)-AR3(I,ITH-1,K-KST+1)).GT.1.D-9)
     $       CALL PMDSTOP 
            Y3=DSGA(I,ITH-1,K-KST)
            Y4=DSGA(I,ITH-1,K-KST+1)
            Y7=VL1A(I,ITH-1,K-KST)
            Y8=VL1A(I,ITH-1,K-KST+1)
            Y11=VL3A(I,ITH-1,K-KST)
            Y12=VL3A(I,ITH-1,K-KST+1)
            Y15=VL2A(I,ITH-1,K-KST)
            Y16=VL2A(I,ITH-1,K-KST+1)
            Y19=VL5A(I,ITH-1,K-KST)
            Y20=VL5A(I,ITH-1,K-KST+1)
            Y23=VL4A(I,ITH-1,K-KST)
            Y24=VL4A(I,ITH-1,K-KST+1)
            DV=4.D0
C     Case ITH.LE.2
         ELSE
            Y3=0.D0
            Y4=0.D0
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
         DLPH=AR3(I,ITH,K-KST+1)-AR3(I,ITH,K-KST)      
         ARYF(K-KST+1)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH
         DVSV=DV
         IF (((Y5+Y6).NE.0.D0).AND.((Y7+Y8).EQ.0.D0)) THEN
			DV=2.D0
	   ELSE		  
			DV=DVSV
	   END IF
         ARYG(K-KST+1)=(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH
         ARYH(K-KST+1)=(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH
         ARYI(K-KST+1)=(Y13+Y14+Y15+Y16)/DV*DLCS*DLPH
C     Do approximation to fraction of segment eclipsed when shadow
C     crosses the segment
	   BB=0.D0
         IF ((K.GT.KST).AND.(ITH.GE.2)) THEN
C     Note:STVS and CSG are stored in a one-dimensional sequence.
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,K) references the lower 
C     right corner of the area segment.
			CSGM=(CSG(I,K)+CSG(I,K-1)+CSG(I,K-N-1)+CSG(I,K-N))/4.D0
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
			IF (CSGM.GT.0.D0) THEN
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
						BB=1.D0
						GO TO 33
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.
     $				(STVS(I,K-N).GE.0.D0)) 
     $				THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K))
      				BB=BB/(BB+STVS(I,K-N-1)+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K)+STVS(I,K-N-1))
      				BB=BB/(BB+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN
      					BB=DABS(STVS(I,K-1)+STVS(I,K)+STVS(I,K-N))
      					BB=BB/(BB+STVS(I,K-N-1))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K-N-1)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05)THEN
     					BB=DABS(STVS(I,K-1))
					BB=BB/(BB+STVS(I,K)+STVS(I,K-N-1)+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K-N-1))
					BB=BB/(BB+STVS(I,K)+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K)+STVS(I,K-N-1))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K)+STVS(I,K-N-1)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K-1))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05)	THEN
						BB=DABS(STVS(I,K))
						BB=BB/(BB+STVS(I,K-1)+STVS(I,K-N-1)
     $						+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05))THEN
      					BB=DABS(STVS(I,K)+STVS(I,K-N-1))
						BB=BB/(BB+STVS(I,K-1)+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN
      					BB=DABS(STVS(I,K)+STVS(I,K-N))
						BB=BB/(BB+STVS(I,K-1)+STVS(I,K-N-1))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)
     $				.AND.(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)) THEN
      					BB=DABS(STVS(I,K-N-1)+STVS(I,K-N))
						BB=BB/(BB+STVS(I,K-1)+STVS(I,K))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
						BB=0.D0
						GO TO 33
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)
     $				THEN
      					BB=DABS(STVS(I,K-N-1))
						BB=BB/(BB+STVS(I,K-1)+STVS(I,K)+STVS(I,K-N))
						GO TO 33
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)THEN
      					BB=DABS(STVS(I,K-N))
						BB=BB/(BB+STVS(I,K-1)+STVS(I,K)+STVS(I,K-N-1))
						GO TO 33
					END IF
					END IF
				END IF
			END IF
		END IF
   33    CONTINUE
C         IF (ITH.EQ.35) THEN
C		WRITE (NOUT1,1003)K
C		WRITE (NOUT1,1001) CSGM,CSG(I,K-N-1),CSG(I,K-N),CSG(I,K-1),
C     $		CSG(I,K)
C          WRITE (NOUT1,1001) BB,STVS(I,K-N-1),STVS(I,K-N),STVS(I,K-1),
C     $        STVS(I,K)
C	    WRITE (NOUT1,1001) Y17,Y18,Y19,Y20
C         END IF		 		 
         ARYJ(K-KST+1)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
C		IF (ITH.EQ.35) WRITE (NOUT1,1001) ARYJ(K-KST+1)
         ARYK(K-KST+1)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K-KST+1)
         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K-KST+1)
         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K-KST+1)
         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K-KST+1)
         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K-KST+1)
C		IF (ITH.EQ.35) WRITE (NOUT1,1001) VITG5(I,ITH)
         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K-KST+1)
   20    CONTINUE
C     ITH=MTH in following section.
      ELSE
         CALL IDW(I,ITH-1,KST)
         N=MPH(I,ITH-1)
         KND=KST+N-1
         DO K=KST+1,KND
            Y1=DSGA(I,ITH-1,K-KST)
            Y2=DSGA(I,ITH-1,K-KST+1)
            Y5=VL1A(I,ITH-1,K-KST)
            Y6=VL1A(I,ITH-1,K-KST+1)
            Y9=VL3A(I,ITH-1,K-KST)
            Y10=VL3A(I,ITH-1,K-KST+1)
            Y13=VL2A(I,ITH-1,K-KST)
            Y14=VL2A(I,ITH-1,K-KST+1)
		  CSGM=(CSG(I,K)+CSG(I,K-1))/2.D0
		  BB=0.D0
		  IF (CSGM.GT.0.D0) THEN
			IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0)) BB=1.D0
		  END IF 
            Y17=VL5A(I,ITH-1,K-KST)*BB
            Y18=VL5A(I,ITH-1,K-KST+1)*BB
            Y21=VL4A(I,ITH-1,K-KST)*BB
            Y22=VL4A(I,ITH-1,K-KST+1)*BB
            DLPH=AR3(I,ITH-1,K-KST+1)-AR3(I,ITH-1,K-KST)
            DV=2.D0
            ARYF(K-KST+1)=(Y1+Y2)/DV*DLCS*DLPH
            ARYG(K-KST+1)=(Y5+Y6)/DV*DLCS*DLPH
            ARYH(K-KST+1)=(Y9+Y10)/DV*DLCS*DLPH
            ARYI(K-KST+1)=(Y13+Y14)/DV*DLCS*DLPH
            ARYJ(K-KST+1)=(Y17+Y18)/DV*DLCS*DLPH
            ARYK(K-KST+1)=(Y21+Y22)/DV*DLCS*DLPH
            VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K-KST+1)
            VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K-KST+1)
            VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K-KST+1)
            VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K-KST+1)
            VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K-KST+1)
            VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K-KST+1)
         END DO
      END IF         
   22 CONTINUE
C     ----------------------------
C      WRITE (NOUT1,102)
C      WRITE (NOUT1,1000)    
      DO ITH=1,MTH
         SURF1=SURF1+VITG1(I,ITH)
         SM10A=SM10A+VITG2(I,ITH)
         BOL1=BOL1+VITG3(I,ITH)
         SMCO=SMCO+VITG4(I,ITH)
         SMDO=SMDO+VITG6(I,ITH)
         SM20A=SM20A+VITG5(I,ITH)
C         WRITE (NOUT1,106) I,ITH
C         WRITE (NOUT1,110) SM20A,VITG5(I,ITH)       
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
      DO 42 ITH=1,MTH
C         WRITE (NOUT1,106) I,ITH
      VITG1(I,ITH)=0.D0
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
      CALL SYP(I,ITH,NTH,VTH)
      CSTH1=DCOS(VTHA)
      CSTH2=DCOS(VTH)
      DLCS=CSTH1-CSTH2      
      CALL IDW(I,ITH,KST)      
      N=MPH(I,ITH)      
      KND=KST+N-1
      IF (N.LE.0) CALL PMDSTOP 
      IF (ITH.EQ.1) GO TO 42
      IF (ITH.LT.MTH) THEN
C
         DO 40 K=KST,KND
C      WRITE (NOUT1,1001) (STVS(I,KKK),KKK=KST,KND)
C	WRITE (NOUT1,1002)
C     DSGA represents an ordinate of fn. to be integrated for surface
C     area.
C     AR3() contains the longitudes on the star of the grid points.	      
         CALL SYO(I,ITH,K,NTH,KK,JTH,ISG,VPHV)
         AR3(I,ITH,K-KST+1)=VPHV
         DSGA(I,ITH,K-KST+1)=VR(I,KK)*VR(I,KK)/CSB(I,KK)
         VL1A(I,ITH,K-KST+1)=DABS(CSG(I,K))*DSGA(I,ITH,K-KST+1)
C     AA is a variable to include segments that are visible.
C     BB is a variable that ultimately will include areas that
C     are eclipsed.
         AA=1.0D0
         BB=0.D0
C     If the point is below the horizon, set AA=0.
         IF (CSG(I,K).LT.0.D0) AA=0.D0
         IF (CSG(I,K).GE.0.D0) BB=1.D0
C     Initially, retain those areas that are above horizon.
         VL5A(I,ITH,K-KST+1)=BB*VL1A(I,ITH,K-KST+1)          
         VL1A(I,ITH,K-KST+1)=AA*VL1A(I,ITH,K-KST+1)
         CFA=(1.D0-U1(I,KK)-U2(I,KK)+U1(I,KK)*DABS(CSG(I,K))
     $   +U2(I,KK)*CSG(I,K)**2)*CF
		DIV=1.D0
		IF (I.NE.ISTAR) THEN
			IF (VTAUSV(KIDX(KK)).LT.400.D0) THEN
				DIV=DEXP(VTAUSV(KIDX(KK)))
			ELSE
				DIV=5.D173
			END IF
		END IF
         VL2A(I,ITH,K-KST+1)=BDI(I,KK)*CFA*AA*VL1A(I,ITH,K-KST+1)/DIV
         VL4A(I,ITH,K-KST+1)=BDI(I,KK)*CFA*BB*VL1A(I,ITH,K-KST+1)/DIV
         VL3A(I,ITH,K-KST+1)=SGMA*(1.23244D3*TMPR(I,KK))**4
     $    *DSGA(I,ITH,K-KST+1)*CF    
         IF (K.EQ.KST) GO TO 40
C     Accumulate integrals for area and related calculations.
         Y1=DSGA(I,ITH,K-KST)
         Y2=DSGA(I,ITH,K-KST+1)
         Y5=VL1A(I,ITH,K-KST)
         Y6=VL1A(I,ITH,K-KST+1)
         Y9=VL3A(I,ITH,K-KST)
         Y10=VL3A(I,ITH,K-KST+1)
         Y13=VL2A(I,ITH,K-KST)
         Y14=VL2A(I,ITH,K-KST+1)
         Y17=VL5A(I,ITH,K-KST)
         Y18=VL5A(I,ITH,K-KST+1)
         Y21=VL4A(I,ITH,K-KST)
         Y22=VL4A(I,ITH,K-KST+1)
C     Guarantee that corresponding longitudes on circles ITH and ITH-1
C     are the same. This means that each colatitued circle has the same 
C     number of grid points.
         IF (I.GT.2) THEN
            IF (DABS(AR3(I,ITH,K-KST+1)-AR3(I,ITH-1,K-KST+1)).GT.1.D-9)
     $         CALL PMDSTOP
            Y3=DSGA(I,ITH-1,K-KST)
            Y4=DSGA(I,ITH-1,K-KST+1)
            Y7=VL1A(I,ITH-1,K-KST)
            Y8=VL1A(I,ITH-1,K-KST+1)
            Y11=VL3A(I,ITH-1,K-KST)
            Y12=VL3A(I,ITH-1,K-KST+1)
            Y15=VL2A(I,ITH-1,K-KST)
            Y16=VL2A(I,ITH-1,K-KST+1)
            Y19=VL5A(I,ITH-1,K-KST)
            Y20=VL5A(I,ITH-1,K-KST+1)
            Y23=VL4A(I,ITH-1,K-KST)
            Y24=VL4A(I,ITH-1,K-KST+1)
            DV=4.D0
C     Case ITH.LE.2
         ELSE
            Y3=0.D0
            Y4=0.D0
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
         DLPH=AR3(I,ITH,K-KST+1)-AR3(I,ITH,K-KST)
         ARYF(K-KST+1)=(Y1+Y2+Y3+Y4)/DV*DLCS*DLPH
	   DVSV=DV
         IF (((Y5+Y6).NE.0.D0).AND.((Y7+Y8).EQ.0.D0)) THEN
			DV=2.D0
	   ELSE
			DV=DVSV
	   END IF
         ARYG(K-KST+1)=(Y5+Y6+Y7+Y8)/DV*DLCS*DLPH
         ARYH(K-KST+1)=(Y9+Y10+Y11+Y12)/DV*DLCS*DLPH
         ARYI(K-KST+1)=(Y13+Y14+Y15+Y16)/DV*DLCS*DLPH
C     Do approximation to fraction of segment eclipsed when shadow
C     crosses the segment
	   BB=0.D0
         IF ((K.GT.KST).AND.(ITH.GE.2)) THEN
C     Note:STVS and CSG are stored in a one-dimensional sequence.
C     There are the same number of points,N,on each colatitude circle.
C     Then the point on colatitude circle ITH-1, at azimuth K, is at 
C     index K-N. For index K-1 on circle ITH, the corresponding point
C     on circle ITH-1 is at index K-N-1.
C     CSGM then is the average cos(gamma) for the four corners of the
C     current area segment. Note that index (I,K) references the lower 
C     right corner of the area segment.
			CSGM=(CSG(I,K)+CSG(I,K-1)+CSG(I,K-N-1)+CSG(I,K-N))/4.D0
C     Must have CSGM positive to restrict segments to ones above
C     horizon.
			IF (CSGM.GT.0.D0) THEN
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
						BB=1.D0
						GO TO 32
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.0.D0)) 
     $				THEN
					IF ((DABS(CSG(I,K-1)-STVS(I,K-1)).GT.1.D-05).AND.
     $				(DABS(CSG(I,K-N-1)-STVS(I,K-N-1)).GT.1.D-05))THEN 
      				BB=DABS(STVS(I,K-1)+STVS(I,K))
					BB=BB/(BB+STVS(I,K-N-1)+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-5).AND.
     $				(DABS(STVS(I,K)-CSG(I,K)).GT.1.D-5).AND.
     $		        (DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)) THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K)+STVS(I,K-N-1))
					BB=BB/(BB+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05)
     $				.AND.(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K-N-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN
      				BB=DABS(STVS(I,K-1)+STVS(I,K-N-1)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-5)) THEN
					BB=DABS(STVS(I,K-1))
					BB=BB/(BB+STVS(I,K)+STVS(I,K-N-1)+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05))THEN 
      				BB=DABS(STVS(I,K-1)+STVS(I,K-N-1))
					BB=BB/(BB+STVS(I,K)+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-1)-CSG(I,K-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)) THEN
					BB=DABS(STVS(I,K-1)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K)+STVS(I,K-N-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN
      					BB=DABS(STVS(I,K)+STVS(I,K-N-1)+STVS(I,K-N))
						BB=BB/(BB+STVS(I,K-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05)THEN 
					BB=DABS(STVS(I,K))
					BB=BB/(BB+STVS(I,K-1)+STVS(I,K-N-1)+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05))THEN 
      				BB=DABS(STVS(I,K)+STVS(I,K-N-1))
					BB=BB/(BB+STVS(I,K-1)+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).LT.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K)-CSG(I,K)).GT.1.D-05).AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN 
					BB=DABS(STVS(I,K)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K-1)+STVS(I,K-N-1))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF ((DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)
     $				.AND.
     $				(DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05))THEN 
					BB=DABS(STVS(I,K-N-1)+STVS(I,K-N))
					BB=BB/(BB+STVS(I,K-1)+STVS(I,K))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
						BB=0.D0
						GO TO 32
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).LT.0.D0).AND.(STVS(I,K-N).GE.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K-N-1)-CSG(I,K-N-1)).GT.1.D-05)
     $				THEN
      				BB=DABS(STVS(I,K-N-1))
					BB=BB/(BB+STVS(I,K-1)+STVS(I,K)+STVS(I,K-N))
						GO TO 32
					END IF
					END IF
				END IF
				IF ((STVS(I,K-1).GE.0.D0).AND.(STVS(I,K).GE.0.D0))THEN
					IF ((STVS(I,K-N-1).GE.0.D0).AND.(STVS(I,K-N).LT.
     $				0.D0)) THEN
					IF (DABS(STVS(I,K-N)-CSG(I,K-N)).GT.1.D-05)THEN 
      				BB=DABS(STVS(I,K-N))
					BB=BB/(BB+STVS(I,K-1)+STVS(I,K)+STVS(I,K-N-1))
						GO TO 32
					END IF
					END IF
				END IF
			END IF
		END IF
   32    CONTINUE		 	
         ARYJ(K-KST+1)=(Y17+Y18+Y19+Y20)/DV*DLCS*DLPH*BB
         ARYK(K-KST+1)=(Y21+Y22+Y23+Y24)/DV*DLCS*DLPH*BB
         VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K-KST+1)
         VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K-KST+1)
         VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K-KST+1)
         VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K-KST+1)
         VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K-KST+1)
         VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K-KST+1)   
   40    CONTINUE
C     ITH=MTH in following section.
      ELSE
         CALL IDW(I,ITH-1,KST)
         N=MPH(I,ITH-1)
         KND=KST+N-1
         DO K=KST+1,KND
            Y1=DSGA(I,ITH-1,K-KST)
            Y2=DSGA(I,ITH-1,K-KST+1)
            Y5=VL1A(I,ITH-1,K-KST)
            Y6=VL1A(I,ITH-1,K-KST+1)
            Y9=VL3A(I,ITH-1,K-KST)
            Y10=VL3A(I,ITH-1,K-KST+1)
            Y13=VL2A(I,ITH-1,K-KST)
            Y14=VL2A(I,ITH-1,K-KST+1)
		  CSGM=(CSG(I,K)+CSG(I,K-1))/2.D0
		  BB=0.D0
		  IF (CSGM.GT.0.D0) THEN
			IF ((STVS(I,K-1).LT.0.D0).AND.(STVS(I,K).LT.0.D0)) BB=1.D0
		  END IF
            Y17=VL5A(I,ITH-1,K-KST)*BB
            Y18=VL5A(I,ITH-1,K-KST+1)*BB
            Y21=VL4A(I,ITH-1,K-KST)*BB
            Y22=VL4A(I,ITH-1,K-KST+1)*BB
            DLPH=AR3(I,ITH-1,K-KST+1)-AR3(I,ITH-1,K-KST)
	      DV=2.D0
            ARYF(K-KST+1)=(Y1+Y2)/DV*DLCS*DLPH
            ARYG(K-KST+1)=(Y5+Y6)/DV*DLCS*DLPH
            ARYH(K-KST+1)=(Y9+Y10)/DV*DLCS*DLPH
            ARYI(K-KST+1)=(Y13+Y14)/DV*DLCS*DLPH
            ARYJ(K-KST+1)=(Y17+Y18)/DV*DLCS*DLPH
            ARYK(K-KST+1)=(Y21+Y22)/DV*DLCS*DLPH
            VITG1(I,ITH)=VITG1(I,ITH)+ARYF(K-KST+1)
            VITG2(I,ITH)=VITG2(I,ITH)+ARYG(K-KST+1)
            VITG3(I,ITH)=VITG3(I,ITH)+ARYH(K-KST+1)
            VITG4(I,ITH)=VITG4(I,ITH)+ARYI(K-KST+1)
            VITG5(I,ITH)=VITG5(I,ITH)+ARYJ(K-KST+1)  
            VITG6(I,ITH)=VITG6(I,ITH)+ARYK(K-KST+1)
         END DO
      END IF      
   42 CONTINUE
C     
C  
C     Do integrations over latitude
C      WRITE (NOUT1,102)
C      WRITE (NOUT1,1000)
      DO ITH=1,MTH
         SURF2=SURF2+VITG1(I,ITH)
         SM10B=SM10B+VITG2(I,ITH)
         BOL2=BOL2+VITG3(I,ITH)
         SMEO=SMEO+VITG4(I,ITH)
         SMFO=SMFO+VITG6(I,ITH)
         SM20B=SM20B+VITG5(I,ITH)
C         WRITE (NOUT1,106) I,ITH
C         WRITE (NOUT1,110) SM20B,VITG5(I,ITH)
      END DO
C	       Write output for accretion disk 
      IF (KKS.LE.3) THEN
         WRITE (NOUT1,131) ARM 
         WRITE (NOUT1,132) AFC
         WRITE (NOUT1,133) BOLRM
         WRITE (NOUT1,134) BOLFCA
         WRITE (NOUT1,150) BOLFC
      END IF        
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
      SURF2=SURF2*RI2 
      ER2A=0.001D0*SURF2
      SM10B=SM10B*RI2
      ER10B=0.001D0*SM10B
      BOLER2=0.001D0*BOL2
      SMEOER=0.001D0*SMEO
      SMFOER=0.001D0*SMFO
      SM20B=SM20B*RI2
      ER20B=0.001D0*SM20B
      IF (KKS.LE.3) THEN
         WRITE (NOUT1,111) I,SURF1,ER1A
         WRITE (NOUT1,111) 3-I,SURF2,ER2A
      END IF   
      IF (KKS.LE.3) THEN
         WRITE (NOUT1,112) I,BOL1,BOLER1
         WRITE (NOUT1,112) 3-I,BOL2,BOLER2
      END IF   
	TRM=0.D0
	TFC=0.D0      
      IF (ARM.GT.0.D0) TRM=(BOLRM/ARM/SGMA)**0.25
      
	IF (AFC.GT.0.D0) TFC=(BOLFC/AFC/SGMA)**0.25
      IF (KKS.LE.3) THEN
         WRITE (NOUT1,135) TRM
         WRITE (NOUT1,136) TFC
      END IF   
      TPRI=(BOL1/SURF1/SGMA)**0.25
      TPSC=(BOL2/SURF2/SGMA)**0.25
      VMG1=CVMG-2.5D0*DLOG10(BOL1/SOLF)
	IF (BOL2.GT.1.D0) THEN
		VMG2=CVMG-2.5D0*DLOG10(BOL2/SOLF)
	ELSE
		VMG2=100.D0
	END IF
      IF (KKS.LE.3) THEN
         WRITE (NOUT1,119) I,TPRI
         WRITE (NOUT1,129) I,VMG1
         WRITE (NOUT1,120) 3-I,TPSC
         WRITE (NOUT1,130) 3-I,VMG2
      END IF   
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
      VLSA=SMDO/SMCO
	  IF (SMEO.NE.0.D0) THEN
		VLSB=SMFO/SMEO
	  END IF
	  IF (SMEO.EQ.0.D0) VLSB=1.D99
      IF (KKS.LE.3) THEN
         WRITE (NOUT1,125) TAV
         WRITE (NOUT1,149) BOLACD
         WRITE (NOUT1,147) BOLSM
         WRITE (NOUT1,148) VMGSM
      END IF   
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
      WRITE (NOUT1,143) FLTFC
      WRITE (NOUT1,144) FLECFC
      WRITE (NOUT1,114) I,U1(I,1),U2(I,1),SMCO,SMCOER,SMDO,
     $   SMDOER,VLSA
      WRITE (NOUT1,115) 3-I,U1(3-I,1),U2(3-I,1),SMEO,SMEOER,SMFO,
     $   SMFOER,VLSB
C     Write output for scattered light.
      WRITE (NOUT1,157) SMARG
	WRITE (NOUT1,158) SMBRG
	WRITE (NOUT1,159) SMCRG
	WRITE (NOUT1,161) SMDRG
C     Write output for bremsstrahlung
      WRITE (NOUT1,162) BRFLX
	WRITE (NOUT1,163) EBRFLX
C     Add contributions of scattered light. Note that BRFLX is the
C     net bremsstrahlung reaching observer after the light lost by
C     eclipse by the companion has been subtracted.
	VLTSM=VLTSM+SMBRG+BRFLX+EBRFLX
	VLTSB=VLTSB+SMCRG+EBRFLX	        
      VLT=VLTSM-VLTSB
      FLOSS=VLTSB/VLTSM
      WRITE (NOUT1,121)VLTSM,VLTSB,VLT 
      WRITE (NOUT1,152) FLOSS
	WRITE (NOUT1,102)
	WRITE (NOUT1,102)          
C    Write output
      CALL WTOUT5(I,IWL,VI,PSI,SP,SMCO,SMCOER,SMDO,SMDOER,SMEO,
     $SMEOER,SMFO,SMFOER,VLTSM,VLTSB,1.D-3*VLTSM,VLT,1.D-3*VLT,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,NOUT2)
	WRITE (NOUT3,108) PSI
	WRITE (NOUT3,160) ARM,APROJR,AECR,BOLRM,FLTRM,FLECRM,AFC,APROJF,
     $AECF,BOLFC,BOLFCA,FLTFC,FLECFC,SURF1,SURF2,SM10A,SM20A,BOL1,SMCO,
     $SMDO,BOL2,SM10B,SM20B,SMEO,SMFO,TRM,TFC,TPRI,TPSC,SURFSM,BOLSM,
     $TAV,VLTSM,VLTSB,VLSA,VLSB,VLT,FLOSS,SMARG,SMBRG,SMCRG,SMDRG      
   60 CONTINUE
      WRITE (NOUT1,123)
      REWIND IN3
	REWIND IN7
  160 FORMAT(8E10.3)
   50 CONTINUE   
   199 CONTINUE   
C  199 CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F2)
      TM=(F2-F1)*1440.D0
      WRITE (NOUT1, 205) TM
      CLOSE (UNIT=13,    STATUS='KEEP')
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
      STOP
      END PROGRAM ACPGDRG
