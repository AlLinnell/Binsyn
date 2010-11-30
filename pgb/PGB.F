      PROGRAM PGB
C  
C     KCTRL IS A CONTROL INTEGER TO DECIDE ON THE FORM OF INPUT   
C     IF KCTRL.NE.0,USE ASSIGNED PHASES FOR ALL CALCULATIONS      
C     IF KCTRL=0,INPUT THREE INTEGERS TO DETERMINE THE DISTRIBUTION      
C     IF KCTRL.NE.0,USE ASSIGNED PHASES FOR ALL CALCULATIONS      
C     IF KCTRL=0,INPUT THREE INTEGERS TO DETERMINE THE DISTRIBUTION      
C     OF POINTS WITHIN CRITICAL PHASE RANGES 
C     N1=NUMBER OF POINTS BETWEEN PHASE 0.0 AND THE PHASE OF INTERNAL 
C     CONTACT. THIS INCLUDES BOTH END POINTS 
C     N2=NUMBER OF POINTS FROM THE PHASE OF INTERNAL CONTACT TO
C     EXTERNAL CONTACT,EXCLUDING INTERNAL AND INCLUDING EXTERNAL  
C     N3=NUMBER OF POINTS FROM EXTERNAL CONTACT TO ELONGATION MAX.    
C     THIS EXCLUDES THE INITIAL POINT AND INCLUDES THE LAST
C     THE SEQUENCE IS REPEATED IN INVERSE ORDER TO THE PHASE OF SEC.
C     MIN.
C     NOTE THAT A SPECIFIC PHASE VALUE IS READ IN FOR SEC.MIN.    
C     IF THE PHASE OF SEC.MIN.IS WAVELENGTH-DEPENDENT,A SEPARATE RUN     
C     IS NECESSARY FOR EACH WAVELENGTH
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION VLPSI(MPHS),LV(13)
      DIMENSION TARY(MTR,MTR)
      DIMENSION FL1(17)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1,BFR
      CHARACTER * 10 DATE,TIME,ZONE
      INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      INTEGER * 2 KEYGOOD,KEYBAD
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA IN1,IN2,IN3,IN4,IN5,IN6/1,2,3,4,5,66/
      DATA NOUT1,NOUT2,NOUT3/7,8,9/
      DATA NOUT4,NOUT5,NOUT6,NOUT7/10,11,12,13/
      DATA NOUT8,NOUT9,NOUT10,NOUT11/14,15,16,17/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
      DATA PHZERO/0.D0/
      DATA ERLT/1.D-13/
      DATA SNRAD/6.9599D10/
      DATA GRAV/6.6704D-8/
      DATA RAD/57.295779513082320876798D0/
      DATA TWPI/6.2831853071795864769D0/
      DATA TPIO2/4.712388980384689858D0/
      DATA PI/3.141592653589793238463D0/
      DATA PIO2/1.5707963267948966192D0/
C     Set default value of OMD
      DATA OMD/90.D0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  102 FORMAT (1X,4F16.13)
  103 FORMAT (1H1)
  104 FORMAT (1X,'PROGRAM PGB',10X,'DATE='I2,'/',I2,'/'I4,2X,'TIME=',I2,
     $':',I2,':',I2,'.',I2)
  105 FORMAT (1X,'Ordinal value of orbital phase=',I5)
  106 FORMAT (10I5)     
  107 FORMAT (1H )      
  108 FORMAT (5F16.13)  
  109 FORMAT (1X,'Control integer=',I5,/,' If the control integer=0,Calc
     $ulate the orbital phase values')
  110 FORMAT (1X,'Maximum number of orbital phase values to input=',I5)
  111 FORMAT (4X,'Array of SHS and SHN values')
  112 FORMAT (1X,'Orbital inclination=',F20.12, 2X, 'degrees')
  113 FORMAT (1X,'Orbital phase values (radians)=')
  114 FORMAT (1X,'Calculated values of orbital phase',/,
     $'Number of points between orbital phase 0.0 and third contact=',
     $I6,/,'Number of points between third and fourth contact=',12X,
     $I5,/,
     $'Number of points between fourth contact and maximum elongation='
     $,I4,
     $/,'Number of points between mid-sec. and third contact=',10X,I5,/
     $,'Number of points between third and fourth contact=',12X,I5,/,
     $'Number of points between fourth contact and maximum elongation='
     $,I4,/,
     $'Total number of points betweeen phase 0.0 and phase two pi ='
     $,2X,I5,/,1X,'Ordinal value of starting orbital phase=',I5,/,
     $1X,'Ordinal value of final orbital phase=',3X,I5)
  115 FORMAT (1X,'Phase of mid-secondary eclipse=',F20.12)
  116 FORMAT (1X,'Revised numbers of points in critical phase ranges=',
     $6I3,/,1X,'Total number of orbital phases=', I5,/,
     $1X,'Revised ordinal starting phase=', I5,/, 1X,
     $'Revised ordinal final phase=',3X,I5)
  117 FORMAT (1X,'Orbital longitude=',F15.10,5X,'Mean anomaly=',F
     $15.10,/,1X,'Eccentric anomaly=',F15.10,5X,'True anomaly=',F
     $15.10,/,1X,'Component separation=',F15.10,1X,'(units of semi-major
     $ axis)',/,1X,'Orbital phase=',F15.10)
  118 FORMAT (1X,'Values of VST and VND,component',I5)
  119 FORMAT (1X,'Lower theta boundary of north polar region entirely be
     $low horizon=',F16.13,/,1X,'corresponding longitude of point tangen
     $t to horizon=',F30.13,/,1X,'Upper theta boundary of south polar re
     $gion entirely below horizon=',F16.13,/,1X,'Corresponding longitude
     $ of point tangent to horizon=',F30.13,/,1X,'Lower theta boundary o
     $f north polar region of full visibility=',F16.13,/,1X,'Correspondi
     $ng longitude of point tangent to horizon=',F26.13,/,1X,'Upper thet
     $a boundary of south polar region of full visibility=',F16.13,/,1X,
     $'Corresponding longitude of point tangent to horizon=',F26.13)
  120 FORMAT (1X,'Theta value of northern edge of shadow=',F16.10,5X,'Th
     $eta value of southern edge of shadow=',F16.10)
  121 FORMAT (1X,'Depth of eclipse=',F12.5)
  122 FORMAT (1X,'Orbital inclination=',F10.6,5X,'Limit=0.5*PI')
  123 FORMAT (1X,'Projected center of star (3-I) at XC=',F7.4,5X,'YC=',F
     $7.4)
  125 FORMAT (5X,10F12.6)
  126 FORMAT (1X,'Values of NRY1,NRY2,...',/,1X,12I5)
  127 FORMAT (7F18.13)
  128 FORMAT (1X,'Values of LV(1),LV(2),...,LV(13)',/,1X,13I5)
  130 FORMAT (1X, 'Orbital eccentricity=',5X,F10.8)
  131 FORMAT (1X,'Longitude of periastron=',F12.8,1X,'degrees')
  132 FORMAT (1X,'Semi-major axis of relative orbit=',F15.8,1X,
     $'Solar radii')
  133 FORMAT (1X,'Orbital longitude of minimum projected separation=',
     $F12.6,1X,'radians')
  134 FORMAT (1X,'True anomaly of minimum projected separation=',5X,
     $F12.6,1X,'radians')
  135 FORMAT (1X,'Mean anomaly of mid-primary minimum=',7X,F15.6,
     $1X,'radians')
  136 FORMAT (1X,'Orbital phase of mid-secondary minimum=',8X,F15.6)
  137 FORMAT (1X,'Position angle of mid-primary minimum=',14X,F10.6,/,
     $1X,'True anomaly of mid-primary minimum=',14X,F12.6,/,
     $1X,'Mean anomaly of mid-primary minimum=',14X,F12.6,/,
     $1X,'Orbital phase of mid-primary minimum=',13X,F12.6)
  138 FORMAT(1X,'Position angle of pri. min. first contact=',8X,F12.6,/
     $,1X,'True anomaly of pri. min. first contact=',10X,F12.6,/,
     $1X,'Mean anomaly of pri. min. first contact=',10X,F12.6,/,
     $1X,'Orbital phase of pri. min. first contact=',9X,F12.6)
  139 FORMAT(1X,'Position angle of pri. min. second contact=',7X,F12.6
     $,/,1X,'True anomaly of pri.min. second contact=',10X,F12.6,/,
     $1X,'Mean anomaly of pri. min. second contact=',9X,F12.6,/,
     $1X,'Orbital phase of pri. min. second contact=',8X,F12.6)
  140 FORMAT(1X,'Position angle of pri. min. third contact=',8X,F12.6,
     $/,1X,'True anomaly of pri. min. third contact=',10X,F12.6,/,
     $1X,'Mean anomaly of pri.min. third contact=',11X,F12.6,/,
     $1X,'Orbital phase of pri. min. third contact=',9X,F12.6)
  141 FORMAT(1X,'Position angle of pri. min. fourth contact=',7X,F12.6
     $,/,1X,'True anomaly of pri. min. fourth contact=',9X,F12.6,/,
     $1X,'Mean anomaly of pri. min. fourth contact=',9X,F12.6,/,
     $1X,'Orbital phase of pri. min. fourth contact=',8X,F12.6)
  142 FORMAT(1X,'Position angle of first max. elongation=',10X,F12.6,/
     $,1X,'True anomaly of first max. elongation=',12X,F12.6,/,
     $1X,'Mean anomaly of first max. elongation=',12X,F12.6,/,
     $1X,'Orbital phase of first max. elongation=',11X,F12.6)
  143 FORMAT(1X,'Position angle of sec. min. first contact='8X,F12.6,
     $/,1X,'True anomaly of sec. min. first contact=',10X,F12.6,/,
     $1X,'Mean anomaly of sec. min. first contact=',10X,F12.6,/,
     $1X,'Orbital phase of sec. min. first contact=',9X,F12.6)
  144 FORMAT(1X,'Position angle of sec. min. second contact=',7X,
     $F12.6,/,1X,'True anomaly of sec. min. second contact=',9X,F12.6
     $,/,1X,'Mean anomaly of sec. min. second contact='9X,F12.6,/,
     $1X,'Orbital phase of sec. min. second contact=',8X,F12.6)
  145 FORMAT(1X,'Position angle of sec. min.=',22X,F12.6,/,
     $1X,'True anomaly of sec. min.=',24X,F12.6,/,
     $1X,'Mean anomaly of sec. min.=',24X,F12.6,/,
     $1X,'Orbital phase of sec. min.=',23X,F12.6)
  146 FORMAT(1X,'Position angle of sec. min. third contact=',8X,F12.6
     $,/,1X,'True anomaly of sec. min. third contact=',10X,F12.6,/,
     $1X,'Mean anomaly of sec. min. third contact=',10X,F12.6,/,
     $1X,'Orbital phase of sec. min. third contact=',9X,F12.6)
  147 FORMAT(1X,'Position angle of sec. min. fourth contact=',7X,
     $F12.6,/,1X,'True anomaly of sec. min. fourth contact=',9X,F12.6,
     $/,1X,'Mean anomaly of sec. min. fourth contact=',9X,F12.6,/,
     $1X,'Orbital phase of sec. min. fourth contact=',8X,F12.6)
  148 FORMAT(1X,'Position angle of second max. elongation=',9X,F12.6,
     $/,1X,'True anomaly of second max. elongation=',11X,F12.6,/,
     $1X,'Mean anomaly of second max. elongation=',11X,F12.6,/,
     $1X,'Orbital phase of second max. elongation=',10X,F12.6)
  255 FORMAT (1X,'Program running time=',F12.4,2X,'seconds')
  256 FORMAT ( 1X,'Current orbital position angle=', F20.12)
C
C*********************************************************
      OPEN (UNIT=19,     FILE='PGBA.ILS', STATUS='OLD')
      READ (19, 100) BFR
      OPEN (UNIT = 18,   FILE = BFR,  STATUS = 'OLD')
      READ (18, 100)     (FL1(J),J=1,17)
      OPEN (UNIT=IN1,   FILE=FL1(1), STATUS='OLD')         
      OPEN (UNIT=IN2,   FILE=FL1(2), STATUS='OLD')         
      OPEN (UNIT=IN3,   FILE=FL1(3), STATUS='OLD')         
      OPEN (UNIT=IN4,   FILE=FL1(4), STATUS='OLD')         
      OPEN (UNIT=IN5,   FILE=FL1(5), STATUS='OLD')         
      OPEN (UNIT=IN6,   FILE=FL1(6), STATUS='OLD')         
      OPEN (UNIT=NOUT1, FILE=FL1(7), STATUS='REPLACE')         
      OPEN (UNIT=NOUT2, FILE=FL1(8), STATUS='REPLACE')         
      OPEN (UNIT=NOUT3, FILE=FL1(9), STATUS='REPLACE')         
      OPEN (UNIT=NOUT4, FILE=FL1(10), STATUS='REPLACE')        
      OPEN (UNIT=NOUT5, FILE=FL1(11), STATUS='REPLACE')        
      OPEN (UNIT=NOUT6, FILE=FL1(12), STATUS='REPLACE')        
      OPEN (UNIT=NOUT7, FILE=FL1(13), STATUS='REPLACE')        
      OPEN (UNIT=NOUT8, FILE=FL1(14), STATUS='REPLACE')
      OPEN (UNIT=NOUT9, FILE=FL1(15), STATUS='REPLACE')
      OPEN (UNIT=NOUT10, FILE=FL1(16), STATUS='REPLACE')
      OPEN (UNIT=NOUT11, FILE=FL1(17), STATUS='REPLACE')        
C********************************************************
      WRITE (IN1,101) KEYBAD
      CLOSE (UNIT=IN1, STATUS = 'KEEP')
      LABEL = ' ***PGB'
      LBLERR=0
      NU=IN2
C      Initialize NKP
      NKP=0
      CALL GTLBL (LABEL, LBLERR)
      IF ( LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (KCTRL)
      CALL DUMMY(5)
      CALL IREAD (NPSI)
      IF (NPSI.GT.MPHS) THEN
      	WRITE (NOUT1,257)
      	STOP 'PGB: NPSI TOO LARGE'
      END IF
  257 FORMAT (1X,'NPSI TOO LARGE')
      CALL DUMMY (1)
      CALL DREAD (VLVID, 'F12.8')
      CALL DUMMY (1)
      CALL DREAD (ECC, 'F11.8')
      CALL DUMMY (1)
      IF (ECC.NE.0.D0) THEN
        CALL DREAD (OMD, 'F11.8')
      ELSE
        CALL DUMMY (1)
      END IF
      CALL DUMMY (1)
      CALL IREAD (N1)
      CALL DUMMY (1)
      CALL IREAD (N2)
      CALL DUMMY (1)
      CALL IREAD (N3)
      CALL DUMMY (1)
      CALL IREAD (N4)
      CALL DUMMY (1)
      CALL IREAD (N5)
      CALL DUMMY (1)
      CALL IREAD (N6)
      CALL DUMMY (1)
      CALL IREAD (LST)
      CALL DUMMY (1)
      CALL IREAD (LND)
      CALL DUMMY(2)
      IF (KCTRL.NE.1) GO TO 10
      DO I=1,NPSI
        CALL DREAD (VLPSI(I), 'F16.13')
        IF (VLPSI(I).GT.TWPI) VLPSI(I)=TWPI
      END DO
C
C     Convert inclination and longitude of periastron from 
C     degrees to radians
   10 VLVI=VLVID/RAD
      OMRAD=OMD/RAD
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (NOUT1,103)     
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
C     ENTER CONTROL FOR SRT'S TO CALL
      WRITE (NOUT1,109) KCTRL 
      WRITE (NOUT1,110) NPSI
      WRITE (NOUT1,112) VLVID
      WRITE (NOUT1,130) ECC
      WRITE (NOUT1,131) OMD
C     Read orbital period      
      NU=IN3
      LABEL = '**CALPT'
      LBLERR=0
      CALL GTLBL(LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL DUMMY(2)
      CALL DREAD(PER,  'F15.12')
      IF (KCTRL.NE.2) GO TO 30
      READ (IN6,106) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN6,127) (VLPSI(K),K=1,NPSI)
      DO K=1,NPSI
         IF (VLPSI(K).GT.TWPI) VLPSI(K)=TWPI
      END DO
      READ (IN6,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      READ (IN6,106) (LV(K),K=1,13)
      IF (KCTRL.EQ.2) CLOSE (UNIT=IN6, STATUS='KEEP')
C     INPUT PERMANENT FILE DATA
   30 CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,
     $IN4)
C     Calculate semi-major axis
      VAR1=GRAV*(VMI+VMI*QS)
      PERSC=PER*86400.D0
      VAR2=PERSC*PERSC/TWPI*VAR1/TWPI
      SMA=DEXP(DLOG(VAR2)/3.D0)
C     Convert to solar radii
      SMA=SMA/SNRAD
C     Set ISV=I from PGA, the star eclipsed at orbital phase 0.0.
      ISV=I      
C     SET RA=SIDE EQUATORIAL RADIUS OF STAR I
      N=NPH(I,NTH/2+1)
      ITH=NTH/2+1
      KQT=N/4+1
      RA=VR(I,ITH,KQT)
C     Set polar radius of star I
      RVSCI=VR(I,1,1)
C     SET RB=SIDE EQUATORIAL RADIUS OF STAR 3-I
      N=NPH(3-I,NTH/2+1)
      ITH=NTH/2+1
      KQT=N/4+1
      RB=VR(3-I,ITH,KQT)
C     Set polar radius of star 3-I
      RVSC3MI=VR(3-I,ITH,1)
C
C     Set condition that RA is radius of smaller star, RB is radius of
C     larger star.
      IF (RA.LT.RB) GO TO 4
      SV=RA
      RA=RB
      RB=SV
    4 IF (KCTRL.NE.0) GO TO 3
C     KCTRL = 0. Set number of fiducial orbital phase points for 
C     complete orbit.      
      NPSI=2*(N1+N2+N3+N4+N5+N6)
      WRITE (NOUT1,114) N1,N2,N3,N4,N5,N6,NPSI,LST,LND
C     NOTE THAT PHSEC IS THE FRACTION OF THE CYCLE AT WHICH MID-SECOND.  
C     ECLIPSE OCCURS.IT IS NOT AN ANGLE.     
    3 VI=VLVI 
      IF (VI.GT.PIO2) GO TO 60 
C  
C  
C     NOTE THAT THE USE OF CSI IS IN CALCULATING THE LINE OF SIGHT FROM  
C     THE STAR   
C  
C
C     The following program section follows the development in Kopal,
C     Close Binary Systems, eqs. 9-18 and following.  
      CSI=-DCOS(VI)      
      SNI=DSIN(VI)
      VLMTH=DABS(CSI/SNI)
      COSOM=DCOS(OMRAD)
      SIGN=1.D0
      SIGNV=DSIGN(SIGN,COSOM)
      VLMTH=VLMTH*SIGNV
C     Position angles mean measurement relative to conjunction.
C     Orbital phase angles mean relative to mid-primary light minimum.
C     Calculate TH0, orbital position angle of mid-pri. min.(radians)
      IF (DABS(VLMTH).GE.0.01D0) THEN
         CALL RTFD(-VLMTH, VLMTH, ECC, OMRAD, VI, TH0, 1.D-9, 20)
      ELSE
C     Kopal equations 9-20
         VAR1=DSIN(OMRAD)
         TH0=-ECC*COSOM*VLMTH**2*(1.D0-ECC*VAR1/SNI**2)
      END IF
      IF (TH0.LT.0.D0) TH0=TH0+TWPI
      IF (TH0.GE.TWPI) TH0=TH0-TWPI
C     Calculate corres. theta for secondary min. (Kopal, eq. 9-20)
C     The following value is a first approximation.
      THTWO=PI-TH0
      DTH=0.02D0
      TH1=THTWO-DTH
      TH2=THTWO
      TH3=THTWO+DTH
      LPCT=1
  500 CALL FTHETA(ECC,OMRAD,TH1,VI,FN1)
      CALL FTHETA(ECC,OMRAD,TH2,VI,FN2)
      CALL FTHETA(ECC,OMRAD,TH3,VI,FN3)
      RT=TH2
      IF (DABS(FN2).LT.1.D-8) GO TO 501
      IF ((FN1*FN3).LT.0.D0) THEN
          CALL RTFD(TH1,TH3,ECC,OMRAD,VI,RT,1.0D-9,20)
      END IF
      IF (FN1*FN3.GT.0.D0) THEN
          IF ((DABS(FN2).LT.DABS(FN1)).AND.(DABS(FN2).LT.DABS(FN3)))THEN
		DTH=0.4D0*(DABS(TH1-TH3))
		TH2=0.5D0*(TH1+TH3)
		TH1=TH2+DTH
		TH3=TH2-DTH
		LPCT=LPCT+1
		IF (LPCT.LT.50) GO TO 500
		CALL PMDSTOP
	  END IF
	IF (DABS(FN1).LT.DABS(FN3)) THEN
		DTH=0.4D0*(DABS(TH1-TH3))
		TH3=TH2
		TH1=TH1+DTH
		TH2=0.5D0*(TH1+TH3)
		LPCT=LPCT+1
		IF (LPCT.LT.50) GO TO 500
		CALL PMDSTOP
	END IF
	IF (DABS(FN1).GE.DABS(FN3)) THEN
		DTH=0.4D0*(DABS(TH1-TH3))
		TH1=TH2
		TH3=TH3-DTH
		TH2=0.5D0*(TH1+TH3)
		LPCT=LPCT+1
		IF (LPCT.LT.50) GO TO 500
		CALL PMDSTOP
	END IF
	END IF
  501 ICT=1
      THV=THTWO
	DLT=0.001D0
	VARSV=1.D10
      IF (THTWO.LT.0.D0) THTWO=THTWO+TWPI
      IF (THTWO.GE.TWPI) THTWO=THTWO-TWPI
C     Calculate true anomaly of mid-pri. min.
      VCONJ=TH0-OMRAD+PIO2
      IF (VCONJ.LT.0.D0) VCONJ=VCONJ+TWPI
      IF (VCONJ.GE.TWPI) VCONJ=VCONJ-TWPI
C     Calculate mean anomaly of mid-pri. min.
      CALL FMEANAN(VCONJ, ECC, VMN0)
      IF (VMN0.LT.0.D0) VMN0=VMN0+TWPI
      IF (VMN0.GE.TWPI) VMN0=VMN0-TWPI
C     Calculate true anomaly of mid-secondary.
      VSEC=PIO2-OMRAD+THTWO
      IF (VSEC.LT.0.D0) VSEC=VSEC+TWPI
      IF (VSEC.GE.TWPI) VSEC=VSEC-TWPI
C     Calculate eccentric anomaly of mid-secondary.
      HFANG=VSEC/2.D0
      TNHFE2=DSQRT((1.D0-ECC)/(1.D0+ECC))*DTAN(HFANG)
      ECAN2=2.D0*DATAN(TNHFE2)
      IF (ECAN2.LT.0.D0) ECAN2=ECAN2+TWPI
      IF (ECAN2.GE.TWPI) ECAN2=ECAN2-TWPI
C     Calculate mean anomaly of mid-secondary
      VMN2=ECAN2-ECC*DSIN(ECAN2)
      IF (VMN2.LT.0.D0) VMN2=VMN2+TWPI
      IF (VMN2.GE.TWPI) VMN2=VMN2-TWPI
C     Calculate PHSEC,orbital phase of mid-secondary. This is the time
C     of mid-sec. as a fraction of the period, times two pi.
C     The time of mid-primary min, in units of TWPI/Period, is PHZERO
      PHSEC=VMN2-VMN0
      IF (PHSEC.LT.0.D0) PHSEC=PHSEC+TWPI
      IF (PHSEC.GE.TWPI) PHSEC=PHSEC-TWPI
C     Calculate orbital phases of max. component separations
C     Do max. elongation following pri.min.
      DLTH=0.1D0
      DDLTH=0.1D0
    7 THTHREE=0.D0
C     Set position angles that bracket the desired position angle
      VLTH1=TH0+PIO2-DLTH
      VLTH2=VLTH1+2.D0*DLTH
C     Calculate values of the function whose root gives the  desired
C     value of theta
      CALL FTHETA(ECC,OMRAD,VLTH1,VI,FNVL1)
      CALL FTHETA(ECC,OMRAD,VLTH2,VI,FNVL2)
C     Test the two function values. We may have hit the root.
      IF (DABS(FNVL1).LT.1.D-12) THTHREE=VLTH1
      IF (DABS(FNVL2).LT.1.D-12) THTHREE=VLTH2
C     If we hit the root, THTHREE will be different from 0.0
      IF (THTHREE.NE.0.D0) GO TO 8
C     If the ratio of the two function values .GT. 0.0, both position
C     angles are on the same side of the root. In that case, increase
C     the separation of the approximation position angles.
      IF (FNVL1/FNVL2.GT.0.D0) THEN
         DLTH=DLTH+DDLTH
            IF (DLTH.LT.PIO2) THEN
               GO TO 7
            ELSE
               CALL PMDSTOP
            END IF
      ELSE
C     The position angles bracket the root. Find the root.
            CALL RTFD(VLTH1,VLTH2,ECC,OMRAD,VI,THTHREE,1.D-9,20)
      END IF
      IF (THTHREE.LT.0.D0) THTHREE=THTHREE+TWPI
      IF (THTHREE.GE.TWPI) THTHREE=THTHREE-TWPI
C     Now repeat the process for the max. elongation following
C     sec. min.
    8 DLTH=0.1D0
   15 THFOUR=0.D0
      VLTH1=THTHREE+PI-DLTH
      VLTH2=VLTH1+2.D0*DLTH
      CALL FTHETA(ECC,OMRAD,VLTH1,VI,FNVL1)
      CALL FTHETA(ECC,OMRAD,VLTH2,VI,FNVL2)
      IF (DABS(FNVL1).LT.1.D-12) THFOUR=VLTH1
      IF (DABS(FNVL2).LT.1.D-12) THFOUR=VLTH2
      IF (THFOUR.NE.0.D0) GO TO 9
      IF (FNVL1/FNVL2.GT.0.D0) THEN
         DLTH=DLTH+DDLTH
            IF (DLTH.LT.PIO2) THEN
               GO TO 15
            ELSE
               CALL PMDSTOP
            END IF
         ELSE
            CALL RTFD(VLTH1,VLTH2,ECC,OMRAD,VI, THFOUR, 1.D-9, 20)
      END IF
      IF (THFOUR.LT.0.D0) THFOUR=THFOUR+TWPI
      IF (THFOUR.GE.TWPI) THFOUR=THFOUR-TWPI
C     We have found the position angles for max. elongation.
C     Now determine the corresponding orbital phases.
C     Calculate the true anomaly of the first max. elongation.
    9 VTHREE=THTHREE-OMRAD+PIO2
      IF (VTHREE.LT.0.D0) VTHREE=VTHREE+TWPI
      IF (VTHREE.GE.TWPI) VTHREE=VTHREE-TWPI
C     Calculate mean anomaly of first max. elongation
      CALL FMEANAN(VTHREE, ECC, VMN3)
      IF (VMN3.LT.0.D0) VMN3=VMN3+TWPI
      IF (VMN3.GE.TWPI) VMN3=VMN3-TWPI
C     Calculate orbital phase of first max. elongation
      PHTHREE=VMN3-VMN0+PHZERO
      IF (PHTHREE.LT.0.D0) PHTHREE=PHTHREE+TWPI
      IF (PHTHREE.GE.TWPI) PHTHREE=PHTHREE-TWPI
C     Calculate true anomaly of second max. elongation
      VFOUR=THFOUR-OMRAD+PIO2
      IF (VFOUR.LT.0.D0) VFOUR=VFOUR+TWPI
      IF (VFOUR.GE.TWPI) VFOUR=VFOUR-TWPI
C     Calculate mean anomaly of second max. elongation
      CALL FMEANAN(VFOUR, ECC, VMN4)
      IF (VMN4.LT.0.D0) VMN4=VMN4+TWPI
      IF (VMN4.GE.TWPI) VMN4=VMN4-TWPI
C     Calculate orbital phase of second max. elongation
      PHFOUR=VMN4-VMN0+PHZERO
      IF (PHFOUR.LT.0.D0) PHFOUR=PHFOUR+TWPI
      IF (PHFOUR.GE.TWPI) PHFOUR=PHFOUR-TWPI
C
C     Calculate orbital position angles for contacts 1-4, both minima.
C
      VISV=VI
      CALL CONTACT(VI,RA,RB,1.D0,OMRAD,ECC,VCONJ,VSEC,THFCP,THSCP,
     $THTCP,THLCP,THFCS,THSCS,THTCS,THLCS)
C     Calculate orbital phases corres. to position angles.
C     Write output for pri. min. first contact
      VTRUE=THFCP-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHFCP=VMNVL-VMN0+PHZERO
      IF (PHFCP.LT.0.D0) PHFCP=PHFCP+TWPI
      IF (PHFCP.GE.TWPI) PHFCP=PHFCP-TWPI
      IF (THFCP.LT.0.D0) THFCP=THFCP+TWPI
      IF (THFCP.GE.TWPI) THFCP=THFCP-TWPI
      WRITE (NOUT1,138)THFCP,VTRUE,VMNVL,PHFCP
      WRITE (NOUT1,107)
C
C     Write output for pri. min. second contact
      VTRUE=THSCP-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHSCP=VMNVL-VMN0+PHZERO
      IF (PHSCP.LT.0.D0) PHSCP=PHSCP+TWPI
      IF (PHSCP.GE.TWPI) PHSCP=PHSCP-TWPI
      IF (THSCP.LT.0.D0) THSCP=THSCP+TWPI
      IF (THSCP.GE.TWPI) THSCP=THSCP-TWPI
      WRITE (NOUT1,139)THSCP,VTRUE,VMNVL,PHSCP
      WRITE (NOUT1,107)
C
C     Write output for mid.-pri. min.
      WRITE (NOUT1,137) TH0,VCONJ,VMN0,PHZERO
      WRITE (NOUT1,107)
C
C     Write output for pri. min. third contact
      VTRUE=THTCP-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHTCP=VMNVL-VMN0+PHZERO
      IF (PHTCP.LT.0.D0) PHTCP=PHTCP+TWPI
      IF (PHTCP.GE.TWPI) PHTCP=PHTCP-TWPI
      IF (THTCP.LT.0.D0) THTCP=THTCP+TWPI
      IF (THTCP.GE.TWPI) THTCP=THTCP-TWPI
      WRITE (NOUT1,140) THTCP,VTRUE,VMNVL,PHTCP
      WRITE (NOUT1,107)
C
C     Write output for pri. min. fourth contact
      VTRUE=THLCP-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHLCP=VMNVL-VMN0+PHZERO
      IF (PHLCP.LT.0.D0) PHLCP=PHLCP+TWPI
      IF (PHLCP.GE.TWPI) PHLCP=PHLCP-TWPI
      IF (THLCP.LT.0.D0) THLCP=THLCP+TWPI
      IF (THLCP.GE.TWPI) THLCP=THLCP-TWPI
      WRITE (NOUT1,141) THLCP,VTRUE,VMNVL,PHLCP
      WRITE (NOUT1,107)
C
C     Write output for first max. elongation
      WRITE (NOUT1,142) THTHREE,VTHREE,VMN3,PHTHREE
      WRITE (NOUT1,107)
C
C     Write output for first contact, sec. min.
      VTRUE=THFCS-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHFCS=VMNVL-VMN0+PHZERO
      IF (PHFCS.LT.0.D0) PHFCS=PHFCS+TWPI
      IF (PHFCS.GE.TWPI) PHFCS=PHFCS-TWPI
      IF (THFCS.LT.0.D0) THFCS=THFCS+TWPI
      IF (THFCS.GE.TWPI) THFCS=THFCS-TWPI
      WRITE (NOUT1,143) THFCS,VTRUE,VMNVL,PHFCS
      WRITE (NOUT1,107)
C
C     Write output for second contact, sec. min.
      VTRUE=THSCS-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHSCS=VMNVL-VMN0+PHZERO
      IF (PHSCS.LT.0.D0) PHSCS=PHSCS+TWPI
      IF (PHSCS.GE.TWPI) PHSCS=PHSCS-TWPI
      IF (THSCS.LT.0.D0) THSCS=THSCS+TWPI
      IF (THSCS.GE.TWPI) THSCS=THSCS-TWPI
      WRITE (NOUT1,144) THSCS,VTRUE,VMNVL,PHSCS
      WRITE (NOUT1,107)
C
C     Write output for mid.-sec.min.
      WRITE (NOUT1, 145) THTWO,VSEC,VMN2,PHSEC
      WRITE (NOUT1,107)
C
C     Write output for third contact, sec. min.
      VTRUE=THTCS-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHTCS=VMNVL-VMN0+PHZERO
      IF (PHTCS.LT.0.D0) PHTCS=PHTCS+TWPI
      IF (PHTCS.GE.TWPI) PHTCS=PHTCS-TWPI
      IF (THTCS.LT.0.D0) THTCS=THTCS+TWPI
      IF (THTCS.GE.TWPI) THTCS=THTCS-TWPI
      WRITE (NOUT1,146) THTCS,VTRUE,VMNVL,PHTCS
      WRITE (NOUT1,107)
C
C     Write output for fourth contact, sec. min.
      VTRUE=THLCS-OMRAD+PIO2
      IF (VTRUE.LT.0.D0) VTRUE=VTRUE+TWPI
      IF (VTRUE.GE.TWPI) VTRUE=VTRUE-TWPI
      CALL FMEANAN(VTRUE, ECC, VMNVL)
      IF (VMNVL.LT.0.D0) VMNVL=VMNVL+TWPI
      IF (VMNVL.GE.TWPI) VMNVL=VMNVL-TWPI
      PHLCS=VMNVL-VMN0+PHZERO
      IF (PHLCS.LT.0.D0) PHLCS=PHLCS+TWPI
      IF (PHLCS.GE.TWPI) PHLCS=PHLCS-TWPI
      IF (THLCS.LT.0.D0) THLCS=THLCS+TWPI
      IF (THLCS.GE.TWPI) THLCS=THLCS-TWPI
      WRITE (NOUT1,147) THLCS,VTRUE,VMNVL,PHLCS
      WRITE (NOUT1,107)
C
C     Write output for second max. elongation
      WRITE (NOUT1,148) THFOUR,VFOUR,VMN4,PHFOUR
      WRITE (NOUT1,107)
      IF ((PHTHREE.LE.ERLT).AND.(PHFOUR.LE.ERLT)) THEN
         PHTS1=PIO2
         PHTS2=TPIO2
      ELSE
         PHTS1=PHTHREE
         PHTS2=PHFOUR
      END IF
      IF (PHSEC.LE.ERLT) THEN
         PHTS3=PI
         PHTS4=0.D0
      ELSE
         PHTS3=PHSEC
         PHTS4=PHZERO
      END IF
C
      IF (KCTRL.NE.0) GO TO 28
      IF (N1.LE.0) CALL PMDSTOP
      NA=N1+1
      VLPSI(1)=PHZERO
      LV(1)=1
      NPSI=1
      V=N1
      IF (DABS(THTCP-TH0).LT.1.D-5) GO TO 201
C
C     Program SPT will need points equally spaced in orbital
C     phase, for interpolation
C
C
C     SET POINTS FROM PHASE PHZERO TO PHTCP,INCLUSIVE
C
      IF ((PHZERO.GT.TPIO2).AND.(PHTCP.LT.PIO2)) THEN
         A=PHTCP-PHZERO+TWPI
      END IF
      IF ((PHZERO.LT.PIO2).AND.(PHTCP.LT.PIO2)) THEN
         A=PHTCP-PHZERO
      END IF
      IF ((PHZERO.GT.TPIO2).AND.(PHTCP.GT.TPIO2)) THEN
         A=PHTCP-PHZERO
      END IF
      DO 200 K=2,N1+1
      W=K-1
      VLPSI(K)=W/V*A+PHZERO
      IF (VLPSI(K).GT.TWPI) VLPSI(K)=VLPSI(K)-TWPI
      NPSI=NPSI+1
  200 CONTINUE
      NRY1=N1+1
      LV(2)=NPSI
      GO TO 202
  201 N1=0
      NA=1
      NRY1=N1
      LV(2)=NPSI
  202 IF (N2.LE.0) CALL PMDSTOP 
C     SET POINTS FROM PHTCP THROUGH PHLCP
      V=N2
      IF ((PHTCP.GT.TPIO2).AND.(PHLCP.LT.PIO2)) THEN
         A=PHLCP-PHTCP+TWPI
      END IF
      IF ((PHTCP.LT.PIO2).AND.(PHLCP.LT.PIO2)) THEN
         A=PHLCP-PHTCP
      END IF
      IF (A.LT.0.D0) PHLCP=PHTCP
      IF (DABS(PHLCP-PHTCP).LE.ERLT) A=0.D0
      IF (DABS(A).LE.ERLT) THEN
         NRY2=0
         LV(3)=NPSI
         GO TO 204
      END IF
      DO 203 K=1,N2     
      W=K 
      VLPSI(NA+K)=W/V*A+PHTCP
      IF (VLPSI(NA+K).GT.TWPI) VLPSI(NA+K)=VLPSI(NA+K)-TWPI
      NPSI=NPSI+1
  203 CONTINUE
      NRY2=N2+1
      LV(3)=NPSI
      GO TO 205
  204 N2=0
      NRY2=0
      LV(3)=NPSI
  205 IF (N3.EQ.0) THEN
         NRY3=0
         LV(4)=NPSI
         GO TO 209
      END IF
C     SET POINTS FROM PHLCP THROUGH TIME OF MAX.ELONGATION
      V=N3
      A=(PHTHREE-PHLCP)
      IF (A.LT.0.D0) THEN
         PHLCP=PHTHREE
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         N3=0
         NRY3=0
         LV(4)=NPSI
         GO TO 209
      END IF
      DO 207 K=1,N3     
      W=K 
      VLPSI(NA+N2+K)=W/V*A+PHLCP
      NPSI=NPSI+1
  207 CONTINUE
      NRY3=N3+1
      LV(4)=NPSI
C     Set points from max. elongation to first contact, sec. min.
  209 V=N6
      A=PHFCS-PHTHREE
      IF (DABS(A).LT.1.D-5) THEN
         PHFCS=PHTHREE
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         N6=0
         NRY4=0
         LV(5)=NPSI
         GO TO 212
      END IF
      DO 210 K=1,N6
      W=K 
      VLPSI(NA+N2+N3+K)=W/V*A+PHTHREE
      NPSI=NPSI+1
  210 CONTINUE
      NRY4=N6+1
      LV(5)=NPSI
C     From first contact,sec. min. to second contact, sec. min.
  212 V=N5
      A=PHSCS-PHFCS
      IF (DABS(A).LT.1.D-5) THEN
         PHSCS=PHFCS
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         N5=0
         NRY5=0
         LV(6)=NPSI
         GO TO 214
      END IF
      DO 213 K=1,N5
      W=K 
      VLPSI(NA+N2+N3+N6+K)=W/V*A+PHFCS
      NPSI=NPSI+1
  213 CONTINUE
      NRY5=N5+1
      LV(6)=NPSI
C     From second contact to mid-secondary
  214 V=N4
      IF (V.LE.1.D0) V=1.D0
      A=PHSEC-PHSCS
      IF (DABS(A).LT.1.D-5) THEN
         PHSCS=PHSEC
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         N4=0
         NRY6=0
         LV(7)=NPSI
         GO TO 216
      END IF
      DO 215 K=1,N4
      W=K
      VLPSI(NA+N2+N3+N6+N5+K)=W/V*A+PHSCS
      NPSI=NPSI+1
  215 CONTINUE
      NRY6=N4+1
      LV(7)=NPSI
C     From PHSEC to PHTCS
  216 IF (N4.LT.1) THEN
         NRY7=0
         LV(8)=NPSI
         GO TO 218
      END IF
      V=N4
      A=PHTCS-PHSEC
      IF (DABS(A).LT.1.D-5) THEN
         PHTCS=PHSEC
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         NRY7=0
         LV(8)=NPSI
         GO TO 218
      END IF
      DO 217 K=1,N4
      W=K
      VLPSI(NA+N2+N3+N6+N5+N4+K)=W/V*A+PHSEC
      NPSI=NPSI+1
  217 CONTINUE
      NRY7=N4+1
      LV(8)=NPSI
C     From PHTCS to PHLCS
  218 IF (N5.EQ.0) THEN
         NRY8=0
         LV(9)=NPSI
         GO TO 220
      END IF
      V=N5
      A=PHLCS-PHTCS
      IF (DABS(A).LT.1.D-5) THEN
         PHLCS=PHTCS
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         NRY8=0
         LV(9)=NPSI
         GO TO 220
      END IF
      DO 219 K=1,N5
      W=K
      VLPSI(NA+N2+N3+N6+N5+2*N4+K)=W/V*A+PHTCS
      NPSI=NPSI+1
  219 CONTINUE
      NRY8=N5+1
      LV(9)=NPSI
C     From PHLCS to PHFOUR
  220 IF (N6.EQ.0) THEN
         NRY9=0
         LV(10)=NPSI
         GO TO 222
      END IF
      V=N6
      A=PHFOUR-PHLCS
      IF (DABS(A).LT.1.D-5) THEN
         PHLCS=PHFOUR
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         NRY9=0
         LV(10)=NPSI
         GO TO 222
      END IF
      DO 221 K=1,N6
      W=K
      VLPSI(NA+N2+N3+N6+2*N5+2*N4+K)=W/V*A+PHLCS
      NPSI=NPSI+1
  221 CONTINUE
      NRY9=N6+1
      LV(10)=NPSI
C     From PHFOUR to PHFCP
  222 IF (N3.EQ.0) THEN
         NRY10=0
         LV(11)=NPSI
         GO TO 224
      END IF
      V=N3
      IF ((PHFCP.LT.PIO2).AND.(PHFOUR.GT.TPIO2)) THEN
         A=PHFCP-PHFOUR+TWPI
      END IF
      IF ((PHFCP.GT.TPIO2).AND.(PHFOUR.GT.PI)) THEN
         A=PHFCP-PHFOUR
      END IF
      IF ((PHFCP.GT.TPIO2).AND.(PHFOUR.GT.PI)) THEN
         A=PHFCP-PHFOUR
      END IF
      IF ((PHFCP.LT.PIO2).AND.(PHFOUR.GT.PI)) THEN
         A=PHFCP-PHFOUR+TWPI
      END IF
      IF (DABS(A).LT.1.D-5) THEN
         PHFCP=PHFOUR
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         NRY10=0
         LV(11)=NPSI
         GO TO 224
      END IF
      DO 223 K=1,N3
      W=K
      VLPSI(NA+N2+N3+2*N6+2*N5+2*N4+K)=W/V*A+PHFOUR
      NPSI=NPSI+1
  223 CONTINUE
      NRY10=N3+1
      LV(11)=NPSI
C     From PHFCP to PHSCP
  224 IF (N2.EQ.0) THEN
         NRY11=0
         LV(12)=NPSI
         GO TO 226
      END IF
      V=N2
      IF ((PHFCP.GT.TPIO2).AND.(PHSCP.GT.TPIO2)) THEN
         A=PHSCP-PHFCP
      END IF
      IF ((PHFCP.LT.PIO2).AND.(PHSCP.LT.PIO2)) THEN
         A=PHSCP-PHFCP
      END IF
      IF ((PHFCP.GT.TPIO2).AND.(PHSCP.LT.PIO2)) THEN
         A=PHSCP-PHFCP+TWPI
      END IF
      IF (DABS(A).LT.1.D-5) THEN
         PHSCP=PHFCP
         A=0.D0
      END IF
      IF (DABS(A).LE.ERLT) THEN
         NRY11=0
         LV(12)=NPSI
         GO TO 226
      END IF
      DO 225 K=1,N2
      W=K
      VLPSI(NA+N2+2*N3+2*N6+2*N5+2*N4+K)=W/V*A+PHFCP
      NPSI=NPSI+1
  225 CONTINUE
      NRY11=N2+1
      LV(12)=NPSI
C     From PHSCP through PHZERO
  226 IF (N1.EQ.0) THEN
         NRY12=0
         LV(13)=NPSI
         VLPSI(2*NA+2*N2+2*N3+2*N4+2*N5+2*N6)=PHZERO+TWPI
      END IF
      IF (N1.GE.1) THEN
         V=N1
         IF ((PHZERO.GT.TPIO2).AND.(PHSCP.GT.TPIO2)) THEN
            A=PHZERO-PHSCP
         END IF
         IF((PHZERO.LT.PIO2).AND.(PHSCP.GT.TPIO2)) THEN
            A=PHZERO-PHSCP+TWPI
         END IF
         IF ((PHZERO.LT.PIO2).AND.(PHSCP.LT.PIO2)) THEN
            A=PHZERO-PHSCP
         END IF
         IF (DABS(A).LT.1.D-5) THEN
            PHSCP=PHZERO
            A=0.D0
         END IF
         IF (DABS(A).LE.ERLT) THEN
            NRY12=0
            LV(13)=NPSI
            GO TO 228
         END IF
         DO 227 K=1,N1
         W=K
         VLPSI(NA+2*N2+2*N3+2*N4+2*N5+2*N6+K)=W/V*A+PHSCP
         NPSI=NPSI+1
  227 CONTINUE
         NRY12=N1+1
         LV(13)=NPSI
  228 CONTINUE
      END IF
   28 DO K=1,NPSI
         IF (DABS(VLPSI(K)-PIO2).LT.1.D-12)   VLPSI(K)=PIO2-1.D-13
         IF (DABS(VLPSI(K)-PI).LT.1.D-12)     VLPSI(K)=PI-1.D-13
         IF (DABS(VLPSI(K)-TPIO2).LT.1.D-12)  VLPSI(K)=TPIO2-1.D-13
      END DO   
      IF (LST.LE.0) LST=1
      IF (LND.LE.0) LND=NPSI   
      IF (LND.GT.NPSI) LND=NPSI
      WRITE (NOUT1,116) N1,N2,N3,N4,N5,N6,NPSI,LST,LND
      WRITE (NOUT1,126) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      WRITE (NOUT1,128) (LV(K),K=1,13)
      WRITE (NOUT1,113)
      WRITE (NOUT1,102) (VLPSI(K),K=1,NPSI)
      WRITE (NOUT1,132) SMA
      WRITE (NOUT5,106) NPSI,N1,N2,N3,N4,N5,N6,LST,LND
      WRITE (NOUT5,127) (VLPSI(KS),KS=1,NPSI)
      WRITE (NOUT5,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      WRITE (NOUT5,106) (LV(K),K=1,13)
      WRITE (NOUT4,127) VI,PHSEC
      WRITE (NOUT4,106) N1,N2,N3,N4,N5,N6
C  

C  
C     PRINCIPAL LOOP FOR SUCCESSIVE VALUES OF ORBITAL PHASE, AS
C     FRACTION OF ONE ORBIT, MEASURED FROM MID-PRIMARY MINIMUM IN
C     UNITS OF TWPI*(T-T0)/PERIOD
C  
C  
      DO 20 KS=LST,LND
C     Initialize SHS(), SHN()
      DO ITH=1,NTH
		SHS(ITH,1)=6.29D0
		SHN(ITH,1)=6.29D0
		SHS(ITH,2)=6.29D0
		SHN(ITH,2)=6.29D0
		SHS(ITH,3)=6.29D0
		SHN(ITH,3)=6.29D0
      END DO
C     NOTE. PSIP is the time from midprimary of the point in question,
C     measured in units of TWPI*(T-T0)/PERIOD
      PSIP=VLPSI(KS)
      IF (PSIP.LT.0.D0) PSIP=PSIP+TWPI
      IF (PSIP.GT.TWPI) PSIP=TWPI
C     SET IEC TO THE ECLIPSING STAR   
      IF ((PSIP.LE.(PHTS1+1.D-13)).OR.(PSIP.GT.(PHTS2+1.D-13))) 
     $IEC=3-ISV
      IF ((PSIP.GT.(PHTS1+1.D-13)).AND.(PSIP.LE.(PHTS2+1.D-13))) 
     $IEC=ISV
C     Set control for quadrant in which PSIP is located
      IF ((PSIP.GE.PHTS4).AND.(PSIP.LE.PHTS1)) ISEQ=1
      IF ((PSIP.GT.PHTS1).AND.(PSIP.LE.PHTS3)) ISEQ=2
      IF ((PSIP.GT.PHTS3).AND.(PSIP.LE.PHTS2)) ISEQ=3
      IF ((PSIP.GT.PHTS2).AND.(PSIP.LT.PHTS4+TWPI)) ISEQ=4
C     Handle rounding error for PSIP
      IF ((PSIP.GE.TWPI).AND.(PSIP.LT.TWPI+1.D-12))THEN
		PSIP=TWPI
		ISEQ=4
      END IF
      IF (PSIP.GE.TWPI+1.D-12) CALL PMDSTOP
C    Set correct values of Q and "I"
C    Note: "I" currently points to the component eclipsed at phase 0.0
C    and set equivalent to ISV earlier in this program. This is the star
C    eclipsed at orbital phase 0.0.
C    In the following program section set IEC as the eclipsing star, whatever
C    the orbital phase.
C    This value of I is associated with mass ratio QS.
      IF ((ISEQ.EQ.1).OR.(ISEQ.EQ.4)) THEN
      	Q=QS
        IEC=3-ISV
      END IF
      IF ((ISEQ.EQ.2).OR.(ISEQ.EQ.3)) THEN
C     For this orbital phase range the star eclipsed at orbital phase 0.0
C     becomes the eclipsing star.
     	I=3-I
      	Q=QN
      	IEC=ISV
      END IF	
C     Calculate the mean anomaly of the point
      VMNAN=PSIP+VMN0
      IF (VMNAN.LT.0.D0) VMNAN=VMNAN+TWPI
      IF (VMNAN.GT.TWPI) VMNAN=VMNAN-TWPI
C     Calculate eccentric anomaly, true anomaly, and orbital position.
      IF (ECC.NE.0.D0) THEN
         CALL VKEP(1.D0,ECC,VMNAN,1.D-9,VECAN,VTRUE,VRD,20,NITER,KER)
         IF (KER.NE.0) CALL PMDSTOP
      ELSE
         VRD=1.D0
         VECAN=PSIP
         VTRUE=PSIP
      END IF
C     Calculate the orbital position angle
      PSI=VTRUE+OMRAD-PIO2
      IF (PSI.LT.0.D0) PSI=PSI+TWPI
      IF (PSI.GT.TWPI) PSI=PSI-TWPI
C     Save this position angle
      PSISV=PSI
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
C     SET TARY FOR STAR I      
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
C     Note: TARY is the matrix to convert from spatial 3D coordinates
C     to plane of sky coordinates. The spatial coods. have the X axis
C     pointing from the star at (0,0,0) at phase 0.0 toward the companion,
C     the Y axis in the orbital plane in the direction of motion, and
C     the Z axis perpendicular to the orbital plane and in the direction
C     to produce a R.H. cood. system. In the projection on the plane of the 
C     sky, the +X axis point "down" as seen by the observer, the +Y axis
C     points to the right as seen by the observer, and the +Z axis points
C     out of the plane of the sky toward the observer. For plotting convenience
C     the eclipsing star is assumed to move from right to left although in
C     a R.H. cood. system the observed eclipsing star moves from left to right.
C     This choice explains the use of variable CM=TWPI-PSI.
C     Calculate separation of centers
      CALL PROJ(TARY,VRD,0.D0,0.D0,XG,YG,ZG)
C  
C     THE COORDINATES OF THE CENTER OF THE SECONDARY STAR,STAR (3-I),    
C     RELATIVE TO THE ORIGIN AT THE CENTER OF THE PRIMARY STAR,STAR (I), 
C     ARE XG,YG  
C
C     CALCULATE GEOMETRIC DEPTH OF ECLIPSE   
      P=(1.D0-ECC**2)*DSQRT(VM0*VM0+VN0*VN0)/(1.D0+ECC*DCOS(VTRUE))  
      P=(P-RB)/RA
      SP=P
      WRITE (NOUT1,107)
      WRITE (NOUT1,105) KS  
      WRITE (NOUT1,117) PSI,VMNAN,VECAN,VTRUE,VRD,PSIP
      WRITE (NOUT1,121) SP
      WRITE (NOUT1,123) XG,YG
C     FILL COSINE GAMMA ARRAY FOR FIRST COMPONENT   
      DO 11 ITH=1,NTH
      N=NPH(I,ITH)      
      DO 1 K=1,N    
      CALL SYN(I,ITH,K,CSG(I,ITH,K))
    1 CONTINUE
   11 CONTINUE
	DO ITH=1,NTH
	NPHV=NPH(I,ITH)
	WRITE (NOUT7,1002)I,ITH
	WRITE (NOUT7,1000) (CSG(I,ITH,K),K=1,NPHV)
	END DO
      CALL VSBL(I,NTH,NOUT7)
      write (nout7,1002) i
	WRITE (NOUT7,1000) VR(I,1,1),VR(3-I,1,1)
	DO ITH=1,NTH
	WRITE (NOUT7,1002) ITH
	WRITE (NOUT7,1000) VST(I,ITH,1),VND(I,ITH,1),VST(I,ITH,2),
     $VND(I,ITH,2),VST(I,ITH,3),VND(I,ITH,3)
	END DO
	WRITE (NOUT7,1001)
	WRITE (NOUT7,1001)
	WRITE (NOUT7,1001)
C     NOW DO SECOND COMPONENT  
      I=3-I
      IEC=3-IEC
      IF (DABS(Q-QS).LT.1.D-13) THEN     
      	Q=QN
      ELSE
      	Q=QS
      END IF
C     Note that PSI is an orbital position angle
      PSI=PSI+PI 
      IF (PSI.GT.TWPI) PSI=PSI-TWPI   
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
C     SET TARY FOR STAR 3-I    
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)     
C     FILL COSINE GAMMA ARRAY,SECOND COMPONENT      
      DO 21 ITH=1,NTH   
      N=NPH(I,ITH)      
      DO 2 K=1,N    
      CALL SYN(I,ITH,K,CSG(I,ITH,K))  
    2 CONTINUE   
   21 CONTINUE   
	DO ITH=1,NTH
	NPHV=NPH(I,ITH)
	WRITE (NOUT7,1002)I,ITH
	WRITE (NOUT7,1000) (CSG(I,ITH,K),K=1,NPHV)
	END DO
      CALL VSBL(I,NTH,NOUT7)
      write (nout7,1002) i
	WRITE (NOUT7,1000) VR(I,1,1),VR(3-I,1,1)
	DO ITH=1,NTH
	WRITE (NOUT7,1002) ITH
	WRITE (NOUT7,1000) VST(I,ITH,1),VND(I,ITH,1),VST(I,ITH,2),
     $VND(I,ITH,2),VST(I,ITH,3),VND(I,ITH,3)
	END DO
	WRITE (NOUT7,1001)
	WRITE (NOUT7,1001)
	WRITE (NOUT7,1001)
 1000 FORMAT (6F12.8)
 1001 FORMAT (' ')
 1002 FORMAT (10I5)
C 1003 FORMAT ('***********************************')
C     RESET INITIAL CONDITIONS
      I=3-I
      IEC=3-IEC
      IF (DABS(Q-QS).LT.1.D-13) THEN     
      	Q=QN
      ELSE
      	Q=QS
      END IF
      PSI=PSISV
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)     
C     SET TARY FOR STAR I
C     Calculate the fractional distance along the major axis from the
C     star at (0,0,0) to the system center of mass.      
      XC=QS/(1.D0+QS)
      IF (IEC.EQ.ISV) XC=QN/(1.D0+QN)
C     Calculate XCMS,YCMS,ZCMS, the plane of the sky coods. of the center of mass.
      CALL PROJ(TARY,XC*VRD,0.D0,0.D0,XCMS,YCMS,ZCM)
C     XCMS, YCMS, ZCM are 3D coordinates of the center of mass relative to
C     an origin of (0,0,0) at the center of star ISV unless IEC.EQ.ISV.
C     Always place the origin of the eclipsed star at (0,0).
      IF (IEC.EQ.ISV) XCMS=XCMS-XG
      IF (IEC.EQ.ISV) YCMS=YCMS-YG
      YCM=XCMS
      XCM=-YCMS
C      CALL VSBL(I,NTH,NOUT7)
      IF (I.EQ.IEC)ICOUNT=1
	IF (I.EQ.3-IEC) ICOUNT=2
	CALL SHAD1(I,NTH,TARY,XBGEQ,YBGEQ,XNDEQ,YNDEQ,ISEQ,NPB,NPE,
     $NKP,A,B,KYA,KYB,ISV,IEC,IRD,ICOUNT,VRD,NOUT7)
C     Note that ISV always denotes the star eclipsed at orbital phase 0.0.
C     At exit from SHAD1, the projected center of star I is at (0,0) if
C     star I.EQ.ISV and is at (XG,YG) if star I.EQ.3-ISV.
C     At this point, star I denotes ISV.
C     Write output with origin at center of star ISV
	DO JJ=1,NPB
		WRITE (NOUT10,1000) XHZB(JJ),YHZB(JJ)
	END DO
	DO JJ=1,NPE
		WRITE (NOUT10,1000) XHZE(JJ),YHZE(JJ)
	END DO
C     Transfer origin to center of mass	DO JJ=1,NPB
        DO JJ=1,NPB
		WRITE (NOUT8,1000) XHZB(JJ)-XCM,-YHZB(JJ)+YCM
	END DO
	DO JJ=1,NPE
		WRITE (NOUT8,1000) XHZE(JJ)-XCM,-YHZE(JJ)+YCM
	END DO
C     Calculate polar coordinate representation of the horizon
C     of the eclipsing star
      CALL HORZ(I,IRC,IFC,NPB,NPE,NKP,
     $I,XBGEQ,YBGEQ,
     $XNDEQ,YNDEQ,IEC,XG,YG,ISV,IRD,1,KCOR,NOUT7)
C     Now calculate horizon points for the other star, I.NE.ISV
      I=3-I
      IEC=3-IEC
      IF (DABS(Q-QS).LT.1.D-13) THEN     
      	Q=QN
      ELSE
      	Q=QS
      END IF
      PSI=PSISV+PI
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
C     SET TARY FOR STAR I      
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
      CALL PROJ(TARY,XC*VRD,0.D0,0.D0,XCMS,YCMS,ZCM)
C     XCMS, YCMS, ZCM are 3D coordinates of the center of mass relative to
C     an origin of (0,0,0) at the center of star ISV unless IEC.EQ.ISV.
C     Always place the origin of the eclipsed star at (0,0).
C      IF (IEC.EQ.ISV) XCMS=XCMS-XG
C      IF (IEC.EQ.ISV) YCMS=YCMS-YG
C      YCM=XCMS
C      XCM=-YCMS
       IF (I.EQ.IEC)ICOUNT=1
	IF (I.EQ.3-IEC) ICOUNT=2
	CALL SHAD1(I,NTH,TARY,XBGEQ,YBGEQ,XNDEQ,YNDEQ,ISEQ,NPB,NPE,
     $NKP,A,B,KYA,KYB,ISV,IEC,IRD,ICOUNT,VRD,NOUT7)
C     Write output with origin at center of other star
	DO JJ=1,NPB
		WRITE (NOUT11,1000) XHZB(JJ),YHZB(JJ)
	END DO
	DO JJ=1,NPE
		WRITE (NOUT11,1000) XHZE(JJ),YHZE(JJ)
	END DO
C      Correct for displacement of origin to (XG,YG) within SHAD1
C	DO JJ=1,NPB
C		XHZB(JJ)=XHZB(JJ)-XG
C		YHZB(JJ)=-YHZB(JJ)+YG
C	END DO
C	DO JJ=1,NPE
C		XHZE(JJ)=XHZE(JJ)-XG
C		YHZE(JJ)=-YHZE(JJ)+YG
C	END DO
C     Transfer origin to center of mass
	DO JJ=1,NPB
C		XHZB(JJ)=XHZB(JJ)-XCM
C		YHZB(JJ)=YHZB(JJ)-YCM
		WRITE (NOUT9,1000) XHZB(JJ)-XCM,-YHZB(JJ)+YCM
	END DO
	DO JJ=1,NPE
C		XHZE(JJ)=XHZE(JJ)-XCM
C		YHZE(JJ)=YHZE(JJ)-YCM
		WRITE (NOUT9,1000) XHZE(JJ)-XCM,-YHZE(JJ)+YCM
	END DO
C      Switch back to the first star
      I=3-I
      IEC=3-IEC
      IF (DABS(Q-QN).LT.1.D-13) THEN      
      	Q=QS
      ELSE
      	Q=QN
      END IF
      PSI=PSISV
      CM=TWPI-PSI
      IF (CM.LT.0.D0) CM=CM+TWPI
C     SET TARY FOR STAR I      
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
	write (nout7,1002) I,ISV,IEC
C     NOW TEST FOR ECLIPSES,ETC.      
C  
C     NOTE THAT FROM 'HERE' THROUGH FIRST SHAD2,COMPONENT ISV REFERS TO THE 
C     ECLIPSING STAR
C     STAR ISV IS THE COMPONENT ECLIPSED AT PRIMARY MINIMUM
C  
	IF ((I.NE.ISV).AND.(I.NE.IEC)) THEN
	WRITE (NOUT7,1002) 1223
	WRITE (NOUT7,1002) I,ISV,IEC
	WRITE (NOUT7,1003) KS,PSIP
 1003 FORMAT(I5,4F15.12)
C     Switch to the other star to point to the eclipsed star
C         I=3-I
C	IF (Q.EQ.QS) THEN
C		Q=QN      
C        ELSE
C		Q=QS
C	END IF
C         PSI=PSI+PI
C	IF (PSI.GT.TWPI) PSI=PSI-TWPI 
C     SET TARY FOR STAR 3-I    
C         CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
C         CALL VSBL(I,NTH,NOUT7)
C	   WRITE (NOUT7,1002) I
C     The quantity SP is the geometric depth of eclipse.
	IF (SP.LT.1.D0)	THEN
         CALL SHAD2(I,NTH,XG,YG,QS,QN,TARY,CSI,SNI,IRC,IFC,ISEQ,
     $   NPB,NPE,NKP,IEC,
     $   ISV,VRD,KCOR,NOUT7)
	DO ITH=1,NTH
	WRITE (NOUT7,1002) ITH
	WRITE (NOUT7,1000) SHS(ITH,1),SHN(ITH,1),SHS(ITH,2),SHN(ITH,2),
     $                   SHS(ITH,3),SHN(ITH,3)
	END DO
	END IF
      GO TO 16
C     NOW RESET I TO I=ISV
C     NOTE THAT FROM HERE,I WILL REFER TO THE ECLIPSED
C     STAR AFTER THE NEXT PROGRAM LINE
      ELSE
	WRITE (NOUT7,1002) 1248
	WRITE (NOUT7,1002) I,ISV,IEC
	WRITE (NOUT7,1003) KS,PSIP
C     The quantity SP is the geometric depth of eclipse.
	IF (SP.LT.1.D0) THEN
         CALL SHAD2(I,NTH,XG,YG,QS,QN,TARY,CSI,SNI,IRC,IFC,ISEQ,
     $   NPB,NPE,NKP,IEC,
     $   ISV,VRD,KCOR,NOUT7)
	DO ITH=1,NTH
	WRITE (NOUT7,1002) ITH
	WRITE (NOUT7,1000) SHS(ITH,1),SHN(ITH,1),SHS(ITH,2),SHN(ITH,2),
     $                   SHS(ITH,3),SHN(ITH,3)
	END DO
	END IF
      END IF
   16 NT=NTL(I,NTH)     
      NV=NTL(3-I,NTH)   
      WRITE (NOUT1,107)     
      CALL WTOUT2(NT,NV,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,NOUT2)
   20 CONTINUE   
      WRITE (NOUT3,106) KCTRL,NPSI
      WRITE (NOUT3,108) (VLPSI(K),K=1,NPSI)      
      WRITE (NOUT3,108) VLVI
      WRITE (NOUT6,106) NPSI,N1,N2,N3,N4,N5,N6
      WRITE (NOUT6,127) (VLPSI(K),K=1,NPSI)
      WRITE (NOUT6,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,
     $NRY9,NRY10,NRY11,NRY12
      WRITE (NOUT6,106) (LV(K),K=1,13)
      WRITE (NOUT6,127) OMRAD,ECC,SMA,TH0,VCONJ,VMN0,PHZERO,THFCP,
     $THSCP,THTCP,THLCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,
     $THTCS,THLCS,THFOUR,PHFCP,PHSCP,PHTCP,PHLCP,PHTHREE,PHFCS,
     $PHSCS,PHTCS,PHLCS,PHFOUR
  88  CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
      WRITE (NOUT1, 255) TM
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=IN3,   STATUS='KEEP')
      CLOSE (UNIT=IN4,   STATUS='KEEP')
      CLOSE (UNIT=IN5,   STATUS='KEEP')
      CLOSE (UNIT=IN6,   STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      CLOSE (UNIT=NOUT6, STATUS='KEEP')
      CLOSE (UNIT=NOUT7, STATUS='KEEP')
      CLOSE (UNIT=NOUT8, STATUS='KEEP')
      CLOSE (UNIT=NOUT9, STATUS='KEEP')
      CLOSE (UNIT=NOUT10, STATUS='KEEP')
      CLOSE (UNIT=NOUT11, STATUS='KEEP')
      OPEN (UNIT=IN1, FILE = FL1(1), STATUS = 'OLD')
         WRITE (IN1,101) KEYGOOD
      CLOSE (UNIT=IN1, STATUS = 'KEEP')
      STOP 'PGB: Completed successfully.'
   60 WRITE (NOUT1,122) VI  
      CALL PMDSTOP
      END  PROGRAM PGB
