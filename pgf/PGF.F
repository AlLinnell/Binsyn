      PROGRAM PGF
	USE MSFLIB
C     PROGRAM TO GENERATE DATA SET CONTAINING PHASE VALUES FOR
C     INPUT TO PROGRAM SPT. THE OUTPUT OF SPT THEN CONTAINS SYNTHETIC
C     LIGHT VALUES AT THE INPUT PHASE VALUES.
C     THE OUTPUT OF PROGRAM PGF IS IN THE FORMAT OF THE OUTPUT OF
C     PROGRAM SPT, BUT WITH ZERO FOR THE SYSTEM LIGHT VALUE
C  
C     KCTRL IS A CONTROL INTEGER TO DECIDE ON THE FORM OF OUTPUT   
C     IF KCTRL.NE.0, CALCULATE THE CONTACT PHASES AND USE INTEGERS     
C     N1,N2,N3,N4,N5, AND N6 TO DISTRIBUTE POINTS.
C     IF KCTRL=0,INPUT THE INTEGER NPSI AND THE TWO PHASE LIMITS 
C     PLT1 AND PLT2. DISTRIBUTE THE NPSI POINTS UNIFORMLY SPACED
C     BETWEEN PLT1 AND PLT2.
C     
C N1=NUMBER OF POINTS BETWEEN PHASE 0.0 AND THIRD
C     CONTACT, PRI. MIN.
C N2=NUMBER OF POINTS FROM THIRD CONTACT TO
C     FOURTH CONTACT
C N3=NUMBER OF POINTS FROM FOURTH CONTACT TO MAX. ELONGATION
C
C N4=NUMBER OF POINTS FROM MID-SEC. MIN. TO THIRD CONTACT,
C     SECONDARY MIN.
C N5=NUMBER OF POINTS FROM THIRD TO FOURTH CONTACT, SECONDARY MIN.
C
C N6=NUMBER OF POINTS FROM FOURTH CONTACT TO MAX. ELONGATION, FOLL.
C     SECONDARY MINIMUM
C     THE SEQUENCE IS REPEATED SYMETTRICALLY ABOUT THE RESPECTIVE
C     MINIMA
C
C     KN IS A CONTROL FOR PRODUCTION OF MULTIPLE WAVELENGTH DATA.
C     IF KN.NE.0, WRITE OUTPUT IN FORMAT OF COMPLETE LIGHT CURVE PHASES,
C     REPEATED NCOL TIMES.
C     IF KN.EQ.0, WRITE OUTPUT IN FORMAT OF NCOL OBSERVATIONS IN
C     IMMEDIATE SUCESSION, REPEATED FOR ALL ORBITAL PHASES.
C
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMN.FOR'
      DIMENSION VLPSI(MOBS),LV(13)
      DIMENSION FL1(3)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 80 LBL
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      INTEGER(4) RESULT
      DATA IN1,IN2,NOUT1,NOUT2/1,2,3,4/
      DATA PHZERO/0.D0/
      DATA ZR/0.D0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  102 FORMAT (1X,4F16.13)
  103 FORMAT (1H1)
  104 FORMAT (1X,'PROGRAM PGF',10X,'DATE='I2,'/',I2,'/'I4,2X,'TIME=',I2,
     $':',I2,':',I2,'.',I2)
  107 FORMAT (1H )
  109 FORMAT (1X,'Control integer=',I5,/,' If the control integer=1,Calc
     $ulate the orbital phase values')
  112 FORMAT (1X,'Orbital inclination=',F20.12, 2X, 'degrees')
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
     $,2X,I5)
  116 FORMAT (1X,'Revised numbers of points in critical phase ranges=',
     $/,1X,6I7,/,1X,'Total number of orbital longitudes=', I5)
  126 FORMAT (1X,'Values of NRY1,NRY2,...',/,1X,12I5)
  128 FORMAT (1X,'Values of LV(1),LV(2),...,LV(13)',/,1X,13I5)
  130 FORMAT (1X, 'Orbital eccentricity=',5X,F10.8)
  131 FORMAT (1X,'Longitude of periastron=',F12.8,1X,'degrees')
  132 FORMAT (1X,'Semi-major axis of relative orbit=',F15.8,1X,
     $'Solar radii')
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
  205 FORMAT (64X,I5)
  255 FORMAT (1X,'Program running time=',F12.4,2X,'minutes')
C  256 FORMAT ( 1X,'Current orbital position angle=', F20.12)
  257 FORMAT (2D20.7)
  258 FORMAT (2D20.7,F5.2)
  260 FORMAT (A80)
C
      NMAX=10000              !Max. number of phase values
C*********************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=4,     FILE='C:\LGTSYN\PGF\PGF.ILS', STATUS='OLD')
      READ (4, 100)     (FL1(J),J=1,3)
      OPEN (UNIT=IN1,   FILE=FL1(1), STATUS='OLD')         !PGF.INF
      OPEN (UNIT=NOUT1, FILE=FL1(2), STATUS='REPLACE')         !PGFF1.DAT
      OPEN (UNIT=NOUT2, FILE=FL1(3), STATUS='REPLACE')         !PGFF2.DAT
C********************************************************
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F1)
      WRITE (NOUT1,103)     
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
C
      LABEL = ' ***PGF'
      LBLERR=0
      NU=IN1
      CALL GTLBL (LABEL, LBLERR)
      IF ( LBLERR.EQ.(-1)) CALL PMDSTOP
C
      READ (IN1,260) LBL               !header for output file
      CALL IREAD (KCTRL)
      CALL DUMMY(5)
      CALL IREAD (KN)
      CALL DUMMY (4)
      CALL IREAD (NCOL)
      CALL DUMMY (2)
      IF (KCTRL.EQ.0) GO TO 20
C
      CALL DREAD (VLVID, 'F11.8')
      CALL DUMMY (1)
      CALL DREAD (ECC, 'F10.8')
      CALL DUMMY (1)
      CALL DREAD (OMD, 'F10.8')
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
      CALL DREAD (RG, 'F6.3')
      CALL DUMMY (2)
      CALL DREAD (RSV, 'F6.3')
      CALL DUMMY (2)
C     Convert inclination and longitude of periastron from 
C     degrees to radians
      VLVI=VLVID/RAD
      OMRAD=OMD/RAD
      GO TO 21
   20 CALL DUMMY(25)
      READ (1,205) NPSI
      IF (NPSI.GT.NMAX) CALL PMDSTOP
      CALL DUMMY (2)
      CALL DREAD (PLT1, 'F8.5')
      CALL DUMMY (2)
      CALL DREAD (PLT2, 'F8.5')
C
C     ENTER CONTROL FOR SRT'S TO CALL
   21 WRITE (NOUT1,109) KCTRL 
      IF (KCTRL.EQ.0) GO TO 1
      WRITE (NOUT1,112) VLVID
      WRITE (NOUT1,130) ECC
      WRITE (NOUT1,131) OMD
C     
      NPSI=2*(N1+N2+N3+N4+N5+N6)
      WRITE (NOUT1,114) N1,N2,N3,N4,N5,N6,NPSI
C     NOTE THAT PHSEC IS THE FRACTION OF THE CYCLE AT WHICH MID-SECOND.  
C     ECLIPSE OCCURS.IT IS NOT AN ANGLE.     
    3 VI=VLVI 
C  
C  
C     NOTE THAT THE USE OF CSI IS IN CALCULATING THE LINE OF SIGHT FROM  
C     THE STAR   
C  
C  
      CSI=-DCOS(VI)      
      SNI=DSIN(VI)
      VLMTH=DABS(CSI/SNI)
      COSOM=DCOS(OMRAD)
      SIGN=1.D0
      SIGNV=DSIGN(SIGN,COSOM)
      VLMTH=VLMTH*SIGNV
C     Calculate TH0, orbital position angle of mid-pri. min.(radians)
      IF (VLMTH.GE.1.D-5) THEN
         CALL RTSAFE(-VLMTH, VLMTH, ECC, OMRAD, VI, 1.D-9, TH0)
      ELSE
         VAR1=DSIN(OMRAD)
         TH0=-ECC*COSOM*VLMTH**2/(1.D0+ECC*VAR1)
      END IF
      IF (TH0.LT.0.D0) TH0=TH0+TWPI
      IF (TH0.GE.TWPI) TH0=TH0-TWPI
C     Calculate corres. theta for secondary min. (Kopal, eq. 9-20)
      THTWO=PI-TH0
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
C     Calculate orbital phase of mid-secondary. This is the time
C     of mid-sec. as a fraction of the period, times two pi.
C     The time of mid-primary min, in units of TWPI/Period, is PHZERO
      PHSEC=VMN2-VMN0+PHZERO
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
            CALL RTSAFE(VLTH1,VLTH2,ECC,OMRAD,VI,1.D-9,THTHREE)
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
            CALL RTSAFE(VLTH1,VLTH2,ECC,OMRAD,VI,1.D-9,THFOUR)
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
      CALL CONTACT(VI,RSV,RG,1.D0,OMRAD,ECC,VCONJ,VSEC,THFCP,THSCP,
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
      IF ((PHTHREE.EQ.0.D0).AND.(PHFOUR.EQ.0.D0)) THEN
         PHTS1=PIO2
         PHTS2=TPIO2
      ELSE
         PHTS1=PHTHREE
         PHTS2=PHFOUR
      END IF
      IF (PHSEC.EQ.0.D0) THEN
         PHTS3=PI
         PHTS4=0.D0
      ELSE
         PHTS3=PHSEC
         PHTS4=PHZERO
      END IF
C
      IF (N1.LE.0) CALL PMDSTOP
      NA=N1+1
      VLPSI(1)=PHZERO
      LV(1)=1
      NPSI=1
      V=N1
      IF (DABS(THTCP-TH0).LT.1.D-5) GO TO 201
C
C     Program SPTSHELL will need points equally spaced in orbital
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
      IF (VLPSI(K).GE.TWPI) VLPSI(K)=VLPSI(K)-TWPI
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
      IF (PHLCP.EQ.PHTCP) A=0.D0
      IF (A.EQ.0.D0) THEN
         NRY2=0
         LV(3)=NPSI
         GO TO 204
      END IF
      DO 203 K=1,N2     
      W=K 
      VLPSI(NA+K)=W/V*A+PHTCP
      IF (VLPSI(NA+K).GE.TWPI) VLPSI(NA+K)=VLPSI(NA+K)-TWPI
      NPSI=NPSI+1
  203 CONTINUE
      NRY2=N2+1
      LV(3)=NPSI
      GO TO 305
  204 N2=0
      NRY2=0
      LV(3)=NPSI
  305 IF (N3.EQ.0) THEN
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
      IF (A.EQ.0.D0) THEN
         N3=0
         NRY3=0
         LV(4)=NPSI
         GO TO 209
      END IF
  206 DO 207 K=1,N3     
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
      IF (A.EQ.0.D0) THEN
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
         IF (A.EQ.0.D0) THEN
            NRY12=0
            LV(13)=NPSI
            GO TO 228
         END IF
         DO 227 K=1,N1
         W=K
         VLPSI(NA+2*N2+2*N3+2*N4+2*N5+2*N6+K)=W/V*A+PHSCP
         NPSI=NPSI+1
  227    CONTINUE
         NRY12=N1+1
         LV(13)=NPSI
  228    CONTINUE
      END IF
   13 WRITE (NOUT1,116) N1,N2,N3,N4,N5,N6,NPSI
      WRITE (NOUT1,126) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      WRITE (NOUT1,128) (LV(K),K=1,13)
      GO TO 30
C      
    1 DPH=(PLT2-PLT1)*TWPI
      N=NPSI-1
      W=N
      DO I=1,NPSI
      V=I-1
      VLPSI(I)=V/W*DPH+PLT1*TWPI
      END DO
C
   30 DO I=1,NPSI
      VLPSI(I)=VLPSI(I)/TWPI
      END DO
      WRITE (NOUT2,260) LBL
      IF (KN.NE.0) GO TO 40
      DO K=1,NPSI
         A=0.D0
         B=VLPSI(K)
		C=1.0
         IF (B.EQ.0.D0) A=1.D-7
		DO L=1,NCOL
			WRITE (NOUT2,258) A,B,C
		END DO
      END DO
      WRITE (NOUT2,257) ZR,ZR
      GO TO 99
   40 DO L=1,NCOL
         DO K=1,NPSI
            A=0.D0
            B=VLPSI(K)
		  C=1.D0
            IF (B.EQ.0.D0) A=1.D-7
            WRITE (NOUT2,258) A,B,C
         END DO
         WRITE (NOUT2,257) ZR,ZR
      END DO
C  

   99 CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F2)
      TM=(F2-F1)*1440.D0
      WRITE (NOUT1, 255) TM
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      STOP
      END 
