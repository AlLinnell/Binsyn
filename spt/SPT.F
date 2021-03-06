      PROGRAM SPT								   
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
	DIMENSION LV(13),KCT(13),LDF(13),VH(13)
C
C     *****************************************************************
C     The following arrays have one dimension of MWL. This is the limit
C     on the number of effective wavelengths for which light curves will
C     be calculated in a single run.
C
      DIMENSION THLT(MWL),RFMG(MWL),RRS(5),VMNER(MWL),
     $SD(MWL),NCT(MWL),PHAS(MWL),CP(MWL),VLSQ(MWL),DCF(MWL)
      DIMENSION DRFMG(MWL)
      DIMENSION VMX(MWL),VMNP(MWL),VMNS(MWL),DP(MWL),DS(MWL),DDPTH(MWL)
      DIMENSION WL(MWL),CORR(MWL),SIGMA(MWL),VNSE(MWL),SUMWTI(MWL),
     $DPHS(MWL),RESID(MWL)
C
C     *****************************************************************
C     The following arrays have one dimension of 10. This is the 
C     maximum number of spots allowed on either stellar component.
C
      DIMENSION VLMD(MTW,MSPT),VGMD(MTW,MWL,MSPT)
      DIMENSION CBT(MTW,MSPT),VMXV(MWL)
      DIMENSION VK(MTW,MWL,MSPT)
      DIMENSION SNB(MTW,MSPT)
C
C	******************************************************************
C
C     The following arrays have a dimension equal to a specified limit
C     on the number of orbital longitudes calculated for the synthetic
C     light curves. If this limit is to be exceeded, reset the following
C     array dimensions and recompile the program.
C
      DIMENSION PSJ(MPHS),SP(MPHS),VCLC(MPHS),VLPSI(MPHS)
      DIMENSION VCLCA(MPHS),SMTA(MPHS),SBTA(MPHS),SMCA(MPHS),
     $SMDA(MPHS),
     $SMEA(MPHS),SMFA(MPHS),PSK(MPHS)
      DIMENSION SMT(MPHS),SBT(MPHS),SMC(MPHS),SMD(MPHS),SME(MPHS),
     $SMF(MPHS)
C     *****************************************************************
C      
      DIMENSION FL1(32)
      DIMENSION VMAG(MOBS,MWL),PHSP(MOBS,MWL),WT(MOBS,MWL)
      COMMON /CQ/FMT,FXSP,PHSV,IWL,KJP,KSP,IC,JL,CBT,NRY11,VH,LL,SNB,
     $SMC,VK,LV,SME,VCLC,MDAT,NRY2,PHAS,RFMG,PSJ,NRY5,KPT,KSQ,DPHS,
     $NRY8,SURF1,VGMD,IWR,SURF2,NSPA,NSPB,PHSEC,VLMD,VMXX,NRY1,NRY3,
     $NRY4,NRY6,NRY7,NRY9,NRY10,NRY12,LDF,VMAG,PHSP,WT
      SAVE /CQ/
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 80 FL1,BFRO
      CHARACTER * 10 DATE,TIME,ZONE
      CHARACTER * 7 LABEL
      CHARACTER FMT*80
      CHARACTER * 1 BFR
      CHARACTER * 73 HD1
      CHARACTER * 73 HD2
      CHARACTER * 73 HD3
      CHARACTER * 73 HD4
      CHARACTER * 73 HD5
      CHARACTER * 73 HD6
      CHARACTER * 73 HD7
      CHARACTER * 73 HD8
      CHARACTER * 73 HD9
      CHARACTER * 73 HD10
      CHARACTER * 73 HD11
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA TWPI/6.2831853071795864769D0/
      DATA CV/-1.08573620476D0/      
      DATA CF/57.29577951D0/
	DATA WTA/1.D0/
	DATA PHRF/0.7D0/
      DATA KFORM/1/            !LATER,READ THIS IN
      DATA ZR1,ZR2/0.D0,0.D0/
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,55,66,7/
      DATA IN8,IN9,IN10,IN11,IN12,IN13,IN14/8,9,10,11,12,13,14/
      DATA NOUT1,NOUT2,NOUT3,NOUT4/15,16,17,18/
      DATA NOUT5,NOUT6,NOUT7,NOUT8,NOUT9,NOUT10/19,20,21,22,23,24/
      DATA NOUT11,NOUT12,NOUT13,NOUT14,NOUT15/25,26,27,28,29/
      DATA NOUT16,NOUT17,NOUT18/30,31,32/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C  
C     NOTE THAT THIS PROGRAM MAY BE USED TO CONSTRUCT A SYNTHETIC LIGHT  
C     CURVE BY CATALOGING TAPE 20    
C  
C     USE PROGRAM PGF TO PRODUCE A FICTITIOUS SET OF INPUT DATA   
C  
  101 FORMAT (2F20.1)
  102 FORMAT (1A1)
  103 FORMAT (' ')
  104 FORMAT (1X,'PROGRAM SPT',10X,'DATE=',I2,'/',I2,'/',I4,
     $2X,'TIME=',I2,':',I2,':',I2,'.',I2)
  105 FORMAT (A73)
  106 FORMAT (10I5)     
  107 FORMAT (1X,'NUMBER OF WAVELENGTHS OF OBSERVATION=',I10,/,1X,
     $'NUMBER OF VALUES OF ORBITAL LONGITUDE=',I10,/,1X,
     $'SEQUENCE CONTROL FOR INPUT DATA='10X,I5)
  108 FORMAT (6F20.12)  
  109 FORMAT (2F15.8,3D20.7)   
  110 FORMAT (8D15.8)   
  111 FORMAT (8F10.7)   
  112 FORMAT (1X,'VALUES OF REFERENCE MAGNITUDES,BY ORDINAL WAVELENGTH V 
     $ALUES')
  113 FORMAT (1X,'VALUES OF THIRD LIGHT,BY ORDINAL WAVELENGTH VALUES') 
  114 FORMAT (1X,'ORBITAL INCLINATION(DEGREES)=',15X,F12.5,1X,/,1X,
     $'ORDINAL WAVELENGTH NUMBER=',19X,I7,/,1X,
     $'THIRD LIGHT=',33X,E15.5,/,1X,'REFERENCE MAGNITUDE=',26X,F10.5
     $,/,1X,'NUMBER OF OBSERVATIONS IN THIS LIGHT CURVE=',I9,/,1X,
     $'MEAN ERROR(IN LIGHT UNITS)=',15X,F15.6,/,1X,
     $'STD.DEVIATION OF LIGHT RESIDUALS=',13X,F10.5,/,1X,
     $'CORRECTION TO REFERENCE MAGNITUDE=',12X,F10.5)
  115 FORMAT (1X,'READ ERROR ON RECORD IC=',I5,5X,'IWL VALUE=',I5)
  116 FORMAT (1X,'LIGHT LEVEL AT MAXIMUM=',23X,F8.3,/,1X,
     $'LIGHT LEVEL AT PRI.MIN.=',22X,F8.3,/,1X,
     $'LIGHT LEVEL AT SEC.MIN.=',22X,F8.3,/,1X,
     $'DEPTH OF PRI.MIN.=',28X,F8.3,/,1X,'DEPTH OF SEC.MIN.=',28X,F8.3,
     $/,1X,'DIFF.OF DEPTHS,PRI.MINUS SEC.=',16X,F8.3,/)
  117 FORMAT (1X,'WAVELENGTHS OF OBSERVATION')     
  118 FORMAT (1X,'WAVELENGTH OF THIS OBSERVATION SET=',12X,F8.4)
  119 FORMAT (1H )      
  120 FORMAT (1X,'THIRD LIGHT OUT OF RANGE.THIRD LIGHT=',F10.5,2X,
     $'FOR WAVELENGTH NUMBER',I5)
  121 FORMAT (2E20.7,F5.2)   
  122 FORMAT (1X,'LONGITUDES OF SPOTS IN DEGREES,COMPONENT',3X,I5)
  123 FORMAT (1X,'ANGULAR RADII OF SPOTS IN DEGREES,COMPONENT',I5)
  124 FORMAT (1X,'LATITUDES OF SPOTS IN DEGREES,COMPONENT',4X,I5)
  125 FORMAT (1X,'TEMPERATURE CONTRAST FROM ADJACENT PHOTOSPHERE, IN KIL
     $OKELVINS,COMPONENT',I5)
  126 FORMAT (10E13.6)  
  127 FORMAT (1X,'PHASE INTERVAL FROM POINT',I5,2X,'TO',I5,2X,'IS',F10.8 
     $,2X,'WHILE STD.INTERVAL IS',F10.8)     
  128 FORMAT (1X,'HORIZONTAL DIFFERENCE TABLE FOR PHASE INTERVAL',F10.7, 
     $2X,'TO',F10.7)    
  129 FORMAT (1X,'TOO FEW TABULAR VALUES FOR DIFFERENCE TABLE',/,1X,     
     $'NUMBER OF TABULAR VALUES=',I5) 
  130 FORMAT (1X,'LIMB DARKENING WITHIN SPOTS,ORDINAL WAVELENGTH=',I5,
     $2X,'COMPONENT',I5)
  131 FORMAT (2F8.2)
  132 FORMAT (1X,'PHASE OF MIDSECONDARY MINIMUM,BY ORDINAL WAVELENGTH')
  133 FORMAT (1X,10F12.5)
  134 FORMAT (1X,'OLD REFERENCE MAGNITUDE=',17X,F15.5,/,1X,'CORRECTION=',
     $30X,F15.5,/,1X,'NEW REFERENCE MAGNITUDE=',17X,F15.5)
  135 FORMAT (8F10.7)
  138 FORMAT (I5,2F12.8,2D18.7)
  140 FORMAT (A36)
  141 FORMAT (A80)
  201 FORMAT (I2)
  205 FORMAT (1X,'Program running time=',F14.4,2X,'seconds')
  300 FORMAT (7F18.13)
C**********************************************************************
      OPEN (UNIT=33,      FILE='SPT.ILS',STATUS='OLD')
      READ (33, 140)      (FL1(J),J=1,32)
      OPEN (UNIT=IN1,    FILE=FL1(1),   STATUS='OLD')
      WRITE ( IN1, 201) KEYBAD
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      OPEN (UNIT=IN2,    FILE=FL1(2),   STATUS='OLD')
      OPEN (UNIT=IN3,    FILE=FL1(3),   STATUS='OLD')
      OPEN (UNIT=IN4,    FILE=FL1(4),   STATUS='OLD')
      OPEN (UNIT=IN5,    FILE=FL1(5),   STATUS='OLD')
      OPEN (UNIT=IN6,    FILE=FL1(6),   STATUS='OLD')
      OPEN (UNIT=IN7,    FILE=FL1(7),   STATUS='OLD')
      OPEN (UNIT=IN8,    FILE=FL1(8),   STATUS='OLD')
      OPEN (UNIT=IN9,    FILE=FL1(9),   STATUS='OLD')
      OPEN (UNIT=IN10,   FILE=FL1(10),  STATUS='OLD')
      OPEN (UNIT=IN11,   FILE=FL1(11),  STATUS='OLD')
      OPEN (UNIT=IN12,   FILE=FL1(12),  STATUS='OLD')
      OPEN (UNIT=IN13,   FILE=FL1(13),  STATUS='OLD')
      OPEN (UNIT=IN14,   FILE=FL1(14),  STATUS='OLD')
      OPEN (UNIT=NOUT1,  FILE=FL1(15),  STATUS='REPLACE')
      OPEN (UNIT=NOUT2,  FILE=FL1(16),  STATUS='REPLACE')
      OPEN (UNIT=NOUT3,  FILE=FL1(17),  STATUS='REPLACE')
      OPEN (UNIT=NOUT4,  FILE=FL1(18),  STATUS='REPLACE')
      OPEN (UNIT=NOUT5,  FILE=FL1(19),  STATUS='REPLACE')
      OPEN (UNIT=NOUT6,  FILE=FL1(20),  STATUS='REPLACE')
      OPEN (UNIT=NOUT7,  FILE=FL1(21),  STATUS='REPLACE')
      OPEN (UNIT=NOUT8,  FILE=FL1(22),  STATUS='REPLACE')
      OPEN (UNIT=NOUT9,  FILE=FL1(23),  STATUS='REPLACE')
      OPEN (UNIT=NOUT10, FILE=FL1(24),  STATUS='REPLACE')
      OPEN (UNIT=NOUT11, FILE=FL1(25),  STATUS='REPLACE')
      OPEN (UNIT=NOUT12, FILE=FL1(26),  STATUS='REPLACE')
      OPEN (UNIT=NOUT13, FILE=FL1(27),  STATUS='REPLACE')
      OPEN (UNIT=NOUT14, FILE=FL1(28),  STATUS='REPLACE')
      OPEN (UNIT=NOUT15, FILE=FL1(29),  STATUS='REPLACE')
      OPEN (UNIT=NOUT16, FILE=FL1(30),  STATUS='REPLACE')
      OPEN (UNIT=NOUT17, FILE=FL1(31),  STATUS='REPLACE')
      OPEN (UNIT=NOUT18, FILE=FL1(32),  STATUS='REPLACE')
C**********************************************************************
C     Read input data
      LABEL = ' ***SPT'
      NU=IN2
      LBLERR=0
      CALL GTLBL ( LABEL, LBLERR)
      IF ( LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD ( KSQ)
      CALL DUMMY (1)
      CALL IREAD ( KPT)
      CALL DUMMY (1)
      CALL IREAD ( IWLS)
      CALL DUMMY (1)
      CALL IREAD ( IWLN)
      CALL DUMMY (1)
      CALL CREAD ( FMT)
C********************************************************************
      HD1= ' IC        PHSP        OBSL           VLIGHT         RESIDUA  
     $L (old ref.) '
      HD2= ' PHSP,OBSL,...(new ref.)  
     $           '
      HD3= ' VLIGHT,PHSP              
     $           '
      HD4= ' OMCLC,PHSP               
     $           '
      HD5= ' system light,incl.3rd lt.
     $           '
      HD6= ' system light,no eclipses 
     $           '
      HD7= ' eclipses light loss      
     $           '
      HD8= ' monochrom. flux,comp.I   
     $           '
      HD9= ' light loss,comp.I        
     $           '
      HD10=' monochrom. flux,comp.3-I 
     $           '
      HD11=' light loss,comp.3-I      
     $           '
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
      WRITE (NOUT2,105) HD1
      WRITE (NOUT3,105) HD2
      WRITE (NOUT4,105) HD3
      WRITE (NOUT5,105) HD4
      WRITE (NOUT8,105) HD5
      WRITE (NOUT9,105) HD6
      WRITE (NOUT10,105) HD7
      WRITE (NOUT11,105) HD8
      WRITE (NOUT12,105) HD9
      WRITE (NOUT13,105) HD10
      WRITE (NOUT14,105) HD11
C     IF KSQ.EQ.0,THE INPUT DATA ARE IN THE SEQUENCE MAGNITUDE,PHASE     
C     IF KSQ.NE.0,THE INPUT DATA ARE IN THE SEQUENCE PHASE,MAGNITUDE     
C     IF KPT=0,DO NOT PRINT DIFFERENCE TABLES
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN3)
C     IF KSP=1,THE COMPONENT IS I
C     IF KSP=2,THE COMPONENT IS 3-I
   33 READ (IN4,106) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN4,300) (VLPSI(K),K=1,NPSI)
      READ (IN4,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
      READ (IN4,106) (LV(K),K=1,13)
      READ (IN4,300) OMRAD,ECC,SMA,TH0,VCONJ,VMN0,PHZERO,THFCP,
     $THSCP,THTCP,THLCP,THTHREE,THTWO,VSEC,VMN2,PHSEC,THFCS,THSCS,
     $THTCS,THLCS,THFOUR,PHFCP,PHSCP,PHTCP,PHLCP,PHTHREE,PHFCS,
     $PHSCS,PHTCS,PHLCS,PHFOUR
    2 CALL RDWT3(I,NTH,NWL,WL,CA,CA,CA,CA,CA,CA,CA,CA,CA,   
     $CA,CA,CA,CA,CA,IN5)
      IF (IWLS.LT.1) IWLS=1    
      IF (IWLN.EQ.0) IWLN=NWL  
      IF (IWLN.GT.NWL) IWLN=NWL
      IF (IWLS.GT.IWLN) IWLN=IWLS     
    3 WRITE (NOUT1,107) NWL,NPSI,KSQ  
      WRITE (NOUT7,106) NWL
      WRITE (NOUT16,106) NWL
C*********************************************************************
C     Read reference magnitudes
      READ (IN9,106,END=399,ERR=299) KWL
      IF (KWL.NE.NWL) CALL PMDSTOP
      READ (IN9,111,END=399,ERR=299) (RFMG(K),K=1,NWL)
C     Read values of third light
      READ (IN10,106,END=399,ERR=299) KWL
      IF (KWL.NE.NWL) CALL PMDSTOP
      READ (IN10,111,END=399,ERR=299) (THLT(K),K=1,NWL)
C     Read phase displacement of entire light curve
      READ (IN14,106,END=399,ERR=299) KWL
      IF (KWL.NE.NWL) CALL PMDSTOP
      READ (IN14,111,END=399,ERR=299) (DPHS(K),K=1,NWL)
C     Read phases of midsecondary light
      READ (IN11,106,END=399,ERR=299) KWL
      IF (KWL.NE.NWL) CALL PMDSTOP
      READ (IN11,111,END=399,ERR=299) (PHAS(K),K=1,NWL)
C     Read values of SIGMA
      READ (IN12,106,END=399,ERR=299) KWL
      IF (KWL.NE.NWL) CALL PMDSTOP
      READ (IN12,111,END=399,ERR=299) (SIGMA(K),K=1,NWL)
C     Read values of VNSE
      READ (IN13,106,END=399,ERR=299) KWL
      IF (KWL.NE.NWL) CALL PMDSTOP
      READ (IN13,111,END=399,ERR=299) (VNSE(K),K=1,NWL)
      WRITE (NOUT1,112)  
      WRITE (NOUT1,133) (RFMG(K),K=1,NWL)
      WRITE (NOUT1,113) 
      WRITE (NOUT1,133) (THLT(K),K=1,NWL)
      WRITE (NOUT1,132)
      WRITE (NOUT1,133) (PHAS(K),K=1,NWL)
    4 WRITE (NOUT1,117)      
      WRITE (NOUT1,133) (WL(K),K=IWLS,IWLN)
C
      NLT=NWL
      IF (NLT.GT.5) NLT=5
C     NOW INPUT DATA FOR OTHER COMPONENT
C
C     RDWT2 INPUTS VIA TAPE7
C     SRT RDWT2 INPUTS DATA FROM PROGRAM PGB
      CALL RDWT2(NT,NU,NTH,I,IEC,PP,OMRAD,ECC,VMNAN,VECAN,VTRUE,VRD,
     $PSIP,KCTRL,KFORM,IN6)
      SI=DSIN(VI)
      CI=DCOS(VI)
      REWIND IN6
C     Read initial identification record for photometric observations
  501 READ (IN7,141,ERR=301,END=301) BFRO
C
C     Determine number of observations per spectral interval.
C
      DO 86 IWL=1,NWL
      NCT(IWL)=0
      DO 80 IC=1,100000
      READ (IN7,FMT,ERR=301,END=81) AAA, BBB, WTB
	WT(IC,IWL)=WTB
      IF ((AAA.EQ.0.D0).AND.(BBB.EQ.0.D0)) GO TO 85
      IF (KSQ.EQ.0) THEN
         VMAG(IC,IWL)=AAA
         PHSP(IC,IWL)=BBB-DPHS(IWL)
         IF (PHSP(IC,IWL).LT.0.D0) PHSP(IC,IWL)=PHSP(IC,IWL)+1.D0
         IF (PHSP(IC,IWL).GT.1.D0) PHSP(IC,IWL)=PHSP(IC,IWL)-1.D0
      ELSE
         VMAG(IC,IWL)=BBB
         PHSP(IC,IWL)=AAA-DPHS(IWL)
         IF (PHSP(IC,IWL).LT.0.D0) PHSP(IC,IWL)=PHSP(IC,IWL)+1.D0
         IF (PHSP(IC,IWL).GT.1.D0) PHSP(IC,IWL)=PHSP(IC,IWL)-1.D0
      END IF      
   80 CONTINUE
   81 CALL PMDSTOP
   85 NCT(IWL)=IC-1
   86 CONTINUE
C
C*****************************************************************
C     Principal wavelength loop
C
      DO 50 IWL=1,NWL
      IWR=IWL
      IF (IWR.GT.5) IWR=5
      DO 82 K=1,13
      KCT(K)=0
   82 CONTINUE
C     INITIALIZE SUM OF SQUARES OF RESIDUALS 
C     RRS IS THE ARRAY OF SUMS OF SQUARES OF RESIDUALS      
      RRS(IWL)=0.D0
C     VMNER IS THE ARRAY OF VALUES OF THE MEAN ERROR
      VMNER(IWL)=0.D0    
      SUMWTI(IWL)=0.D0
      CP(IWL)=0.D0
      VLSQ(IWL)=0.D0
C  
C     READ IN CALCULATED REFERENCE LIGHT CURVE      
C  
      DO 40 IC=1,NPSI   
C     RDWT7 INPUTS VIA TAPE17  
C     THESE ARE THE DATA WRITTEN VIA WTOUT5 IN PGD  
      CALL RDWT7(I,IL,VI,PSJ(IC),SP(IC),SMC(IC),ERC,SMD(IC),ERD,
     $SME(IC),ERE,SMF(IC),ERF,SMT(IC),SBT(IC),ERT,VCLC(IC),ERR,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,IN8)
C  
   40 CONTINUE
C     NPSI theoretical data points now stored for wavelength no. IWL
C 
C  
C     JL=TOTAL NUMBER OF ENTRIES IN CALCULATED LIGHT CURVE 
C     LV(1) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO PRIMARY MIN.   
C     LV(2) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO INTERNAL
C     TANGENCY FOLLOWING MIDPRIMARY   
C     LV(3) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO EXTERNAL
C     TANGENCY FOLLOWING PRI.MIN.
C     LV(4) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO MAX.LIGHT,     
C     NEAR THE QUADRATURE PHASE FOLLOWING MIDPRIMARY
C     LV(5) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO EXTERNAL
C     CONTACT BEFORE MIDSECONDARY
C     LV(6) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO INTERNAL
C     CONTACT BEFORE MIDSECONDARY
C     LV(7) IDENTIFIES THE ORDINAL POINT CORRESPONDING TO MIDSECONDARY   
C     LV(8)-LV(13) IDENTIFY COMPARABLE POINTS IN SEQUENCE,WITH LV(13)    
C     IDENTIFYING MIDPRIMARY   
C
   44 JL=NPSI
      WRITE (NOUT15,106) (LV(K),K=1,13)
      WRITE (NOUT15,106) NRY1,NRY2,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,
     $NRY10,NRY11,NRY12
  136 VH(1)=PSJ(LV(1))  
      VH(2)=PSJ(LV(2))  
      VH(3)=PSJ(LV(3))  
      VH(4)=PSJ(LV(4))  
      VH(5)=PSJ(LV(5))  
      VH(6)=PSJ(LV(6))  
      VH(7)=PSJ(LV(7))  
      VH(8)=PSJ(LV(8))  
      VH(9)=PSJ(LV(9))  
      VH(10)=PSJ(LV(10))
      VH(11)=PSJ(LV(11))
      VH(12)=PSJ(LV(12))
      VH(13)=PSJ(LV(13))
      VMX(IWL)=VCLC(1)
      DO 155 J=2,NPSI   
      IF (VCLC(J).GT.VMX(IWL)) VMX(IWL)=VCLC(J)
  155 CONTINUE   
      VMNP(IWL)=VCLC(LV(1))
      VMNS(IWL)=VCLC(LV(7))
      DP(IWL)=(VMNP(IWL)-VMX(IWL))/VMX(IWL)
      DS(IWL)=(VMNS(IWL)-VMX(IWL))/VMX(IWL)
      DDPTH(IWL)=DP(IWL)-DS(IWL)
      VMNP(IWL)=VMNP(IWL)/VMX(IWL)
      VMNS(IWL)=VMNS(IWL)/VMX(IWL)
      DO IC=1,NPSI
         VCLCA(IC)=VCLC(IC)/VMX(IWL)
         SMTA(IC)=SMT(IC)/VMX(IWL)
         SBTA(IC)=SBT(IC)/VMX(IWL)
         SMCA(IC)=SMC(IC)/VMX(IWL)
         SMDA(IC)=SMD(IC)/VMX(IWL)
         SMEA(IC)=SME(IC)/VMX(IWL)
         SMFA(IC)=SMF(IC)/VMX(IWL)
      END DO   
C
      NR=1
C     VMX(IWL)=MAX.LIGHT FOLLOWING PRI.MIN.
C     VMNP(IWL)=LIGHT LEVEL AT PRI.MIN.
C     VMNS(IWL)=LIGHT LEVEL AT SECONDARY MIN.
C     DP(IWL)=DEPTH OF PRI.MIN.
C     DS(IWL)=DEPTH OF SEC.MIN.
C     DDPTH(IWL)=DIFFERENCE OF ECLIPSE DEPTHS
C     AT THIS POINT WE HAVE COMPLETED THE THEORETICAL LT.CVE. FOR IWL
C    
C     THE FOLLOWING LOOP WRITES THE SYNTHETIC LIGHT CURVE TO TAPE 22
C     IT IS IN A SUITABLE FORM FOR PLOTTING
C
C     THE LOOP ALSO WRITES FILES FOR THE OTHER INTEGRATION QUANTITIES
C
  137 DO 169 J=1,JL
      PSK(J)=PSJ(J)/TWPI
      IF (PSK(J).GT.0.75D0) PSK(J)=PSK(J)-1.D0
  169 CONTINUE
      
      DO 170 J=1,JL
C
C     ADD THIRD LIGHT
C
      VLIGHT=(VCLCA(J)+THLT(IWL))/(1.D0+THLT(IWL))
      WRITE (NOUT8,121) PSK(J),VLIGHT,WTA
      VLIGHT=SMTA(J)
      WRITE (NOUT9,121) PSK(J),VLIGHT,WTA
      VLIGHT=SBTA(J)
      WRITE (NOUT10,121) PSK(J),VLIGHT,WTA
      VLIGHT=SMCA(J)
      WRITE (NOUT11,121) PSK(J),VLIGHT,WTA
      VLIGHT=SMDA(J)
      WRITE (NOUT12,121) PSK(J),VLIGHT,WTA
      VLIGHT=SMEA(J)
      WRITE (NOUT13,121) PSK(J),VLIGHT,WTA
      VLIGHT=SMFA(J)
      WRITE (NOUT14,121) PSK(J),VLIGHT,WTA
  170 CONTINUE
C     Write file separators
      WRITE (NOUT8,101) ZR1,ZR2
      WRITE (NOUT9,101) ZR1,ZR2
      WRITE (NOUT10,101) ZR1,ZR2
      WRITE (NOUT11,101) ZR1,ZR2
      WRITE (NOUT12,101) ZR1,ZR2
      WRITE (NOUT13,101) ZR1,ZR2
      WRITE (NOUT14,101) ZR1,ZR2 
C  
C     INPUT PHOTOMETRIC DATA FOR WAVELENGTH NUMBER IWL
      PHSV=0.D0
      MLT=NCT(IWL)
C
C     LOOP FOR TREATING INDIVIDUAL OBSERVATIONS*********************
C
      VMXX=0.D0
      REWIND NOUT17
      DO IC=1,13
         LDF(IC)=0
      END DO
      DO 20 IC=1,MLT
  800 FORMAT (1X,'IC=',I5,5X,I5,F10.5,5X,F10.5)
      CALL SPTIN(NOUT1,NOUT17)
       IF (IC.GT.1190) THEN
       AAA=0.D0
       END IF
      IF (KJP.NE.0) GO TO 70
   20 CONTINUE
      ENDFILE NOUT17
      VMXV(IWL)=VMXX
      REWIND NOUT17
      RESID(IWL)=0.D0
      DO IC=1,MLT
      READ (NOUT17, 109) PHSP(IC,IWL),OBSL,VLS
      VLIGHT=(VLS+THLT(IWL)*VMXV(IWL))/(VMXV(IWL)+THLT(IWL)*VMXV(IWL))
C     OBSL IS THE OBSERVED LIGHT VALUE,RELATIVE TO THE REFERENCE
C     MAGNITUDE, VLIGHT IS THE CALCULATED LIGHT VALUE
C     OMCLC IS THE LIGHT RESIDUAL     
      OMCLC=OBSL-VLIGHT
      IF (DABS(OMCLC).GT.1.D0) THEN
         A=DABS(OMCLC)
         OMCLC=OMCLC/A
      END IF
      IF (VLIGHT.LE.0.D0) THEN
         WRITE (NOUT1,206) VLIGHT,IC,PHSP(IC,IWL), OBSL, VLS
         STOP "SPT: Quitting: VLIGHT <= 0.0"
      END IF
  206 FORMAT (1X,'VLIGHT=',F10.5,5X,'IC=',I5,5X,'PHSP=',F8.5,5X,
     $'OBSL=',E10.4,5X,'VLS=',E10.4)           
      WGHT=100.D0*SIGMA(IWL)*DSQRT(VLIGHT**VNSE(IWL))
      IF (WGHT.GT.50.D0) WGHT=50.D0
      IF (WGHT.EQ.0.D0) WGHT=1.D0
C     CALCULATE THE SQUARE OF THE RESIDUAL AND ADD TO THE SUM     
      RRS(IWL)=RRS(IWL)+OMCLC*OMCLC*WGHT
      SUMWTI(IWL)=SUMWTI(IWL)+WGHT
C     ADD THE RESIDUAL TO THE SUM OF RESIDUALS      
      VMNER(IWL)=VMNER(IWL)+WGHT*DABS(OMCLC)
      CP(IWL)=CP(IWL)+OBSL*VLIGHT
      VLSQ(IWL)=VLSQ(IWL)+VLIGHT*VLIGHT    
  323 PHPT=PHSP(IC,IWL)
      IF (PHPT.GT.PHRF) PHPT=PHPT-1.D0  
      WRITE (NOUT2,138) IC,PHPT,OBSL,VLIGHT,OMCLC
      RESID(IWL)=RESID(IWL)+OMCLC
      END DO
C     Write file marker
      WRITE (NOUT2,109) ZR1,ZR2
   50 CONTINUE   
   70 DO 71 IWL=1,NWL   
      MLT=NCT(IWL)
      V=MLT 
C     CALCULATE STD.DEVIATION OF RESIDUALS FOR WAVELENGTH NUMBER IWL     
      SD(IWL)=DSQRT(RRS(IWL)/SUMWTI(IWL)*V/(V-1.D0))  
C     CALCULATE MEAN ERROR OF RESIDUALS FOR WAVELENGTH NUMBER IWL 
      VMNER(IWL)=VMNER(IWL)/V  
C     CALCULATE NEW REFERENCE MAGNITUDES
      RFLGT=DEXP(RFMG(IWL)/CV)+RESID(IWL)/MLT
      DRFMG(IWL)=-CV*DLOG(DABS(RFLGT))
      AAA=0.D0
   71 CONTINUE   
      DO 72 IWL=1,NWL   
      WRITE (NOUT1,118) WL(IWL)    
      MLT=NCT(IWL)
      WRITE (NOUT1,114) CF*VI,IWL,THLT(IWL),RFMG(IWL),MLT,VMNER(IWL),
     $SD(IWL),DRFMG(IWL)
      CORR(IWL)=DRFMG(IWL)-RFMG(IWL)
      WRITE (NOUT1,134) RFMG(IWL),CORR(IWL),DRFMG(IWL)
      WRITE (NOUT1,116) 1.D0,VMNP(IWL),VMNS(IWL),DP(IWL),DS(IWL),
     $DDPTH(IWL)
   72 CONTINUE   
      REWIND NOUT2
      READ (NOUT2,105) BFR
      DO 60 IWL=1,NWL
      MLT=NCT(IWL)
      ZCOR=1.D0-DEXP(-RFMG(IWL)/CV)
      DO 59 IC=1,MLT
      READ (NOUT2,138) KK,PHSP(IC,IWL),OBSL,VLIGHT,OMCLC
      WRITE (NOUT3,138)  KK,PHSP(IC,IWL),OBSL,VLIGHT,OMCLC
      AAA=PHSP(IC,IWL)
      IF (AAA.EQ.1.D0)  AAA=-1.D-12
      IF (AAA.GT.0.7D0) AAA=AAA-1.D0
	WRITE (NOUT4,121) VLIGHT-ZCOR,AAA,WT(IC,IWL)
      WRITE (NOUT5,121) OMCLC+ZCOR,AAA,WTA
   59 CONTINUE
C     Read file marker
      READ (NOUT2,101) PHSP(IC,IWL),OBSL
      IF ((PHSP(IC,IWL).NE.0.D0).AND.(OBSL.NE.0.D0)) CALL PMDSTOP
C     Write file markers
      WRITE (NOUT3,101) ZR1,ZR2
      WRITE (NOUT4,101) ZR1,ZR2
      WRITE (NOUT5,101) ZR1,ZR2
   60 CONTINUE
      WRITE (NOUT2,111) (DRFMG(IWL),IWL=1,NWL)
      WRITE (NOUT7,111) (DRFMG(IWL),IWL=1,NWL)
C     Output standard deviations of light residuals
      WRITE (NOUT16,135) (SD(IWL),IWL=1,NWL)
C     Output mean squared residual
      WRITE (NOUT16,110) (RRS(IWL),IWL=1,NWL)
C     Write updated file of observations
      WRITE (NOUT18, 141) BFRO
      DO IWL=1,NWL
         KWL=NCT(IWL)
         DO IC=1,KWL
            AAA=PHSP(IC,IWL)
            IF (AAA.EQ.1.D0)  AAA=-1.D-12
            IF (AAA.GT.0.7D0) AAA=AAA-1.D0
            IF (KSQ.EQ.0) THEN
               WRITE (NOUT18,FMT) VMAG(IC,IWL), AAA
            ELSE
               WRITE (NOUT18,FMT) AAA, VMAG(IC,IWL)
            END IF
         END DO
         WRITE (NOUT18,FMT) ZR1,ZR2
      END DO                  
C     Set code for successfull exit
      ERR=0
   99 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
      WRITE (NOUT1, 205) TM
      CLOSE (UNIT=    IN2, STATUS='KEEP')
      CLOSE (UNIT=    IN3, STATUS='KEEP')
      CLOSE (UNIT=    IN4, STATUS='KEEP')
      CLOSE (UNIT=    IN5, STATUS='KEEP')
      CLOSE (UNIT=    IN6, STATUS='KEEP')
      CLOSE (UNIT=    IN7, STATUS='KEEP')
      CLOSE (UNIT=    IN8, STATUS='KEEP')
      CLOSE (UNIT=    IN9, STATUS='KEEP')
      CLOSE (UNIT=   IN10, STATUS='KEEP')
      CLOSE (UNIT=   IN11, STATUS='KEEP')
      CLOSE (UNIT=   IN12, STATUS='KEEP')
      CLOSE (UNIT=   IN13, STATUS='KEEP')
      CLOSE (UNIT=   IN14, STATUS='KEEP')
      CLOSE (UNIT=   IN15, STATUS='KEEP')
      CLOSE (UNIT=   IN16, STATUS='KEEP')
      CLOSE (UNIT=   IN17, STATUS='KEEP')
      CLOSE (UNIT=   IN18, STATUS='KEEP')
      CLOSE (UNIT=   IN19, STATUS='KEEP')
      CLOSE (UNIT=   IN20, STATUS='KEEP')
      CLOSE (UNIT=   IN21, STATUS='KEEP')
      CLOSE (UNIT=   IN22, STATUS='KEEP')
      CLOSE (UNIT=   IN23, STATUS='KEEP')
      CLOSE (UNIT=   IN24, STATUS='KEEP')
      CLOSE (UNIT=   IN25, STATUS='KEEP')
      CLOSE (UNIT=   IN26, STATUS='KEEP')
      CLOSE (UNIT=   IN27, STATUS='KEEP')
      CLOSE (UNIT=  NOUT1, STATUS='KEEP')
      CLOSE (UNIT=  NOUT2, STATUS='KEEP')
      CLOSE (UNIT=  NOUT3, STATUS='KEEP')
      CLOSE (UNIT=  NOUT4, STATUS='KEEP')
      CLOSE (UNIT=  NOUT5, STATUS='KEEP')
      CLOSE (UNIT=  NOUT6, STATUS='KEEP')
      CLOSE (UNIT=  NOUT7, STATUS='KEEP')
      CLOSE (UNIT=  NOUT8, STATUS='KEEP')
      CLOSE (UNIT=  NOUT9, STATUS='KEEP')
      CLOSE (UNIT= NOUT10, STATUS='KEEP')
      CLOSE (UNIT= NOUT11, STATUS='KEEP')
      CLOSE (UNIT= NOUT12, STATUS='KEEP')
      CLOSE (UNIT= NOUT13, STATUS='KEEP')
      CLOSE (UNIT= NOUT14, STATUS='KEEP')
      CLOSE (UNIT= NOUT15, STATUS='KEEP')
      CLOSE (UNIT= NOUT16, STATUS='KEEP')
      CLOSE (UNIT= NOUT17, STATUS='KEEP')
      CLOSE (UNIT= NOUT18, STATUS='KEEP')
      OPEN (UNIT=IN1,    FILE=FL1(1),   STATUS='OLD')
      WRITE (IN1, 201) KEYGOOD
      CLOSE (UNIT=    IN1, STATUS='KEEP')
      IF (ERR.EQ.0) THEN
          STOP 'SPT: Completed successfully.'
      ELSE
          CALL PMDSTOP
      END IF
  301 WRITE (NOUT1,115) IC,IWL     
C     Set error code
      ERR=1
      GO TO 99   
  302 WRITE (NOUT1,120) THLT(K),K  
C     Set error code
      ERR=2
      GO TO 99
  299 CALL PMDSTOP
      STOP
  399 CALL PMDSTOP
      STOP
      END  PROGRAM SPT
