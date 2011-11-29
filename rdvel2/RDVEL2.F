      PROGRAM RDVEL2
C     Program to provide arguments for spectrum synthesis program
      INCLUDE 'COMA.FOR'
      INCLUDE 'ACOMA.FOR'
      INCLUDE 'COMM.FOR'
      INCLUDE 'COMR.FOR'
      DIMENSION WL(MWL)
      DIMENSION UC(MTW),C1(MTW),C2(MTW),T0(MTW),A(MTW),BCF(MTW),UA(MTW)
      DIMENSION TARY(MTR,MTR),FL1(21)
C
	DIMENSION SMA1(MPHM),SMA2(MPHM),SMA1P(MPHM),SMA2P(MPHM)
      DIMENSION SMA1E(MPHM),SMA2E(MPHM)
C
      DIMENSION STX(MTW,MTLM),
     $STY(MTW,MTLM),STZ(MTW,MTLM),STVS(MTW,MTHM,MPHM),PRSH(MTW,MTHM,MTW)
     $,VXP(MTW,MTHM,MTW),VYP(MTW,MTHM,MTW),ARYF(MTHM),ARYG(MTHM),
     $ARYFA(MTHM)
      DIMENSION VLA(MTW,MTHM,MPHM)
C
      DIMENSION AGLD(MSG),RD(MSG),XHZ(MSG),YHZ(MSG)
      DIMENSION CSGR(MRM,MSG),CSGF(MRING,MSG),
     $PXR(MRM,MSG),PYR(MRM,MSG),PZR(MRM,MSG),PXF(MRING,MSG),
     $PYF(MRING,MSG),
     $PZF(MRING,MSG),PRFH(MRING,MSG),PRRH(MRM,MSG),RXHZL(MRM),
     $RYHZL(MRM),RXHZG(MRM),RYHZG(MRM),PRVFG(MRM),PRVFL(MRM)
C
      DIMENSION PJARM(MRM,MSG),VRRM(MRM,MSG),RVLD(MSG),ANG(MSG)
      DIMENSION PJAFC(MRING,MSG),VFACE(MRING,MSG),VCRC(MRING),PHA(MSG)
      DIMENSION TMPRF(MRING,MSG),TMPRR(MRM,MSG),GBFR(MSG),
     $TMPRFA(MRING,MSG)
      DIMENSION SMV(MRING,MSG),RMIFLX(MRM,MSG),VC(MRM)
	DIMENSION PHSRV(MPHS),RDVL1(MPHS),RDVL2(MPHS),VRD1SV(MPHS),
     $VRD2SV(MPHS)
	DIMENSION VL1A(2,MTHM,MPHM)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 33 HD1
      CHARACTER * 36 FL1
      CHARACTER * 10 DATE,TIME,ZONE
      INTEGER * 2 IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      INTEGER * 2 IYR
	INTEGER * 2 KEYGOOD, KEYBAD
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
      DATA PI,TWPI/3.1415926535898D0,6.2831853071796D0/
      DATA GRAV/6.6704D-8/
	DATA DLT/1.0D-13/
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8,IN9/1,2,3,4,55,66,7,8,9/
	DATA IN10,IN11/10,11/
      DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5,NOUT6/12,13,14,15,16,17/
      DATA NOUT7,NOUT8,NOUT9/18,19,20/
	DATA IN12/21/
  100 FORMAT (A36)
  101 FORMAT (1X,'Projected area, accretion disk face=',E15.5,/,1X,
     $'Projected area, accretion disk rim=',E15.5)
  102 FORMAT (1X,'Surface area, component',I5,1X,'=',E12.6,1X,
     $'cm**2')
  103 FORMAT (' ')
  104 FORMAT (1X,'PROGRAM RDVEL2',30X,'DATE=',I2,'/',I2,'/',I4,2X,
     $'TIME=',I2,':',I2,':',I2,'.',I2)
  105 FORMAT (1X,'Number of values of orbital longitude=',I5,/,1X, 
     $'Ordinal value of starting orbital longitude=',I11,/,1X,
     $'Ending value of orbital longitude=',I5)
  106 FORMAT (10I5)     
  107 FORMAT (A33)
  108 FORMAT (1X,'Radial velocity of component   I=',F12.7,2X,'km/sec',
     $/,1X,'Radial velocity of component 3-I=',F12.7,2X,'km/sec')
  109 FORMAT (1X,'Orbital speed                   =',F12.7,2X,
     $'km/sec')
  110 FORMAT (1X,'Projected area, ring number',I5,2X,'=',E14.6)
  112 FORMAT (1X,'Surface areas, consecutive colatitudes')
  113 FORMAT (8E12.5)
  114 FORMAT (1X,'Projected area, component',I5,1X,'=',E12.6,1X,
     $'cm**2')
  115 FORMAT (1X,'Projected areas, consecutive colatitudes')
  116 FORMAT (1X,'Projected eclipsed area',E14.6,1X,'cm**2')
  117 FORMAT (1X,'Projected eclipsed areas, consecutive colatitudes')
  118 FORMAT (1X,'Visibility keys, by colatitude strips')
  119 FORMAT (20F4.1)
  120 FORMAT (I2)
  122 FORMAT (4E16.8)
  127 FORMAT (1X,'Orbital longitude=',F12.7,5X,
     $'Ordinal orbital longitude=',I5)
  128 FORMAT (F12.7,2F12.5)
  129 FORMAT (6E20.12)
  162 FORMAT (10F8.3)
  206 FORMAT (3I5)
  200 FORMAT (A80)
  205 FORMAT (1X,'Program running time=',F12.3,2X,'seconds')
C***********************************************************
      OPEN (UNIT=22, FILE='RDVEL2.ILS', STATUS='OLD')
      READ (22, 100) (FL1(J),J=1,21)
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
      OPEN (UNIT=NOUT5, FILE=FL1(16), STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(17), STATUS='REPLACE')
      OPEN (UNIT=NOUT7, FILE=FL1(18), STATUS='REPLACE')
      OPEN (UNIT=NOUT8, FILE=FL1(19), STATUS='REPLACE')
      OPEN (UNIT=NOUT9, FILE=FL1(20), STATUS='REPLACE')
	OPEN (UNIT=IN12,  FILE=FL1(21), STATUS='OLD')
C***********************************************************
      WRITE (IN12,120) KEYBAD
	CLOSE (UNIT=IN12, STATUS = 'KEEP')
      HD1='THEORETICAL RADIAL VELOCITY CURVE'
      WRITE (NOUT2,107) HD1
C     Read input data
      LBLERR=0
      LABEL = '*RDVEL2'
      NU=IN1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NPSI)
      CALL DUMMY (1)
      CALL IREAD (NPST)
      CALL DUMMY (1)
      CALL IREAD (NPSN)
	CALL DUMMY (1)
	CALL DREAD (GAM1,'F10.3')
	CALL DUMMY (1)
	CALL DREAD (GAM2,'F10.3')
	CALL DUMMY (1)
	CALL IREAD (KEY)

C
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
C  
C
      READ (IN4,106) KCTRL,NPSI
      IF (NPST.EQ.0) NPST=1    
      IF (NPSN.EQ.0) NPSN=NPSI
   10 WRITE (NOUT1,105) NPSI,NPST,NPSN
C     GET ORBITAL PERIOD
      NU=IN5
      CALL DUMMY(7)
      CALL DREAD (PER, 'F15.12')
      PER=PER*86400.0D0            !Convert to seconds
C     INPUT PERMANENT FILE DATA FROM PGA
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,VVJ,IN2)
C     Input temperature data
      CALL RDWT3(I,NT,NWL,WL,UC(I),UC(3-I),C1(I),C2(I),
     $C1(3-I),C2(3-I),T0(I),T0(3-I),A(I),A(3-I),BCF(I),BCF(3-I),UA(I),
     $UA(3-I),IN6)
      RI2=RI*RI
C     Input data from program ACPGA
      CALL ACRD1(NRM,NSG,NRING,RA,RB,HV,AVL,
     $ITHU,ITHL,ISTAR,NTYPE,VC,HVBL,THBL,IN7)
C     Input data from program ACPGC
      CALL ACRD2(NW,NSG,NRM,NRING,IRCT,WLV,
     $TMPRF,TMPRFA,SMV,TMPRR,RMIFLX,IN9)
C     Dimensions are in units of component separation
      RAP=RA
      RBP=RB
      HVP=HV
C     Calculate V0, the relative orbital speed, in km/sec.
C     RI is the semi-major axis in cm.
C     PER is the orbital period in sec.
      V0=TWPI*RI/PER/100000.0D0       
C     Calculate orbital speeds for the separate components.
      V1=V0*QS/(1.D0+QS)
      V2=V0*QN/(1.D0+QN)
C     Calculate orbital speed of center of accretion disk
      IF (ISTAR.EQ.I) THEN
	 VISTAR=V1
      ELSE
	 VISTAR=V2
      END IF
C     CALCULATE ORBITAL ANGULAR VELOCITY
C     Note that the product of this angular velocity and radial dist.
C     from rotation axis, in cm., gives the linear velocity in km/sec.
      ANGVL=V0/RI
C     Save the azimuthal division angles for the rim in PHA
      DO J=1,NSG
	 PHA(J)=PHIR(J)
      END DO
C
C
C
C     PRINCIPAL LOOP,FOR SUCCESSIVE VALUES OF ORBITAL LONGITUDE   
C
      DO 60 KS=1,NPSN 
C     Initialize component radial velocities and normalizing factors
      RDVL1V=0.D0
	RDVL2V=0.D0
	VNORM1=0.D0
	VNORM2=0.D0
C     INPUT PGB DATA
      CALL RDWT2(NT,NU,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN3)
C     Save orbital position angle.
      PSIT=PSI
      PSIV=PSI
C     Save orbital inclination.
      VLVI=VI
C     Calculate inclination in degrees.
      viv=vi*57.2957795131D0
      CSI=DCOS(VI)
      SNI=DSIN(VI)
      Q=QS
      IF (ISTAR.EQ.I) THEN
		CMV=PSI
		CM=CMV
	ELSE
		CMV=TWPI-PSIV
		CM=CMV
	END IF
      SNPSI=DSIN(CMV)
      SNPSIA=SNPSI
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
      VM0S=VM0
      WRITE (NOUT1,127) PSI,KS
      IF (KS.LT.NPST) GO TO 60
C     Calculate radial velocity of component 1.
      VRD1=V1*SNI*SNPSIA
	VRD1SV(KS)=VRD1
	IF (KEY.EQ.0) GO TO 12
C     Input data from program ACPGB
C     Note that this inputs file STVS, the visibility keys for the two stars.
      IF (NRING.GT.0) THEN
      CALL ACRD3(NSG,NRM,NRING,CSGR,CSGF,PXR,PYR,PZR,
     $PXF,PYF,PZF,XHZ,YHZ,PRFH,PRSH,PRRH,XRMC,YRMC,RBP,STX,STY,STZ,
     $STVS,RXHZL,RYHZL,RXHZG,RYHZG,PRVFG,PRVFL,XUL,XLL,YUL,YLL,RVLD,
     $ANG,RD,AGLD,VXP,VYP,IFC,IRC,IFCS,IRCS,IGAIN,IZ,IEC,IFLAG,NTH,IBDY,
     $IN8)
      END IF
C     Input data from PGD
      DO IPPP=1,2
		DO ITH=1,NTH
C		NN=NPH(IPPP,ITH)
			READ (IN10,106) IWL,KSS,IPP,ITHP,NN
			READ (IN10,162) (STVS(IPP,ITHP,KK),KK=1,NN)
                        IF (KSS.GE.50) THEN
                        ZZZ=0.D0
                        END IF
		END DO
	END DO		               
C     Input data from PGD for intensity weighting
      OPEN (UNIT=29,FILE='DBG.DAT')
      DO IPP=1,2
		DO ITH=1,NTH
C                NN=NPH(IPP,ITH)
			READ (IN11,106) IWL,KSP,IP,ITHP,NN
                        WRITE (29,106) IWL,KSP,IP,ITHP,NN
                        IF (KSP.GE.2) THEN
                        ZZZ=0.D0
                        END IF
C     VL2(IP,ITHP,KK) is the contribution from segment KK to the light from
C     component IP at wavelength index IWL. Calculated in PGD.
			READ (IN11,129) (VL2(IP,ITHP,KK),KK=1,NN)
                        WRITE (29,129) (VL2(IP,ITHP,KK),KK=1,NN)
		END DO
	END DO
	DO IPP=1,2
		DO ITH=1,NTH
C                NN=NPH(IPP,ITH)
			READ (IN11,106) IPPP,ITHP,NN
                        WRITE (29,106) IPPP,ITHP,NN
C      VL1A(IPPP,ITH,KK) is the projected area of segment KK of component IPPP
C      In particular, VL1A(IPPP,ITH,1) is the projected area of strip ITH-1-->
C      ITH. The remaining VL1A(IPPP,ITH,KK) entries are the projected areas of
C      segments KK-1-->KK, starting with KK=2.
			READ (IN11,129) (VL1A(IPPP,ITHP,KK),KK=1,NN)
                        WRITE (29,129) (VL1A(IPPP,ITHP,KK),KK=1,NN)
		END DO
	END DO
        CLOSE (UNIT=29, STATUS='KEEP')
C        IF (IP.LT.1000) GO TO 199
C     Calculate mean intensity.
      SMVL2C1=0.D0
	SMVL2C2=0.D0
	CNT=0.D0
	DO ITH=1,NTH
		NN=NPH(I,ITH)
		DO K=1,NN
			SMVL2C1=SMVL2C1+VL2(I,ITH,K)
			CNT=CNT+1.D0
		END DO
	END DO
C     Normalize by mean intensity
	SMVL2C1=SMVL2C1/CNT
	DO ITH=1,NTH
		NN=NPH(I,ITH)
		DO K=1,NN
			VL2(I,ITH,K)=VL2(I,ITH,K)/SMVL2C1
		END DO
	END DO
C
C     Do same for second component
	CNT=0.D0
	DO ITH=1,NTH
		NN=NPH(3-I,ITH)
		DO K=1,NN
			SMVL2C2=SMVL2C2+VL2(3-I,ITH,K)
			CNT=CNT+1.D0
		END DO
	END DO
	SMVL2C2=SMVL2C2/CNT
	DO ITH=1,NTH
		NN=NPH(3-I,ITH)
		DO K=1,NN
			VL2(3-I,ITH,K)=VL2(3-I,ITH,K)/SMVL2C2
		END DO
	END DO
C     Calculate radial velocities for rim of accretion disk
      IF (NRM.EQ.0) GO TO 11
C     Calculate rim keplerian speed.
      IF (ISTAR.EQ.1) THEN
	 VMISTAR=VMI
      ELSE
	 VMISTAR=QS*VMI
      END IF
      GVMS=GRAV*VMISTAR
      ANGVLRM=DSQRT(GVMS/(RAP*RI)**3)
      RI2=RI*RI
C     Angular vel. in radians/sec.
      VRM=(1.D0-FVF(NRING))*ANGVLRM*RI*RAP/100000.D0
C     Rim velocity in km./sec.
C     Calculate factor for projecting accretion face segments onto
C     orbital plane.
      ALPHA=DATAN(HVP/RAP)
      CSALPHA=DCOS(ALPHA)
      AFC=0.D0      
C     Now do bottom face of accretion disk.
      DO K=1,NRING-1
      APROJRG=0.D0
C     Calculate keplerian speed for inner edge of division circle.
	 D3=RGRD(K)*RI
	 ANGVLA=DSQRT(GVMS/D3**3)
C     Angular velocity in radians/sec.
	 VCRC(K)=(1.D0-FVF(K))*ANGVLA*D3/100000.D0
	 AA=AVL**2*(RGRD(K+1)**2-RGRD(K)**2)/2.D0
C     Circular velocity in km./sec.
	    DO J=1,NSG-1
	       DLTAREA=AA*(PHIR(J+1)-PHIR(J))
	       CSGMN=(CSGF(K,J)+CSGF(K,J+1)+CSGF(K+1,J)
     $               +CSGF(K+1,J+1))/4.D0
	       IF (CSGMN.LT.0.D0) CSGMN=0.D0            
	       IF (PRFH(K,J).GE.0.D0) THEN
		  PJAFC(K,J)=DLTAREA*CSGMN
	       ELSE
		  PJAFC(K,J)=0.D0
	       END IF
	       APROJRG=APROJRG+DLTAREA*CSGMN
	    END DO     
	    PJAFC(K,NSG)=0.D0
	    WRITE (NOUT1,110) K,APROJRG
	    AFC=AFC+APROJRG
      END DO
      PJAFC(NRING,NSG)=0.D0
      VCRC(NRING)=VRM
      DO K=1,NRING
	    DO J=1,NSG
	       VRD1F=VCRC(K)*VL0*DSIN(PHIR(J))+VCRC(K)*VM0*
     $              DCOS(PHIR(J))+VISTAR*SNI*SNPSI
	       VFACE(K,J)=-VRD1F
	   END DO
	    WRITE (NOUT9,129) (PJAFC(K,J),J=1,NSG)
	    WRITE (NOUT9,129) (PRFH(K,J),J=1,NSG)
	    WRITE (NOUT9,129) (CSGF(K,J),J=1,NSG)
	    WRITE (NOUT9,129) (VFACE(K,J),J=1,NSG)
      END DO
C     Calculate output quantities for rim.
      ARM=0.D0
      DO K=1,NRM-1
	 TH1=THR(K)
	 TH2=THR(K+1)
	 CS1=DCOS(TH1)
	 CS2=DCOS(TH2)
	 DH=AVL*RA*(CS1/SNTR(K)-CS2/SNTR(K+1))
	 DO J=1,NSG-1
	    DRPH=AVL*RA*(PHIR(J+1)-PHIR(J))
	    CRM=0.D0
	    IF (K.NE.NRM) THEN
	    IF ((CSGR(K+1,J).LT.0.D0).AND.(CSGR(K+1,J+1).GE.0.D0))
     $       CRM=1.D0
	    IF ((CSGR(K+1,J).GE.0.D0).AND.(CSGR(K+1,J+1).LT.0.D0))
     $       CRM=1.D0
	    IF ((CSGR(K+1,J).GE.0.D0).AND.(CSGR(K+1,J+1).GE.0.D0))
     $       CRM=1.D0
	    ELSE
	    IF ((CSGR(K,J).LT.0.D0).AND.(CSGR(K,J+1).GE.0.D0))CRM=1.D0
	    IF ((CSGR(K,J).GE.0.D0).AND.(CSGR(K,J+1).LT.0.D0))CRM=1.D0
	    IF ((CSGR(K,J).GE.0.D0).AND.(CSGR(K,J+1).GE.0.D0))CRM=1.D0
	    END IF
	    IF (J.NE.NRM) THEN
	     CSGRM=(CSGR(K,J)+CSGR(K,J+1)+CSGR(K+1,J)+CSGR(K+1,J+1))/
     $        4.D0
	    ELSE
	     CSGRM=(CSGR(K,J)+CSGR(K,J+1))/2.D0
	    END IF 
	     PJARM(K,J)=DH*DRPH*DABS(CSGRM)*CRM
	     ARM=ARM+PJARM(K,J)
	 END DO
	 PJARM(K,NSG)=0.D0
      END DO
      DO K=1,NRM
	 DO J=1,NSG
	    VRD1RM=VRM*VL0*DSIN(PHIR(J))+VRM*VM0*DCOS(PHIR(J))+
     $           VISTAR*SNI*SNPSI
	    VRRM(K,J)=-VRD1RM
	 END DO
	 WRITE (NOUT9,129) (PJARM(K,J),J=1,NSG)
	 IF (K.EQ.NRM) THEN
	    WRITE (NOUT9,129) (PRRH(K,J),J=1,NSG)
	 ELSE
	    WRITE (NOUT9,129) (PRRH(K+1,J),J=1,NSG)
	 END IF
	 WRITE (NOUT9,129) (CSGR(K,J),J=1,NSG)
	 WRITE (NOUT9,129) (VRRM(K,J),J=1,NSG)
      END DO
      WRITE (NOUT1,101)AFC,ARM 
C
C     Now do the stellar components.
C     The star at the origin always comes first.
C     This is the component eclipsed at primary minimum.
C     LOOP TO CALCULATE ARRAYS OF SURFACE AREA ELEMENTS AND INTENSITIES  
C     IN THE LINE OF SIGHT     
C
C******************Factor CF***********************************
   11 CF=RI*RI
C***************************************************************
C     Initialize accumulators
      SMAREA1=0.D0
	SMAREA1P=0.D0
	SMAREC1=0.D0 
	RDVL1P=0.D0
	VNORM1P=0.D0
C
C     Long loop for star I
C	 
      DO 22 ITH=1,NTH
      SMA1(ITH)=0.D0
	SMA1P(ITH)=0.D0
	SMA1E(ITH)=0.D0
	RDVL1V=0.D0
	VNORM1=0.D0
      N=NPH(I,ITH)
      IF (N.LE.0) CALL PMDSTOP
      IF (ITH.EQ.1) THEN
	 SNCF=0.D0
	 DTH=0.D0
      ELSE
	 IF (ITH.EQ.NTH) THEN
		VTHA=TH(I,ITH)
		VTHB=TH(I,ITH-1)
	    DTH=DABS(VTHB-VTHA)
	    SNCF=(DCOS(VTHB)-DCOS(VTHA))/DTH
	 ELSE
		VTHA=TH(I,ITH-1)
		VTHB=TH(I,ITH)
	    DTH=DABS(VTHA-VTHB)
	    SNCF=DABS(DCOS(VTHA)-DCOS(VTHB))/DTH
	 END IF
      END IF
C
C     Inner loop for star I
C.
      DO 20 K=2,N   
C     DSG REPRESENTS AN ELEMENT OF SURFACE AREA     
	VPHV=PH(I,ITH,K)
	SNVPHV=DSIN(VPHV)
	CSVPHV=DCOS(VPHV)
      IF (N.EQ.1) THEN
	 DPH=TWPI
	 DSG(I,ITH,K)=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)*SNCF*
     $	DTH*DPH*RI2
      ELSE
	 IF (K.LT.N) THEN
		VPHA=PH(I,ITH,K+1)
	    DPH=VPHA-VPHV
	    DSG1=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)*SNCF*DTH*DPH*RI2
	    DSG2=VR(I,ITH,K+1)*VR(I,ITH,K+1)/CSB(I,ITH,K+1)*SNCF*
     $		DTH*DPH*RI2
	    IF (K.EQ.1) THEN
	       DSG(I,ITH,K)=0.5D0*DSG1
	    ELSE
	       DSG(I,ITH,K)=0.5D0*(DSG1+DSG2)
	    END IF
	 ELSE
	    DSG(I,ITH,K)=0.5D0*DSG2
	 END IF
      END IF
C     Accumulate area of latitude strip ITH
      SMA1(ITH)=SMA1(ITH)+DSG(I,ITH,K)
C     VL1 REPRESENTS AN ELEMENT OF SURFACE AREA PROJECTED ON THE PLANE
C     OF THE SKY 
C      VL1(I,ITH,K)=CSG(I,ITH,K)*DSG(I,ITH,K)
C     Set VL1 equal to input value from PGD.
      VL1(I,ITH,K)=VL1A(I,ITH,K)
C     Restrict sum to visible area.
C      IF (VL1(I,ITH,K).LT.0.0D0) VL1(I,ITH,K)=0.0D0
C     Accumulate projected area for latitude strip ITH
      SMA1P(ITH)=SMA1P(ITH)+VL1(I,ITH,K)
C     Accumulate projected areas of eclipsed surface
	IF (STVS(I,ITH,K).LT.0.D0) THEN
		SMA1E(ITH)=SMA1E(ITH)+VL1(I,ITH,K)
	END IF
C     Calculate radial velocity for that area element, relative to
C     center of star
C     VROT is spatial velocity, in km/sec, relative to center of star.
C     VR(I,ITH,K)*RI*SNT(I,ITH) is spatial distance from rotation
C     axis to surface point, in cm. 
      VROT=FV(I)*ANGVL*VR(I,ITH,K)*RI*SNT(I,ITH)
C     Calculate velocity components relative to center of star.
C     A velocity component toward the obsever is counted as negative.
C     The direction cosines of the rotational velocity vector are
C     [sin(phi),cos(phi),0.0]
C     Calculate the cosine of the angle between the rotational velocity
C     vector and the line of sight.
      CSVEL=-SNVPHV*VL0-CSVPHV*VM0
C     Calculate radial velocity relative to center of star
      VRD1A=-VROT*CSVEL
C     Now add radial velocity of star center relative to system center of 
C     mass
      VRD1A=VRD1A-VRD1
C     Now add spatial radial velocity of component (ideally the same as
C     the system center of mass radial velocity).
      VRD1A=VRD1A+GAM1
C     Save radial velocity contribution of that surface element.
      IF (CSG(I,ITH,K).GT.0.D0) THEN
		VLA(I,ITH,K)=VRD1A
	ELSE
		VLA(I,ITH,K)=0.D0
	END IF
C     Accumulate intensity-weighted radial velocity for component I,
C     and normalizing factor
      IF (STVS(I,ITH,K).GE.0.D0) THEN
C     VL2(I,ITH,K) is the light flux toward the observer at the
C     surface element in question.
c		vl2(i,ith,k)=1.d0
		RDVL1V=RDVL1V+VLA(I,ITH,K)*VL2(I,ITH,K)
		VNORM1=VNORM1+VL2(I,ITH,K)
	END IF
   20 CONTINUE
C
C     End of inner loop
C
      RDVL1P=RDVL1P+RDVL1V
	VNORM1P=VNORM1+VNORM1P
	VPHV=PH(I,ITH,1)
	IF (I.EQ.ISTAR) THEN
C     This condition will be true if I is the star with the accretion disk.
C     Program ACPGF6 requires star IOBJ=3 to be the object with the accretion disk.
C     IOBJ=3 must directly follow the data for the accretion disk rim in the input
C     stream to program ACPGF6.
		WRITE (NOUT5,206) I,ITH,N
		WRITE (NOUT6,206) I,ITH,N
		WRITE (NOUT5,129) (VL1(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (STVS(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (CSG(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (VLA(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (PH(I,ITH,KN),KN=1,N)
		WRITE (NOUT6,129) (G(I,ITH,KN),KN=1,N)
		WRITE (NOUT6,129) (TMPR(I,ITH,KN),KN=1,N)
		WRITE (NOUT6,129) (FIN(I,ITH,KN),KN=1,N)
	ELSE
C     This part of the code is reached if star I is the object without the accretion
C     disk. In that case, save the output in preparation for adding it after writing
C     the data for the star with the accretion disk to the file for input to
C     program ACPGF6.
		WRITE (NOUT7,206) I,ITH,N
		WRITE (NOUT8,206) I,ITH,N
		WRITE (NOUT7,129) (VL1(I,ITH,KN),KN=1,N)
		WRITE (NOUT7,129) (STVS(I,ITH,KN),KN=1,N)
		WRITE (NOUT7,129) (PH(I,ITH,KN),KN=1,N)
		WRITE (NOUT8,129) (G(I,ITH,KN),KN=1,N)
		WRITE (NOUT8,129) (TMPR(I,ITH,KN),KN=1,N)
		WRITE (NOUT8,129) (FIN(I,ITH,KN),KN=1,N)
	END IF
   22 CONTINUE
C
C     End of long loop
C
C     Normalize the radial velocity
      IF (VNORM1P.NE.0.D0) THEN
		RDVL1P=RDVL1P/VNORM1P
	ELSE
		RDVL1P=0.D0
	END IF
C
      DO ITH=1,NTH
      SMAREA1=SMAREA1+SMA1(ITH)
      END DO
      WRITE (NOUT1,102) I,SMAREA1
	WRITE (NOUT1,112)
	WRITE (NOUT1,113) (SMA1(ITH),ITH=1,NTH)
      DO ITH=1,NTH
	SMAREA1P=SMAREA1P+SMA1P(ITH)
	SMAREC1=SMAREC1+SMA1E(ITH)
	END DO
	WRITE (NOUT1,114) I,SMAREA1P
	WRITE (NOUT1,115)
	WRITE (NOUT1,113) (SMA1P(ITH),ITH=1,NTH)
      WRITE (NOUT1,116) SMAREC1
	WRITE (NOUT1,117)
	WRITE (NOUT1,113) (SMA1E(ITH),ITH=1,NTH)
	WRITE (NOUT1,118)
	DO ITH=1,NTH
		N=NPH(I,ITH)
		WRITE (NOUT1,106) ITH,N
		WRITE (NOUT1,119) (STVS(I,ITH,K),K=1,N)
		WRITE (NOUT1,103)
	END DO
C  
   12   I=3-I      
C  
C     CORRESPONDING LOOP FOR SECOND COMPONENT
C     This is the component eclipsed at secondary minimum.
C  
      Q=QN
      PSIV=PSIV+PI
      IF (PSIV-TWPI-DLT.LT.0.D0) PSIV=PSIV-TWPI
      PSI=PSIV
      CMV=PSIV
      IF (CMV.LT.0.0D0) CMV=CMV+TWPI
      CM=CMV
      SNPSI=DSIN(CMV)
      SNPSIB=SNPSI
C     Calculate radial velocity of component 2.
      VRD2=V2*SNI*SNPSIB
	VRD2SV(KS)=VRD2
	IF (KEY.EQ.0) GO TO 13
      CALL LSGT(CSI,SNI,CM,VL0,VM0,VN0,SPSI,CPSI,TARY)
      SMAREA2=0.D0
	SMAREA2P=0.D0
	SMAREC2=0.D0
	RDVL2P=0.D0
	VNORM2P=0.D0
C
C     Long loop for star 3-I
C
      DO 42 ITH=1,NTH
      SMA2(ITH)=0.D0
	SMA2P(ITH)=0.D0
	SMA2E(ITH)=0.D0
	RDVL2V=0.D0
	VNORM2=0.D0
      N=NPH(I,ITH)      
      IF (N.LE.0) CALL PMDSTOP
      IF (ITH.EQ.1) THEN
	  SNCF=0.D0
	  DTH=0.D0
      ELSE
	  IF (ITH.EQ.NTH) THEN
		VTHA=TH(I,ITH)
		VTHB=TH(I,ITH-1)
	    DTH=DABS(VTHB-VTHA)
	    SNCF=(DCOS(VTHB)-DCOS(VTHA))/DTH
	 ELSE
		VTHA=TH(I,ITH-1)
		VTHB=TH(I,ITH)
	    DTH=DABS(VTHB-VTHA)
	    SNCF=DABS(DCOS(VTHA)-DCOS(VTHB))/DTH
	 END IF
      END IF
C
C     Inner loop for star 3-I
C
      DO 40 K=2,N   
	VPHV=PH(I,ITH,K)
	SNVPHV=DSIN(VPHV)
	CSVPHV=DCOS(VPHV)
      IF (N.EQ.1) THEN
		DPH=TWPI
		DSG(I,ITH,K)=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)*SNCF*
     $	DTH*DPH*RI2
      ELSE
	 IF (K.LT.N) THEN
		VPHA=PH(I,ITH,K+1)
	    DPH=VPHA-VPHV
	    DSG1=VR(I,ITH,K)*VR(I,ITH,K)/CSB(I,ITH,K)*SNCF*DTH*DPH*RI2
	    DSG2=VR(I,ITH,K+1)*VR(I,ITH,K+1)/CSB(I,ITH,K+1)*SNCF*
     $		DTH*DPH*RI2
		IF (K.EQ.1) THEN
			DSG(I,ITH,K)=0.5D0*DSG1
		ELSE
			DSG(I,ITH,K)=0.5D0*(DSG1+DSG2)
		END IF
	 ELSE
	    DSG(I,ITH,K)=0.5D0*DSG2
	 END IF
      END IF
C     Accumulate area of latitude strip ITH
      SMA2(ITH)=SMA2(ITH)+DSG(I,ITH,K)
C     VL1 represents an element of surface area projected on the plane
C     of the sky.
C      VL1(I,ITH,K)=CSG(I,ITH,K)*DSG(I,ITH,K)
C     Set VL1 equal to input from PGD
      VL1(I,ITH,K)=VL1A(I,ITH,K)
C     Restrict sum to visible area.
C      IF (VL1(I,ITH,K).LT.0.0D0) VL1(I,ITH,K)=0.0D0
C     Accumulate projected area for latitude strip ITH
      SMA2P(ITH)=SMA2P(ITH)+VL1(I,ITH,K)
C     Accumulate projected area of eclipsed surface
	IF (STVS(I,ITH,K).LT.0.D0) THEN
		SMA2E(ITH)=SMA2E(ITH)+VL1(I,ITH,K)
	END IF
C     Calculate radial velocity for that area element, relative to
C     center of star
C     VROT is spatial velocity, in km/sec, relative to center of star.
C     VR(I,ITH,K)*RI*SNT(I,ITH) is spatial distance from rotation
C     axis to surface point, in cm. 
	VROT=FV(I)*ANGVL*VR(I,ITH,K)*RI*SNT(I,ITH)
C     Calculate velocity components relative to center of star
C     A velocity component toward the obsever is counted as negative.
C     Note that in changing to the companion the sense of the axes
C     have been changed by increasing the orbital phase angle by pi.
C     The direction cosines of the rotational velocity vector are
C     [sin(phi),cos(phi),0.0]
C     Calculate the cosine of the angle between the rotational velocity
C     vector and the line of sight.
      CSVEL=-SNVPHV*VL0-CSVPHV*VM0
C     Calculate radial velocity
      VRD2A=-VROT*CSVEL
C     Now add radial velocity of star center relative to system center of
C     mass
      VRD2A=VRD2A-VRD2
C     Now add spatial radial velocity of component (ideally the same as
C     the system center of mass radial velocity).
      VRD2A=VRD2A+GAM2
C     Save radial velocity contribution of that surface element.
      VLA(I,ITH,K)=VRD2A
C     Accumulate flux-weighted radial velocity for component 3-I, and
C     normalizing factor.
      IF (STVS(I,ITH,K).GE.0.D0) THEN
C     VL2(I,ITH,K) is the light flux toward the observer at the surface
C     element in question.
C     Through the following statement, RDVL2V is the accumulated
C     flux-weighted radial velocity sum, not normalized, for the current
C     ITH strip.
c		vl2(i,ith,k)=1.d0
		RDVL2V=RDVL2V+VLA(I,ITH,K)*VL2(I,ITH,K)
C     VNORM2 is the corresponding sum of the flux values alone. 
		VNORM2=VNORM2+VL2(I,ITH,K)
	END IF
   40 CONTINUE
C
C     End of inner loop, star 3-I
C
C     Accumulate sum over ITH values
      RDVL2P=RDVL2P+RDVL2V
	VNORM2P=VNORM2+VNORM2P
	VPHV=PH(I,ITH,1)
	IF (I.NE.ISTAR) THEN
C     The star originally designated as star I has been replaced by 3-I,
C     so that now star I is the object without the accretion disk if
C     originally star I designated the object with the accretion disk.
C     NOUT7 and NOUT8 data will be written to the output file after
C     NOUT5 and NOUT6 have been written there.
		WRITE (NOUT7,206) I,ITH,N
		WRITE (NOUT8,206) I,ITH,N
		WRITE (NOUT7,129) (VL1(I,ITH,KN),KN=1,N)
		WRITE (NOUT7,129) (STVS(I,ITH,KN),KN=1,N)
		WRITE (NOUT7,129) (CSG(I,ITH,KN),KN=1,N)
		WRITE (NOUT7,129) (VLA(I,ITH,KN),KN=1,N)
		WRITE (NOUT7,129) (PH(I,ITH,KN),KN=1,N)
		WRITE (NOUT8,129) (G(I,ITH,KN),KN=1,N)
		WRITE (NOUT8,129) (TMPR(I,ITH,KN),KN=1,N)
		WRITE (NOUT8,129) (FIN(I,ITH,KN),KN=1,N)
	ELSE
C     This part of the code isolates the star with the accretion disk.
C     Write output to NOUT5 and NOUT6, since those files will be written
C     to the output file first.
		WRITE (NOUT5,206) I,ITH,N
		WRITE (NOUT6,206) I,ITH,N
		WRITE (NOUT5,129) (VL1(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (STVS(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (CSG(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (VLA(I,ITH,KN),KN=1,N)
		WRITE (NOUT5,129) (PH(I,ITH,KN),KN=1,N)
		WRITE (NOUT6,129) (G(I,ITH,KN),KN=1,N)
		WRITE (NOUT6,129) (TMPR(I,ITH,KN),KN=1,N)
		WRITE (NOUT6,129) (FIN(I,ITH,KN),KN=1,N)
	END IF
   42 CONTINUE
C
C     End of long loop, star 3-I
C
C     Normalize the radial velocity
	IF (VNORM2P.GT.0.D0) THEN
		RDVL2P=RDVL2P/VNORM2P
	END IF
      DO ITH=1,NTH
      SMAREA2=SMAREA2+SMA2(ITH)
      END DO
      WRITE (NOUT1,102) I,SMAREA2
	WRITE (NOUT1,112)
	WRITE (NOUT1,113) (SMA2(ITH),ITH=1,NTH)
	DO ITH=1,NTH
	SMAREA2P=SMAREA2P+SMA2P(ITH)
	SMAREC2=SMAREC2+SMA2E(ITH)
	END DO
	WRITE (NOUT1,114) I,SMAREA2P
	WRITE (NOUT1,115)
	WRITE (NOUT1,113) (SMA2P(ITH),ITH=1,NTH)
      WRITE (NOUT1,116) SMAREC2
	WRITE (NOUT1,117)
	WRITE (NOUT1,113) (SMA2E(ITH),ITH=1,NTH)
	WRITE (NOUT1,118)
	DO ITH=1,NTH
		N=NPH(I,ITH)
		WRITE (NOUT1,106) ITH,N
		WRITE (NOUT1,119) (STVS(I,ITH,K),K=1,N)
        WRITE (NOUT1,103)
	END DO
C
C     Write orbital speed
   13   WRITE (NOUT1,109) V0
C     Write radial velocities of stellar components at this orbital longitude
      WRITE (NOUT1,108) VRD1,VRD2
	PHSRV(KS)=PSIT
	RDVL1(KS)=RDVL1P
	RDVL2(KS)=RDVL2P
C     Test replacement values ***************************
C	RDVL1(KS)=VRD1
C	RDVL2(KS)=VRD2
C     ***************************************************
   60 CONTINUE
c        CLOSE (UNIT=29, STATUS='KEEP')
C        IF (IP.LT.1000) GO TO 199
C     Sort the radial velocity points into ascending phase sequence
      DO KS=1,NPSN
		DO J=1,NPSN-1
			IF (PHSRV(J).GT.PHSRV(J+1)) THEN
				SV=PHSRV(J+1)
				PHSRV(J+1)=PHSRV(J)
				PHSRV(J)=SV
				SV=RDVL1(J+1)
				RDVL1(J+1)=RDVL1(J)
				RDVL1(J)=SV
				SV=RDVL2(J+1)
				RDVL2(J+1)=RDVL2(J)
				RDVL2(J)=SV
			END IF
		END DO
	END DO
C     Write out the intensity-weighted radial velocities
      DO KS=1,NPSN
C		WRITE (NOUT2,128) PHSRV(KS),RDVL1(KS),RDVL2(KS)
		WRITE (NOUT2,128) PHSRV(KS),VRD1SV(KS),VRD2SV(KS)
	END DO
	IF (KEY.EQ.0) GO TO 199
C     Now write the output for the stellar data in the sequence needed
C     for program ACPGF6.
C      if (i.lt.1000) go to 199
      REWIND NOUT5
      REWIND NOUT6
      REWIND NOUT7
      REWIND NOUT8
      REWIND NOUT9
C     Read data back in and write output in correct order
C  
      DO 70 KS=1,NPSN
      WRITE (NOUT3,106) NTH,NRM,NSG,NRING
      II=0
      IF (NRM.GT.0) THEN
	 IOB=1
      ELSE
	 IOB=3
      END IF   
	 DO K=1,NRING
	    WRITE (NOUT3,206) IOB,K,NSG
	    READ (NOUT9,129) (PJAFC(K,J),J=1,NSG)
	    READ (NOUT9,129) (PRFH(K,J),J=1,NSG)
	    READ (NOUT9,129) (CSGF(K,J),J=1,NSG)
	    READ (NOUT9,129) (VFACE(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (PJAFC(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (PRFH(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (CSGF(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (VFACE(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (PHA(J),J=1,NSG)
	 END DO
	 DO K=1,NRING
	    WRITE (NOUT4,206) IOB,K,NSG
	    DO J=1,NSG
	       GBFR(J)=GF(K)
	    END DO
	    WRITE (NOUT4,122) (GBFR(J),J=1,NSG)
		  WRITE (NOUT4,122) (TMPRF(K,J),J=1,NSG)
			WRITE (NOUT4,122) (SMV(K,J),J=1,NSG)
			END DO      
	 IOB=IOB+1
	 DO K=1,NRM
	    WRITE (NOUT3,206) IOB,K,NSG
	    READ (NOUT9,129) (PJARM(K,J),J=1,NSG)
	    READ (NOUT9,129) (PRRH(K,J),J=1,NSG)
	    READ (NOUT9,129) (CSGR(K,J),J=1,NSG)
	    READ (NOUT9,129) (VRRM(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (PJARM(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (PRRH(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (CSGR(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (VRRM(K,J),J=1,NSG)
	    WRITE (NOUT3,122) (PHA(J),J=1,NSG)
	 END DO
	 DO K=1,NRM
	    WRITE (NOUT4,206) IOB,K,NSG
	    DO J=1,NSG
	       GBFR(J)=GR(K)
	    END DO
	    WRITE (NOUT4,122) (GBFR(J),J=1,NSG)
	    WRITE (NOUT4,122) (TMPRR(K,J),J=1,NSG)
			WRITE (NOUT4,122) (RMIFLX(K,J),J=1,NSG)
	 END DO 
	IOB=3
      DO ITL=1,NTH
	    READ (NOUT5,206) I,ITH,N
		WRITE (NOUT3,206) IOB,ITH,N
	    READ (NOUT5,129) (VL1(I,ITH,KN),KN=1,N)
	    READ (NOUT5,129) (STVS(I,ITH,KN),KN=1,N)
	    READ (NOUT5,129) (CSG(I,ITH,KN),KN=1,N)
	    READ (NOUT5,129) (VLA(I,ITH,KN),KN=1,N)
	    READ (NOUT5,129) (PH(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (VL1(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (STVS(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (CSG(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (VLA(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (PH(I,ITH,KN),KN=1,N)
	    READ (NOUT6,206) I,ITH,N
	    READ (NOUT6,129) (G(I,ITH,KN),KN=1,N)
	    READ (NOUT6,129) (TMPR(I,ITH,KN),KN=1,N)
		READ (NOUT6,129) (FIN(I,ITH,KN),KN=1,N)
		WRITE (NOUT4,206) IOB,ITH,N
		WRITE (NOUT4,122) (G(I,ITH,KN),KN=1,N)
		WRITE (NOUT4,122) (TMPR(I,ITH,KN),KN=1,N)
		WRITE (NOUT4,122) (FIN(I,ITH,KN),KN=1,N)
      END DO
C     Now do other star      
	IOB=4
      DO ITL=1,NTH
	    READ (NOUT7,206) I,ITH,N
		WRITE (NOUT3,206) IOB,ITH,N
	    READ (NOUT7,129) (VL1(I,ITH,KN),KN=1,N)
	    READ (NOUT7,129) (STVS(I,ITH,KN),KN=1,N)
	    READ (NOUT7,129) (CSG(I,ITH,KN),KN=1,N)
	    READ (NOUT7,129) (VLA(I,ITH,KN),KN=1,N)
	    READ (NOUT7,129) (PH(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (VL1(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (STVS(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (CSG(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (VLA(I,ITH,KN),KN=1,N)
		WRITE (NOUT3,122) (PH(I,ITH,KN),KN=1,N)
	    READ (NOUT8,206) I,ITH,N
	    READ (NOUT8,129) (G(I,ITH,KN),KN=1,N)
	    READ (NOUT8,129) (TMPR(I,ITH,KN),KN=1,N)
		READ (NOUT8,129) (FIN(I,ITH,KN),KN=1,N)
		WRITE (NOUT4,206) IOB,ITH,N
		WRITE (NOUT4,122) (G(I,ITH,KN),KN=1,N)
		WRITE (NOUT4,122) (TMPR(I,ITH,KN),KN=1,N)
		WRITE (NOUT4,122) (FIN(I,ITH,KN),KN=1,N)
      END DO
   70 CONTINUE
C     End of processing
C
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
      WRITE (NOUT1, 205) TM
  199 CLOSE (UNIT=IN1,   STATUS='KEEP')
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
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      CLOSE (UNIT=NOUT6, STATUS='KEEP')
      CLOSE (UNIT=NOUT7, STATUS='KEEP')
      CLOSE (UNIT=NOUT8, STATUS='KEEP')
      CLOSE (UNIT=NOUT9, STATUS='KEEP')
	  OPEN (UNIT=IN12, FILE=FL1(21), STATUS = 'OLD')
	  WRITE (IN12, 120) KEYGOOD
	  CLOSE (UNIT=IN12, STATUS = 'KEEP')
      STOP 'RDVEL2 Completed successfully'
      END  PROGRAM RDVEL2
