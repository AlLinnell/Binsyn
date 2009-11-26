C     *****************************************
C     File COMA.FOR
C     *****************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     
      PARAMETER (NTHM    =   101,   ! maximum number of + latitudes
     $           MPTHM   =   101,   ! maximum number of long. to pi
     $           MTHM    =   201,	  ! maximum total latitudes
     $           MPHM    =   201,   ! maximum number of long., complete circle
     $           MQTM    = 10101,   ! maximum number of points, 1/4 component
     $           MTLM    = 40401,   ! maximum number of points, full component
     $           MNKP    =    30,   ! max. number of points, neck lat. profile
     $		   NTHD    =   101,
     $           MPTHD   =   101,
     $           MTHD    =   201,
     $           MPHD    =   201,
     $           MQTD    = 10101,
     $           MTLD    = 40401,
     $           MNKD    =    30,
     $		   MPHS    =   300,
     $           MWL     =     5,
     $           MOBS    = 10000,
     $           MDPTH   =    70,
     $		   MSPT    =    10,
     $		   MTW     =     2,
     $           MTR     =     3,
     $		   MFR     =     4,
     $           MFV     =     5,
     $           MSX     =     6,
     $           MET     =     8,
     $           MTN     =    10)
C     First COMMON block
      DIMENSION FV(2),VL(2,2),OMC(2,2)
      DIMENSION VX(2),VYZ(2),BKT1(2),XMN(2),ALMX(2),ALMY(2)
      DIMENSION OM(2),ALM(2,MQTM),ANU(2,NTHM),VR(2,MQTM),POPR(2,MQTM)
      DIMENSION POPL(2,MQTM),POPN(2,MQTM)
      DIMENSION G(2,MQTM),SNT(2,NTHM),ZL(2,MQTM),ZM(2,MQTM),ZN(2,MQTM)
      DIMENSION CSB(2,MQTM),AMU(2,MQTM)
      DIMENSION TH(2,NTHM)
      DIMENSION NPH(2,MPTHM),NTL(2,MPTHM),MPH(2,MTHM),MTL(2,MTHM)
	DIMENSION RVI(MPTHM),GVRDI(MPTHM),GVTHI(MPTHM),RV3MI(MPTHM),
     $GVRD3MI(MPTHM),GVTH3MI(MPTHM)
      COMMON /CA/ Q,FV,VL,OMC,
     $VX,VYZ,BKT1,VOM,OM,EL,ZMN,XMN,YMN,ALMX,ALMY, 
     $ALM,ANU,VR,POPR,POPL,POPN,
     $G,SNT,ZL,ZM,ZN,CSB,AMU,SNP,CSP,    
     $PLPT,PMPT,PNPT,PLPP,PMPP,POPT,POPP,POPX,POPY,POPZ,   
     $TH,NPH,NTL,MPH,MTL,RVI,GVRDI,GVTHI,RV3MI,GVRD3MI,GVTH3MI
C
C     Second COMMON block
      DIMENSION VST(2,MTHM,3),VND(2,MTHM,3),CSG(2,MTLM)
      DIMENSION SHS(MTHM,3),SHN(MTHM,3)   
      DIMENSION PHMXA(2,MTHM,4),GMXA(2,MTHM,4),PHMNA(2,MTHM,4),
     $GMNA(2,MTHM,4)    
      DIMENSION THP(2),PHP(2),THM(2),PHM(2),TFP(2),PFP(2),TFM(2),PFM(2)  
      DIMENSION PH(2,MQTM)
      DIMENSION DSG(2,MQTM)
C      DOUBLE PRECISION CM,VL0,VM0,VN0,VI,PSI,SHP,SHM,PSHP,PSHM,
C     $SHUP,PHUP,SHDN,PHDN
      COMMON /CB/ PH,CM,VL0,VM0,VN0,VI,PSI,VST,VND,CSG,SHS,SHN,    
     $THP,PHP,THM,PHM,PHMXA,GMXA,PHMNA,GMNA,  
     $TFP,PFP,TFM,PFM,SHP,SHM,PSHP,PSHM,      
     $DSG,SHUP,PHUP,SHDN,PHDN
C
C     Third COMMON block
      DIMENSION RS(MTHM,3),ALP(MTHM,3),XR(MTHM),YR(MTHM),XF(MTHM),
     $YF(MTHM)
	DIMENSION ARYA(MTHM,3),ARYB(MTHM,3),ARYC(MNKP,3)
	DIMENSION ARYAS(MTHM,3),ARYBS(MTHM,3),ARYCS(MNKP,3)
	DIMENSION ALPS(MTHM,3),RSS(MTHM,3)
	DIMENSION XHZB(MTHM),YHZB(MTHM),XHZE(MTHM),YHZE(MTHM),XNK(MNKP),
     $YNK(MNKP)
	DIMENSION XS(MTHM,3),XN(MTHM,3),YS(MTHM,3),YN(MTHM,3)
	DIMENSION ERXT(MTHM),PHXT(MTHM),THRY(MTHM),ERA(MTHM),VNA(MTHM),
     $VNG(MTHM),ERV(MTHM)
	COMMON /CH/ RS,ALP,XR,YR,XF,YF,ARYA,ARYB,ARYC,ARYAS,ARYBS,ARYCS,
     $ALPS,RSS,XHZB,YHZB,XHZE,
     $YHZE,XNK,YNK,XS,XN,YS,YN,ERXT,PHXT,THRY,ERA,VNA,VNG,ERV
C    
C     Fourth COMMON block
      DIMENSION VL1(2,MTLM),VL2(2,MTLM),VL3(2,MQTM)   
      DIMENSION BDI(2,MQTM),BDJ(2,MQTM),U1(2,MQTM),U2(2,MQTM)    
      DIMENSION S(2,MQTM),TMPR(2,MQTM),TMPRA(2,MQTM),FIN(2,MQTM),
     $FARY(MQTM)      
      COMMON /CC/ BDI,BDJ,U1,U2,S,TMPR,TMPRA,FIN,FARY,    
     $VL1,VL2,VL3   
C     
C     Fifth COMMON block
      DIMENSION RWL(14),RTMP(22),RGRAV(10),U1R(10,22,14),
     $U2R(10,22,14),RFLX(2,5,6,MTLM),WLVRY(MTLM)
      COMMON /CD/ RWL,RTMP,RGRAV,U1R,U2R,RFLX,WLVRY
C     
C     Sixth COMMON block
C     In the following arrays, the second dimension should be set to 
C     the maximum number of grid points on the complete photosphere of
C     a single stellar component.
C
      DIMENSION PLA1(2,MTLM),PLB1(2,MTLM),PLU(2,MTLM),PLTE(2,MTLM)
      COMMON /CE/ PLA1,PLB1,PLU,PLTE
C
C     Seventh COMMON block
C     In the following arrays, the dimension should be set to the
C     maximum number of grid points on two octants of a given stellar
C     component.
      DIMENSION RVLT(MQTD),ALT(MQTD),AMT(MQTD),ANT(MQTD),AGL(MQTD)
C     In the following arrays, the dimension should be set to the
C     maximum number of grid points on the complete photosphere of a
C     given stellar component.
      DIMENSION RVL(MTLD),AL(MTLD),AM(MTLD),AN(MTLD),VPH(MTLD),
     $PX(MTLD),
     $PY(MTLD),
     $PZ(MTLD),XV(MTLD),YV(MTLD),AZ(MTLD),QX(MTLD),QY(MTLD),
     $QZ(MTLD)
      COMMON /CS/RVLT,ALT,AMT,ANT,AGL,RVL,AL,AM,AN,VPH,PX,PY,PZ,XV,YV,
     $AZ,QX,QY,QZ
