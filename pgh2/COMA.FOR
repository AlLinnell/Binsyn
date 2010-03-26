C     *****************************************
C     File COMA.FOR
C     *****************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     
      PARAMETER (MTHM    =   201,   ! maximum number of colatitudes
     $           MPHM    =   201,   ! maximum number of longitudes
     $           MTLM    = 40401,   ! maximum number of points, full component
     $           MNKP    =    30,   ! max. number of points, neck lat. profile
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
      DIMENSION OM(2),ALM(2,MTHM,MPHM),ANU(2,MTHM,MPHM),
     $VR(2,MTHM,MPHM),POPR(2,MTHM,MPHM)
      DIMENSION POPL(2,MTHM,MPHM),POPN(2,MTHM,MPHM)
      DIMENSION G(2,MTHM,MPHM),SNT(2,MTHM),ZL(2,MTHM,MPHM),
     $ZM(2,MTHM,MPHM),ZN(2,MTHM,MPHM)
      DIMENSION CSB(2,MTHM,MPHM),AMU(2,MTHM,MPHM)
      DIMENSION TH(2,MTHM)
      DIMENSION NPH(2,MTHM),NTL(2,MTHM),MPH(2,MTHM),MTL(2,MTHM)
      COMMON /CA/ Q,FV,VL,OMC,
     $VX,VYZ,BKT1,VOM,OM,EL,ZMN,XMN,YMN,ALMX,ALMY, 
     $ALM,ANU,VR,POPR,POPL,POPN,
     $G,SNT,ZL,ZM,ZN,CSB,AMU,SNP,CSP,    
     $PLPT,PMPT,PNPT,PLPP,PMPP,POPT,POPP,POPX,POPY,POPZ,   
     $TH,NPH,NTL,MPH,MTL,RVI,GVRDI,GVTHI,RV3MI,GVRD3MI,GVTH3MI
C
C     Second COMMON block
      DIMENSION VST(2,MTHM,3),VND(2,MTHM,3),CSG(2,MTHM,MPHM)
      DIMENSION SHS(MTHM,3),SHN(MTHM,3)   
      DIMENSION PHMXA(2,MTHM,4),GMXA(2,MTHM,4),PHMNA(2,MTHM,4),
     $GMNA(2,MTHM,4)    
      DIMENSION THP(2),PHP(2),THM(2),PHM(2),TFP(2),PFP(2),TFM(2),PFM(2)  
      DIMENSION PH(2,MTHM,MPHM)
      DIMENSION DSG(2,MTHM,MPHM)
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
      DIMENSION VL1(2,MTHM,MPHM),VL2(2,MTHM,MPHM),VL3(2,MTHM,MPHM)   
      DIMENSION BDI(2,MTHM,MPHM),BDJ(2,MTHM,MPHM),U1(2,MTHM,MPHM),
     $U2(2,MTHM,MPHM)    
      DIMENSION S(2,MTHM,MPHM),TMPR(2,MTHM,MPHM),TMPRA(2,MTHM,MPHM),
     $FIN(2,MTHM,MPHM),FARY(MTLM)      
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
      DIMENSION PLA1(2,MTHM,MPHM),PLB1(2,MTHM,MPHM),PLU(2,MTHM,MPHM),
     $PLTE(2,MTHM,MPHM)
      COMMON /CE/ PLA1,PLB1,PLU,PLTE
C
C     Seventh COMMON block
C     In the following arrays, the dimension should be set to the
C     maximum number of grid points on the complete photosphere of a
C     given stellar component.
      DIMENSION 
     $PX(MTLM),
     $PY(MTLM),
     $PZ(MTLM),XV(MTLM),YV(MTLM),QX(MTLM),QY(MTLM),
     $QZ(MTLM)
      COMMON /CS/PX,PY,PZ,XV,YV,
     $QX,QY,QZ
