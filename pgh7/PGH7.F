      PROGRAM PGH7
C     Program to calculate partial derivatives of binary star system
C     light with respect to longitude of periastron
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION VV(2)
      DIMENSION SP(300),PSJ(300),VCLC(300)
      DIMENSION SMT(300),SBT(300),PSJP(300),SMTP(300),SBTP(300)
      DIMENSION PSJQ(300),SMTQ(300),SBTQ(300)
      DIMENSION VCLCG(300)
      DIMENSION VCLCH(300),WL(5),FL1(13)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 24 FL1
        CHARACTER * 10 DATE,TIME,ZONE
	INTEGER (2) IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA GVAL/6.6704D-8/
C***************************
      DATA IN1,IN2,IN3,IN4,IN5,IN6/1,2,3,4,55,66/
      DATA IN7/7/
      DATA IN8/8/
      DATA IN9,IN10,NOUT1,NOUT2/9,10,11,13/
      DATA KSC1/12/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C***************************
  100 FORMAT (A24)
  101 FORMAT (I2)
  102 FORMAT (1H )      
  103 FORMAT (1H1)      
  104 FORMAT (1X,'PROGRAM PGH4',28X,'DATE=',I2,'/',I2,'/',I4,
     $2X,'TIME=',I2,':',I2,':',I2,'.',I2,/,13X,
     $'***OUT**',/,1X,'*')
  105 FORMAT (1X,'NUMBER OF VALUES OF ORBITAL LONGITUDE=',I5)
  106 FORMAT (10I5)     
  123 FORMAT (6D20.12)
  126 FORMAT (5D22.13)  
  514 FORMAT ( 1X,F8.5,I5,F10.5,8D12.5)
C*********************************************************************
      OPEN (UNIT=   14, FILE='PGH7.ILS', STATUS='OLD')
      READ (14, 100)    (FL1(J),J=1,13)
      OPEN (UNIT=  IN1, FILE= FL1(1), STATUS='OLD')
      WRITE (IN1,101) KEYBAD
      REWIND IN1
      OPEN (UNIT=  IN2, FILE= FL1(2), STATUS='OLD')
      OPEN (UNIT=  IN3, FILE= FL1(3), STATUS='OLD')
      OPEN (UNIT=  IN4, FILE= FL1(4), STATUS='OLD')
      OPEN (UNIT=  IN5, FILE= FL1(5), STATUS='OLD')
      OPEN (UNIT=  IN6, FILE= FL1(6), STATUS='OLD')
      OPEN (UNIT=  IN7, FILE= FL1(7), STATUS='OLD')
      OPEN (UNIT=  IN8, FILE= FL1(8), STATUS='OLD')
      OPEN (UNIT=  IN9, FILE= FL1(9), STATUS='OLD')
      OPEN (UNIT= IN10, FILE=FL1(10), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(11), STATUS='REPLACE')
      OPEN (UNIT= KSC1, FILE=FL1(12), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(13), STATUS='REPLACE')
C*********************************************************************
    4 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (NOUT1,103)   
      WRITE (NOUT1,104)IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
C  
C  
C  
  300 READ (IN4,106) KCTRL,NPSI
      WRITE (NOUT1,105) NPSI
C     INPUT PGA CENTRAL REFERENCE DATA
  301 CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QR,QN,RI,VMI,VVI,VVJ,IN2)
      VV(I)=VVI  
      VV(3-I)=VVJ
      A=QR*QR/(1.0D0+QR)/2.0D0
      RCHPR1=VVI*RI/GVAL/VMI-A
      RCHPR2=VVJ*RI/GVAL/VMI-A
      REWIND IN2
C     INPUT PGC DATA NEEDED FOR NWL AND WAVELENGTH VALUES    
      READ (IN5,106) I,NT,NWL
      READ (IN5,126) CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM,CN
      READ (IN5,126) (WL(K),K=1,NWL)
      REWIND IN5
C*********************************************************************
C     INPUT PGB CENTRAL REFERENCE DATA    
C     INPUT VIA IN3
      CALL RDWT2(NT,NU,NTH,I,IEC,SL,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN3)
      OMRADR=OMRAD
      ECCR=ECC
      REWIND IN3
C*********************************************************************
C     INPUT VIA IN9,PGB DATA FOR LARGER LONG. OF PERIASTRON
C  
      CALL RDWT2(NT,NU,NTH,I,IEC,SL,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN9)
      REWIND IN9
      OMRADA=OMRAD
C
C*********************************************************************
C     INPUT VIA IN10,PGB DATA FOR SMALLER LONG. OF PERIASTRON
C  
      CALL RDWT2(NT,NU,NTH,I,IEC,SL,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN10)
      REWIND IN10
      OMRADB=OMRAD
      DOMRAD=(OMRADA-OMRADB)/2.0D0
C
C**********************************************************************
C  
C     PRINCIPAL LOOP    
C  
C**********************************************************************
C  
   18 DO 90 IWL=1,NWL   
      WLV=WL(IWL)
      IF (ECCR.EQ.0.D0) GO TO 871
C
C**********************************************************************
C      INPUT ON IN6,PGD CENTRAL REFERENCE DATA
C
C     RDWT7 DATA COME FROM PGD AND CONTAIN THE CALCULATED LIGHT VALUES FOR
C     REFERENCE PARAMETERS AT FIDUCIAL PHASES
      DO 80 KS=1,NPSI
      CALL RDWT7(I,IL,VJ,PSJ(KS),SP(KS),SMC,ERC,SMD,ERD,SME,ERE,SMF,ERF, 
     $SMT(KS),SBT(KS),ERT,VCLC(KS),ERR,PSINT,PSEXT,surf1,surf2,KCTRL,
     $IN6)
   80 CONTINUE
C     FIND THE MAX VALUE,VMX
      VMX=-1.0D0
      DO 8 KS=1,NPSI
      IF (VMX.LT.VCLC(KS)) VMX=VCLC(KS)
    8 CONTINUE
C     NORMALIZE TO VMX
      DO 9 KS=1,NPSI
      VCLC(KS)=VCLC(KS)/VMX
    9 CONTINUE
C  
C      INPUT ON IN7 PGD DATA FOR LARGER LONG. OF PERIASTRON
C  
C*******************************************************************
      DO 81 KS=1,NPSI   
      CALL RDWT7(IY,IZ,VIP,PSJP(KS),SPP,SMCP,ERCP,SMDP,ERDP,SMEP,EREP,  
     $SMFP,ERFP,SMTP(KS),SBTP(KS),ERTP,VCLCG(KS),ERRP,PSINTP,PSEXTP,
     $surf1,surf2,KCTRLP,IN7)
   81 CONTINUE
C     FIND MAXIMUM VALUE
      VMX=-1.D+5
      DO KS=1,NPSI
      IF (VMX.LT.VCLCG(KS)) VMX=VCLCG(KS)
      END DO
C     NORMALIZE TO VMX
      DO 12 KS=1,NPSI
      VCLCG(KS)=VCLCG(KS)/VMX
   12 CONTINUE
C
C**********************************************************************
C      INPUT ON IN8 PGD DATA FOR SMALLER LONG. OF PERIASTRON
C  
      DO 82 KS=1,NPSI   
      CALL RDWT7(IY,IZ,VIP,PSJQ(KS),SPP,SMCP,ERCP,SMDP,ERDP,SMEP,EREP,  
     $SMFP,ERFP,SMTQ(KS),SBTQ(KS),ERTQ,VCLCH(KS),ERRP,PSINT,PSEXTP,      
     $surf1,surf2,KCTRLP,IN8)
   82 CONTINUE
C     FIND MAXIMUM VALUE, VMX
      VMX=-1.D+5
      DO KS=1,NPSI
      IF (VMX.LT.VCLCH(KS)) VMX=VCLCH(KS)
      END DO
C     NORMALIZE TO VMX
      DO 14 KS=1,NPSI
      VCLCH(KS)=VCLCH(KS)/VMX
   14 CONTINUE
C     CHECK CONSISTENCY OF PHASES
      DO 870 KS=1,NPSI  
      IF (DABS(PSJ(KS)-PSJP(KS)).GT.0.0000001D0) CALL PMDSTOP
      IF (DABS(PSJ(KS)-PSJQ(KS)).GT.0.0000001D0) CALL PMDSTOP
  870 CONTINUE
C     CALCULATE QUANTITIES NEEDED FOR DERIVATIVES
  871 DO 83 KS=1,NPSI
      DM1=VCLC(KS)-VCLCH(KS)   
      DF0=VCLCG(KS)-VCLC(KS)   
      D1M1=DF0-DM1      
      SM=(DM1+DF0)/2.0D0
      WRITE (NOUT1,514) WLV,I,PSJ(KS),DM1,DF0,D1M1,SM 
      WRITE (NOUT2,123) PSJ(KS),SM     
C  
   84 WRITE (KSC1,123) PSJ(KS),OMRADR,DOMRAD,SM,D1M1
   83 CONTINUE
      WRITE (NOUT1,102)
C  
C**************************************************************************
   90 CONTINUE
      CLOSE (UNIT=  IN2, STATUS='KEEP')
      CLOSE (UNIT=  IN3, STATUS='KEEP')
      CLOSE (UNIT=  IN4, STATUS='KEEP')
      CLOSE (UNIT=  IN5, STATUS='KEEP')
      CLOSE (UNIT=  IN6, STATUS='KEEP')
      CLOSE (UNIT=  IN7, STATUS='KEEP')
      CLOSE (UNIT=  IN8, STATUS='KEEP')
      CLOSE (UNIT=  IN9, STATUS='KEEP')
      CLOSE (UNIT= IN10, STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT= KSC1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      WRITE (IN1,101) KEYGOOD
      CLOSE (UNIT=  IN1, STATUS='KEEP')
   38 STOP 'PGH7 Completed successfully.'
      END  PROGRAM PGH7
