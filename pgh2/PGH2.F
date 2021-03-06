      PROGRAM PGH2
C     Program to calculate partial derivatives of binary star system
C     light with respect to photospheric potential, component 3-I
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION VV(2),VVA(2),VVB(2)
      DIMENSION SP(300),PSJ(300),VCLC(300),VCLCD(300)
      DIMENSION SMT(300),SBT(300),PSJP(300),SMTP(300),SBTP(300)
      DIMENSION PSJQ(300),SMTQ(300),SBTQ(300)
      DIMENSION VCLCC(300)
      DIMENSION WL(5),FL1(13)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 24 FL1
      CHARACTER * 10 DATE,TIME,ZONE
	INTEGER (2) IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA GVAL/6.6704D-8/
C***************************
      DATA IN1,IN2,IN3,IN4,IN5,IN6/1,2,3,4,55,66/
      DATA IN7,IN8/7,8/
      DATA IN9,IN10/9,10/
      DATA NOUT1,NOUT2/11,13/
      DATA KSC3/12/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C***************************
  100 FORMAT (A24)
  101 FORMAT (I2)
  102 FORMAT (1H )      
  103 FORMAT (1H1)      
  104 FORMAT (1X,'PROGRAM PGH2',28X,'DATE=',I2,'/',I2,'/',I4,
     $2X,'TIME=',I2,':',I2,':',I2,'.',I2,/,13X,
     $'***OUT**',/,1X,'*')
  105 FORMAT (1X,'NUMBER OF VALUES OF ORBITAL LONGITUDE=',I5)
  106 FORMAT (10I5)     
  123 FORMAT (6D20.12)
  126 FORMAT (5D22.13)  
  514 FORMAT ( 1X,F8.5,I5,F10.5,8D12.5)
C*********************************************************************
      OPEN (UNIT=   14, FILE='PGH2.ILS', STATUS='OLD')
      READ ( 14, 100)   (FL1(J),J=1,13)
      OPEN (UNIT=  IN1, FILE= FL1(1),STATUS='OLD')
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
      OPEN (UNIT= IN10, FILE= FL1(10), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE= FL1(11), STATUS='REPLACE')
      OPEN (UNIT= KSC3, FILE= FL1(12), STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE= FL1(13), STATUS='REPLACE')
C*********************************************************************
      CALL SYSTEM_CLOCK(CLOCK1)   ! Get current clock time
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)  ! Get current date and time
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)

      WRITE (NOUT1,103)   
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
C     INPUT PGB CENTRAL REFERENCE DATA    
C     INPUT VIA IN3
      CALL RDWT2(NT,NU,NTH,I,IEC,SL,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN3)
      VIR=VI     
      REWIND IN3
C*********************************************************************
C**********************************************************************
C  
C     INPUT VIA IN9,PGA DATA FOR LOWER POTENTIAL,STAR 3-I
C  
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QSA,QNQ,RI,VMI,VVI,VVJ,IN9)
      REWIND IN9
C     NOTE THAT THE PHOTOSPHERIC POTENTIAL OF COMPONENT 3-I IS INDEPENDENT      
C     OF COMPONENT I ONLY FOR A SEMIDETACHED OR DETACHED SYSTEM   
      IF (QSA.NE.QR) CALL PMDSTOP     
      VVA(3-I)=VVJ*RI/GVAL/VMI-A
C  
C     INPUT VIA IN10,PGA DATA FOR HIGHER POTENTIAL,STAR 3-I
C  
      CALL RDWT1(FL,HFER,I,KC,KL,NT,NTH,MN,QSA,QNQ,RI,VMI,VVI,VVJ,IN10)
      REWIND IN10
C     NOTE THAT THE PHOTOSPHERIC POTENTIAL OF COMPONENT 3-I IS INDEPENDENT      
C     OF COMPONENT I ONLY FOR A SEMIDETACHED OR DETACHED SYSTEM   
      IF (QSA.NE.QR) CALL PMDSTOP     
      VVB(3-I)=VVJ*RI/GVAL/VMI-A
C     CALCULATE DIFF.IN ROCHE POT.,SET DVPT2
      DVPT2=(VVB(3-I)-VVA(3-I))/2.0D0
      I=3-I
C************************************************************************
C  
C     PRINCIPAL LOOP    
C  
C************************************************************************
C  
   18 DO 90 IWL=1,NWL   
      WLV=WL(IWL)
C  
C*************************************************************************
      DO 80 KS=1,NPSI
C     RDWT7 DATA COME FROM PGD AND CONTAIN THE CALCULATED LIGHT VALUES FOR      
C     REFERENCE PARAMETERS AT FIDUCIAL PHASES
C  
C      INPUT ON IN6,PGD CENTRAL REFERENCE DATA
C  
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
C**********************************************************************
C     INPUT PGD DATA BELONGING TO THE PGA DATA ON IN9 FOR THE LOWER
C     POTENTIAL,STAR 3-I
C     INPUT ON IN7
      DO 61 KS=1,NPSI   
      CALL RDWT7(IY,IZ,VIP,PSJP(KS),SPP,SMCP,ERCP,SMDP,ERDP,SMEP,EREP,  
     $SMFP,ERFP,SMTP(KS),SBTP(KS),ERTP,VCLCC(KS),ERRP,PSINTP,PSEXTP,
     $surf1,surf2,KCTRLP,IN7)
   61 CONTINUE
C     DETERMINE THE MAXIMUM VALUE, VMX
      VMX=-1.D+5
      DO KS=1,NPSI
      IF (VMX.LT.VCLCC(KS)) VMX=VCLCC(KS)
      END DO
C     NORMALIZE TO VMX
      DO 40 KS=1,NPSI
      VCLCC(KS)=VCLCC(KS)/VMX
   40 CONTINUE
C
C**********************************************************************
C     THESE DATA BELONG TO THE PGA DATA ON IN10
C     THEY COME FROM THE PGD DATA ON IN8,AND BELONG TO THE HIGHER
C     POTENTIAL,STAR 3-I
      DO 62 KS=1,NPSI   
      CALL RDWT7(IY,IZ,VIP,PSJQ(KS),SPP,SMCP,ERCP,SMDP,ERDP,SMEP,EREP,  
     $SMFP,ERFP,SMTQ(KS),SBTQ(KS),ERTQ,VCLCD(KS),ERRP,PSINT,PSEXTP,      
     $surf1,surf2,KCTRLP,IN8)
   62 CONTINUE
C     FIND THE MAXIMUM VALUE, VMX
      VMX=-1.D+5
      DO KS=1,NPSI
      IF (VMX.LT.VCLCD(KS)) VMX=VCLCD(KS)
      END DO
C     NORMALIZE TO VMX
      DO 42 KS=1,NPSI
      VCLCD(KS)=VCLCD(KS)/VMX
   42 CONTINUE
C     CHECK CONSISTENCY OF PHASES
      DO 670 KS=1,NPSI  
      IF (DABS(PSJ(KS)-PSJP(KS)).GT.0.0000001D0) CALL PMDSTOP 
      IF (DABS(PSJ(KS)-PSJQ(KS)).GT.0.0000001D0) CALL PMDSTOP 
  670 CONTINUE
C     CALCULATE QWUANTITIES NEEDED FOR DERIVATIVES
      DO 63 KS=1,NPSI   
      DM1=VCLC(KS)-VCLCC(KS)   
      DF0=VCLCD(KS)-VCLC(KS)   
      D1M1=DF0-DM1      
      SM=(DM1+DF0)/2.0D0
      WRITE (NOUT1,514) WLV,I,PSJ(KS),DM1,DF0,D1M1,SM
      WRITE (NOUT2,123) PSJ(KS),SM      
C  
   64 WRITE (KSC3,123) PSJ(KS),VV(I),DVPT2,SM,D1M1
   63 CONTINUE   
      WRITE (NOUT1,102)
      I=3-I      
C  
C**********************************************************************
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
      CLOSE (UNIT= KSC3, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      WRITE (IN1, 101) KEYGOOD
      CLOSE (UNIT=  IN1, STATUS='KEEP')
   38 STOP "PGH2: Completed successfully"
      END PROGRAM PGH2
