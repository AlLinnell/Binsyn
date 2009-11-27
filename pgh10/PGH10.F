      PROGRAM PGH10		
C	USE MSFLIB
C     Program to calculate partial derivatives of binary star system
C     light with respect to bolometric albedo, component I
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION SP(300),PSJ(300),VCLC(300)
      DIMENSION SMT(300),SBT(300),PSJP(300),SMTP(300),SBTP(300)
      DIMENSION PSJQ(300),SMTQ(300),SBTQ(300),VCLCA(300),VCLCB(300)
      DIMENSION WL(90),FL1(11)
	DIMENSION TE(2),T0(2),A(2),BCF(2),UA(2),
     $UC(2),C1(2),C2(2),WLA(90),T0A(2),AA(2),
     $BCFA(2),UAA(2),UCA(2),C1A(2),C2A(2),
     $WLB(90),T0B(2),AB(2),BCFB(2),UAB(2),
     $UCB(2),C1B(2),C2B(2),TEA(2),TEB(2)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 24 FL1
	INTEGER (4) RESULT
	INTEGER (2) IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
C***************************
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,5,6,7/
      DATA IN8/8/
      DATA IN9,NOUT1/9,10/
      DATA KSC2/11/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C***************************
  100 FORMAT (A24)
  101 FORMAT (I2)
  102 FORMAT (1H )      
  103 FORMAT (1H1)      
  104 FORMAT (1X,'PROGRAM PGH10',28X,'DATE=',I2,'/',I2,'/',I4,
     $2X,'TIME=',I2,':',I2,':',I2,'.',I2,/,13X,
     $'***OUT**',/,1X,'*')
  105 FORMAT (1X,'NUMBER OF VALUES OF ORBITAL LONGITUDE=',I5)
  106 FORMAT (10I5)     
  123 FORMAT (6D20.12)
  126 FORMAT (6D20.13)  
  514 FORMAT ( 1X,F8.5,I5,F10.5,8D12.5)
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=   12, FILE='C:\LGTSYN\PGH10\PGH10.ILS', STATUS='OLD')
      READ (12, 100)    (FL1(J),J=1,11)
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
      OPEN (UNIT=NOUT1, FILE=FL1(10), STATUS='REPLACE')
      OPEN (UNIT= KSC2, FILE=FL1(11), STATUS='REPLACE')
C*********************************************************************
    4 CONTINUE
C    4 CALL GETDAT (IYR,IMON,IDAY)
C      CALL GETTIM (IHR,IMIN,ISEC,I100TH)
      WRITE (NOUT1,103)   
      WRITE (NOUT1,104)IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
C  
C  
C  
  300 READ (IN4,106) KCTRL,NPSI
      WRITE (NOUT1,105) NPSI
C     INPUT PGC CENTRAL REFERENCE DATA
  301 CALL RDWT3(I,NT,NWL,WL,AX,BX,CX,DX,EX,FX,GX,
     $ZX,ZZ,HX,BA,BB,UAZ,UBZ,IN2)
	UC(I)=AX
	UC(3-I)=BX
	C1(I)=CX
	C2(I)=DX
	C1(3-I)=EX
	C2(3-I)=FX
	T0(I)=GX
	T0(3-I)=ZX
	A(I)=ZZ
	A(3-I)=HX
	BCF(I)=BA
	BCF(3-I)=BB
	UA(I)=UAZ
	UA(3-I)=UBZ
	TE(I)=T0(I)
	TE(3-I)=T0(3-I)
      REWIND IN2
C*********************************************************************
C     INPUT PGB CENTRAL REFERENCE DATA    
C     INPUT VIA IN3
      CALL RDWT2(NT,NU,NTH,I,IEC,SL,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,IN3)
      VIR=VI
      REWIND IN3
C*********************************************************************
C     INPUT VIA IN8,PGC DATA FOR HIGHER BOLOM. ALBEDO, STAR I
C  
      CALL RDWT3(I,NTA,NWLA,WLA,UCA(I),UCA(3-I),
     $C1A(I),C2A(I),C1A(3-I),C2A(3-I),T0A(I),T0A(3-I),AA(I),AA(3-I),
     $BCFA(I),BCFA(3-I),UAA(I),UAA(3-I),IN8)
      REWIND IN8
	TEA(I)=T0A(I)
	TEA(3-I)=T0A(3-I)
C
C*********************************************************************
C     INPUT VIA IN9,PGC DATA FOR LOWER BOLOM. ALBEDO, STAR I
C  
      CALL RDWT3(I,NTB,NWLB,WLB,UCB(I),UCB(3-I),
     $C1B(I),C2B(I),C1B(3-I),C2B(3-I),T0B(I),T0B(3-I),AB(I),AB(3-I),
     $BCFB(I),BCFB(3-I),UAB(I),UAB(3-I),IN9)
      REWIND IN9
	TEB(I)=T0B(I)
	TEB(3-I)=T0B(3-I)
C     CALCULATE DIFF. IN BOLOM. ALBEDO, SET DVT1
      DVT1=(AA(I)-AB(I))/2.D0
C
C**********************************************************************
C  
C     PRINCIPAL LOOP    
C  
C**********************************************************************
C  
   18 DO 90 IWL=1,NWL   
      WLV=WL(IWL)
C
C**********************************************************************
C      INPUT ON IN5,PGD CENTRAL REFERENCE DATA
C
C     RDWT7 DATA COME FROM PGD AND CONTAIN THE CALCULATED LIGHT VALUES FOR
C     REFERENCE PARAMETERS AT FIDUCIAL PHASES
      DO 80 KS=1,NPSI
      CALL RDWT7(I,IL,VJ,PSJ(KS),SP(KS),SMC,ERC,SMD,ERD,SME,ERE,SMF,ERF, 
     $SMT(KS),SBT(KS),ERT,VCLC(KS),ERR,PSINT,PSEXT,surf1,surf2,KCTRL,
     $IN5)
   80 CONTINUE
C     FIND THE MAX VALUE,VMX
      VMX=-1.0D+5
      DO 8 KS=1,NPSI
      IF (VMX.LT.VCLC(KS)) VMX=VCLC(KS)
    8 CONTINUE
C     NORMALIZE TO VMX
      DO 9 KS=1,NPSI
      VCLC(KS)=VCLC(KS)/VMX
    9 CONTINUE
C
C******************************************************************
C
C     INPUT PGD DATA BELONGING TO THE PGD DATA ON IN8 FOR THE HIGHER
C     BOLOM.ALBEDO, STAR I  
C     INPUT ON IN6 
C  
C*******************************************************************
      DO 51 KS=1,NPSI   
      CALL RDWT7(IY,IZ,VIP,PSJP(KS),SPP,SMCP,ERCP,SMDP,ERDP,SMEP,EREP,  
     $SMFP,ERFP,SMTP(KS),SBTP(KS),ERTP,VCLCA(KS),ERRP,PSINTP,PSEXTP,
     $surf1,surf2,KCTRLP,IN6)
   51 CONTINUE
C     FIND MAXIMUM VALUE
      VMX=-1.D+5
      DO KS=1,NPSI
      IF (VMX.LT.VCLCA(KS)) VMX=VCLCA(KS)
      END DO
C     NORMALIZE TO VMX
      DO 12 KS=1,NPSI
      VCLCA(KS)=VCLCA(KS)/VMX
   12 CONTINUE
C
C**********************************************************************
C      THE FOLLOWING DATA BELONG TO THE PGC DATA ON IN9
C      THE DATA COME FROM PGD VIA IN7, AND BELONG TO THE LOWER
C      BOLOM. ALBEDO, STAR I
C  
      DO 52 KS=1,NPSI   
      CALL RDWT7(IY,IZ,VIP,PSJQ(KS),SPP,SMCP,ERCP,SMDP,ERDP,SMEP,EREP,  
     $SMFP,ERFP,SMTQ(KS),SBTQ(KS),ERTQ,VCLCB(KS),ERRP,PSINT,PSEXTP,      
     $surf1,surf2,KCTRLP,IN7)
   52 CONTINUE
C     FIND MAXIMUM VALUE, VMX
      VMX=-1.D+5
      DO KS=1,NPSI
      IF (VMX.LT.VCLCB(KS)) VMX=VCLCB(KS)
      END DO
C     NORMALIZE TO VMX
      DO 14 KS=1,NPSI
      VCLCB(KS)=VCLCB(KS)/VMX
   14 CONTINUE
C     CHECK CONSISTENCY OF PHASES
      DO 530 KS=1,NPSI  
      IF (DABS(PSJ(KS)-PSJP(KS)).GT.0.0000001D0) CALL PMDSTOP
      IF (DABS(PSJ(KS)-PSJQ(KS)).GT.0.0000001D0) CALL PMDSTOP
  530 CONTINUE
C     CALCULATE QUANTITIES NEEDED FOR DERIVATIVES
      DO 53 KS=1,NPSI
      DM1=VCLCA(KS)-VCLC(KS)   
      DF0=VCLC(KS)-VCLCB(KS)   
      D1M1=DM1-DF0      
      SM=(DM1+DF0)/2.0D0
      WRITE (NOUT1,514) WLV,I,PSJ(KS),DM1,DF0,D1M1,SM      
C  
   54 WRITE (KSC2,123) PSJ(KS),A(I),DVT1,SM,D1M1
   53 CONTINUE
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
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT= KSC2, STATUS='KEEP')
      WRITE (IN1,101) KEYGOOD
      CLOSE (UNIT=  IN1, STATUS='KEEP')
   38 STOP
      END  PROGRAM PGH10
