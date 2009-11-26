      SUBROUTINE EDCS(C,SC,KFP,WTLEXP,IRANK,BL,BDL,KGPCT,LMPT,NSC1,
     $NSC2,NSC4,NSC5,
     $NIP,PM1,PM2,PM3,PM4,SM1,SM2,SM3,SM4,KY,KTPI1,KTPI2,NRY1,NRY2
     $,NRY3,NRY4,NRY5,NRY6,NRY7,NRY8,NRY9,NRY10,NRY11,NRY12,LAMDAO,
     $LAMDAT,PMINOC,PMINTR,NPTMX,NWL,
     $RSMCSLW,RSESQ,NTO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IORD(50),LWT(5),NIP(5),SOMCSL(5),SOMCSLW(5)
      DIMENSION G2(50),UVL(5),RSOMCSL(5),RSMCSLW(5)
      DIMENSION SCORR(50,50),PCORR(50,50)
      DIMENSION X(51),G(50),A(50,50),C(51),SC(50,50),A2(50,50),
     $TID(50,50)
      DIMENSION VH(13),LV(13)
      DIMENSION B(50),Z(50)
      DIMENSION PHR(300)
      DIMENSION KEY(50),VCLC(300)
      DIMENSION XRY01(11),XRY02(11),XRY03(11),XRY04(11),XRY05(11),
     $XRY06(11),XRY07(11),XRY08(11),XRY09(11),XRY10(11),XRY11(11),
     $XRY12(11)
      DIMENSION RSDV(10000,5)
      COMMON/A/PI,TWOPI,HPI,CF,GVAL
      COMMON/B/PH,DM,VL,WEMP,IORD,VH,LV    
      COMMON /C/PTYPE   
      COMMON/D/VI,ECC,OMD,GAM,VK1,VK2,POT1,POT2,QS,BA1,BA2,GD1,GD2,
     $TP1,TP2,US1,US2,TZ11,TZ21,TZ12,TZ22,TZ13,TZ23,TZ14,TZ24,TZ15,
     $TZ25,VL31,VL32,VL33,VL34,VL35,UVL,CNORM,AID,N,NSIT,NOIT,ISTEP
      COMMON /E/ PHR,VCLC,KEY
      COMMON /F/ XRY01,XRY02,XRY03,XRY04,XRY05,XRY06,XRY07,XRY08,XRY09,  
     $XRY10,XRY11,XRY12,
     $IVI,IVE,IOM,IGAM,IK1,IK2,IPT1,IPT2,IVQ,IBA1,IBA2,IGD1,IGD2,ITP1,
     $ITP2,IUS1,IUS2,ITZ11,ITZ21,ITZ12,ITZ22,ITZ13,ITZ23,ITZ14,ITZ24,
     $ITZ15,ITZ25,IL31,IL32,IL33,IL34,IL35,IRF1,IRF2,IRF3,IRF4,IRF5 
      COMMON /G/ RSDV
      SAVE /A/
      SAVE /B/   
      SAVE /C/   
      SAVE /D/   
      SAVE /E/   
      SAVE /F/
      SAVE /G/   
      REAL LAMDAO,LAMDAT
      INTEGER PTYPE     
C  
C     KMN=0 FOR OCCULTATION AND TRANSIT DATA 
C     KMN=1 FOR OCCULTATION DATA ONLY 
C     KMN=2 FOR TRANSIT DATA ONLY     
C     IF(NOIT.GE.KFP) PRINT OUT DATA ON EACH POINT  
C  
C  
   17 FORMAT (I5,2F12.8,2E20.7)
C     The values of IORD are listed by DIFCORR beginning with label 43   
      IVI=   IORD(1)
      IVE=   IORD(2)
      IOM=   IORD(3)
      IPT1=  IORD(4)
      IPT2=  IORD(5)
      IVQ=   IORD(6)
      IBA1=  IORD(7)
      IBA2=  IORD(8)
      IGD1=  IORD(9)
      IGD2=  IORD(10)
      ITP1=  IORD(11)
      ITP2=  IORD(12)
      IUS1=  IORD(13)
      IUS2=  IORD(14)
      ITZ11= IORD(15)
      ITZ21= IORD(16)
      ITZ12= IORD(17)
      ITZ22= IORD(18)
      ITZ13= IORD(19)
      ITZ23= IORD(20)
      ITZ14= IORD(21)
      ITZ24= IORD(22)
      ITZ15= IORD(23)
      ITZ25= IORD(24)
      IL31=  IORD(25)
      IL32=  IORD(26)
      IL33=  IORD(27)
      IL34=  IORD(28)
      IL35=  IORD(29)
      IRF1=  IORD(30)
      IRF2=  IORD(31)
      IRF3=  IORD(32)
      IRF4=  IORD(33)
      IRF5=  IORD(34)
 1000 FORMAT (10I5)
 1001 FORMAT (10F8.4)
 1003 FORMAT (6D12.5)
 1004 FORMAT (1X,'EDCS')
C     Set phase values for pri. min. contacts 1,2,3,4 as
C     fractions of complete cycle.
      PM1=PM1/TWOPI     
      PM2=PM2/TWOPI     
      PM3=PM3/TWOPI     
      PM4=PM4/TWOPI
C     Set phase values for sec. min. contacts 1,2,3,4 as
C     fractions of complete cycle.           
      SM1=SM1/TWOPI     
      SM2=SM2/TWOPI     
      SM3=SM3/TWOPI     
      SM4=SM4/TWOPI
C     Make sure contacts are within proper phase limits and
C     sequence properly.           
      IF(PM1.GE.1.0D0) PM1=PM1-1.0D0
      IF(PM2.GE.1.0D0) PM2=PM2-1.0D0
      IF(PM3.GE.1.0D0) PM3=PM3-1.0D0
      IF(PM4.GT.1.0D0) PM4=PM4-1.0D0
      IF(PM1.LT.0.0D0) PM1=PM1+1.0D0
      IF(PM2.LT.0.0D0) PM2=PM2+1.0D0
      IF(PM3.LT.0.0D0) PM3=PM3+1.0D0
      IF(PM4.LT.0.0D0) PM4=PM4+1.0D0
      IF(PM3.EQ.0.0D0) PM3=1.0D0
C     Initialize arrays for light curve solution
    2 DO 4 I=1,50
      G(I)=0.0D0   
      DO 3 J=1,50
    3 A(I,J)=0.0D0 
    4 CONTINUE   
C
C     If KY.EQ.0, primary minimum is an occultation.
C     If KY.NE.0, primary minimum is a transit
C  
      IF (KY.EQ.0) GO TO 6     
      GO TO 7
C     Set occultation phase values, pri. min.          
    6 OC1=PM1    
      OC2=PM4    
      OC3=PM2    
      OC4=PM3    
      KTPIO=KTPI1
C     Set transit phase values, sec. min.      
      TR1=SM1    
      TR2=SM4    
      TR3=SM2    
      TR4=SM3    
      KTPIT=KTPI2
      GO TO 8
C     Set occultation phase values, sec. min.          
    7 OC1=SM1    
      OC2=SM4    
      OC3=SM2    
      OC4=SM3    
      KTPIO=KTPI2
C     Set transit phase values, pri. min.      
      TR1=PM1    
      TR2=PM4    
      TR3=PM2    
      TR4=PM3    
      KTPIT=KTPI1
C     Initialize counters for number of points in various
C     phase intervals.     
    8 NO=0
      NOEP=0     
      NOET=0     
      NTEP=0     
      NTET=0     
      NPNUSE=0
C     Set number of fiducial phase points in critical phase 
C     ranges      
      ND1=NRY1
      IF (ND1.EQ.0) ND1=1
      ND2=NRY2
      IF (NRY3.GT.0) THEN
         ND3=NRY3
      ELSE
         ND3=1
      END IF
      IF (NRY4.GT.0) THEN
         ND4=NRY4
      ELSE
         ND4=1
      END IF
      ND5=NRY5
      IF (NRY6.GT.0) THEN
         ND6=NRY6
      ELSE
         ND6=1
      END IF
      IF (NRY7.GT.0) THEN
         ND7=NRY7
      ELSE
         ND7=1
      END IF
      ND8=NRY8
      IF (NRY9.GT.0) THEN
         ND9=NRY9
      ELSE
         ND9=1
      END IF
      IF (NRY10.GT.0) THEN
         ND10=NRY10
      ELSE
         ND10=1
      END IF
      ND11=NRY11
      IF (NRY12.GT.0) THEN
         ND12=NRY12
      ELSE
         ND12=1
      END IF
C  
C     AVLOC=AVE.VALUE OF VL AT TOTAL PHASE OF OCCULTATION  
C     NO=NUMBER OF POINTS OUTSIDE ECLIPSES   
C     NOEP=NUMBER OF POINTS IN OCCULTATION ECLIPSE PARTIAL PHASE  
C     NOET=NUMBER OF POINTS IN OCCULTATION ECLIPSE TOTAL PHASE    
C     NTEP=NUMBER OF POINTS IN TRANSIT ECLIPSE PARTIAL PHASE      
C     NTET=NUMBER OF POINTS IN TRANSIT ECLIPSE TOTAL PHASE 
C     NPNUSE=NUMBER OF POINTS NOT USED
C     PTYPE=0 FOR POINT OUTSIDE ECLIPSE   1 FOR TOTAL PHASE OF
C     OCCULTATION
C     2 FOR PARTIAL PHASE OF OCCULTATION   3 FOR ANNULAR PHASE    
C     4 FOR PARTIAL PHASE OF TRANSIT   5 FOR POINTS NOT ALLOWED
C     Points not allowed are those whose residual from prediction is
C     larger than a specified amount
C  
C  
C  
C     CALCULATE MATRIX OF COEFFICIENT UNKNOWNS      
C     CLASSIFY POINT TYPE      
C     COUNT ACCORDING TO TYPE  
C
  108 FORMAT (3E20.12)
C***************************************************************
C***************************************************************
C     Major wavelength loop for setting up condition equations
    9 DO IWL=1,NWL
      LWT(IWL)=0
      LUL=NIP(IWL)-KGPCT*LMPT
      SYSQ=0.D0
C     Loop for setting up condition equations for a given wavelength
C     LUL is the number of points to include in the solution for
C     the given wavelength.
      DO 80 I=1,LUL
      LCT=KGPCT*LMPT+I
C     Read an observed point.
   71 READ (NSC2,108,END=88) PH ,OBSL,WEMP
C     Test for terminator record for the current wavelength
      IF ((PH.EQ.0.D0).AND.(OBSL.EQ.0.D0)) GO TO 71
C     The input phase is in units of a complete cycle, while
C     the stored critical phase ranges are in units of
C     two pi for a complete cycle.      
      PHS=PH*TWOPI      
      IF (PHS.LT.0.0D0) PHS=PHS+TWOPI
      IF (PHS.GT.TWOPI) PHS=PHS-TWOPI
C     CLASSIFY OBSERVATION INTO SEPARATE COMPUTED SECTIONS OF
C     THEORETICAL LIGHT CURVE
C  
   48 IF ((PHS.GE.XRY01(1)).AND.(PHS.LT.XRY01(ND1))) GO TO 601
      IF ((PHS.GE.XRY02(1)).AND.(PHS.LT.XRY02(ND2))) GO TO 603
      IF ((PHS.GE.XRY03(1)).AND.(PHS.LT.XRY03(ND3))) GO TO 605
      IF ((PHS.GE.XRY04(1)).AND.(PHS.LT.XRY04(ND4))) GO TO 607
      IF ((PHS.GE.XRY05(1)).AND.(PHS.LT.XRY05(ND5))) GO TO 609
      IF ((PHS.GE.XRY06(1)).AND.(PHS.LT.XRY06(ND6))) GO TO 611
      IF ((PHS.GE.XRY07(1)).AND.(PHS.LT.XRY07(ND7))) GO TO 613
      IF ((PHS.GE.XRY08(1)).AND.(PHS.LT.XRY08(ND8))) GO TO 615
      IF ((PHS.GE.XRY09(1)).AND.(PHS.LT.XRY09(ND9))) GO TO 617
      IF ((PHS.GE.XRY10(1)).AND.(PHS.LT.XRY10(ND10))) GO TO 619
      IF ((PHS.GE.XRY11(1)).AND.(PHS.LT.XRY11(ND11))) GO TO 621
      IF ((PHS.GE.XRY12(1)).AND.(PHS.LT.XRY12(ND12))) GO TO 623
      IF (DABS(PHS-XRY12(ND12)).GT.1.D-9) THEN
         WRITE (NTO,1002) PHS,XRY12(ND12)
         GO TO 999
 1002 FORMAT (1X,'END RANGE ERROR', 2F20.13)
      END IF
      GO TO 623  
C    WITHIN PRIMARY MINIMUM,EQUAL TO OR BEYOND PHASE ZERO  
  601 CONTINUE
      GO TO 631
C     PARTIAL PHASE,PRIMARY MINIMUM,EGRESS   
  603 CONTINUE
      GO TO 633
C     BETWEEN END OF PARTIAL PHASE,PRI.MIN.,AND QUADRATURE PHASE  
  605 CONTINUE
      GO TO 635
C     BETWEEN QUADRATURE PHASE AND START OF PARTIAL PHASE,SEC.MIN.
  607 CONTINUE
      GO TO 637
C     PARTIAL PHASE,SEC.MIN.,INGRESS  
  609 CONTINUE
      GO TO 639
C     COMPLETE PHASE,SEC.MIN.,INGRESS 
  611 CONTINUE
      GO TO 641
C     COMPLETE PHASE,SEC.MIN.,EGRESS  
  613 CONTINUE
      GO TO 643
C     PARTIAL PHASE,SEC.MIN.,EGRESS   
  615 CONTINUE
      GO TO 645
C     BETWEEN END OF PARTIAL PHASE,SEC.MIN.,AND QUADRATURE 
  617 CONTINUE
      GO TO 647
C     BETWEEN QUADRATURE PHASE AND PARTIAL PHASE,PRI.MIN.  
  619 CONTINUE
      GO TO 649
C     PARTIAL PHASE,PRI.MIN.,INGRESS  
  621 CONTINUE
      GO TO 651
C     COMPLETE PHASE,PRI.MIN.,INGRESS 
  623 CONTINUE
      VLIGHT=VCLC(LV(13))
      GO TO 655
C     Establish the classification of this point and increment
C     the corresponding type counter.
C     This sequence extends through label 322      
  631 CONTINUE
 4201 IF (KY.EQ.0) PTYPE=1
      IF (KY.NE.0) PTYPE=3     
      IF (KY.EQ.0) NOET=NOET+1 
      IF (KY.NE.0) NTET=NTET+1 
      GO TO 322   
  633 CONTINUE
 4202 IF (KY.EQ.0) PTYPE=2
      IF (KY.NE.0) PTYPE=4     
      IF (KY.EQ.0) NOEP=NOEP+1 
      IF (KY.NE.0) NTEP=NTEP+1 
      GO TO 322   
  635 CONTINUE
 4203 PTYPE=0
      NO=NO+1    
      GO TO 322   
  637 CONTINUE
 4204 PTYPE=0
      NO=NO+1    
      GO TO 322   
  639 CONTINUE
 4205 IF (KY.EQ.0) PTYPE=4     
      IF (KY.NE.0) PTYPE=2     
      IF (KY.EQ.0) NTEP=NTEP+1 
      IF (KY.NE.0) NOEP=NOEP+1 
      GO TO 322   
  641 CONTINUE
 4206 IF (KY.EQ.0) PTYPE=3
      IF (KY.NE.0) PTYPE=1     
      IF (KY.EQ.0) NTET=NTET+1 
      IF (KY.NE.0) NOET=NOET+1 
      GO TO 322   
  643 CONTINUE
 4207 IF (KY.EQ.0) PTYPE=3
      IF (KY.NE.0) PTYPE=1     
      IF (KY.EQ.0) NTET=NTET+1 
      IF (KY.NE.0) NOET=NOET+1 
      GO TO 322   
  645 CONTINUE
 4208 IF (KY.EQ.0) PTYPE=4
      IF (KY.NE.0) PTYPE=2     
      IF (KY.EQ.0) NTEP=NTEP+1 
      IF (KY.NE.0) NOEP=NOEP+1 
      GO TO 322   
  647 CONTINUE
 4209 PTYPE=0
      NO=NO+1    
      GO TO 322   
  649 CONTINUE
 4210 PTYPE=0
      NO=NO+1    
      GO TO 322   
  651 CONTINUE
 4211 IF (KY.EQ.0) PTYPE=2
      IF (KY.NE.0) PTYPE=4     
      IF (KY.EQ.0) NOEP=NOEP+1 
      IF (KY.NE.0) NTEP=NTEP+1 
      GO TO 322   
  653 CONTINUE
  655 IF (KY.EQ.0) PTYPE=1
      IF (KY.NE.0) PTYPE=3     
      IF (KY.EQ.0) NOET=NOET+1 
      IF (KY.NE.0) NTET=NTET+1 
  322 CONTINUE
C     End of sequence for establishing classification for 
C     current point
C  
C     Read theoretical light value and corres. phase
      READ (NSC4,17,END=323) IDX,PHSRF,BF1,VLIGHT,BF2
C     Read light derivatives and corresponding phase      
      READ (NSC5,666)PHSVL,DLDI,DLDECC,DLDOM,DLDPT1,DLDPT2,DLDQ,DLDA1,
     $DLDA2,DLDB1,DLDB2,DLDT1,DLDT2,DLDUC1,DLDUC2,DLDTZP,DLDTZS,DLDL3
C     Make sure derivatives and theor. light are for the same
C     phase.
      IF (DABS(PHSRF-PHSVL).GT.1.D-10) GO TO 600
C     If the theor. light and observed light are for the same
C     phase, continue. Otherwise flag the error and stop.      
      IF (DABS(PHSRF-PH).LT.0.000001D0) GO TO 323
  600 WRITE (NTO,324) PH,PHSRF,PHSVL,IWL,I
      CALL PMDSTOP
  324 FORMAT (1X,'*** VALUE OF OBSERVED PHASE=',F8.5,4X,'CALC.PHASE=',
     +F8.5,4X,'PHSVL=',F8.5,4X,'IWL=',I4,4X,'ORDINAL LOCATION=',I5)
  666 FORMAT (8D15.8)
C  
C     Continuation  
C     OBSL IS THE OBSERVED LIGHT VALUE,RELATIVE TO THE REFERENCE
C     MAGNITUDE VLIGHT IS THE CALCULATED LIGHT VALUE
C     VL31 IS THE THIRD LIGHT CONTRIBUTION AT ORDINAL WAVELENGTH IWL     
C     RSD IS THE LIGHT RESIDUAL  
  323 RSD=OBSL-VLIGHT
C     First wavelength setup
      IF (IWL.EQ.1) THEN
         DLDTZP1=DLDTZP
         DLDTZP2=0.D0
         DLDTZP3=0.D0
         DLDTZP4=0.D0
         DLDTZP5=0.D0
         DLDTZS1=DLDTZS
         DLDTZS2=0.D0
         DLDTZS3=0.D0
         DLDTZS4=0.D0
         DLDTZS5=0.D0
         DLDL31=DLDL3
         DLDL32=0.D0
         DLDL33=0.D0
         DLDL34=0.D0
         DLDL35=0.D0
         OBSL1=OBSL
         OBSL2=0.D0
         OBSL3=0.D0
         OBSL4=0.D0
         OBSL5=0.D0
      END IF
C     Second wavelength setup      
      IF (IWL.EQ.2) THEN
         DLDTZP1=0.D0
         DLDTZP2=DLDTZP
         DLDTZP3=0.D0
         DLDTZP4=0.D0
         DLDTZP5=0.D0
         DLDTZS1=0.D0
         DLDTZS2=DLDTZS
         DLDTZS3=0.D0
         DLDTZS4=0.D0
         DLDTZS5=0.D0
         DLDL31=0.D0
         DLDL32=DLDL3
         DLDL33=0.D0
         DLDL34=0.D0
         DLDL35=0.D0
         OBSL1=0.D0
         OBSL2=OBSL
         OBSL3=0.D0
         OBSL4=0.D0
         OBSL5=0.D0
      END IF
C     Third wavelength setup      
      IF (IWL.EQ.3) THEN
         DLDTZP1=0.D0
         DLDTZP2=0.D0
         DLDTZP3=DLDTZP
         DLDTZP4=0.D0
         DLDTZP5=0.D0
         DLDTZS1=0.D0
         DLDTZS2=0.D0
         DLDTZS3=DLDTZS
         DLDTZS4=0.D0
         DLDTZS5=0.D0
         DLDL31=0.D0
         DLDL32=0.D0
         DLDL33=DLDL3
         DLDL34=0.D0
         DLDL35=0.D0
         OBSL1=0.D0
         OBSL2=0.D0
         OBSL3=OBSL
         OBSL4=0.D0
         OBSL5=0.D0
      END IF
C     Fourth wavelength setup      
      IF (IWL.EQ.4) THEN
         DLDTZP1=0.D0
         DLDTZP2=0.D0
         DLDTZP3=0.D0
         DLDTZP4=DLDTZP
         DLDTZP5=0.D0
         DLDTZS1=0.D0
         DLDTZS2=0.D0
         DLDTZS3=0.D0
         DLDTZS4=DLDTZS
         DLDTZS5=0.D0
         DLDL31=0.D0
         DLDL32=0.D0
         DLDL33=0.D0
         DLDL34=DLDL3
         DLDL35=0.D0
         OBSL1=0.D0
         OBSL2=0.D0
         OBSL3=0.D0
         OBSL4=OBSL
         OBSL5=0.D0
      END IF
C     Fifth wavelength setup      
      IF (IWL.EQ.5) THEN
         DLDTZP1=0.D0
         DLDTZP2=0.D0
         DLDTZP3=0.D0
         DLDTZP4=0.D0
         DLDTZP5=DLDTZP
         DLDTZS1=0.D0
         DLDTZS2=0.D0
         DLDTZS3=0.D0
         DLDTZS4=0.D0
         DLDTZS5=DLDTZS
         DLDL31=0.D1
         DLDL32=0.D0
         DLDL33=0.D0
         DLDL34=0.D0
         DLDL35=DLDL3
         OBSL1=0.D0
         OBSL2=0.D0
         OBSL3=0.D0
         OBSL4=0.D0
         OBSL5=OBSL
      END IF
C     End of separate wavelength setup sequence
C     The following sequence establishes the light derivatives 
C     array required for the solution for parameter increments.
C     Note that the arguments for the X array are the inclusion
C     parameters set in the control table DIFFCOR.INF. The
C     parameters must be in integer sequence for the X array to
C     set up properly for a soluton. At least two parameters
C     must be selected for inclusion in the solution. The
C     quantity CFX can control the stiffness of the corrections
C     that are calculated. If CFX is set to a value larger than
C     1.0, smaller parameter increments will be calculated. This
C     option could be used if the initial parameter correction
C     tends to overshoot the required corection. Note that
C     different CFX values could be used for different 
C     parameters. Note that the DIFCORR.INF parameter FRA also
C     provides control by applying only the fraction FRA of the
C     calculated increment to the next value, for all parameters.      
      CFX=1.0D0
      X(IVI)=DLDI/CFX
      X(IVE)=DLDECC/CFX
      X(IOM)=DLDOM/CFX
      X(IPT1)=DLDPT1/CFX
      X(IPT2)=DLDPT2/CFX 
      X(IVQ)=DLDQ/CFX    
      X(IBA1)=DLDA1/CFX  
      X(IBA2)=DLDA2/CFX  
      X(IGD1)=DLDB1/CFX  
      X(IGD2)=DLDB2/CFX  
      X(ITP1)=DLDT1/CFX  
      X(ITP2)=DLDT2/CFX  
      X(IUS1)=DLDUC1/CFX 
      X(IUS2)=DLDUC2/CFX 
      X(ITZ11)=DLDTZP1/CFX
      X(ITZ21)=DLDTZS1/CFX
      X(ITZ12)=DLDTZP2/CFX
      X(ITZ22)=DLDTZS2/CFX
      X(ITZ13)=DLDTZP3/CFX
      X(ITZ23)=DLDTZS3/CFX
      X(ITZ14)=DLDTZP4/CFX
      X(ITZ24)=DLDTZS4/CFX
      X(ITZ15)=DLDTZP5/CFX
      X(ITZ25)=DLDTZS5/CFX
      X(IL31)=DLDL31/CFX
      X(IL32)=DLDL32/CFX
      X(IL33)=DLDL33/CFX
      X(IL34)=DLDL34/CFX
      X(IL35)=DLDL35/CFX
      X(IRF1)=-OBSL1
      X(IRF2)=-OBSL2
      X(IRF3)=-OBSL3
      X(IRF4)=-OBSL4
      X(IRF5)=-OBSL5
C     Note that the theoretical light values, as well as the observed
C     light values, go through all phases for a given wavelength
C     before going to the next wavelength.
      VL=VLIGHT
      TL=VL
C     BDL is a normalization factor to divide the light values
C     by before calculating a weight for the solution.
      IF (BDL.EQ.0.D0) GO TO 999
C     BL is a control for omitting deviant points from the solution      
      IF((DABS(RSD)-BL).GE.0.0D0) GO TO 79
C     Calculate weight for the current point      
      WL=( TL    /BDL)**(2.D0*WTLEXP)
C     Set the value of Y, the standard symbol used in
C     Linnell, 1989, ApJ, 342, 449      
      Y=RSD
      IF (WL.EQ.0.D0) GO TO 999
C     Calculate sum of squares of residuals from current
C     theoretical light curve.      
      SYSQ=SYSQ+Y**2*WEMP   /WL
      GO TO 790  
C     POINTS NOT USEABLE      (PTYPE=5)      
   79 PTYPE   =5 
      NPNUSE=NPNUSE+1   
  790 WRITE (NSC1) DLDI,DLDECC,DLDOM,DLDPT1,DLDPT2,
     $DLDQ,DLDA1,DLDA2,DLDB1,
     $DLDB2,DLDT1,DLDT2,DLDUC1,DLDUC2,DLDTZP1,DLDTZS1,DLDTZP2,DLDTZS2,
     $DLDTZP3,DLDTZS3,DLDTZP4,DLDTZS4,DLDTZP5,DLDTZS5,
     $DLDL31,DLDL32,DLDL33,DLDL34,DLDL35,
     $PTYPE,VL,WEMP,PH,OBSL1,OBSL2,OBSL3,OBSL4,OBSL5, TL,VLIGHT,RSD,WL
      LWT(IWL)=LWT(IWL)+1
      IF (PTYPE.EQ.5) GO TO 80
C     Enter current point in condition equations.
C     Array G is in standard format from 
C     Linnell 1989, ApJ, 342, 449.      
   70 DO 75 L=1,IRANK   
      G(L)=G(L)+X(L)*Y   *WEMP   /WL  
      DO 74 M=L,IRANK
C     The Linnell paper uses the symbol D for what here is symbol A.
C     See eq. 13 of paper.         
      A(L,M)=A(L,M)+X(L)*X(M)*WEMP   /WL     
   74 A(M,L)=A(L,M)     
   75 CONTINUE
C     Completion of entry in condition equations for current
C     point.      
   80 CONTINUE
C     End of loop for current wavelength   
   78 END DO
C     End of loop over all wavelengths. Now ready to begin
C     solution for parameter increments. Note that the solution
C     includes points for all wavelengths.   
C***************************************************************
C***************************************************************
   88 ENDFILE NSC1
C  
C  
C  
C     CALCULATE A INVERSE      
C
C     NEWN is the number of points used in the solution  
      NEWN=NOEP+NOET+NTEP+NTET+NO
C     Save copy of array G.           
      DO 84 I=1,50
   84 G2(I)=G(I) 
C     DISCONTINUE SOLUTION IF NUMBER OF POINTS IS INSUFFICIENT    
      IF (NEWN.LT.10) GO TO 990
C     WE NOW HAVE THE NORMAL EQUATIONS IN THE MATRIX FORM AC=G
C     Calculate sum of squares of entries in A matrix.
c     Used in test for incipient indeterminacy.
      SELMSQ=0.0D0 
   89 DO 91 I=1,IRANK   
      DO 90 J=1,IRANK   
      SELMSQ=SELMSQ+A(I,J)**2
C     Save current values of matrix A        
   90 A2(I,J)=A(I,J)    
   91 CONTINUE
C      
    1 FORMAT (' ')
      WRITE (NTO,200)   
  200 FORMAT(' MATRIX A=MATRIX OF COEFF. IN NORMAL EQUATIONS',/,
     1' MATRIX C=COLUMN MATRIX OF UNKNOWNS TO BE DETERMINED',/,   
     2' MATRIX G=COLUMN MATRIX OF OBSERVED QUANTITIES',/,  
     3' MATRIX EQUATION OF FORM AC=G IS TO BE SOLVED',/,   
     4' PROGRAM CALCULATES A INVERSE,THEN C=(A INVERSE)AC=(A INVERSE)G'
     5,/,' PROGRAM CHECKS (A INVERSE)A=I',/)
  115 WRITE (NTO,105)   
  105 FORMAT(1H ,'  MATRIX A',/)
      CALL WTMTXA(A,IRANK,NTO)
      WRITE (NTO,280)
  280 FORMAT (/,1H ,'  MATRIX G',/)
      DO I=1,IRANK
      WRITE (NTO,109) G(I)
      END DO
      GO TO 125
  109 FORMAT(1H ,D11.4)
  111 FORMAT(1H ,7(D11.4,1X))
C***************************************************************
C     MATIN SOLVES AC=G FOR C
  125 CALL MATIN(A,IRANK,G,1,DETERM)
C***************************************************************
C     THE MATRIX A HAS NOW BEEN REPLACED BY ITS INVERSE AND THE MATRIX
C     G HAS NOW BEEN REPLACED BY C 
C     DETERM contains the value of the determinant of the array A
      TRANK=IRANK
      RDET=DETERM**(2.D0/TRANK)
C     Set value of RSDET, measures approach to indeterminacy.
C     See discussion on P.452 of Linnell, 1989, ApJ, 342, 449.      
      RSDET=SELMSQ/RDET
C     TID initially contains product of array A by its inverse.
  131 DO 135 I=1,IRANK  
      DO 134 J=1,IRANK  
      TID(I,J)=0.0D0
      DO 133 K=1,IRANK  
  133 TID(I,J)=TID(I,J)+A(I,K)*A2(K,J)
  134 CONTINUE   
  135 CONTINUE
C     TID initially should have off-diagonal elements of 0.0
C     and diagonal elements of 1.0. Now subtract 1.0 from
C     diagonal elements. Should produce null matrix.     
      DO 150 I=1,IRANK  
  150 TID(I,I)=TID(I,I)-1.D0
      WRITE (NTO,160)   
  160 FORMAT(/,1H ,'  A INVERSE',/)
C     Write out matrix  A(-1)
      CALL WTMTXA(A,IRANK,NTO)
      WRITE (NTO,281)
  281 FORMAT (/,1H ,'  MATRIX C',/)
      DO I=1,IRANK
      WRITE (NTO,109) G(I)
      END DO
      GO TO 206
  206 WRITE (NTO,207)   
  207 FORMAT(/,1H ,'  (A INVERSE)A-I',/)
C     Write out what should be close to null matrix  
      CALL WTMTXA(TID,IRANK,NTO)
  208 FORMAT(1H ,7(D12.5,1X))
  209 CONTINUE
C     Set entries in solution array C  
      DO 215 I=1,50
      IF(IORD(I).GT.50) GO TO 214
      C(I)=G(IORD(I)) 
      GO TO 215  
  214 C(I)=0.0D0
  215 CONTINUE
C     Set up array for correlation coeffs. Enter elements of inverse
C     matrix in matrix SC.    
      DO 217 I=1,IRANK  
      DO 216 J=I,IRANK  
      SC(I,J)=A(I,J)    
      SC(J,I)=A(I,J)
  216 CONTINUE    
  217 CONTINUE
C  
C     PRINT OUT INFORMATION ON EACH DATA POINT,THROUGH NPTMX POINTS
C     ONLY
C
C***************************************************************
C***************************************************************
  218 SUMSQ=0.0D0
      SUMLTSQ=0.0D0
      SQLT=0.0D0
      TN=NEWN
C     Set sum of squares of weighted residuals (Y^2 values) in SDSQ          
      SDSQ=SYSQ
C        
      DO 221 I=1,IRANK
C     Calculate S' by eq. 16 of Linnell, 1989, ApJ, 342, 449.        
      SDSQ=SDSQ-G(I)*G2(I)     
  221 CONTINUE
C     Calculate "calculated" std. dev. of residuals. See eq. 18 of
C     paper, where we apply the condition S=S' to within calculated
C     error.  
      SIGMA=DSQRT (DABS(SDSQ)/(TN-IRANK))
      SIGMASQ=SIGMA*SIGMA
      IF(NOIT.LT.KFP) GO TO 219
      IF (I.GT.NPTMX) GO TO 219
      WRITE (NTO,283)
  283 FORMAT (1H )
      WRITE (NTO,220)   
  220 FORMAT(1H ,'   TYPE   PHASE   OBS.LIGHT CAL.LIGHT    DIFF       WL
     $       YEST      DIFF')
      LINCT=0    
  219 KRT=1      
      REWIND NSC1
      REWIND NSC2
      KICT=0
C
C*********************************************************************      
C     Loop over all wavelengths      
      DO IWL=1,NWL
C     LUL=number of points used for the current wavelength      
      LUL=LWT(IWL)
      IF (LMPT.LT.LUL) LUL=LMPT
      DO 270 I=1,LUL    
      LCT=KICT*LMPT+I   
      READ (NSC1,END=231) DLDI,DLDECC,DLDOM,DLDPT1,
     $DLDPT2,DLDQ,DLDA1,DLDA2,DLDB1,DLDB2,DLDT1,DLDT2,DLDUC1,DLDUC2,
     $DLDTZP1,DLDTZS1,DLDTZP2,DLDTZS2,DLDTZP3,DLDTZS3,DLDTZP4,DLDTZS4,
     $DLDTZP5,DLDTZS5,DLDL31,DLDL32,DLDL33,DLDL34,DLDL35,
     $PTYPE,VL,WEMP,PH,OBSL1,OBSL2,OBSL3,OBSL4,OBSL5,TL,VLIGHT,RSD,WL
      GO TO 232  
  231 LCT=LCT-1  
  232 IF(NOIT.LT.KFP) GO TO 230
  230 PHASE=PH*TWOPI
C     If this point is not usable do not include it.      
      IF (PTYPE.EQ.5) GO TO 250
C     Calculate that part of estimate of residual that is independent
C     of wavelength. See the L.H.S. of eq. 10 of differentials
C     correction paper.
      YEST=DLDI*C(1)+DLDECC*C(2)+DLDOM*C(3)+
     $DLDPT1*C(4)+DLDPT2*C(5)+
     $DLDQ*C(6)+DLDA1*C(7)+DLDA2*C(8)+DLDB1*C(9)+
     $DLDB2*C(10)+DLDT1*C(11)+DLDT2*C(12)+DLDUC1*C(13)+
     $DLDUC2*C(14)
C     Addition for first wavelength
      IF (IWL.EQ.1) THEN
      YEST=YEST+DLDTZP1*C(15)+DLDTZS1*C(16)+
     $DLDL31*C(25)
     $+UVL(1)*C(30)
      END IF
C     Addition for second wavelength      
      IF (IWL.EQ.2) THEN
      YEST=YEST+
     $DLDTZP2*C(17)+DLDTZS2*C(18)+
     $DLDL32*C(26)+
     $UVL(2)*C(31)
      END IF
C     Addition for third wavelength      
      IF (IWL.EQ.3) THEN
      YEST=YEST+
     $DLDTZP3*C(19)+
     $DLDTZS3*C(20)+
     $DLDL33*C(27)+
     $UVL(3)*C(32)
      END IF
C     Addition for fourth wavelength      
      IF (IWL.EQ.4) THEN
      YEST=YEST+
     $DLDTZP4*C(21)+DLDTZS4*C(22)+
     $DLDL34*C(28)+
     $UVL(4)*C(33)
      END IF
C     Addition for fifth wavelength
      IF (IWL.EQ.5) THEN
      YEST=YEST+
     $DLDTZP5*C(23)+DLDTZS5*C(24)+
     $DLDL35*C(29)+
     $UVL(5)*C(34)
      END IF
C     Now have completed calculation of YEST
C     Y=residual using current parameters      
      Y=RSD
C     DIFF=residual using incremented parameters            
      DIFF=Y-YEST
      SQLT=SQLT+RSD**2  
      SUMLTSQ=SUMLTSQ+DIFF**2
C     Calculate weighted sum of squares of residuals using incremented
C     parameters.Note that as convergence occurs, the elements of the
C     C matrix approach 0.0, so the values of YEST approach 0.0 and
C     DIFF approaches coincidence with Y.        
      SUMSQ=SUMSQ+DIFF**2*WEMP/WL
      IF (IWL.EQ.1) OBSL=OBSL1
      IF (IWL.EQ.2) OBSL=OBSL2
      IF (IWL.EQ.3) OBSL=OBSL3
      IF (IWL.EQ.4) OBSL=OBSL4
      IF (IWL.EQ.5) OBSL=OBSL5
  243 IF(NOIT.LT.KFP) GO TO 270
      IF (I.GT.NPTMX) GO TO 270
  530 WRITE (NTO,244) LCT,PTYPE,PH,OBSL,TL,RSD,WL,YEST,
     $DIFF
  244 FORMAT(I4,1X,I2,1X, 9(F9.6,1X))
      GO TO 270
C     POINT NOT USED    
  250 IF(NOIT.LT.KFP) GO TO 270
      WRITE (NTO,251) LCT,PHASE,TL    
  251 FORMAT(1H ,I4,2X,3H***,1X,F9.6,2X,F10.7)
  270 CONTINUE
      END DO
C***************************************************************
C***************************************************************
      KICT=KICT+1
      KTST=NIP(IWL)-(KICT+1)*LMPT
      IF (KTST.LT.0) LUL=NIP(IWL)-KICT*LMPT
C  
C
C     Calculate estimated std. dev. of residuals for next iteration.  
  271 SESQ=SUMSQ/(NEWN-IRANK)  
      AVSUM=SUMLTSQ/TN  
      DO 368 II=1,IRANK
      DO 367 J=II,IRANK
C     Multiply element of inverse matrix by square of estimated
C     std. dev. of residuals. See eq. 20 of Linnell paper.      
      SC(II,J)=SC(II,J)*SESQ
C     Calculate correlation coefficients. See eq. 22 of Linnell paper.      
      SCORR(II,J)=SC(II,J)/DSQRT(SC(II,II)*SC(J,J))
      SCORR(J,II)=SCORR(II,J)
  367 SC(J,II)=SC(II,J)
  368 CONTINUE   
      WRITE (NTO,4550)  
      WRITE (NTO,362) SUMLTSQ,AVSUM   
  362 FORMAT(1H ,'SUM OF SQUARES OF LIGHT RESIDUALS',24X,
     $'SUMLTSQ=',E12.5,/
     2' MEAN RESIDUAL SQUARED (LIGHT)',29X,'AVSUM =',E12.5)
  360 WRITE (NTO,361) SUMSQ,IRANK,SESQ,SIGMASQ,DETERM,RSDET,      
     2SELMSQ,RDET
      WRITE (NTO,363)
  361 FORMAT(1H ,'SUM OF SQUARES OF DIFF COLUMN,WEIGHTED,',/,
     $' (DELTA(DELTA L))*WEMP/WL',30X,'    SUMSQ',1X,'=',E12.5,/,
     $' ENTRY ON PREVIOUS LINE DIVIDED BY(N-',I2,')',20X,
     $'SESQ =',E12.5,/,
     $' SIGMA**2',49X,'SIGMASQ=',E12.5,/,
     $' DETERMINANT OF COEFFICIENTS IN NORMAL EQUATIONS',10X,
     3'DETERM =',E12.5,/,
     4' RATIO OF SUM OF SQUARES OF ELEMENTS OF A TO M/2TH ROOT',/,
     $' OF DETERMINANT OF A',45X,'=',E12.5,/,
     6' SUM OF SQUARES OF ELEMENTS OF A',33X,'=',E12.5,/,
     7' M/2TH ROOT OF DETERMINANT OF A',34X,'=',E12.5,/)
  363 FORMAT(1X,'IF THE FLOATING POINT EXPONENT IN THE LAST PREVIOUS NUM
     $BER IS OF THE ORDER OF THE NUMBER OF',/,1X,
     $'DIGITS IN THE COMPUTER WORD LENGTH, MATRIX A IS CLOSE TO SINGULAR
     $')   
  370 WRITE (NTO,371)   
  371 FORMAT(     //,' IN FOLLOWING MATRIX,THE ENTRIES ON THE PRINCIPAL  
     1DIAGONAL ARE THE SQUARES',/,    ' OF THE STANDARD DEVIATIONS ASSOC 
     2IATED WITH THE ENTRIES IN THE COLUMN MATRIX OF',/,
     3' UNKNOWNS,C.',/,
     4        ' THE OFF-DIAGONAL ENTRIES ARE THE COVARIANCES ASSOCIATED
     5WITH THE SAME COLUMN',/,' MATRIX,WRITTEN AS C(I,J)',/)
      CALL WTMTXA(SC,IRANK,NTO)
  373 FORMAT(1H ,10(D11.4,2X))
  386 WRITE (NTO,385)   
  385 FORMAT(//' MATRIX OF SIMPLE CORRELATION COEFFICIENTS WRITTEN AS SC 
     2ORR(I,J)',/)      
      CALL WTMTXA(SCORR,IRANK,NTO)
  393 DO 389 II=1,50
      B(II)=0.0D0
  389 CONTINUE
C***************************************************************
      CALL MATIN(SCORR,IRANK,B,0,DTSCORR)
C***************************************************************
C     SCORR IS NOW SCORR INVERSE      
      WRITE (NTO,391) DTSCORR  
  391 FORMAT (/,' DETERMINANT OF MATRIX OF SIMPLE CORRELATION COEFFICIEN 
     2TS=',E11.3)
C     Calculate partial correlation coeficients. See eq. 23 of
C     Linnell paper.
      DO 388 II=1,IRANK
      DO 387 J=1,IRANK
      PCORR(II,J)=-SCORR(II,J)/DSQRT (SCORR(II,II)*SCORR(J,J))
      PCORR(J,II)=PCORR(II,J)
  387 CONTINUE
  388 CONTINUE   
      WRITE (NTO,392)
  392 FORMAT(//' MATRIX OF PARTIAL CORRELATION COEFFICIENTS WRITTEN AS P 
     2CORR(I,J)',/)     
      CALL WTMTXA(PCORR,IRANK,NTO)
  396 WRITE (NTO,395) KY,KTPI1,KTPI2,PMINOC,PMINTR,LAMDAO,LAMDAT  
  395 FORMAT(/,' KY  =',I1,'(PRIMARY ECLIPSE (OCCULTATION=0,TRANSIT=1))'
     2/,' KTPI1=',I2,'(TYPE OF PRIMARY ECLIPSE (TOTAL=0,PARTIAL=1,NONE=
     2-1))',/,' KTPI2=',I2,
     2' (TYPE OF SECONDARY ECLIPSE (TOTAL=0,PARTIAL=1,NONE=-1))',/,      
     3' MINIMUM P (OCCULTATION)   PMINOC=',F11.7,/,
     4' MINIMUM P (TRANSIT)       PMINTR=',F11.7,/,
     5' LIGHT FOR MINIMUM P (OCC) LAMDAO=',F11.7,/, 
     6' LIGHT FOR MINIMUM P (TRA) LAMDAT=',F11.7)   
      WRITE (NTO,376) OC1,TR1,OC3,TR3,OC4,TR4,OC2,TR2      
  376 FORMAT(//,' PHASES OF EXTERNAL AND INTERNAL CONTACT',/,' OC1=',
     1F9.6,9X,' TR1=',F9.6,/,' OC2=',F9.6,9X,' TR2=',F9.6,/,' OC3=',
     2F9.6,9X,' TR3=',F9.6,/,' OC4=',F9.6,9X,' TR4=',F9.6)
      REWIND NSC1
C
C     Calculate weighted and unweighted sums of squares of residuals
C     by separate colors.      
      DO IWL=1,NWL
      LUL=LWT(IWL)
      SOMCSL(IWL)=0.D0
      SOMCSLW(IWL)=0.D0
C     LUL is number of observations for the current wavelength            
  380 DO 382 II=1,LUL
      READ (NSC1,END=383) (Z(KKK),KKK=1,29),PTYPE,VL,WEMP,PH,OBSL1,
     $OBSL2,OBSL3,OBSL4,OBSL5,TL,VLIGHT,RSD,WL
C     Calculate unweighted sum of squares of residuals     
      SOMCSL(IWL)=SOMCSL(IWL)+RSD*RSD
C     Calculate weighted sum of squares of residuals      
      SOMCSLW(IWL)=SOMCSLW(IWL)+RSD**2*WEMP/WL
C     Save RSD
      RSDV(II,IWL)=RSD      
  382 CONTINUE
  383 END DO
C
C     Calculate related std. dev. of residuals by separate colors
      DO IWL=1,NWL
      LUL=LWT(IWL)
      RSOMCSL(IWL)=DSQRT (SOMCSL(IWL)/(LUL-IRANK))
      RSMCSLW(IWL)=DSQRT(SOMCSLW(IWL)/(LUL-IRANK))
      END DO
C      
  384 WRITE (NTO,4536) NEWN
      WRITE (NTO,4537) NO      
      WRITE (NTO,4538) NOEP,NOET,NTEP,NTET   
      WRITE (NTO,4539) SUMLTSQ,SQLT   
      WRITE (NTO,4540) AVSUM
C     Calculate estimated std. dev. of residuals         
      RSESQ=DSQRT (SESQ)
      WRITE (NTO,4541) SUMSQ,RSESQ    
 4541 FORMAT(1H ,'SUM OF SQUARES OF DIFF COLUMN WEIGHTED,',/,
     $'(DELTA(DELTA L))**2*WEMP/WL='9X,'SUMSQ =',E13.5,2X,/,20X,
     $'STD.DEVIATION',4X,'RSESQ =',E13.5,/)
C     
      DO IWL=1,NWL
      WRITE (NTO,4544) IWL,SOMCSL(IWL),RSOMCSL(IWL),SOMCSLW(IWL),
     $RSMCSLW(IWL)
      END DO
C      
 4544 FORMAT(1H ,' ORDINAL WAVELENGTH NUMBER',20X,I2,/
     $'SUM OF SQUARES OF LIGHT RESIDUALS',10X,'=',E13.5,2X,
     $/,20X,'STD.DEVIATION',10X,'=',E13.5,/,
     $' SUM OF SQUARES OF WEIGHTED LIGHT RESIDUALS=',
     $E13.5,/,20X,'STD.DEVIATION',10X,'=',E13.5,/)
 4536 FORMAT(' NUMBER OF POINTS USED IN FITTING PARAMETERS     =',I6)
 4537 FORMAT(' NUMBER OF POINTS OUTSIDE ECLIPSE',16X,'=',I6)
 4538 FORMAT(' NUMBER OF POINTS IN PARTIAL PHASE OF OCCULTATION=',I6,
     1/,' NUMBER OF POINTS IN TOTAL PHASE OF OCCULTATION  =',I6,/,
     2' NUMBER OF POINTS IN PARTIAL PHASE OF TRANSIT    =',I6,/,
     3' NUMBER OF POINTS IN TOTAL PHASE OF TRANSIT      =',I6,/)
 4539 FORMAT(' SUM OF RESIDUALS SQUARED (LIGHT)',10X,'=',E13.5,/,16X,
     $'      (OLD VALUE)',10X,'=',E13.5)
 4540 FORMAT(' MEAN RESIDUAL SQUARED (LIGHT)',13X,'=',E13.5)
 4550 FORMAT(1H )
      GO TO 999  
C  
  990 WRITE (NTO,991)   
  991 FORMAT(' THERE WERE AN INSUFFICIENT NUMBER OF POINTS FOR SOLUTION' 
     1)   
      NSIT=1     
  999 RETURN     
      END 
