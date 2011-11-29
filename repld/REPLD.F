      PROGRAM REPLD
C     Program to read in file PGDX3.DAT and file SYNPH28.DAT
C     The latter file is the output of synthetic photometry.
C     REPLD then substitutes the photometric data from SYNPH28.DAT
C     for the photometric data in PGDX3.dat and writes the
C     output to PGDX3R.DAT in the same format as PGDX3.DAT.
C     An external routine can copy PGDX3R.DAT to PGDX3.DAT
C     and subsequent routines can treat the synthetic photometry
C     in exactly the same way as normal monochromatic photometry.
      INCLUDE 'COMA.FOR'
      DIMENSION PSJ(MPHS),SP(MPHS),VCLC(MPHS),VLPSI(MPHS)
      DIMENSION VCLCA(MPHS),SMTA(MPHS),SBTA(MPHS),SMCA(MPHS),
     $SMDA(MPHS),
     $SMEA(MPHS),SMFA(MPHS),PSK(MPHS)
      DIMENSION SMT(MPHS),SBT(MPHS),SMC(MPHS),SMD(MPHS),SME(MPHS),
     $SMF(MPHS)
      DIMENSION VLT(MPHS),PHS(MPHS)
      DIMENSION FL1(5)
      CHARACTER * 80 FL1,BFRO
      CHARACTER FMT*80
      DATA IN1,IN2,IN3,IN4,IN5/1,2,3,4,55/
C     READ IN CALCULATED REFERENCE LIGHT CURVE      
C
  100 FORMAT (A36)
  101 FORMAT (10I5)   
  102 FORMAT (7F18.13)
  103 FORMAT (2D20.7)
      OPEN (UNIT=66,      FILE='REPLD.ILS',STATUS='OLD')
      READ (66, 100)      (FL1(J),J=1,5)
      OPEN (UNIT=IN1,    FILE=FL1(1),   STATUS='OLD')
      OPEN (UNIT=IN2,    FILE=FL1(2),   STATUS='OLD')
      OPEN (UNIT=IN3,    FILE=FL1(3),   STATUS='OLD')
      OPEN (UNIT=IN4,    FILE=FL1(4),   STATUS='OLD')
      OPEN (UNIT=IN5,    FILE=FL1(5),   STATUS='OLD')
      READ (IN3,101) NPSI,N1,N2,N3,N4,N5,N6
      READ (IN3,102) (VLPSI(K),K=1,NPSI)
      READ (IN5,101) NPSI,NPST,NPSN,NWL
      DO 50 IWL=1,NWL    
      DO 40 IC=1,NPSI   
C     RDWT7 INPUTS VIA TAPE17  
C     THESE ARE THE DATA WRITTEN VIA WTOUT5 IN PGD  
      CALL RDWT7(I,IL,VI,PSJ(IC),SP(IC),SMC(IC),ERC,SMD(IC),ERD,
     $SME(IC),ERE,SMF(IC),ERF,SMT(IC),SBT(IC),ERT,VCLC(IC),ERR,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,IN1)
      AA=1.D0  
   40 CONTINUE
C     Read in the synthetic photometry data
      READ (IN2, 100) BFRO
      DO 41 IC=1,NPSI
      READ (IN2,103) B,C
      VLT(IC)=B
      PHS(IC)=C
   41 END DO
      BB=1.D0
      DO 42 IC=1,NPSI
      B=VCLC(IC)
      C=VLT(IC)
      VCLC(IC)=VLT(IC)
   42 END DO
      CC=1.D0
      DO 43 IC=1,NPSI
      CALL WTOUT5(I,IWL,VI,PSJ(IC),SP(IC),SMC(IC),ERC,SMD(IC),ERD,
     $SME(IC),ERE,SMF(IC),ERF,SMT(IC),SBT(IC),ERT,VCLC(IC),ERR,THSCP,
     $THFCP,SURF1,SURF2,KCTRL,IN4)
      AA=1.D0
   43 END DO
      DD=1.D0
   50 END DO
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      CLOSE (UNIT = IN2, STATUS = 'KEEP')
      CLOSE (UNIT = IN3, STATUS = 'KEEP')
      CLOSE (UNIT = IN4, STATUS = 'KEEP')
      CLOSE (UNIT = IN5, STATUS = 'KEEP')
      STOP 'REPLD Successfully completed'
      END PROGRAM REPLD
