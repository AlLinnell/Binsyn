      PROGRAM INCREM
C     Program to update parameter input files from DIFCORR output
C
C
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION VL3L(5),TZ1(5),TZ2(5),RFMGA(5),FL1(7)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LBL
      CHARACTER * 36 FL1
      DATA KIN/1/
      DATA IN1,IN2,NOUT1,NOUT2,NOUT3,NOUT4/2,3,4,5,66,7/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
      EL=0.000000001D0
C
  100 FORMAT (A36)
  101 FORMAT (I2)
  106 FORMAT (10I5)
  110 FORMAT (6D20.13)
  111 FORMAT (13X,A7,/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'Update 
     $value of Q',32X,'Q',9X,':',1X,F14.11,/,1X,'Update value of Roche p
     $ot.,comp.I',18X,'OMNU(I)   :',1X,F14.11,/,1X,'Update value of Roch
     $e pot.,comp.3-I',16X,'OMNU(3-I) :',1X,F14.11)
  112 FORMAT (13X,A7,/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'*',/,1X,
     $'Update value of VI',32X,'VLVI',8X,':',F15.11,/,/,
     $1X,'Update value of ECC',32X,'ECC',8X,':',F15.11,/,/,
     $1X,'Update value of OMD',32X,'OMD',8X,':',1X,F15.11)
  113 FORMAT (13X,A7,/,1X,'*',/,1X,'*',/,1X,'*',/,1X,'*',/,1X,
     $'Update value of comp.I limb dark.scaling coeff.',7X,
     $'UC(I)',3X,':',F10.7,/
     $,1X,'Update value of comp.3-I limb dark.scaling coeff.',5X,
     $'UC(3-I) :',F10.7,/,1X,'Update value of TE(I)',33X,'TE(I)',
     $3X,':',F14.9,/,1X,'Update value of TE(3-I)',31X,'TE(3-I) :',F14.9,
     $/,1X,
     $'Update value of bolom.albedo,comp.I',19X,'A(I)',4X,':',F13.9,/,
     $1X,'Update value of bolom.albedo,comp,3-I',17X,'A(3-I)  :',F13.9,
     $/,1X,'Update value of grav.br.coeff.,comp.I',17X,'BC(I)',3X,':',
     $F13.9,/,1X,'Update value of grav.br.coeff.,comp.3-I',15X,
     $'BC(3-I)',1X,':',F13.9)
C**********************************************************************
C
      OPEN (UNIT= 8, FILE='INCREM.ILS',STATUS= 'OLD')
      READ (8,100)       (FL1(J),J=1,7)
      OPEN (UNIT=  KIN,  FILE=  FL1(1),  STATUS= 'OLD')
      WRITE ( KIN, 101) KEYBAD
      REWIND KIN
      OPEN (UNIT=  IN1,  FILE=  FL1(2),  STATUS= 'OLD')
      OPEN (UNIT=  IN2,  FILE=  FL1(3),  STATUS= 'OLD')
      OPEN (UNIT=NOUT1,  FILE=  FL1(4),  STATUS= 'REPLACE')
      OPEN (UNIT=NOUT2,  FILE=  FL1(5),  STATUS= 'REPLACE')
      OPEN (UNIT=NOUT3,  FILE=  FL1(6),  STATUS= 'REPLACE')
      OPEN (UNIT=NOUT4,  FILE=  FL1(7),  STATUS= 'REPLACE')
C**********************************************************************
      READ (IN2,106) I,NT,NWL
      READ (IN1,110) AID,ECC,OMD,POT1,POT2,QS,BA1,BA2,GD1,
     $GD2,TE1,TE2,US1,US2
      READ (IN1,110) (VL3L(K),K=1,NWL)
      READ (IN1,110) (TZ1(K),K=1,NWL)
      READ (IN1,110) (TZ2(K),K=1,NWL)
      READ (IN1,110) (RFMGA(K),K=1,NWL)
      READ (IN1,110) SGMA,SGMAP
      OM(I)=POT1
      OM(3-I)=POT2
      Q=QS
C     Determine whether updated potentials need to be modified
      KEY=0
      CALL SETLAG(I,EL,20)
      IF ((OM(I).GE.OMC(I,1)).AND.(OM(3-I).GE.OMC(I,1))) GO TO 41
      IF ((OM(I).GE.OMC(I,2)).AND.(OM(3-I).GE.OMC(I,2))) GO TO 30
      IF (OM(I).LT.OMC(I,2)) OM(I)=OMC(I,2)
      IF (OM(3-I).LT.OMC(I,2)) OM(3-I)=OMC(I,2)
   35 OM(3-I)=OM(I)
      KEY=1
      GO TO 41
   30 IF ((OM(I).LT.OMC(I,1)).AND.(OM(3-I).LT.OMC(I,1))) GO TO 35
      IF (OM(I).LT.OMC(I,1)) OM(I)=OMC(I,1)
      IF (OM(3-I).LT.OMC(I,1)) OM(3-I)=OMC(I,1)
   41 IF (KEY.EQ.1) OM(3-I)=OM(I)
      POT1=OM(I)
      POT2=OM(3-I)
      LBL='UPDATEA'
      WRITE (NOUT1,111) LBL,QS,POT1,POT2
      LBL='UPDATEB'
      WRITE (NOUT2,112) LBL,AID,ECC,OMD
      LBL='UPDATEC'
      WRITE (NOUT3,113) LBL,US1,US2,TE1,TE2,BA1,BA2,GD1,GD2
      WRITE (NOUT4,110) AID,ECC,OMD,POT1,POT2,QS,BA1,BA2,
     $GD1,GD2,TE1,TE2,US1,US2,VL3L(1),SGMA,SGMAP
      CLOSE (UNIT=  IN1,  STATUS='KEEP')
      CLOSE (UNIT=  IN2,  STATUS='KEEP')
      CLOSE (UNIT=NOUT1,  STATUS='KEEP')
      CLOSE (UNIT=NOUT2,  STATUS='KEEP')
      CLOSE (UNIT=NOUT3,  STATUS='KEEP')
      CLOSE (UNIT=NOUT4,  STATUS='KEEP')
      WRITE ( KIN, 101) KEYGOOD
      CLOSE (UNIT=  KIN,  STATUS='KEEP')
      STOP 'INCREM Completed successfully.'
      END PROGRAM INCREM
