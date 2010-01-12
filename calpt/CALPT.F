      PROGRAM CALPT
C     PROGRAM TO CALCULATE THE PHOTOSPHERIC POTENTIAL(CGS) FOR A        
C     CLOSE BINARY                                                      
C     PARAMETERS TO SPECIFY ARE                                         
C VMI,PER                                                               
C QS                                                                    
C ROCHE POTENTIALS                                                      
C ROTATION PARAMETERS                                                   

      INCLUDE 'COMA.FOR'
      INCLUDE 'COMR.FOR'
      INCLUDE 'COMM.FOR'
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      COMMON /SUN/ SUNRAD, SUNMASS, SUNLUM, SUNMBOL
      CHARACTER * 7 LABEL
      CHARACTER * 20 OBJ
      CHARACTER * 36 FL1,BUF
      CHARACTER * 10 DATE,TIME,ZONE
      INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DIMENSION FL1(3)
      DATA TWPI/6.2831853071796D0/
      DATA GRAV/6.6704D-8/                                              
      DATA SUNRAD/ 6.9599D10   /    !6.9599E10 cm.           Allen,3rd. ed.
      DATA SUNMASS/1.9891D33   /    !1.9891E33 gm.           Allen,3rd. ed.
      DATA SUNLUM/3.8268D33    /    !3.8268E33 erg./cm.**2/s.Allen,3rd. ed.
      DATA SUNMBOL/4.72D0      /    !4.72 solar bol.mag.,    Schmidt
      DATA IN1,IN2,NOUT1/1,2,3/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
C
C     THE FOLLOWING ARE INSTALLATION PARAMETERS     
C
      DATA KL/20/
      DATA I/1/  
C  
C  
C  
  100 FORMAT (A36)
  101 FORMAT (I2)
  103 FORMAT (1H1)
  104 FORMAT (1X,'PROGRAM CALPT',26X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/,13X,'**OUT**',/,1X,'*')
  105 FORMAT (1X,'*')
  106 FORMAT (1X,'Name of object',46X,':',A20)
  108 FORMAT (6F20.12)  
  109 FORMAT (6D20.12)  
  110 FORMAT (1X,'Mass ratio,in the sense(mass of nearer star at phase 0
     $.0)/(mass',/,10X,'of star being eclipsed at phase 0.0)',7X,
     $'Q',7X,':',F12.8)
  111 FORMAT (1X,'Mass of component 1,where component 1=star eclipsed at
     $',/,10X,'phase 0.0,in solar mass units',14X,'VMA',5X,':',F10.6)
  112 FORMAT (1X,'Mass of component 1,in gm.',26X,'VMI',5X,':',E18.11)
  113 FORMAT (1X,'Mass of component 2,where component 2=eclipsing star a
     $t',/,10X,'phase 0.0,in solar mass units',14X,'VMB',5X,':',F10.6)
  114 FORMAT (1X,'Mass of component 2,in gm.',26X,'VM2',5X,':',E18.11)
  115 FORMAT (1X,'Photosphere potential,first component,in cgs units',
     $2X,'VV',6X,':',E18.11)
  116 FORMAT (1X,'Photosphere potential,second component,in cgs units'
     $,1X,'VW',6X,':',E18.11)
  117 FORMAT (1X,'Separation of centers,in solar radii',16X,'AS',6X,
     $':',F12.8)
  118 FORMAT (1X,'Separation of centers,in cm.',24X,'RI',6X,':',E20.13)
  119 FORMAT (1X,'Orbital period,in days',30X,'PER',5X,':',F17.12)
  120 FORMAT (1X,'Roche potential,first component photosphere',9X,
     $'OM(I)',3X,':',F19.13)
  121 FORMAT (1X,'Roche potential,second component photosphere',8X,
     $'OM(3-I)',1X,':',F19.13)
  122 FORMAT (1X,'Filling factor',38X,'FL',6X,':',F8.6)
  123 FORMAT (1X,'Roche potential,inner critical surface',14X,'OMC(I,1):
     $',F13.9)
  124 FORMAT (1X,'Distance,center of first component to L1 point',6X,
     $'VL(I,1)',1X,':',F11.9)
  125 FORMAT (1X,'Distance,center of second component to L1 point',5X,
     $'XMN(3-I):',F11.9)
  126 FORMAT (1X,'Roche potential,outer critical surface',14X,'OMC(I,2):
     $',F13.9)
  127 FORMAT (1X,'Ratio of asynchronous rotation, first component',5X,
     $'FV(I)   :',F10.4)
  128 FORMAT (1X,'Ratio of asynchronous rotation, second component',4X,
     $'FV(3-I) :',F10.4)
  129 FORMAT (1X,'Notes:')
  130 FORMAT (1X,'Reset OM(3-I)=OM(I)')
  131 FORMAT (1X,'Reset components not to overflow inner critical sur
     $face')
  132 FORMAT (1X,'Roche potential, inner critical surface',11X,
     $'OMC(3-I,1):',F13.9,/,1X,'referred to second component center')
  133 FORMAT (1X,'Roche potential, outer critical surface',11X,
     $'OMC(3-I,2):',F13.9,/,1X,'referred to second component center')
  134 FORMAT (1X,'Roche potential,second component photosphere',8X,
     $'OM(3-I)',1X,':',F15.9,/,1X,
     $'referred to second component center')
  135 FORMAT (1X,'Distance, center of first component to ctr. mass',
     $4X,'RCM',5X,':',F13.9)
  136 FORMAT (1X,'Distance, first component to L2 point',15X,
     $'VL(I,2) :',F13.9)
  137 FORMAT (1X,'Distance, second component to L2 point',13X,
     $'VL(3-I,2):',F13.9)
  138 FORMAT (1X,'CGS inner critical potential, first component',7X,
     $'OML11   :',E18.11)
  139 FORMAT (1X,'CGS outer critical potential, first component',7X,
     $'OML12   :',E18.11)
  140 FORMAT (1X,'CGS inner critical potential, second component',6X,
     $'OML21   :',E18.11)
  141 FORMAT (1X,'CGS outer critical potential, second component',6X,
     $'OML22   :',E18.11)
  205 FORMAT (1X,'Program running time=',F12.3,2X,'seconds')
C
C     THE FOLLOWING IS AN INSTALLATION PARAMETER    
      EL=0.000000001D0
      N=1
C  
C  
C
C********************************************
C
      OPEN (UNIT=5,  FILE= 'CALPTA.ILS', STATUS='OLD')
      READ (5, 100,  ERR= 97, END= 98) BUF
      OPEN (UNIT=4,  FILE= BUF, STATUS='OLD')
      READ ( 4, 100, ERR= 97, END= 98) (FL1(J),J=1,3)
      OPEN (UNIT=IN1,   FILE=FL1(1),STATUS='OLD')
      OPEN (UNIT=IN2,   FILE=FL1(2),STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(3),STATUS='REPLACE')
      WRITE (IN1,101) KEYBAD
      REWIND IN1
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (NOUT1,103)
      WRITE (NOUT1,104) IMON,IDAY,IYR,IHR,IMIN,ISEC
C     Input data
C*********************************************
C
      LABEL = '**CALPT'
      LBLERR=0
      NU=IN2
C     Find label and point to first data line
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
C     Read name of object
      CALL CREAD ( OBJ )
      WRITE (NOUT1,106) OBJ
      WRITE (NOUT1,105)
C     Read value of stellar mass
      CALL DREAD (VMI,  'F10.6' )
C     Read orbital period
      CALL DREAD (PER,  'F16.12' )
C     Read mass ratio
      CALL DREAD (Q,  'F16.12' )
C     Read Roche potential,first component
      CALL DREAD (OM(I), 'F16.12' )
C     Read Roche potential,second component
      CALL DREAD (OM(3-I), 'F16.12' )
C     Read ratio of asynchronous rotation, first component
      CALL DREAD (FV(I), 'F11.3')
C     Read ratio of asynchronous rotation, second component
      CALL DREAD (FV(3-I), 'F11.3')
C     Convert solar mass to grams
      VMA=VMI
      VMI=VMI*SUNMASS
      VMB=Q*VMA
      VM2=Q*VMI
      PER=PER*86400.0D0
C     Work with smaller number to avoid overflow
      PE=PER/64000.0D0          !40.0 cubed
      A3=PE/TWPI/TWPI*GRAV*VMI*(1.0D0+Q)*PE
      B=(DLOG(A3))/3.0D0
      RI=DEXP(B)*1600.0D0
      AS=RI/SUNRAD
      CALL SETLAG(I,EL,KL,Q,VL,XMN,OMC,NOUT1)
      A=Q*Q/(1.0D0+Q)/2.0D0
      VV=(OM(I)+A)*GRAV*VMI/RI
      QN=1.D0/Q
      A=QN*QN/(1.D0+QN)/2.D0
C     CALCULATE ROCHE POTENTIAL OF SECOND COMPONENT REFERRED TO ITS
C     CENTER
      OM2=OM(3-I)/Q+(Q-1.D0)/2.D0/Q
      VW=(OM2+A)*GRAV*VM2/RI
      FL=(OMC(I,1)-OM(I))/(OMC(I,1)-OMC(I,2))
      IF (FL.LT.0.0D0) FL=0.0D0
      KY2=0
      KY3=0
      PER=PER/86400.0D0
C     CALCULATE DISTANCE FROM FIRST COMPONENT TO CENTER OF MASS
      RCM=1.D0/(1.D0+Q)
C     CALCULATE CGS POTENTIALS FOR INNER AND OUTER CRITICAL POTENTIALS
C     AROUND BOTH COMPONENTS
      A=GRAV*VMI/RI
      B=Q*Q/2.D0/(1.D0+Q)
      OML11=A*(OMC(I,1)+B)
      OML12=A*(OMC(I,2)+B)
      A=GRAV*VM2/RI
      B=QN*QN/2.D0/(1.D0+QN)
      OML21=A*(OMC(3-I,1)+B)
      OML22=A*(OMC(3-I,2)+B)
C     IF BOTH COMPONENTS ARE INSIDE THE INNER CRITICAL SURFACE,
C     EXIT
      IF ((OM(I).GE.OMC(I,1)).AND.(OM(3-I).GE.OMC(I,1))) GO TO 41
C     IF BOTH COMPONENTS ARE INSIDE THE OUTER CRITICAL SURFACE,
C     GO TO 30
      IF ((OM(I).GE.OMC(I,2)).AND.(OM(3-I).GE.OMC(I,2))) GO TO 30
C     NEITHER COMPONENT CAN BE OUTSIDE THE OUTER CRITICAL SURFACE
      IF (OM(I).LT.OMC(I,2)) THEN 
      OM(I)=OMC(I,2)
      ENDIF
      IF (OM(3-I).LT.OMC(I,2)) THEN 
      OM(3-I)=OMC(I,2)
      ENDIF
   35   OM(3-I)=OM(I)
      KY3=1
C      IF (I.lt.10) STOP
   40 CALL SETLAG(I,EL,KL,Q,VL,XMN,OMC,NOUT1)
      A=Q*Q/(1.0D0+Q)/2.0D0
      VV=(OM(I)+A)*GRAV*VMI/RI
      QN=1.D0/Q
      A=QN*QN/(1.D0+QN)/2.D0
      OM2=OM(3-I)/Q+(Q-1.D0)/2.D0/Q
      VW=(OM2+A)*GRAV*VM2/RI
      FL=(OMC(I,1)-OM(I))/(OMC(I,1)-OMC(I,2))
      IF (FL.LT.0.0D0) FL=0.0D0
   41 WRITE (NOUT1,110) Q
      WRITE (NOUT1,105)
      WRITE (NOUT1,111) VMA
      WRITE (NOUT1,112) VMI
      WRITE (NOUT1,105)
      WRITE (NOUT1,113) VMB
      WRITE (NOUT1,114) VM2
      WRITE (NOUT1,105)
      WRITE (NOUT1,115) VV
      WRITE (NOUT1,116) VW
      WRITE (NOUT1,105)
      WRITE (NOUT1,117) AS
      WRITE (NOUT1,118) RI
      WRITE (NOUT1,105)
      WRITE (NOUT1,119) PER
      WRITE (NOUT1,105)
      WRITE (NOUT1,120) OM(I)
      WRITE (NOUT1,121) OM(3-I)
      WRITE (NOUT1,105)
      WRITE (NOUT1,127) FV(I)
      WRITE (NOUT1,128) FV(3-I)
      WRITE (NOUT1,105)
      WRITE (NOUT1,122) FL
      WRITE (NOUT1,105)
      WRITE (NOUT1,123) OMC(I,1)
      WRITE (NOUT1,124) VL(I,1)
      WRITE (NOUT1,125) XMN(3-I)
      WRITE (NOUT1,126) OMC(I,2)
      WRITE (NOUT1,132) OMC(3-I,1)
      WRITE (NOUT1,133) OMC(3-I,2)
      WRITE (NOUT1,134) OM2
      WRITE (NOUT1,135) RCM
      WRITE (NOUT1,136) VL(I,2)
      WRITE (NOUT1,137) VL(3-I,2)
      WRITE (NOUT1,138) OML11
      WRITE (NOUT1,139) OML12
      WRITE (NOUT1,140) OML21
      WRITE (NOUT1,141) OML22
      WRITE (NOUT1,129)
      IF (KY3.NE.1) GO TO 42
      WRITE (NOUT1,130)
   42 IF (KY2.NE.1) GO TO 99
      WRITE (NOUT1,131)
      GO TO 99
   30 IF ((OM(I).LT.OMC(I,1)).AND.(OM(3-I).LT.OMC(I,1))) GO TO 35
      IF (OM(I).LT.OMC(I,1)) THEN 
	  OM(I)=OMC(I,1)
	  ENDIF
      IF(OM(3-I).LT.OMC(I,1)) THEN 
	  OM(3-I)=OMC(I,1)
	  ENDIF
      KY2=1
      GO TO 40
   99 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      WRITE (*,*) CLOCK1, CLOCK2, COUNT_RATE, REAL(2/1000)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
      WRITE (NOUT1, 205) TM
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      WRITE (IN1, 101) KEYGOOD
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      CLOSE ( 4, STATUS = 'KEEP')
	  CLOSE ( 5, STATUS = 'KEEP')
      STOP 'End of Program'
   97 WRITE ( *, 200)
  200 FORMAT (1X,'Read error on file CALPT.ILS')
      STOP 'Read error on file CALPT.ILS'
   98 WRITE ( *, 201)
  201 FORMAT (1X,'End of file on file CALPT.ILS')
      STOP 'End of file on file CALPT.ILS'
      END PROGRAM CALPT

