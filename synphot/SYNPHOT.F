      PROGRAM SYNPHOT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Program to weight a spectrum by photometric response
C     functions and integrate the product to produce simulated
C     photometric quantities.
C     Input files as follows:
C     NOTE!! The U,B,V,R, and I filters are files interpolated to 1 Angstrom spacing
C     by an external program. The SYNPHOT.INF file includes corresponding counts of
C     the number of data lines. SYNPHOT assumes any filter actually used has been
C     interpolated to 1 Angstrom spacing. Thus, the currently listed filters for
C     units IN8-IN14 would need to be replaced for actual use by SYNPHOT.
C     IN1     SYNPHOT.INF information file
C     IN2     PHTUBE.DAT phototube response function
C     IN3     UFLT.DAT U filter response function
C     IN4     BFLT.DAT B filter response function
C     IN5     VFLT.DAT V filter response function
C     IN6     RFLT.DAT R filter response function
C     IN7     IFLT.DAT I filter response function
C     IN8     j2mass.dat J filter response function
C     IN9     h2mass.dat H filter response function
C     IN10    k2mass.dat K filter response function
C     IN11    LFLTRES.DAT L filter response function
C     IN12    MFLTRES.DAT M filter response function
C     IN13    FUV.DAT currently the FUV filter response function
C     IN14    NUV.DAT currently the NUV filter response function
C     IN15    PGBX3SV.DAT the list of orbital longitudes.
C     Output files are as follows:
C     NOUT1	Output log. Provides running time.
C     NOUT2	First filter transmission function.
C             Typically, U filter.
C     NOUT3	Product function, 1st filter and spectrum (first long.only)
C     NOUT4	Second filter transmission function.
C             Typically, B filter. 
C     NOUT5	Product function, 2nd filter and spectrum (1st long.)
C     NOUT6	Third filter transmission function.
C             Typically, V filter.
C     NOUT7	Product function, 3rd filter and spectrum (1st long.)
C     NOUT8	Fourth filter transmission function.
C             Typically R filter.
C     NOUT9	Product function, 4th filter and spectrum (1st long.)
C     NOUT10  Fifth filter transmission function
C             Typically I filter
C     NOUT11  Product function, 5th filter and spectrum (1st long.)
C     NOUT12  Sixth filter transmission function
C             Typically J filter
C     NOUT13  Product function, 6th filter and spectrum (1st long.)
C     NOUT14  Seventh filter transmission function
C             Typically H filter
C     NOUT15  Product function, 7th filter and spectrum (1st long.)
C     NOUT16  Eighth filter transmission function
C             Typically K_s filter
C     NOUT17  Product function, 8th filter and spectrum (1st long.)
C     NOUT18  Ninth filter transmission function
C             Typically L filter
C     NOUT19  Product function, 9th filter and spectrum (1st long.)
C     NOUT20  Tenth filter transmission function
C             Typically M filter
C     NOUT21  Product function, 10th filter and spectrum (1st long.)
C     NOUT22  Eleventh filter transmission function
C             Typically FUV filter
C     NOUT23  Product function, 11th filter and spectrum (1st long.)
C     NOUT24  Twelvth filter transmission function
C             Typically NUV filter
C     NOUT25  Product function, 12th filter and spectrum (1st. long)
C     NOUT26  UBVRI output
C     NOUT27  JKLMN output
C     NOUT28  Concatenated light curves, all colors specified for output
C
C     The input control file is in SYNPHOT.INF
C     The address list of input spectra is in the input control file.
C     The default value is SYNPHOT.ADD
C     The	address list of corresponding orbital phases is in the
C     control file PGBX3SV.DAT.
C
C     Note that the spectra for input to SYNPHOT typically are for an
C     entire binary star system. Synthetic system spectra typically are
C     produced by program SYNACSP. In this case, the spectra have names
C     of the form smzzxxxx.yyy, where the "sm" refers to the system 
C     spectrum, xxxx provides the spectrum range, and yyy provides the
C     orbital phase. zz provides an option to specify the Teff of the
C     donor, in Kk.
C     If it is important to calculate synthetic photometry for 
C     particular components of the system, then rerun SYNACSP to select
C     that component, and name the output spectra appropriately,
C     as c1, or c2, or fc, or rm.   
C      INCLUDE 'COMA.FOR'
      DIMENSION FL1(49),PSIV(300)
      DIMENSION TU(1000),FUWL(1000)
      DIMENSION TUBEWL(2000),TUBERS(2000)
      DIMENSION SWL(700000),FLX1(700000),SWLTP(700000),FLX1TP(700000)
      DIMENSION ULTA(10000),FUTP(10000),TUTP(10000)
      DIMENSION BLTA(10000),VLTA(10000),RLTA(10000),VILTA(10000)
      DIMENSION TRANSP(700000)
      DIMENSION FBWL(1000),TB(1000),FBTP(10000),TBTP(10000)
      DIMENSION FVWL(1000),TV(1000),FVTP(10000),TVTP(10000)
      DIMENSION FRWL(1000),TR(1000),FRTP(10000),TRTP(10000)
      DIMENSION FIWL(1000),TI(1000),FITP(10000),TITP(10000)
      DIMENSION FJWL(1000),TJ(1000),FJTP(10000),TJTP(10000)
      DIMENSION FHWL(1000),THA(1000),FHTP(10000),THATP(10000)
      DIMENSION FKWL(1000),TK(1000),FKTP(10000),TKTP(10000)
      DIMENSION FLWL(1000),TL(1000),FLTP(10000),TLTP(10000)
      DIMENSION FMWL(1000),TM(1000),FMTP(10000),TMTP(10000)
      DIMENSION FFUVWL(1000),TFUV(1000),FUVTP(10000),TUVTP(10000)
      DIMENSION FNUVWL(1000),TNUV(1000),FNUVTP(10000),TNUVTP(10000)
      DIMENSION VJLTA(10000),VHLTA(10000),VKLTA(10000),VLLTA(10000),
     $VMLTA(10000),FUVA(10000),VNUVA(10000)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 24 FL1
      CHARACTER * 7 LABEL
      CHARACTER * 24 ADDR
      CHARACTER * 24 FMTR
      CHARACTER * 80 ADDSP
      CHARACTER * 10 DATE,TIME,ZONE
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      INTEGER (2) IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DATA ZRO/0.D0/
      DATA ER/1.0E-10/
      DATA TWPI/6.2831853071796D0/
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,5,66,7/
      DATA IN8,IN9,IN10/8,9,10/
      DATA IN11,IN12,IN13,IN14,IN15/11,12,13,14,15/
      DATA NOUT1,NOUT2,NOUT3,NOUT4,NOUT5,NOUT6/16,17,18,19,20,21/
      DATA NOUT7,NOUT8,NOUT9,NOUT10,NOUT11/22,23,24,25,26/
      DATA NOUT12,NOUT13,NOUT14,NOUT15,NOUT16/27,28,29,30,31/
      DATA NOUT17,NOUT18,NOUT19,NOUT20,NOUT21/32,33,34,35,36/
      DATA NOUT22,NOUT23,NOUT24/37,38,39/
      DATA NOUT25,NOUT26,NOUT27,NOUT28/40,41,42,43/
  100 FORMAT (A80)
  101 FORMAT (A24)
  102 FORMAT (10I5)
  104 FORMAT (F6.1,F10.4)
  105 FORMAT (2F12.5)
  108 FORMAT (F12.5,E12.5)
  109 FORMAT (F10.5)
  110 FORMAT (2E20.7)
  111 FORMAT (F10.7,4F8.4,5F10.3)
  112 FORMAT (5F16.13)
  113 FORMAT (1X,'Normalization divisor, first filter= ',2X,D15.6)
  114 FORMAT (1X,'Normalization divisor, second filter=',2X,D15.6)
  115 FORMAT (1X,'Normalization divisor, third filter= ',2X,D15.6)
  116 FORMAT (1X,'Normalization divisor, fourth filter=',2X,D15.6)
  117 FORMAT (4D22.13)
  118 FORMAT (1X,'Normalization divisor, fifth filter=',3X,D15.6)
  119 FORMAT (1X,'Normalization divisor, sixth filter=',3X,D15.6)
  120 FORMAT (1X,'Normalization divisor, seventh filter=',1X,D15.6)
  121 FORMAT (1X,'Normalization divisor, eighth filter=',2X,D15.6)
  122 FORMAT (1X,'Normalization divisor, ninth filter=',3X,D15.6)
  123 FORMAT (1X,'Normalization divisor, tenth filter=',3X,D15.6)
  124 FORMAT (1X,'Normalization divisor, eleventh filter=',D15.6)
  125 FORMAT (1X,'Normalization divisor, twelvth filter=',1X,D15.6)
  126 FORMAT (2X,'UMG =',F10.6,3X,'ULT =',1PD15.6)
  127 FORMAT (2X,'BMG =',F10.6,3X,'BLT =',1PD15.6)
  128 FORMAT (2X,'VMG =',F10.6,3X,'VLT =',1PD15.6)
  129 FORMAT (2X,'RMG =',F10.6,3X,'RLT =',1PD15.6)
  130 FORMAT (1X,'VIMG =',F10.6,2X,'VILT =',1PD15.6)
  131 FORMAT (1X,'VJMG =',F10.6,2X,'VJLT =',1PD15.6)
  132 FORMAT (1X,'VKMG =',F10.6,2X,'VKLT =',1PD15.6)
  133 FORMAT (1X,'VLMG =',F10.6,2X,'VLLT =',1PD15.6)
  134 FORMAT (1X,'VMMG =',F10.6,2X,'VMLT =',1PD15.6)
  135 FORMAT (1X,'VNMG =',F10.6,2X,'VNLT =',1PD15.6)
  136 FORMAT (1X,'FUVMG =',F10.6,2X,'FUVLT =',1PD15.6)
  137 FORMAT (1X,'VNUVMG =',F10.6,2X,'VNUVLT =',1PD15.6)
  138 FORMAT (' ')
  201 FORMAT (1X,'PROGRAM SYNPHOT',30X,'DATE=',I2,'/',I2,'/',I4,2X,
     $'TIME=',I2,':',I2,':',I2,'.',I2)
  202 FORMAT (1X,'Number of values of orbital longitude=',12X,I5,/,1X,
     $'Ordinal value of starting orbital longitude=',I11,/,1X,
     $'Ending value of orbital longitude=',16X,I5)
  203 FORMAT (1X,'Program running time=',F14.4,2X,'minutes')
  204 FORMAT (A19,I5,A18,I5)
  250 FORMAT ('system light,incl.3rd lt.')
C*********************************************************************
      OPEN (UNIT =50,FILE = 'SYNPHOT.ILS',
     $STATUS ='OLD')
      READ (50,101)      (FL1(J),J=1,43)
      OPEN (UNIT = IN1,   FILE = FL1(1),  STATUS = 'OLD')
      OPEN (UNIT = IN2,   FILE = FL1(2),  STATUS = 'OLD')
      OPEN (UNIT = IN3,   FILE = FL1(3),  STATUS = 'OLD')
      OPEN (UNIT = IN4,   FILE = FL1(4),  STATUS = 'OLD')
      OPEN (UNIT = IN5,   FILE = FL1(5),  STATUS = 'OLD')
      OPEN (UNIT = IN6,   FILE = FL1(6),  STATUS = 'OLD')
      OPEN (UNIT = IN7,   FILE = FL1(7),  STATUS = 'OLD')
      OPEN (UNIT = IN8,   FILE = FL1(8),  STATUS = 'OLD')
      OPEN (UNIT = IN9,   FILE = FL1(9),  STATUS = 'OLD')
      OPEN (UNIT = IN10,  FILE = FL1(10), STATUS = 'OLD')
      OPEN (UNIT = IN11,  FILE = FL1(11), STATUS = 'OLD')
      OPEN (UNIT = IN12,  FILE = FL1(12), STATUS = 'OLD')
      OPEN (UNIT = IN13,  FILE = FL1(13), STATUS = 'OLD')
      OPEN (UNIT = IN14,  FILE = FL1(14), STATUS = 'OLD')
      OPEN (UNIT = IN15,  FILE = FL1(15), STATUS = 'OLD')
      OPEN (UNIT = NOUT1, FILE = FL1(16), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT2, FILE = FL1(17), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT3, FILE = FL1(18), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT4, FILE = FL1(19), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT5, FILE = FL1(20), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT6, FILE = FL1(21), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT7, FILE = FL1(22), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT8, FILE = FL1(23), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT9, FILE = FL1(24), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT10,FILE = FL1(25), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT11,FILE = FL1(26), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT12,FILE = FL1(27), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT13,FILE = FL1(28), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT14,FILE = FL1(29), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT15,FILE = FL1(30), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT16,FILE = FL1(31), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT17,FILE = FL1(32), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT18,FILE = FL1(33), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT19,FILE = FL1(34), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT20,FILE = FL1(35), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT21,FILE = FL1(36), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT22,FILE = FL1(37), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT23,FILE = FL1(38), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT24,FILE = FL1(39), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT25,FILE = FL1(40), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT26,FILE = FL1(41), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT27,FILE = FL1(42), STATUS = 'REPLACE')
      OPEN (UNIT = NOUT28,FILE = FL1(43), STATUS = 'REPLACE')
C********************************************************************
C
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (NOUT1,201) IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
C*********************************************************************
      LBLERR=0
      LABEL = 'SYNPHOT'
      NU=IN1
      CALL GTLBL( LABEL,LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NPST)
      CALL DUMMY (1)
      CALL IREAD (NPSN)
      CALL DUMMY (1)
C     Read distance to system in parsecs
      CALL DREAD (VREF,'E15.8')
C     Convert to cm.
      VREF=VREF*3.085678E18
C     Convert to distance squared
      VREF=VREF*VREF
      CALL DUMMY(1)
C     Read key to specify handling of spectroscopic data:
C     If KEY1=0, do synthetic photometry on flux basis.
C     If KEY1=1, do synthetic photometry on photon counting basis.
      CALL IREAD (KEY1)
      CALL DUMMY(1)
C     Read key to specify type of light curve to produce:
C     If KEY2=0, produce light curve normalized to 1.0 at light maximum.
C     If KEY2=1, produce light curve on absolute flux basis.
      CALL IREAD (KEY2)          
      CALL DUMMY(1)
C     KY1 must=1 for output to include first filter data
      CALL IREAD (KY1)
C     Read number of points defining first filter response
      CALL IREAD (NFLT1)
C     KY2 must=1 for output to include second filter data
      CALL IREAD (KY2)
C     Read number of points defining second filter response
      CALL IREAD (NFLT2)
C     KY3 must=1 for output to include third filter data
      CALL IREAD (KY3)
C     Read number of points defining third filter response
      CALL IREAD (NFLT3)
C     KY4 must=1 for output to include fourth filter data
      CALL IREAD (KY4)
C     Read number of points defining fourth filter response
      CALL IREAD (NFLT4)
C     KY5 must=1 for output to include fifth filter data
      CALL IREAD (KY5)
C     Read number of points defining fifth filter response
      CALL IREAD (NFLT5)
C     KY6 must=1 for output to include sixth filter response
      CALL IREAD (KY6)
C     Read number of points defining sixth filter response
      CALL IREAD (NFLT6)
C     KY7 must=1 for output to include seventh filter response
      CALL IREAD (KY7)
C     Read number of points defining seventh filter response
      CALL IREAD (NFLT7)
C     KY8 must=1 for output to include eighth filter response
      CALL IREAD (KY8)
C     Read number of points defining eighth filter response
      CALL IREAD (NFLT8)
C     KY9 must=1 for output to include ninth filter response
      CALL IREAD (KY9)
C     Read number of points defining ninth filter response
      CALL IREAD (NFLT9)
C     KY10 must=1 for output to include tenth filter response
      CALL IREAD (KY10)
C     Read number of points defining tenth filter response
      CALL IREAD (NFLT10)
C     KY11 must=1 for output to include eleventh filter response
      CALL IREAD (KY11)
C     Read number of points defining eleventh filter response
      CALL IREAD (NFLT11)
C     KY12 must=1 for output to include twelvth filter response
      CALL IREAD (KY12)
C     Read number of points defining twelvth filter response
      CALL IREAD (NFLT12)
      CALL IREAD (ICTRL)
C     Read filter wavelength limits
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F11=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F12=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F21=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F22=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F31=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F32=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F41=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F42=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F51=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F52=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F61=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F62=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F71=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F72=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F81=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F82=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F91=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F92=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F101=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F102=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F111=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F112=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F121=FLTR
      CALL DREAD (FLTR,'F9.1')
	IF (FLTR.NE.0.D0) F122=FLTR
	CALL DUMMY (2)
C     Address of list of spectrum files.
      READ (NU,101) ADDR
	OPEN (UNIT=60, FILE = ADDR, STATUS = 'OLD')
C     Format of data in spectrum files
      CALL DUMMY (1)
	READ (NU,101) FMTR
C     Read number of orbital longitudes in data set, NPSI
      READ (IN15, 102) KCTRL, NPSI
	IF (NPSI.GT.300) CALL PMDSTOP
      NADD=NPSI
	READ (IN15, 112) (PSIV(K),K=1,NPSI)
C     Read response function of phototube.
C     Note that the response function file is generic. Must copy
C     the explicit phototube response function into the named
C     phototube file for input to this program.
      DO J=1,1000
		READ (IN2, 104, END=1, ERR=2) TUBEWL(J), TUBERS(J)
      END DO
    2 STOP 99
    1 NTUBE=J-1
C*********************************************************************
C     Do first response function, first (U) filter.
C     NFLT1 is number of data points defining response function.
      IF (KY1.GT.0) THEN
         DO J=1,NFLT1
         READ (IN3, 104, ERR=901) FUWL(J),TU(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TU(J)=TU(J)*FUWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT1,0,FUWL,TU,FUTP,TUTP,NUFLT)
	END IF
C*********************************************************************
C     Do second response function, second (B) filter.
      IF (KY2.GT.0) THEN
         DO J=1,NFLT2
         READ (IN4, 104, ERR=901) FBWL(J),TB(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TB(J)=TB(J)*FBWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT2,0,FBWL,TB,FBTP,TBTP,NBFLT)
	END IF
C*********************************************************************
C     Do third response function, third (V) filter.
      IF (KY3.GT.0) THEN
         DO J=1,NFLT3
         READ (IN5, 104, ERR=901) FVWL(J),TV(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TV(J)=TV(J)*FVWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT3,0,FVWL,TV,FVTP,TVTP,NVFLT)
	END IF
C*********************************************************************
C     Do fourth response function, fourth (R) filter.
      IF (KY4.GT.0) THEN
         DO J=1,NFLT4
         READ (IN6, 104, ERR=901) FRWL(J),TR(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TR(J)=TR(J)*FRWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT4,0,FRWL,TR,FRTP,TRTP,NRFLT)
	END IF
C*********************************************************************
C     Do fifth response function, fifth (I) filter.
      IF (KY5.GT.0) THEN
         DO J=1,NFLT5
         READ (IN7, 104, ERR=901) FIWL(J),TI(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TI(J)=TI(J)*FIWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT5,0,FIWL,TI,FITP,TITP,NIFLT)
	END IF
C*********************************************************************
C     Do sixth response function, sixth (J) filter.
      IF (KY6.GT.0) THEN
         DO J=1,NFLT6
         READ (IN8, 104, ERR=901) FJWL(J),TJ(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TJ(J)=TJ(J)*FJWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT6,0,FJWL,TJ,FJTP,TJTP,NJFLT)
	END IF
C*********************************************************************
C     Do seventh response function, seventh (H) filter.
      IF (KY7.GT.0) THEN
         DO J=1,NFLT7
         READ (IN9, 104, ERR=901) FHWL(J),THA(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            THA(J)=THA(J)*FHWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT7,0,FHWL,THA,FHTP,THATP,NHFLT)
	END IF
C*********************************************************************
C     Do eighth response function, eighth (K_s) filter.
      IF (KY8.GT.0) THEN
         DO J=1,NFLT8
         READ (IN10, 104, ERR=901) FKWL(J),TK(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TK(J)=TK(J)*FKWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT8,0,FKWL,TK,FKTP,TKTP,NKFLT)
	END IF
C*********************************************************************
C     Do ninth response function, ninth (L) filter.
      IF (KY9.GT.0) THEN
         DO J=1,NFLT9
         READ (IN11, 104, ERR=901) FLWL(J),TL(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TL(J)=TL(J)*FLWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT9,0,FLWL,TL,FLTP,TLTP,NLFLT)
	END IF
C*********************************************************************
C     Do tenth response function, tenth (M) filter.
      IF (KY10.GT.0) THEN
         DO J=1,NFLT10
         READ (IN12, 104, ERR=901) FMWL(J),TM(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TM(J)=TM(J)*FMWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT10,0,FMWL,TM,FMTP,TMTP,NMFLT)
	END IF
C*********************************************************************
C     Do eleventh response function, eleventh (FUV) filter.
      IF (KY11.GT.0) THEN
         DO J=1,NFLT11
         READ (IN13, 104, ERR=901) FFUVWL(J),TFUV(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TFUV(J)=TFUV(J)*FFUVWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT11,0,FFUVWL,TFUV,FUVTP,TUVTP,NFUVFLT)
	END IF
C*********************************************************************
C     Do twelvth response function, twelvth (NUV) filter.
      IF (KY12.GT.0) THEN
         DO J=1,NFLT12
         READ (IN14, 104, ERR=901) FNUVWL(J),TNUV(J)
C     If KEY1 has specified photon counting, multiply by wavelength in cm.
         IF (KEY1.EQ.1) THEN
            TNUV(J)=TNUV(J)*FNUVWL(J)*1.0D-8
         END IF
         END DO
C     Produce interpolated response function
        CALL TPOL(NFLT12,0,FNUVWL,TNUV,FNUVTP,TNUVTP,NNUVFLT)
	END IF
C**********************************************************************
C
C     Now have finished reading response functions for all
C     filters.
      IF (NPSI.GT.NADD) CALL PMDSTOP
      IF (NPST.EQ.0) NPST=1
      IF (NPSN.EQ.0) NPSN=NPSI
	WRITE (NOUT1,202) NPSI,NPST,NPSN
	VLTRF=1.D0
	URF=1.D0
	UMX=0.D0
	BMX=0.D0
	VMX=0.D0
	RMX=0.D0
	VIMX=0.D0
        VJMX=0.D0
        VHMX=0.D0
        VKMX=0.D0
        VLMX=0.D0
        VMMX=0.D0
	FUVMX=0.D0
	FNUVMX=0.D0
C     Run through all orbital longitudes
      DO 20 J=1,NPSN
      PRINT 204,'ordinal longitude',    J,'total longitudes', NPSN
      IF (J.GE.74) THEN
         ZZZ=0.D0
      END IF
C     The list of spectrum files is in a data file referenced 
C     in the file SYNPHOT.INF and named ADDR. Associated with 
C     unit 60
C     Now read unit 60 for the explicit address of an individual 
C     spectrum inserted in ADDSP
	READ (60,100) ADDSP
	PSJ=PSIV(J)/TWPI
	IF (PSJ.GT.0.7D0) PSJ=PSJ-1.D0
C     Open the file for a specific spectrum.
	OPEN (61,FILE = ADDSP, STATUS='OLD')
      DO K=1,700000
C     Read in spectrum wavelengths and corresponding fluxes.
C     SWL(K) is the wavelength
C     FLX1(K) is the combined spectrum flux
	   READ (61,FMTR, ERR=903, END=11) SWL(K),FLX1(K)
           ZZ=0.D0
      END DO
      CALL PMDSTOP 	                   !Too many wavelengths
C     Start with the designated ordinal longitude
   11 IF (J.LT.NPST) GO TO 20
C     Set up array of integer Angstrom wavelengths extending over
C     input spectrum.
      WL1=SWL(1)
      IWL1=WL1
      WL2=SWL(K-1)
      IWL2=WL2
      NWL=IWL2-IWL1
      DO K=1,NWL+1
         SWLTP(K)=IWL1+K-1
      END DO
C     Interpolate spectrum to integer Angstroms.
      KLM=K-1                     !No. of wavelengths in spectrum
      KKST=1
C     Interpolate a point into FLX1TP() for each point
C     in SWLTP().
      DO L=1,NWL
C     Select a point in array of wavelengths for interpolation.
        A=SWLTP(L)
C     Find points in SWL() that bracket the WLTP() point.
      DO KK=KKST,NWL
      RWL1=SWL(KK)
      RWL2=SWL(KK+1)
C     If the two WL() points follow A, step to next interval.
        IF ((A.LT.RWL1).AND.(A.LT.RWL2)) THEN
           FLX1TP(L)=0.D0
           GO TO 10
        END IF
C     If the first WL() point coincides with A, set FLX1TP(LL)
C     equal to the corresponding FLX1() point.
        IF (DABS(A-RWL1).LE.ER) THEN
           FLX1TP(L)=FLX1(KK)
           GO TO 10
        END IF
        IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
C    Start of section for parabolic interpolation
          IF (KK.EQ.1) THEN
            X1=RWL1
            X2=RWL2
            X3=SWL(KK+2)
            Y1=FLX1(KK)
            Y2=FLX1(KK+1)
            Y3=FLX1(KK+2)
            TM1X=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2X=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1X/TM2X
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            FLX1TP(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-FLX1(KK)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-FLX1(KK+1)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-FLX1(KK+2)
            AA=0.D0
            GO TO 10
          END IF   !IF (KK.EQ.1) THEN
          IF (KK.EQ.NWL-1) THEN
            X1=SWL(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=FLX1(KK-1)
            Y2=FLX1(KK)
            Y3=FLX1(KK+1)
            TM1X=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2X=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1X/TM2X
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            FLX1TP(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-FLX1(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-FLX1(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-FLX1(KK+1)
            AA=0.D0
            GO TO 10
          END IF  !IF (KK.EQ.NWL-1) THEN
          IF ((KK.GT.1).AND.(KK.LT.NWL-1)) THEN
            X1=SWL(KK-1)
            X2=RWL1
            X3=RWL2
            Y1=FLX1(KK-1)
            Y2=FLX1(KK)
            Y3=FLX1(KK+1)
            TM1X=(X3-X2)*(Y2-Y1)-(X2-X1)*(Y3-Y2)
            TM2X=(X2*X2-X1*X1)*(X3-X2)-(X3*X3-X2*X2)*(X2-X1)
            CTM=TM1X/TM2X
            TM3=(Y2-Y1)-CTM*(X2*X2-X1*X1)
            TM4=(X2-X1)
            BTM=TM3/TM4
            ATM=Y1-BTM*X1-CTM*X1*X1
            YVL=ATM+BTM*A+CTM*A*A
            FLX1TP(L)=YVL
            YTS1=ATM+BTM*X1+CTM*X1*X1
            ER1=YTS1-FLX1(KK-1)
            YTS2=ATM+BTM*X2+CTM*X2*X2
            ER2=YTS2-FLX1(KK)
            YTS3=ATM+BTM*X3+CTM*X3*X3
            ER3=YTS3-FLX1(KK+1)
            AA=0.D0
            GO TO 10
          END IF   ! IF ((KK.GT.1).AND.(KK.LT.NWL-1)) THEN
C      If the second WL() point coincides with A, set RSTP(L)
C      equal to the corresponding RS() point.
        IF (DABS(A-RWL2).LE.ER) THEN
           FLX1TP(L)=FLX1(KK+1)
           GO TO 10
        END IF
      END IF   !IF ((A.GT.RWL1).AND.(A.LT.RWL2)) THEN
      END DO   !DO KK=KKST,KRG-1
   10 CONTINUE
      END DO   !DO L=1,NWL
C*********************************************************************
C     Now multiply the spectrum by the response functions to produce
C     the arguments to be integrated for simulated photometric
C     observations
C*********************************************************************
C     KY1 must be set to 1 to include first (U) filter data
         IF (KY1.NE.1) GO TO 31
            SM=0.D0
            UNORM=0.D0
            KK=1
            DO K=1,NUFLT
  150       CONTINUE
            WL1=SWLTP(KK)
            WL2=FUTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 251
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 150
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  251       TRANSP(K)=FLX1TP(KK)*TUTP(K)
            END DO
            DO K=1,NUFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FUTP(K)
C     NOUT2 lists the first filter transmission
               WRITE (NOUT2,108) WL,TUTP(K)
C     NOUT3 lists the first band product function for the
C		combined spectrum.
               WRITE (NOUT3,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoidal or Simpson's rule integration.
             ITG=1
            CALL ITGFN(FUTP,TRANSP,ITG,NUFLT,SM)
C            CALL INTEG(FUWL,TRANSP,FUWL(1),FUWL(NFLT1),NFLT1,SM)
C     Must normalize for response function.
		  CALL ITGFN(FUTP,TUTP,ITG,NUFLT,UNORM)
		  SM=SM/UNORM
C     ULT is the response measured through the first filter.
            ULT=SM
		 IF (ULT.GT.ULTMX) ULTMX=ULT
		 ULTA(J)=ULT
C     End of section for first filter data
C*********************************************************************
C     KY2 must be 1 to include second (B) filter data
   31       IF (KY2.NE.1) GO TO 22
            SM=0.D0
            BNORM=0.D0
            KK=1
            DO K=1,NBFLT
  151       CONTINUE
            WL1=SWLTP(KK)
            WL2=FBTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 252
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 151
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  252       TRANSP(K)=FLX1TP(KK)*TBTP(K)
            END DO
            DO K=1,NBFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FBTP(K)
C     NOUT4 lists the second filter transmission
               WRITE (NOUT4,108) WL,TBTP(K)
C     NOUT5 lists the second band product function for the
C		combined spectrum.
               WRITE (NOUT5,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FBTP,TRANSP,ITG,NBFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FBTP,TBTP,ITG,NBFLT,BNORM)
		  SM=SM/BNORM
C     BLT is the response measured through the second filter.
            BLT=SM
		 IF (BLT.GT.BLTMX) BLTMX=BLT
		 BLTA(J)=BLT
C     End of section for second filter
C**********************************************************************
C     KY3 must be 1 to include third (V) filter data
   22	    IF (KY3.NE.1) GO TO 23
            SM=0.D0
            VNORM=0.D0
            KK=1
            DO K=1,NVFLT
  152       CONTINUE
            WL1=SWLTP(KK)
            WL2=FVTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 254
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 152
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  254       TRANSP(K)=FLX1TP(KK)*TVTP(K)
            END DO
            DO K=1,NVFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FVTP(K)
C     NOUT6 lists the third filter transmission
               WRITE (NOUT6,108) WL,TVTP(K)
C     NOUT7 lists the third band product function for the
C		combined spectrum.
               WRITE (NOUT7,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoidal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FVTP,TRANSP,ITG,NVFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FVTP,TVTP,ITG,NVFLT,VNORM)
		  SM=SM/VNORM
C     VLT is the response measured through the third filter.
            VLT=SM
		 IF (VLT.GT.VLTMX) VLTMX=VLT
		 VLTA(J)=VLT
C     End of section for third filter data
C********************************************************************
C     KY4 must equal 1 for the fourth (R) filter data to be included
   23      IF (KY4.NE.1) GO TO 24
            SM=0.D0
            RNORM=0.D0
            KK=1
            DO K=1,NRFLT
  153       CONTINUE
            WL1=SWLTP(KK)
            WL2=FRTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 253
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 153
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  253         TRANSP(K)=FLX1TP(KK)*TRTP(K)
            END DO
            DO K=1,NRFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FRTP(K)
C     NOUT8 lists the fourth filter transmission
               WRITE (NOUT8,108) WL,TRTP(K)
C     NOUT9 lists the fourth band product function for the
C		combined spectrum.
               WRITE (NOUT9,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoidal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FRTP,TRANSP,ITG,NRFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FRTP,TRTP,ITG,NRFLT,RNORM)
		  SM=SM/RNORM
C     RLT is the response measured through the fourth filter.
            RLT=SM
		 IF (RLT.GT.RLTMX) RLTMX=RLT
		 RLTA(J)=RLT
C	End of section for fourth filter
C********************************************************************
C     KY5 must equal 1 for the fifth (I) filter data to be included
   24      IF (KY5.NE.1) GO TO 37
           SM=0.D0
           VINORM=0.D0
            KK=1
            DO K=1,NIFLT
  154       CONTINUE
            WL1=SWLTP(KK)
            WL2=FITP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 255
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 154
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  255       TRANSP(K)=FLX1TP(KK)*TITP(K)
            END DO
            DO K=1,NIFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FITP(K)
C     NOUT10 lists the fifth filter transmission
               WRITE (NOUT10,108) WL,TITP(K)
C     NOUT11 lists the fifth band product function for the
C		combined spectrum.
               WRITE (NOUT11,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FITP,TRANSP,ITG,NIFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FITP,TITP,ITG,NIFLT,VINORM)
		  SM=SM/VINORM
C     VILT is the response measured through the fifth filter.
            VILT=SM
		 IF (VILT.GT.VILTMX) VILTMX=VILT
		 VILTA(J)=VILT
C     End of section for fifth filter
C********************************************************************
C     KY6 must equal 1 for the sixth (J) filter data to be included
   37      IF (KY6.NE.1) GO TO 38
           SM=0.D0
           VJNORM=0.D0
            KK=1
            DO K=1,NJFLT
  155       CONTINUE
            WL1=SWLTP(KK)
            WL2=FJTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 256
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 155
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  256       TRANSP(K)=FLX1TP(KK)*TJTP(K)
            END DO
            DO K=1,NJFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FJTP(K)
C     NOUT12 lists the sixth filter transmission
               WRITE (NOUT12,108) WL,TJTP(K)
C     NOUT13 lists the first band product function for the
C		combined spectrum.
               WRITE (NOUT13,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FJTP,TRANSP,ITG,NJFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FJTP,TJTP,ITG,NJFLT,VJNORM)
		  SM=SM/VJNORM
C     VJLT is the response measured through the sixth filter.
            VJLT=SM
		 IF (VJLT.GT.VJLTMX) VJLTMX=VJLT
		 VJLTA(J)=VJLT
C     End of section for sixth filter
C********************************************************************
C     KY7 must equal 1 for the seventh (H) filter data to be included
   38      IF (KY7.NE.1) GO TO 52
           SM=0.D0
           VHNORM=0.D0
            KK=1
            DO K=1,NHFLT
  156       CONTINUE
            WL1=SWLTP(KK)
            WL2=FHTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 257
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 156
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  257       TRANSP(K)=FLX1TP(KK)*THATP(K)
            END DO
            DO K=1,NHFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FHTP(K)
C     NOUT14 lists the first filter transmission
               WRITE (NOUT14,108) WL,TRANSP(K)
C     NOUT15 lists the first band product function for the
C		combined spectrum.
               WRITE (NOUT15,108) WL,THATP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FHTP,TRANSP,ITG,NHFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FHTP,THATP,ITG,NHFLT,VHNORM)
		  SM=SM/VHNORM
C     VHLT is the response measured through the seventh filter.
            VHLT=SM
		 IF (VHLT.GT.VHLTMX) VHLTMX=VHLT
		 VHLTA(J)=VHLT
C     End of section for seventh filter
C********************************************************************
C     KY8 must equal 1 for the eighth (K_s) filter data to be included
   52      IF (KY8.NE.1) GO TO 53
            SM=0.D0
            VKNORM=0.D0
            KK=1
            DO K=1,NKFLT
  157       CONTINUE
            WL1=SWLTP(KK)
            WL2=FKTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 258
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 157
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  258       TRANSP(K)=FLX1TP(KK)*TKTP(K)
            END DO
            DO K=1,NKFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FKTP(K)
C     NOUT16 lists the eighth filter transmission
               WRITE (NOUT16,108) WL,TKTP(K)
C     NOUT17 lists the eighth band product function for the
C		combined spectrum.
               WRITE (NOUT17,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FKTP,TRANSP,ITG,NKFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FKTP,TKTP,ITG,NKFLT,VKNORM)
		  SM=SM/VKNORM
C     VKLT is the response measured through the eighth filter.
           VKLT=SM
		 IF (VKLT.GT.VKLTMX) VKLTMX=VKLT
		 VKLTA(J)=VKLT
C     End of section for eighth filter
C********************************************************************
C     KY9 must equal 1 for the ninth (L) filter data to be included
   53      IF (KY9.NE.1) GO TO 54
           SM=0.D0
           VLNORM=0.D0
            KK=1
            DO K=1,NLFLT
  158       CONTINUE
            WL1=SWLTP(KK)
            WL2=FLTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 259
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 158
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  259       TRANSP(K)=FLX1TP(KK)*TLTP(K)
            END DO
            DO K=1,NLFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FLTP(K)
C     NOUT18 lists the first filter transmission
               WRITE (NOUT18,108) WL,TLTP(K)
C     NOUT19 lists the first band product function for the
C		combined spectrum.
               WRITE (NOUT19,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANSP. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FLTP,TRANSP,ITG,NLFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FLTP,TLTP,ITG,NLFLT,VLNORM)
		  SM=SM/VLNORM
C     VLLT is the response measured through the ninth filter.
            VLLT=SM
		 IF (VLLT.GT.VLLTMX) VLLTMX=VLLT
		 VLLTA(J)=VLLT
C     End of section for ninth filter
C********************************************************************
C     KY10 must equal 1 for the tenth (M) filter data to be included
   54      IF (KY10.NE.1) GO TO 55
           SM=0.D0
           VMNORM=0.D0
            KK=1
            DO K=1,NMFLT
  159       CONTINUE
            WL1=SWLTP(KK)
            WL2=FMTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 260
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 159
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  260       TRANSP(K)=FLX1TP(KK)*TMTP(K)
            END DO
            DO K=1,NMFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FMTP(K)
C     NOUT20 lists the tenth filter transmission
               WRITE (NOUT20,108) WL,TMTP(K)
C     NOUT21 lists the tenth band product function for the
C		combined spectrum.
               WRITE (NOUT21,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANSP. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FMTP,TRANSP,ITG,NMFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FMTP,TMTP,ITG,NMFLT,VMNORM)
		  SM=SM/VMNORM
C     VMLT is the response measured through the tenth filter.
            VMLT=SM
		 IF (VMLT.GT.VMLTMX) VMLTMX=VMLT
		 VMLTA(J)=VMLT
C     End of section for tenth filter
C********************************************************************
C     KY11 must equal 1 for the eleventh (FUV) filter data to be included
   55      IF (KY11.NE.1) GO TO 26
           SM=0.D0
           FUVNORM=0.D0
            KK=1
            DO K=1,NFUVFLT
  160       CONTINUE
            WL1=SWLTP(KK)
            WL2=FUVTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 261
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 160
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  261       TRANSP(K)=FLX1TP(KK)*TUVTP(K)
            END DO
            DO K=1,NFUVFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FUVTP(K)
C     NOUT22 lists the first filter transmission
               WRITE (NOUT22,108) WL,TUVTP(K)
C     NOUT23 lists the eleventh band product function for the
C		combined spectrum.
               WRITE (NOUT23,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANSP. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FUVTP,TRANSP,ITG,NFUVFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FUVTP,TUVTP,ITG,NFUVFLT,FUVNORM)
		  SM=SM/FUVNORM
C     FUVLT is the response measured through the eleventh filter.
            FUVLT=SM
		 IF (FUVLT.GT.FUVLTMX) FUVLTMX=FUVLT
		 FUVA(J)=FUVLT
C	End of section for eleventh filter
C********************************************************************
C     KY12 must equal 1 for the twelvth (NUV) filter data to be included
   26      IF (KY12.NE.1) GO TO 15
           SM=0.D0
           VNUVNORM=0.D0
            KK=1
            DO K=1,NNUVFLT
  161       CONTINUE
            WL1=SWLTP(KK)
            WL2=FNUVTP(K)
            IF (WL1.NE.WL2) THEN
               IF (WL1.GE.WL2) GO TO 262
               IF (WL1.LT.WL2) THEN
                  KK=KK+1
                  GO TO 161
               END IF
               CALL PMDSTOP  !WL1 cannot be greater than WL2
            END IF
C     Calculate product of filter transmission and spectrum flux.
  262       TRANSP(K)=FLX1TP(KK)*TNUVTP(K)
            END DO
            DO K=1,NNUVFLT
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=FNUVTP(K)
C     NOUT24 lists the twelvth filter transmission
               WRITE (NOUT2,108) WL,TNUVTP(K)
C     NOUT25 lists the twelvth band product function for the
C		combined spectrum.
               WRITE (NOUT3,108) WL,TRANSP(K)
            END IF
            END DO
C     Now have weighted spectrum in TRANS. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
C     Set control for trapezoinal or Simpson's rule integration.
            ITG=1
            CALL ITGFN(FNUVTP,TRANSP,ITG,NNUVFLT,SM)
C     Must normalize for response function.
		  CALL ITGFN(FNUVTP,TNUVTP,ITG,NNUVFLT,VNUVNORM)
		  SM=SM/VNUVNORM
C     VNUVLT is the response measured through the twelvth filter.
            VNUVLT=SMC
		 IF (VNUVLT.GT.VNUVLTMX) VNUVLTMX=VNUVLT
		 VNUVA(J)=VNUVLT
C     End of section for twelvth filter
C*********************************************************************
C     Write output.
   15       CONTINUE
C**************************************************************
C**************************************************************
              ULT=ULTMX/VREF
	      IF (KY1.EQ.1) UMG=-2.5D0*LOG10(ULT)
              BLT=BLTMX/VREF
	      IF (KY2.EQ.1) BMG=-2.5D0*LOG10(BLT)
              VLT=VLTMX/VREF
	      IF (KY3.EQ.1) VMG=-2.5D0*LOG10(VLT)
              RLT=RLTMX/VREF
	      IF (KY4.EQ.1) RMG=-2.5D0*LOG10(RLT)
              VILT=VILTMX/VREF
	      IF (KY5.EQ.1) VIMG=-2.5D0*LOG10(VILT)
              VJLT=VJLTMX/VREF
	      IF (KY6.EQ.1) VJMG=-2.5D0*LOG10(VJLT)
              VKLT=VKLTMX/VREF
	      IF (KY7.EQ.1) VKMG=-2.5D0*LOG10(VKLT)
              VLLT=VLLTMX/VREF
	      IF (KY8.EQ.1) VLMG=-2.5D0*LOG10(VLLT)
              VMLT=VMLTMX/VREF
	      IF (KY9.EQ.1) VMMG=-2.5D0*LOG10(VMLT)
              VNLT=VNLTMX/VREF
	      IF (KY10.EQ.1) VNMG=-2.5D0*LOG10(VNLT)
              FUVLT=FUVLTMX/VREF
	      IF (KY11.EQ.1) FUVMG=-2.5D0*LOG10(FUVLT)
              VNUVLT=VNUVLTMX/VREF
	      IF (KY12.EQ.1) VNUVMG=-2.5D0*LOG10(VNUVLT)
	      WRITE (NOUT26,111) PSJ,UMG-BMG,BMG-VMG,VMG-RMG,
     $      RMG-VIMG,UMG,BMG,VMG,RMG,VIMG
	      WRITE (NOUT27,111) PSJ,VJMG-VKMG,VKMG-VLMG,VLMG-VMMG,
     $      VMMG-VNMG,VJMG,VKMG,VLMG,VMMG,VNMG
      IF (J.EQ.1) THEN
        WRITE (NOUT1, 138)
        IF (KY1.EQ.1) THEN
            WRITE (NOUT1, 126) UMG,ULT
        END IF
        IF (KY2.EQ.1) THEN
            WRITE (NOUT1, 127) BMG,BLT
        END IF
        IF (KY3.EQ.1) THEN
            WRITE (NOUT1, 128) VMG,VLT
        END IF
        IF (KY4.EQ.1) THEN
            WRITE (NOUT1, 129) RMG,RLT
        END IF
        IF (KY5.EQ.1) THEN
            WRITE (NOUT1, 130) VIMG,VILT
        END IF
        IF (KY6.EQ.1) THEN
            WRITE (NOUT1, 131) VJMG,VJLT
        END IF
        IF (KY7.EQ.1) THEN
            WRITE (NOUT1, 132) VKMG,VKLT
        END IF
        IF (KY8.EQ.1) THEN
            WRITE (NOUT1, 133) VLMG,VLLT
        END IF
        IF (KY9.EQ.1) THEN
            WRITE (NOUT1, 134) VMMG,VMLT
        END IF
        IF (KY10.EQ.1) THEN
            WRITE (NOUT1, 135) VNMG,VNLT
        END IF
        IF (KY11.EQ.1) THEN
            WRITE (NOUT1, 136) FUVLT,FUVMG
        END IF
        IF (KY12.EQ.1) THEN
            WRITE (NOUT1, 137) VNUVLT,VNUVMG
        END IF
        WRITE (NOUT1, 138)
      END IF
   20 CONTINUE
C      Produce light curves, cancatenated
		WRITE (NOUT28,250)
		IF (KY1.NE.1) GO TO 41
		DO J=1,NPSN
                  IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) ULTA(J)/ULTMX,PSIV(J)/TWPI
                  END IF
                  IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) ULTA(J)/VREF,PSIV(J)/TWPI
                  END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C		
   41		IF (KY2.NE.1) GO TO 42
		DO J=1,NPSN
                  IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) BLTA(J)/BLTMX,PSIV(J)/TWPI
                  END IF
                  IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) BLTA(J)/VREF,PSIV(J)/TWPI
                  END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   42		IF (KY3.NE.1) GO TO 43
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VLTA(J)/VLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   43		IF (KY4.NE.1) GO TO 44
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) RLTA(J)/RLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) RLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   44		IF (KY5.NE.1) GO TO 60
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VILTA(J)/VILTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VILTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   60		IF (KY6.NE.1) GO TO 61
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VJLTA(J)/VJLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VJLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   61		IF (KY7.NE.1) GO TO 62
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
                	WRITE (NOUT28,110) VHLTA(J)/VHLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VHLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   62		IF (KY8.NE.1) GO TO 63
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VKLTA(J)/VKLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VKLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   63		IF (KY9.NE.1) GO TO 64
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VLLTA(J)/VLLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VLLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   64		IF (KY10.NE.1) GO TO 45
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VMLTA(J)/VMLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VMLTA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   45		IF (KY11.NE.1) GO TO 46
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) FUVA(J)/FUVLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) FUVA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   46		IF (KY12.NE.1) GO TO 51
		DO J=1,NPSN
                   IF (KEY2.NE.1) THEN
			WRITE (NOUT28,110) VNUVA(J)/VNUVLTMX,PSIV(J)/TWPI
                   END IF
                   IF (KEY2.EQ.1) THEN
			WRITE (NOUT28,110) VNUVA(J)/VREF,PSIV(J)/TWPI
                   END IF
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
   51       CONTINUE
   	    WRITE (NOUT1,113) ULTMX
	    WRITE (NOUT1,114) BLTMX
	    WRITE (NOUT1,115) VLTMX
	    WRITE (NOUT1,116) RLTMX
	    WRITE (NOUT1,118) VILTMX
	    WRITE (NOUT1,119) VJLTMX
	    WRITE (NOUT1,120) VHLTMX
	    WRITE (NOUT1,121) VKLTMX
	    WRITE (NOUT1,122) VLLTMX
	    WRITE (NOUT1,123) VMLTMX
	    WRITE (NOUT1,124) FUVLTMX
	    WRITE (NOUT1,125) VNUVLTMX
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
      WRITE (NOUT1,203) TM
C
C
C
C*********************************************************************
   21 CLOSE (UNIT = IN1,   STATUS = 'KEEP')
      CLOSE (UNIT = IN2,   STATUS = 'KEEP')
      CLOSE (UNIT = IN3,   STATUS = 'KEEP')
      CLOSE (UNIT = IN4,   STATUS = 'KEEP')
      CLOSE (UNIT = IN5,   STATUS = 'KEEP')
      CLOSE (UNIT = IN6,   STATUS = 'KEEP')
      CLOSE (UNIT = IN7,   STATUS = 'KEEP')
      CLOSE (UNIT = IN8,   STATUS = 'KEEP')
      CLOSE (UNIT = IN9,   STATUS = 'KEEP')
      CLOSE (UNIT = IN10,  STATUS = 'KEEP')
      CLOSE (UNIT = NOUT1, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT2, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT3, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT4, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT5, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT6, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT7, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT8, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT9, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT10, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT11, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT12, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT13, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT14, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT15, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT16, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT17, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT18, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT19, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT20, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT21, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT22, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT23, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT24, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT25, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT26, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT27, STATUS = 'KEEP')
      CLOSE (UNIT = NOUT28, STATUS = 'KEEP')
      CLOSE (UNIT = 60, STATUS = 'KEEP')
      CLOSE (UNIT = 61, STATUS = 'KEEP')
      STOP 'SYNPHOT: Completed Successfully.'
  900 FORMAT (1X,'Input error, filter data')
  901 WRITE (NOUT1,900)
      STOP
  902 FORMAT (1X,'Input error, spectrum data')
  903 WRITE (NOUT1,902)
      STOP
  904 WRITE (NOUT1,905)
  905 FORMAT (1X,'End of file, spectrum data')
      END PROGRAM SYNPHOT
C
      SUBROUTINE ITGFN(AR1,AR2,KY,N,SM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AR1(10000),AR2(10000)
      SM=0.D0
      H=AR1(2)-AR1(1)
      IF (KY.EQ.0) THEN
C     Trapezoidal rule
      DO L=2,N
         ORD=(AR2(L)+AR2(L-1))/2.D0
         SM=SM+ORD*H
         IF (L.GE.N-5) THEN
         ZZZ=0.D0
         END IF
      END DO
      END IF
      IF (KY.EQ.1) THEN
C     Simpson's Rule
      CF=H/3.D0
      LM=N/2
      JPT=1
      DO L=1,LM
            A=AR2(JPT)
            B=AR2(JPT+1)
            C=AR2(JPT+2)
            SM=SM+A+4.D0*B+C
            JPT=JPT+2
      END DO
      SM=SM*CF
      END IF
      RETURN
      END SUBROUTINE ITGFN
