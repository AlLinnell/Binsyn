      PROGRAM SYNPHOT
C     Program to weight a spectrum by photometric response
C     functions and integrate the product to produce simulated
C     photometric quantities.
C     Input files as follows:
C     IN1     SYNPHOT.INF information file
C     IN2     PHTUBE.DAT phototube response function
C     IN3     UFLTBES.DAT U filter response function
C     IN4     BFLTBES.DAT B filter response function
C     IN5     VFLTBES.DAT V filter response function
C     IN6     RFLTBES.DAT R filter response function
C     IN7     IFLTBES.DAT I filter response function
C     IN8     JFLTBES.DAT J filter response function
C     IN9     HFLTRES.DAT K filter response function
C     IN10    KFLTRES.DAT L filter response function
C     IN11    LFLTRES.DAT M filter response function
C     IN12    MFLTRES.DAT N filter response function
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
C             Typically K filter
C     NOUT15  Product function, 7th filter and spectrum (1st long.)
C     NOUT16  Eighth filter transmission function
C             Typically L filter
C     NOUT17  Product function, 8th filter and spectrum (1st long.)
C     NOUT18  Ninth filter transmission function
C             Typically M filter
C     NOUT19  Product function, 9th filter and spectrum (1st long.)
C     NOUT20  Tenth filter transmission function
C             Typically N filter
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

      INCLUDE 'COMA.FOR'
      DIMENSION FVWL(2000),TV(2000),FBWL(2000),TB(2000),FUWL(2000),
     $TU(2000),TV2(2000),TU2(2000),FL1(49),
     $TB2(2000),FRWL(2000),TR(2000),TR2(2000),FIWL(2000),TI(2000),
     $TI2(2000),FFUVWL(2000),TFUV(2000),TFUV2(2000),FNUVWL(2000),
     $TNUV(2000),TNUV2(2000)
      DIMENSION FJWL(2000),TJ(2000),TJ2(2000),FHWL(2000),THA(2000),
     $THA2(2000),FKWL(2000),TK(2000),TK2(2000),FLWL(2000),TL(2000),
     $TL2(2000),FMWL(2000),TM(2000),TM2(2000)
      DIMENSION RT2(2000),TUBEWL(2000),TUBERS(2000),
     $TRANS(70000)
      DIMENSION SWL(700000),FLX1(700000),WTSP(700000),
     $TRANSP(700000),
     $PI(700000)
	DIMENSION PSIV(300)
	DIMENSION ULTA(2000),BLTA(2000),VLTA(2000),RLTA(2000),VILTA(2000),
     $FUVA(2000),VNUVA(2000),VJLTA(2000),VKLTA(2000),VLLTA(2000),
     $VMLTA(2000),VNLTA(2000)
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
C     Default values of filter wavelength limits
      DATA F11,F12/1000.0,6000.0/
	DATA F21,F22/3000.0,9000.0/
	DATA F31,F32/3000.0,9500.0/
	DATA F41,F42/4000.0,9999.0/
	DATA F51,F52/5000.0,9999.0/
	DATA F61,F62/ 900.0,1500.0/
	DATA F71,F72/1200.0,2800.0/
	DATA F81,F82/1200.0,2800.0/
	DATA F91,F92/1200.0,2800.0/
	DATA F101,F102/1200.0,2800.0/
	DATA F111,F112/1200.0,2800.0/
	DATA F121,F122/1200.0,2800.0/
  100 FORMAT (A80)
  101 FORMAT (A24)
  102 FORMAT (10I5)
  104 FORMAT (F9.1,F9.3)
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
      CALL DREAD (VREF,'E15.8')
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
C     KY12 must=1 for output to include thirteenth filter response
      CALL IREAD (KY13)
C     Read number of points defining thirteenth filter response
      CALL IREAD (NFLT13)
C     KY12 must=1 for output to include fourteenth filter response
      CALL IREAD (KY14)
C     Read number of points defining fourteenth filter response
      CALL IREAD (NFLT14)
C     If the photometry was done with a photon counting system,
C     then ICTRL=1
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
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F131=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F132=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F141=FLTR
      CALL DREAD (FLTR,'F8.1')
	IF (FLTR.NE.0.D0) F142=FLTR
	CALL DUMMY (2)
C     Address of list of spectrum files.
      READ (NU,101) ADDR
	OPEN (UNIT=60, FILE = ADDR, STATUS = 'OLD')
C     Format of data in spectrum files
      CALL DUMMY (1)
	READ (NU,101) FMTR
C     Read maximum number of spectra
	CALL IREAD (NADD)
C     Read number of orbital longitudes in data set, NPSI
      READ (IN15, 102) KCTRL, NPSI
	IF (NPSI.GT.300) CALL PMDSTOP
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
      NRF=NTUBE
    5		TUWL=TUBEWL(NRF)
		IF (TUWL.EQ.0.D0) THEN
			NRF=NRF-1
			GO TO 5
		END IF
		TLWL=TUBEWL(1)
      DWL=TUBEWL(2)-TUBEWL(1)
	DV1=(TUBERS(2)-TUBERS(1))/DWL
	DV2=(TUBERS(NTUBE)-TUBERS(NTUBE-1))/DWL
C     Produce spline fit to response function, phototube.
      CALL SPLINE(TUBEWL,TUBERS,NTUBE,DV1,DV2,RT2)
C*********************************************************************
C     Do third response function, third (U) filter.
C     NFLT3 is number of data points defining response function.
      IF (KY3.GT.0) THEN
         DO J=1,NFLT3
         READ (IN3, 104, ERR=901, END=16) FUWL(J),TU(J)
         END DO
   16    DWL=FUWL(2)-FUWL(1)
         DV1=(TU(2)-TU(1))/DWL
         DV2=(TU(NFLT3)-TU(NFLT3-1))/DWL
C     Produce spline fit to transmission function, third filter
      CALL SPLINE(FUWL,TU,NFLT3,DV1,DV2,TU2)
	END IF
C*********************************************************************
C     Do fourth response function, fourth (B) filter.
      IF (KY4.GT.0) THEN
         DO J=1,NFLT4
         READ (IN4, 104, ERR=901, END=17) FBWL(J),TB(J)
         END DO
   17    DWL=FBWL(2)-FBWL(1)
         DV1=(TB(2)-TB(1))/DWL
         DV2=(TB(NFLT4)-TB(NFLT4-1))/DWL
C     Produce spline fit to transmission function, fourth filter
         CALL SPLINE(FBWL,TB,NFLT4,DV1,DV2,TB2)
	END IF
C*********************************************************************
C     Do fifth response function, fifth (V) filter.
      IF (KY5.GT.0) THEN
         DO J=1,NFLT5
         READ (IN5, 104, ERR=901, END=18) FVWL(J),TV(J)
         END DO
   18    DWL=FVWL(2)-FVWL(1)
         DV1=(TV(2)-TV(1))/DWL
         DV2=(TV(NFLT5)-TV(NFLT5-1))/DWL
C     Produce spline fit to transmission function, fifth filter
         CALL SPLINE(FVWL,TV,NFLT5,DV1,DV2,TV2)
	END IF
C*********************************************************************
C     Do sixth response function, sixth (R) filter.
      IF (KY6.GT.0) THEN
         DO J=1,NFLT6
         READ (IN6, 104, ERR=901, END=19) FRWL(J),TR(J)
         END DO
   19    DWL=FRWL(2)-FRWL(1)
         DV1=(TR(2)-TR(1))/DWL
         DV2=(TR(NFLT6)-TR(NFLT6-1))/DWL
C     Produce spline fit to transmission function, sixth filter
		CALL SPLINE(FRWL,TR,NFLT6,DV1,DV2,TR2)
	END IF
C*********************************************************************
C     Do seventh response function, seventh (I) filter.
      IF (KY7.GT.0) THEN
         DO J=1,NFLT7
         READ (IN7, 104, ERR=901, END=29) FIWL(J),TI(J)
         END DO
   29    DWL=FIWL(2)-FIWL(1)
         DV1=(TI(2)-TI(1))/DWL
         DV2=(TI(NFLT7)-TI(NFLT7-1))/DWL
C     Produce spline fit to transmission function, seventh filter
		CALL SPLINE(FIWL,TI,NFLT7,DV1,DV2,TI2)
	END IF
C*********************************************************************
C     Do eighth response function, eighth (J) filter.
      IF (KY8.GT.0) THEN
         DO J=1,NFLT8
         READ (IN8, 104, ERR=901, END=27) FJWL(J),TJ(J)
         END DO
   27    DWL=FJWL(2)-FJWL(1)
         DV1=(TJ(2)-TJ(1))/DWL
         DV2=(TJ(NFLT8)-TJ(NFLT8-1))/DWL
C     Produce spline fit to transmission function, eigth filter
		CALL SPLINE(FJWL,TJ,NFLT8,DV1,DV2,TJ2)
	END IF
C*********************************************************************
C     Do ninth response function, ninth (H) filter.
      IF (KY9.GT.0) THEN
         DO J=1,NFLT9
         READ (IN9, 104, ERR=901, END=28) FHWL(J),THA(J)
         END DO
   28    DWL=FHWL(2)-FHWL(1)
         DV1=(THA(2)-THA(1))/DWL
         DV2=(THA(NFLT9)-THA(NFLT9-1))/DWL
C     Produce spline fit to transmission function, ninth filter
		CALL SPLINE(FHWL,THA,NFLT9,DV1,DV2,THA2)
	END IF
C*********************************************************************
C     Do tenth response function, tenth (K) filter.
      IF (KY10.GT.0) THEN
         DO J=1,NFLT10
         READ (IN10, 104, ERR=901, END=32) FKWL(J),TK(J)
         END DO
   32    DWL=FKWL(2)-FKWL(1)
         DV1=(TK(2)-TK(1))/DWL
         DV2=(TK(NFLT10)-TK(NFLT10-1))/DWL
C     Produce spline fit to transmission function, tenth filter
		CALL SPLINE(FKWL,TK,NFLT10,DV1,DV2,TK2)
	END IF
C*********************************************************************
C     Do eleventh response function, eleventh (L) filter.
      IF (KY11.GT.0) THEN
         DO J=1,NFLT11
         READ (IN11, 104, ERR=901, END=33) FLWL(J),TL(J)
         END DO
   33    DWL=FLWL(2)-FLWL(1)
         DV1=(TL(2)-TL(1))/DWL
         DV2=(TM(NFLT11)-TM(NFLT11-1))/DWL
C     Produce spline fit to transmission function, eleventh filter
		CALL SPLINE(FLWL,TL,NFLT11,DV1,DV2,TL2)
	END IF
C*********************************************************************
C     Do twelvth response function, twelvth (M) filter.
      IF (KY12.GT.0) THEN
         DO J=1,NFLT12
         READ (IN12, 104, ERR=901, END=34) FMWL(J),TM(J)
         END DO
   34    DWL=FMWL(2)-FMWL(1)
         DV1=(TM(2)-TM(1))/DWL
         DV2=(TM(NFLT12)-TM(NFLT12-1))/DWL
C     Produce spline fit to transmission function, twelvth filter
		CALL SPLINE(FMWL,TM,NFLT12,DV1,DV2,TM2)
	END IF
C*********************************************************************
C     Do thirteenth response function, thirteenth (FUV) filter.
      IF (KY13.GT.0) THEN
         DO J=1,NFLT13
         READ (IN13, 104, ERR=901, END=39) FFUVWL(J),TFUV(J)
         END DO
   39    DWL=FFUVWL(2)-FFUVWL(1)
         DV1=(TFUV(2)-TFUV(1))/DWL
         DV2=(TFUV(NFLT13)-TFUV(NFLT13-1))/DWL
C     Produce spline fit to transmission function, thirteenth filter
		CALL SPLINE(FFUVWL,TFUV,NFLT13,DV1,DV2,TFUV2)
	END IF
C*********************************************************************
C     Do fourteenth response function, fourteenth (NUV) filter.
      IF (KY14.GT.0) THEN
         DO J=1,NFLT14
         READ (IN14, 104, ERR=901, END=36) FNUVWL(J),TNUV(J)
         END DO
   36    DWL=FNUVWL(2)-FNUVWL(1)
         DV1=(TNUV(2)-TNUV(1))/DWL
         DV2=(TNUV(NFLT14)-TNUV(NFLT14-1))/DWL
C     Produce spline fit to transmission function, fourteenth filter
		CALL SPLINE(FNUVWL,TNUV,NFLT14,DV1,DV2,TNUV2)
	END IF
C**********************************************************************
C
C     Now have finished reading response functions for all
C     filters.
      IF (NPSI.GT.NADD) CALL PMDSTOP
      IF (NPST.EQ.0) NPST=1
      IF (NPSN.EQ.0) NPSN=NPSIC
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
      END DO
      CALL PMDSTOP 	                   !Too many wavelengths
C     Start with the designated ordinal longitude
   11 IF (J.LT.NPST) GO TO 20
C     Now multiply the spectrum by the response functions to produce
C     the arguments to be integrated for simulated photometric
C     observations
C*********************************************************************
C     KY1 must be set to 1 to include first (U) filter data
         KLM=K-1                !No. of wavelengths in spectrum
         IF (KY1.NE.1) GO TO 31
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)              !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C     Response of U filter
            CALL SPLINT(FUWL,TU,TU2,NFLT1,WL,TRANS(K))
C     Determine Response Function	value at the wavelength in question
            PI(K)=TRANSP(K)*TRANS(K)
		  WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     Write function details for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT2 lists the first filter transmission
               WRITE (NOUT2,105) WL,TRANS(K)
C     NOUT3 lists the first band product function for the
C		combined spectrum.
               WRITE (NOUT3,108) WL,WTSP(K)
            END IF
            END DO
C     Now have weighted spectrum in WTSP. Integrate to produce photom.
C     quantities
C     SM is integral over product function.
            CALL INTEG(SWL,WTSP,F11,F12,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F11,F12,KLM,UNORM)
		  SM=SM/UNORM
C     ULT is the response measured through the first filter.
            ULT=SM
		 IF (ULT.GT.ULTMX) ULTMX=ULT
		 ULTA(J)=ULT
C     End of section for first filter data
C*********************************************************************
C     KY2 must be 1 to include second (B) filter data
   31       IF (KY2.NE.1) GO TO 22
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)         !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FBWL,TB,TB2,NFLT2,WL,TRANS(K))
            PI(K)=TRANSP(K)*TRANS(K)
		  WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     Produce detailed function data for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT4 lists the transmission function for the second filter
               WRITE (NOUT4,105) WL,TRANS(K)
C     NOUT5 lists the normalized product function for the 2nd filter
               WRITE (NOUT5,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F21,F22,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F21,F22,KLM,VNORM)
		  SM=SM/VNORM
C     BLT contains the photometric response for the combined spectrum,
C		second filter.
            BLT=SM
		  IF (BLT.GT.BLTMX) BLTMX=BLT
	      BLTA(J)=BLT
C     End if section for second filter
C**********************************************************************
C     KY3 must be 1 to include third (V) filter data
C
   22		IF (KY3.NE.1) GO TO 23
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)         !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FVWL,TV,TV2,NFLT3,WL,TRANS(K))
            PI(K)=TRANSP(K)*TRANS(K)
		  WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     Produce detailed function data for first orbital longitude
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT9 lists the transmission function for the third filter
               WRITE (NOUT6,105) WL,TRANS(K)
C     NOUT10 lists the normalized product function for the third filter
               WRITE (NOUT7,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F31,F32,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F31,F32,KLM,VNORM)
		  SM=SM/VNORM
C     VLT contains the photometric response for the combined spectrum,
C		V filter.
            VLT=SM
		  IF (VLT.GT.VLTMX) VLTMX=VLT
	      VLTA(J)=VLT
C     End of section for third filter data
C********************************************************************
C
C     KY4 must equal 1 for the fourth (R) filter data to be included
   23      IF (KY4.NE.1) GO TO 24
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FRWL,TR,TR2,NFLT4,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT6 contains the fourth filter response function
              WRITE (NOUT8,105) WL,TRANS(K)
C     NOUT7 contains the normalized product function output
              WRITE (NOUT9,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F41,F42,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F41,F42,KLM,VNORM)
		  SM=SM/VNORM
C     RLT contains the photometric response for the system spectrum
            RLT=SM
		  IF (RLT.GT.RLTMX) RLTMX=RLT
	      RLTA(J)=RLT
C	End of section for fourth filter
C********************************************************************
C
C     KY5 must equal 1 for the fifth (I) filter data to be included
   24      IF (KY5.NE.1) GO TO 37
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FIWL,TI,TI2,NFLT5,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT6 contains the fifth filter response function
              WRITE (NOUT10,105) WL,TRANS(K)
C     NOUT7 contains the normalized product function output
              WRITE (NOUT11,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F51,F52,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F51,F52,KLM,VNORM)
		  SM=SM/VNORM
C     VILT contains the photometric response for the system spectrum
            VILT=SM
		  IF (VILT.GT.VILTMX) VILTMX=VILT
	      VILTA(J)=VILT
C     End of section for fifth filter
C********************************************************************
C
C     KY6 must equal 1 for the sixth (J) filter data to be included
   37      IF (KY6.NE.1) GO TO 38
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FJWL,TJ,TJ2,NFLT6,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT12 contains the sixth filter response function
              WRITE (NOUT12,105) WL,TRANS(K)
C     NOUT13 contains the normalized product function output
              WRITE (NOUT13,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F61,F62,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F61,F62,KLM,VNORM)
		  SM=SM/VNORM
C     VILT contains the photometric response for the system spectrum
            VJLT=SM
		  IF (VJLT.GT.VJLTMX) VJLTMX=VJLT
	      VJLTA(J)=VJLT
C     End of section for sixth filter
C********************************************************************
C
C     KY7 must equal 1 for the seventh (K) filter data to be included
   38      IF (KY7.NE.1) GO TO 52
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FKWL,TK,TK2,NFLT7,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT14 contains the seventh filter response function
              WRITE (NOUT14,105) WL,TRANS(K)
C     NOUT15 contains the normalized product function output
              WRITE (NOUT15,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F71,F72,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F71,F72,KLM,VNORM)
		  SM=SM/VNORM
C     VKLT contains the photometric response for the system spectrum
            VKLT=SM
		  IF (VKLT.GT.VKLTMX) VKLTMX=VKLT
	      VKLTA(J)=VKLT
C     End of section for seventh filter
C********************************************************************
C
C     KY8 must equal 1 for the eighth (L) filter data to be included
   52      IF (KY8.NE.1) GO TO 53
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FLWL,TL,TL2,NFLT8,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT16 contains the eighth filter response function
              WRITE (NOUT16,105) WL,TRANS(K)
C     NOUT17 contains the normalized product function output
              WRITE (NOUT17,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F81,F82,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F81,F82,KLM,VNORM)
		  SM=SM/VNORM
C     VLLT contains the photometric response for the system spectrum
            VLLT=SM
		  IF (VLLT.GT.VLLTMX) VLLTMX=VLLT
	      VLLTA(J)=VLLT
C     End of section for eighth filter
C********************************************************************
C
C     KY9 must equal 1 for the ninth (M) filter data to be included
   53      IF (KY9.NE.1) GO TO 54
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FMWL,TM,TM2,NFLT9,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT18 contains the ninth filter response function
              WRITE (NOUT18,105) WL,TRANS(K)
C     NOUT19 contains the normalized product function output
              WRITE (NOUT19,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F91,F92,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F91,F92,KLM,VNORM)
		  SM=SM/VNORM
C     VILT contains the photometric response for the system spectrum
            VMLT=SM
		  IF (VMLT.GT.VMLTMX) VMLTMX=VMLT
	      VMLTA(J)=VMLT
C     End of section for ninth filter
C********************************************************************
C
C     KY10 must equal 1 for the tenth (N) filter data to be included
   54      IF (KY10.NE.1) GO TO 55
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FNWL,TN,TN2,NFLT10,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT20 contains the tenth filter response function
              WRITE (NOUT20,105) WL,TRANS(K)
C     NOUT21 contains the normalized product function output
              WRITE (NOUT21,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F101,F102,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F101,F102,KLM,VNORM)
		  SM=SM/VNORM
C     VNLT contains the photometric response for the system spectrum
            VNLT=SM
		  IF (VNLT.GT.VNLTMX) VNLTMX=VNLT
	      VNLTA(J)=VNLT
C     End of section for tenth filter
C********************************************************************
C
C     KY11 must equal 1 for the eleventh filter data to be included
   55      IF (KY11.NE.1) GO TO 26
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FFUVWL,TFUV,TFUV2,NFLT6,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT22 contains the eleventh filter response function
              WRITE (NOUT22,105) WL,TRANS(K)
C     NOUT23 contains the normalized product function output
              WRITE (NOUT23,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F111,F112,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F111,F112,KLM,VNORM)
		  SM=SM/VNORM
C     ULT contains the photometric response for the system spectrum
            FUVLT=SM
		  IF (FUVLT.GT.FUVLTMX) FUVLTMX=FUVLT
	      FUVA(J)=FUVLT
C	End of section for eleventh filter
C********************************************************************
C
C     KY12 must equal 1 for the twelvth filter data to be included
   26      IF (KY12.NE.1) GO TO 15
            DO K=1,KLM
            WL=SWL(K)
            FLUX=FLX1(K)            !Combined spectrum
C     Response of phototube
		  CALL SPLINT(TUBEWL,TUBERS,RT2,NTUBE,WL,TRANSP(K))
C
            CALL SPLINT(FNUVWL,TNUV,TNUV2,NFLT7,WL,TRANS(K))
		  PI(K)=TRANSP(K)*TRANS(K)
            WTSP(K)=PI(K)*FLUX
		  IF (ICTRL.EQ.1) THEN
			PI(K)=PI(K)*WL
			WTSP(K)=WTSP(K)*WL
		  END IF
            END DO
            DO K=1,KLM
C     List detailed function output only for the first orbital long.
            IF (J.EQ.NPST) THEN
               WL=SWL(K)
C     NOUT24 contains the twelvth filter response function
              WRITE (NOUT24,105) WL,TRANS(K)
C     NOUT25 contains the normalized product function output
              WRITE (NOUT25,108) WL,WTSP(K)
            END IF
            END DO
            CALL INTEG(SWL,WTSP,F121,F122,KLM,SM)
C     Must normalize for response function.
		  CALL INTEG(SWL,PI,F121,F122,KLM,VNORM)
		  SM=SM/VNORM
C     VNUVLT contains the photometric response for the system spectrum
            VNUVLT=SM
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
C      Produce light curves, concatenated
		WRITE (NOUT28,250)
		IF (KY1.NE.1) GO TO 41
		DO J=1,NPSN
			WRITE (NOUT28,110) ULTA(J)/ULTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C		
   41		IF (KY2.NE.1) GO TO 42
		DO J=1,NPSN
			WRITE (NOUT28,110) BLTA(J)/BLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   42		IF (KY3.NE.1) GO TO 43
		DO J=1,NPSN
			WRITE (NOUT28,110) VLTA(J)/VLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   43		IF (KY4.NE.1) GO TO 44
		DO J=1,NPSN
			WRITE (NOUT28,110) RLTA(J)/RLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   44		IF (KY5.NE.1) GO TO 60
		DO J=1,NPSN
			WRITE (NOUT28,110) VILTA(J)/VILTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   60		IF (KY6.NE.1) GO TO 61
		DO J=1,NPSN
			WRITE (NOUT28,110) VJLTA(J)/VJLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   61		IF (KY7.NE.1) GO TO 62
		DO J=1,NPSN
			WRITE (NOUT28,110) VKLTA(J)/VKLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   62		IF (KY8.NE.1) GO TO 63
		DO J=1,NPSN
			WRITE (NOUT28,110) VLLTA(J)/VLLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   63		IF (KY9.NE.1) GO TO 64
		DO J=1,NPSN
			WRITE (NOUT28,110) VMLTA(J)/VMLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   64		IF (KY10.NE.1) GO TO 45
		DO J=1,NPSN
			WRITE (NOUT28,110) VNLTA(J)/VNLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   45		IF (KY11.NE.1) GO TO 46
		DO J=1,NPSN
			WRITE (NOUT28,110) FUVA(J)/FUVLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
C
   46		IF (KY12.NE.1) GO TO 51
		DO J=1,NPSN
			WRITE (NOUT28,110) VNUVA(J)/VNUVLTMX,PSIV(J)/TWPI
		END DO
		WRITE (NOUT28,110) ZRO,ZRO
   51       CONTINUE
   	    WRITE (NOUT1,113) ULTMX
	    WRITE (NOUT1,114) BLTMX
	    WRITE (NOUT1,115) VLTMX
	    WRITE (NOUT1,116) RLTMX
	    WRITE (NOUT1,118) VILTMX
	    WRITE (NOUT1,119) VJLTMX
	    WRITE (NOUT1,120) VKLTMX
	    WRITE (NOUT1,121) VLLTMX
	    WRITE (NOUT1,122) VMLTMX
	    WRITE (NOUT1,123) VNLTMX
	    WRITE (NOUT1,124) FUVLTMX
	    WRITE (NOUT1,125) VNUVLTMX
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      A=CLOCK1
      B=CLOCK2
      C=COUNT_RATE
      TM=(B-A)/C
C      WRITE (NOUT1,203) TM
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
      SUBROUTINE INTEG(WL,FN,WL1,WL2,N,SM)
C     Trapezoidal integration routine.
C     Choose trapezoidal because function may vary rapidly.
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION WL(700000),FN(700000)
	SM=0.D0
	WL1A=WL1
	WL2A=WL2
	IF (WL1A.LT.WL(1)) CALL PMDSTOP
	IF (WL2A.GT.WL(N)) THEN
		IF (WL2A-0.1D0.GT.WL(N))CALL PMDSTOP
		WL2A=WL(N)
	END IF
	DO J=1,N-1
		J1=J
		IF ((WL1A.GE.WL(J)).AND.(WL1A.LT.WL(J+1))) GO TO 1
	END DO
	CALL PMDSTOP
    1	FL1=FN(J1)
	DO J=J1+1,N
		WL2A=WL(J)
		FL2=FN(J)
		DWL=WL2A-WL1A
		DFL=0.5D0*(FL1+FL2)
		SM=SM+DWL*DFL
		WL1A=WL2A
		FL1=FL2
	END DO
	RETURN
      END SUBROUTINE INTEG
