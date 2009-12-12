      PROGRAM ACPGF6
C	USE MSFLIB
C
C     ================================================================
C
C     Program for integration of local synthetic spectra 
C     (for individual disk rings, disk rim, and latitudes on the
C     stellar components) to get the total synthetic spectrum of a
C     general close binary system with an accretion disk.
C     Any of these objects may be missing.
C
C     For convenience, all common blocks are given in the include file 
C     COMPG5.FOR, which is described in detail below:
C
C    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    File COMPG5.FOR
C    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
c
CC      dimensions of arrays:
CC
C      PARAMETER (MLAT   = 201,    ! maximum number of latitides
C     *           MLON   = 201,    ! maximum number of longitudes
C     *           MRING  = 201,    ! maximum number of disk rings
C     *           MSEGF  = 501,    ! maximum number of azimuthal
C                                  ! segments
C     *           MRIM   =  25,    ! maximum number of rim latitudes
C     *           MSEGR  = 501,    ! maximum number of rim segments
C     *           MOBJ   =   4,    ! maximum number of objects
C     *           MMU    =  10,    ! maximum number of angles 
C     *           MLAM   = 500000,  ! maximum number of wavelengths 
C     *           MLAM1  = 500000,  ! maximum number of fine-grid wavelengths
C     *           MMODC  =   50,   ! maximum number of computed models
C     *           MPOINT = 2000,   ! maximum number of points corresponding
C     *                            ! to one computed model
C     *           MTCOMP =    75,   ! maximum number of Teff's
C     *           MGCOMP =    5,   ! maximum number of log g's
C     *           VELC   = 2.997925E5)   ! light speed
CC
CC      Actual parameters:
CC
CC      NRING   - number of the disk rings
CC      NSEGF   - number of azimuthal segments of the disk face
CC      NRIM    - number of the disk rim latitude segments
CC      NSEGR   - number of the disk rim azimuthal segments
CC      NLAT1   - number of the latitude rings on the central star surface
CC      NLON1   - number of the longitudinal segments on this star
CC      NLAT2   - number of the latitude rings on the other star surface
CC      NLON2   - number of the longitudinal segments on this star
CC      MPH     - array of the number of longitudes for given latitude
CC                and star
CC    
CC      if any of these numbers equals to zero, the given object is not
CC      considered
CC
C      COMMON/PARA/NRING,NSEGF,NRIM,NSEGR,NLAT1,NLON1,NLAT2,NLON2,
C     *            NLAT(MOBJ),NLON(MOBJ),
C     *            MPH(MLAT,MOBJ)
CC
CC
CC      Parameters for all segments which should be supplied by ACPGE:
CC
CC      COSG    - cosine of the angle between surface normal and the 
CC                line of sight
CC      AREA    - projected area of a segment
CC      VISIB   - visibility parameter for a segment
CC      VPRO    - projected velocity of a segment to the line of sight
CC
CC      for generally four objects, distinguished by the last character 
CC      the first index labels latitudes (or radial distance for disk)
CC      the second index labels longitudes (or azimuths in disk)
CC      the third index labels the object:
CC
CC      an individual disk ring                              - IOBJ=1
CC      an individual latitude ring on the disk rim          - IOBJ=2
Cc      an individual latitude ring of the disk central star - IOBJ=3
Cc      an individual latitude ring of the other star        - IOBJ=4
CC
C      COMMON/SEGM/COSG(MLAT,MLON,MOBJ),
C     *            AREA(MLAT,MLON,MOBJ),
C     *            VISIB(MLAT,MLON,MOBJ),
C     *            VPRO(MLAT,MLON,MOBJ)
Cc
CC      Parameters for input synthetic spectra of all objects
CC      the first index labels the individual model, 
CC      the second index label the object:
Cc
C      CHARACTER*60 FILNAM(MMODC,MOBJ)
C      COMMON/FILE/FILNAM         ! names of files where synthetic spectra
Cc                                !  for all models are stored
Cc
Cc      Information about angles for which the synthetic spectra of all 
Cc      objects are computed:
Cc     
Cc      ANGLE(IMU,I,IOBJ) - angles (labelled by IMU) for ring I of
Cc                          object IOBJ
Cc      NANGLE(I,IOBJ)    - number of angles for ring I of object IOBJ
Cc     
C      COMMON/ANGL/ANGLE(MMU,MMODC,MOBJ), 
C     *            NANGLE(MMODC,MOBJ)
CC                                      
Cc     coefficient of linear limb darkening
Cc     (has the meaning only if the input spectrum contains fluxes, not 
Cc     intensities. if =0, input spectrum have to be
Cc     given as a set of intensities
CC
C      COMMON/LIMD/CLD0(MMODC,MOBJ), ! all values for all models and objects
C     *            CLD,             ! the current value
C     *            CLCON            ! conversion factor = 4/(1-CLD/3)
Cc
CC     synthetic spectra
CC
C      COMMON/SPEC/WLAM(MLAM),      ! wavelengths in the synthetic spectra
C     *            OBSLAM(MLAM),    ! observer's frame wavelengths
C     *            RINT0(MLAM,MMU), ! detailed synthetic spectrum 
C     *            RINSEG(MLAM),    ! working array: current spectrum
C     *            V1,V2,           ! curr. limiting values of proj.velocity
C     *            VELRES           ! velocity resolution parameter
Cc
CC     fine-mesh synthetic spectra
CC
C      COMMON/SPEC/WLAM1(MLAM1),     ! wavelengths in the synthetic spectra
C     *            RINT1(MLAM1,MMU), ! detailed synthetic spectrum 
C     *            WLM1,             ! first (lowest) wavelength
C     *            WLCON,            ! contant for evaluating wavel.indices
C     *            DWFINE,           ! fine-mesh wavelength step
C     *            NLAM1             ! number of fine-mesh wavelengths
Cc
Cc     Parameters for a current segment
Cc
C      COMMON/CURR/AMU0(MMU),      ! normal angles
C     *            NMU,            ! number of the normal angles 
C     *            NLSYN,          ! number of wavelengths in the synthetic
C     *            NLOBS           ! number of observer's frame wavelengths
Cc
Cc     Paramters for indivudual computed models for all objects
Cc
C      COMMON/MODS1/CONTR(MPOINT,MMODC,MOBJ),  ! interpol.coeff.
C     *             ILAT1(MPOINT,MMODC,MOBJ),  ! latitudes
C     *             ILON1(MPOINT,MMODC,MOBJ),  ! longitudes
C     *             NPOINT(MMODC,MOBJ),        ! number of points
C     *             NMODC(MOBJ),               ! number of computed models
C     *             IRRAD(MOBJ)                ! irradiation switch
C
C    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INCLUDE 'COMPG5.FOR'
	DOUBLE PRECISION AA,RSUMT,RSUM,AREAS,ARECL
      DIMENSION RSUMT(MLAM),RSUM(MLAM,MOBJ)
	DIMENSION AREAS(MLAT,MOBJ),ARECL(MLAT,MOBJ)
C
      DIMENSION FL1(11)
      CHARACTER * 80 FL1
	CHARACTER * 60 BF1
	INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
	INTEGER(4) RESULT
	INTEGER * 2 KEYGOOD,KEYBAD
      DATA IN1,IN2,IN3,IN4,IN5/5,10,20,21,1/
      DATA NO1,NO2/91,6/
	DATA IN6/7/
	DATA IN7,IN8/8,9/
      DATA ZR/0.0/         !FILE SEPARATOR
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
  100 FORMAT (A80)
  101 FORMAT (I2)
  106 FORMAT (10I5)
  107 FORMAT (' ')
  108 FORMAT (1X,'PROGRAM ACPGF6',30X,'DATE=',I2,'/',I2,'/',I4,2X,
     $ 'TIME=',I2,':',I2,':',I2,'.',I2)
  109 FORMAT (A60)
  111 FORMAT (64X,I5)
C      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C     Open standard input files
      OPEN (UNIT = 11, FILE = 'ACPGF6.ILS', 
     $	STATUS = 'OLD')
      READ (11, 100) (FL1(J),J=1,10)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      OPEN (UNIT = IN2, FILE = FL1(2), STATUS = 'OLD')
      OPEN (UNIT = IN3, FILE = FL1(3), STATUS = 'OLD')
      OPEN (UNIT = IN4, FILE = FL1(4), STATUS = 'OLD')
      OPEN (UNIT = NO1, FILE = FL1(5), STATUS = 'REPLACE')
      OPEN (UNIT = NO2, FILE = FL1(6), STATUS = 'REPLACE')
      OPEN (UNIT = IN5, FILE = FL1(7), STATUS = 'OLD')
	OPEN (UNIT = IN6, FILE = FL1(8), STATUS = 'OLD')
	OPEN (UNIT = IN7, FILE = FL1(9), STATUS = 'OLD')
	OPEN (UNIT = IN8, FILE = FL1(10),STATUS = 'OLD')
	WRITE (IN6, 101) KEYBAD
	CLOSE (UNIT=IN6, STATUS = 'KEEP')
C      CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
	WRITE (NO2,107)
	WRITE (NO2,108) IMON,IDAY,IYR,IHR,IMIN,ISEC,I100TH
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F1)
C     Read control data for program ACPGA to check consistency
      READ (IN7,109) BF1
      READ (IN7,109) BF1
      READ (IN7,109) BF1
      READ (IN7,109) BF1
      READ (IN7,109) BF1
	READ (IN7,111) ISTAR
      READ (IN7,109) BF1
	READ (IN7,111) NRMA
      READ (IN7,109) BF1
	READ (IN7,111) NSGA
      READ (IN7,109) BF1
	READ (IN7,111) NRANG
C     Read control data for program PGA to check for consistency
	READ (IN8,109) BF1
	READ (IN8,109) BF1
	READ (IN8,109) BF1
	READ (IN8,109) BF1
	READ (IN8,109) BF1
	READ (IN8,111) NTHA
	READ (IN8,111) NPHA
C
            READ (IN5,106) KCTRL,NPSI
C     Loop through all orbital longitudes
      DO 95 LPCT=1,NPSI            
C
C     Initialization 
C
      CALL INITIA
      DO 10 ILAM=1,NLOBS
	 RSUMT(ILAM)=0.D0
	 DO 10 IOBJ=1,NOBJ
	    RSUM(ILAM,IOBJ)=0.D0
   10 CONTINUE
C
C
C     ****************************
C     loop over individual objects
C     ****************************
C
      DO 200 IOBJ=1,NOBJ
      IF(NLAT(IOBJ).LE.0) GO TO 200
C
C     ---------------------------
C     loop over individual models
C     ---------------------------
C
      DO 130 IMOD=1,NMODC(IOBJ)
C
C        read the synthetic spectrum of the current model
C
	 CALL READSP(IMOD,IOBJ)
C
C        loop over individual points on the star's surface 
C        for which the current model IMOD contributes (i.e.
C        also those points to which the model IMOD contributes
C        as one of the interpolated models)
C
	 DO 120 IPOINT=1,NPOINT(IMOD,IOBJ)
	    ILAT=ILAT1(IPOINT,IMOD,IOBJ)
	    ILON=ILON1(IPOINT,IMOD,IOBJ)
C
C           i) check whether the segment is visible; skip if not
C
	    IF(VISIB(ILAT,ILON,IOBJ).LT.0.0D0) THEN
			GO TO 120
		ELSE
			IF ((VISIB(1,ILON,IOBJ).LT.0.0D0).AND.
     $	         (IOBJ.EQ.2))THEN
				GO TO 120
			END IF
		END IF
C
C           ii) emergent specific intensity from the segment - 
C           ie. projected velocity shifted synthetic spectrum 
C           interpolated to a prescribed set of observers-frame wavelengths
C           and given angle betwen normal to the surface and line of sight
C
C           AMU - angle betwen normal to the surface and line of sight,
C           VPROJ - velocity of the segment projected to the line of sight/c
C
	    AMU=COSG(ILAT,ILON,IOBJ)
	    VPROJ=VPRO(ILAT,ILON,IOBJ)/VELC
C
C           limiting projection velocities for a given segment
C
	    IF(ILON.GT.1) THEN
	       V1=(VPRO(ILAT,ILON-1,IOBJ)/VELC+VPROJ)*0.5D0
	     ELSE
	       V1=VPROJ
	    END IF
	    IF(ILON.LT.MPH(ILAT,IOBJ)) THEN
	       V2=(VPRO(ILAT,ILON+1,IOBJ)/VELC+VPROJ)*0.5D0
	     ELSE
	       V2=VPROJ
	    END IF
c
	    CALL SEGSP(AMU,VPROJ)
C
C           contribution of the segment to the total emergent spectrum,
C           weighted by the projected area of the segment
C           and by the corresponding interpolation coefficient
C
 	    DO 110 ILAM=1,NLOBS
			AA=RINSEG(ILAM)*AREA(ILAT,ILON,IOBJ)*
     *		CONTR(IPOINT,IMOD,IOBJ)
	       RSUM(ILAM,IOBJ)=RSUM(ILAM,IOBJ)+AA
  110       CONTINUE
  120    CONTINUE
  130 CONTINUE
C
  200 CONTINUE
      DO IOBJ=1,4
		DO ILAT=1,NLAT(IOBJ)
			AREAS(ILAT,IOBJ)=0.0D0
			ARECL(ILAT,IOBJ)=0.0D0
			NLONA=MPH(ILAT,IOBJ)
			DO J=1,NLONA
				IF (VISIB(ILAT,J,IOBJ).GE.0.0D0) THEN
					AREAS(ILAT,IOBJ)=AREAS(ILAT,IOBJ)+AREA(ILAT,J,
     $				IOBJ)
	            ELSE
					ARECL(ILAT,IOBJ)=ARECL(ILAT,IOBJ)+AREA(ILAT,J,
     $                IOBJ)
				END IF
			END DO
		END DO
		WRITE (NO2,206) IOBJ
		WRITE (NO2,207) (AREAS(ILAT,IOBJ),ILAT=1,NLAT(IOBJ))
		WRITE (NO2,208)
		WRITE (NO2,207) (ARECL(ILAT,IOBJ),ILAT=1,NLAT(IOBJ)) 
          AREASM=0.0D0
		ARECLSM=0.0D0
		DO ILAT=1,NLAT(IOBJ)
			AREASM=AREASM+AREAS(ILAT,IOBJ)
			ARECLSM=ARECLSM+ARECL(ILAT,IOBJ)
		END DO
	WRITE (NO2,209) AREASM
	WRITE (NO2,310) ARECLSM
	WRITE (NO2,311) AREASM+ARECLSM
	END DO
  206 FORMAT (1X,'Object number',I5,/,1X,'Accumulated proj. area 
     $(visible)')
  207 FORMAT (8E10.3)
  208 FORMAT (1X,'Accumulated projected area (eclipsed)')
  209 FORMAT (1X,'Total visible projected area', E12.5)
  310 FORMAT (1X,'Total eclipsed area', E12.5)
  311 FORMAT (1X,'Total projected area', E12.5)
      
C
C     ****************************
C     output of the final spectrum
C     ****************************
C
      DO 300 ILAM=1,NLOBS
	 DO 210 IOBJ=1,NOBJ
	    RSUMT(ILAM)=RSUMT(ILAM)+RSUM(ILAM,IOBJ)
  210    CONTINUE
	 WRITE(NO1,601) OBSLAM(ILAM),RSUMT(ILAM),
     *                 rsum(ilam,1),rsum(ilam,2),
     *                 rsum(ilam,3),rsum(ilam,4)
  300 CONTINUE
      WRITE (NO1,602)ZR
	REWIND IN1
C     End of loop through orbital longitudes
   95 CONTINUE
C      CALL GETDAT(IYR,IMON,IDAY)
C      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F2)
      TM=(F2-F1)*1440.D0
      WRITE (NO2,205) TM
  205 FORMAT(1X,'Program running time=',F12.4,2X,'minutes')         
  601 FORMAT(1H ,F10.3,1P5E13.5)
  602 FORMAT (1H ,F10.3)
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      CLOSE (UNIT = IN2, STATUS = 'KEEP')
      CLOSE (UNIT = IN3, STATUS = 'KEEP')
      CLOSE (UNIT = NO1, STATUS = 'KEEP')
      CLOSE (UNIT = NO2, STATUS = 'KEEP')
      CLOSE (UNIT = IN4, STATUS = 'KEEP')
      CLOSE (UNIT = IN5, STATUS = 'KEEP')
	OPEN  (UNIT = IN6, FILE = FL1(8), STATUS = 'OLD')
	WRITE (IN6, 101) KEYGOOD
	CLOSE (UNIT = IN6, STATUS = 'KEEP')
      STOP
      END PROGRAM ACPGF6


C
C     ******************************************************************
C
C
      SUBROUTINE READSP(IMOD,IOBJ)
C     ============================
C
C     read the synthetic spectrum for the current feature, i.e. either
C
C     an individual disk ring                              - IOBJ=1
C     an individual latitude ring on the disk rim          - IOBJ=2
C     an individual latitude ring of the disk central star - IOBJ=3
C     an individual latitude ring of the other star        - IOBJ=4
C
C     IMOD numbers different models for a given object
C     i.e. either disk rings, or disk rim, or latitudes on either star 
C
      INCLUDE 'COMPG5.FOR'
      DIMENSION RINPT(MMU)
      DATA INFILE /90/
      CLD=CLD0(IMOD,IOBJ) 
      CLCON=4.D0/(1.-CLD/3.D0)
C
C     first, determine whether the spectrum is going to be read or not
C     (it is not read if the same file was already read before - 
C      this may happen in case where for instance different latitude
C      rings are described by the same model)
C
      IF(IMOD.GT.1.AND.
     *   FILNAM(IMOD,IOBJ).EQ.FILNAM(IMOD-1,IOBJ)) RETURN
C
C     set up array AMU0 - normal angles for which synthetic spectra were
C     calculated, and their number NMU, for the current object and model
C
      NMU=NANGLE(IMOD,IOBJ)
      DO 10 IMU=1,NMU
	 AMU0(IMU)=ANGLE(IMU,IMOD,IOBJ)
   10 CONTINUE
C
C     open the corresponding file containing the synthetic spectrum
C     of the current ring
C
      OPEN(INFILE,FILE=FILNAM(IMOD,IOBJ),STATUS='OLD')
C
C     read the spectrum wavelength by wavelength
C
C     1. read specific intensities (if NMU > 1)
C
      ILAM=0
      IF(NMU.GT.1) THEN
   20    CONTINUE
	 READ(INFILE,*,ERR=60,END=60) XLAM,FLAM,(RINPT(IMU),IMU=1,NMU)
	 IF(NLOBS.GT.0.AND.XLAM.LT.OBSLAM(1)-5.D0) GO TO 20
	 IF(NLOBS.GT.0.AND.XLAM.GT.OBSLAM(NLOBS)+5.D0) GO TO 60
	 ILAM=ILAM+1
	 WLAM(ILAM)=XLAM
	 DO 30 IMU=1,NMU
	    RINT0(ILAM,IMU)=RINPT(IMU)
   30    CONTINUE
	 GO TO 20
C
C     2. read fluxes (if NMU < 1)
C
       ELSE
	 IMU=1
	 NMU=1
   40    CONTINUE
	 READ(INFILE,*,ERR=60,END=60) XLAM,FLAM
	 IF(NLOBS.GT.0.AND.XLAM.LT.OBSLAM(1)-5.D0) GO TO 40
	 IF(NLOBS.GT.0.AND.XLAM.GT.OBSLAM(NLOBS)+5.D0) GO TO 60
	 ILAM=ILAM+1
	 WLAM(ILAM)=XLAM
	 RINT0(ILAM,IMU)=FLAM*CLCON
	 GO TO 40
      END IF
   60 CONTINUE
      NLSYN=ILAM
      CLOSE(INFILE)
      IF(NLSYN.GT.MLAM) CALL STOP('NLSYN TOO BIG')
C
C     in the case of first reading of the spectrum, and if the input
C     parameter NLOBS (number of wavelength points of the observer's
C     frame wavelengths) is zero, set up the observer's frame wavelengths
C     as the intrinsic wavelengths of the first synthetic spectrum
C
      IF(NLOBS.EQ.0) THEN
	 NLOBS=NLSYN
	 DO 70 ILAM=1,NLOBS
	    OBSLAM(ILAM)=WLAM(ILAM)
   70    CONTINUE
      END IF
C
C     interpolate to a finer wavelength resolution
C
      WLM1=WLAM(1)
      WLM2=WLAM(NLSYN)
      DWL1=WLM2-WLM1
      NLAM1=DWL1/DWFINE+1
      IF(NLAM1.GT.MLAM1) CALL STOP('NLAM1 TOO BIG')
      WLM1=WLM1-0.5D0*DWFINE
      WLCON=(NLAM1-1)/DWL1

      I0=1
      I1=I0+1
      DWL=1.D0/(WLAM(I1)-WLAM(I0))
      DO 100 ILAM=1,NLAM1
	 WL=WLM1+(ILAM-1)*DWFINE
	 WLAM1(ILAM)=WL
	 IF(WL.GE.WLAM(I1)) THEN
   80       I0=I0+1
	    I1=I0+1
	    IF(WL.GT.WLAM(I1)) GO TO 80
	    IF(WLAM(I1).NE.WLAM(I0)) DWL=1./(WLAM(I1)-WLAM(I0))
	 END IF
	 A0=(WLAM(I1)-WL)*DWL
	 A1=(WL-WLAM(I0))*DWL
	 IF ((DABS(A0).GT.1.0D0).OR.(DABS(A1).GT.1.0D0)) THEN
		IF (WL.LT.WLAM(I0))THEN
			A0=1.0D0
			A1=0.0D0
			WLAM1(ILAM)=WLAM(I0)
		END IF
		IF (WL.GT.WLAM(I1)) THEN
			A0=0.0D0
			A1=1.0D0
			WLAM1(ILAM)=WLAM(I1)
		END IF
	 END IF
	 DO IMU=1,NMU
	    RINT1(ILAM,IMU)=A0*RINT0(I0,IMU)+A1*RINT0(I1,IMU)
	 END DO
  100 CONTINUE 
C     
      RETURN
      END
C
C     ******************************************************************
C
C
      SUBROUTINE SEGSP(AMU,VPROJ0)
C     ============================
C
C     calculates radiation from a given segment, i.e. for a given 
C     projection velocity. 
C     The routine performs a loop over observer's-frame wavelengths;
C     first transforms a current wavelength to the rest frame of the
C     given segment by the Doppler shift corresponding to the projection 
C     velocity, and then interpolates the synthetic spectrum intensity
C     to this wavelength.
C
C     Input: AMU    - angle between normal to the surface and line of sight 
C            VPROJ0 - projection velocity
C     Output (through COMMON) : RINSEQ - radiation from the segment
C
      INCLUDE 'COMPG5.FOR'
	DIMENSION ARYL(NMU)
c
C     first determine the number of velocity partitions (velocity
C     separation between individual subpoints of the given segment
C     does not exceed VELRES - teh velocity ersolution parameter)
C
      DELV=DABS(V1-V2)
      IF(DELV.GT.VELRES) THEN
	  NVEL=DELV/VELRES
	  NVEL=NVEL+1
	  DVEL=(V2-V1)/(NVEL-1)
	  V0=V1
       ELSE
	  NVEL=1
	  DVEL=0
	  V0=VPROJ0
      END IF
      XNVEL=1.D0/NVEL
C
      DO 1 ILAM=1,NLOBS
    1    RINSEG(ILAM)=0.D0
      IF(NMU.GT.1) THEN
C     
C     **** first possibility - using calculated specific intensities
C
C     ---- i) interpolation coefficients for interpolation of the 
C     given AMU to the original set of normal angles
C     for which the specific intensities were calculated -AMU0(I)
C
C     IMU1, IMU2     - have the meaning that 
C                      AMU0(IMU1) < AMU < AMU0(IMU2)
C     COEFF1, COEFF2 - interapolation coefficients, having the 
C                      meaning:
C                      f(AMU)= COEFF1*f(AMU0(IMU1))+COEFF2*f(AMU0(IMU2))
C
C
      DO 10 IMU=2,NMU
	  IF(AMU.GT.AMU0(IMU)) GO TO 20
   10 CONTINUE
   20 CONTINUE
C
      IF(IMU.LE.NMU) THEN
      IMU1=IMU-1
      IMU2=IMU
      X=AMU0(IMU2)-AMU0(IMU1)
      COEFF1=(AMU0(IMU2)-AMU)/X
      COEFF2=(AMU-AMU0(IMU1))/X
	ELSE
	IMU2=IMU-1
	IMU1=IMU2-1
	END IF

C
C     ---- ii) interpolation in shifted wavelengths and angles
C
      DO 100 IVEL=0,NVEL-1
	  VPROJ=V0+IVEL*DVEL
	  VPR1=1.D0-VPROJ
	  ILM0=1
	  DO 90 ILAM=1,NLOBS
	    ALAM0=OBSLAM(ILAM)*VPR1
	    INDEX=WLCON*(ALAM0-WLM1)+1
	    INDEX=MAX(INDEX,1)
	    INDEX=MIN(INDEX,NLAM1)
		IF (IMU.LE.NMU) THEN
	    RINSEG(ILAM)=RINSEG(ILAM)+COEFF1*RINT1(INDEX,IMU1)+
     *                   COEFF2*RINT1(INDEX,IMU2)
		ELSE
		IF (AMU.LE.0.D0) GO TO 90
C     Procedure for intensity near limb
		DO JJ=1,NMU
			ARYL(JJ)=RINT1(INDEX,JJ)
		END DO
C     Do Lagrangian extrapolation
		CALL LAGIN(AMU,VIO1,AMU0,ARYL,3,NMU,MM,1.0D0)
		IF (MM.LT.3) THEN
			CALL STOP ('Error in LAGIN')
		END IF
		RINSEG(ILAM)=RINSEG(ILAM)+VIO1
		END IF
   90    CONTINUE
  100 CONTINUE
C
      ELSE
C
C     **** second possibility:
C     if NMU le 1, use a linear limb darkening law.
C     there is no interpolation in angle, since the intensity is now
C     given by an analytical expression
C
      CLDAMU=1.D0-CLD+CLD*AMU
      DO 200 IVEL=0,NVEL-1
	  VPROJ=V1+IVEL*DVEL
	  VPR1=1.D0-VPROJ
	  ILM0=1
	  IMU1=1
	  DO 110 ILAM=1,NLOBS
	    ALAM0=OBSLAM(ILAM)*VPR1
	    INDEX=WLCON*(ALAM0-WLM1)+1
	    INDEX=MAX(INDEX,1)
	    INDEX=MIN(INDEX,NLAM1)
	    RINSEG(ILAM)=RINSEG(ILAM)+RINT1(INDEX,IMU1)*CLDAMU
  110    CONTINUE
  200 CONTINUE
      END IF
C
C     proper normalization (1/NVEL)
C
      DO 210 ILAM=1,NLOBS
	  RINSEG(ILAM)=RINSEG(ILAM)*XNVEL
  210 CONTINUE
      RETURN
      END
C
C     ******************************************************************
C
C
C
      SUBROUTINE INITIA
C     =================
C     
C     Initialization routine
C
      INCLUDE 'COMPG5.FOR'
      DIMENSION R0(MLAT),DR0(MLAT),AZIM0(MLAT)
      DIMENSION PH(MLON)
      REAL MSTAR
      PARAMETER (GRCON = 6.668D-8,
     *           HA    = 0.5D0,
     *           PI    = 3.1415926535898D0,
     *           PI2   = 6.28318530718D0)
      PARAMETER( IN1   = 5,
     *           IN2   = 10,
     *           IN3   = 20,
     *           IN4   = 21)
C
      PIHA=PI/2.D0
      NOBJ=4
C
C     ############################################################
C     STANDARD INPUT FOR ACPGF
C     ############################################################
C
C     ***** 1. line of input file
C
C     observer's frame wavelengths + input switch
C
C     NLOBS  - i) if >0 -  number of the observer's frame wavelenghts;
C                          wavelenghts are then equidistant (see the
C                          two following parameters;
C              ii) if =0 - observer's frame wavelength points are taken
C                          to be the same wavelengths as those used in
C                          computing detailed synthetic spectrum
C     OBLAM0 - the value of the first wavelenght
C     DLAM0  - distance between wavelength points
C
C     INMODE - switch to determine the format of input:
C            = 0 - standard input taken as an output from ACPGE
C            = 1 - temporary (testing) input (for a disk face only)
C
C     VELRES - velocity resolution. It has the meaning that if the
C              projected velocity difference between two neighboring
C              points at the stellar surface is larger than VELRES,
C              the program performs an internal integration over 
C              a spread of velocities on the segment
C              VELRES is in km/s
C
C     DWFINE - "fine" wavelength step. It has the meaning that the
C              synthetic spectrum is NOT interpolated to the velocity
C              shifted wavelengths. Instead, it is taken as a simple
C              step function with step DWFINE (so, instead of interpolating,
C              one just find the index)
C
      READ(IN1,*) NLOBS,OBLAM0,DLAM0,INMODE,VELRES,DWFINE
      VELRES=VELRES/VELC
      IF(NLOBS.GT.0) THEN
	  DO 10 ILAM=1,NLOBS
	    OBSLAM(ILAM)=OBLAM0+(ILAM-1)*DLAM0
   10    CONTINUE
      END IF
C
C     ***** 2. line of input file
C
C     NRING   - number of the disk rings
C     NSEGF   - number of azimuthal segments of the disk face
C     NRIM    - number of the disk rim latitude segments
C     NSEGR   - number of the disk rim azimuthal segments
C     NLAT1   - number of the latitude rings on the central star surface
C     NLON1   - number of the longitudinal segments on this star
C     NLAT2   - number of the latitude rings on the other star surface
C     NLON2   - number of the longitudinal segments on this star
C    
C     IRRAD  - switch indicating whether the irradiation option
C              is turned on;
C            = 0 - no irradiation;
C            = 1 - irradiation (additional input from unit 20)
C
C     if any of these number equals to zero, the given object is not
C     considered
C     These values, if non-zero, may be overwritten by the input from
C     unit IN2 (=10)
C
      READ(IN1,*) NRING,NSEGF,NRIM,NSEGR,NLAT1,NLON1,NLAT2,NLON2,
     *            IRRAD(1),IRRAD(2),IRRAD(3),IRRAD(4)
C
C     Test for consistency on number of rings, segments, and rim rings
      IF (NRANG.NE.NRING) STOP
	IF (NRMA.NE.NRIM) STOP
	IF (NSGA.NE.NSEGF) STOP
	IF (NSGA.NE.NSEGR) STOP
C     Test for consistency on grid density on stars
      IF (NLAT1.NE.NTHA) STOP
	IF (NLAT2.NE.NTHA) STOP
	IF (NLON1.NE.NPHA) STOP
	IF (NLON2.NE.NPHA) STOP
      NLAT(1)=NRING
      NLON(1)=NSEGF
      NLAT(2)=NRIM
      NLON(2)=NSEGR
      NLAT(3)=NLAT1
      NLON(3)=NLON1
      NLAT(4)=NLAT2
      NLON(4)=NLON2
C
C     check dimensions
C
      IF(NLOBS.GT.MLAM) CALL STOP('NLOBS TOO BIG')
      IF(NRING.GT.MRING) CALL STOP('NRING TOO LARGE')
      IF(NSEGF.GT.MSEGF) CALL STOP('NSEGF TOO LARGE')
      IF(NRIM.GT.MRIM) CALL STOP('NRIM TOO LARGE')
      IF(NSEGR.GT.MSEGR) CALL STOP('NSEGR TOO LARGE')
      IF(NLAT1.GT.MLAT) CALL STOP('NLAT1 TOO LARGE')
      IF(NLON1.GT.MLON) CALL STOP('NLON1 TOO LARGE')
      IF(NLAT2.GT.MLAT) CALL STOP('NLAT2 TOO LARGE')
      IF(NLON2.GT.MLON) CALL STOP('NLON2 TOO LARGE')
C
      CALL MODSET
C
C     ******************************************************************
C     information about individual model spectra for all objects
C     which are considered (i.e. NLAT(IOBJ) > 0)
C     ******************************************************************
C
C     FILNAM   - name of the file where the spectrum is stored
C                (output on unit 41 generated by Synspecd)
C     NMU      - number of angle points for which the emergent intensities
C                of the synthetic spectrum were generated (by Synspecd).
C                By default, these angles are equidistant in mu (cosine),
C                between 1 (max) and AMUMIN (the next input parameter).
C                These values must be the same as those used in Synspec
C                (for stars) or Synspecd (for disk)
C              - if NMU < 0, values of angles are non-standard and are
C                read  
C     AMUMIN   - see above
C     CLD0     - coefficient of limb darkening (only if NMU=0)
C          
      DO 200 IOBJ=1,NOBJ
	  NMOD=NMODC(IOBJ)
	  DO 120 IMOD=1,NMOD
	    READ(IN1,*) FILNAM(IMOD,IOBJ),NMU,AMUMIN,CLD0(IMOD,IOBJ)
	    IF(NMU.GT.MMU) CALL STOP('NMU TOO LARGE')
	    IF(NMU.EQ.0) GO TO 120
	    NANGLE(IMOD,IOBJ)=ABS(NMU)
C
C           determination of the original normal angles:  
C           i) equidistant between 1 and AMUMIN - if NMU > 0
C           ii) read from unit IN1 - if NMU < 0
C
	    IF(NMU.GT.1) THEN
	       DMU=(1.D0-AMUMIN)/(NMU-1)
	       DO 110 IMU=1,NMU
		  ANGLE(IMU,IMOD,IOBJ)=1.D0-(IMU-1)*DMU
  110          CONTINUE
	     ELSE IF(NMU.LT.0) THEN
	       NMU=ABS(NMU)
	       READ(IN1,*) (ANGLE(IMU,IMOD,IOBJ),IMU=1,ABS(NMU))
	    END IF
  120    CONTINUE
  200 CONTINUE
C
C
C     ############################################################
C     ADDITIONAL INPUT FOR ACPGF - OUTPUT FROM ACPGE
C     ############################################################
C
      READ(IN2,*) MTH,NRM,NSG,NRING
      NLAT1=MTH
      NLAT2=MTH
      NRIM=NRM
      NSEGR=NSG
      NSEGF=NSG
C
      NLAT(1)=NRING
      NLON(1)=NSEGF
      NLAT(2)=NRIM
      NLON(2)=NSEGR
      NLAT(3)=NLAT1
      NLAT(4)=NLAT2
C
      IF(INMODE.EQ.0) THEN
C
C     loop over objects
C
      DO IOBJ=1,NOBJ
	  IF(NLAT(IOBJ).GT.0) THEN
C
C        if the object IOBJ is considered, loop over latitudes
C
	  DO 230 ILAT=1,NLAT(IOBJ)
	    READ(IN2,*) IOB,II,NLON1
	    MPH(ILAT,IOBJ)=NLON1
	    READ(IN2,*) (AREA(ILAT,J,IOBJ),J=1,NLON1)
	    READ(IN2,*) (VISIB(ILAT,J,IOBJ),J=1,NLON1)
	    READ(IN2,*) (COSG(ILAT,J,IOBJ),J=1,NLON1)
	    READ(IN2,*) (VPRO(ILAT,J,IOBJ),J=1,NLON1)
	    READ(IN2,*) (PH(J),J=1,NLON1)
  230    CONTINUE
	  END IF
      END DO
      RETURN
      END IF
C
C     ############################################################
C     temporary routines -evaluation of the above quantities for 
C     a disk face
C     and CORRESPONDING ADDITIONAL INPUT
C     ############################################################
C
C     MSTAR      - M(central star), either in M(Sun), or in grams;
C     RSTAR      - R(central star), either in R(Sun), or in cm
C
      READ(IN4,*) MSTAR,RSTAR
      IF(MSTAR.LT.1.D16) MSTAR=MSTAR*1.989D33
      IF(RSTAR.LT.1.D3) RSTAR=RSTAR*6.9598D10
      VKEP0=DSQRT(GRCON*MSTAR)*1.D-5
C
c     DINCL  - disk inclination;
C            - if positive - expresed as cos(i);
C            - if negative - expressed as angle (in degrees)
c     CLD    - coefficient of linear limb darkening
C              (has the meaning only if the input spectrum contains 
C              fluxes, not intensities.)
C
      READ(IN4,*) DINCL,CLD
      IF(DINCL.GT.0) THEN
	  AMU=DINCL
       ELSE
	  AMU=DCOS(-DINCL/180.D0*PI)
      END IF
C
C     determination of the azimutal angles of the individual seqments:
C     equidistant between 0 and pi, and representeD by an odd number
C     of points (in order to have pi/2 as one of the points)
C
C     NAZIM0   - number of azimutal seqments per quadrant
C                (more precisely, the total number of azimutal 
C                points considered by the program - NSEGF - is taken
C                as 2*NAZIM0-1)
C
      READ(IN4,*) NAZIM0
      NAZIM=2*NAZIM0-1
      IF(NAZIM.GT.MSEGF) CALL STOP('NSEGF TOO LARGE')
      IF(NAZIM.LE.1) THEN
	  NAZIM=1
	  AZIM0(1)=0.D0
       ELSE
	  DO 310 IAZ=1,NAZIM
	    AZIM0(IAZ)=PI*(IAZ-1)/(NAZIM-1)
  310    CONTINUE
      END IF
      NSEGF=NAZIM
C
C     RO - radial distance of the ring from the central star
C          (in unit of the stellar radius)
C
      DO 320 I=1,NRING         
	  READ(IN4,*) R0(I)
	  R0(I)=R0(I)*RSTAR
  320 CONTINUE
C
C     determination of DR0 - the radial thickness of a ring
C      (if there is only one ring, DR0 = 1)
C
      IF(NRING.EQ.1) THEN
	  DR0(1)=1.D0
       ELSE
	  DR0(1)=HA*(R0(2)-R0(1))
	  DR0(NRING)=HA*(R0(NRING)-R0(NRING-1))
	  IF(NRING.GT.2) THEN
	    DO 330 IRING=2,NRING-1
	       DR0(IRING)=HA*(R0(IRING+1)-R0(IRING-1))
  330       CONTINUE
	  END IF
      END IF
C
C     finally, all parameters for the disk face
C
      IOBJ=3
      DO 400 IRING=1,NRING
	  R=R0(IRING)
	  AREA0=PI2*R*DR0(IRING)/NSEGF 
c         area=1./nring/nsegf
	  VKEP=VKEP0/DSQRT(R)*DSQRT(1.D0-AMU*AMU)
	  CLD0(IRING,1)=CLD
	  DO 400 ISEG=1,NSEGF
	    AZIM=AZIM0(ISEG)
	    VPROJ=VKEP*1.D5*DCOS(AZIM)
	    COSG(IRING,ISEG,IOBJ)=AMU
	    AREA(IRING,ISEG,IOBJ)=AREA0
	    VISIB(IRING,ISEG,IOBJ)=1.D0
	    VPRO(IRING,ISEG,IOBJ)=VPROJ
c
c           diagnostic output of basic parameters for the segment
c           (commented out here)
c
C           vrel=vproj0/2.997925e5
C           write(6,600) azim,vproj,vrel,area
  400 CONTINUE
  600 format(/' azim,vproj,rel.shift,area:',2f10.3,1P2E15.3/)
      RETURN
      END
C
C     ******************************************************************
C
      
c
c
c
      subroutine stop(text)
c
c     stops the program and writes a text
c
      character*(*) text
      write(6,10) text
   10 format(1x,a)
      stop
      end
c
C
C     ******************************************************************
C
c
      SUBROUTINE MODSET
C     =================
C
C     input model atmosphere initialization routine
C     reads the synthetic spectra for all model stellar atmosphere
C     involved, and calculates the interpolation coefficients for
C     interpolation of computed models to actual model paramaters:
C     a) (Teff, log g) pairs - for models without irradiation; or
C     b) (total irradiated flux F_ir, Teff) pairs - for irradiated 
C        models.
C     all that for all actual points on the stellar surfaces
C
      INCLUDE 'COMPG5.FOR'
      DIMENSION TCOM1(MTCOMP), GCOM1(MGCOMP),IMODC1(MTCOMP,MGCOMP),
     *          T1(MLON),G1(MLON)
      DIMENSION RCOM1(MTCOMP), ECOM1(MTCOMP),IMODC2(MTCOMP,MTCOMP),
     *          R1(MLON),E1(MLON)
      PARAMETER (IN1=5, 
     *           IN3=20)
C
C
C     *****************
C     loop over objects
C     *****************
C
      DO 200 IOBJ=1,NOBJ
      IF(NLAT(IOBJ).LE.0) GO TO 200
C
C     if the object is considered, input of the following parameters:
C     (from the standard input - unit 5)
C
C     NTCOM1 - number of different T_eff's in computed grid of models
C     NGCOM1 - number of different log g's in computed grid of models
C     TCOM1  - values of T_eff's
C     GCOM1  - values of log g's
C     NRCOM1 - number of different incident fluxes in computed grid of models
C     NECOM1 - number of different Teff's in computed grid of irrad. models
C     RCOM1  - values of total irradiated fluxes F_ir
C     ECOM1  - values of T_eff's for irradited models
C
      NRCOM1=0
      NECOM1=0  
      READ(IN1,*) NTCOM1,NGCOM1
      IF(NTCOM1.GT.MTCOMP) CALL STOP('NTCOM1 TOO LARGE')
      IF(NGCOM1.GT.MGCOMP) CALL STOP('NGCOM1 TOO LARGE')
	DO I=1,NTCOM1
		READ(IN1,*) TCOM1(I)
	END DO
	DO I=1,NGCOM1
		READ(IN1,*) GCOM1(I)
	END DO
      IF(IRRAD(IOBJ).GT.0) THEN
	  READ(IN1,*) NECOM1,NRCOM1
	  IF(NRCOM1.GT.MTCOMP) CALL STOP('NRCOM1 TOO LARGE')
	  IF(NECOM1.GT.MTCOMP) CALL STOP('NECOM1 TOO LARGE')
	  IF(NECOM1.GT.0) READ(IN1,*) (ECOM1(I),I=1,NECOM1)
	  IF(NRCOM1.GT.0) READ(IN1,*) (RCOM1(I),I=1,NRCOM1)
      END IF
      IMOD=0
C     Set up IMODC1, IMODC2 repeatedly for each object.
C     Overwrite previous values for preceeding object
      DO 10 I=1,NTCOM1
	  DO 10 J=1,NGCOM1
	    IMOD=IMOD+1
	    IMODC1(I,J)=IMOD
	    NPOINT(IMOD,IOBJ)=0
   10 CONTINUE
      IF(IRRAD(IOBJ).GT.0) THEN
      DO 15 I=1,NECOM1
	  DO 15 J=1,NRCOM1
	    IMOD=IMOD+1
	    IMODC2(I,J)=IMOD
	    NPOINT(IMOD,IOBJ)=0
   15 CONTINUE
      END IF
      NMODC(IOBJ)=IMOD
C
C     loop over latitudes and longitudes of object IOBJ
C
      DO 190 ILAT=1,NLAT(IOBJ)
	  READ(IN3,*) IOB,II,NLON1
	  READ(IN3,*) (G1(J),J=1,NLON1)
	  READ(IN3,*) (T1(J),J=1,NLON1)
	  IF(IRRAD(IOBJ).GE.0) READ(IN3,*) (R1(J),J=1,NLON1)
	  DO 180 ILON=1,NLON1
	    T=T1(ILON)
	    G=G1(ILON)
	    R=R1(ILON)
	    G=DLOG10(DABS(G))
	    IF (IOBJ.GE.3) T=T*1000.D0
	    IF(IRRAD(IOBJ).EQ.0) THEN
			R=0.D0
		ENDIF
	    E=T
C
C           interpolation for models without irradiation
C
	    IF(R.LE.0) THEN
C
C           interpolation in T_eff
C
	    IT1=1
	    DO 20 IT=1,NTCOM1
C     TCOM1(IT) in ascending sequence
	       IF(T.LT.TCOM1(IT)) GO TO 30
   20       CONTINUE
   30       IF(IT.GE.2) IT1=IT-1
C     IT1 now points at the model temp. immediately preceeding
C     the given T value.
C
C           no extrapolation beyond the boundary values (NOINT=1)
C
	    NOINT=0
	    IF(T.LT.TCOM1(1).OR.T.GT.TCOM1(NTCOM1)) NOINT=1
C
C           interpolation in log g
C
	    IG1=1
	    DO 40 IG=1,NGCOM1
C     GCOM1(IG) in ascending sequence
	       IF(G.LT.GCOM1(IG)) GO TO 50
   40       CONTINUE
   50       IF(IG.GE.2) IG1=IG-1
C     IG1 now points at the model gravity immediately preceeding
C     the given G value.
C
C           no extrapolation beyond the boundary values (NOING=1)
C
	    NOING=0
	    IF(G.LT.GCOM1(1).OR.G.GT.GCOM1(NGCOM1)) NOING=1
C
C           set up the appropriate interpolation coefficients
C
	    IT2=IT1+1
	    IG2=IG1+1
C     IT2, IG2 point at the models immediately following the 
C     given T, G values.
	    DX=TCOM1(IT2)-TCOM1(IT1)
	    DY=GCOM1(IG2)-GCOM1(IG1)
	    IF(NTCOM1.GT.1.AND.NOINT.EQ.0) THEN
	       X1=(T-TCOM1(IT1))/DX
	       X2=(TCOM1(IT2)-T)/DX
	     ELSE
	       X1=0.D0
	       X2=1.D0
	    END IF
	    IF(NGCOM1.GT.1.AND.NOING.EQ.0) THEN
	       Y1=(G-GCOM1(IG1))/DY
	       Y2=(GCOM1(IG2)-G)/DY
	     ELSE
	       Y1=0.D0
	       Y2=1.D0
	    END IF
C
C           first point IT1, IG1
C
	    IMOD=IMODC1(IT1,IG1)
	    IF(IMOD.LE.0) CALL STOP(' no available model')
	    NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	    IPOINT=NPOINT(IMOD,IOBJ)
	    ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	    ILON1(IPOINT,IMOD,IOBJ)=ILON
		CONTR(IPOINT,IMOD,IOBJ)=Y2*X2
C
C           second point IT2, IG1
C
	    IF(NTCOM1.GT.1.AND.NOINT.EQ.0) THEN
	       IMOD=IMODC1(IT2,IG1)
	       NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	       IPOINT=NPOINT(IMOD,IOBJ)
	       ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	       ILON1(IPOINT,IMOD,IOBJ)=ILON
		   CONTR(IPOINT,IMOD,IOBJ)=Y2*X1
	    END IF
C
C           third point IT1, IG2
C
	    IF(NGCOM1.GT.1.AND.NOING.EQ.0) THEN
	       IMOD=IMODC1(IT1,IG2)
	       NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	       IPOINT=NPOINT(IMOD,IOBJ)
	       ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	       ILON1(IPOINT,IMOD,IOBJ)=ILON
		   CONTR(IPOINT,IMOD,IOBJ)=Y1*X2
	    END IF
C
C           fourth point IT2, IG2
C
	    IF(NTCOM1.GT.1.AND.NGCOM1.GT.1.AND.
     *         NOINT.EQ.0.AND.NOING.EQ.0) THEN
	       IMOD=IMODC1(IT2,IG2)
	       NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	       IPOINT=NPOINT(IMOD,IOBJ)
	       ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	       ILON1(IPOINT,IMOD,IOBJ)=ILON
		   CONTR(IPOINT,IMOD,IOBJ)=Y1*X1
	    END IF
	    ELSE
C       
C           analogously for models computed with irradiation
C
C
C           interpolation in incident flux
C
	    IR1=1
	    DO 120 IR=1,NRCOM1
	       IF(R.LT.RCOM1(IR)) GO TO 130
  120       CONTINUE
  130       IF(IR.GE.2) IR1=IR-1
C
C           no extrapolation beyond the boundary values (NOINR=1)
C
	    NOINR=0
	    IF(R.LT.RCOM1(1).OR.R.GT.RCOM1(NRCOM1)) NOINR=1
C
C           interpolation in effective temp. for irradiated models
C
	    IE1=1
	    DO 140 IE=1,NECOM1
	       IF(E.LT.ECOM1(IE)) GO TO 150
  140       CONTINUE
  150       IF(IE.GE.2) IE1=IE-1
C                                               
C           no extrapolation beyond the boundary values (NOINE=1)
C
	    NOINE=0
	    IF(E.LT.ECOM1(1).OR.E.GT.ECOM1(NECOM1)) NOINE=1
C
C           set up the appropriate interpolation coefficients
C
	    IR2=IR1+1
	    IE2=IE1+1
	    DX=ECOM1(IE2)-ECOM1(IE1)
	    DY=RCOM1(IR2)-RCOM1(IR1)
	    IF(NECOM1.GT.1.AND.NOINE.EQ.0) THEN
	       X1=(E-ECOM1(IE1))/DX
	       X2=(ECOM1(IE2)-E)/DX
	     ELSE
	       X1=0.D0
	       X2=1.D0
	    END IF
	    IF(NRCOM1.GT.1.AND.NOINR.EQ.0) THEN
	       Y1=(R-RCOM1(IR1))/DY
	       Y2=(RCOM1(IR2)-R)/DY
	     ELSE
	       Y1=0.D0
	       Y2=1.D0
	    END IF
C
C           first point IE1, IR1
C
	    IMOD=IMODC2(IE1,IR1)
	    IF(IMOD.LE.0) CALL STOP(' no available model')
	    NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	    IPOINT=NPOINT(IMOD,IOBJ)
	    ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	    ILON1(IPOINT,IMOD,IOBJ)=ILON
	    CONTR(IPOINT,IMOD,IOBJ)=Y2*X2
C
C           second point IE2, IR1
C
	    IF(NECOM1.GT.1.AND.NOINE.EQ.0) THEN
	       IMOD=IMODC2(IE2,IR1)
	       NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	       IPOINT=NPOINT(IMOD,IOBJ)
	       ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	       ILON1(IPOINT,IMOD,IOBJ)=ILON
	       CONTR(IPOINT,IMOD,IOBJ)=Y2*X1
	    END IF
C
C           third point IE1, IR2
C
	    IF(NRCOM1.GT.1.AND.NOINR.EQ.0) THEN
	       IMOD=IMODC2(IE1,IR2)
	       NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	       IPOINT=NPOINT(IMOD,IOBJ)
	       ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	       ILON1(IPOINT,IMOD,IOBJ)=ILON
	       CONTR(IPOINT,IMOD,IOBJ)=Y1*X2
	    END IF
C
C           fourth point IE2, IR2
C
	    IF(NECOM1.GT.1.AND.NRCOM1.GT.1.AND.
     *         NOINE.EQ.0.AND.NOINR.EQ.0) THEN
	       IMOD=IMODC2(IE2,IR2)
	       NPOINT(IMOD,IOBJ)=NPOINT(IMOD,IOBJ)+1
	       IPOINT=NPOINT(IMOD,IOBJ)
	       ILAT1(IPOINT,IMOD,IOBJ)=ILAT
	       ILON1(IPOINT,IMOD,IOBJ)=ILON
	       CONTR(IPOINT,IMOD,IOBJ)=Y1*X1
	    END IF
	      END IF
  180    CONTINUE
  190 CONTINUE
  200 CONTINUE
      RETURN
      END      
