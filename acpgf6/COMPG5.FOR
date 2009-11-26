C
C    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C    File COMPGF.FOR
C    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C      dimensions of arrays:
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (MLAT   = 201,    ! maximum number of latitudes
     *           MLON   = 201,    ! maximum number of longitudes
     *           MRING  = 201,    ! maximum number of disk rings
     *           MSEGF  = 501,    ! maximum number of azimuthal
                                  ! segments
     *           MRIM   =  25,    ! maximum number of rim latitudes
     *           MSEGR  = 501,    ! maximum number of rim segments                             
     *           MOBJ   =   4,    ! maximum number of objects
     *           MMU    =  10,    ! maximum number of angles 
     *           MLAM   = 500000,  ! maximum number of wavelengths 
     *           MLAM1  = 500000,  ! maximum number of fine-grid wavelengths
     *           MMODC  =   50,   ! maximum number of computed models
     *           MPOINT = 20000,   ! maximum number of points corresponding
     *                            ! to one computed model
     *           MTCOMP =   75,   ! maximum number of Teff's
     *           MGCOMP =    5,   ! maximum number of log g's
     *           VELC   = 2.997925E5)   ! light speed
C
C      Actual parameters:
C
C      NRING   - number of the disk rings
C      NSEGF   - number of azimuthal segments of the disk face
C      NRIM    - number of the disk rim latitude segments
C      NSEGR   - number of the disk rim azimuthal segments
C      NLAT1   - number of the latitude rings on the central star surface
C      NLON1   - number of the longitudinal segments on this star
C      NLAT2   - number of the latitude rings on the other star surface
C      NLON2   - number of the longitudinal segments on this star
C      MPH     - array of the number of longitudes for given latitude
C                and star
C    
C      if any of these numbers equals to zero, the given object is not
C      considered
C      NRANG   - number of rings specified in program ACPGA
C
      COMMON/PARA/NRING,NSEGF,NRIM,NSEGR,NLAT1,NLON1,NLAT2,NLON2,
     *            NLAT(MOBJ),NLON(MOBJ),NOBJ,
     *            MPH(MLAT,MOBJ),NRANG,NRMA,NSGA,NTHA,NPHA
C
C
C      Parameters for all segments which should be supplied by ACPGE:
C
C      COSG    - cosine of the angle between surface normal and the 
C                line of sight
C      AREA    - projected area of a segment
C      VISIB   - visibility parameter for a segment
C      VPRO    - projected velocity of a segment to the line of sight
C
C      for generally four objects, distinguished by the last character 
C      the first index labels latitudes (or radial distance for disk)
C      the second index labels longitudes (or azimuths in disk)
C      the third index labels the object:
C
C      an individual disk ring                              - IOBJ=1
C      an individual latitude ring on the disk rim          - IOBJ=2
c      an individual latitude ring of the disk central star - IOBJ=3
c      an individual latitude ring of the other star        - IOBJ=4
C
      COMMON/SEGM/COSG(MLAT,MLON,MOBJ),
     *            AREA(MLAT,MLON,MOBJ),
     *            VISIB(MLAT,MLON,MOBJ),
     *            VPRO(MLAT,MLON,MOBJ)
c
C      Parameters for input synthetic spectra of all objects
C      the first index labels the individual model, 
C      the second index label the object:
c
      CHARACTER*80 FILNAM(MMODC,MOBJ)
      COMMON/FILE/FILNAM         ! names of files where synthetic spectra
c                                !  for all models are stored
c
c      Information about angles for which the synthetic spectra of all 
c      objects are computed:
c     
c      ANGLE(IMU,I,IOBJ) - angles (labelled by IMU) for ring I of
c                          object IOBJ
c      NANGLE(I,IOBJ)    - number of angles for ring I of object IOBJ
c     
      COMMON/ANGL/ANGLE(MMU,MMODC,MOBJ), 
     *            NANGLE(MMODC,MOBJ)
C                                      
c     coefficient of linear limb darkening
c     (has the meaning only if the input spectrum contains fluxes, not 
c     intensities. if =0, input spectrum have to be
c     given as a set of intensities
C
      COMMON/LIMD/CLD0(MMODC,MOBJ), ! all values for all models and objects
     *            CLD,             ! the current value
     *            CLCON            ! conversion factor = 4/(1-CLD/3)
c
C     synthetic spectra
C
      COMMON/SPEC/WLAM(MLAM),      ! wavelengths in the synthetic spectra
     *            OBSLAM(MLAM),    ! observer's frame wavelengths
     *            RINT0(MLAM,MMU), ! detailed synthetic spectrum 
     *            RINSEG(MLAM),    ! working array: current spectrum
     *            V1,V2,           ! curr. limiting values of proj.velocity
     *            VELRES           ! velocity resolution parameter
c
C     fine-mesh synthetic spectra
C
      COMMON/SPEC/WLAM1(MLAM1),     ! wavelengths in the synthetic spectra
     *            RINT1(MLAM1,MMU), ! detailed synthetic spectrum 
     *            WLM1,             ! first (lowest) wavelength
     *            WLCON,            ! contant for evaluating wavel.indices
     *            DWFINE,           ! fine-mesh wavelength step
     *            NLAM1             ! number of fine-mesh wavelengths
c
c     Parameters for a current segment
c
      COMMON/CURR/AMU0(MMU),      ! normal angles
     *            NMU,            ! number of the normal angles 
     *            NLSYN,          ! number of wavelengths in the synthetic
     *            NLOBS           ! number of observer's frame wavelengths
c
c     Paramters for indivudual computed models for all objects
c
      COMMON/MODS1/CONTR(MPOINT,MMODC,MOBJ),  ! interpol.coeff.
     *             ILAT1(MPOINT,MMODC,MOBJ),  ! latitudes
     *             ILON1(MPOINT,MMODC,MOBJ),  ! longitudes
     *             NPOINT(MMODC,MOBJ),        ! number of points
     *             NMODC(MOBJ),               ! number of computed models
	*             IRRAD(MOBJ)                ! irradiation switch            
c
