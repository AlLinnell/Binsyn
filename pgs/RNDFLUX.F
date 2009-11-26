C *****************************************************************************
      

      FUNCTION  RNDFLUX ( SIGMA, FLUX, B )

C     This function subroutine calculates random errors in normalized light
C     values which ere added to the given light [FLUX] according to formula (1)
C     in :
C              ApJ,162:683-686, Nov 1970
C              Albert P. Linnell & Deanne D. Proctor
C              ON WEIGHTS IN KOPALS'S ITERATIVE METHOD FOR TOTAL ECCLIPSE


      IMPLICIT DOUBLE PRECISION  ( A-H, O-Z )

	IFLAG=0
	
      RNDERR  = GASDEV(IFLAG)                ! normalized Gauss distribution

      DL      = SIGMA * RNDERR * FLUX**B    ! see formula (1)

      RNDFLUX = FLUX + DL                   ! return random light
 
      RETURN

      END


C *****************************************************************************


