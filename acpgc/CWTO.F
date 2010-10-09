      SUBROUTINE CWTO(NW,NSG,NRM,NRING,IRCT,WLV,TMPRF,TMPRFA,SMV,TMPRR,
     $RMIFLX,NOUT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to output data from Program ACPGC
C     U1F(I,J)=first order limb darkening coeff. for face of disk
C     U2F(I,J)=second order limb dark. coeff. for face of disk
C     BDIF(I,J)=intensity normal to face of disk, per micron, at
C        wavelength WLV
C     TMPRF(I,J)=Teff of face with irradiation if keys so specify
C     TMPRFA(I,J)=Teff of face without irradiation
C     SMV(I,J)=incident flux from central star
C     U1RM(I,J)=first order limb dark. coeff. for rim
C     U2RM(I,J)=second order limb dark. coeff. for rim
C     BDIR(I,J)=intensity normal to rim, in microns,
C         for wavelength WLV
C     TMPRR(I,J)=Teff of rim with irradiation RMIFLX
C     RMIFLX(I,J)=incident flux from external source
      INCLUDE 'ACOMA.FOR'
      DIMENSION TMPRF(MRING,MSG),TMPRR(MRM,MSG),TMPRFA(MRING,MSG),
     $SMV(MRING,MSG),RMIFLX(MRM,MSG)
  100 FORMAT(4I5)
  101 FORMAT(5E22.13)
      WRITE (NOUT,100) NW,NSG,NRM,NRING,IRCT
      WRITE (NOUT,101) WLV
      DO I=1,NRING
      WRITE (NOUT,100) I
      WRITE (NOUT,101) (U1F(I,J),J=1,NSG)
      WRITE (NOUT,101) (U2F(I,J),J=1,NSG)
      WRITE (NOUT,101) (BDIF(I,J),J=1,NSG)
      WRITE (NOUT,101) (TMPRF(I,J),J=1,NSG)
      WRITE (NOUT,101) (TMPRFA(I,J),J=1,NSG)
	WRITE (NOUT,101) (SMV(I,J),J=1,NSG)
      END DO
      DO I=1,NRM
      WRITE (NOUT,100) I
      WRITE (NOUT,101) (U1RM(I,J),J=1,NSG)
      WRITE (NOUT,101) (U2RM(I,J),J=1,NSG)
      WRITE (NOUT,101) (BDIR(I,J),J=1,NSG)
      WRITE (NOUT,101) (TMPRR(I,J),J=1,NSG)
	WRITE (NOUT,101) (RMIFLX(I,J),J=1,NSG)
      END DO
      RETURN
      END
