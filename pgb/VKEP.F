      SUBROUTINE VKEP(A,E,VM,Q,VE,V,R,NMAX,N,KER)
C     Program to solve Kepler's equation for position in elliptic
C     orbits.
C     Input, A=    semi-major axis
C            E=    orbital eccentricity
C            VM=   mean anomaly
C            Q=    error limit on solution
C            VE=   eccentric anomaly
C            V=    true anomaly
C            R=    separation of components at true anomaly
C            N=    number of iterations needed to solve Kepler's eqn.
C            NMAX= maximum number of iterations allowed
C            KER=  error return, =0 if OK, =1 if require more than
C                  NMAX iterations, =2 if eccentricity is outside
C                  bounds
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA TWPI/6.2831853071796D0/
      KER=0
      VM=DMOD(VM,TWPI)
   40 IF (E)5,6,4
    6 VE=VM
      V=VM
      R=A
      N=0
      GO TO 3
    4 IF (1.D0-E)5,5,8
    8 VE=VM+E*DSIN(VM)
      DO 2 K=1,NMAX
      N=K
      VMC=VE-E*DSIN(VE)
      DE=(VM-VMC)/(1.D0-E*DCOS(VE))
      IF (DABS(DE).LT.Q) GO TO 9
    2 VE=VE+DE
      KER=1
      GO TO 9
    3 RETURN
    9 VE=VE+DE
      R=A*(1.D0-E*DCOS(VE))
      V=DSQRT((1.D0+E)/(1.D0-E))*DTAN(VE/2.D0)
      V=2.D0*DATAN(V)
   10 IF (V.GT.0.D0) GO TO 11
      V=V+TWPI
      GO TO 10
   11 IF (V.LT.TWPI) GO TO 3
      V=V-TWPI
      GO TO 11
    5 KER=2
      GO TO 3
      END
