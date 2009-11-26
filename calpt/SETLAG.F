      SUBROUTINE SETLAG (I,ERP,KLM1)
C SUBROUTINE TO SET LOCATIONS OF L1, L2 POINTS
C ALSO CALCULATES INNER AND OUTER CRITICAL POTENTIALS
       INCLUDE 'COMA.FOR'
C     The following value of VLMU will be .LE. 0.5, irrespective what
C     value Q has.
C     VLMU is the mass of the less massive component, in units of the
C     system mass.
       VLMU=Q/(1.D0+Q)
       OMVMU=1.D0-VLMU
C     X1 is the X axis coordinate of object VLMU.
C     See the discussion in Moulton, p.290ff.
       X1=-VLMU
C     X2 is the X axis coordinate of the other object.
       X2=VLMU/Q
C FIND L1
C     The L1 point lies between the two objects. Start a scan of the
C     interval between them at a point essentially at VLMU.
       VMN=X1+0.001D0
C     The end point in the search is essentially at the other component.
       VMX=X2-0.001D0
C     Divide the interval into 10 steps.
       DX=(VMX-VMN)/10.D0
C     Set the initial X displaced in the negative X direction by DX
       X=VMN-DX
C     The following DO loop is guaranteed to find an approximate
C     location of the L1 point.
       DO J=1,11
C     Increment X at each step. Since the entry value of X was 
C     displaced in the negative X direction by DX, the first X value
C     is VMN, which is very close to X1.
        X=X+DX
C     ROOTFN tests the condition for double points on the X axis, as
C     given in Moulton, p.291. Very close to X1 the value of ROOTFN
C     is negative. Someplace between X1 and X2 the value of ROOTFN,
C     returned in parameter V, becomes positive. Then the location of
C     L1 has been bracketed between X-DX and X.
        CALL ROOTFN (X,X1,X2,OMVMU,VLMU,V)
        IF (V.GT.0.D0) GO TO 10
       END DO
       CALL PMDSTOP
C     On reaching here, the L1 point lies between X-DX and X.
 10    VMX=X
       VMN=X-DX
C     Call RTFD to locate L1 precisely. It lies between VMN and VMX.
C     X1 and X2 are the X axis locations of the two components, with
C     the separation=1.0. OMVMU and VLMU are the normalized masses
C     calculated above, VLG1 is the location of the L1 point relative
C     to component VLMU, ROOTFN is the SRT that tests for the root
C     condition, ERP is the allowed error, and KLM1 is the maximum
C     number of iterations to find the L1 point.
       CALL RTFDA (VMN,VMX,X1,X2,OMVMU,VLMU,VLG1,ERP,KLM1)
C     Calculate coordinate value for L1 point
       VL(I,1)=VLG1+VLMU               ! L1 wrt comp. 1
C     Calculate Roche potential for L1 point
       CALL RCH0 (VL(I,1),OMC(I,1))
       XMN(I)=VL(I,1)
       XMN(3-I)=1.D0-XMN(I)
C     Set for opposite value of mass ratio
       Q=1.D0/Q
C     Calculate L1 point with respect to other component.
       VL(3-I,1)=1.D0-VL(I,1)           ! L1 wrt comp. 2
C     Roche potential with reespect to other component
       CALL RCH0 (VL(3-I,1),OMC(3-I,1))
C     Reset mass ratio for original value
       Q=1.D0/Q
C     Choose proper continuation to find L2 point
       IF (Q.GT.1.D0) GOTO 100
c m2 <= m1 or q <= 1; i.e. L3,m1,origin,L1,m2,L2
c FIND L2
C     Set maximum possible distance to L2 point, from origin.
       VMX=2.D0
C     Minimum possible distance to L2 point, from origin.
       VMN=X2+0.001D0
       DX=(VMX-VMN)/10.D0
C     Set starting X displaced by -DX
       X=VMN-DX
       DO J=1,11
        X=X+DX
C     Test for double point. Initial value of V is negative. At some
C     point between VMN and VMX, the value of V will be negative.
C     At that point, the root is bracketed between X-DX and X.
        CALL ROOTFN (X,X1,X2,OMVMU,VLMU,V)
        IF (V.GT.0.D0) GO TO 20
       END DO
       CALL PMDSTOP
C     Now use RTFDA to locate root precisely.
 20    VMX=X
       VMN=X-DX
       CALL RTFDA (VMN,VMX,X1,X2,OMVMU,VLMU,VLG2,ERP,KLM1)
       VL(I,2)=VLG2-X1                 ! L2 wrt comp. 1
C     Calculate Roche potential at L2 point
       CALL RCH0 (VL(I,2),OMC(I,2))
       Q=1.D0/Q
       VL(3-I,2)=VL(I,2)-1.D0           ! L2 wrt comp. 2
       OMC(3-I,2)=1.D0/VL(3-I,2)+Q*(1.D0/(VL(3-I,2)+1.D0)+VL(3-I,2))
     $ +0.5D0*(1.D0+Q)*VL(3-I,2)**2
       Q=1.D0/Q
       goto 200

c m2 > m1 or q > 1; i.e. L2,m1,L1,origin,m2,L3
c FIND L2
 100   VMX=X1-0.001D0
       VMN=-2.D0
       DX=(VMX-VMN)/10.D0
       X=VMN-DX
       DO J=1,11
        X=X+DX
        CALL ROOTFN (X,X1,X2,OMVMU,VLMU,V)
        IF (V.GT.0.D0) GO TO 110
       END DO
       CALL PMDSTOP
 110   VMX=X
       VMN=X-DX
       CALL RTFDA (VMN,VMX,X1,X2,OMVMU,VLMU,VLG2,ERP,KLM1)
       VL(I,2)=X1-VLG2                 ! L2 wrt comp. 1
       OMC(I,2)=0.5D0*(1.D0+Q)*VL(I,2)**2+Q*(1.D0/(VL(I,2)+1.D0)+
     $ VL(I,2))+1.D0/VL(I,2)
       Q=1.D0/Q
       VL(3-I,2)=VL(I,2)+1.D0           ! L2 wrt comp. 2
       CALL RCH0 (VL(3-I,2),OMC(3-I,2))
       Q=1.D0/Q
 200   RETURN
      END
