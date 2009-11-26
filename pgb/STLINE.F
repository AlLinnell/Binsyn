      SUBROUTINE STLINE(AR3,AR2,KY,IER)
C     Subroutine to determine the equation of a straight line through
C     two points of arrays AR3 and AR2
C     Straight line goes through first two or final two points,
C          depending on control KY
C     At entry, AR3 contains the abscissae, AR2 the ordinates
C     At exit, AR2 contains the parameters of the straight line
C     The straight line equation is y=b+mx
C     Fir final two points, we have
C     b=(x3y2-x2y3)/(x3-x2), m=(y3-y2)/(x3-x2)
C     At exit, AR2(1)=b, AR2(2)=m
C     If (x3-x2)=0.0, return with error code 129 in IER
C     Works in conjunction with SRT PARAB
C
      DIMENSION AR3(3),AR2(3)
      IF (KY.EQ.0) DON=AR3(2)-AR3(1)
      IF (KY.EQ.1) DON=AR3(3)-AR3(2)
      IF (DON.NE.0.0) GO TO 2
      IER=129
      GO TO 99
    2 IER=0
      IF (KY.EQ.0) VNUM=(AR3(2)*AR2(1)-AR3(1)*AR2(2))
      IF (KY.EQ.1) VNUM=(AR3(3)*AR2(2)-AR3(2)*AR2(3))
      IF (KY.EQ.0) VNUM2=AR2(2)-AR2(1)
      IF (KY.EQ.1) VNUM2=AR2(3)-AR2(2)
      AR2(1)=VNUM/DON
      AR2(2)=VNUM2/DON
      AR2(3)=0.D0
   99 RETURN
      END

