C     QSA1:[RM307.DEVELOP]COMD.FOR
C     Fourth COMMON block
      DIMENSION RWL(14),RTMP(22),RGRAV(10),U1R(10,22,14),
     $U2R(10,22,14),RFLX(2,5,6,40401),WLVRY(40401)
      COMMON /CD/ RWL,RTMP,RGRAV,U1R,U2R,RFLX,WLVRY
      SAVE /CD/
      