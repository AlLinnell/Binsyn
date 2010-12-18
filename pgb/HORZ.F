      SUBROUTINE HORZ(ISEQ,IRC,IFC,NPB,NPE,NKP,								
     $I,XBGEQ,YBGEQ,
     $XNDEQ,YNDEQ,IEC,XG,YG,ISV,IRD,ICOUNT,KCOR,NOUT7)
C     SRT TO DETERMINE COEFFICIENTS FOR PARABOLIC FITS TO ARRAYS
C     OF HORIZON POINTS OF A BINARY COMPONENT,AS PROJECTED ON THE PLANE  
C     OF THE SKY 
C     THE PROJECTED HORIZON POINTS,FOR THE TWO STELLAR COMPONENTS,ARE
C     IN ARRAYS XHZB,YHZB AND XHZE,YHZE
C     THE PROJECTED POINTS FOR NECK POINTS,IF THE EXIST,ARE IN XNK,YNK
C     THEY HAVE BEEN PRODUCED BY SRT SHAD1   
C     ARYB CONTAINS THE COEFFS.FOR THE CCW BRANCH
C     ARYA CONTAINS THE COEFFS.FOR THE CW BRANCH
C     ARYC CONTAINS THE COEFFS.FOR THE NECK
C     HORZ IS CALLED BY SRT SHAD2 AND THIS SRT DETERMINES FITS TO THE    
C     PROJECTED BOUNDARY OF THE ECLIPSING STAR      
C     HORZ MAY BE USED TO DETERMINE FITS TO THE PROJECTED BOUNDARY OF
C     THE ECLIPSED STAR. PROGRAM PLOTB DOES THIS.
C     THE XS,YS AND XN,YN COORDINATES HAVE THEIR ORIGIN AT THE
C     PROJECTED CENTER OF THE STAR AT THE ORIGIN
C     THE XS AND XN COORDINATES ARE MEASURED ALONG THE PROJECTED LINE
C     OF CENTERS,POSITIVE TOWARD THE CENTER OF THE COMPANION
C     IEC DESIGNATES THE ECLIPSING STAR,3-IEC IS THE ECLIPSED STAR
C     ISV IS THE DESIGNATION OF THE STAR BEING ECLIPSED AT ORBITAL
C     PHASE 0.0
C     KL IS THE DESIGNATION OF THE THETA CIRCLE,AROUND THE UPPER POLE,   
C     WHICH IS THE FIRST PARTLY VISIBLE PROFILE BEYOND THOSE ENTIRELY    
C     BELOW THE HORIZON 
C     LJ IS THE DESIGNATION OF THE LAST PARTLY VISIBLE THETA CIRCLE,     
C     AROUND THE LOWER POLE,BEFORE THOSE THAT ARE FULLY VISIBLE   
C     NOTE THAT SRT SHAD1 HAS PREPARED ARRAYS FOR THE HORIZON POINTS,
C     IN ARRAYS XHZB,YHZB,XHZE,YHZE.
C     THERE ARE NPB POINTS IN ARRAYS XHZB,YHZB
C     THERE ARE NPE POINTS IN ARRAYS XHZE,YHZE
C     THERE ARE NKP POINTS IN ARRAYS XNK,YNK
C     THE PROJECTED COODS.OF THE CENTER OF THE STAR NOT AT THE
C     ORIGIN ARE AT (XG,YG)
C     IRD IS A PARAMETER WHICH INDICATES THE ORDER IN WHICH THE ENTRIES
C     IN YR,YF ARE SORTED.IF IRD=1.0,SORT IN DECREASING SEQUENCE.IF
C     IRD=-1.0,SORT IN INCREASING SEQUENCE.THE INITIAL ENTRY IN YR OR
C     YF THEN IS THE POINT MORE REMOTE FROM THE PROJECTED CENTER OF THE
C     COMPANION
	INCLUDE 'COMA.FOR'
      DIMENSION AR2(MTR),AR3P(MTR),ALPP(MTHM,2),RSP(MTHM,2),POLY1(3),
     $POLY2(3)
C
      DATA PIO2/1.5707963267948966192D0/
      DATA PI/3.141592653589793238463D0/
      DATA TWPI/6.2831853071795864769D0/
      DATA TPIO2/9.424777960769379715363D0/
      DATA ANG/6.29D0/
      DATA TST/4.0D0/
	DATA ERLT/1.D-13/
      DO 1 K=1,MTHM
      XR(K)=10.0D0
      YR(K)=10.0D0
      XF(K)=10.0D0
      YF(K)=10.0D0
    1 CONTINUE   
      OPEN (UNIT=30,FILE='SCRATCH.DAT',STATUS='REPLACE')
C      PSIT=PSI
C      IF ((I.EQ.2).AND.(ICOUNT.EQ.1)) PSIT=PSI-PI
C      IF (ICOUNT.EQ.2) PSIT=PSI-PI
C      IF (PSIT.LT.0.0D0) PSIT=PSIT+TWPI
C     Test to determine the displacements to use to transfer
C     origin to center of star I
C     When the displacement is complete, the extremes of YHZB
C     will be symmetric about the origin, (~), so the mean of
C     YHZB(1) and YHZB(NPB) will be essentially 0.0. Hence,
C     calculate A and B below. Whichever is near 0.0 identifies
C     the displacement to use. In some orbital longitudes the
C     Y displacement (sideways) is near 0.0 anyhow and only a
C     displacement in X is needed. Use a comparable test in
C     XHZB in this case. If both the displacements are small,
C     but the X displacement is smaller, use the YHZB test.
      NRF=NPB
	IF (ICOUNT.EQ.1) THEN
	OPEN (UNIT=33,FILE='SCRATCHB.DAT',STATUS='REPLACE')
	DO K=1,NPE
		WRITE (33,1000) K,XHZE(K),YHZE(K)
	END DO
	DO K=1,NPB
		WRITE (33,1000) K,XHZB(K),YHZB(K)
	END DO
	END IF
c      IF (ICOUNT.EQ.2) THEN
	DO J=1,NPB
c		WRITE (30,1000)J,XHZB(J),YHZB(J)
	END DO
c	WRITE (30,1001)
	DO J=1,NPE
c		WRITE (30,1000) J,XHZE(J),YHZE(J)
	END DO
c	WRITE (30,1001)
c	WRITE (30,1000) NPB,XBGEQ,YBGEQ
c	WRITE (30,1000) NPE,XNDEQ,YNDEQ
c	WRITE (30,1001)
c	WRITE (30,1001)
c      END IF
    2 IF (YHZB(NRF).GT.TST) THEN
         NRF=NRF-1
         IF ((NPB-NRF).LT.3) GO TO 2
            CALL PMDSTOP
      END IF
C	A=DSQRT((SMX+XG)**2+(SMY+YG)**2)
C      B=DSQRT((SMX-XG)**2+(SMY-YG)**2)
	IF (B.LT.A) KCOR=-1
	IF (B.GE.A) KCOR=1 
C	WRITE (30,1000) NRF,SMX,SMY
C	WRITE (30,1000) NRF,XG,YG
C	WRITE (30,1000) NRF,A,B
C	WRITE (30,1111) I,ISV
 1111 FORMAT (5I5)                       
C     If I.EQ.ISV, the origin already is located at the projected
C     center of star I.      
      IF (I.EQ.ISV) GO TO 40
C     DISPLACE ORIGIN TO PROJECTED CENTER OF STAR, IF NECESSARY
C     I.NE.ISV so center of star is at YG,XG
      DO 3 K=1,NPB
      IF (ISV.NE.IEC) THEN
      XHZB(K)=XHZB(K)-YG
      YHZB(K)=YHZB(K)-XG
      ELSE
      XHZB(K)=XHZB(K)+YG
      YHZB(K)=YHZB(K)+XG
      END IF
    3 CONTINUE
      DO 4 K=1,NPE
      IF (ISV.NE.IEC) THEN 
      XHZE(K)=XHZE(K)-YG
      YHZE(K)=YHZE(K)-XG
      ELSE
      XHZE(K)=XHZE(K)+YG
      YHZE(K)=YHZE(K)+XG
      END IF
    4 CONTINUE
      IF (ISV.NE.IEC) THEN
         XBGEQ=XBGEQ-YG
         YBGEQ=YBGEQ-XG
         XNDEQ=XNDEQ-YG
         YNDEQ=YNDEQ-XG
      ELSE
         XBGEQ=XBGEQ+YG
         YBGEQ=YBGEQ+XG
         XNDEQ=XNDEQ+YG
         YNDEQ=YNDEQ+XG
      END IF
 2000 FORMAT (2F40.20)	
      DO 7 K=1,NKP
      IF (ISV.NE.IEC) THEN
      XNK(K)=XNK(K)-YG
      YNK(K)=YNK(K)-XG
      ELSE
      XNK(K)=XNK(K)+YG
      YNK(K)=YNK(K)+XG
      END IF
    7 CONTINUE
   40 CONTINUE
C 1000 FORMAT (I5,2F40.20)
C 1001 FORMAT (' ')
      IF (ICOUNT.EQ.2) THEN
	DO JJ=1,NPB
c	WRITE (30,1000) JJ,XHZB(JJ),YHZB(JJ)
	END DO
c	WRITE (30,1001)
	DO JJ=1,NPE
c	WRITE (30,1000) JJ,XHZE(JJ),YHZE(JJ)
	END DO
c	WRITE (30,1001)
c	WRITE (30,1000) NPB,XBGEQ,YBGEQ
c	WRITE (30,1000) NPE,XNDEQ,YNDEQ
c	WRITE (30,1001)
c	WRITE (30,1001)
	END IF
c	CLOSE (30,STATUS='KEEP')
C	IF (I.LT.1000) STOP  
C     At this point, the origin is at the projected center of
C     star I. We now must locate the horizon point most remote
C     from the companion.
C     XINIT and YINIT will be the coods. of the point on the
C     horizon of star I most remote from the companion.      
C     Initialize XINIT, YINIT to values that are wrong and
C     will be corrected.
C     The two equatorial points on the horizon of star I are
C     XBGEQ,YBGEQ and XNDEQ,YNDEQ. One of these is the point
C     most remote from the companion, for orbital longitudes
C     near maximum elongations.
      XINIT=ANG
      YINIT=ANG
C     Choose initial point for XR,YR arrays and XF,YF arrays.
C     We have already displaced origin, so XBGEQ,YBGEQ no longer
C     are initialized to ANG. Use related test.
C     Neither of the following two tests applies when no neck
C     points are visible.
      IF ((XBGEQ.GT.3.D0).AND.(YBGEQ.GT.3.D0)) THEN
C     Both equatorial points cannot be initialized to default here.
         IF ((XNDEQ.GT.3.D0).AND.(YNDEQ.GT.3.D0)) CALL PMDSTOP
         XINIT=XNDEQ
         YINIT=YNDEQ
      END IF
      IF ((XNDEQ.GT.3.D0).AND.(YNDEQ.GT.3.D0)) THEN
         IF ((XBGEQ.GT.3.D0).AND.(YBGEQ.GT.3.D0)) CALL PMDSTOP
         XINIT=XBGEQ
         YINIT=YBGEQ
      END IF
C     If we arrive here one equatorial point may have default value.
C     If true, XINIT, YINIT have been assigned
      IF (XINIT.NE.ANG) GO TO 28
C     If we arrive here, neither equatorial point has default value.
C      
C     The initial point must be the more remote one from the companion
C
C     The conditions on IRD, from SHAD1, are:
C         If IRD=-1, sort YR,YF entries into an increasing sequence
C         If IRD= 1, sort YR,YF entries into a decreasing sequence
C
      IF (ISEQ.EQ.1) THEN
         IF (I.EQ.IEC) THEN
C     IRD=-1         
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XBGEQ
               YINIT=YBGEQ
            ELSE
               XINIT=XNDEQ
               YINIT=YNDEQ
            END IF
         ELSE
C     IRD=-1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XNDEQ
               YINIT=YNDEQ
            ELSE
               XINIT=XBGEQ
               YINIT=YBGEQ
            END IF
         END IF
      END IF
      IF (ISEQ.EQ.2) THEN
         IF (I.EQ.IEC) THEN
C     IRD=1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XNDEQ
               YINIT=YNDEQ
            ELSE
               XINIT=XBGEQ
               YINIT=YBGEQ
            END IF
         ELSE   
C     IRD=-1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XBGEQ
               YINIT=YBGEQ
            ELSE
               XINIT=XNDEQ
               YINIT=YNDEQ
            END IF
         END IF
      END IF
      IF (ISEQ.EQ.3) THEN
         IF (I.EQ.IEC) THEN
C     IRD=-1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XBGEQ
               YINIT=YBGEQ
            ELSE
               XINIT=XNDEQ
               YINIT=YNDEQ
            END IF
         ELSE
C     IRD=1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XNDEQ
               YINIT=YNDEQ
            ELSE
               XINIT=XBGEQ
               YINIT=YBGEQ
            END IF
         END IF
      END IF
      IF (ISEQ.EQ.4) THEN
         IF (I.EQ.IEC) THEN
C     IRD=1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XNDEQ
               YINIT=YNDEQ
            ELSE
               XINIT=XBGEQ
               YINIT=YBGEQ
            END IF
         ELSE
C     IRD=-1
            IF (YBGEQ.LT.YNDEQ) THEN
               XINIT=XBGEQ
               YINIT=YBGEQ
            ELSE
               XINIT=XNDEQ
               YINIT=YNDEQ
            END IF
         END IF
      END IF                                                                                                                                               
   28 CONTINUE
C     Now have the equatorial point on star I that is most remote
C     from the companion set in XINIT and YINIT 
C
C     Locate terminal points for two branches of the horizon.
C     The CCW branch for the eclipsing star will go in arrays XR,YR.
C     The CW branch for the eclipsing star will go in arrays XF,YF.
C     The common terminal point of both branches is the
C     point most remote from the companion or origin.
C     Now fill the XR,YR arrays
C     Place in order of distance from companion or origin, with 
C     most remote point first
C
C     Set most remote point from companion common to both branches
      IRC=1
      IFC=1
      XF(1)=XINIT
      YF(1)=YINIT
      XR(1)=XINIT
      YR(1)=YINIT
C	WRITE (30,2000) XINIT,YINIT
C      WRITE (30,2000) XF(1),YF(1)
C     Copy remaining points into proper branches
C     Determine position angles for two end points of array
C     First position angle is common to both branches
C     Last position angle divides two branches
C     Even if there is a neck, the last position angle will correspond
C     to the terminal point of one branch or the other. If this point
C     is the start of the neck, the position angles of the points on
C     the other branch will occur before reaching the limiting position
C     angle.
      IF (YF(1).NE.0.D0) THEN
         RFAG1=DATAN(XF(1)/DABS(YF(1)))
C	 IF ((YF(1).LT.0.D0).AND.(DABS(XF(1)).LT.1.D-13)) RFAG1=-PI
C	 IF ((YF(1).GT.0.D0).AND.(DABS(XF(1)).LT.1.D-13)) RFAG1=PI
      ELSE
         IF (XF(1).LT.0.D0) RFAG1=-PIO2
         IF (XF(1).GE.0.D0) RFAG1=PIO2
      END IF
      ALP(1,2)=RFAG1
      ALP(1,1)=RFAG1
      RS(1,2)=DSQRT(XR(1)*XR(1)+YR(1)*YR(1))
      RS(1,1)=RS(1,2)
      RSV1=RS(1,2)
      AGLSV1=ALP(1,2)
      RSV2=0.D0
      AGLSV2=0.D0
      DLR1=0.D0
      DLR2=0.D0
      DLAGL1=0.D0
      DLAGL2=0.D0
C      WRITE (30,1000) 333,XF(1),YF(1)
C      WRITE (30,1000) 333,RFAG1
C      WRITE (30,1000) 333,ALP(1,2),RS(1,2)
C      WRITE (30,1000) 1,ALP(1,1),RS(1,1)
C	WRITE (30,1001)
C     Test for most positive X cood. of neck points.
C     Set LNKRF1 as its index value. Use in test for XR,YR branch.
C     Set LNKRF2 as index for opposite end of neck. Use in test for
C     XF,YF branch.
      LNKRF1=0
      IF (NKP.GT.1) THEN
         IF (XNK(1).GT.XNK(NKP)) THEN
            LNKRF1=1
         ELSE
            LNKRF1=NKP
         END IF
      END IF
      LNKRF2=NKP-LNKRF1+1               
C     Now copy points from the starting point to the upper pole, in
C     XR,YR
      IF ((DABS(XINIT-XBGEQ).LT.1.D-10).AND.
     $ (DABS(YINIT-YBGEQ).LT.1.D-10)) THEN
         KND=0
C     Find the index value for the equatorial point
C	WRITE (30,1000) 0,XBGEQ,YBGEQ
         DO K=1,NPB
	  AAA=XHZB(K)
	  BBB=YHZB(K)
	  CCC=AAA-XBGEQ
	  DDD=BBB-YBGEQ
C         WRITE (30,1000) K,CCC,DDD
C         WRITE (30,1000) K,BBB,DDD
         IF ((DABS(DABS(XHZB(K))-DABS(XBGEQ)).LT.1.D-10).AND.
     $   (DABS(DABS(YHZB(K))-DABS(YBGEQ)).LT.1.D-10)) KND=K
C	 IF (DABS(CCC).LT.1.D-10) THEN
C		IF (DABS(DDD).LT.1.D-10) THEN
C			KND=K
C			WRITE (30,1111) KND,K
C			IF (KND.NE.0) GO TO 51
C		END IF
C	 END IF
         END DO
C	 WRITE (30,1111) KND
C     If we didn't find the point, call error
C	 IF (KND.GT.0) GO TO 51
         IF (KND.EQ.0) CALL PMDSTOP
C     The equatorial point already has been entered in XR,YR
C     Start with next point and go to upper pole, in reverse order.
   51   DO 22 K=1,KND
C	  WRITE (30,1000) K,XR(K),YR(K)
C     Test whether point already is on list. Skip if yes.         
            DO KK=1,IRC
            AA=DABS(XR(KK)-XHZB(KND-K+1))
            BB=DABS(YR(KK)-YHZB(KND-K+1))
            IF ((AA.LT.1.D-10).AND.(BB.LT.1.D-10)) THEN
	    GO TO 20
	    END IF
            END DO 
C     Test for permissible entry on list.            
            IF ((DABS(XHZB(KND-K+1)-ANG).LE.ERLT).OR.
     $		(DABS(YHZB(KND-K+1)-ANG).LE.ERLT)) GO TO 22
C     Initializing sequence to produce second differences for subsequent
C     predictions of radii and position angles.                      
               IRC=IRC+1
               XR(IRC)=XHZB(KND-K+1)
               YR(IRC)=YHZB(KND-K+1)
               IF ((DABS(XR(IRC)-ANG).LE.ERLT).OR.
     $			(DABS(YR(IRC)-ANG).LE.ERLT)) CALL PMDSTOP
               CALL RCTPOL(IRD,XR(IRC),YR(IRC),RS(IRC,2),ALP(IRC,2),1)
		IF (DABS(ALP(IRC,2)).LT.1.D-13) ALP(IRC,2)=0.D0
C	WRITE (30,1000) K,XHZB(KND-K+1),YHZB(KND-K+1)
C	WRITE (30,1000) IRC,ALP(IRC,2),RS(IRC,2)
C	WRITE (30,1001)
   20 CONTINUE
C      WRITE (30,1000) IRC,ALP(IRC,2),RS(IRC,2)
   22    CONTINUE
      DO K=1,KND
C        WRITE (30,1000) K,XR(K),YR(K)
      END DO
C      WRITE (30 1001)
      DO K=1,KND
C        WRITE (30,1000) K,ALP(K,2),RS(K,2)
      END DO
C	CLOSE (30,STATUS='KEEP')
C     Now have copied points from the starting point to the upper pole
C     Continue by copying the remaining points from the upper pole to
C     the equator or start of neck into XR,YR
C     We started at the equatorial point most remote from companion,
C     so this branch contains the neck, if there is one.
         KND=0
         IF (NKP.LE.1) THEN
            DO K=1,NPE
               IF ((DABS(XNDEQ-XHZE(K)).LT.1.D-10).AND.
     $         (DABS(YNDEQ-YHZE(K)).LT.1.D-10)) KND=K
            END DO
            IF ((NKP.EQ.1).AND.(KND.EQ.0)) THEN
               DO K=1,NPE
                  IF (XHZE(K).LT.XNDEQ) KND=K
               END DO
            END IF      
         ELSE
C     Note that we are moving from the upper pole, where the XHZE points
C     are negative, toward more positive values. We want to include the
C     last XHZE point before the
C     neck point that has the most negative XNK value. LNKRF2 designates
C     that neck point.        
            DO K=1,NPE-1
               IF (XHZE(K).GT.XNK(LNKRF2)) KND=K-1 
C     Jump on the first test success
               IF (KND.NE.0) GO TO 25               
            END DO
         END IF      
   25     CONTINUE
          IF (KND.EQ.0) CALL PMDSTOP
C     Now have found last point for XR,YR branch.
C     Copy remaining points into XR,YR
C     Make sure no point in XHZE,YHZE duplicates a
C     point in XR,YR.
C        If the first point in XR,YR has already been copied from
C        XHZE,YHZE, start with the second point.
          IF((DABS(XR(IRC)-XHZE(1)).LT.1.D-10).AND.
     $     (DABS(YR(IRC)-YHZE(1)).LT.1.D-10)) THEN
             KBG=2
          ELSE
C     Start with the first point          
             KBG=1
          END IF
          DO 53 K=KBG,KND
C     Test whether point already is on list. Skip if yes.         
            DO KK=1,IRC
               IF ((DABS(XR(KK)-XHZE(K)).LT.1.D-10).AND.
     $         (DABS(YR(KK)-YHZE(K)).LT.1.D-10)) GO TO 53
            END DO 
               IRC=IRC+1
               XR(IRC)=XHZE(K)
               YR(IRC)=YHZE(K)
               CALL RCTPOL(IRD,XR(IRC),YR(IRC),RS(IRC,2),ALP(IRC,2),1)
		IF (DABS(ALP(IRC,2)).LT.1.D-13) ALP(IRC,2)=0.D0
C	WRITE (30,1000) K,XHZE(K),YHZE(K)
C	WRITE (30,1000) IRC,XR(IRC),YR(IRC)
C	WRITE (30,1000) IRC,ALP(IRC,2),RS(IRC,2)
C	WRITE (30,1111) 494
C	WRITE (30,1001)
   53     CONTINUE
      ELSE
C     If we arrive here, XINIT.EQ.XNDEQ.(Continuation of IF statement.)
         KND=0

         DO K=1,NPE
C     Find the index of the equatorial point         
         IF ((DABS(XHZE(K)-XNDEQ).LT.1.D-10).AND.
     $   (DABS(YHZE(K)-YNDEQ).LT.1.D-10)) KND=K
         END DO
C     If we didn't find the point, call error
         IF (KND.EQ.0) CALL PMDSTOP
C     The equatorial point already has been entered in XR,YR
         DO 54 K=1,KND
C     Start with the next point and go to upper pole, in reverse order              
C     Test whether point already is on list. Skip if yes.         
            DO KK=1,IRC
               IF ((DABS(XR(KK)-XHZE(KND-K+1)).LT.1.D-10).AND.
     $         (DABS(YR(KK)-YHZE(KND-K+1)).LT.1.D-10)) GO TO 54
            END DO 
C     Test for permissible entry on list.            
               IF ((XHZE(KND-K+1).GT.ANG).OR.(YHZE(KND-K+1).GT.ANG))
     $             GO TO 54          
C     Initializing sequence to produce second differences for subsequent
C     predictions of radii and position angles.                      
               IRC=IRC+1
               XR(IRC)=XHZE(KND-K+1)
               YR(IRC)=YHZE(KND-K+1)
               IF ((XR(IRC).GT.TST).OR.(YR(IRC).GT.TST)) CALL PMDSTOP
               CALL RCTPOL(IRD,XR(IRC),YR(IRC),RS(IRC,2),ALP(IRC,2),1)
		IF (DABS(ALP(IRC,2)).LT.1.D-13) ALP(IRC,2)=0.D0
C	WRITE (30,1000) IRC,ALP(IRC,2),RS(IRC,2)
C	WRITE (30,1111) 528
C	WRITE (30,1001)
   54    CONTINUE
C     Now have copied points from the starting point to the upper pole
C     Continue by copying the remaining points from the upper pole to
C     the equator or start of neck into XR,YR
         KND=0
         IF (NKP.LE.1) THEN
            DO K=1,NPB
               IF ((DABS(XHZB(K)-(XBGEQ)).LT.1.D-10).AND.
     $         (DABS(YHZB(K)-(YBGEQ)).LT.1.D-10)) KND=K 
            END DO
         ELSE
            DO K=1,NPB-1
               IF (XHZB(K+1).GT.XNK(LNKRF2)) KND=K
C     Jump on first test success              
               IF (KND.NE.0) GO TO 29
            END DO
         END IF
   29    CONTINUE
         IF (KND.EQ.0) THEN
            IF (NKP.EQ.1) THEN
               IF ((DABS(XBGEQ-XNK(1)).LT.1.D-10).AND.
     $          (DABS(YBGEQ-YNK(1)).LT.1.D-10)) THEN
                  DO JJ=1,NPB
                     IF ((XHZB(JJ)-XBGEQ).LT.0.D0) KND=JJ
                  END DO
               ELSE
                  CALL PMDSTOP
               END IF
            ELSE
               CALL PMDSTOP
            END IF
         END IF                  
C     Now have found last points for XR,YR branch.
C     Copy remaining points into XR,YR.
C        If the first point in XR,YR has already been copied from
C        XHZB, YHZB, start with the second point.
         IF ((DABS(XR(IRC)-XHZB(1)).LT.1.D-10).AND.
     $   (DABS(YR(IRC)-YHZB(1)).LT.1.D-10)) THEN
            KBG=2
         ELSE
C        Start with the first point         
            KBG=1
         END IF
         DO 55 K=KBG,KND
C     Test whether point already is on list. Skip if yes.         
            DO KK=1,IRC
               IF ((DABS(XR(KK)-XHZB(K)).LT.1.D-10).AND.
     $         (DABS(YR(KK)-YHZB(K)).LT.1.D-10)) GO TO 55
            END DO 
               IRC=IRC+1
               XR(IRC)=XHZB(K)
               YR(IRC)=YHZB(K)
               IF ((XR(IRC).GT.TST).OR.(YR(IRC).GT.TST)) CALL PMDSTOP
               CALL RCTPOL(IRD,XR(IRC),YR(IRC),RS(IRC,2),ALP(IRC,2),1)
		IF (DABS(ALP(IRC,2)).LT.1.D-13) ALP(IRC,2)=0.D0
C	WRITE (30,1000) IRC,ALP(IRC,2),RS(IRC,2)
C	WRITE (30,1000) IRC,XR(IRC),YR(IRC)
C	WRITE (30,1111) 586
C	WRITE (30,1001)
   55    CONTINUE
      END IF
C     Make sure XNDEQ,YNDEQ are present
	KND=0
	DO K=1,IRC
	IF ((XR(K).NE.XNDEQ).AND.(YR(K).NE.YNDEQ)) KND=K
	END DO
	IF (KND.EQ.0) THEN
	IRC=IRC+1
	XR(IRC)=XNDEQ
	YR(IRC)=YNDEQ
	CALL RCTPOL(IRD,XR(IRC),YR(IRC),RS(IRC,2),ALP(IRC,2),1)
	IF (DABS(ALP(IRC,2)).LT.1.D-13) ALP(IRC,2)=0.D0
C	WRITE (30,1000) IRC,ALP(IRC,2),RS(IRC,2)
C	WRITE (30,1000) IRC,XR(IRC),YR(IRC)
C	WRITE (30 1111) 602
C	WRITE (30,1001)
	END IF
C     Test for proper sequence of ALP() values
      DO K=2,IRC-1
		DF=ALP(K+1,2)-ALP(K,2)
		IF (DABS(DF).GT.5.D0) THEN
			IF (ALP(K+1,2).LT.0.D0) THEN
				ALP(K+1,2)=ALP(K+1,2)+TWPI
			ELSE
				ALP(K+1,2)=ALP(K+1,2)-TWPI
			END IF
		END IF
		IF ((DABS(DF).GT.3.D0).AND.(DABS(DF).LE.5.D0)) THEN
			IF (K.LT.IRC-2) THEN
				DF2=ALP(K+2,2)-ALP(K+1,2)
				DF3=DF2-DF
				IF (DABS(DF3).LT.PIO2) THEN
					ALP(K+1,2)=ALP(K+1,2)+PI
				ELSE
					ALP(K+1,2)=ALP(K+1,2)-PI
				END IF
			ELSE
				DF2=ALP(K+1,2)-ALP(K-1,2)
				DF3=DF2-DF
				IF (DABS(DF3).LT.PIO2) THEN
					ALP(K+1,2)=ALP(K+1,2)+PI
				ELSE
					ALP(K+1,2)=ALP(K+1,2)-PI
				END IF
			END IF
		END IF
	END DO
C     Sort into proper ALP() sequence
      IF (ALP(2,2).LT.ALP(1,2)) IISQ=-1
	IF (ALP(2,2).GT.ALP(1,2)) IISQ=+1
      DO K=1,IRC
		DO J=1,IRC-1
			IF (IISQ.EQ.-1) THEN
				IF (ALP(J+1,2).GT.ALP(J,2)) THEN
					SV=XR(J+1)
					XR(J+1)=XR(J)
					XR(J)=SV
					SV=YR(J+1)
					YR(J+1)=YR(J)
					YR(J)=SV
					SV=RS(J+1,2)
					RS(J+1,2)=RS(J,2)
					RS(J,2)=SV
					SV=ALP(J+1,2)
					ALP(J+1,2)=ALP(J,2)
					ALP(J,2)=SV
				END IF
			END IF
			IF (IISQ.EQ.+1) THEN 
				IF (ALP(J+1,2).LT.ALP(J,2)) THEN
					SV=XR(J+1)
					XR(J+1)=XR(J)
					XR(J)=SV
					SV=YR(J+1)
					YR(J+1)=YR(J)
					YR(J)=SV
					SV=RS(J+1,2)
					RS(J+1,2)=RS(J,2)
					RS(J,2)=SV
					SV=ALP(J+1,2)
					ALP(J+1,2)=ALP(J,2)
					ALP(J,2)=SV
				END IF
			END IF
		END DO
	END DO
C      OPEN (UNIT=30,FILE='SCRATCH.DAT',STATUS='REPLACE')
      DO K=1,IRC
C        WRITE (30,1000) K,ALP(K,2),RS(K,2)
      END DO
C	WRITE (30,1001)
C      CLOSE (30,STATUS='KEEP')
C     Now go to the starting point for XF,YF
C     Copy the points from the starting point to the lower pole
C     Following sequence follows logic for XR,YR branch.
      IF ((DABS(XINIT-XBGEQ).LT.1.D-10).AND.(DABS(YINIT-YBGEQ).LT.
     $   1.D-10)) THEN
         KND=0
         DO K=1,NPB
         IF ((DABS(XHZB(K)-(XBGEQ)).LT.1.D-10).AND.
     $   (DABS(YHZB(K)-(YBGEQ)).LT.1.D-10)) KND=K
         END DO 
C     If we don't find the point, call error
         IF (KND.EQ.0) CALL PMDSTOP 
C     The equatorial point has already been entered in XF,YF         
C     Start with first point beyond equator, go in direct sense to
C     lower pole.
         DO 62 K=KND+1,NPB
            DO KK=1,IFC
               IF ((DABS(XF(KK)-XHZB(K)).LT.1.D-10).AND.
     $         (DABS(YF(KK)-YHZB(K)).LT.1.D-10)) GO TO 62
            END DO
               IF ((XHZB(K).GT.TST).OR.(YHZB(K).GT.TST)) GO TO 62 
               IFC=IFC+1
               XF(IFC)=XHZB(K)
               YF(IFC)=YHZB(K)
               IF ((XF(IFC).GT.TST).OR.(YF(IFC).GT.TST)) THEN
                  CALL PMDSTOP
               END IF   
               CALL RCTPOL(IRD,XF(IFC),YF(IFC),RS(IFC,1),ALP(IFC,1),2)
		IF (DABS(ALP(IFC,1)).LT.1.D-13) ALP(IFC,1)=0.D0
   62    CONTINUE
C     We now are at the lower pole. Continue on to equator 
C     or start of neck in reverse order.
C     Find index for final point in XF,YF arrays.
         KND=0
         IF (NKP.LE.1) THEN
            DO K=1,NPE-1
               IF ((DABS(XHZE(NPE-K)-XNDEQ).LT.1.D-10).AND.

     $         (DABS(YHZE(NPE-K)-YNDEQ).LT.1.D-10)) KND=K
            END DO
            IF ((NKP.EQ.1).AND.(KND.EQ.0)) THEN
               DO K=1,NPE
                  IF (XHZE(NPE-K+1).GT.XNDEQ) KND=K
               END DO
            END IF      
         ELSE
C     Find last point before neck. Note that we are starting at the lower
C     pole, where the XHZE points are positive, moving toward negative,
C     so we want to stop just before the most positive XNK value.
C     The value of LNKRF1 points at the most positive XNK value.         
            DO K=1,NPE-1
               IF (XHZE(NPE-K+1).LT.XNK(LNKRF1)) KND=K-1 
C     Jump on the first success
               IF (KND.NE.0) GO TO 24               
            END DO
         END IF
   24    CONTINUE
         IF (KND.EQ.0) CALL PMDSTOP
C     Have found index for final point. Copy points into XF, YF.
         IF ((DABS(XF(IFC)-XHZE(NPE-1)).LT.1.D-10).AND.
     $   (DABS(YF(IFC)-YHZE(NPE-1)).LT.1.D-10)) THEN
            KBG=2
         ELSE
            KBG=1
         END IF
         DO 63 K=KBG,KND
            DO KK=1,IFC
            AA=DABS(XF(KK)-XHZE(NPE-K+1))
            BB=DABS(YF(KK)-YHZE(NPE-K+1))
            IF ((AA.LT.1.D-10).AND.(BB.LT.1.D-10)) THEN
	    GO TO 50
	    END IF
            END DO 
               IF ((XHZE(NPE-K+1).GT.TST).OR.
     $          (YHZE(NPE-K+1).GT.TST)) GO TO 63           
               IFC=IFC+1
               XF(IFC)=XHZE(NPE-K+1)
               YF(IFC)=YHZE(NPE-K+1)
               CALL RCTPOL(IRD,XF(IFC),YF(IFC),RS(IFC,1),ALP(IFC,1),2)
		IF (DABS(ALP(IFC,1)).LT.1.D-13) ALP(IFC,1)=1.D0
   50 CONTINUE
   63    CONTINUE
      ELSE
C     If we arrive here, XINIT.EQ.XNDEQ
         KND=0
         DO K=1,NPE
C     Find the index of the equatorial point         
            IF ((DABS(XHZE(K)-XNDEQ).LT.1.D-10).AND.
     $      (DABS(YHZE(K)-YNDEQ).LT.1.D-10)) KND=K
         END DO
         IF (KND.EQ.0) CALL PMDSTOP 
         DO 64 K=KND+1,NPE
            DO KK=1,IFC
               IF ((DABS(XF(KK)-XHZE(K)).LT.1.D-10).AND.
     $         (DABS(YF(KK)-YHZE(K)).LT.1.D-10)) GO TO 64
            END DO          
               IF ((XHZE(K).GT.TST).OR.(YHZE(K).GT.TST)) GO TO 64 
               IFC=IFC+1
               XF(IFC)=XHZE(K)
               YF(IFC)=YHZE(K)
               IF ((XF(IFC).GT.TST).OR.(YF(IFC).GT.TST)) CALL PMDSTOP
               CALL RCTPOL(IRD,XF(IFC),YF(IFC),RS(IFC,1),ALP(IFC,1),2)
		IF (DABS(ALP(IFC,1)).LT.1.D-13) ALP(IFC,1)=0.D0
   64    CONTINUE
         KND=0
         IF (NKP.LE.1) THEN
            DO K=1,NPB
               IF ((DABS(XHZB(NPB-K+1)-XBGEQ).LT.1.D-10).AND.
     $         (DABS(YHZB(NPB-K+1)-YBGEQ).LT.1.D-10)) KND=K
            END DO
         ELSE
            DO K=1,NPB-1
               IF (XHZB(K+1).GT.XNK(LNKRF2)) KND=K
               IF (KND.NE.0) GO TO 23
            END DO
         END IF
   23    CONTINUE
         IF (KND.EQ.0) THEN
            IF (NKP.EQ.1) THEN
               IF ((DABS(XBGEQ-XNK(1)).LT.1.D-10).
     $          AND.(DABS(YBGEQ-YNK(1)).LT.1.D-10)) THEN
                  DO JJ=1,NPB
                     IF ((XHZB(NPB-JJ+1)-XBGEQ).GE.0.D0) KND=JJ
                  END DO
               ELSE
                  CALL PMDSTOP
               END IF
            ELSE
               CALL PMDSTOP
            END IF
         END IF                  
         IF ((DABS(XF(IFC)-XHZB(NPB)).LT.1.D-10).AND.
     $   (DABS(YF(IFC)-YHZB(NPB)).LT.1.D-10)) THEN
            KBG=2
         ELSE
            KBG=1
         END IF
         DO 76 K=KBG,KND
            DO KK=1,IFC
               IF ((DABS(XF(KK)-XHZB(NPB-K+1)).LT.1.D-10).AND.
     $         (DABS(YF(KK)-YHZB(NPB-K+1)).LT.1.D-10)) GO TO 76
            END DO          
               IF ((XHZB(NPB-K+1).GT.TST).OR.(YHZB(NPB-K+1).GT.TST))
     $            GO TO 76           
               IFC=IFC+1
               XF(IFC)=XHZB(NPB-K+1)
               YF(IFC)=YHZB(NPB-K+1)
               IF ((XF(IFC).GT.TST).OR.(YF(IFC).GT.TST)) CALL PMDSTOP
               CALL RCTPOL(IRD,XF(IFC),YF(IFC),RS(IFC,1),ALP(IFC,1),2)
		IF (DABS(ALP(IFC,1)).LT.1.D-13) ALP(IFC,1)=0.D0
   76    CONTINUE
      END IF
C     Make sure XNDEQ,YNDEQ are present
      KND=0
      DO K=1,IFC
      IF ((DABS(XF(K)-XNDEQ).LE.ERLT).AND.(DABS(YF(K)-YNDEQ).LE.ERLT)) 
     $KND=K
      END DO
	IF (KND.EQ.0) THEN
	IFC=IFC+1
	XF(IFC)=XNDEQ
	YF(IFC)=YNDEQ
	CALL RCTPOL(IRD,XF(IFC),YF(IFC),RS(IFC,1),ALP(IFC,1),2)
	IF (DABS(ALP(IFC,1)).LT.1.D-13) ALP(IFC,1)=0.D0
      END IF
      	DO K=1,IFC
C	  WRITE (30,1000) K,ALP(K,1),RS(K,1)
	END DO
C	WRITE (30,1001)
C     First check ALP(1,1)
      DF=ALP(2,1)-ALP(1,1)
	IF (DABS(DF).GT.5.D0) THEN
	  IF (ALP(2,1).LT.0.D0) THEN
		ALP(1,1)=ALP(1,1)-TWPI
	  ELSE
		ALP(1,1)=ALP(1,1)+TWPI
	  END IF
	END IF
	DO K=1,5
C	  WRITE (30,1000) K,ALP(K,1),RS(K,1)
	END DO
C	WRITE(30,1001)
C     Test for proper sequence of ALP() values
      DO K=1,IFC-1
		DF=ALP(K+1,1)-ALP(K,1)
		IF (DABS(DF).GT.5.D0) THEN
			IF (ALP(K+1,1).LT.0.D0) THEN
				ALP(K+1,1)=ALP(K+1,1)+TWPI
			ELSE
				ALP(K+1,1)=ALP(K+1,1)-TWPI
			END IF
		END IF
	END DO
      	DO K=1,IFC
c	  WRITE (30,1000) K,XF(K),YF(K)
C	  WRITE (30,1000) K,ALP(K,1),RS(K,1)
	END DO
C	WRITE (30,1001)
C     Sort into proper ALP() sequence
      IF (ALP(2,1).LT.ALP(1,1)) IISQ=-1
	IF (ALP(2,1).GT.ALP(1,1)) IISQ=+1
      DO K=1,IFC
		DO J=1,IFC-1
			IF (IISQ.EQ.-1) THEN
				IF (ALP(J+1,1).GT.ALP(J,1)+ERLT) THEN
					SV=XF(J+1)
					XF(J+1)=XF(J)
					XF(J)=SV
					SV=YF(J+1)
					YF(J+1)=YF(J)
					YF(J)=SV
					SV=RS(J+1,1)
					RS(J+1,1)=RS(J,1)
					RS(J,1)=SV
					SV=ALP(J+1,1)
					ALP(J+1,1)=ALP(J,1)
					ALP(J,1)=SV
				END IF
			END IF
			IF (IISQ.EQ.+1) THEN 
				IF (ALP(J+1,1).LT.ALP(J,1)-ERLT) THEN
					SV=XF(J+1)
					XF(J+1)=XF(J)
					XF(J)=SV
					SV=YF(J+1)
					YF(J+1)=YF(J)
					YF(J)=SV
					SV=RS(J+1,1)
					RS(J+1,1)=RS(J,1)
					RS(J,1)=SV
					SV=ALP(J+1,1)
					ALP(J+1,1)=ALP(J,1)
					ALP(J,1)=SV
				END IF
			END IF
		END DO
	END DO
C	WRITE (30,1001)
	DO K=1,IFC
C	   WRITE (30,1000) K,ALP(K,1),RS(K,1)
	END DO
C	WRITE (30,1001)
      RFAG3=(ALP(IRC,2)+ALP(IFC,1)-TWPI)/2.D0
      IF (NKP.GT.1) GO TO 56
      IF (NKP.EQ.1) GO TO 27
C     Set nearest point to companion common to both branches
C     This is for the case with no neck point
      IF (DABS(ALP(IRC,2)).GE.DABS(ALP(IFC,1))) GO TO 112
      IF (DABS(ALP(IRC,2)).LT.DABS(ALP(IFC,1))) GO TO 113
      CALL PMDSTOP
  112	DIFF1=DABS(DABS(XF(IFC))-DABS(XR(IRC)))
      DIFF2=DABS(DABS(YF(IFC))-DABS(YR(IRC)))
	IF ((DIFF1.GT.1.D-11).AND.(DIFF2.GT.1.D-11)) GO TO 102
      GO TO 49
  113	DIFF1=DABS(DABS(XR(IRC))-DABS(XF(IFC)))
      DIFF2=DABS(DABS(YR(IRC))-DABS(YF(IFC)))
	IF ((DIFF1.GT.1.D-11).AND.(DIFF2.GT.1.D-11)) GO TO 104
      GO TO 49
  104 IRC=IRC+1
      XR(IRC)=XF(IFC)
      YR(IRC)=YF(IFC)
      ALP(IRC,2)=ALP(IFC,1)-TWPI
      RS(IRC,2)=RS(IFC,1)
      GO TO 49
  102 IFC=IFC+1
      XF(IFC)=XR(IRC)
      YF(IFC)=YR(IRC)
      ALP(IFC,1)=ALP(IRC,2)+TWPI
      RS(IFC,1)=RS(IRC,2)
      GO TO 49
C     One neck point.Set in both branches,if not already present.
   27 DO J=1,IRC
         IF ((DABS(YR(J)-YNK(1)).LT.1.0D-10).AND.(DABS(XR(J)-XNK(1)).LT.
     $   1.0D-10)) GO TO 47
      END DO
C     Point not in CCW branch. Insert with proper tests
C     Find nominal insertion location
      IRCRF=0
      IF (YR(1).LT.YR(IRC)) THEN
         DO J=1,IRC-1
            IF ((YR(J).LT.YNK(1)).AND.(YR(J+1).GT.YNK(1))) IRCRF=IRC
         END DO
      ELSE
         DO J=1,IRC-1
            IF ((YR(J).GT.YNK(1)).AND.(YR(J+1).LT.YNK(1))) IRCRF=IRC
         END DO
      END IF
      IF (IRCRF.EQ.0) THEN
         IF ((XNK(1).GT.TST).OR.(YNK(1).GT.TST)) CALL PMDSTOP
         CALL RCTPOL(IRD,XNK(1),YNK(1),RSVA,AGLSVA,1)
	 IF (DABS(AGLSVA).LT.1.D-13) AGLSVA=0.D0
         IRC=IRC+1
         XR(IRC)=XNK(1)
         YR(IRC)=YNK(1)
         RS(IRC,2)=RSVA
         ALP(IRC,2)=AGLSVA
         GO TO 47
      END IF               
      DLR2=RS(IRCRF,2)-RS(IRCRF-1,2)
      DLAGL2=ALP(IRCRF,2)-ALP(IRCRF-1,2)
      IF (IRCRF.LT.3) THEN
         DLR1=0.D0
         DLAGL1=0.D0
      ELSE   
         DLR1=RS(IRCRF-1,2)-RS(IRCRF-2,2)
         DLAGL1=ALP(IRCRF-1,2)-ALP(IRCRF-2,2)
      END IF   
      DL2RS=DLR2-DLR1
      DL2AGL=DLAGL2-DLAGL1
      DLRLMT=3.D0*DABS(DL2RS)
         IF (DLRLMT.LT.0.5D-2*RS(IRCRF,2)) DLRLMT=0.5D-2*RS(IRCRF,2)
		DLAGLMT=3.D0*DABS(DL2AGL)
         IF (DLAGLMT.LT.0.5D0*DABS(DLAGL2)) 
     $      DLAGLMT=0.5D0*DABS(DLAGL2)
      DLR2=DLR1+DL2RS
      DLAGL2=DLAGL1+DL2AGL
      PDRS=RS(IRCRF,2)+DLR2
      PDAGL=ALP(IRCRF,2)+DLAGL2
      IF ((XNK(1).GT.TST).OR.(YNK(1).GT.TST)) CALL PMDSTOP
      CALL RCTPOL(IRD,XNK(1),YNK(1),RSVA,AGLSVA,1)
	IF (DABS(AGLSVA).LT.1.D-13) AGLSVA=0.D0
      AGLV=AGLSVA
      IF ((DABS(PDRS-RSVA).GT.DLRLMT).OR.
     $   (DABS(PDAGL-AGLSVA).GT.DLAGLMT)) GO TO 47  
  115 DO J=1,IRC-1
      IF ((AGLV.LT.ALP(J,2)).AND.(AGLV.GE.ALP(J+1,2))) GO TO 116
      END DO
      JS=IRC+1
      GO TO 30
  116 JS=J
      DO J=1,IRC+1-JS
      XR(IRC+2-J)=XR(IRC+1-J)
      YR(IRC+2-J)=YR(IRC+1-J)
      ALP(IRC+2-J,2)=ALP(IRC+1-J,2)
      RS(IRC+2-J,2)=RS(IRC+1-J,2)
      END DO
   30 XR(JS)=XNK(1)
      YR(JS)=YNK(1)
      ALP(JS,2)=AGLV
      RS(JS,2)=DSQRT(XNK(1)*XNK(1)+YNK(1)*YNK(1))
      IRC=IRC+1
      IF (ALP(IRC,2).LT.RFAG3) RFAG3=ALP(IRC,2)
C     Test other branch
   47 DO J=1,IFC
      IF ((DABS(YF(J)-YNK(1)).LT.1.0D-10).AND.(DABS(XF(J)-XNK(1)).LT.
     $1.0D-10)) GO TO 49
      END DO
C     Only add neck point if closure is needed
      TS=DABS(ALP(IFC,1)-ALP(IRC,2)-TWPI)
      IF (TS.LT.1.D-6) GO TO 49
C     Locate nominal insertion location
      IFCRF=0
      IF (YF(1).LT.YF(IFC)) THEN
         DO J=1,IFC-1
            IF ((YF(J).LT.YNK(1)).AND.(YF(J+1).GT.YNK(1))) IFCRF=IFC
         END DO
      ELSE
         DO J=1,IFC-1
            IF ((YF(J).GT.YNK(1)).AND.(YF(J+1).LT.YNK(1))) IFCRF=IFC
         END DO
      END IF
      IF (IFCRF.EQ.0) THEN
         IF ((XNK(1).GT.TST).OR.(YNK(1).GT.TST)) CALL PMDSTOP
         CALL RCTPOL(IRD,XNK(1),YNK(1),RSVA,AGLSVA,2)
	  IF (DABS(AGLSVA).LT.1.D-13) AGLSVA=0.D0
         IFC=IFC+1
         XF(IFC)=XNK(1)
         YF(IFC)=YNK(1)
         RS(IFC,1)=RSVA
         ALP(IFC,1)=AGLSVA
         GO TO 49
      END IF               
      DLR2=RS(IFCRF,1)-RS(IFCRF-1,1)
      DLAGL2=ALP(IFCRF,1)-ALP(IFCRF-1,1)
      IF (IFCRF.LT.3) THEN
         DLR1=0.D0
         DLAGL1=0.D0
      ELSE   
         DLR1=RS(IFCRF-1,1)-RS(IFCRF-2,1)
         DLAGL1=ALP(IFCRF-1,1)-ALP(IFCRF-2,1)
      END IF   
      DL2RS=DLR2-DLR1
      DL2AGL=DLAGL2-DLAGL1
      DLRLMT=3.D0*DABS(DL2RS)
         IF (DLRLMT.LT.0.5D-2*RS(IFCRF,1)) DLRLMT=0.5D-2*RS(IFCRF,1)
      DLAGLMT=3.D0*DABS(DL2AGL)
         IF (DLAGLMT.LT.0.5D0*DABS(DLAGL2)) 
     $      DLAGLMT=0.5D0*DABS(DLAGL2)
      DLR2=DLR1+DL2RS
      DLAGL2=DLAGL1+DL2AGL
      PDRS=RS(IFCRF,1)+DLR2
      PDAGL=ALP(IFCRF,1)+DLAGL2
      IF ((XNK(1).GT.TST).OR.(YNK(1).GT.TST)) CALL PMDSTOP
      CALL RCTPOL(IRD,XNK(1),YNK(1),RSVA,AGLSVA,2)
	IF (DABS(AGLSVA).LT.1.D-13) AGLSVA=0.D0
      AGLV=AGLSVA
      IF ((DABS(PDRS-RSVA).GT.DLRLMT).OR.
     $   (DABS(PDAGL-AGLSVA).GT.DLAGLMT)) GO TO 49       
  118 DO J=1,IFC-1
      IF ((AGLV.GT.ALP(J,1)).AND.(AGLV.LE.ALP(J+1,1))) GO TO 119
      END DO
      JS=IFC+1
      GO TO 32
  119 JS=J
      DO J=1,IFC+1-JS
      XF(IFC+2-J)=XF(IFC+1-J)
      YF(IFC+2-J)=YF(IFC+1-J)
      ALP(IFC+2-J,1)=ALP(IFC+1-J,1)
      RS(IFC+2-J,1)=RS(IFC+1-J,1)
      END DO
   32 XF(JS)=XNK(1)
      YF(JS)=YNK(1)
      ALP(JS,1)=AGLV
      RS(JS,1)=DSQRT(XNK(1)*XNK(1)+YNK(1)*YNK(1))
      IFC=IFC+1
      GO TO 49
C     Case in which NKP.GT.1
C     If there is a neck,make sure start and end of neck is common
C     with the two branches
C     Start of neck
   56 CONTINUE
C     Calculate position angles of neck points and sort in order of
C     neck points
      DO J=1,NKP
      AGLV=DATAN(XNK(J)/DABS(YNK(J)))
      IF ((YNK(J).GT.0.0D0).AND.(IRD.EQ.(-1))) GO TO 120
      IF ((YNK(J).LT.0.0D0).AND.(IRD.EQ.1)) GO TO 120
      IF ((YNK(J).LT.0.0D0).AND.(IRD.EQ.(-1))) GO TO 121
      IF ((YNK(J).GT.0.0D0).AND.(IRD.EQ.1)) GO TO 121
      CALL PMDSTOP
  120 AGLV=-PI-AGLV
      IF (AGLV.LE.RFAG3) AGLV=AGLV+TWPI
  121 ALP(J,3)=AGLV
      RS(J,3)=DSQRT(XNK(J)*XNK(J)+YNK(J)*YNK(J))
      END DO
C     DETERMINE CORRECT ORDER OF SORT
      JJ=0
      KK=0
      DO J=1,NKP
      IF (ALP(J,3).LT.0.0D0) JJ=JJ+1
      IF (ALP(J,3).GE.0.0D0) KK=KK+1
      END DO
C     NOW SORT INTO CORRECT ORDER
      DO 123 K=1,NKP
      DO 122 J=1, NKP-1
      IF (JJ.GE.KK) GO TO 125
C     SORT INTO INCREASING SEQUENCE
      IF (ALP(J,3).LT.ALP(J+1,3)) GO TO 122
      GO TO 126
C     SORT INTO DECREASING SEQUENCE
  125 IF (ALP(J,3).GE.ALP(J+1,3)) GO TO 122
  126 SV=YNK(J+1)
      YNK(J+1)=YNK(J)
      YNK(J)=SV
      SV=XNK(J+1)
      XNK(J+1)=XNK(J)
      XNK(J)=SV
      SV=ALP(J+1,3)
      ALP(J+1,3)=ALP(J,3)
      ALP(J,3)=SV
      SV=RS(J+1,3)
      RS(J+1,3)=RS(J,3)
      RS(J,3)=SV
  122 CONTINUE
  123 CONTINUE
C     CHECK THAT THERE IS NO MODULO TWPI PROBLEM
      IF (DABS(ALP(1,3)-ALP(NKP,3)).LT.PIO2) GO TO 124
C     MAKE ENTRIES ALL POSITIVE OR ALL NEGATIVE
      IF (JJ.GE.KK) GO TO 12
      DO J=1,NKP
      IF (ALP(J,3).LT.0.0D0) ALP(J,3)=ALP(J,3)+TWPI
      END DO
      GO TO 13
   12 DO J=1,NKP
      IF (ALP(J,3).GE.0.0D0) ALP(J,3)=ALP(J,3)-TWPI
      END DO
C     NOW SORT AGAIN
   13 DO 15 K=1,NKP
      DO 14 J=1,NKP-1
      IF (JJ.GE.KK) GO TO 16
C     SORT INTO INCREASING SEQUENCE
      IF (ALP(J,3).LT.ALP(J+1,3)) GO TO 14
      GO TO 17
C     SORT INTO DECREASING SEQUENCE
   16 IF (ALP(J,3).GE.ALP(J+1,3)) GO TO 14
   17 SV=YNK(J+1)
      YNK(J+1)=YNK(J)
      YNK(J)=SV
      SV=XNK(J+1)
      XNK(J+1)=XNK(J)
      XNK(J)=SV
      SV=ALP(J+1,3)
      ALP(J+1,3)=ALP(J,3)
      ALP(J,3)=SV
      SV=RS(J+1,3)
      RS(J+1,3)=RS(J,3)
      RS(J,3)=SV
   14 CONTINUE
   15 CONTINUE
  124 CONTINUE
C     ALP(J,3) NOW ALL POSITIVE OR ALL NEGATIVE
C     Add terminal points of neck points to branches, if not already
C     present
C     ALP(1,3) must be negative to belong to the IRC branch
C     Do not add neck points to eclipsed component unless NKP.GT.2
      IF ((ICOUNT.EQ.2).AND.(NKP.LE.2)) GO TO 49
      IF ((DABS(ALP(IRC,2)-ALP(1,3)).LT.1.0D-9).AND.(DABS(RS(IRC,2)-
     $RS(1,3)).LT.1.0D-9)) GO TO 31
      IF ((DABS(ALP(IRC-1,2)-ALP(1,3)).LT.1.0D-9).AND.(DABS(RS(IRC-1,
     $2)-RS(1,3)).LT.1.0D-9)) GO TO 31
      IF (ALP(1,3).GE.0.0D0) GO TO 9
C     No coincidence with end point. Add end point to CCW branch.
      IRC=IRC+1
      ALP(IRC,2)=ALP(1,3)
      RS(IRC,2)=RS(1,3)
      XR(IRC)=XNK(1)
      YR(IRC)=YNK(1)
      GO TO 31
C     No coincidence at start of neck. Check end of neck.
    9 TSAG=ALP(NKP,3)-TWPI
      IF ((DABS(ALP(IRC,2)-TSAG).LT.1.0D-9).AND.(DABS(RS(IRC,2)-
     $RS(NKP,3)).LT.1.0D-9)) GO TO 31
      IF ((DABS(ALP(IRC-1,2)-TSAG).LT.1.0D-9).AND.(DABS(RS(IRC-1,
     $2)-RS(NKP,3)).LT.1.0D-9)) GO TO 31
      IF (ALP(NKP,3).LT.0.0D0) GO TO 31
      IRC=IRC+1
      ALP(IRC,2)=TSAG
      RS(IRC,2)=RS(NKP,3)
      XR(IRC)=XNK(NKP)
      YR(IRC)=YNK(NKP)
C     Check other end point            
   31 IF ((DABS(ALP(IFC,1)-ALP(1,3)).LT.1.0D-9).AND.(DABS(
     $RS(IFC,1)-RS(1,3)).LT.1.0D-9)) GO TO 49
      IF ((DABS(ALP(IFC-1,1)-ALP(1,3)).LT.1.0D-9).AND.(DABS(
     $RS(IFC-1,1)-RS(1,3)).LT.1.0D-9)) GO TO 49
      IF (ALP(1,3).LT.0.0D0) GO TO 10
      IFC=IFC+1
      ALP(IFC,1)=ALP(1,3)
      RS(IFC,1)=RS(1,3)
      XF(IFC)=XNK(1)
      YF(IFC)=YNK(1)
      GO TO 49
C     Check end of neck
   10 TSAG=ALP(NKP,3)+TWPI
      IF ((DABS(ALP(IFC,1)-TSAG).LT.1.0D-9).AND.(DABS(RS(IFC,1)-
     $RS(NKP,3)).LT.1.0D-9)) GO TO 49
      IF ((DABS(ALP(IFC-1,1)-TSAG).LT.1.0D-9).AND.(DABS(RS(IFC-1,1)
     $-RS(NKP,3)).LT.1.0D-9)) GO TO 49
      IF (ALP(NKP,3).GE.0.0D0) GO TO 49
      IFC=IFC+1
      ALP(IFC,1)=TSAG
      RS(IFC,1)=RS(NKP,3)
      XF(IFC)=XNK(NKP)
      YF(IFC)=YNK(NKP)

C     Now have one neck point on each XF,YF branch
   49 CONTINUE
C      OPEN (UNIT=30,FILE='SCRATCH.DAT',STATUS='REPLACE')
      DO K=1,IFC
C        WRITE (30,1000) K,ALP(K,1),RS(K,1)
      END DO
C      CLOSE (30,STATUS='KEEP')
C     Remove any degeneracy in ALP(J,1) entries
      JJ=0
      DO J=1,IFC-1
		IF (DABS(ALP(J,1)-ALP(J+1,1)).GT.1.0D-10) THEN
			JJ=JJ+1
			ALPP(JJ,1)=ALP(J,1)
			RSP(JJ,1)=RS(J,1)
		ELSE
			IF (J.GT.2) THEN
				JJ=JJ+1
				RSP(JJ,1)=(RS(J-1,1)+RS(J+1,1))/2.0
				POLY1(1)=RS(J-2,1)
				POLY1(2)=RS(J-1,1)
				POLY1(3)=RS(J+1,1)
				IF (DABS(POLY1(2)-POLY1(1)).LT.1.D-10) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
				GO TO 34 
				END IF
				IF (DABS(POLY1(3)-POLY1(2)).LT.1.D-10) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
				GO TO 34 
				END IF
				POLY2(1)=ALP(J-2,1)
				POLY2(2)=ALP(J-1,1)
				POLY2(3)=ALP(J+1,1)
				DO KK=1,3
					IF (DABS(RS(J,1)-POLY1(KK)).LT.1.D-10) GO TO 33
				END DO
				IF ((RS(J-1,1).LE.RS(J,1)).AND.(RS(J,1).LE.RS(J+1,1)))
     $				THEN
				CALL LAGIN(RS(J,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 34
				END IF
				GO TO 42
				END IF
				IF ((RS(J-1,1).GE.RS(J,1)).AND.(RS(J,1).GE.RS(J+1,1))) 
     $				THEN
				CALL LAGIN(RS(J,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 34
				END IF
				GO TO 42
				END IF
				CALL LAGIN(RSP(JJ,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 34
				END IF
   42				IF (DABS(ALP(J+1,1)).GT.DABS(ALP(J-1,1))) THEN
					IF(DABS(VO).GT.DABS(ALP(J-1,1)).AND.DABS(VO).LT.
     $					DABS(ALP(J+1,1))) THEN

						ALPP(JJ,1)=VO
					ELSE
						ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					END IF
				ELSE
					IF (DABS(VO).LT.DABS(ALP(J-1,1)).AND.(DABS(VO).GT.
     $					DABS(ALP(J+1,1)))) THEN
						ALPP(JJ,1)=VO
					ELSE
						ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					END IF
				END IF
				GO TO 34
   33				IF (KK.NE.3) CALL PMDSTOP
				CALL LAGIN(RSP(JJ,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 34
				END IF
				GO TO 42
   34				CONTINUE
     			ELSE
				JJ=JJ+1
				RSP(JJ,1)=(RS(J-1,1)+RS(J+1,1))/2.D0

				POLY1(1)=RS(J-1,1)
				POLY1(2)=RS(J+1,1)
				POLY1(3)=RS(J+2,1)
				IF (DABS(POLY1(2)-POLY1(1)).LT.1.D-10) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
				GO TO 36 
				END IF
				IF (DABS(POLY1(3)-POLY1(2)).LT.1.D-10) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
				GO TO 36 
				END IF
				POLY2(1)=ALP(J-1,1)
				POLY2(2)=ALP(J+1,1)
				POLY2(3)=ALP(J+2,1)
				DO KK=1,3
					IF (DABS(RS(J,1)-POLY1(KK)).LT.1.D-10) GO TO 35
				END DO
				IF ((RS(J-1,1).LE.RS(J,1)).AND.(RS(J,1).LE.RS(J-1,1)))
     $				THEN
				CALL LAGIN(RS(J,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 36
				END IF
				GO TO 43
				END IF
				IF ((RS(J-1,1).GE.RS(J,1)).AND.(RS(J,1).GE.RS(J-1,1))) 
     $				THEN
				CALL LAGIN(RS(J,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 36
				END IF
				GO TO 43
				END IF
				CALL LAGIN(RSP(JJ,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 36
				END IF
   43				IF (DABS(ALP(J+1,1)).GT.DABS(ALP(J-1,1))) THEN
					IF(DABS(VO).GT.DABS(ALP(J-1,1)).AND.DABS(VO).LT.
     $					DABS(ALP(J+1,1))) THEN
						ALPP(JJ,1)=VO
					ELSE
						ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					END IF
				ELSE
					IF (DABS(VO).LT.DABS(ALP(J-1,1)).AND.(DABS(VO).GT.
     $					DABS(ALP(J+1,1)))) THEN
						ALPP(JJ,1)=VO
					ELSE
						ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					END IF
				END IF
				GO TO 36
   35				IF (KK.NE.3) CALL PMDSTOP
				CALL LAGIN(RSP(JJ,1),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JJ,1)=(ALP(J-1,1)+ALP(J+1,1))/2.D0
					GO TO 36
				END IF
				GO TO 43
   36				CONTINUE
			END IF
		END IF
	END DO
C     Insert last point
	JJ=JJ+1
	ALPP(JJ,1)=ALP(IFC,1)
	RSP(JJ,1)=RS(IFC,1)
C     Reset IFC and copy correct arrays back to original arrays
      IFC=JJ
	DO J=1,JJ
		ALP(J,1)=ALPP(J,1)
		RS(J,1)=RSP(J,1)
	END DO
C	Remove any degeneracy in ALP(K,2) entries.
      JK=0
      DO J=1,IRC-1
		IF (DABS(ALP(J,2)-ALP(J+1,2)).GT.1.0D-10) THEN
			JK=JK+1
			ALPP(JK,2)=ALP(J,2)
			RSP(JK,2)=RS(J,2)
		ELSE
		IF (J.GT.2) THEN
			JK=JK+1
			RSP(JK,2)=(RS(J-1,2)+RS(J+1,2))/2.D0
			POLY1(1)=RS(J-2,2)
			POLY1(2)=RS(J-1,2)
			POLY1(3)=RS(J+1,2)
			IF (DABS(POLY1(2)-POLY1(1)).LT.1.D-10) THEN
				ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
				GO TO 38
			END IF
			IF (DABS(POLY1(3)-POLY1(2)).LT.1.D-10) THEN
				ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
				GO TO 38
			END IF
			POLY2(1)=ALP(J-2,2)
			POLY2(2)=ALP(J-1,2)
			POLY2(3)=ALP(J+1,2)
			DO KK=1,3
				IF (DABS(RS(J,2)-POLY1(KK)).LT.1.D-10) GO TO 37
			END DO
			IF ((RS(J-1,2).LE.RS(J,2)).AND.(RS(J,2).LE.RS(J-1,2)))THEN
				CALL LAGIN(RS(J,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 38 
				END IF
				GO TO 44
			END IF
			IF ((RS(J-1,2).GE.RS(J,2)).AND.(RS(J,2).GE.RS(J-1,2)))THEN
				CALL LAGIN(RS(J,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 38 
				END IF
				GO TO 44
			END IF
			CALL LAGIN(RSP(JK,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 38 
				END IF
   44				IF (DABS(ALP(J+1,2)).GT.DABS(ALP(J-1,2))) THEN
					IF(DABS(VO).GT.DABS(ALP(J-1,2)).AND.DABS(VO).LT.
     $					DABS(ALP(J+1,2))) THEN
						ALPP(JK,2)=VO
					ELSE
						ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					END IF
				ELSE
					IF (DABS(VO).LT.DABS(ALP(J-1,2)).AND.(DABS(VO).GT.
     $					DABS(ALP(J+1,2)))) THEN
						ALPP(JK,2)=VO
					ELSE
						ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					END IF
				END IF
			GO TO 38
   37			CALL LAGIN(RSP(JK,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 38 
				END IF
			GO TO 44
   38			CONTINUE
		ELSE
			JK=JK+1
			RSP(JK,2)=(RS(J-1,2)+RS(J+1,2))/2.D0
			POLY1(1)=RS(J-1,2)
			POLY1(2)=RS(J+1,2)
			POLY1(3)=RS(J+2,2)
			IF (DABS(POLY1(2)-POLY1(1)).LT.1.D-10) THEN
				ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
				GO TO 41
			END IF
			IF (DABS(POLY1(3)-POLY1(2)).LT.1.D-10) THEN
				ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
				GO TO 41
			END IF
			POLY2(1)=ALP(J-1,2)
			POLY2(2)=ALP(J+1,2)
			POLY2(3)=ALP(J+2,2)
			DO KK=1,3
				IF (DABS(RS(J,2)-POLY1(KK)).LT.1.D-10) GO TO 39
			END DO
			IF ((RS(J-1,2).LE.RS(J,2)).AND.(RS(J,2).LE.RS(J-1,2)))THEN
				CALL LAGIN(RS(J,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 41 
				END IF
				GO TO 45
			END IF
			IF ((RS(J-1,2).GE.RS(J,2)).AND.(RS(J,2).GE.RS(J-1,2)))THEN
				CALL LAGIN(RS(J,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 41 
				END IF
				GO TO 45
			END IF
			CALL LAGIN(RSP(JK,2),VO,POLY1,POLY2,2,3,KR,1.D0)
				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 41 
				END IF
   45				IF (DABS(ALP(J+1,2)).GT.DABS(ALP(J-1,2))) THEN
					IF(DABS(VO).GT.DABS(ALP(J-1,2)).AND.DABS(VO).LT.
     $					DABS(ALP(J+1,2))) THEN
						ALPP(JK,2)=VO
					ELSE
						ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					END IF
				ELSE
					IF (DABS(VO).LT.DABS(ALP(J-1,2)).AND.(DABS(VO).GT.
     $					DABS(ALP(J+1,2)))) THEN
						ALPP(JK,2)=VO
					ELSE
						ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					END IF
				END IF
			GO TO 41
   39			IF (KK.NE.3) CALL PMDSTOP

			CALL LAGIN(RSP(JK,2),VO,POLY1,POLY2,2,3,KR,1.D0)

				IF (KR.LT.3) THEN
					ALPP(JK,2)=(ALP(J-1,2)+ALP(J+1,2))/2.D0
					GO TO 41 
				END IF
			GO TO 45
   41			CONTINUE
			END IF
		END IF
	END DO
C     Insert last point
	JK=JK+1
	ALPP(JK,2)=ALP(IRC,2)
	RSP(JK,2)=RS(IRC,2)
C     Reset IRC and copy correct arrays back to original arrays
      IRC=JK
	DO J=1,JK
		ALP(J,2)=ALPP(J,2)
		RS(J,2)=RSP(J,2)
	END DO
C     We now have filled the arrays of points for the two branches
C     of the horizon
C  

   26 K=2

C
 1000 FORMAT (I5,2F15.8)
 1001 FORMAT (' ')  
 	IF (ICOUNT.EQ.1) THEN
	DO K=1,IRC
		WRITE (33,1000) K,ALP(K,2),RS(K,2)
	END DO
	DO K=1,IFC
		WRITE (33,1000) K,ALP(K,1),RS(K,1)
	END DO
	END IF
        IF (ICOUNT.EQ.2) THEN
	DO JJ=1,IRC
c	WRITE (NOUT7,1000) JJ,ALP(JJ,2),RS(JJ,2)
	END DO
c	WRITE (NOUT7,1001)
	DO JJ=1,IFC
c	WRITE (NOUT7,1000) JJ,ALP(JJ,1),RS(JJ,1)
	END DO
	END IF
c      CLOSE (30,STATUS='KEEP')
C     PASS PARABOLAS ALONG THE SMALL THETA BRANCH   
C  
C
   67 CONTINUE
C	Set for small theta reference
      K=2
      KLM=IRC-1
      IF (KLM.LE.2) CALL PMDSTOP
      DO 405 JL=1,KLM
      JST=JL
      JND=JST+2
      JSB=0
  106 JST=JST+JSB
      JND=JST+2
      IF ((JST.GE.1).AND.(JND.LE.IRC)) GO TO 107
      IF (JND.GT.IRC) JSB=-1
      IF (JST.LT.1) JSB=1
      GO TO 106
  107 DO LL=JST,JND
      AR2(LL-JST+1)=RS(LL,K)
      AR3P(LL-JST+1)=ALP(LL,K)
      END DO
      CALL PARAB(AR3P,AR2)
      DO 401 J=1,3
      ARYB(JL,J)=AR2(J)    
  401 CONTINUE   
  405 CONTINUE
C     PASS PARABOLAS ALONG THE LARGE THETA BRANCH   
C  
  404 K=1 
   68 KLM=IFC-1
      IF (KLM.LE.2) CALL PMDSTOP
      DO 406 JL=1,KLM
      JST=JL
      JND=JST+2
      JSB=0							  
  108 JST=JST+JSB
      JND=JST+2
      IF ((JST.GE.1).AND.(JND.LE.IFC)) GO TO 109
      IF (JND.GT.IFC) JSB=-1
      IF (JST.LT.1) JSB=1
      GO TO 108
  109 DO LL=JST,JND
      AR2(LL-JST+1)=RS(LL,K)
      AR3P(LL-JST+1)=ALP(LL,K)
      END DO
      CALL PARAB(AR3P,AR2)
      DO 402 J=1,3
      ARYA(JL,J)=AR2(J)    
  402 CONTINUE   
  406 CONTINUE
C
C     Fit neck boundary
      IF (NKP.LE.1) GO TO 85
   71 K=3
      KLM=NKP-1
      IF (NKP.GE.3) GO TO 407
      DO LL=1,2
      AR2(LL)=RS(LL,K)
      AR3P(LL)=ALP(LL,K)
      END DO
      CALL STLINE(AR3P,AR2,0,IER)
      IF (IER.EQ.129) CALL PMDSTOP
      ARYC(1,1)=AR2(1)
      ARYC(1,2)=AR2(2)
      ARYC(1,3)=0.0D0
      GO TO 408
  407 DO 410 JL=1,KLM
      JST=JL
      JND=JST+2
      JSB=0
      KCT=0
  411 JST=JST+JSB
      JND=JST+2
      KCT=KCT+1
      IF ((JST.GE.1).AND.(JND.LE.NKP)) GO TO 412
      IF (JND.GT.NKP) JSB=-1
      IF (JND.LT.1) JSB=1
      IF (KCT.LE.4) GO TO 411
      CALL PMDSTOP
  412 DO LL=JST,JND
      AR2(LL-JST+1)=RS(LL,K)
      AR3P(LL-JST+1)=ALP(LL,K)
      END DO
      CALL PARAB(AR3P,AR2)
      DO 409 J=1,3
      ARYC(JL,J)=AR2(J)
  409 CONTINUE
C     CHECK FOR EXTREME COEFFICIENTS
      IF ((ABS(ARYC(JL,3)).LT.2.5D0).AND.(ABS(ARYC(JL,3)).LT.20.0D0))
     $GO TO 410
      DO LL=JST,JND
      AR2(LL-JST+1)=RS(LL,K)
      AR3P(LL-JST+1)=ALP(LL,K)
      END DO
      KY=0
      IF (JL.EQ.KLM) KY=1
      CALL STLINE(AR3P,AR2,KY,IER)
      IF (IER.EQ.129) CALL PMDSTOP
  410 CONTINUE
  408 CONTINUE
C
C     Test ALP 
   85 TS=ALP(1,1)-ALP(1,2)
      IF ((DABS(TS)-PI).GT.1.D-10) THEN
		IF ((DABS(TS)-TWPI).GT.1.D-10) THEN
			IF (DABS(TS).GT.1.D-10) CALL PMDSTOP
		END IF
	END IF 
	IF (NKP.EQ.0) THEN
		AA=ALP(IFC,1)
		BB=ALP(IRC,2)
		TS=ALP(IFC,1)-ALP(IRC,2)
		IF (DABS(TS).GT.1.D-10) THEN
			IF (DABS(DABS(TS)-TWPI).GT.2.D-13) CALL PMDSTOP
		END IF
	END IF     
   86 IF (I.EQ.ISV) GO TO 403  
C     RETURN ORIGIN TO INPUT VALUE    
C      DO 6 K=1,IRC
C      IF (B.LT.A) GO TO 90
C      XR(K)=XR(K)-XG
C      YR(K)=YR(K)-YG
C      GO TO 6   
C   90 XR(K)=XR(K)+XG
C      YR(K)=YR(K)+YG
C    6 CONTINUE
C      DO 5 K=1,IFC
C      IF (B.LT.A) GO TO 92
C      XF(K)=XF(K)-XG
C      YF(K)=YF(K)-YG
C      GO TO 5    
C   92 XF(K)=XF(K)+XG
C      YF(K)=YF(K)+YG
C    5 CONTINUE   
C      DO 73 K=1,NKP
C      IF (B.LT.A) GO TO 84
C      XNK(K)=XNK(K)-XG
C      YNK(K)=YNK(K)-YG
C      GO TO 73
C   84 XNK(K)=XNK(K)+XG
C      YNK(K)=YNK(K)+YG
C   73 CONTINUE
      DO 300 K=1,NPB
      IF (ISV.NE.IEC) THEN
      XHZB(K)=XHZB(K)+YG
      YHZB(K)=YHZB(K)+XG
      ELSE
      XHZB(K)=XHZB(K)-YG
      YHZB(K)=YHZB(K)-XG
      END IF
  300 CONTINUE
      DO 400 K=1,NPE
      IF (ISV.NE.IEC) THEN 
      XHZE(K)=XHZE(K)+YG
      YHZE(K)=YHZE(K)+XG
      ELSE
      XHZE(K)=XHZE(K)-YG
      YHZE(K)=YHZE(K)-XG
      END IF
  400 CONTINUE
      IF (ISV.NE.IEC) THEN
         XBGEQ=XBGEQ+YG
         YBGEQ=YBGEQ+XG
         XNDEQ=XNDEQ+YG
         YNDEQ=YNDEQ+XG
      ELSE
         XBGEQ=XBGEQ-YG
         YBGEQ=YBGEQ-XG
         XNDEQ=XNDEQ-YG
         YNDEQ=YNDEQ-XG
      END IF
      DO 700 K=1,NKP
      IF (ISV.NE.IEC) THEN
      XNK(K)=XNK(K)+YG
      YNK(K)=YNK(K)+XG
      ELSE
      XNK(K)=XNK(K)-YG
      YNK(K)=YNK(K)-XG
      END IF
  700 CONTINUE
  403 CONTINUE
	IF (ICOUNT.EQ.2) THEN 	
	WRITE (30,1001)
	DO KK=1,IRC
	WRITE (30,1000) KK,XR(KK),YR(KK)
	END DO
	WRITE (30,1001)
	DO KK=1,IFC
	WRITE (30,1000) KK,XF(KK),YF(KK)
	END DO
	END IF
	CLOSE (30, STATUS='KEEP')
      IF (ICOUNT.EQ.2) THEN
	OPEN (UNIT=31,FILE='SCRATCHL.DAT',STATUS='REPLACE')
	DO J=1,NPB
		WRITE (31,1000)J,XHZB(J),YHZB(J)
	END DO
	WRITE (31,1001)
	DO J=1,NPE
		WRITE (31,1000) J,XHZE(J),YHZE(J)
	END DO
	WRITE (31,1001)
	WRITE (31,1000) NPB,XBGEQ,YBGEQ
	WRITE (31,1000) NPE,XNDEQ,YNDEQ
	WRITE (31,1001)
	WRITE (31,1001)
	CLOSE (31, STATUS='KEEP')
	CLOSE (33, STATUS='KEEP')
      END IF
  999 RETURN
      END 
