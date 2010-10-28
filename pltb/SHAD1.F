      SUBROUTINE SHAD1(I,NTH,
     $TARY,XBGEQ,YBGEQ,XNDEQ,YNDEQ,ISEQ,
     $NPB,NPE,NKP,K,L,KYA,KYB,ISV,IEC,IRD,ICOUNT,VRD,NOUT7)
C     SRT TO PRODUCE ARRAYS OF HORIZON POINTS FOR A STELLAR COMPONENT    
C     PROJECTED ON THE PLANE OF THE SKY      
C     PROJECTIONS OF STARTING POINTS ARE IN XS,YS   
C     PROJECTIONS OF END POINTS ARE IN XN,YN 
C     BECAUSE OF THE EFFECTS OF ORBITAL INCLINATION,THERE MAY BE FEWER
C     THAN NTH HORIZON POINTS ON EACH BRANCH
C     NPB CONTAINS A COUNT OF THE ACTUAL NUMBER OF HORIZON POINTS
C     IN THE STARTING BRANCH OF THE HORIZON
C     NPE CONTAINS A COUNT OF THE ACTUAL NUMBER OF HORIZON POINTS
C     IN THE ENDING BRANCH OF THE HORIZON
C     ISV DESIGNATES THE STAR ECLIPSED AT ORBITAL PHASE 0.0
C     IF I.NE.ISV,DISPLACE THE ORIGIN OF I BY -VRD
C     IEC DESIGNATES THE ECLIPSING STAR      
C     NOTE THAT SYMMETRY REQUIRES AS MANY POINTS IN XS AS IN XN,SO
C     ONLY THE ONE PARAMETER,NP,IS NEEDED ON OUTPUT
C     THIS SRT TESTS FOR INSTANCES IN WHICH PART OF THE NECK REGION
C     WOULD BE VISIBLE BUT IS BLOCKED BY AN INTERVENING PORTION OF
C     THE ECLIPSING COMPONENT
      INCLUDE 'COMA.FOR'
	INCLUDE 'COMM.FOR'
      DIMENSION TARY(MTR,MTR)
      DIMENSION XRY(MSX),YRY(MSX),DLX(MFR),DLY(MFR),DLRD(MFR)
	DIMENSION AR1(6),AR2(6)
	DIMENSION AR3(MPHM),AR4(MPHM),TSARY4(MPHM)
	DIMENSION TSARY5(MPHM),XCAP1S(MTHM),YCAP1S(MTHM),XCAP1N(MTHM),
     $YCAP1N(MTHM),XCAP2S(MTHM),YCAP2S(MTHM),XCAP2N(MTHM),YCAP2N(MTHM)
	DIMENSION XST(NTH),YST(NTH),XSN(NTH),YSN(NTH)
	DIMENSION BUF1(MTHM),BUF2(MTHM),BUF3(MTHM),BUF4(MTHM)
C
      DATA PI/3.141592653589793238463D0/
      DATA TWPI/6.2831853071795864769D0/
      DATA ANG/6.29D0/
	DATA ERLT/1.D-13/
	IF (NKP.LE.1) THEN
		VPHS=0.D0
		VPHN=0.D0
	END IF
      IRD=0
      ER=EL
C     Initialize INDX. The program currently does not recognize neck points
      INDX=1
C     Initialize equatorial horizon points to default exclusion
C     from horizon lists
      XBGEQ=ANG
      YBGEQ=ANG
      XNDEQ=ANG
      YNDEQ=ANG
      DO  K=1,MTHM
      XHZB(K)=ANG
      YHZB(K)=ANG
      XHZE(K)=ANG
      YHZE(K)=ANG
      END DO
      DO K=1,MNKP
      XNK(K)=ANG 
      YNK(K)=ANG 
      END DO
      DO K=1,MTHM
      DO  L=1,3
      XS(K,L)=ANG
      XN(K,L)=ANG
      YS(K,L)=ANG
      YN(K,L)=ANG
      END DO
      END DO
C
C     Following tests are for the ordering parameter IRD. It is used
C     later by SRT HORZ. If IRD=1, sort the YR and YF entries in HORZ
C     into decreasing sequence. If IRD=-1, sort the YR and YF entries
C     in HORZ into increasing sequence. When this is done, the initial
C     entry in the YR and YF lists will be the most remote point from
C     the companion.
      IF ((ISEQ.LT.1).OR.(ISEQ.GT.4)) CALL PMDSTOP
      IF (ISEQ.EQ.1) THEN
         IF (I.EQ.IEC) THEN
            IRD=-1
         ELSE
            IRD=1
         END IF      
      END IF
      IF (ISEQ.EQ.2) THEN
         IF (I.EQ.IEC) THEN
            IRD=1
         ELSE
            IRD=-1
         END IF
      END IF
      IF (ISEQ.EQ.3) THEN
         IF (I.EQ.IEC) THEN
            IRD=-1
         ELSE
            IRD=1
         END IF
      END IF
      IF (ISEQ.EQ.4) THEN
         IF (I.EQ.IEC) THEN
            IRD=1
         ELSE
            IRD=-1
         END IF
      END IF
      IF (IRD.EQ.0) CALL PMDSTOP
C
C
      ITHST=-1
	ISW=1
	KCTUPS=0
	KCTUPN=0
	KTH=0
C     Calculate plane-of sky coods. of starting and ending points
C     for all visible segments.
	DO ITH=1,NTH
	 SNTV=SNT(I,ITH)
C     All ANU() values are the same for a given ITH
	 ANUV=ANU(I,ITH,1)
	 N=NPH(I,ITH)
	 DO J=1,3
C     Calculate 3D radius, RVS.
       IF (DABS(VST(I,ITH,J)-ANG).GT.1.D-6) THEN
        CALL SGLTR(I,ITH,VST(I,ITH,J),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $	VCSGS)
C     Calculate 3D coordinates
        CS=DCOS(VST(I,ITH,J))
	  SN=DSIN(VST(I,ITH,J))
	  XSV=RVS*SNTV*CS
	  YSV=RVS*SNTV*SN
	  ZSV=RVS*ANUV
C     Project coods., XAS,YAS are plane-of sky coods.
	  CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
C     Save plane-of-sky information.
        XS(ITH,J)=XAS
	  YS(ITH,J)=YAS
	 END IF
C     Calculate 3D radius, RVS.
       IF (DABS(VND(I,ITH,J)-ANG).GT.1.D-6) THEN
        CALL SGLTR(I,ITH,VND(I,ITH,J),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $	VCSGS)
C     Calculate 3D coordinates
        CS=DCOS(VND(I,ITH,J))
	  SN=DSIN(VND(I,ITH,J))
	  XSV=RVS*SNTV*CS
	  YSV=RVS*SNTV*SN
	  ZSV=RVS*ANUV
C     Project coods., XAS,YAS are plane-of sky coods.
	  CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
C     Save plane-of-sky information.
        XN(ITH,J)=XAS
	  YN(ITH,J)=YAS
	 END IF
	 END DO
	END DO
C	IF (ICOUNT.EQ.2) THEN
C		OPEN (UNIT=19,FILE='../oa/SCRATCHH.DAT',STATUS='REPLACE')
C		DO K=1,NTH
C			WRITE(19,1001) XS(K,1),YS(K,1),XN(K,1),YN(K,1)
C		END DO
C		DO K=1,NTH
C			WRITE (19,1001) XS(K,2),YS(K,2),XN(K,2),YN(K,2)
C		END DO
C		CLOSE(19,STATUS='KEEP')
C	END IF
 1001 FORMAT(4F12.8)
C     Find the ITH value, ITHST, which marks the first colatitude
C     profile which has a segment visible to the observer.
C     Also find the ITH value, ITHND, which marks the last colatitude
C     profile which has a segment passing below the observer's horizon. 
	DO ITH=1,NTH/2+1
		N=NPH(I,ITH)
C     If this test is satisfied, ITHST lies within the control range.
		DO K=1,N
			IF (CSG(I,ITH,K).GE.0.D0) THEN
				IF (ITHST.LT.0) ITHST=ITH
			END IF
		END DO
	END DO
	DO ITH=1,NTH/2
		ITHP=NTH-ITH
		N=NPH(I,ITHP)
		ITHTS=-1
			DO K=1,N
C     If this test is satisfied, ITHND lies within the conrol range.
				IF (CSG(I,ITHP,K).LT.0.D0) ITHTS=1
			END DO
			IF (ITHTS.EQ.1) THEN
				IF (ISW.GT.0.D0) THEN
					ITHND=ITHP
					ISW=-1
				END IF
			END IF
	END DO
C     The values of ITHST and ITHND have been identified.
	DO ITH=ITHST,ITHND
C     Isolate case in which only one XS() and one XN() differ from ANG
      IF ((DABS(VST(I,ITH,1)-ANG).GT.1.0E-9).AND.(DABS(VST(I,ITH,2)-ANG)
     $	.LT.1.0E-9)) THEN
	  IF (DABS(VST(I,ITH,3)-ANG).LT.1.0D-9) THEN
		XST(ITH)=XS(ITH,1)
		YST(ITH)=YS(ITH,1)
		XSN(ITH)=XN(ITH,1)
		YSN(ITH)=YN(ITH,1)
	  IF (ITH.EQ.NTH/2+1) THEN
	    XBGEQ=XS(ITH,1)
		YBGEQ=YS(ITH,1)
		XNDEQ=XN(ITH,1)
		YNDEQ=YN(ITH,1)
	  END IF
		END IF
	END IF
C     Case in which two segments of colatitude circle are visible.
      IF ((DABS(VST(I,ITH,1)-ANG).GT.1.0E-9).AND.(DABS(VST(I,ITH,2)-ANG)
     $	.GT.1.0D-9)) THEN
	 IF (DABS(VST(I,ITH,3)-ANG).LT.1.0D-9) THEN
	    XST(ITH)=XS(ITH,2)
		YST(ITH)=YS(ITH,2)
		XSN(ITH)=XN(ITH,1)
		YSN(ITH)=YN(ITH,1)
	 IF (ITH.EQ.NTH/2+1) THEN
	    XBGEQ=XS(ITH,2)
		YBGEQ=YS(ITH,2)
		XNDEQ=XN(ITH,1)
		YNDEQ=YN(ITH,1)
	 END IF
		END IF
	END IF
C     Case in which three segments of colatitude circle are visible
      IF ((DABS(VST(I,ITH,1)-ANG).GT.1.0E-9).AND.(DABS(VST(I,ITH,2)-ANG)
     $	.GT.1.0D-9)) THEN
	  IF (DABS(VST(I,ITH,3)-ANG).GT.1.0D-9) THEN
	    XST(ITH)=XS(ITH,2)
	    YST(ITH)=YS(ITH,2)
	    XSN(ITH)=XN(ITH,2)
	    YSN(ITH)=YN(ITH,2)
	  IF (ITH.EQ.NTH/2+1) THEN
	    XBGEQ=XS(ITH,2)
		YBGEQ=YS(ITH,2)
		XNDEQ=XN(ITH,2)
		YNDEQ=YN(ITH,2)
	  END IF
	  END IF
	END IF
	END DO
C     End of test for ITH values between ITHST and ITHND.
C     Determine horizon points for polar cap including pole that is
C     below the observer's horizon.
      IF (ITHST.EQ.1) GO TO 300
      IF (ITHST.EQ.2) THEN
	CSGA=CSG(I,1,1)
	IF (DABS(CSGA).LE.1.0D-13) THEN
	SNTV=0.D0
	ANUV=1.D0
    	SN=0.D0
	CS=1.D0
	XSV=RVS*SNTV*CS
	YSV=RVS*SNTV*SN
	ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	KCTUPS=1
	XCAP1S(KCTUPS)=XAS
	YCAP1S(KCTUPS)=YAS
	KCTUPN=1
	XCAP1N(KCTUPN)=XAS
	YCAP1N(KCTUPN)=YAS
	END IF
	IF ((CSGA.LT.0.D0).AND.(DABS(CSGA).GT.1.0D-13)) THEN
      A=VST(I,ITHST,1)
	B=VND(I,ITHST,1)
	C=(A+B)/2.D0
	DTH=TH(I,2)
	DTHA=DTH/5
	DO L=1,6
	TTH=DTHA*(L-1)
	AR1(L)=TTH
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,AR2(L),I,NTH)
        IF (DABS(AR2(L)).LT.1.0D-13) GO TO 80
	END DO
	CALL LAGIN(0.D0,TTH,AR2,AR1,4,6,MM,1.D0)
	IF (MM.LE.2) CALL PMDSTOP
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,VCSGS,I,NTH)
   80	SNTV=DSIN(TTH)
	ANUV=DCOS(TTH)
    	SN=DSIN(C)
	CS=DCOS(C)
	XSV=RVS*SNTV*CS
	YSV=RVS*SNTV*SN
	ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	KCTUPS=1
	XCAP1S(KCTUPS)=XAS
	YCAP1S(KCTUPS)=YAS
	KCTUPN=1
	XCAP1N(KCTUPN)=XAS
	YCAP1N(KCTUPN)=YAS
	END IF
	IF (CSGA.GT.0.D0) THEN
      A=VST(I,ITHST,1)
	B=VND(I,ITHST,1)
	C=(A+B)/2.D0+PI
	DTH=TH(I,2)
	DTHA=DTH/5
	DO L=1,6
	TTH=DTHA*(L-1)
	AR1(L)=TTH
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,AR2(L),I,NTH)
        IF (DABS(AR2(L)).LT.1.0D-13) GO TO 81
	END DO
	CALL LAGIN(0.D0,TTH,AR2,AR1,4,6,MM,1.D0)
	IF (MM.LE.2) CALL PMDSTOP
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,VCSGS,I,NTH)
   81	SNTV=DSIN(TTH)
	ANUV=DCOS(TTH)
    	SN=DSIN(C)
	CS=DCOS(C)
	XSV=RVS*SNTV*CS
	YSV=RVS*SNTV*SN
	ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	KCTUPS=1
	XCAP1S(KCTUPS)=XAS
	YCAP1S(KCTUPS)=YAS
	KCTUPN=1
	XCAP1N(KCTUPN)=XAS
	YCAP1N(KCTUPN)=YAS
	END IF
	END IF
C
C
      IF (ITHST.GT.2) THEN
      KCTUPS=0
	KCTUPN=0
      A=VST(I,ITHST,1)
	B=VND(I,ITHST,1)
	C=VST(I,ITHST,2)
	D=VND(I,ITHST,2)
C     First case is when two segments are visible on colatitude ITHST
C     and the first segment starts on the central meridian and the
C     second segment end on the central meridian.
	JL=ITHND-ITHST
	JCT=(NTH-JL)/2
C     Separate out first case, set A=starting position angle
	IF (DABS(A-D+TWPI).LT.1.E-12) THEN
		A=C-TWPI
	END IF
	DPH=(B-A)/JCT
	J=(B-A)/DPH
	JJ=J/2
	J=2*JJ
	IF (J.LT.4) J=4
	DPH=(B-A)/J
	A=A+DPH/2.D0
	DO JJ=1,J
		TSARY4(JJ)=A+DPH*(JJ-1)
	END DO
	DTH=TH(I,2)
C     Start with first value of TSARY4()
      DO KK=1,J
	KRF=KK
C     Start at upper pole, omitting the pole itself, and go at most
C     to the equator.
	 DO ITH=1,NTH/2
	 TTH=ITH*DTH
C     Calculate the 3D radius, RVS, to the point in question.
	  CALL SGLTP(TTH,TSARY4(KRF),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $   VCSGS,I,NTH)
	  AR3(ITH)=TTH
	  AR4(ITH)=VCSGS
	  IF ((AR4(ITH).GE.0.D0).AND.(AR4(ITH-1).LT.0.D0)) GO TO 9
	 END DO
	 CALL PMDSTOP
    9	  DTHA=DTH/5
    	  DO L=1,6
		AR1(L)=AR3(ITH-1)+DTHA*(L-1)
	    CALL SGLTP(AR1(L),TSARY4(KRF),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $		AR2(L),I,NTH)
        END DO
	  CALL LAGIN(0.D0,TTH,AR2,AR1,4,6,MM,1.D0)
	  IF (MM.LE.2) CALL PMDSTOP
	  CALL SGLTP(TTH,TSARY4(KRF),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $	VCSGS,I,NTH)
	  SNTV=DSIN(TTH)
	  ANUV=DCOS(TTH)
    	  SN=DSIN(TSARY4(KRF))
	  CS=DCOS(TSARY4(KRF))
	  XSV=RVS*SNTV*CS
	  YSV=RVS*SNTV*SN
	  ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	  CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
C     If the point is among the first half of the longitudes in the test
C     list, it corresponds to XS(), YS()
	  IF (KK.LE.J/2) THEN
	   KCTUPS=KCTUPS+1
	   XCAP1S(KCTUPS)=XAS
	   YCAP1S(KCTUPS)=YAS
C     If the point is in the second half of the longitudes in the test
C     it corresponds to XN(), YN().
	  ELSE
	   KCTUPN=KCTUPN+1
	   XCAP1N(KCTUPN)=XAS
	   YCAP1N(KCTUPN)=YAS
	 END IF
   	END DO
	END IF
C     End of test for additional horizon points on upper polar cap.
C
C     Now do test for additional horizon points around lower pole.
C     Initialize counter for points added near lower pole.
  300 IF (NTH-ITHND.EQ.1) THEN
	CSGA=CSG(I,NTH,1)
	IF (CSGA.EQ.0.D0) THEN
	SNTV=0.D0
	ANUV=-1.D0
    	SN=0.D0
	CS=1.D0
	XSV=RVS*SNTV*CS
	YSV=RVS*SNTV*SN
	ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	KCTLPS=1
	XCAP2S(KCTLPS)=XAS
	YCAP2S(KCTLPS)=YAS
	KCTLPN=1
	XCAP2N(KCTLPN)=XAS
	YCAP2N(KCTLPN)=YAS
	END IF 
	IF (CSGA.LT.0.D0) THEN
      A=VST(I,ITHND,1)
	B=VND(I,ITHND,1)
	C=(A+B)/2.D0
	DTH=TH(I,2)
	DTHA=DTH/5
	DO L=1,6
	TTH=PI-DTHA*(L-1)
	AR1(L)=TTH
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,AR2(L),I,NTH)
        IF (DABS(AR2(L)).LT.1.0D-13) GO TO 78
	END DO
	CALL LAGIN(0.D0,TTH,AR2,AR1,4,6,MM,1.D0)
	IF (MM.LE.2) CALL PMDSTOP
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,VCSGS,I,NTH)
   78	SNTV=DSIN(TTH)
	ANUV=DCOS(TTH)
    	SN=DSIN(C)
	CS=DCOS(C)
	XSV=RVS*SNTV*CS
	YSV=RVS*SNTV*SN
	ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	KCTLPS=1
	XCAP2S(KCTLPS)=XAS
	YCAP2S(KCTLPS)=YAS
	KCTLPN=1
	XCAP2N(KCTLPN)=XAS
	YCAP2N(KCTLPN)=YAS
	END IF
C
	IF (CSGA.GT.0.D0) THEN
	IF ((DABS(VST(I,ITHND,2)-ANG).LT.1.D-11).AND.
     $	(DABS(VND(I,ITHND,2)-ANG).LT.1.D-11)) THEN
		A=VST(I,ITHND,1)
		B=VND(I,ITHND,1)
		C=(A+B)/2.D0+PI
	END IF
	IF ((DABS(VST(I,ITHND,2)-ANG).GT.1.D-11).AND.
     $	(DABS(VND(I,ITHND,2)-ANG).GT.1.D-11)) THEN
		A=VND(I,ITHND,1)
		B=VST(I,ITHND,2)
		C=(A+B)/2.D0
	END IF
	DTH=TH(I,2)
	DTHA=DTH/5
	DO L=1,6
	TTH=PI-DTHA*(L-1)
	AR1(L)=TTH
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,AR2(L),I,NTH)
        IF (DABS(AR2(L)).LT.1.0D-13) GO TO 21
	END DO
	CALL LAGIN(0.D0,TTH,AR2,AR1,4,6,MM,1.D0)
	IF (MM.LE.2) CALL PMDSTOP
	CALL SGLTP(TTH,C,ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,VCSGS,I,NTH)
   21	SNTV=DSIN(TTH)
	ANUV=DCOS(TTH)
    	SN=DSIN(C)
	CS=DCOS(C)
	XSV=RVS*SNTV*CS
	YSV=RVS*SNTV*SN
	ZSV=RVS*ANUV
C     Calculate plane-of-sky coods.
	CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	KCTLPS=1
	XCAP2S(KCTLPS)=XAS
	YCAP2S(KCTLPS)=YAS
	KCTLPN=1
	XCAP2N(KCTLPN)=XAS
	YCAP2N(KCTLPN)=YAS
	END IF
	END IF
C
      IF (NTH-ITHND.GT.1) THEN
      KCTLPS=0
	KCTLPN=0
	ITHNDA=ITHND
C     Case in which two segments are visible, with central meridian
C     within the region of visibility.
C     Note that the profiles to be examined must extend to the far
C     side of the star.
  111	IF ((VST(I,ITHND,1).LT.1.D-11).AND.(DABS(VND(I,ITHND,2)-TWPI).LT.
     $1.D-11)) THEN
	A=VND(I,ITHND,1)
	B=VST(I,ITHND,2)
	IF (DABS(B-A).LT.ERLT) THEN
		ITHND=ITHND-1
		IF (ITHNDA-ITHND.LT.2) THEN
			GO TO 111
		ELSE
			CALL PMDSTOP
		END IF
	END IF
	END IF
C     Case in which one segment is visible
C     Note that the profiles must extend to the far side of the star.
	IF (DABS(VST(I,ITHND,2)-ANG).LT.1.D-11) THEN
	A=VND(I,ITHND,1)
	B=VST(I,ITHND,1)+TWPI
	END IF
C	Case in which three segments are visible. 
      IF (DABS(VST(I,ITHND,3)-ANG).GT.5.0D-3) THEN
	A=VND(I,ITHND,2)
	B=VST(I,ITHND,2)+TWPI
	END IF
C
	JL=ITHND-ITHST
	JCT=(NTH-JL)/2
	DPH=(B-A)/JCT
	J=(B-A)/DPH
	JJ=J/2
	J=2*JJ
	IF (J.LT.4) J=4
	DPH=(B-A)/J
	A=A+DPH/2.D0
	DO JJ=1,J
	 TSARY5(JJ)=A+DPH*(JJ-1)
	 IF (TSARY5(JJ).GT.TWPI) TSARY5(JJ)=TSARY5(JJ)-TWPI
	END DO
	DTH=TH(I,2)
C     Start with first value of TSARY5()
      DO KK=1,J
	KRF=KK
C     Start at last colatitude before lower pole
	 DO ITH=1,NTH
	 TTH=PI-ITH*DTH
	  CALL SGLTP(TTH,TSARY5(KRF),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $   VCSGS,I,NTH)
	  AR3(ITH)=TTH
	  AR4(ITH)=VCSGS
	  IF ((AR4(ITH).LT.0.D0).AND.(AR4(ITH-1).GE.0.D0)) 
     $	GO TO 16
	  END DO
	  CALL PMDSTOP
   16	  DTHA=DTH/5
        DO L=1,6
		AR1(L)=AR3(ITH)+DTHA*(L-1)
		CALL SGLTP(AR1(L),TSARY5(KRF),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $		AR2(L),I,NTH)
	  END DO
	  CALL LAGIN(0.D0,TTH,AR2,AR1,4,6,MM,1.D0)
	  IF (MM.LE.2) CALL PMDSTOP
	  CALL SGLTP(TTH,TSARY5(KRF),ER,RVS,GVS,VLVS,VMVS,VNVS,VCSBS,
     $	VCSGS,I,NTH)
	  SNTV=DSIN(TTH)
	  ANUV=DCOS(TTH)
	  SN=DSIN(TSARY5(KRF))
	  CS=DCOS(TSARY5(KRF))
	  XSV=RVS*SNTV*CS
	  YSV=RVS*SNTV*SN
	  ZSV=RVS*ANUV
	  CALL PROJ(TARY,XSV,YSV,ZSV,XAS,YAS,ZAS)
	  KCTLP=KCTLP+1
	  IF (KK.LE.J/2) THEN
	   KCTLPS=KCTLPS+1
	   XCAP2S(KCTLPS)=XAS
	   YCAP2S(KCTLPS)=YAS
	  ELSE
	   KCTLPN=KCTLPN+1
	   XCAP2N(KCTLPN)=XAS
	   YCAP2N(KCTLPN)=YAS
	 END IF
	 END DO
	END IF
C     Check for possible interchange of lower cap starting and
C     ending points
      SUM1=0.D0
	DO J=1,KCTUPS
		SUM1=SUM1+YCAP1S(J)
	END DO
	SUM2=0.D0
	DO J=1,KCTUPN
		SUM2=SUM2+YCAP1N(J)
	END DO
	SUM3=0.D0
	DO J=1,KCTLPS
		SUM3=SUM3+YCAP2S(J)
	END DO
	SUM4=0.D0
	DO J=1,KCTLPN
		SUM4=SUM4+YCAP2N(J)
	END DO
	IF ((DABS(SUM3-SUM2).LT.DABS(SUM3-SUM1)).AND.(DABS(SUM4-SUM1).LT.
     $DABS(SUM4-SUM2))) THEN
		KCTLPSS=KCTLPS
		KCTLPNS=KCTLPN
		DO J=1,KCTLPS
			BUF1(J)=XCAP2S(J)
			BUF2(J)=YCAP2S(J)
		END DO
		DO J=1,KCTLPN
			BUF3(J)=XCAP2N(J)
			BUF4(J)=YCAP2N(J)
		END DO
		DO J=1,KCTLPN
			XCAP2S(J)=BUF3(J)
			YCAP2S(J)=BUF4(J)
		END DO
		DO J=1,KCTLPS
			XCAP2N(J)=BUF1(J)
			YCAP2N(J)=BUF2(J)
		END DO
		KCTLPS=KCTLPNS
		KCTLPN=KCTLPSS
	END IF
C     End of lower cap processing
C
C     Sort upper cap starting points into increasing sequence
      DO J=1,KCTUPS
		DO JJ=1,KCTUPS-1
			IF (DABS(YCAP1S(JJ)).GT.DABS(YCAP1S(JJ+1))) THEN
				SV=YCAP1S(JJ)
				YCAP1S(JJ)=YCAP1S(JJ+1)
				YCAP1S(JJ+1)=SV 
				SV=XCAP1S(JJ)
				XCAP1S(JJ)=XCAP1S(JJ+1)
				XCAP1S(JJ+1)=SV 
			END IF
		END DO
	END DO
C     Sort lower cap starting points into proper sequence
      IF ((YST(ITHND).GT.0.D0).AND.(YCAP2S(1).LT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).LT.0.D0) THEN
			GO TO 151
		END IF
	END IF
      IF ((YST(ITHND).GT.0.D0).AND.(YCAP2S(1).GT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).LT.0.D0) THEN
			GO TO 160
		END IF
	END IF
      IF ((YST(ITHND).GT.0.D0).AND.(YCAP2S(1).LT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).GT.0.D0) THEN
			GO TO 150
		END IF
	END IF
      IF ((YST(ITHND).GT.0.D0).AND.(YCAP2S(1).GT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).GT.0.D0) THEN
			GO TO 150
		END IF
	END IF
      IF ((YST(ITHND).LT.0.D0).AND.(YCAP2S(1).LT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).LT.0.D0) THEN
			GO TO 151
		END IF
	END IF
      IF ((YST(ITHND).LT.0.D0).AND.(YCAP2S(1).GT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).LT.0.D0) THEN
			GO TO 160
		END IF
	END IF
      IF ((YST(ITHND).LT.0.D0).AND.(YCAP2S(1).LT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).GT.0.D0) THEN
			GO TO 160
		END IF
	END IF
      IF ((YST(ITHND).LT.0.D0).AND.(YCAP2S(1).GT.0.D0)) THEN
		IF (YCAP2S(KCTLPS).GT.0.D0) THEN
			GO TO 160
		END IF
	END IF
  150 DO J=1,KCTLPS
		DO JJ=1,KCTLPS-1
			IF (DABS(YCAP2S(JJ)).LT.DABS(YCAP2S(JJ+1))) THEN
				SV=YCAP2S(JJ)
				YCAP2S(JJ)=YCAP2S(JJ+1)
				YCAP2S(JJ+1)=SV 
				SV=XCAP2S(JJ)
				XCAP2S(JJ)=XCAP2S(JJ+1)
				XCAP2S(JJ+1)=SV 
			END IF
		END DO
	END DO
	GO TO 160
  151    DO J=1,KCTLPS
		DO JJ=1,KCTLPS-1
			IF (DABS(YCAP2S(JJ)).GT.DABS(YCAP2S(JJ+1))) THEN
				SV=YCAP2S(JJ)
				YCAP2S(JJ)=YCAP2S(JJ+1)
				YCAP2S(JJ+1)=SV 
				SV=XCAP2S(JJ)
				XCAP2S(JJ)=XCAP2S(JJ+1)
				XCAP2S(JJ+1)=SV 
			END IF
		END DO
	END DO
C     Copy starting points from both caps and ITHST, ITHND region
C     into arrays XHZB(), YHZB().
  160	 DO JJ=1,KCTUPS
		XHZB(JJ)=XCAP1S(JJ)
		YHZB(JJ)=YCAP1S(JJ)
	 END DO
	 JJ=JJ-1
	 DO ITH=ITHST,ITHND
		JJ=JJ+1
		XHZB(JJ)=XST(ITH)
		YHZB(JJ)=YST(ITH)
	 END DO
	 DO JK=1,KCTLPS
		JJ=JJ+1
		XHZB(JJ)=XCAP2S(JK)
		YHZB(JJ)=YCAP2S(JK)
	 END DO
	 NPB=JJ
C     Sort upper cap ending points into increasing sequence
      DO J=1,KCTUPN
		DO JJ=1,KCTUPN-1
			IF (DABS(YCAP1N(JJ)).GT.DABS(YCAP1N(JJ+1))) THEN
				SV=YCAP1N(JJ)
				YCAP1N(JJ)=YCAP1N(JJ+1)
				YCAP1N(JJ+1)=SV 
				SV=XCAP1N(JJ)
				XCAP1N(JJ)=XCAP1N(JJ+1)
				XCAP1N(JJ+1)=SV 
			END IF
		END DO
	END DO
C     Sort lower cap ending points into proper sequence
      IF ((YSN(ITHND).GT.0.D0).AND.(YCAP2N(1).LT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).LT.0.D0) THEN
			GO TO 153
		END IF
	END IF
      IF ((YSN(ITHND).GT.0.D0).AND.(YCAP2N(1).GT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).LT.0.D0) THEN
			GO TO 161
		END IF
	END IF
      IF ((YSN(ITHND).GT.0.D0).AND.(YCAP2N(1).LT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).GT.0.D0) THEN
			GO TO 152
		END IF
	END IF
      IF ((YSN(ITHND).GT.0.D0).AND.(YCAP2N(1).GT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).GT.0.D0) THEN
			GO TO 152
		END IF
	END IF
      IF ((YSN(ITHND).LT.0.D0).AND.(YCAP2N(1).LT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).LT.0.D0) THEN
			GO TO 152
		END IF
	END IF
      IF ((YSN(ITHND).LT.0.D0).AND.(YCAP2N(1).GT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).LT.0.D0) THEN
			GO TO 161
		END IF
	END IF
      IF ((YSN(ITHND).LT.0.D0).AND.(YCAP2N(1).LT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).GT.0.D0) THEN
			GO TO 161
		END IF
	END IF
      IF ((YSN(ITHND).LT.0.D0).AND.(YCAP2N(1).GT.0.D0)) THEN
		IF (YCAP2N(KCTLPN).GT.0.D0) THEN
			GO TO 161
		END IF
	END IF
  152    DO J=1,KCTLPN
		DO JJ=1,KCTLPN-1
			IF (DABS(YCAP2N(JJ)).LT.DABS(YCAP2N(JJ+1))) THEN
				SV=YCAP2N(JJ)
				YCAP2N(JJ)=YCAP2N(JJ+1)
				YCAP2N(JJ+1)=SV 
				SV=XCAP2N(JJ)
				XCAP2N(JJ)=XCAP2N(JJ+1)
				XCAP2N(JJ+1)=SV 
			END IF
		END DO
	END DO
	GO TO 161
  153    DO J=1,KCTLPN
		DO JJ=1,KCTLPN-1
			IF (DABS(YCAP2N(JJ)).GT.DABS(YCAP2N(JJ+1))) THEN
				SV=YCAP2N(JJ)
				YCAP2N(JJ)=YCAP2N(JJ+1)
				YCAP2N(JJ+1)=SV 
				SV=XCAP2N(JJ)
				XCAP2N(JJ)=XCAP2N(JJ+1)
				XCAP2N(JJ+1)=SV 
			END IF
		END DO
	END DO
C     Copy ending points from both caps and ITHST, ITHND region
C     into arrays XHZE(), YHZE().
  161	 DO JJ=1,KCTUPN
		XHZE(JJ)=XCAP1N(JJ)
		YHZE(JJ)=YCAP1N(JJ)
	 END DO
	 JJ=JJ-1
	 DO ITH=ITHST,ITHND
		JJ=JJ+1
		XHZE(JJ)=XSN(ITH)
		YHZE(JJ)=YSN(ITH)
	 END DO
	 DO JK=1,KCTLPN
		JJ=JJ+1
		XHZE(JJ)=XCAP2N(JK)
		YHZE(JJ)=YCAP2N(JK)
	 END DO
	 NPE=JJ
C     ONE BRANCH OF THE HORIZON BELONGS TO THE VST ARRAY   
C     THE OTHER BRANCH BELONGS TO THE VND ARRAY     
C     THE FIRST POINT COMMON TO BOTH BRANCHES,HAS CENTER OF STAR POLAR   
C     ANGLE COODS. TFP(I),PFP(I) OR THP(I),PHP(I)   
C     THP=Region around upper pole entirely below horizon
C     TFP=Region around upper pole entirely above horizon
C     PHP=Longitude for which THP is on the horizon
C     THM=Region around lower pole entirely below horizon
C     TFM=Region around lower pole entirely above horizon
C     PHM=Longitude for which THM is on the horizon
C     If KYA=0, use TFP in tests
C     If KYA=1, use THP in tests
C     If KYB=0, use TFM in tests
C     If KYB=1, use THM in tests
C
C     The following visibility test for neck points is based on a
C     comparison of neck point Y coods. with the XS,YS,XN,YN coods
C     that have been sorted into an ascending sequence by their Y
C     cood. values and stored in XRY, YRY. If a neck point Y cood.
C     coincides with an extremity Y cood. then that neck point is
C     visible and should be entered on the saved list of neck points.
C     If a neck point Y cood. lies between the extremity Y coods., and
C     does not coincide at either end, it is hidden and should not be
C     entered on the saved list of neck points.
C     
C     Test for visibility of neck point. 
C     If this is a visible neck point, enter it in the XNK,YNK arrays.
C     Begin with VST. VPHS is the opening angle of the neck at that ITH.
      IF (NKP.EQ.0) GO TO 98
      JREF=0
      DO J=1,3
         IF (DABS(VST(I,ITH,J)-VPHS).LT.1.D-9) JREF=J
      END DO
C     If there is a valid comparison, test extremities
      IF (JREF.NE.0) THEN
C     Compare with first extremity      
         IF (DABS(YS(ITH,JREF)-YRY(1)).LE.ERLT) THEN
C     The neck point is visible and starts the visibility region of the
C     star for this ITH
		IF (DABS(XS(ITH,JREF)-ANG).GT.1.D-9) THEN         
			CALL SRTA(XS(ITH,JREF),YS(ITH,JREF),XNK,YNK,1,30)
			NKP=NKP+1
		END IF
            IF (ITH.EQ.NTH/2+1) THEN
               XBGEQ=XS(ITH,JREF)
               YBGEQ=YS(ITH,JREF)
            END IF
C     If we have found a visible neck point starting the visibility 
C     region, the end point should be found by extrapolation
            DLX(1)=XHZE(NPE)-XN(ITH,1)
            DLY(1)=YHZE(NPE)-YN(ITH,1)
            DLRD(1)=DSQRT(DLX(1)**2+DLY(1)**2)
            DLX(2)=XHZE(NPE)-XN(ITH,2)
            DLY(2)=YHZE(NPE)-YN(ITH,2)
            DLRD(2)=DSQRT(DLX(2)**2+DLY(2)**2)
C     DLMN is 1/2 the "diameter" of the star, if the extremes
C     represent points at opposite horizons. The distance of a 
C     putative horizon point from the last point on the same horizon
C     must be less than DLMN.            
            DLMN=DABS(YRY(1)-YRY(INDX))/2.D0
            IF (DLRD(1).LT.DLRD(2)) THEN
               IF (DLRD(1).LT.DLMN) THEN            
                  CALL SRTA(XN(ITH,1),YN(ITH,1),XHZE,YHZE,1,200)
                  NPE=NPE+1
                  IF (ITH.EQ.NTH/2+1) THEN
                     XNDEQ=XN(ITH,1)
                     YNDEQ=YN(ITH,1)
                  END IF 
                  GO TO 61
               END IF   
            ELSE
               IF (DLRD(2).LT.DLMN) THEN
                  CALL SRTA(XN(ITH,2),YN(ITH,2),XHZE,YHZE,1,200)
                  NPE=NPE+1
                  IF (ITH.EQ.NTH/2+1) THEN
                     XNDEQ=XN(ITH,2)
                     YNDEQ=YN(ITH,2)
                  END IF
               END IF
               GO TO 61                                           
            END IF               
         END IF
C     No hit at first extremity. Try other extremity.      
         IF (DABS(YS(ITH,JREF)-YRY(INDX)).LE.ERLT) THEN
C     The neck point is visible and starts the visibility region of the
C     star for this ITH         
            CALL SRTA(XS(ITH,JREF),YS(ITH,JREF),XNK,YNK,1,30)
            NKP=NKP+1
            IF (ITH.EQ.NTH/2+1) THEN
               XBGEQ=XS(ITH,JREF)
               YBGEQ=YS(ITH,JREF)
            END IF
C     If we have found a visible neck point starting the visibility
C     region, the end point should be found by extrapolation.            
            DLX(1)=XHZE(NPE)-XN(ITH,1)
            DLY(1)=YHZE(NPE)-YN(ITH,1)
            DLRD(1)=DSQRT(DLX(1)**2+DLY(1)**2)
            DLX(2)=XHZE(NPE)-XN(ITH,2)
            DLY(2)=YHZE(NPE)-YN(ITH,2)
            DLRD(2)=DSQRT(DLX(2)**2+DLY(2)**2)
            DLMN=DABS(YRY(1)-YRY(INDX))/2.D0
            IF (DLRD(1).LT.DLRD(2)) THEN
               IF (DLRD(1).LT.DLMN) THEN            
                  CALL SRTA(XN(ITH,1),YN(ITH,1),XHZE,YHZE,1,200)
                  NPE=NPE+1
                  IF (ITH.EQ.NTH/2+1) THEN
                     XNDEQ=XN(ITH,1)
                     YNDEQ=YN(ITH,1)
                  END IF
                  GO TO 61
               END IF
            ELSE
               IF (DLRD(2).LT.DLMN) THEN
                  CALL SRTA(XN(ITH,2),YN(ITH,2),XHZE,YHZE,1,200)
                  NPE=NPE+1
                  IF (ITH.EQ.NTH/2+1) THEN
                     XNDEQ=XN(ITH,2)
                     YNDEQ=YN(ITH,2)
                  END IF
                  GO TO 61
               END IF                              
            END IF   
         END IF
      END IF   
C     No coincidence of VST point with an extremity.         
C
C     Test VND
      JREF=0
      DO J=1,3
         IF (DABS(VND(I,ITH,J)-VPHN).LT.1.D-9) JREF=J
      END DO
      IF (JREF.NE.0) THEN
         IF (DABS(YN(ITH,JREF)-YRY(1)).LE.ERLT) THEN
C     The neck point is visible and ends the visibility region of the
C     star for this ITH         
            CALL SRTA(XN(ITH,JREF),YN(ITH,JREF),XNK,YNK,1,30)
            NKP=NKP+1
            IF (ITH.EQ.NTH/2+1) THEN
               XNDEQ=XN(ITH,JREF)
               YNDEQ=YN(ITH,JREF)
            END IF
C     If the neck point is visible and ends the visibility region,
C     the starting point should be found by extrapolation
               DLX(1)=XHZB(NPB)-XS(ITH,1)
               DLY(1)=YHZB(NPB)-YS(ITH,1)
               DLRD(1)=DSQRT(DLX(1)**2+DLY(1)**2)
               DLX(2)=XHZB(NPB)-XS(ITH,2)
               DLY(2)=YHZB(NPB)-YS(ITH,2)
               DLRD(2)=DSQRT(DLX(2)**2+DLY(2)**2)
               DLMN=DABS(YRY(1)-YRY(INDX))
               IF (DLRD(1).LT.DLRD(2)) THEN
                  IF (DLRD(1).LT.DLMN) THEN
                     CALL SRTA(XS(ITH,1),YS(ITH,1),XHZB,YHZB,1,200)
                     NPB=NPB+1
                     IF (ITH.EQ.NTH/2+1) THEN
                        XBGEQ=XS(ITH,1)
                        YBGEQ=YS(ITH,1)
                     END IF
                     GO TO 61               
                  END IF   
               ELSE
                  IF ((DLRD(2).LT.DLMN).AND.(XS(ITH,2).NE.ANG)) THEN
                     CALL SRTA(XS(ITH,2),YS(ITH,2),XHZB,YHZB,1,200)
                     NPB=NPB+1
                     IF (ITH.EQ.NTH/2+1) THEN
                        XBGEQ=XS(ITH,2)
                        YBGEQ=YS(ITH,2)
                     END IF
                     GO TO 61               
                  END IF                           
               END IF   
            IF (DABS(YN(ITH,JREF)-YRY(INDX)).LE.ERLT) THEN
C     The neck point is visible and ends the visibility region of the
C     star for this ITH
               CALL SRTA(XN(ITH,JREF),YN(ITH,JREF),XNK,YNK,1,30)
               NKP=NKP+1
               IF (ITH.EQ.NTH/2+1) THEN
                  XNDEQ=XN(ITH,JREF)
                  YNDEQ=YN(ITH,JREF)
               END IF
               DLX(1)=XHZB(NPB)-XS(ITH,1)
               DLY(1)=YHZB(NPB)-YS(ITH,1)
               DLRD(1)=DSQRT(DLX(1)**2+DLY(1)**2)
               DLX(2)=XHZB(NPB)-XS(ITH,2)
               DLY(2)=YHZB(NPB)-YS(ITH,2)
               DLRD(2)=DSQRT(DLX(2)**2+DLY(2)**2)
               DLMN=DABS(YRY(1)-YRY(INDX))
               IF (DLRD(1).LT.DLRD(2)) THEN
                  IF (DLRD(1).LT.DLMN) THEN
                     CALL SRTA(XS(ITH,1),YS(ITH,1),XHZB,YHZB,1,200)
                     NPB=NPB+1
                     IF (ITH.EQ.NTH/2+1) THEN
                        XBGEQ=XS(ITH,1)
                        YBGEQ=YS(ITH,1)
                     END IF
                     GO TO 61               
                  END IF   
               ELSE
                  IF ((DLRD(2).LT.DLMN).AND.(XS(ITH,2).NE.ANG)) THEN
                     CALL SRTA(XS(ITH,2),YS(ITH,2),XHZB,YHZB,1,200)
                     NPB=NPB+1
                     IF (ITH.EQ.NTH/2+1) THEN
                        XBGEQ=XS(ITH,2)
                        YBGEQ=YS(ITH,2)
                     END IF
                     GO TO 61               
                  END IF                              
               END IF
            END IF
         END IF
      END IF               
C     The starting neck point does not coincide with either Y cood 
C     extremity. It is not visible.
C     The ending neck point does not coincide with either Y cood
C     extremity. It is not visible. 
C     Enter horizon points by normal process.
      DLXHZ=XHZB(NPB)-XHZB(NPB-1)
      DLYHZ=YHZB(NPB)-YHZB(NPB-1)
      PDX=XHZB(NPB)+DLXHZ
      PDY=YHZB(NPB)+DLYHZ
      DLX(1)=PDX-XRY(1)
      DLY(1)=PDY-YRY(1)
      DLRD(1)=DSQRT(DLX(1)**2+DLY(1)**2)
      DLX(2)=PDX-XRY(INDX)
      DLY(2)=PDY-YRY(INDX)
      DLRD(2)=DSQRT(DLX(2)**2+DLY(2)**2)
      IF ((DLRD(1).LT.DLRD(2)).AND.(XRY(1).NE.ANG)) THEN
         CALL SRTA(XRY(1),YRY(1),XHZB,YHZB,1,200)
         NPB=NPB+1
         IF (ITH.EQ.NTH/2+1) THEN
            XBGEQ=XRY(1)
            YBGEQ=YRY(1)
         END IF
         CALL SRTA(XRY(INDX),YRY(INDX),XHZE,YHZE,1,200)
         NPE=NPE+1
         IF (ITH.EQ.NTH/2+1) THEN
            XNDEQ=XRY(INDX)
            YNDEQ=YRY(INDX)
         END IF
         GO TO 61
      ELSE
         CALL SRTA(XRY(1),YRY(1),XHZE,YHZE,1,200)
         NPE=NPE+1
         IF (ITH.EQ.NTH/2+1) THEN
            XNDEQ=XRY(1)
            YNDEQ=YRY(1)
         END IF
         CALL SRTA(XRY(INDX),YRY(INDX),XHZB,YHZB,1,200)
         NPB=NPB+1
         IF (ITH.EQ.NTH/2+1) THEN
            XBGEQ=XRY(INDX)
            YBGEQ=YRY(INDX)
         END IF
         GO  TO 61
      END IF    

C     If I.EQ.IEC, and NKP.GE.2, make sure starting and ending 
C     neck points are on appropriate horizon lists
      IF ((I.EQ.IEC).AND.(NKP.GE.2)) THEN
C     Assume the neck points should be added to the branch with
C     fewer points      
         IF (NPB.LE.NPE) THEN
            JREF=0
            IF (XNK(1).LT.XNK(NKP)) KREF=1
            IF (XNK(NKP).LT.XNK(1)) KREF=NKP
C     Since entries on the XHZB, YHZB list start from the upper 
C     pole, those entries always are in increasing XHZB sequence.            
            DO J=1,NPB-1
               IF ((XHZB(J).LE.XNK(KREF)).AND.
     $            (XHZB(J+1).GT.XNK(KREF))) JREF=J
            END DO
            IF (JREF.EQ.0) CALL PMDSTOP
C     Make sure the neck point goes at the indicated place
            DLX(1)=XNK(KREF)-XHZB(JREF)
            DLY(1)=YNK(KREF)-YHZB(JREF)
            DLRD(1)=DSQRT(DLX(1)**2+DLY(1)**2)
            JREFA=0
            DO J=1,NPE-1
               IF ((XHZE(J).LE.XNK(KREF)).AND.
     $            (XHZE(J+1).GT.XNK(KREF))) JREFA=J
            END DO
            IF (JREFA.EQ.0) CALL PMDSTOP
            DLX(2)=XNK(KREF)-XHZE(JREFA)
            DLY(2)=YNK(KREF)-YHZE(JREFA)
            DLRD(2)=DSQRT(DLX(2)**2+DLY(2)**2)
            IF ((DLRD(1).LT.DLRD(2)).AND.(DLRD(1).NE.0.D0)) THEN
C     The assumption that the point goes on XHZB,YHZB was 
C     correct. The test .NE.0  makes sure the point has not
C     already been added.            
               CALL SRTA(XNK(KREF),YNK(KREF),XHZB,YHZB,1,200)
               NPB=NPB+1
C     The point at the other end of the neck should also be
C     added            
               KREF=NKP-KREF+1
               CALL SRTA(XNK(KREF),YNK(KREF),XHZB,YHZB,1,200)
               NPB=NPB+1
               KREF=NKP-KREF+1               
            END IF               
C     If the following test is true, the assumption the point 
C     goes on the XHZB,YHZB branch was wrong.
            IF ((DLRD(2).LT.DLRD(1)).AND.(DLRD(2).NE.0.D0)) THEN
               CALL SRTA(XNK(KREF),YNK(KREF),XHZE,YHZE,1,200)
               NPE=NPE+1
               KREF=NKP-KREF+1
               CALL SRTA(XNK(KREF),YNK(KREF),XHZE,YHZE,1,200)
               NPE=NPE+1
               KREF=NKP-KREF+1
            END IF                           
         END IF
      END IF
   61 CALL PMDSTOP
   60 CONTINUE
   98 CONTINUE
C     Displace points if I.NE.ISV so center of star is at XG,YG.
      IF (I.NE.ISV) THEN
      CALL PROJ(TARY,VRD,0.D0,0.D0,XG,YG,ZG)
	SMY=0.D0
	DO JJ=1,NPB
		SMY=SMY+YHZB(JJ)
	END DO
	DO JJ=1,NPE
		SMY=SMY+YHZE(JJ)
	END DO
	DV=NPB+NPE
	SMY=SMY/DV
	SMX=0.D0
	DO JJ=1,NPB
		SMX=SMX+XHZB(JJ)
	END DO
	DO JJ=1,NPE
		SMX=SMX+XHZE(JJ)
	END DO
	SMX=SMX/DV
	A=DSQRT((SMX+XG)**2+(SMY+YG)**2)
	B=DSQRT((SMX-XG)**2+(SMY-YG)**2)
	DO JJ=1,NPB
		IF (B.LT.A) THEN
			XHZB(JJ)=XHZB(JJ)+XG
			YHZB(JJ)=YHZB(JJ)+YG
		ELSE
			XHZB(JJ)=XHZB(JJ)-XG
			YHZB(JJ)=YHZB(JJ)-YG
		END IF
	END DO
	DO JJ=1,NPE
		IF (B.LT.A) THEN
			XHZE(JJ)=XHZE(JJ)+XG
			YHZE(JJ)=YHZE(JJ)+YG
		ELSE
			XHZE(JJ)=XHZE(JJ)-XG
			YHZE(JJ)=YHZE(JJ)-YG
		END IF
	END DO
	IF (B.LT.A) THEN
		XBGEQ=XBGEQ+XG
		YBGEQ=YBGEQ+YG
		XNDEQ=XNDEQ+XG
		YNDEQ=YNDEQ+YG
	ELSE
		XBGEQ=XBGEQ-XG
		YBGEQ=YBGEQ-YG
		XNDEQ=XNDEQ-XG
		YNDEQ=YNDEQ-YG
	END IF
	END IF
   99 CONTINUE
C      IF (ICOUNT.EQ.2) THEN
C	OPEN (UNIT=19,FILE='../oa/SCRATCHH.DAT',STATUS='REPLACE')
C	DO K=1,NPE
C		WRITE (19,1000) XHZE(K),YHZE(K)
C	END DO
C	DO K=1,NPB
C		WRITE (19,1000) XHZB(K),YHZB(K)
C	END DO
C	CLOSE (19, STATUS='KEEP')
C	END IF
 1000 FORMAT (2F12.8)
      RETURN     
      END 
