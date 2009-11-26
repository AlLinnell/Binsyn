      SUBROUTINE TDTPL3A(J,K,L,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,
     $VG,TMP,WL,B) 
      INCLUDE 'COMA.FOR'
      DIMENSION GRPH(MTW,MFV),TMPH(MTW,MSX),KGR(MTW),KTMP(MTW)
C     Set control for interpolating in wavelength
C     Default (KEY=0) is not to interpolate      
      KEY=0
C     If tabular interval exceeds 20. angstroms, interpolate      
      IF (WLVRY(2)-WLVRY(1).GT.20.D0) KEY=1
C
C     Calculate B1
C     Is there more than one reference temperature?
C*********************************************************************      
      IF (KTMP(ICOMP).GT.1) THEN
C     Yes.
C     Is our given temperature at the end of the range?      
         IF (K+1.LE.KTMP(ICOMP)) THEN
C     The given temperature is not at the end of the range. 
C     We can interpolate.         
             CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J,K,L),TMPH(ICOMP,K+1),
     $          RFLX(ICOMP,J,K+1,L),TMP,B1)
C     The given temperature is at the end of the range.     
         ELSE
            B1=RFLX(ICOMP,J,K,L)
         END IF
C     There is only one reference temperature         
      ELSE
         B1=RFLX(ICOMP,J,K,L)
      END IF
C*********************************************************************
C
C     Calculate B2
C*********************************************************************      
C     Is there more than one reference gravity value?     
      IF (KGR(ICOMP).GT.1) THEN
C     Yes.
C     Are we at the end of the tabular gravity range?      
         IF (J+1.LE.KGR(ICOMP)) THEN
C     No.
C     Are we at the end of the tabular temperature range?        
            IF(K+1.LE.KTMP(ICOMP)) THEN
C     No. We can interpolate                     
               CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J+1,K,L),
     $            TMPH(ICOMP,K+1),
     $            RFLX(ICOMP,J+1,K+1,L),TMP,B2)
C     We are at the end of the temperature range.     
            ELSE
               B2=RFLX(ICOMP,J+1,K,L)
            END IF
C     We are at the end of the gravity range.               
         ELSE
C     Are we at the end of the temperature range?         
            IF (K+1.LE.KTMP(ICOMP)) THEN
               B2=RFLX(ICOMP,J,K+1,L)
            ELSE
               B2=RFLX(ICOMP,J,K,L)
            END IF
         END IF
      ELSE
         IF (K+1.LE.KTMP(ICOMP)) THEN
            CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J,K,L),TMPH(ICOMP,K+1),
     $         RFLX(ICOMP,J,K+1,L),TMP,B2)
         ELSE
            B2=RFLX(ICOMP,J,K,L)
         END IF             
      END IF
C*********************************************************************
C
C     Test whether we continue
      IF (KEY.EQ.0) GO TO 1
C
C     Calculate   B3
C*********************************************************************            
      IF (L+1.LE.NWLV) THEN
         IF (K+1.LE.KTMP(ICOMP)) THEN            
            CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J,K,L+1),
     $         TMPH(ICOMP,K+1),
     $         RFLX(ICOMP,J,K+1,L+1),TMP,B3)
         ELSE
            B3=RFLX(ICOMP,J,K,L+1)
         END IF
      ELSE
         IF (K+1.LE.KTMP(ICOMP)) THEN
            B3=RFLX(ICOMP,J,K+1,L)
         ELSE
            B3=RFLX(ICOMP,J,K,L)
         END IF
      END IF
C
C*********************************************************************
C
C     Calculate B4
C*********************************************************************      
      IF (L+1.LE.NWLV) THEN
         IF (J+1.LE.KGR(ICOMP)) THEN
            IF (K+1.LE.KTMP(ICOMP)) THEN               
               CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J+1,K,L+1),
     $            TMPH(ICOMP,K+1),
     $            RFLX(ICOMP,J+1,K+1,L+1),TMP,B4)
            ELSE
               IF (K+1.LE.KTMP(ICOMP)) THEN
                  B4=RFLX(ICOMP,J+1,K+1,L+1)
               ELSE
                  B4=RFLX(ICOMP,J+1,K,L+1)
               END IF      
            END IF
         ELSE
            IF(K+1.LE.KTMP(ICOMP)) THEN
               CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J,K,L+1),
     $            TMPH(ICOMP,K+1),
     $            RFLX(ICOMP,J,K+1,L+1),TMP,B4)
            ELSE
               B4=RFLX(ICOMP,J,K,L+1)
            END IF         
         END IF
      ELSE
         IF (J+1.LE.KGR(ICOMP)) THEN
            IF (K+1.LE.KTMP(ICOMP)) THEN
               CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J+1,K,L),
     $            TMPH(ICOMP,K+1),
     $            RFLX(ICOMP,J+1,K+1,L),TMP,B4)
            ELSE
               B4=RFLX(ICOMP,J+1,K,L)
            END IF
         ELSE
            CALL LNTER(TMPH(ICOMP,K),RFLX(ICOMP,J,K,L),TMPH(ICOMP,K+1),
     $           RFLX(ICOMP,J,K+1,L),TMP,B4)
         END IF
      END IF
C*********************************************************************      
    1 IF (J+1.LE.KGR(ICOMP)) THEN 
         CALL LNTER(GRPH(ICOMP,J),B1,GRPH(ICOMP,J+1),B2,VG,B5)
      ELSE
         B5=B1
      END IF
      B=B5
      IF (KEY.EQ.0) GO TO 3      
      IF (L+1.LE.NWLV) THEN
         IF (J+1.LE.KGR(ICOMP)) THEN      
         CALL LNTER(GRPH(ICOMP,J),B3,GRPH(ICOMP,J+1),B4,VG,B6)
         ELSE
         B6=B3
         END IF
      ELSE
         B6=B2
      END IF
      IF (J+1.LE.NWLV) THEN      
         CALL LNTER(WLVRY(L),B5,WLVRY(L+1),B6,WL,B)
      ELSE
         B=B5
      END IF         
    3 RETURN     
      END 
