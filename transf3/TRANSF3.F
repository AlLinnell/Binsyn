      PROGRAM TRANSF3
C     Program to copy output files from SUMMARY to new input files
      DIMENSION FL1(5)
      CHARACTER * 36 FL1
      CHARACTER * 82 BUFF
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  104 FORMAT (A36)
  105 FORMAT (I2)
  106 FORMAT (A82)
C********************************************************************
C
      OPEN ( UNIT = 66,  FILE = 'TRANSF3.ILS',
     $STATUS = 'OLD')
      READ ( 66, 104)    (FL1(J), J=1,5)
      OPEN ( UNIT = 1, FILE = FL1(1),   STATUS = 'OLD')
      WRITE ( 1, 105) KEYBAD
      REWIND 1
      OPEN ( UNIT = 2, FILE = FL1(2),  STATUS = 'OLD')
      OPEN ( UNIT = 3, FILE = FL1(3),  STATUS = 'OLD')
      OPEN ( UNIT = 4, FILE = FL1(4),  STATUS = 'REPLACE')
      OPEN ( UNIT =55, FILE = FL1(5),  STATUS = 'REPLACE')
C***************************************************************
    1 READ (2,106, END=2,ERR=99) BUFF
      WRITE (4,106) BUFF
      GO TO 1
    2 READ (3,106, END=3,ERR=99) BUFF
      WRITE (55,106) BUFF
      GO TO 2
    3 CLOSE (2, STATUS = 'DELETE')
      CLOSE (3, STATUS = 'DELETE')
      CLOSE (4, STATUS = 'KEEP')
      CLOSE (55, STATUS = 'KEEP')
      WRITE (1, 105) KEYGOOD
      CLOSE (1, STATUS = 'KEEP')
      STOP 'TRANSF3: Completed successfully.'
   99 CALL PMDSTOP   
      END PROGRAM TRANSF3
