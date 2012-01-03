pro DLDPT1,iprint=iprint
set_viewport, 0.15,0.80,0.1,0.85
;
if keyword_set(iprint) then begin
    set_plot,'ps'
;    device,/land
;    device,/encalsulated,filename='dldpt1.eps'
endif
;
XA=FLTARR(90000)
YA=XA
J=0
GET_LUN, UNIT
OPENR, UNIT, '~/plotfiles/DLDPT1.DAT'
WHILE (NOT EOF(UNIT)) DO BEGIN
READF, UNIT, FORMAT='(2F15.8)',XX,YY
XA(J)=XX
YA(J)=YY
J=J+1
ENDWHILE
FREE_LUN, UNIT
X=FLTARR(J)
Y=X
FOR K=0,J-1 DO BEGIN
X(K)=XA(K)
Y(K)=YA(K)
ENDFOR
plot,X,Y,$
charsize=1.3,charthick=1.7,$
xtit='Orbital Phase',$
ytit='DL/DPT1',$
xrange=[-0.30,0.70],$
yrange=[-0.05,0.15],$
xstyle=1,ystyle=1,thick=1.0
if keyword_set(iprint) then begin
    device,/close
    set_plot,'x'
endif
end