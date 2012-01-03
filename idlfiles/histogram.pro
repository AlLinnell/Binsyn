pro HISTOGRAM,iprint=iprint
set_viewport, 0.15,0.90,0.1,0.85
;
if keyword_set(iprint) then begin
    set_plot,'ps'
;    device,/land
;    device,/encalsulated,filename='histogram.eps'
endif
;
N=2500
X=FLTARR(51)
Y=X
GET_LUN, UNIT
OPENR, UNIT, '~/plotfiles/HISTO.DAT'
FOR K=0,50 DO BEGIN
READF, UNIT, FORMAT='(I5,I15)',II,JJ
X(K)=II
Y(K)=JJ
ENDFOR
FREE_LUN, UNIT
plot,X,Y,$
charsize=1.3,charthick=1.7,$
title='Histogram of residuals for all observation wavelengths.',$
xtit='Bin number',$
ytit='Number of entries',$
xrange=[1,51],$
yrange=[0,N],$
xstyle=1,ystyle=1,thick=1.0
xsym=[25,25]
ysym=[0,N]
oplot,xsym,ysym,thick=2.0,linestyle=0
if keyword_set(iprint) then begin
    device,/close
    set_plot,'x'
endif
end