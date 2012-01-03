pro RDVEL,iprint=iprint
;***************************************
;device,decomposed=0
;loadct,5
;***************************************
;
; NOTE: To produce jpeg file output, activate the statements between
; the ********** boundaries. In this case, do not use the input
; parameter iprint.
; For normal printable output, deactivate the statements between 
; the ********** boundaries.
; To preserve the color output capabilities, leave the statements
; between the top two ********* active. To produce black--white normal
; output, deactivate the device... and loadct,5 statements, and
; remove the background=, and color= statements in the plot statements.
;
set_viewport, 0.15, 0.85, 0.10, 0.85
sz=1.0
;
if keyword_set(iprint) then begin
        set_plot,'ps'
;     device,/land
;      device,/encapsulated, filename='Walker1954V.eps'
;      device,/encapsulated, filename='FUVwd05.eps'
endif
;blue=50
;red=120
;white=255
;black=0
;
;*****************************************
;x=1600
;y=1200
;window,0,xs=x,ys=y,/pixmap
;sz=(1.8*x)/!d.x_size
;!x.margin=10
;!y.margin=7
;*****************************************
;
;file1='d:\cvsystems\WX LMi\FUV.dat'
twpi=6.2831853
sz=1.0
ph=0.00
xrge=[0.0,1.0]
yrge=[-250.0,250.0]
;
GET_LUN, UNIT
J=0
OPENR, UNIT, '~/binsyn/oa/RDVL2.DAT'
READF, UNIT, FORMAT='(A80)', BFR
print, BFR
WHILE(NOT EOF(UNIT)) DO BEGIN
READF, UNIT, FORMAT='(3F12.7)',A,B,C
J=J+1
ENDWHILE
FREE_LUN, UNIT
x=FLTARR(J)
y=x
z=x
OPENR, UNIT, '~/binsyn/oa/RDVL2.DAT'
READF, UNIT, FORMAT='(A80)', BFR
FOR K=0,J-1 DO BEGIN
READF, UNIT, FORMAT='(3F12.7)',A,B,C
x(K)=A
y(K)=B
z(K)=C
ENDFOR
FREE_LUN, UNIT
plot,x,y,$
;title='Component radial velocities',thick=sz*1.0,$
charsize=sz*1.3,charthick=sz*1.7,$
xtit='Orbital Phase',$ 
;ytit='Normalized Magnitude',$
ytit='Radial velocities (km/s)',$
xrange=xrge,$
yrange=yrge,$
xstyle=1,ystyle=1,/NODATA
oplot,x/twpi+ph,y
;oploterr,x+ph,y/cf1-zp,e
;background=blue,color=white
oplot,x/twpi+ph,-z
;
if keyword_set(iprint) then begin
        device,/close
;        spawn,'print idl.ps'
        set_plot,'x'
endif
;
;*****************************************
;im=tvrd(true=1)
;wdelete
;write_jpeg,'ltcva.jpg',im,true=1,quality=100
;*****************************************
;
end