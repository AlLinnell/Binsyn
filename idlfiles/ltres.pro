pro ltres,iprint=iprint
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
angstrom = '!6!sA!r!u!9 %!6!n!x'
;
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
file1='~/binsyn/oa/COL1.DAT'
sz=1.0
ph=0.00
xrge=[-0.30,0.70]
yrge=[-0.03,0.02]
;
plt,file1,x,y,$
ncol=2,xcol=2,ycol=1,delbeg=1,noplt=1
;a=findgen(16) * (!pi*2/16.)
;usersym, cos(a),sin(a),/fill
;
;
plot,x,y,$
;title='Model fit to white light observations',thick=sz*1.0,$
charsize=sz*1.3,charthick=sz*1.7,$
xtit='Orbital Phase',$ 
;ytit='Normalized Magnitude',$
ytit='Normalized Light Residuals',$
xrange=xrge,$
yrange=yrge,$
xstyle=1,ystyle=1,/NODATA
oplot,x+ph,y
;oploterr,x+ph,y/cf1-zp,e
;background=blue,color=white
;
;xa=fltarr(2)
;ya=xa
;xa=[0.5,0.5]
;ya=[0.5,0.6]
;oplot,xa,ya,thick=sz*2.0
;xb=xa & yb=ya
;xb=[0.47,0.53]
;yb=[0.6,0.6]
;oplot,xb,yb,thick=sz*2.0
;xc=xa & yc=ya
;xc=[0.47,0.53]
;yc=[0.5,0.5]
;oplot,xc,yc,thick=sz*2.0
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