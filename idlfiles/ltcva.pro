pro ltcva,iprint=iprint
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
set_viewport, 0.10, 0.85, 0.10, 0.55
sz=1.0
;
if keyword_set(iprint) then begin
        set_plot,'ps'
;     device,/land
      device,/encapsulated, filename='ltcva.eps'
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
file1='~/binsyn/oa/OBSLC1.DAT'
;file1='d:\uxuma\phot\Bapta.dat'
;file1='d:\uxuma\phot\uxuma1974f5.dat'
;file2='D:\cvsystems\WX LMi\BTHLC.dat'
file2='~/binsyn/oa/THLC1.DAT'
;file2='d:\uxuma\phot\sm1006v.dat'
;file3='d:\cvsystems\sdss J1031+20\NUVlgt.dat'
cf1=0.00
zp=0.0
cf2=0.00
y1dv=1.0
vmp2=1.000
ph=0.0
pha=-1.0
phcor=-0.50
ycor=0.0
;y2dv=9.5214e24
;y2dv=1.50e25
;y2dv=0.205572e30
y2dv=1.0
;ynorm=1.0e16
ynorm=1.00
xrge=[-0.30,0.70]
;yrge=[7.0e-16,1.2e-15]
yrge=[0.60,1.05]
;
plt,file1,x,y,$
ncol=2,xcol=2,ycol=1,delbeg=1,noplt=1
;y=y/23.
;print,x,y
;stop
;a=findgen(16) * (!pi*2/16.)
;usersym, cos(a),sin(a),/fill
;
;
plot,x,y,$
;title='Model fit to white light observations',thick=sz*1.0,$
charsize=sz*1.3,charthick=sz*1.7,$
xtit='Orbital Phase',$ 
;ytit='Normalized Magnitude',$
ytit='Normalized Light',$
;ytit='!6Flux (erg s!E-1!N cm!E-2!N' +angstrom+'!E-1!N)',$
xrange=xrge,$
yrange=yrge,$
xstyle=1,ystyle=1,/NODATA
oplot,x+ph,y/y1dv+cf1,$
thick=sz*1.0,$
;psym=8,$
psym=1,$
;psym=0,$
symsize=sz*0.1
;print,y/y1dv+cf1
;print,x+ph
;print,y+cf1
;oploterr,x+ph,y/cf1-zp,e
;background=blue,color=white
;
plt,file2,x,y,$
ncol=2,xcol=2,ycol=1,delbeg=1,noplt=1
x1=x*vmp2+pha
y1=y/y2dv
;print,y1
y1=y1/ynorm
;print,x1
;print,y1
;
oplot,x1,y1+cf2,thick=sz*1.0
;color=red
;
;plt,file3,x1,y1,$
;ncol=2,xcol=2,ycol=1,delbeg=1,noplt=1
;
;oplot,x1+phcor,y1+ycor,$
;psym=1,$
;symsize=sz*1.5,thick=1.5
;
; Error bar
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
