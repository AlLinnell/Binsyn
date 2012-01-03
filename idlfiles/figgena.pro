pro figgena,iprint=iprint
device,DECOMPOSED=0
;
;***************************************************************
;***************************************************************
;The following statement produces multiple plots on a page,
;specifically, two across and two vertically.
!P.MULTI=[0,2,2]
;
if keyword_set(iprint) then begin
        set_plot,'ps'
;     device,/land
;        device,/encapsulated,filename='.eps'
endif
;
loadct=0
;  ********************************************************
;  FIRST PLOT
;  ********************************************************

;
file1='d:\cvsystems\uxuma\hst\prism.front.10.spec'
file2='D:\cvsystems\uxuma\hst\prism.back.10.spec'
file3='d:\cvsystems\uxuma\hst\prism.stream.10.spec'
file6='d:\cvsystems\uxuma\intprism10.dat'
file7='d:\cvsystems\uxuma\intprism10K.dat'
file8='d:\cvsystems\uxuma\intprismzeroK.dat'
angstrom = '!6!sA!r!u!9 %!6!n!x'
;
rf1=1.9e-3
add2=0.00
rf2=1.9e-3
dv3=1.0
rf3=1.9e-3
add3=0.00
rf4=8.5e28
add4=0.00
rf6=3.178675e15
rf7=2.1388343e16
rf8=2.1388343E16
dv8=1.0
dlam=0.0
;
plt,file1,xv,yv,nocol=2,delbeg=0,nrows=300000,/nopl
yv=yv/rf1
plot,xv,yv,charth=2,thick=1.0,$
xthick=2.,ythick=2.,xchar=1.0,ychar=1.0,$
;title='Figure 4e',$
xtit='!6Wavelength ('+angstrom+') !6',$
; Note: The !6 at the end of the previous line sets the axis numbering
; to vector font 6.
;xtit='Wavelength ('+string("305B)+')',$
;ytit='!6Flux (erg s!E-1!N cm!E-2!N' +angstrom+'!E-1!N)',$
;ytit='!6Flux (10!E-12!N erg s!E-1!N cm!E-2!N'+angstrom+'!E-1!N)',$
ytit='!6Intensity !C(10!E-3!N erg s!E-1!N cm!E-2!N hz!E-1!N sr!E-1!N)',$
;ytit='Rectified flux ratio',$
charsize=1.00,$
background=white,color=black,$
yrange=[0.0,1.5],ystyle=1,xrange=[900,10000],xstyle=1,/NODATA
;
; **exchange for color**
oplot,xv,yv,thick=2.5,linestyle=0,color=150
;
;errplot,xv,yv-0.04,yv+0.04,width=0.02,thick=1.0
;
plt,file2,x,y,nrows=100000,delbeg=0,/nopl
y=y/rf2
oplot,x+dlam,y+add2,thick=1.3,linestyle=0,color=120
;
;errplot,x,y-0.07,y+0.07,width=0.02,thick=1.0
;
plt,file3,xb,yb,nrows=100000,delbeg=0,/nopl
yb=yb/rf3
oplot,xb+dlam,yb+add2,thick=1.3,linestyle=0
;
;plt,file4,xc,yc,nrows=100000,delbeg=0,/nopl
;yc=yc/rf4
;oplot,xc+dlam,yc+add2,thick=1.3,linestyle=0
;
;plt,file5,xd,yd,nrows=100000,delbeg=0,/nopl
;yd=yd/rf5
;oplot,xd+dlam,yd+add2,thick=1.3,linestyle=0
;
plt,file6,xe,ye,nrows=300000,/nopl
ye=ye/rf6
;
oplot,xe,ye,thick=1.5,linestyle=0,color=black
;
;plt,file7,xf,yf,delbeg=1,nrows=300000,/nopl
;yf=yf/rf7
;
;oplot,xf,yf,thick=4.0,linestyle=1,color=black
;
;plt,file8,xg,yg,nrows=300000,/nopl
;yg=yg/rf8
;
;oplot,xg+dlam,yg,thick=1.0,linestyle=0,color=black
;
;xsym=[6347.1,6347.1]
;ysym=[0.71,0.74]
;oplot,xsym,ysym,thick=2.0,linestyle=0
xrf=8500.
yrf=1.0
xyouts,xrf,yrf,'a',orientation=0,size=1.5,charthick=1.5
xrf=6000.
yrf=1.0
xyouts,xrf,yrf,'0.10',orientation=0,size=1.5,charthick=1.5
;***********************************************************
; SECOND PLOT
;***********************************************************
file1='d:\cvsystems\uxuma\hst\prism.front.15.spec'
file2='D:\cvsystems\uxuma\hst\prism.back.15.spec'
file3='d:\cvsystems\uxuma\hst\prism.stream.15.spec'
file6='d:\cvsystems\uxuma\intprism15.dat'
file7='d:\cvsystems\uxuma\RutB.dat'
rf1=1.9e-3
rf2=1.9e-3
rf3=1.9e-3
rf6=5.0250e15
rf7=1.0e-3
plt,file1,xv,yv,nocol=2,delbeg=0,nrows=300000,/nopl
yv=yv/rf1
plot,xv,yv,charth=2,thick=1.0,$
xthick=2.,ythick=2.,xchar=1.0,ychar=1.0,$
;title='Figure 4f',$
xtit='!6Wavelength ('+angstrom+') !6',$
; Note: The !6 at the end of the previous line sets the axis numbering
; to vector font 6.
;xtit='Wavelength ('+string("305B)+')',$
;ytit='!6Flux (erg s!E-1!N cm!E-2!N' +angstrom+'!E-1!N)',$
;ytit='!6Flux (10!E-12!N erg s!E-1!N cm!E-2!N'+angstrom+'!E-1!N)',$
ytit='!6Intensity !C(10!E-3!N erg s!E-1!N cm!E-2!N hz!E-1!N sr!E-1!N)',$
;ytit='Rectified flux ratio',$
charsize=1.00,$
background=white,color=black,$
yrange=[0.0,0.8],ystyle=1,xrange=[900,10000],xstyle=1,/NODATA
;
; **exchange for color**
oplot,xv,yv,thick=2.5,linestyle=0,color=150
;
;
plt,file2,x,y,nrows=100000,delbeg=0,/nopl
y=y/rf2
oplot,x+dlam,y+add2,thick=1.3,linestyle=0,color=120
;errplot,x,y-0.07,y+0.07,width=0.02,thick=4.0
;
plt,file3,x,y,nrows=100000,delbeg=0,/nopl
y=y/rf3
oplot,x+dlam,y+add2,thick=1.3,linestyle=0
;
plt,file6,xe,ye,nrows=300000,/nopl
ye=ye/rf6
;
oplot,xe,ye,thick=1.5,linestyle=0,color=black
;
plt,file7,xf,yf,delbeg=1,nrows=300000,/nopl
yf=yf/rf7
;
oplot,xf,yf,thick=4.0,linestyle=1,color=black
;
;plt,file8,xg,yg,nrows=300000,/nopl
;yg=yg/rf8
;
;oplot,xg+dlam,yg,thick=1.0,linestyle=0,color=black
xrf=8500.
yrf=0.6
xyouts,xrf,yrf,'b',orientation=0,size=1.5,charthick=1.5
xrf=6000.
yrf=0.6
xyouts,xrf,yrf,'0.15',orientation=0,size=1.5,charthick=1.5
;*********************************************************
; THIRD PLOT
;*********************************************************
file1='d:\cvsystems\uxuma\hst\prism.front.20.spec'
file2='D:\cvsystems\uxuma\hst\prism.back.20.spec'
file3='d:\cvsystems\uxuma\hst\prism.stream.20.spec'
file6='d:\cvsystems\uxuma\intprism20.dat'
;file7='d:\cvsystems\uxuma\intprism20K.dat'
;file8='d:\cvsystems\uxuma\intprismzeroK.dat'
rf1=1.9e-3
rf2=1.9e-3
rf3=1.9e-3
rf6=6.40985e15
rf7=2.1388343E16
rf8=2.1388343E16
dv8=1.0
plt,file1,xv,yv,nocol=2,delbeg=0,nrows=300000,/nopl
yv=yv/rf1
plot,xv,yv,charth=2,thick=1.0,$
xthick=2.,ythick=2.,xchar=1.0,ychar=1.0,$
;title='Figure 4g',$
xtit='!6Wavelength ('+angstrom+') !6',$
; Note: The !6 at the end of the previous line sets the axis numbering
; to vector font 6.
;xtit='Wavelength ('+string("305B)+')',$
;ytit='!6Flux (erg s!E-1!N cm!E-2!N' +angstrom+'!E-1!N)',$
;ytit='!6Flux (10!E-12!N erg s!E-1!N cm!E-2!N'+angstrom+'!E-1!N)',$
ytit='!6Intensity !C(10!E-3!N erg s!E-1!N cm!E-2!N hz!E-1!N sr!E-1!N)',$
;ytit='Rectified flux ratio',$
charsize=1.00,$
background=white,color=black,$
yrange=[0.0,0.5],ystyle=1,xrange=[900,10000],xstyle=1,/NODATA
;
oplot,xv,yv,thick=2.5,linestyle=0,color=150
;
plt,file2,x,y,nrows=100000,delbeg=0,/nopl
y=y/rf2
;
oplot,x,y,thick=1.3,linestyle=0,color=120
;errplot,x,y-0.07,y+0.07,width=0.02,thick=4.0
;
plt,file3,xa,ya,nrows=100000,delbeg=0,/nopl
ya=ya/rf3
;
oplot,xa,ya,thick=1.3,linestyle=0
;
plt,file6,xe,ye,nrows=300000,/nopl
ye=ye/rf6
;
oplot,xe,ye,thick=1.5,linestyle=0,color=black
;
;plt,file7,xf,yf,nrows=300000,/nopl
;yf=yf/rf7
;
;oplot,xf,yf,thick=1.5,linestyle=0,color=black
;
;plt,file8,xg,yg,nrows=300000,/nopl
;yg=yg/rf8
;
;oplot,xg,yg,thick=1.0,linestyle=0,color=black
xrf=8500.
yrf=0.4
xyouts,xrf,yrf,'c',orientation=0,size=1.5,charthick=1.5
xrf=6000.
yrf=0.4
xyouts,xrf,yrf,'0.20',orientation=0,size=1.5,charthick=1.5
;*********************************************************
;*********************************************************
;
;*********************************************************
; FOURTH PLOT
;*********************************************************
file1='d:\cvsystems\uxuma\hst\prism.front.25.spec'
file2='D:\cvsystems\uxuma\hst\prism.back.25.spec'
file3='d:\cvsystems\uxuma\hst\prism.stream.25.spec'
file6='d:\cvsystems\uxuma\intprism25.dat'
file7='d:\cvsystems\uxuma\RutC.dat'
;file8='d:\cvsystems\uxuma\intprismzeroK.dat'
rf1=1.9e-3
rf2=1.9e-3
rf3=1.9e-3
rf6=8.71785e15
rf7=1.0e-3
rf8=2.1388343E16
dv8=1.0
plt,file1,xv,yv,nocol=2,delbeg=0,nrows=300000,/nopl
yv=yv/rf1
plot,xv,yv,charth=2,thick=1.0,$
xthick=2.,ythick=2.,xchar=1.0,ychar=1.0,$
;title='Figure 4h',$
xtit='!6Wavelength ('+angstrom+') !6',$
; Note: The !6 at the end of the previous line sets the axis numbering
; to vector font 6.
;xtit='Wavelength ('+string("305B)+')',$
;ytit='!6Flux (erg s!E-1!N cm!E-2!N' +angstrom+'!E-1!N)',$
;ytit='!6Flux (10!E-12!N erg s!E-1!N cm!E-2!N'+angstrom+'!E-1!N)',$
ytit='!6Intensity !C(10!E-3!N erg s!E-1!N cm!E-2!N hz!E-1!N sr!E-1!N)',$
;ytit='Rectified flux ratio',$
charsize=1.00,$
background=white,color=black,$
yrange=[0.0,0.30],ystyle=1,xrange=[900,10000],xstyle=1,/NODATA
;
oplot,xv,yv,thick=2.5,linestyle=0,color=150
;
plt,file2,x,y,nrows=100000,delbeg=0,/nopl
y=y/rf2
;
oplot,x,y,thick=1.3,linestyle=0,color=120
;errplot,x,y-0.07,y+0.07,width=0.02,thick=4.0
;
plt,file3,xa,ya,nrows=100000,delbeg=0,/nopl
ya=ya/rf3
;
oplot,xa,ya,thick=1.3,linestyle=0
;
plt,file6,xe,ye,nrows=300000,/nopl
ye=ye/rf6
;
oplot,xe,ye,thick=1.5,linestyle=0,color=black
;
plt,file7,xf,yf,delbeg=1,nrows=300000,/nopl
yf=yf/rf7
;
oplot,xf,yf,thick=4.0,linestyle=1,color=black
;
;plt,file8,xg,yg,nrows=300000,/nopl
;yg=yg/rf8
;
;oplot,xg,yg,thick=1.0,linestyle=0,color=black
xrf=2000.
yrf=0.25
xyouts,xrf,yrf,'d',orientation=0,size=1.5,charthick=1.5
xrf=1900.
yrf=0.20
xyouts,xrf,yrf,'0.25',orientation=0,size=1.5,charthick=1.5
;*********************************************************
;*********************************************************
; If P.MULTI is set above, return it to one plot per page
!P.MULTI=0
if keyword_set(iprint) then begin
        device,/close
;        spawn,'print idl.ps'
        set_plot,'win'
endif
set_viewport
;
return
end
