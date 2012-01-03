pro pltdd2a,ibg,icl,iprint=iprint
set_viewport, 0.15, 0.95, 0.05, 0.75
!P.POSITION=[0.15,0.02,.82,0.92]
;!P.REGION=[0.18,0.07,0.85,0.97]
;
;device, GET_DECOMPOSED=olddc
;device, DECOMPOSED=0
;loadct, 5
;
if keyword_set(iprint) then begin  
        set_plot,'ps'
		device,/inches,xsize=8.4
		device,/inches,ysize=6.0
;       device,/land
;        device,/encapsulated,filename='phase0p65.eps'
endif
;
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
sz=1.0
;blue=50
;red=120
;white=255
;black=0
;icl=white
;
;*****************************************
;x=1600
;y=1200
;window,0,xs=x,ys=y,/pixmap
;sz=(1.8*x)/!d.x_size
;!x.margin=1
;!y.margin=5
;*****************************************
;
;grid of points on both stars, rim, and accretion disk face
plt,'~/plotfiles/PLTDD2.DAT',x,y,/nopl
;
plot,x,y,psym=2,symsize=sz*0.05,thick=sz*1.0,xstyle=1,ystyle=1,$
xrange=[1.0,-1.0],yrange=[-1.0,1.0]
;xrange=[-0.30,0.30],yrange=[-0.40,0.20]
;background=black,color=white
;
;face points by separate rings
;get_lun, l0
;openr,l0,'c:\oa2\face.dat'
;if not eof(l0) then begin
;readf, format = '(10i5)', l0, NRING
;readf, format = '(10i5)', l0, KRING,NSG
;x=fltarr(1)
;y=x
;xa=fltarr(2)
;ya=xa
;xv=fltarr(nsg)
;yv=xv
;xva=xv
;yva=xv
;for k=0,nsg-1 do begin
;readf, format = '(2f12.5)',l0, x,y
;xv(k)=x
;yv(k)=y
;endfor
;oplot,xv,yv,color=ic1,thick=sz*1.0
;xva=xv
;yva=yv
;end of first ring
;
;do remaining rings
;for l=0,nring-2 do begin
;xv=xva
;yv=yva
;readf, format = '(10i5)', l0, KRING, NSG
;for k=0,nsg-1 do begin
;readf, format = '(2f12.5)', l0, x,y
;xva(k)=x
;yva(k)=y
;endfor
;oplot, xva,yva,color=ic1,thick=sz*1.0
;for k=0,nsg-1 do begin
;for m=0,1 do begin
;if (m eq 0) then begin
;xa(m)=xv(k)
;ya(m)=yv(k)
;endif else begin
;xa(m)=xva(k)
;ya(m)=yva(k)
;endelse
;endfor
;oplot, xa,ya,color=ic1,thick=sz*1.0
;endfor
;endfor
;
;

;endif
;close,l0
;free_lun,l0
;
;
;eclipsing star, large theta
get_lun, l1
openr,l1,'~/plotfiles/HZPLT1.DAT'
if not eof(l1) then begin
        close,l1
        plt,'~/plotfiles/HZPLT1.DAT',x1,y1,/nopl
        oplot,x1,y1,thick=sz*1.0,color=icl
endif
close,l1
free_lun,l1
;eclipsed star, large theta
get_lun, l2
openr,l2,'~/plotfiles/HZPLT2.DAT'
if not eof(l2) then begin
        close,l2
        plt,'~/plotfiles/HZPLT2.DAT',x2,y2,/nopl
        oplot,x2,y2,thick=sz*1.0,color=icl
endif
close,l2
free_lun,l2
;one vertical rim horizon
get_lun, l3
openr,l3,'~/plotfiles/HZPLT3.DAT'
if not eof(l3) then begin
        close,l3
        plt,'~/plotfiles/HZPLT3.DAT',x3,y3,/nopl
        oplot,x3,y3,thick=sz*1.0,color=icl
endif
close,l3
free_lun,l3
;other vertical rim horizon
get_lun, l4
openr,l4,'~/plotfiles/HZPLT4.DAT'
if not eof(l4) then begin
        close,l4
        plt,'~/plotfiles/HZPLT4.DAT',x4,y4,/nopl
        oplot,x4,y4,thick=sz*1.0,color=icl
endif
close,l4
free_lun,l4
;eclipsed star, large theta, second segment
get_lun, l5
openr,l5,'~/plotfiles/HZPLT5.DAT'
if not eof(l5) then begin
        close,l5
        plt,'~/plotfiles/HZPLT5.DAT',x5,y5,/nopl
        oplot,x5,y5,thick=sz*1.0,color=icl
endif
close,l5
free_lun,l5
;bottom rim, third segment
get_lun, l6
openr,l6,'~/plotfiles/HZPLT6.DAT'
if not eof(l6) then begin
        close,l6
        plt,'~/plotfiles/HZPLT6.DAT',x6,y6,/nopl
        oplot,x6,y6,thick=sz*1.0,color=icl
endif
close,l6
free_lun,l6
;eclipsing star, small theta, initial branch
get_lun, l7
openr,l7,'~/plotfiles/HZPLT7.DAT'
if not eof(l7) then begin
        close,l7
        plt,'~/plotfiles/HZPLT7.DAT',x7,y7,/nopl
        oplot,x7,y7,thick=sz*1.0,color=icl
endif
close,l7
free_lun,l7
;eclipsing star, small theta, second branch
get_lun, l8
openr,l8,'~/plotfiles/HZPLT8.DAT'
if not eof(l8) then begin
        close,l8
        plt,'~/plotfiles/HZPLT8.DAT',x8,y8,/nopl
        oplot,x8,y8,thick=sz*1.0,color=icl
endif
close,l8
free_lun,l8
;eclipsed star, small theta, initial branch
get_lun, l9
openr,l9,'~/plotfiles/HZPLT9.DAT'
if not eof(l9) then begin
        close,l9
        plt,'~/plotfiles/HZPLT9.DAT',x9,y9,/nopl
        oplot,x9,y9,thick=sz*1.0,color=icl
endif
close,l9
free_lun,l9
;eclipsed star, small theta, second branch
get_lun, l10
openr,l10,'~/plotfiles/HZPLT10.DAT'
if not eof(l10) then begin
        close,l10
        plt,'~/plotfiles/HZPLT10.DAT',x10,y10,/nopl
        oplot,x10,y10,thick=sz*1.0,color=icl
endif
close,l10
free_lun,l10
;top rim, second section
get_lun, l11
openr,l11,'~/plotfiles/HZPLT11.DAT'
if not eof(l11) then begin
        close,l11
        plt,'~/plotfiles/HZPLT11.DAT',x11,y11,/nopl
        oplot,x11,y11,thick=sz*1.0,color=icl
endif
close,l11
free_lun,l11
;bottom rim, second section
get_lun, l12
openr,l12,'~/plotfiles/HZPLT12.DAT'
if not eof(l12) then begin
        close,l12
        plt,'~/plotfiles/HZPLT12.DAT',x12,y12,/nopl
        oplot,x12,y12,thick=sz*1.0,color=icl
endif
close,l12
free_lun,l12
;top rim, first section
get_lun, l13
openr,l13,'~/plotfiles/HZPLT13.DAT'
if not eof(l13) then begin
        close,l13
        plt,'~/plotfiles/HZPLT13.DAT',x13,y13,/nopl
        oplot,x13,y13,thick=sz*1.0,color=icl
endif
close,l13
free_lun,l13
;bottom rim, first section
get_lun, l14
openr,l14,'~/plotfiles/HZPLT14.DAT'
if not eof(l14) then begin
        close,l14
        plt,'~/plotfiles/HZPLT14.DAT',x14,y14,/nopl
        oplot,x14,y14,thick=sz*1.0,color=icl
endif
close,l14
free_lun,l14
;first vertical rim, second section
get_lun, l15
openr,l15,'~/plotfiles/HZPLT15.DAT'
if not eof(l15) then begin
        close,l15
        plt,'~/plotfiles/HZPLT15.DAT',x15,y15,/nopl
        oplot,x15,y15,thick=sz*1.0,color=icl
endif
close,l15
free_lun,l15
;other vertical rim, second section
get_lun, l16
openr,l16,'~/plotfiles/HZPLT16.DAT'
if not eof(l16) then begin
        close,l16
        plt,'~/plotfiles/HZPLT16.DAT',x16,y16,/nopl
        oplot,x16,y16,thick=sz*1.0,color=icl
endif
close,l16
free_lun,l16
;eclipsing star, large theta, second section
get_lun, l17
openr,l17,'~/plotfiles/HZPLT17.DAT'
if not eof(l17) then begin
        close,l17
        plt,'~/plotfiles/HZPLT17.DAT',x17,y17,/noplt
        oplot,x17,y17,thick=sz*1.0,color=icl
endif
close,l17
free_lun,l17
;
;bottom edge of rim, third section
get_lun, l18
openr,l18,'~/plotfiles/HZPLT18.DAT'
if not eof(l18) then begin
        close,l18
        plt,'~/plotfiles/HZPLT18.DAT',x18,y18,/noplt
        oplot,x18,y18,thick=sz*1.0,color=icl
endif
close,l18
free_lun,l18
;
;Input PLTDD8.DAT
;This is the input for the rim bright spot
;
get_lun, l19
;openr,l19,'~/plotfiles/PLTDD8.DAT'
;if not eof(l19) then begin
;        close,l19
;        plt,'~/plotfiles/PLTDD8.DAT',x19,y19,/nopl
;        oplot,x19,y19,psym=2,symsize=sz*0.1, thick=sz*1.0,color=icl
;        polyfill,x19,y19,$
;        line_fill=1,orientation=60,spacing=sz*0.01,thick=sz*1.0,linestyle=0
;endif
;close,l19
;free_lun,l19
;
;get_lun, l20
;openr,l20,'~/plotfiles/PLTB6.DAT'
;if not eof(l20) then begin
;        close,l20
;        plt,'~/plotfiles/PLTB6.DAT',x3,y3,/nopl
;        oplot,x3,y3,psym=3,symsize=sz*0.1, thick=sz*1.0,color=icl
;        polyfill,x3,y3,$
;        line_fill=1,orientation=60,spacing=sz*0.01,thick=sz*1.0,linestyle=0
;endif
;close,l20
;free_lun,l20
;
;get_lun, l21
;openr,l21,'~/plotfiles/PLTB7.DAT'
;if not eof(l21) then begin
;        close,l21
;        plt,'~/plotfiles/PLTB7.DAT',x4,y4,/nopl
;        oplot,x4,y4,psym=3,symsize=sz*0.1, thick=sz*1.0,color=icl
;        polyfill,x4,y4,$
;        line_fill=1,orientation=60,spacing=sz*0.01,thick=sz*1.0,linestyle=0
;endif
;close,l21
;free_lun,l21
;
;get_lun, l22
;openr,l22,'~/plotfiles/PLTB8.DAT'
;if not eof(l22) then begin
;        close,l22
;        plt,'~/plotfiles/PLTB8.DAT',x5,y5,/nopl
;        oplot,x5,y5,psym=3,symsize=sz*0.1, thick=sz*1.0,color=icl
;        polyfill,x5,y5,$
;        line_fill=1,orientation=60,spacing=sz*0.01,thick=sz*1.0,linestyle=0
;endif
;close,l22
;free_lun,l22
;
;get_lun, l23
;openr,l23,'~/plotfiles/PLTB9.DAT'
;if not eof(l23) then begin
;        close,l23
;        plt,'~/plotfiles/PLTB9.DAT',x6,y6,/nopl
;        oplot,x6,y6,psym=3,symsize=sz*0.1, thick=sz*1.0,color=icl
;        polyfill,x6,y6,$
;        line_fill=1,orientation=60,spacing=sz*0.01,thick=sz*1.0,linestyle=0
;endif
;close,l23
;free_lun,l23
;device, DECOMPOSED=olddc
if keyword_set(iprint) then begin
        device,/close
;        spawn,'print idl.ps'
;        spawn,'copy idl.ps pltdd2.ps'
        set_plot,'x'
        set_viewport
endif
;
;*****************************************
;im=tvrd(true=1)
;wdelete
;write_jpeg,'ltcva.jpg',im,true=1,quality=100
;*****************************************

end
