pro pltb,iprint=iprint
;
;set_viewport, 0.15, 0.80, 0.05, 0.95
;set_viewport, 0.35, 0.65, 0.05, 0.46538
set_viewport, 0.35, 0.65, 0.50, 0.91538
;!P.POSITION=[0.15,0.15,.75,1.0]
if keyword_set (iprint) then begin
        set_plot,'ps'
;        device,/land
	device,/encapsulated, filename='pltb.eps'
endif
;
plt,'~/plotfiles/PLTB2.DAT',x,y,ncol=4,nrows=100000,delbeg=1,/nopl
;
plot,x,y,psym=7,symsize=0.005,THICK=0.5,xstyle=1,ystyle=1,$
;xrange=[-0.24,-0.19],yrange=[-0.05,0.00]
;xrange=[0.11,0.16],yrange=[0.04,0.09]
;xrange=[-0.025,0.025],yrange=[0.05,0.10]
;
;xrange=[-0.1,0.2],yrange=[-0.1,0.2]
xrange=[-0.3,0.3],yrange=[-0.3,0.3],/NODATA
oplot,x,y,psym=7,symsize=0.08,THICK=1.0
;
get_lun, l1
openr,l1,'~/plotfiles/PLTB4.DAT'
if not eof(l1) then begin
        close,l1
        plt,'~/plotfiles/PLTB4.DAT',x1,y1,/nopl
        oplot,x1,y1,linestyle=0,thick=1.0
endif
free_lun,l1
get_lun, l2
openr,l2,'~/plotfiles/PLTB5.DAT'
if not eof(l2) then begin
        close,l2
        plt,'~/plotfiles/PLTB5.DAT',x2,y2,/nopl
        oplot,x2,y2,psym=7,symsize=0.2
endif
free_lun,l2
get_lun, l3
openr,l3,'~/plotfiles/PLTB6.DAT'
if not eof(l3) then begin
        close,l3
        plt,'~/plotfiles/PLTB6.DAT',x3,y3,/nopl
        oplot,x3,y3,psym=3,symsize=0.1,thick=0.5
		polyfill,x3,y3,$
		line_fill=1,orientation=60,spacing=0.01,thick=1.0,linestyle=0
endif
free_lun,l3
get_lun, l4
openr,l4,'~/plotfiles/PLTB7.DAT'
if not eof(l4) then begin
        close,l4
        plt,'~/plotfiles/PLTB7.DAT',x4,y4,/nopl
        oplot,x4,y4,psym=3,symsize=0.1,thick=0.5
		polyfill,x4,y4,$
		line_fill=1,orientation=60,spacing=0.01,thick=1.0,linestyle=0
endif
free_lun,l4
get_lun, l5
openr,l5,'~/plotfiles/PLTB8.DAT'
if not eof(l5) then begin
        close,l5
        plt,'~/plotfiles/PLTB8.DAT',x4,y4,/nopl
        oplot,x4,y4,psym=3,symsize=0.1,thick=0.5
		polyfill,x4,y4,$
		line_fill=1,orientation=60,spacing=0.01,thick=1.0,linestyle=0
endif
free_lun,l5
get_lun, l6
openr,l6,'~/plotfiles/PLTB9.DAT'
if not eof(l6) then begin
        close,l6
        plt,'~/plotfiles/PLTB9.DAT',x4,y4,/nopl
        oplot,x4,y4,psym=3,symsize=0.1,thick=0.5
		polyfill,x4,y4,$
		line_fill=1,orientation=60,spacing=0.01,thick=1.0,linestyle=0
endif
free_lun,l6
get_lun, l7
openr,l7,'~/plotfiles/PLTB10.DAT'
if not eof(l7) then begin
        close,l7
        plt,'~/plotfiles/PLTB10.DAT',x5,y5,/nopl
        oplot,x5,y5,linestyle=0,thick=1.0
endif
free_lun,l7
get_lun, l8
openr,l8,'~/plotfiles/PLTB11.DAT'
if not eof(l8) then begin
        close,l8
        plt,'~/plotfiles/PLTB11.DAT',x6,y6,/nopl
        oplot,x6,y6,linestyle=0,thick=1.0
endif
free_lun,l8
get_lun, l9
openr,l9,'~/plotfiles/PLTB12.DAT'
if not eof(l9) then begin
        close,l9
        plt,'~/plotfiles/PLTB12.DAT',x7,y7,/nopl
        oplot,x7,y7,linestyle=0,thick=1.0
endif
free_lun,19
get_lun, l10
openr,l10,'~/plotfiles/PLTB13.DAT'
if not eof(l10) then begin
        close,l10
        plt,'~/plotfiles/PLTB13.DAT',x8,y8,/nopl
        oplot,x8,y8,linestyle=0,thick=1.0
endif
free_lun,l10
get_lun, l11
openr,l11,'~/plotfiles/PLTB14.DAT'
if not eof(l11) then begin
        close,l11
        plt,'~/plotfiles/PLTB14.DAT',x9,y9,/nopl
        oplot,x9,y9,linestyle=0,thick=1.0
endif
free_lun,l11
get_lun, l12
openr,l12,'~/plotfiles/PLTB15.DAT'
if not eof(l12) then begin
        close,l12
        plt,'~/plotfiles/PLTB15.DAT',x10,y10,/nopl
        oplot,x10,y10,linestyle=0,thick=1.0
endif
free_lun,l12
get_lun, l13
openr,l13,'~/plotfiles/PGDX8.DAT'
if not eof(l13) then begin
        close,l13
        plt,'~/plotfiles/PGDX8.DAT',x10,y10,/nopl
        oplot,x10,y10,linestyle=0,thick=3.0
endif
free_lun,l13
close,l13
get_lun, l14
;openr,l14,'~/plotfiles/SCRATCHH.DAT'
;if not eof(l14) then begin
;        close,l14
;        plt,'~/plotfiles/SCRATCHH.DAT',x10,y10,/nopl
;        oplot,x10,y10,linestyle=0,thick=2.5
;endif
free_lun,l14
close,l14
get_lun, l15
openr,l15,'~/plotfiles/PGBX8.DAT'
if not eof(l15) then begin
        close,l15
        plt,'~/plotfiles/PGBX8.DAT',x10,y10,delbeg=1,/nopl
;        oplot,x10,y10,linestyle=0,thick=2.5
endif
free_lun,l15
close,l15
get_lun, l16
openr,l16,'~/plotfiles/PGBX9.DAT'
if not eof(l16) then begin
        close,l16
        plt,'~/plotfiles/PGBX9.DAT',x10,y10,delbeg=1,/nopl
;        oplot,x10,y10,linestyle=0,thick=2.5
endif
free_lun,l16
close,l16
;
;get_lun,l13
;openr,l13,'d:\oa2\pltb.out'
;if not eof(l13) then begin
;	close,l13
;	plt,'d:\oa2\pltb.out',x11,y11,/nopl
;	oplot,x11,y11,linestyle=0,thick=3.0
;endif
;free_lun,l13
;
if keyword_set (iprint) then begin
        device,/close
;        spawn,'print idl.ps'
;        spawn,'copy idl.ps pltb.ps'
        set_plot,'x'
endif
set_viewport
return
end
