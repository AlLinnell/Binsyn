pro onespa,iprint=iprint
set_viewport,0.10,0.9,0.10,0.90
if keyword_set(iprint) then begin
        set_plot,'ps'
;        DEVICE, FILENAME='idl.ps', /COLOR
;        device,/land
;		device,/encapsulated, filename='fig1.eps'
endif
;
angstrom = '!6!sA!r!u!9 %!6!n!x'
;
;file='~/cvsystems/RWSex/outburst/r0900m3p0em1108.7'
;file='~/cvsystems/RWSex/newmod/r4500ir08.7'
file='~/binsyn/systems/Test8/T7000g45n08.7'
;file='~binsyn/systems/Test8/BBT9790.DAT'
;file='~/binsyn/synspadd/SMad0830.001'
;file='~/binsyn/oa/SYSC2.DAT'
;file='~/plotfiles/SYSSM.DAT'
;file='~/plotfiles/SYSFC.DAT'
;file='~/marcs_package/t4p0kcr.dat'
;file='~/Hubeny/synspec49/fort.7'
;file='~/binsyn/systems/Test8/SYSSM.DAT'
;file='~/modatm/t4000g40ltsm.7'
;file='~/modatm/Kurucz/flux/T8000G40.DAT'
rf=1.0E6
xrange=[950.0,3000.0]
yrange=[0.0,6.0]
plt,file,xv,yv,nocol=2,$
xcol=1,ycol=2,$
/nopl,nrows=5000000
if (rf ne 0.0) then yv=yv/rf
plot,xv,yv,$
xr=xrange,/xsty,xthick=2,$ 
yr=yrange,/ysty,ythick=2,$
xtit='Wavelength ('+angstrom+')',$
;xtit='Wavelength =' + STRING("305B),$
;ytit='Flux',$
;ytit='!6Flux (10!E-17!N erg s!E-1!N cm!E-2!N'+angstrom+'!E-1!N)',$
ytit='!6Flux (10!E6!N erg s!E-1!N cm!E-2!N'+angstrom+'!E-1!N)',$
;ytit='Transmission',$
chars=1.0,charth=2, /NODATA
oplot, xv,yv,thick=1.0
;xtit='!17Wavelength [A]!X',$
;ytit='!17Flux!X',chars=1.5,charth=2
;
if keyword_set(iprint) then begin
        device,/close
;        spawn,'print idl.ps'
;        spawn,'copy idl.ps fig9.ps'
        set_plot,'x'
endif
set_viewport
;
return
end
