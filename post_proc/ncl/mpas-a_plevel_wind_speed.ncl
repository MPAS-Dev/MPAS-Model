;=============================================================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;load "./gsn_csm.ncl"
;=============================================================================================

; TEMPLATE TO PLOT A MAP OF 850 hPa, 500 hPa, and 200 hPa WIND SPEED AND HEIGHT:

begin

;SET RESOURCES:
res = True
res@gsnDraw             = False
res@gsnFrame            = False
res@gsnMaximize         = False
res@gsnSpreadColors     = True

res@mpProjection        = "CylindricalEquidistant"
res@mpDataBaseVersion   = "MediumRes" 
res@mpCenterLatF        = 0.
res@mpCenterLonF        = 0.

res@cnFillMode    = "AreaFill"
res@cnFillOn      = True
res@cnLinesOn     = False
res@cnInfoLabelOn = False

res@mpMinLatF  =  -90.
res@mpMaxLatF  =   90.
res@mpMinLonF  = -180.
res@mpMaxLonF  =  180.

sres = True
sres@gsnDraw   = False
sres@gsnFrame  = False
sres@cnFillOn  = False
sres@cnLinesOn = True
sres@cnLineLabelsOn = True
sres@cnLineThicknessF = 2.6
sres@cnLineLabelFontHeightF = 0.009
sres@cnInfoLabelFontHeightF = 0.01
sres@cnLineLabelPlacementMode = "Computed"
sres@cnLineLabelDensityF = 0.80


diri = "./"
file_ncl = "x1.40962.output.2005-05-05_00.00.00.nc"
f = addfile(diri+file_ncl,"r")
print(file_ncl)


r2d = 180.0d/(atan(1)*4.0d) ; conversion from radians to degrees. 
res@sfXArray  = f->lonCell * r2d
res@sfYArray  = f->latCell * r2d
sres@sfXArray = f->lonCell * r2d
sres@sfYArray = f->latCell * r2d


;gets dimensions of 2d array of 850 hPa zonal wind speed at cell centers:
dims = dimsizes(f->uzonal_850hPa)
nTime  = dims(0)   ; number of ouput time.
nCells = dims(1)   ; number of cells.
print(" ")
print("nTime = "+nTime +"   nCells = "+nCells)

terrain = f->zgrid(:,0)

;850 hPa WIND SPEED AND HEIGHT:
wks = gsn_open_wks("pdf","windspeed850hPa_withGeop.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  =  2.
res@cnMaxLevelValF  = 32.
res@cnLevelSpacingF =  2.
res@gsnLeftString   = "850 hPa WIND SPEED"
res@gsnRightString  = "[m s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   height = f->height_850hPa(iTime,:)
   height@_FillValue = default_fillvalue(typeof(height))
   height = where(height-terrain.gt.0.,height,height@_FillValue)

   wind = sqrt(f->uzonal_850hPa(iTime,:)^2 + f->umeridional_850hPa(iTime,:)^2)
   wind@_FillValue = default_fillvalue(typeof(wind))
   wind = where(height-terrain.gt.0.,wind,wind@_FillValue)
   plot = gsn_csm_contour_map(wks,wind,res)

   plot_ov = gsn_csm_contour(wks,height,sres)
   overlay(plot,plot_ov)
   draw(plot)
   frame(wks)

end do

;500 hPa WIND SPEED AND HEIGHT:
wks = gsn_open_wks("pdf","windspeed500hPa_withGeop.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  =  5.
res@cnMaxLevelValF  = 50.
res@cnLevelSpacingF =  5.
res@gsnLeftString   = "500 hPa WIND SPEED"
res@gsnRightString  = "[m s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   height = f->height_500hPa(iTime,:)
   height@_FillValue = default_fillvalue(typeof(height))
   height = where(height-terrain.gt.0.,height,height@_FillValue)

   wind = sqrt(f->uzonal_500hPa(iTime,:)^2 + f->umeridional_500hPa(iTime,:)^2)
   wind@_FillValue = default_fillvalue(typeof(wind))
   wind = where(height-terrain.gt.0.,wind,wind@_FillValue)
   plot = gsn_csm_contour_map(wks,wind,res)

   plot_ov = gsn_csm_contour(wks,height,sres)
   overlay(plot,plot_ov)
   draw(plot)
   frame(wks)

end do

;200 hPa WIND SPEED AND HEIGHT:
wks = gsn_open_wks("pdf","windspeed200hPa_withGeop.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  =  5.
res@cnMaxLevelValF  = 70.
res@cnLevelSpacingF =  5.
res@gsnLeftString   = "200 hPa WIND SPEED"
res@gsnRightString  = "[m s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime
   wind = sqrt(f->uzonal_200hPa(iTime,:)^2 + f->umeridional_200hPa(iTime,:)^2)
   plot = gsn_csm_contour_map(wks,wind,res)

   height = f->height_200hPa(iTime,:)
   plot_ov = gsn_csm_contour(wks,height,sres)
   overlay(plot,plot_ov)
   draw(plot)
   frame(wks)

end do

end

;=============================================================================================
