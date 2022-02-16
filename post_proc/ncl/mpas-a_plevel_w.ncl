;=============================================================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;load "./gsn_csm.ncl"
;=============================================================================================

; TEMPLATE TO PLOT A MAP OF 850 hPa, 500 hPa, and 200 hPa VERTICAL VELOCITY:

begin

;SET RESOURCES:
res = True
res@gsnDraw             = True
res@gsnFrame            = True
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


diri = "./"
file_ncl = "x1.40962.output.2005-05-05_00.00.00.nc"
f = addfile(diri+file_ncl,"r")
print(file_ncl)


r2d = 180.0d/(atan(1)*4.0d) ; conversion from radians to degrees. 
res@sfXArray  = f->lonCell * r2d
res@sfYArray  = f->latCell * r2d


;gets dimensions of 2d array of 850 hPa vertical velocity:
dims = dimsizes(f->w_850hPa)
nTime  = dims(0)   ; number of ouput time.
nCells = dims(1)   ; number of cells.
print(" ")
print("nTime = "+nTime +"   nCells = "+nCells)

terrain = f->zgrid(:,0)

;850 hPa VERTICAL VELOCITY:
wks = gsn_open_wks("pdf","verticalvelocity850hPa.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels = (/-0.04,-0.02,0.00,0.01,0.02,0.03,0.04,0.05,0.10,0.12,0.15/)
res@gsnLeftString   = "850 hPa VERTICAL VELOCITY"
res@gsnRightString  = "[m s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   height = f->height_850hPa(iTime,:)
   wvel = f->w_850hPa(iTime,:)
   wvel@_FillValue = default_fillvalue(typeof(wvel))
   wvel = where(height-terrain.gt.0.,wvel,wvel@_FillValue)
   plot = gsn_csm_contour_map(wks,wvel,res)

end do

;500 hPa VERTICAL VELOCITY:
wks = gsn_open_wks("pdf","verticalvelocity500hPa.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels = (/-0.04,-0.02,0.00,0.01,0.02,0.03,0.04,0.05,0.10,0.12,0.15/)
res@gsnLeftString   = "500 hPa VERTICAL VELOCITY"
res@gsnRightString  = "[m s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   height = f->height_500hPa(iTime,:)
   wvel = f->w_500hPa(iTime,:)
   wvel@_FillValue = default_fillvalue(typeof(wvel))
   wvel = where(height-terrain.gt.0.,wvel,wvel@_FillValue)
   plot = gsn_csm_contour_map(wks,wvel,res)

end do

;200 hPa TEMPERATURE AND HEIGHT:
wks = gsn_open_wks("pdf","verticalvelocity200hPa.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels = (/-0.04,-0.02,0.00,0.01,0.02,0.03,0.04,0.05,0.10,0.12,0.15/)
res@gsnLeftString   = "200 hPa VERTICAL VELOCITY"
res@gsnRightString  = "[m s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   wvel = f->w_200hPa(iTime,:)
   plot = gsn_csm_contour_map(wks,wvel,res)

end do

end

;=============================================================================================
