;=============================================================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;load "./gsn_csm.ncl"
;=============================================================================================

; TEMPLATE TO PLOT A MAP OF THE PRECIPITABLE WATER.

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
res@sfXArray = f->lonCell * r2d
res@sfYArray = f->latCell * r2d


;gets dimensions of 2d array of precipitable water:
dims = dimsizes(f->precipw)
nTime  = dims(0)   ; number of ouput time.
nCells = dims(1)   ; number of cells.
print(" ")
print("nTime = "+nTime +"   nCells = "+nCells)


wks = gsn_open_wks("pdf","precipw.mesh_40962")
gsn_define_colormap(wks,"gui_default")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  =  4.
res@cnMaxLevelValF  = 56.
res@cnLevelSpacingF =  4. 
res@gsnLeftString   = "PRECIPITABLE WATER"
res@gsnRightString  = "[kg m~S~-2~N~]"

;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime
   precipw = f->precipw(iTime,:)
   plot = gsn_csm_contour_map(wks,precipw,res)

end do

end

;=============================================================================================
