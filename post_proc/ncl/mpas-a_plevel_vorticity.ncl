;=============================================================================================
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
;load "./gsn_csm.ncl"
;=============================================================================================

; TEMPLATE TO PLOT A MAP OF 850 hPa, 500 hPa, and 200 hPa RELATIVE VORTICITY:

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
res@sfXArray  = f->lonVertex * r2d
res@sfYArray  = f->latVertex * r2d
;sres@sfXArray = f->lonCell * r2d
;sres@sfYArray = f->latCell * r2d


;gets dimensions of 2d array of 850 hPa relative vorticity:
dims = dimsizes(f->vorticity_850hPa)
nTime     = dims(0)   ; number of ouput time.
nVertices = dims(1)   ; number of vertices.
print(" ")
print("nTime = "+nTime +"   nVertices = "+nVertices)

vertexDegree = 3
areaTriangle      = f->areaTriangle
kiteAreasOnVertex = f->kiteAreasOnVertex
cellsOnVertex     = f->cellsOnVertex

;interpolation terrain from cell centers to cell vertices:
terrain = f->zgrid(:,0)
terrain_atVertex = new((/nVertices/),typeof(terrain))
do iVert = 0, nVertices-1
   terrain_atVertex(iVert) = 0.
   do iVertD = 0, vertexDegree-1
      terrain_atVertex(iVert) = terrain_atVertex(iVert) \
              + kiteAreasOnVertex(iVert,iVertD)*terrain(cellsOnVertex(iVert,iVertD)-1)
   end do
   terrain_atVertex(iVert) = terrain_atVertex(iVert)/areaTriangle(iVert)
end do

;850 hPa RELATIVE VORTICTY:
wks = gsn_open_wks("pdf","vorticity850hPa.mesh_40962")
gsn_define_colormap(wks,"BlueRed")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  = -28.
res@cnMaxLevelValF  =  28.
res@cnLevelSpacingF =   4.
res@gsnLeftString   = "850 hPa RELATIVE VORTICITY"
res@gsnRightString  = "[x1.e-05 s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   height = f->height_850hPa(iTime,:)
   height_atVertex = new((/nVertices/),typeof(height))
   do iVert = 0, nVertices-1
      height_atVertex(iVert) = 0.
      do iVertD = 0, vertexDegree-1
         height_atVertex(iVert) = height_atVertex(iVert) \
                 + kiteAreasOnVertex(iVert,iVertD)*height(cellsOnVertex(iVert,iVertD)-1)
      end do
      height_atVertex(iVert) = height_atVertex(iVert)/areaTriangle(iVert)
   end do
   height@_FillValue = default_fillvalue(typeof(height))
   height = where(height-terrain.gt.0.,height,height@_FillValue)

   vorticity = f->vorticity_850hPa(iTime,:) * 1.e5
   vorticity@_FillValue = default_fillvalue(typeof(vorticity))
   vorticity = where(height_atVertex-terrain_atVertex.gt.0.,vorticity,vorticity@_FillValue)
   plot = gsn_csm_contour_map(wks,vorticity,res)

end do

;500 hPa RELATIVE VORTICITY:
wks = gsn_open_wks("pdf","vorticity500hPa.mesh_40962")
gsn_define_colormap(wks,"BlueRed")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  = -24.
res@cnMaxLevelValF  =  20.
res@cnLevelSpacingF =   4.
res@gsnLeftString   = "500 hPa RELATIVE VORTICITY"
res@gsnRightString  = "[x1.e-05 s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime

   height = f->height_500hPa(iTime,:)
   height_atVertex = new((/nVertices/),typeof(height))
   do iVert = 0, nVertices-1
      height_atVertex(iVert) = 0.
      do iVertD = 0, vertexDegree-1
         height_atVertex(iVert) = height_atVertex(iVert) \
                 + kiteAreasOnVertex(iVert,iVertD)*height(cellsOnVertex(iVert,iVertD)-1)
      end do
      height_atVertex(iVert) = height_atVertex(iVert)/areaTriangle(iVert)
   end do
   height@_FillValue = default_fillvalue(typeof(height))
   height = where(height-terrain.gt.0.,height,height@_FillValue)

   vorticity = f->vorticity_500hPa(iTime,:) * 1.e5
   vorticity@_FillValue = default_fillvalue(typeof(vorticity))
   vorticity = where(height_atVertex-terrain_atVertex.gt.0.,vorticity,vorticity@_FillValue)
   plot = gsn_csm_contour_map(wks,vorticity,res)

end do

;200 hPa RELATIVE VORTICITY:
wks = gsn_open_wks("pdf","vorticity200hPa.mesh_40962")
gsn_define_colormap(wks,"BlueRed")

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF  = -20.
res@cnMaxLevelValF  =  20.
res@cnLevelSpacingF =   5.
res@gsnLeftString   = "200 hPa RELATIVE VORTICITY"
res@gsnRightString  = "[x1.e-05 s~S~-1~N~]"
;loop over the number of output in file_ncl:
do iTime = 0,nTime-1

   xtime = chartostring(f->xtime(iTime,0:19))
   res@tiMainString    = "QUASI-UNIFORM MESH 40962 ~C~          "+xtime
   vorticity = f->vorticity_200hPa(iTime,:) * 1.e5
   plot = gsn_csm_contour_map(wks,vorticity,res)

end do

end

;=============================================================================================
