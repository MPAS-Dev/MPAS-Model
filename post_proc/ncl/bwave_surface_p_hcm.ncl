load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  ;
  ; The (lat,lon) the plot is to be centered over
  ;
  cenLat   = 0.0
  cenLon   = 0.0

  ;
  ; Projection to use for plot
  ;
;  projection = "Orthographic"
;  projection = "CylindricalEquidistant"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  r2d = 57.2957795             ; radians to degrees

 ; wks = gsn_open_wks("pdf","surface_p")
 ;wks2 = gsn_open_wks("pdf","surface_vor0_cntl")
 ; setvalues NhlGetWorkspaceObjectId()
 ;   "wsMaximumSize" : 300000000
 ; end setvalues
  wks3 = gsn_open_wks("pdf","850hPa_vor_pxg_10")
  setvalues NhlGetWorkspaceObjectId()
    "wsMaximumSize" : 300000000
  end setvalues
  gsn_define_colormap(wks3,"BlWhRe")
 ; wks4 = gsn_open_wks("pdf","850hpa_temp")
 ; wks5 = gsn_open_wks("pdf","slp_pert")

  f = addfile("HR95HK_hcm/output_655362_HR95HK_hcm.nc","r")
  g = addfile("HR95HK_hcm/diag.0000-01-02_00.00.00.nc","r")
  g2 = addfile("HR95HK_hcm/diag.0000-01-10_00.00.00.nc","r")

 ; f = addfile("output_655362.0000-01-01_00.00.00.nc","r")
 ; g = addfile("diag.0000-01-02_00.00.00.nc","r")
 ; g2 = addfile("diag.0000-01-10_00.00.00.nc","r")

  lonCell   = f->lonCell(:) * r2d
  latCell   = f->latCell(:) * r2d
  lonVertex = f->lonVertex(:) * r2d
  latVertex = f->latVertex(:) * r2d
  lonEdge = f->lonEdge(:) * r2d
  latEdge = f->latEdge(:) * r2d
  verticesOnCell = f->verticesOnCell(:,:)
  alpha = f->angleEdge(:)
  vor850_0=g->vorticity_850hPa
  vor850_9=g2->vorticity_850hPa
 
printVarSummary(vor850_9)

 ; cf1 = f->cf1
 ; cf2 = f->cf2
 ; cf3 = f->cf3


  res                      = True
  res@gsnMaximize          = True
  res@gsnSpreadColors      = True
  res@gsnPaperOrientation  = "Landscape"
  res@sfXArray             = lonCell
  res@sfYArray             = latCell
  ;res@sfXArray             = lonEdge
  ;res@sfYArray             = latEdge

  res@cnFillOn             = True
  res@cnFillMode           = "AreaFill"
;  res@cnLevelSpacingF      =  10.0
  res@cnInfoLabelOn        = True
;  res@cnLevelSelectionMode = "ManualLevels"
;  res@cnMinLevelValF       =  997. 
;  res@cnMaxLevelValF       = 1003.
;  res@cnLevelSpacingF      =    0.5 
  res@cnLinesOn            = True
  res@cnLineLabelsOn       = True
  res@cnInfoLabelOn        = True
  res@cnInfoLabelFontHeightF = 0.007

;  res@lbLabelBarOn         = True
;  res@lbLabelAutoStride    = True
;  res@lbBoxLinesOn         = False

;  res@mpProjection      = projection
;  res@mpDataBaseVersion = "MediumRes"
;  res@mpGridAndLimbDrawOrder = "PreDraw"
;  res@mpGridLineColor   = "Background"
;  res@mpGridAndLimbOn   = True
  res@mpCenterLatF      = cenLat
  res@mpCenterLonF      = cenLon
  res@mpOutlineOn       = False
  res@mpFillOn          = False
  res@mpPerimOn         = False
  res@gsnFrame          = False
  res@mpMinLatF         = 0.

  res@gsnContourNegLineDashPattern = 1
  ;res@cnLineDashPattern = 0

; Info for vorticity plot
 resvor                      = True
  resvor@gsnMaximize          = True
  resvor@gsnSpreadColors      = True
  resvor@gsnPaperOrientation  = "Landscape"
  resvor@sfXArray             = lonVertex
  resvor@sfYArray             = latVertex

  resvor@cnFillOn             = True
;  resvor@cnFillPallete        = "BlWhRe"
;  resvor@cnFillMode           = "AreaFill"
;  resvor@cnLevelSpacingF      =  10.0
  resvor@cnInfoLabelOn        = True
  resvor@cnLevelSelectionMode = "ManualLevels"
  resvor@cnMinLevelValF       =  -1e-06
  resvor@cnMaxLevelValF       =  1e-06
  resvor@cnLevelSpacingF      =   2e-07
  resvor@cnLinesOn            = True
  resvor@cnLineLabelsOn       = False
  resvor@cnInfoLabelOn        = False

  resvor@cnInfoLabelFontHeightF = 0.007

;  resvor@mpProjection      = projection
;  resvor@mpDataBaseVersion = "MediumRes"
;  res@mpGridAndLimbDrawOrder = "PreDraw"
;  res@mpGridLineColor   = "Background"
;  res@mpGridAndLimbOn   = True
;  resvor@mpCenterLatF      = cenLat
;  resvor@mpCenterLonF      = cenLon
;  resvor@mpOutlineOn       = False
;  resvor@mpFillOn          = False
 ; resvor@mpPerimOn         = False
 ; resvor@gsnFrame          = False
  resvor@mpMaxLatF         = -20.
  resvor@mpOutlineOn = False
 ; resvor@gsnContourNegLineDashPattern = 1
  resvor@cnLineDashPattern = 0
   resvor@gsnPolar="SH"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
 
  v850=vor850_9(0,:)-vor850_0(0,:)
  v8502=vor850_0(0,:) 
  vnew = dim_rmvmean(v850) 
;  mapvor850 = gsn_csm_contour_map(wks3,vnew,resvor)
;  mapvor8502= gsn_csm_contour_map(wks3,v8502,resvor)
; plot = gsn_csm_contour_map_polar(wks2,v8502,resvor)  
 plot = gsn_csm_contour_map_polar(wks3,v850,resvor)
;  frame(wks2)
  frame(wks3)
end

