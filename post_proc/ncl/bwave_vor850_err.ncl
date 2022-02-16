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

  wks = gsn_open_wks("pdf","850hPa_vor_err")
  setvalues NhlGetWorkspaceObjectId()
    "wsMaximumSize" : 300000000
  end setvalues
  gsn_define_colormap(wks,"BlWhRe")

  fname = getenv("FNAME")
  f = addfile(fname,"r")


  lonCell   = f->lonCell(:) * r2d
  latCell   = f->latCell(:) * r2d
  lonVertex = f->lonVertex(:) * r2d
  latVertex = f->latVertex(:) * r2d
  lonEdge = f->lonEdge(:) * r2d
  latEdge = f->latEdge(:) * r2d
  verticesOnCell = f->verticesOnCell(:,:)
  alpha = f->angleEdge(:)
  vor850_0=f->vorticity(0,:,11)
 


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
 nTime = 11
 do iTime = 0, nTime-1
  DAY = iTime
 vor850_t=f->vorticity(iTime,:,11)
  v850=vor850_t-vor850_0


;  mapvor850 = gsn_csm_contour_map(wks3,vnew,resvor)
;  mapvor8502= gsn_csm_contour_map(wks3,v8502,resvor)
; plot = gsn_csm_contour_map_polar(wks2,v8502,resvor)  
 plot = gsn_csm_contour_map_polar(wks,v850,resvor)
;  frame(wks2)
  frame(wks)
  end do
end

