load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  ;
  ; Whether to do color-filled plot (filled=True) or
  ;   to plot contours of height field (filled=False)
  ;
;  filled   = True
  filled   = False

  ;
  ; The (lat,lon) the plot is to be centered over
  ;
  cenLat   = 0.0
  cenLon   = 0.0

  ;
  ; Projection to use for plot
  ;
;  projection = "Orthographic"
  projection = "CylindricalEquidistant"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  r2d = 57.2957795             ; radians to degrees

  maxedges = 7

  wks = gsn_open_wks("pdf","mesh_resolution")
  colors = (/"white","black","lightskyblue1","lightskyblue1","bisque"/)
  gsn_define_colormap(wks,colors)

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

  res                      = True
  res@gsnMaximize          = True
  res@gsnSpreadColors      = True

  res@sfXArray             = lonCell
  res@sfYArray             = latCell

  res@cnFillMode           = "AreaFill"

  if (filled) then
     res@cnFillOn             = True
     res@cnLinesOn            = False
     res@cnLineLabelsOn       = False
  else
     res@cnFillOn             = False
     res@cnLinesOn            = True
     res@cnLineLabelsOn       = True
  end if

  res@cnLevelSpacingF      = 8.0
  res@cnInfoLabelOn        = True
  res@lbLabelAutoStride    = True
  res@lbBoxLinesOn         = False

  res@mpProjection      = projection
  res@mpDataBaseVersion = "MediumRes"
  res@mpCenterLatF      = cenLat
  res@mpCenterLonF      = cenLon
  res@mpGridAndLimbOn   = True
  res@mpGridAndLimbDrawOrder = "PreDraw"
  res@mpGridLineColor   = "Background"
  res@mpOutlineOn       = True
  res@mpDataBaseVersion = "Ncarg4_1"
  res@mpDataSetName     = "Earth..3"
  res@mpOutlineBoundarySets = "Geophysical"
  res@mpFillOn          = True
  res@mpPerimOn         = True
  res@gsnFrame          = False
  res@cnLineThicknessF  = 2.0
  res@cnLineColor       = "NavyBlue"
  res@mpOceanFillColor  = 3
  res@mpInlandWaterFillColor  = 3
  res@mpLandFillColor  = 4
  res@tiMainString = "Approximate mesh resolution (km)"

  minSpacingKm = sqrt(min(f->areaCell(:))*2.0/sqrt(3.0))*6371.229
  print("Approx. min grid distance is "+minSpacingKm)

  t = stringtointeger(getenv("T"))
  fld = minSpacingKm / (f->meshDensity(:)^0.25)
  res@cnLineDashPattern = 0
  map = gsn_csm_contour_map(wks,fld,res)

  frame(wks)

end

