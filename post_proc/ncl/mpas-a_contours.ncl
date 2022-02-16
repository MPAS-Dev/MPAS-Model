load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  ;
  ; Which field to plot
  ;
  plotfield = "h"
;  plotfield = "ke"
;  plotfield = "vorticity"

  ;
  ; Whether to plot wind vectors
  ;
;  winds    = True
  winds    = False

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

  wks = gsn_open_wks("pdf","atm_contours")
  gsn_define_colormap(wks,"gui_default")

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

  if (plotfield .eq. "h" .or. plotfield .eq. "ke") then
     res@sfXArray             = lonCell
     res@sfYArray             = latCell
  end if
  if (plotfield .eq. "vorticity") then
     res@sfXArray             = lonVertex
     res@sfYArray             = latVertex
  end if

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

;  res@cnLevelSpacingF      =  50.0
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
  res@mpFillOn          = False
  res@mpPerimOn         = True
  res@gsnFrame          = False
  res@cnLineThicknessF  = 2.0
  res@cnLineColor       = "NavyBlue"

  t = stringtointeger(getenv("T"))
  if (plotfield .eq. "h") then
;     fld = f->xice(t,:)
;     fld = f->sst(t,:)
;     fld = f->surface_pressure(t,:)
;     fld = f->pressure_base(t,:,25) + f->pressure_p(t,:,25)
     fld = f->theta(t,:,25)
  end if
  if (plotfield .eq. "ke") then
     fld = f->ke(t,:,0)
  end if
  if (plotfield .eq. "vorticity") then
     fld = f->vorticity(t,:,0)
  end if
  res@cnLineDashPattern = 0
  map = gsn_csm_contour_map(wks,fld,res)

  if (winds) then
     u   = f->u(t,:,0)
     v   = f->v(t,:,0)
     esizes = dimsizes(u)
     u_earth = new(dimsizes(u),float)
     v_earth = new(dimsizes(u),float)
     lat_edge = new(dimsizes(u),float)
     lon_edge = new(dimsizes(u),float)
     do i=0,esizes(0)-1
        u_earth(i) = doubletofloat(u(i)*cos(alpha(i)) - v(i)*sin(alpha(i)))
        v_earth(i) = doubletofloat(u(i)*sin(alpha(i)) + v(i)*cos(alpha(i)))
        lat_edge(i) = doubletofloat(latEdge(i))
        lon_edge(i) = doubletofloat(lonEdge(i))
     end do

     wmsetp("VCH",0.0010)
     wmsetp("VRN",0.010)
     wmsetp("VRS",100.0)
     wmsetp("VCW",0.10)

     wmvectmap(wks, lat_edge, lon_edge, u_earth, v_earth)
  end if

  frame(wks)

end

