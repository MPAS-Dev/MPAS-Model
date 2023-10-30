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
  projection = "CylindricalEquidistant"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  r2d = 57.2957795             ; radians to degrees

  wks = gsn_open_wks("pdf","surface_p")

  f = addfile("output.nc","r")

  lonCell   = f->lonCell(:) * r2d
  latCell   = f->latCell(:) * r2d
  lonVertex = f->lonVertex(:) * r2d
  latVertex = f->latVertex(:) * r2d
  lonEdge = f->lonEdge(:) * r2d
  latEdge = f->latEdge(:) * r2d
  verticesOnCell = f->verticesOnCell(:,:)
  alpha = f->angleEdge(:)
  cf1 = f->cf1
  cf2 = f->cf2
  cf3 = f->cf3


  res                      = True
  res@gsnMaximize          = True
  res@gsnSpreadColors      = True

  res@sfXArray             = lonCell
  res@sfYArray             = latCell
  ;res@sfXArray             = lonEdge
  ;res@sfYArray             = latEdge

  res@cnFillOn             = False
;  res@cnFillMode           = "AreaFill"
;  res@cnLevelSpacingF      =  10.0
;  res@cnInfoLabelOn        = True
;  res@cnLevelSelectionMode = "ManualLevels"
;  res@cnMinLevelValF       =  940. 
;  res@cnMaxLevelValF       = 1024.
;  res@cnLevelSpacingF      =    4. 
  res@cnLinesOn            = True
  res@cnLineLabelsOn       = False
  res@cnInfoLabelOn        = True
  res@cnInfoLabelFontHeightF = 0.007

;  res@lbLabelBarOn         = True
;  res@lbLabelAutoStride    = True
;  res@lbBoxLinesOn         = False

  res@mpProjection      = projection
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

  nTime = 17
  do iTime =0, nTime-1
  DAY = iTime

  res@gsnLeftString     = ":F21:P:B::F21:sfc:N: at day " + DAY + " "
  res@gsnLeftStringFontHeightF = 0.012
  res@gsnRightString    = " "
  res@gsnRightStringFontHeightF = 0.01

  ;There are two option for surface pressure
  ;option1 -> using coefficient cf1, cf2, cf3
  ;h0= (f->pressure_p(iTime,:,0) + f->pressure_base(iTime,:,0))/100.
  ;h1= (f->pressure_p(iTime,:,1) + f->pressure_base(iTime,:,1))/100.
  ;h2= (f->pressure_p(iTime,:,2) + f->pressure_base(iTime,:,2))/100.
  ;h = (cf1*h0 + cf2*h1 + cf3*h2)

  ;option2 -> using simple extrapolation 
  rdzw = f->rdzw(:)
  p = f->pressure(iTime,:,0)
  rho=f->rho(iTime,:,:)
  qv = f->qv(iTime,:,:) 
  h = (p + 0.5/rdzw(0)*9.80616*(1.25*rho(:,0) - .25*rho(:,1)))/100. ;for dry!!!
  ;h = (p + 0.5/rdzw(0)*9.80616*(1.25*rho(:,0)*(1.+qv(:,0)) - .25*rho(:,1)*(1.+qv(:,1))))/100. ;for moist!!!

  fld = h

  map = gsn_csm_contour_map(wks,fld,res)
  frame(wks)
    
  end do

end

