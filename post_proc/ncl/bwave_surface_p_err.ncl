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
; projection="PolarStereographic"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  r2d = 57.2957795             ; radians to degrees

  wks = gsn_open_wks("pdf","surface_p_err")
  setvalues NhlGetWorkspaceObjectId()
    "wsMaximumSize" : 5000000
  end setvalues
 ; wks2 = gsn_open_wks("pdf","surface_slp_t")
   setvalues NhlGetWorkspaceObjectId()
    "wsMaximumSize" : 50000000
  end setvalues

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
  cf1 = f->cf1
  cf2 = f->cf2
  cf3 = f->cf3


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
  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF       =  997. 
  res@cnMaxLevelValF       = 1002.
  res@cnLevelSpacingF      =    1.0 
  res@cnLinesOn            = True
  res@cnLineLabelsOn       = True
  res@cnInfoLabelOn        = True
  res@cnInfoLabelFontHeightF = 0.007

;  res@lbLabelBarOn         = True
;  res@lbLabelAutoStride    = True
;  res@lbBoxLinesOn         = False

  res@mpProjection      = projection
  res@mpDataBaseVersion = "MediumRes"
;  res@mpGridAndLimbDrawOrder = "PreDraw"
;  res@mpGridLineColor   = "Background"
;  res@mpGridAndLimbOn   = True
  res@mpCenterLatF      = cenLat
  res@mpCenterLonF      = cenLon
  res@mpOutlineOn       = False
  res@mpFillOn          = False
  res@mpPerimOn         = False
  res@gsnFrame          = False
  res@mpMinLatF         = -90.

  res@gsnContourNegLineDashPattern = 1
  ;res@cnLineDashPattern = 0
  
  ;========================
  ; get list of all files and open as "one big file"
  ;========================                             
;     all_files = systemfunc ("ls diag*.nc")
;     fall      = addfiles (all_files, "r")   ; note the "s" of addfile
  ;========================
  ; choose how files are combined and read in variable across files
  ;========================  
;     ListSetType (fall, "cat") 
;     slp    = fall[:]->mslp 
;     vor850 = fall[:]->vorticity_850hPa
 ;    temp850 = fall[:]->temperature_850hPa                    
 ;    printVarSummary(vor850)
 ;    printVarSummary(temp850)
  h0_0= (f->pressure(0,:,0))/100.
  h1_0= (f->pressure(0,:,1))/100.
  h2_0= (f->pressure(0,:,2))/100.
  h_0= (cf1*h0_0 + cf2*h1_0 + cf3*h2_0)  
  nTime = 11
 do iTime = 0, nTime-1
  DAY = iTime
;print(DAY)
  res@gsnLeftString     = ":F21:P:B::F21:sfc:N: at day " + DAY + " "
  res@gsnLeftStringFontHeightF = 0.012
  res@gsnRightString    = " "
  res@gsnRightStringFontHeightF = 0.01

  h0= (f->pressure(iTime,:,0))/100.
  h1= (f->pressure(iTime,:,1))/100.
  h2= (f->pressure(iTime,:,2))/100.
  h = (cf1*h0 + cf2*h1 + cf3*h2)
 
  fld = h_0-h

   map = gsn_csm_contour_map(wks,h,res)
  ;mapsp = gsn_csm_contour_map(wks2,h,res)

  frame(wks)
  frame(wks2)

    
  end do

end

