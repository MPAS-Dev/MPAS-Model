load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  r2d = 57.2957795             ; radians to degrees

  maxedges = 6 

  wks = gsn_open_wks("pdf","mesh")
  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")

  f = addfile("grid.nc","r")

  xVertex = f->xVertex(:)
  yVertex = f->yVertex(:)
  zVertex = f->zVertex(:)
  verticesOnCell = f->verticesOnCell(:,:)
  verticesOnEdge = f->verticesOnEdge(:,:)
  x   = f->lonCell(:) * r2d
  y   = f->latCell(:) * r2d
  lonCell = f->lonCell(:) * r2d
  latCell = f->latCell(:) * r2d
  lonVertex = f->lonVertex(:) * r2d
  latVertex = f->latVertex(:) * r2d
  lonEdge = f->lonEdge(:) * r2d
  latEdge = f->latEdge(:) * r2d

  res                      = True
  res@gsnMaximize          = True
  res@gsnSpreadColors      = True

  res@sfXArray             = x
  res@sfYArray             = y

  res@cnFillOn             = True
  res@cnFillMode           = "RasterFill"
  res@cnLinesOn            = False
  res@cnLineLabelsOn       = False
  res@cnInfoLabelOn        = True

  res@lbLabelAutoStride    = True
  res@lbBoxLinesOn         = False

  res@mpProjection      = "CylindricalEquidistant"
  res@mpDataBaseVersion = "MediumRes"
  res@mpCenterLatF      = 0.
  res@mpCenterLonF      = 0.
  res@mpGridAndLimbOn   = False
  res@mpOutlineOn       = True
  res@mpFillOn          = False
  res@mpPerimOn         = False
  res@gsnFrame          = False
  res@lbLabelBarOn      = False

  h   = f->areaCell(:)

  xpoly = new((/4/), "double")
  ypoly = new((/4/), "double")

  res@cnConstFLabelOn = False
  map = gsn_csm_contour_map(wks,h,res)

  lres = True
  lres@gsLineThicknessF = 0.10

  esizes = dimsizes(latEdge)
  ecx = new((/esizes(0),2/),double)
  ecy = new((/esizes(0),2/),double)
  do j=0,esizes(0)-1
     ecy(j,0) = latVertex(verticesOnEdge(j,0)-1)
     ecx(j,0) = lonVertex(verticesOnEdge(j,0)-1)
     ecy(j,1) = latVertex(verticesOnEdge(j,1)-1)
     ecx(j,1) = lonVertex(verticesOnEdge(j,1)-1)
  end do

  do j=0,esizes(0)-1
     if (abs(ecx(j,0) - ecx(j,1)) .gt. 180.0) then
        if (ecx(j,0) .gt. ecx(j,1)) then
           ecx(j,0) = ecx(j,0) - 360.0
        else
           ecx(j,1) = ecx(j,1) - 360.0
        end if
     end if
  end do

  do j=0,esizes(0)-1
     gsn_polyline(wks,map,ecx(j,:),ecy(j,:),lres)
  end do

  frame(wks)

end

