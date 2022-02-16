load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  r2d = 57.2957795             ; radians to degrees

  wks = gsn_open_wks("pdf","atm_mesh")

  colors = (/"white","black","lightskyblue1","lightskyblue1","bisque"/)
;  colors = (/"white","black","white","white","grey90"/)
  gsn_define_colormap(wks,colors)

  fname = getenv("FNAME")
  f = addfile(fname,"r")

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

  res@mpProjection      = "Orthographic"
  res@mpDataBaseVersion = "MediumRes"
  res@mpCenterLatF      =  50.
  res@mpCenterLonF      = -100.
  res@mpCenterRotF      = -100.
  res@mpGridAndLimbOn   = False
  res@mpOutlineOn       = True
  res@mpFillOn          = True
  res@mpPerimOn         = False
  res@gsnFrame          = False
  res@mpOceanFillColor  = 3
  res@mpInlandWaterFillColor  = 3
  res@mpLandFillColor  = 4

  map = gsn_csm_map(wks,res)

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

