load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  r2d = 57.2957795             ; radians to degrees

  wks = gsn_open_wks("pdf","atm_mesh")

  colors = (/"white","black","lightskyblue1","lightskyblue1","bisque", "blue", "green", "red"/)
;  colors = (/"white","black","white","white","grey90"/)
  gsn_define_colormap(wks,colors)

  fname =  "grid.nc" ;getenv("FNAME")
  f = addfile(fname,"r")

  xVertex = f->xVertex(:)
  yVertex = f->yVertex(:)
  zVertex = f->zVertex(:)
  verticesOnCell = f->verticesOnCell(:,:)
  verticesOnEdge = f->verticesOnEdge(:,:)
  cellsOnEdge = f->cellsOnEdge(:,:)
  x   = f->lonCell(:) * r2d
  y   = f->latCell(:) * r2d
  lonCell = f->lonCell(:) * r2d
  latCell = f->latCell(:) * r2d
  lonVertex = f->lonVertex(:) * r2d
  latVertex = f->latVertex(:) * r2d
  lonEdge = f->lonEdge(:) * r2d
  latEdge = f->latEdge(:) * r2d
  indCell = f->indexToCellID(:)
  indVert = f->indexToVertexID(:) 
  indEdge = f->indexToEdgeID(:)
   
  res                      = True
  res@gsnMaximize          = True

  res@mpProjection      = "Orthographic"
  res@mpDataBaseVersion = "MediumRes"
  res@mpCenterLatF      =  0 ;50.
  res@mpCenterLonF      = 0 ;-100.
  res@mpCenterRotF      = -90 ;-100.
  res@mpGridAndLimbOn   = False
  res@mpOutlineOn       = True
  res@mpFillOn          = True
  res@mpPerimOn         = False
  res@gsnFrame          = False
  res@mpOceanFillColor  = 0 ;3
  res@mpInlandWaterFillColor  = 0 ;3
  res@mpLandFillColor  = 0 ;4
  res@mpGeophysicalLineThicknessF = 0.10
  res@mpGreatCircleLinesOn   =  True
  
  map = gsn_csm_map(wks,res)

; --------------------
; Voronoi Cells 
;------------------------

  lres = True
  lres@gsLineThicknessF = 0.40

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

;-------------------------
; Triangles
;-------------------------

  esizes = dimsizes(latEdge)
  ecx = new((/esizes(0),2/),double)
  ecy = new((/esizes(0),2/),double)
  do j=0,esizes(0)-1
     ecy(j,0) = latCell(cellsOnEdge(j,0)-1)
     ecx(j,0) = lonCell(cellsOnEdge(j,0)-1)
     ecy(j,1) = latCell(cellsOnEdge(j,1)-1)
     ecx(j,1) = lonCell(cellsOnEdge(j,1)-1)
     
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
  
  lres = True
  lres@gsLineThicknessF = 0.40
  lres@gsLineColor = colors(5)
  
  do j=0,esizes(0)-1
     gsn_polyline(wks,map,ecx(j,:),ecy(j,:),lres)
  end do
  
  ;--------------------------
  ; Edge midpoints
  ;--------------------------
   polyres                   = True
   polyres@gsMarkerIndex     = 1          ; polymarker style
   polyres@gsMarkerSizeF     = 0.015          ; polymarker size
   polyres@gsMarkerColor  = colors(6)     ; marker color

  gsn_polymarker(wks,map,lonEdge,latEdge, polyres) 

  txres               = True
  txres@txFontHeightF = 0.007
  txres@txFont        = "helvetica-bold"
  txres@txAngleF   = 90
  gsn_text(wks,map,sprintf("%6.4g",indVert),lonVertex,latVertex,txres)

  txres@txFont        = "helvetica"
  gsn_text(wks,map,sprintf("%6.4g",indCell),lonCell,latCell,txres)
  
  txres@txFont        = "helvetica"
  txres@txJust = "TopLeft"
  gsn_text(wks,map,sprintf("%6.4g",indEdge),lonEdge,latEdge,txres)
  
  ;--------------------------
  ; Edge midpoints of HCm grid
  ;--------------------------
  fname2 =  "grid_hcm.nc" ;getenv("FNAME")
  f2 = addfile(fname2,"r")

  lonEdge2 = f2->lonEdge(:) * r2d
  latEdge2 = f2->latEdge(:) * r2d

   polyres2                   = True
   polyres2@gsMarkerIndex     = 2          ; polymarker style
   polyres2@gsMarkerSizeF     = 0.004          ; polymarker size
   polyres2@gsMarkerColor  = colors(7)     ; marker color

  gsn_polymarker(wks,map,lonEdge2,latEdge2, polyres2)

  frame(wks)

  
end

