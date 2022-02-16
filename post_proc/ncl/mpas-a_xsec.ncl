load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
  r2d = 57.2957795             ; radians to degrees
  pi  = 3.14159265

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;
  ; Which field to plot
  ;
;  plotfield = "w"
  plotfield = "theta"
;  plotfield = "ke"
;  plotfield = "vorticity"


  ;
  ; Whether to plot horizontal wind vectors
  ;
;  horiz_winds    = True
  horiz_winds    = False

  ;
  ; Whether to do color-filled plot (filled=True) or
  ;   to plot contours of height field (filled=False)
  ;
  filled   = True
;  filled   = False

  ;
  ; Starting and ending locations (in degrees)
  ; Exercise caution when setting these: setting start_lon=90.0 and end_lon=-90.0
  ;   would create a cross-section including the prime meridian, whereas setting
  ;   start_lon=90.0 and end_lon=270.0 would create a cross-section containing
  ;   the date line, for example.
  ;   
  ;
  start_lat = 40.0
  start_lon = -140.0
  end_lat = 40.0
  end_lon = -80.0

  ;
  ; The number of points along the cross section
  ;
  nsec = 250

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  wks = gsn_open_wks("pdf","atm_xsec")
  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")

  fname = getenv("FNAME")
  f = addfile(fname,"r")

  lonCell   = f->lonCell(:) * r2d
  latCell   = f->latCell(:) * r2d
  xCell     = f->xCell(:)
  yCell     = f->yCell(:)
  zCell     = f->zCell(:)
  lonVertex = f->lonVertex(:) * r2d
  latVertex = f->latVertex(:) * r2d
  xVertex = f->xVertex(:)
  yVertex = f->yVertex(:)
  zVertex = f->zVertex(:)
  lonEdge = f->lonEdge(:) * r2d
  latEdge = f->latEdge(:) * r2d
  xEdge = f->xEdge(:)
  yEdge = f->yEdge(:)
  zEdge = f->zEdge(:)
  zgrid = f->zgrid(:,:) / 1000.0
  verticesOnCell = f->verticesOnCell(:,:)
  edgesOnCell = f->edgesOnCell(:,:)
  nCellsOnCell = f->nEdgesOnCell(:)
  cellsOnCell = f->cellsOnCell(:,:)
  alpha = f->angleEdge(:)

  dims = dimsizes(latCell)
  nCells = dims(0)

  start_lat = start_lat / r2d
  start_lon = start_lon / r2d
  end_lat = end_lat / r2d
  end_lon = end_lon / r2d

  radius = 6371220.0
  xsec_latitude  = start_lat
  xsec_longitude = start_lon
  xsec_lat_inc = (end_lat - start_lat) / (int2flt(nsec) - 1.0)
  xsec_lon_inc = (end_lon - start_lon) / (int2flt(nsec) - 1.0)

  xsecx = new((/nsec/),float)
  xsecy = new((/nsec/),float)
  xsecz = new((/nsec/),float)
  xsec_cell_id = new((/nsec/),integer)
  xsec_edge_id = new((/nsec/),integer)
  xsec_vtx_id = new((/nsec/),integer)
  xsec_id = new((/nsec/),integer)

  ; Compute (x,y,z) coordinates for points on cross section
  do i=0,nsec-1
     xsecx(i) = radius * cos(xsec_longitude) * cos(xsec_latitude)
     xsecy(i) = radius * sin(xsec_longitude) * cos(xsec_latitude)
     xsecz(i) = radius * sin(xsec_latitude)
     xsec_latitude  = xsec_latitude  + xsec_lat_inc
     xsec_longitude = xsec_longitude + xsec_lon_inc
  end do

  ; Find cell containing first cross section point
  dmin = 2.0 * radius
  cellmin = -1
  do i=0,nCells-1
     d = sqrt((xCell(i) - xsecx(0))^2.0 + (yCell(i) - xsecy(0))^2.0 + (zCell(i) - xsecz(0))^2.0)
     if (d .lt. dmin) then
        cellmin = i
        dmin = doubletofloat(d)
     end if
  end do
  xsec_cell_id(0) = cellmin

  ; For the remaining cross section points, find the grid cell containing them
  do j=1,nsec-1
     moved = 1
     do while (moved .ne. 0)
        moved = 0
        d = sqrt((xCell(cellmin) - xsecx(j))^2.0 + (yCell(cellmin) - xsecy(j))^2.0 + (zCell(cellmin) - xsecz(j))^2.0)
        do k=0,nCellsOnCell(cellmin)-1
           dn = sqrt((xCell(cellsOnCell(cellmin,k)-1) - xsecx(j))^2.0 + (yCell(cellsOnCell(cellmin,k)-1) - xsecy(j))^2.0 + (zCell(cellsOnCell(cellmin,k)-1) - xsecz(j))^2.0)
           if (dn .lt. d) then
              d = dn
              nearest = (/cellsOnCell(cellmin,k)/)-1
              moved = 1
           end if
        end do
        if (moved .eq. 1) then
           cellmin = nearest
        end if
     end do
     xsec_cell_id(j) = cellmin
  end do

  ; For all cross section points, find the nearest vertex and edge
  do i=0,nsec-1
     iVtx = verticesOnCell(xsec_cell_id(i),0) - 1
     iEdge = edgesOnCell(xsec_cell_id(i),0) - 1
     xsec_edge_id(i) = iEdge
     xsec_vtx_id(i) = iVtx
     de = sqrt((xEdge(iEdge) - xsecx(i))^2.0 + (yEdge(iEdge) - xsecy(i))^2.0 + (zEdge(iEdge) - xsecz(i))^2.0)
     dv = sqrt((xVertex(iVtx) - xsecx(i))^2.0 + (yVertex(iVtx) - xsecy(i))^2.0 + (zVertex(iVtx) - xsecz(i))^2.0)
     do j=1,nCellsOnCell(xsec_cell_id(i))-1
        iVtx = verticesOnCell(xsec_cell_id(i),j) - 1
        iEdge = edgesOnCell(xsec_cell_id(i),j) - 1
        de_test = sqrt((xEdge(iEdge) - xsecx(i))^2.0 + (yEdge(iEdge) - xsecy(i))^2.0 + (zEdge(iEdge) - xsecz(i))^2.0)
        dv_test = sqrt((xVertex(iVtx) - xsecx(i))^2.0 + (yVertex(iVtx) - xsecy(i))^2.0 + (zVertex(iVtx) - xsecz(i))^2.0)
        if (de_test .lt. de) then
           de = de_test
           xsec_edge_id(i) = iEdge
        end if     
        if (dv_test .lt. dv) then
           dv = dv_test
           xsec_vtx_id(i) = iVtx
        end if     
     end do
  end do

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; At this point, xsec_cell_id(:), xsec_edge_id(:), and xsec_vtx_id(:) contains the cell, edge, 
  ;   and vertex IDs of the nearest points to those along the cross section
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  res                      = True
  res@gsnMaximize          = False
  res@gsnSpreadColors      = True

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

  res@cnLevelSpacingF      =  0.01
  res@cnInfoLabelOn        = True

  res@lbLabelAutoStride    = True
  res@lbBoxLinesOn         = False

  res@gsnFrame          = False


  ;
  ; Select field to be plotted, and set generic array xsec_id(:) to contain IDs of
  ;    locations (cell, edge, or vertex) in that field containg cross section points
  ;

  t = stringtointeger(getenv("T"))
  if (plotfield .eq. "w") then
     fld1 = f->w(t,:,:)
     ldims = dimsizes(fld1)
     fld = new((/ldims(0),ldims(1)-1/),"double")
     ; Average w to center of layers
     do i=0,ldims(0)-1
        do j=0,ldims(1)-2
           fld(i,j) = 0.5*(fld1(i,j)+fld1(i,j+1))
        end do
     end do
     nVertLevels = ldims(1)
     nVertLevels = nVertLevels-1
     xsec_id(:) = xsec_cell_id(:)
  end if
  if (plotfield .eq. "theta") then
     fld = f->theta(t,:,:)
     ldims = dimsizes(fld)
     nVertLevels = ldims(1)
     xsec_id(:) = xsec_cell_id(:)
  end if
  if (plotfield .eq. "ke") then
     fld = f->ke(t,:,:)
     ldims = dimsizes(fld)
     nVertLevels = ldims(1)
     xsec_id(:) = xsec_cell_id(:)
  end if
  if (plotfield .eq. "vorticity") then
     fld = f->vorticity(t,:,:)
     ldims = dimsizes(fld)
     nVertLevels = ldims(1)
     xsec_id(:) = xsec_vtx_id(:)
  end if
  res@cnLineDashPattern = 0

  height1 = new((/nVertLevels+1,nsec/),float)
  height = new((/nVertLevels+1,nsec+1/),float)
  x      = new((/nVertLevels+1,nsec+1/),float)

  ; Extract field from along cross section into plotting array
  arr = new((/nVertLevels,nsec/),float) 
  do i=0,nsec-1
     do j=0,nVertLevels-1
;        arr(j,i) = 0.5*doubletofloat(fld(xsec_id(i),j)+fld(xsec_id(i),j+1))
        arr(j,i) = doubletofloat(fld(xsec_id(i),j))
        height1(j,i) = doubletofloat(zgrid(xsec_id(i),j))
     end do
     j = nVertLevels
     height1(j,i) = doubletofloat(zgrid(xsec_id(i),j))
  end do

  do j=0,nVertLevels
     x(j,nsec) = int2flt(nsec) + 0.5
     x(j,0) = 0.5 
     height(j,0) = height1(j,0)
     height(j,nsec) = height1(j,nsec-1)
  end do

  do i=1,nsec-1
     do j=0,nVertLevels
        height(j,i) = 0.5*(height1(j,i) + height1(j,i-1))
        x(j,i) = int2flt(i) + 0.5 
     end do
  end do

  xpoly = new((/5/), "float")
  ypoly = new((/5/), "float")

  minfld = min(arr)
  maxfld = max(arr)
  fldrange = maxfld - minfld

  res@trYMinF = min(zgrid)
  res@trYMaxF = max(zgrid)
  res@trXMinF = int2flt(0)
  res@trXMaxF = int2flt(nsec+1)

  res@tiYAxisString = "z(km)"
  res@tiYAxisFontHeightF = 0.017
  res@tiXAxisString = "cell"
  res@tiXAxisFontHeightF = 0.017

  map = gsn_csm_xy(wks,x,height,res)

  do i=0,nsec-1
  do j=0,nVertLevels-1
     xpoly(0) = x(j,i)
     xpoly(1) = x(j,i+1)
     xpoly(2) = x(j+1,i+1)
     xpoly(3) = x(j+1,i)
     xpoly(4) = x(j,i)

     ypoly(0) = height(j,i)
     ypoly(1) = height(j,i+1)
     ypoly(2) = height(j+1,i+1)
     ypoly(3) = height(j+1,i)
     ypoly(4) = height(j,i)

     res@gsFillColor = doubletointeger(195*(arr(j,i) - minfld)/fldrange+2)
     gsn_polygon(wks,map,xpoly,ypoly,res);
  end do
  end do

  if (horiz_winds) then
     u   = f->u(t,:,:)
     v   = f->v(t,:,:)
     esizes = dimsizes(u)
     nVertLevels = esizes(1)
     u_earth = new((/nVertLevels,nsec/),float)
     v_earth = new((/nVertLevels,nsec/),float)
     x_edge = new((/nVertLevels,nsec/),float)
     y_edge = new((/nVertLevels,nsec/),float)
     do i=0,nsec-1
     do j=0,nVertLevels-1
        u_earth(j,i) = doubletofloat(u(xsec_edge_id(i),j)*cos(alpha(xsec_edge_id(i))) - v(xsec_edge_id(i),j)*sin(alpha(xsec_edge_id(i))))
        v_earth(j,i) = doubletofloat(u(xsec_edge_id(i),j)*sin(alpha(xsec_edge_id(i))) + v(xsec_edge_id(i),j)*cos(alpha(xsec_edge_id(i))))
        x_edge(j,i) = i
        y_edge(j,i) = j
     end do
     end do

     wmsetp("VCH",0.0010)
     wmsetp("VRN",0.010)
     wmsetp("VRS",50.0)
     wmsetp("VCW",0.10)

     wmvect(wks, x_edge, y_edge, u_earth, v_earth)
  end if

  ;
  ; Draw label bar
  ;

  xcb = new((/4/), "float")
  ycb = new((/4/), "float")

  tres = True
  tres@txAngleF = 90.0
  tres@txFontHeightF = 0.013
  do i=2,200
     xcb(0) = 0.125 + i*0.75/198
     ycb(0) = 0.08

     xcb(1) = 0.125 + (i+1)*0.75/198
     ycb(1) = 0.08

     xcb(2) = 0.125 + (i+1)*0.75/198
     ycb(2) = 0.10

     xcb(3) = 0.125 + i*0.75/198
     ycb(3) = 0.10

     tres@gsFillColor = i

     gsn_polygon_ndc(wks,xcb,ycb,tres);

     j = (i-2) % 20
     if ((j .eq. 0) .or. (i .eq. 200)) then
        ff = minfld + int2flt(i-2) * fldrange / 198.0
        label = sprintf("%8.3g", ff)
        gsn_text_ndc(wks, label, xcb(0), 0.050, tres)
     end if

  end do

  frame(wks)

end

