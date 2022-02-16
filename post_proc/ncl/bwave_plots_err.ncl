load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;  wks = gsn_open_wks("pdf","surface_p_test")
  setvalues NhlGetWorkspaceObjectId()
    "wsMaximumSize" : 5000000
  end setvalues
 
  fname = getenv("FNAME")
  f = addfile(fname,"r")


  cf1 = f->cf1
  cf2 = f->cf2
  cf3 = f->cf3
  dims = dimsizes(f->u)
  nTime  = dims(0) 

 err_p = fspan(0,nTime,nTime)
 times = fspan(0,nTime,nTime)
 err_max_p = fspan(0,nTime,nTime)
 err_vor = fspan(0,nTime,nTime)
 err_max_vor = fspan(0,nTime,nTime)
 
  h0_0= (f->pressure(0,:,0))/100.
  h1_0= (f->pressure(0,:,1))/100.
  h2_0= (f->pressure(0,:,2))/100.
  h_0= (cf1*h0_0 + cf2*h1_0 + cf3*h2_0)
  vor850_0=f->vorticity(0,:,11)
  
     
;  nTime = 11
 do iTime = 0, nTime-1
  DAY = iTime
;print(DAY)

  
  h0= (f->pressure(iTime,:,0))/100.
  h1= (f->pressure(iTime,:,1))/100.
  h2= (f->pressure(iTime,:,2))/100.
  h = (cf1*h0 + cf2*h1 + cf3*h2)
  vor850_t=f->vorticity(iTime,:,11)
  
  err_p(iTime)=sqrt(avg((h_0-h)^2))
  err_max_p(iTime)=max(abs(h_0-h))
  err_vor(iTime)=sqrt(avg((vor850_t-vor850_0)^2))
  err_max_vor(iTime)=max(abs(vor850_t-vor850_0))
  times(iTime)=iTime
    
  end do
print(err_p)
print(times)
print(err_max_p)
print(err_vor)
print(err_max_vor)
; Output errors to ascii file. Change w to a if you want to append
 write_table("err.out","w",[/times,err_p,err_max_p,err_vor,err_max_vor/], \
              "%2.0f %10.8f %10.8f %10.8f %10.8f")
              
;Plot Errors
wks = gsn_open_wks("pdf" ,"RMSE_p")          ; ps,pdf,x11,ncgm,eps

  resxy                       = True            ; plot mods desired
 plot                      = gsn_xy(wks,times,err_p,resxy)


end

