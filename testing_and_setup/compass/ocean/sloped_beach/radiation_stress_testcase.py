import numpy as np
import matplotlib.pyplot as plt

# Physical constants
g = 9.81
rho = 997.0

# Solve for the wavenumber, k, using the linear dispersion relationship:
#    sigma**2 = gk*tanh(kh)
# given the period, T, and depth, h.
def wavenumber(T,h):
  
  # Period to frequency
  sigma = 2.0*np.pi/T

  # Initial guess
  k = sigma**2.0/(g*np.sqrt(np.tanh(sigma**2*h/g)))
  
  # Newton's method to solve for k
  maxit = 100  
  tol = 1e-8
  for it in range(maxit):
    
    kh = np.multiply(k,h)
    f = g*np.multiply(k,np.tanh(kh))-sigma**2
    fp = g*np.tanh(kh)+g*np.multiply(kh,1.0-np.square(np.tanh(kh)))
    knew = k - np.divide(f,fp)

    if np.amax(np.absolute(knew-k)) < tol:
      print it
      break
    k = knew

  return knew

def plot_rad_stress(X,Y,var,name):

  fig = plt.figure()
  ax = fig.add_subplot(2,1,1)
  if np.amin(var) != np.amax(var):
    levels = np.linspace(np.amin(var),np.amax(var),100)
    cf = ax.contourf(X,Y,var,levels=levels)
  else:
    cf = ax.contourf(X,Y,var)
  plt.colorbar(cf,ax=ax,orientation='horizontal')
  ax.axis('image')
  ax.set_title(name)
  ax.set_xlabel('x (m)')
  ax.set_ylabel('y (m)')
  ax = fig.add_subplot(2,1,2)
  ax.plot(X[0,:],var[0,:])
  ax.set_xlabel('x (m)')
  ax.set_ylabel(name)
  plt.tight_layout()
  plt.savefig(name+'.png',bbox_inches='tight')
  plt.close()

if __name__ == '__main__':

  # Domain parameters
  Lx  =150
  Ly = 50
  nx = 150
  ny = 50
  h0 = 2.1
  
  # Initialize domain coordinates
  x = np.linspace(0,Lx,nx)
  y = np.linspace(0,Ly,ny)
  X,Y = np.meshgrid(x,y)
  
  # Initialize bathymetry
  Z = np.zeros(X.shape)
  Z = (1.0/40.0)*X + .1
  idx = np.where(X >= 80)
  Z[idx] = h0
  
  # Plot bathymetry
  plt.figure()
  plt.contourf(X,Y,Z)
  plt.colorbar(orientation='horizontal')
  plt.axis('image')
  plt.title('depth')
  plt.xlabel('x (m)')
  plt.ylabel('y (m)')
  plt.savefig('h.png',bbox_inches='tight')
  
  # Wave parameters
  H0 = 0.6
  T = 5.0
  gamma = .78
  theta = 0.0
  h = Z
  sigma = 2.0*np.pi/T
  
  # Calculate radiation stress
  k0 = wavenumber(T,h0)
  n0 = 0.5*(1 + (2.0*k0*h0)/np.sinh(2.0*k0*h0))
  Cg0 = n0*sigma/k0
  k = wavenumber(T,h)
  kh = np.multiply(k,h)
  n = 0.5*(1.0 + np.divide((2.0*kh),np.sinh(2.0*kh)))
  C = sigma/k
  Cg = np.multiply(n,C)
  H = H0*np.sqrt(Cg0/Cg) # Assumes Kr=1
  hb = np.minimum(gamma*h,H)
  E = (1.0/8.0)*rho*g*hb**2
  Sxx = np.multiply(E,(n*(np.cos(theta)**2 + 1.0) - 0.5))
  Syy = np.multiply(E,(n*(np.sin(theta)**2 + 1.0) - 0.5))
  Sxy = 0.5*np.multiply(E,n*(np.sin(2.0*theta)))
  
  # x-dervative of bathymetry
  dhdx = np.zeros(X.shape)
  idx = np.where(X <= 80.0)
  dhdx[idx] = 1.0/40.0
  
  # x-derivative of wavenumber
  dkdx = -np.multiply(np.square(k),dhdx)
  denom = np.multiply(np.square(np.cosh(kh)),np.tanh(kh)) + kh
  dkdx = np.divide(dkdx,denom)
  
  # x-derivative of n
  fac = np.multiply(dkdx,h) + np.multiply(k,dhdx)
  dndx = np.multiply(fac,np.sinh(kh)-np.multiply(kh,np.cosh(kh)))
  dndx = np.divide(dndx,np.square(np.sinh(kh)))
  
  # x-derivative of energy
  f = np.divide(n0/k0*k,n)
  dfdx = n0/k0*np.divide(np.multiply(dkdx,n) - np.multiply(dndx,k),np.square(n))
  dHdx = np.multiply(0.5*H0/np.sqrt(f),dfdx)
  idx = np.where(h <= H/gamma)
  dHdx[idx] = gamma*(1.0/40.0)
  dEdx = .25*rho*g*np.multiply(hb,dHdx)
  
  # Derivative of radation stress
  dSxxdx = np.multiply(dEdx,n*(np.cos(theta)**2 + 1.0) - 0.5) + np.multiply(E,dndx*(np.cos(theta)**2 + 1.0))
  
  # Plot radiation stress
  plot_rad_stress(X,Y,hb,'hb')
  plot_rad_stress(X,Y,Sxx,'Sxx')
  plot_rad_stress(X,Y,Syy,'Syy')
  plot_rad_stress(X,Y,Sxy,'Sxy')
  plot_rad_stress(X,Y,dSxxdx,'dSxxdx')
  
  fig = plt.figure()
  plt.plot(X[0,:],dSxxdx[0,:])
  dx = x[1]-x[0]
  dSxxdx_approx = (Sxx[:,1:]-Sxx[:,:-1])/dx
  plt.plot(X[0,:-1],dSxxdx_approx[0,:])
  plt.tight_layout()
  plt.savefig('check_deriv.png',bbox_inches='tight')
  plt.close()
