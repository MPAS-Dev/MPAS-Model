      	program Grid_Refine
        include "scvt.m"

C  The file "nodes_s.dat" must exist and read it
      	open(15,file='locs.dat.out',status='unknown')
        read(15,*) N
      	do node = 1,N
           read(15,200) X(node),Y(node),Z(node)
      	enddo
      	close(15)
        print *,"Number of Starting Generators = ", N

C  Generate the Delaunay triangles
        CALL TRMESH (N,X,Y,Z,LIST,LPTR,LEND,LNEW,IWK,IWK(N+1),DS,IER)

       	DO  NODE = 1,N   
            LPL = LEND(NODE)
            LP = LPL
            K = 0
10          K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NEIGH(NODE,K) = ND
            IF (LP .NE. LPL) GOTO 10
            NEISZ(NODE) = K
       	ENDDO
  
       	K=N+1

C  Add points into the middle of edges
       	DO I = 1,N
           DO J = 1,NEISZ(I)
              IF (NEIGH(I,J).GT.I) then   
                 X(K) = 0.5*(X(I)+X(NEIGH(I,J)))
                 Y(K) = 0.5*(Y(I)+Y(NEIGH(I,J)))    
                 Z(K) = 0.5*(Z(I)+Z(NEIGH(I,J)))
                 R = sqrt(X(K)**2+Y(K)**2+Z(K)**2)
                 X(K) = X(K)/R
                 Y(K) = Y(K)/R
                 Z(K) = Z(K)/R
                 K=K+1
              ENDIF
           ENDDO
       	ENDDO
       	NG=K-1
        print *,"Number of Final Generators = ",NG
 
      	open(16,file='locs.dat.out.refined',status='unknown')
        write(16,100) NG        
      	do node = 1,NG
           write(16,200) X(node),Y(node),Z(node)
        enddo
        close(16)
100     format(I10)
!200     format(I10,3X,F16.10,3X,F16.10,3X,F16.10)
200     format(10x,f22.10,f23.10,f23.10)

       	end program Grid_Refine
