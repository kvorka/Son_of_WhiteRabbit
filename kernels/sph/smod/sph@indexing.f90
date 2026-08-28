submodule (sph) indexing
  implicit none; contains
  
  module procedure jm
    
    jm = ij*(ij+1)/2+im+1
    
  end procedure jm
  
  module procedure jml
    
    jml = 3*(ij*(ij+1)/2+im)+il
    
  end procedure jml
  
  module procedure mj
    
    mj = (ijmax+1)*im - im*(im+1)/2 + ij + 1
    
  end procedure mj
  
end submodule indexing