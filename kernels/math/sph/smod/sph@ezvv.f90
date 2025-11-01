submodule (sph) ezvv
  implicit none; contains
  
  module procedure ezvv_sub
    integer           :: ij, im, ijm, jm0, jm1, jm2
    complex(kind=dbl) :: cfac
    
    cfac = cunit * fac
    
    ij = 0
      im = 0
        ijm = 1
        
        cjml(1,ijm) = czero
        cjml(2,ijm) = czero
        cjml(3,ijm) = sqrt(2._dbl/3._dbl) * cajml(3) * cfac
    
    ij = 1
      do im = 0, ij
        ijm = ijm+1
        
        jm1 = 3 * ( (ij-1)*(ij  ) / 2 + im )
        jm0 = 3 * ( (ij  )*(ij+1) / 2 + im )
        jm2 = 3 * ( (ij+1)*(ij+2) / 2 + im )
        
        cjml(1,ijm) = (                                          -im * cajml(jm0-1) /  ij           ) * cfac
        cjml(2,ijm) = ( sqrt((ij+1)*((ij  )**2-im**2)/(2*ij+1._dbl)) * cajml(jm1+1) /  ij         - &
                      &                                           im * cajml(jm0  ) / (ij*(ij+1)) + &
                      & sqrt((ij  )*((ij+1)**2-im**2)/(2*ij+1._dbl)) * cajml(jm2-1) /     (ij+1)    ) * cfac
        cjml(3,ijm) = (                                           im * cajml(jm0+1) /     (ij+1)  + &
                      & sqrt((ij+2)*((ij+1)**2-im**2)/(2*ij+3._dbl)) * cajml(jm2  ) /     (ij+1)    ) * cfac
      end do
    
    do ij = 2, np-1
      do im = 0, ij
        ijm = ijm+1
        
        jm1 = 3 * ( (ij-1)*(ij  ) / 2 + im )
        jm0 = 3 * ( (ij  )*(ij+1) / 2 + im )
        jm2 = 3 * ( (ij+1)*(ij+2) / 2 + im )
        
        cjml(1,ijm) = ( sqrt((ij-1)*((ij  )**2-im**2)/(2*ij-1._dbl)) * cajml(jm1  ) /  ij         - &
                      &                                           im * cajml(jm0-1) /  ij           ) * cfac
        cjml(2,ijm) = ( sqrt((ij+1)*((ij  )**2-im**2)/(2*ij+1._dbl)) * cajml(jm1+1) /  ij         - &
                      &                                           im * cajml(jm0  ) / (ij*(ij+1)) + &
                      & sqrt((ij  )*((ij+1)**2-im**2)/(2*ij+1._dbl)) * cajml(jm2-1) /     (ij+1)    ) * cfac
        cjml(3,ijm) = (                                           im * cajml(jm0+1) /     (ij+1)  + &
                      & sqrt((ij+2)*((ij+1)**2-im**2)/(2*ij+3._dbl)) * cajml(jm2  ) /     (ij+1)    ) * cfac
      end do
    end do
    
    ij = np
      do im = 0, ij
        ijm = ijm+1
        
        jm1 = 3 * ( (ij-1)*(ij  ) / 2 + im )
        jm0 = 3 * ( (ij  )*(ij+1) / 2 + im )
        jm2 = 3 * ( (ij+1)*(ij+2) / 2 + im )
        
        cjml(1,ijm) = ( sqrt((ij-1)*(ij**2-im**2)/(2*ij-1._dbl)) * cajml(jm1  ) /  ij       - &
                      &                                       im * cajml(jm0-1) /  ij         ) * cfac
        cjml(2,ijm) = ( sqrt((ij+1)*(ij**2-im**2)/(2*ij+1._dbl)) * cajml(jm1+1) /  ij       - &
                      &                                       im * cajml(jm0  ) / (ij*(ij+1)) ) * cfac
        cjml(3,ijm) = (                                       im * cajml(jm0+1) /     (ij+1)  ) * cfac
      end do
      
  end procedure ezvv_sub
  
end submodule ezvv