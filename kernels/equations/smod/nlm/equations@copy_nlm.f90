submodule (equations) copy_nlm
  implicit none; contains
  
  module procedure copy2_nlm1_sub
    
    call copy2_carray_sub( this%mm1*n, fac, this%nlm1, this%rhs1(0,2) )
    
  end procedure copy2_nlm1_sub
  
  module procedure copy3_nlm1_sub
    
    call copy3_carray_sub( this%mm1*n, fac, this%nlm1, this%rhs1(0,2) )
    
  end procedure copy3_nlm1_sub
  
  module procedure copy2_nlm2_sub
    
    call copy2_carray_sub( this%mm1*n, fac, this%nlm2, this%rhs2(0,2) )
    
  end procedure copy2_nlm2_sub
  
  module procedure copy3_nlm2_sub
    
    call copy3_carray_sub( this%mm1*n, fac, this%nlm2, this%rhs2(0,2) )
    
  end procedure copy3_nlm2_sub
  
end submodule copy_nlm