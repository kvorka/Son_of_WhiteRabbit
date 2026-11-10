submodule (equations) fill_nlm
  implicit none; contains
  
  module procedure fill_nlm1_sub
    
    call copy_carray_sub( this%mm1, arr(this%mm*(this%mm+1)/2+1), this%nlm1(0,ir) )
    
  end procedure fill_nlm1_sub
  
  module procedure fill_nlm2_sub
    
    call copy_carray_sub( this%mm1, arr(this%mm*(this%mm+1)/2+1), this%nlm2(0,ir) )
    
  end procedure fill_nlm2_sub
  
end submodule fill_nlm