submodule (physicalobject) nl_all
  implicit none; contains
  
  module procedure init_nl_all_sub
    integer :: ir
    
    allocate( this%ntemp(this%jms,2:this%nd) )
    allocate( this%ntorr(this%jms,2:this%nd) )
    allocate( this%nsph1(this%jms,2:this%nd) )
    allocate( this%nsph2(this%jms,2:this%nd) )
    
    !$omp parallel do
    do ir = 2, this%nd
      call zero_carray_sub( this%jms, this%ntemp(1,ir) )
      call zero_carray_sub( this%jms, this%ntorr(1,ir) )
      call zero_carray_sub( this%jms, this%nsph1(1,ir) )
      call zero_carray_sub( this%jms, this%nsph2(1,ir) )
    end do
    !$omp end parallel do
    
  end procedure init_nl_all_sub
  
end submodule nl_all