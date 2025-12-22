submodule (lateral_grid) vcvv_vcvxv
  implicit none; contains
  
  module procedure vcvv_vcvxv_sub
    complex(kind=dbl), allocatable :: cc(:), cr(:)
    
    !! Arrays for transforms preparation
    allocate( cc(9*this%rxd%jms1) ); call zero_carray_sub( 9*this%rxd%jms1, cc )
    allocate( cr(4*this%rxd%jms1) ); call zero_carray_sub( 4*this%rxd%jms1, cr )
    
    !! Vectors decomposition
    call this%rxd%vec2scal_jm_to_mj_sub( v, q, curlv, cc )
    
    !! Transform
    call this%transform_sub( 4, 9, cc, cr, grid_op_vcvv_vcvxv_sub )
    
    !! Rearranging indexing
    call this%rxd%scal2scal_mj_to_jm_sub( cr, 4, 1, ntemp )
    call this%rxd%scal2vec_mj_to_jm_sub( cr, 4, 2, nsph1, ntorr, nsph2)
    
    !! Cleaning
    deallocate( cc, cr )
    
  end procedure vcvv_vcvxv_sub
  
end submodule vcvv_vcvxv