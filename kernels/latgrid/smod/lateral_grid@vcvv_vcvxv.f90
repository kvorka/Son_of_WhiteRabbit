submodule (lateral_grid) vcvv_vcvxv
  implicit none; contains
  
  module procedure vcvv_vcvxv_sub
    integer                        :: nca, ncc, ncr
    complex(kind=dbl), allocatable :: cc(:), cr(:), ca(:)
    
    !! Arrays for transforms preparation
    ncc = 9*this%rxd%jms1
    ncr = 4*this%rxd%jms1
    
    allocate( cc(ncc), cr(ncr) )
    
    !! The output array needs to be zeroed before the computation
    call zero_carray_sub( ncr, cr )
    
    !! Temporal array for vector to scalars decomposition
    nca = 9*this%rxd%jms
    
    allocate( ca(nca) ); call copy_vcvv_vcvxv_sub( this%rxd%jms, v, q, curlv, ca )
    
    !! Vectors decomposition
    call this%rxd%vec2scal_jm_to_mj_sub( 3, ca, cc )
    
    !! Clean temporal array
    deallocate( ca )
    
    !! Transform
    call this%transform_sub( 4, 9, cc, cr, grid_op_vcvv_vcvxv_sub )
    
    !! Rearranging indexing
    call this%rxd%scal2scal_mj_to_jm_sub( cr, 4, 1, ntemp )
    call this%rxd%scal2vec_mj_to_jm_sub( cr, 4, 2, nsph1, ntorr, nsph2)
    
    !! Cleaning
    deallocate( cc, cr )
    
  end procedure vcvv_vcvxv_sub
  
end submodule vcvv_vcvxv