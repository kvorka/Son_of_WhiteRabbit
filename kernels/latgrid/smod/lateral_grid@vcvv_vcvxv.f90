submodule (lateral_grid) vcvv_vcvxv
  implicit none; contains
  
  module procedure vcvv_vcvxv_sub
    integer                        :: ijml
    complex(kind=dbl), allocatable :: ca(:,:), cc(:), cr(:)
    
    !! Arrays for transforms preparation
    call this%rxd%allocate_scalars_sub( 9, cc )
    call this%rxd%allocate_scalars_sub( 4, cr )
    
    !! Vector decomposition
    allocate( ca(3,this%rxd%jmv) )
      
      !$omp simd
      do ijml = 1, this%rxd%jmv
        ca(1,ijml) = v(ijml)
        ca(2,ijml) = q(ijml)
        ca(3,ijml) = curlv(ijml)
      end do
      
      call this%rxd%vec2scal_jml_to_mj_sub( ca, 3, cc, 9, 1 )
      
    deallocate(ca)
    
    !! Transform
    call this%transform_sub( 4, 9, cc, cr, grid_op_vcvv_vcvxv_sub )
    
    !! Rearranging indexing
    call this%rxd%scal2scal_mj_to_jm_sub( cr, 4, 1, ntemp )
    call this%rxd%scal2vecscal_mj_to_jm_sub( cr, 4, 2, nsph1, ntorr, nsph2)
    
    !! Cleaning
    deallocate( cc, cr )
    
  end procedure vcvv_vcvxv_sub
  
end submodule vcvv_vcvxv