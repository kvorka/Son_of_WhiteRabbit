submodule (lateral_grid) vcvv_vcvxv
  implicit none; contains
  
  module procedure vcvv_vcvxv_sub
    complex(kind=dbl), allocatable :: ca(:), cc(:), cr(:)
    
    !Array preparation
    call this%rxd%allocate_vectors_sub( 3, ca )
    call this%rxd%allocate_scalars_sub( 9, cc )
    call this%rxd%allocate_scalars_sub( 4, cr )
    
    call this%rxd%vec2vec_jml_to_jml_sub( v,     ca, 5, 1 )
    call this%rxd%vec2vec_jml_to_jml_sub( q,     ca, 5, 2 )
    call this%rxd%vec2vec_jml_to_jml_sub( curlv, ca, 5, 2 )
    
    call this%rxd%vec2scal_jml_to_mj_sub( ca, 3, cc, 9, 1 )
    
    deallocate(ca)
    
    !Transform
    call this%transform_sub( 4, 9, cc, cr, grid_op_vcvv_vcvxv_sub )
    
    !Rearranging indexing
    call this%rxd%scal2scal_mj_to_jm_sub( cr, 4, 1, cjm, 4, 1 )
    call this%rxd%scal2vecscal_mj_to_jm_sub( cr, 4, 2, cjm, 4, 2 )
    
    !Cleaning
    deallocate( cc, cr )
    
  end procedure vcvv_vcvxv_sub
  
end submodule vcvv_vcvxv