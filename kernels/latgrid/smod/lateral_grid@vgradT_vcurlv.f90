submodule (lateral_grid) vgradT_vcurlv
  implicit none; contains
  
  module procedure vgradT_vcurlv_sub
    integer                        :: nca, ncc, ncr
    complex(kind=dbl), allocatable :: cc(:), cr(:), ca(:)
    
    !! Array dimensions for transform: the temporal storage for 2grid transform
    !! needs scalar length of jms, while the actual array needs jms1 (jmax+1,jmax+1)
    !! due to transform from spectra to x,y,z vector components
    nca = 9*this%rxd%jms
    ncc = 9*this%rxd%jms1
    ncr = 4*this%rxd%jms1
    
    !! Allocate temporal array and copy the data: this is really just a transposition
    !! to have the data in a contiguous storage before the heavy lifting
    allocate( ca(nca) )
    
    call copy_vgradT_vcurlv_sub( this%rxd%jms, v, q, curlv, ca )
    
    !! Allocate the array for x,y,z components and transform the 3 vectors into 9 scalars
    !! representing the expansions for cartesian components: despite after the previous
    !! copy, the vectors are ordered as v(l-1), q(l-1), curlv(l-1), v(l), q(l) ... for
    !! best cache behaviour, at the end of the transform, a small transposition occurs
    !! and the output layout is vx, vy, vz, qx, qy, qz, curlvx, curlvy, curlvz
    allocate( cc(ncc), cr(ncr) )
    
    call this%rxd%vec2scal_jm_to_mj_sub( 3, ca, cc )
    
    deallocate( ca )
    
    !! After all the preparation, the transform is here: on the output, vgradT is stored
    !! in cr(1,*), while vcurlvx, vcurlvy and vcurlz are in cr(2:4,*)
    call this%transform_sub( 4, 9, cc, cr, grid_op_vgradT_vcurlv_sub )
    
    deallocate( cc )
    
    !! Another layer of transposing: from (4,mj) to (mj,4). If any other dimensions should
    !! be used, like (7,mj) to (mj,7) might occur with dynamo simulations, these should be
    !! addded to math@ctrans.f90 by hand.
    allocate( ca(ncr) )
    
    call trans_carray_sub( 4, this%rxd%jms1, cr, ca )
    
    deallocate( cr )
    
    !! Now we can freely transer the data into destination fields with a changed indexation
    call this%rxd%scal2scal_mj_to_jm_sub( ca(1), ntemp )
    call this%rxd%scal2vec_mj_to_jm_sub( ca(this%rxd%jms1+1), nsph1, ntorr, nsph2 )
    
    deallocate( ca )
    
  end procedure vgradT_vcurlv_sub
  
end submodule vgradT_vcurlv