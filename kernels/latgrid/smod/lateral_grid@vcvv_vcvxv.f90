submodule (lateral_grid) vgradT_vcurlv
  implicit none; contains
  
  module procedure vgradT_vcurlv_sub
    integer                        :: nca, ncv, ncs, ncc, ncr
    complex(kind=dbl), allocatable :: cc(:), cr(:), ca(:)
    
    !! Array dimensions for transform: the temporal storage for 2grid transform
    !! needs scalar length of jms, while the actual array needs jms1 (jmax+1,jmax+1)
    !! due to transform from spectra to x,y,z vector components
    nca = 9*this%rxd%jms
    ncv = 3*this%rxd%jms
    ncs = 1*this%rxd%jms
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
    allocate( cc(ncc) )
    
    call this%rxd%vec2scal_jm_to_mj_sub( 3, ca, cc )
    
    deallocate( ca )
    
    !! Allocate output array and zero it: in this array, 1 scalar and 3 cartesian
    !! components are being computed - the scalar is for vgradT, while the vectors
    !! are for vcurlv
    allocate( cr(ncr) )
    
    call zero_carray_sub( ncr, cr )
    
    !! After all the preparation, the transform is here: on the output, vgradT is stored
    !! in cr(1,*), while vcurlvx, vcurlvy and vcurlz are in cr(2:4,*)
    call this%transform_sub( 4, 9, cc, cr, grid_op_vgradT_vcurlv_sub )
    
    !! Another layer of transposing: from (4,mj) to (jm,4)
    call this%rxd%scal2scal_mj_to_jm_sub( cr, 4, 1, ntemp )
    call this%rxd%scal2vec_mj_to_jm_sub( cr, 4, 2, nsph1, ntorr, nsph2)
    
    !! Cleaning
    deallocate( cc, cr )
    
  end procedure vgradT_vcurlv_sub
  
end submodule vgradT_vcurlv