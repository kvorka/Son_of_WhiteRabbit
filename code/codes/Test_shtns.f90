program Test_shtns
  use omp_lib
  use lateral_grid2
  implicit none
  
  integer, parameter :: jmax = 213
  
  type(T_lateralGrid2)           :: latgrid
  integer                        :: ij, im
  real(kind=dbl)                 :: time1, time2
  type(c_ptr)                    :: sgrid1_c, sgrid2_c, sgrid3_c
  real(kind=dbl),    pointer     :: sgrid1(:), sgrid2(:), sgrid3(:)
  complex(kind=dbl), allocatable :: arr1(:,:), arr2(:,:), arr(:,:)
  
  call latgrid%init_sub(jmax)
  
  allocate( arr(latgrid%jms,3), arr1(latgrid%jms,3), arr2(latgrid%jms,3) )
  
  arr  = czero
  arr1 = czero
  arr2 = czero
  
  call latgrid%alloc_grid_sub( sgrid1_c, sgrid1 )
  call latgrid%alloc_grid_sub( sgrid2_c, sgrid2 )
  call latgrid%alloc_grid_sub( sgrid3_c, sgrid3 )
  
  ij = 0
    im = 0
      call random_number( arr(jm(ij,im),3)%re )
      arr(jm(ij,im),3)%im = zero
  
  do ij = 1, latgrid%jmax
    im = 0
      call random_number( arr(jm(ij,im),1)%re )
      arr(jm(ij,im),1)%im = zero
       
      call random_number( arr(jm(ij,im),2)%im )
      arr(jm(ij,im),2)%re = zero
      
      call random_number( arr(jm(ij,im),3)%re )
      arr(jm(ij,im),3)%im = zero
      
    do im = 1, ij
      call random_number( arr(jm(ij,im),1)%re )
      call random_number( arr(jm(ij,im),1)%im )
      call random_number( arr(jm(ij,im),2)%re )
      call random_number( arr(jm(ij,im),2)%im )
      call random_number( arr(jm(ij,im),3)%re )
      call random_number( arr(jm(ij,im),3)%im )
    end do
  end do
  
  arr1 = arr
  
  time1 = omp_get_wtime()
  call latgrid%jm_to_grid_sub(arr1(1,3), sgrid1)
  call latgrid%grid_to_jm_sub(sgrid1, arr2(1,3))
  time2 = omp_get_wtime()
  
  write(*,*) time2-time1, maxval(abs(arr(:,3)-arr2(:,3)))
  
  arr1 = arr
  arr2 = czero
  
  time1 = omp_get_wtime()
  call latgrid%jml_to_grid_sub(arr1(1,1), arr1(1,2), arr1(1,3), sgrid1, sgrid2, sgrid3)
  call latgrid%grid_to_jml_sub(sgrid1, sgrid2, sgrid3, arr2(1,1), arr2(1,2), arr2(1,3))
  time2 = omp_get_wtime()
  
  write(*,*) time2-time1, maxval(abs(arr(:,1)-arr2(:,1))), &
                        & maxval(abs(arr(:,2)-arr2(:,2))), &
                        & maxval(abs(arr(:,3)-arr2(:,3)))
  
end program Test_shtns