module binaryio
  use math
  implicit none
  
  interface
    module subroutine write_2d_binfile_sub(filenum, filepath, narr, arr)
      integer,           intent(in) :: filenum
      character(len=*),  intent(in) :: filepath
      integer,           intent(in) :: narr
      complex(kind=dbl), intent(in) :: arr(*)
    end subroutine write_2d_binfile_sub
    
    module subroutine write_3d_binfile_sub(filenum, filepath, narr, arr, nrr, rr)
      integer,           intent(in) :: filenum
      character(len=*),  intent(in) :: filepath
      integer,           intent(in) :: narr
      complex(kind=dbl), intent(in) :: arr(*)
      integer,           intent(in) :: nrr
      real(kind=dbl),    intent(in) :: rr(*)
    end subroutine write_3d_binfile_sub
    
    module subroutine read_2d_binfile_sub(filenum, filepath, narr, arr)
      integer,           intent(in)  :: filenum
      character(len=*),  intent(in)  :: filepath
      integer,           intent(in)  :: narr
      complex(kind=dbl), intent(out) :: arr(*)
    end subroutine read_2d_binfile_sub
    
    module subroutine read_3d_binfile_sub(filenum, filepath, narr, arr, nrr, rr)
      integer,           intent(in)  :: filenum
      character(len=*),  intent(in)  :: filepath
      integer,           intent(in)  :: narr
      complex(kind=dbl), intent(out) :: arr(*)
      integer,           intent(in)  :: nrr
      real(kind=dbl),    intent(out) :: rr(*)
    end subroutine read_3d_binfile_sub
  end interface
  
end module binaryio