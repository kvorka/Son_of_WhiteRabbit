submodule (math) int2str
  implicit none; contains
  
  module procedure int2str_fn
    
    write(str,'(I0)') n
    
  end procedure int2str_fn
  
end submodule int2str