#include "fxcpy.h"

void fxzshf_c( const int n,
               const int *restrict it,
               const int ioff,
               const int m,
                     double *restrict x )
{
    
    // Howmany FFTs are we handeling
    const int xsize1 = 8 * vlen * m;
    
    // Aligned temporal store for shuffling
    double *restrict y = aligned_alloc( alignement, 8*xsize1 );
    
    // Main cycle
    int j = 0;
    int i30, i31;
    
    while ( j < n/2-2 ) {
        
        i30 = it[j];
        
        if ( i30 < 0 ) {
            
            j += 1;
            
        } else {
            
            j  += 1;
            i31 = it[j];
            
            fxcpy_c( m, x+xsize1*i30, y );
            
            while ( i31 >= 0 ) {
                
                fxcpy_c( m, x+xsize1*i31, x+xsize1*i30 );
                i30 = i31;
                
                j  += 1;
                i31 = it[j];
                
            }
            
            j += 1;
            
            fxcpy_c( m, x+xsize1*(i31-ioff), x+xsize1* i30       );
            fxcpy_c( m, y,                   x+xsize1*(i31-ioff) );
            
        }
        
    }
    
    // Cleaning
    free( y );
    
}