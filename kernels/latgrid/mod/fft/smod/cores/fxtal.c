#include "fx2a.h"
#include "fx2b.h"
#include "fx3a.h"
#include "fx3b.h"
#include "fx4a.h"
#include "fx4b.h"
#include "fx5a.h"
#include "fx5b.h"

void fxztal_c( const int n,
               const int *restrict it,
               const double *restrict t,
               const int m,
                     double *restrict x )

{
    
    int l   = n / 2;
    int i   = it[0];
    int isd = 0;
    int k1  = 1;
    int ip  = i % 4 + 2;
    
    switch ( ip ) {
        
        case 4:
            fxzm4b_c( m, l, x );
        break;
        
        case 2:
            fxzm2b_c( m, l, x );
        break;
        
        case 3:
            fxzm3b_c( m, l, x );
        break;
        
        case 5:
            fxzm5b_c( m, l, x );
        break;
        
    }
    
    const int icddmax = it[1];
    
    for ( int icdd = 1; icdd < icddmax; icdd++ ) {
        
        l   = l / ip;
        i   = i / 4;
        isd = isd + k1 * (ip-1);
        k1  = k1 * ip;
        ip  = i % 4 + 2;
        
        switch ( ip ) {
            
            case 4:
                fxzm4a_c( m, k1, l, x, t+2*isd );
            break;
            
            case 2:
                fxzm2a_c( m, k1, l, x, t+2*isd );
            break;
            
            case 3:
                fxzm3a_c( m, k1, l, x, t+2*isd );
            break;
            
            case 5:
                fxzm5a_c( m, k1, l, x, t+2*isd );
            break;
            
        }
        
    }
    
}