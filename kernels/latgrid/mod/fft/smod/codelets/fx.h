#ifndef FX_H
#define FX_H

#include <stddef.h>
#include <immintrin.h>

#if defined ( mem32 )
    
    typedef __m256d t_vec;
    
    // length of one register in doubles
    #define vlen 4
    
    // vector intrinsics
    #define t_set1(val)      _mm256_set1_pd(val)
    #define t_load(ptr)      _mm256_load_pd(ptr)
    #define t_store(ptr,val) _mm256_store_pd((ptr),(val))
    #define t_add(a, b)      _mm256_add_pd((a),(b))
    #define t_sub(a, b)      _mm256_sub_pd((a),(b))
    #define t_mul(a, b)      _mm256_mul_pd((a),(b))
    
    #if defined (__FMA__)
        #define t_fmadd(a, b, c)  _mm256_fmadd_pd((a),(b),(c))
        #define t_fnmadd(a, b, c) _mm256_fnmadd_pd((a),(b),(c))
    #endif
    
#else
    
    typedef __m512d t_vec;
    
    // length of one register in doubles
    #define vlen 8
    
    // vector intrinsics
    #define t_set1(val)       _mm512_set1_pd(val)
    #define t_load(ptr)       _mm512_load_pd(ptr)
    #define t_store(ptr,val)  _mm512_store_pd((ptr),(val))
    #define t_add(a, b)       _mm512_add_pd((a),(b))
    #define t_sub(a, b)       _mm512_sub_pd((a),(b))
    #define t_mul(a, b)       _mm512_mul_pd((a),(b))
    #define t_fmadd(a, b, c)  _mm512_fmadd_pd((a),(b),(c))
    #define t_fnmadd(a, b, c) _mm512_fnmadd_pd((a),(b),(c))
    
#endif

#endif