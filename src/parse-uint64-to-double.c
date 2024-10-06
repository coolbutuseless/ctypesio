
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Reverse mem copy
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
static void rev_memcpy(uint8_t *dst, uint8_t *src, size_t N) {
  for (size_t i = 0; i < N; i++) {
    dst[i] = src[N - 1 - i]; 
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP uint64_to_dbl_(SEXP vec_) {
  
  if (TYPEOF(vec_) != RAWSXP || length(vec_) != 8) {
    error("uint64_to_dbl_(): Need 8 raw bytes");
  }
  
  uint64_t *val = (uint64_t *)RAW(vec_);
  
  return ScalarReal((double)(*val));
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// @param x_ raw vector containing C integers in the given 'type' encoding
// @param type_ one of 'uint32', 'uint64' or 'int64'
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP convert_cint_to_rdbl_(SEXP x_, SEXP type_, SEXP big_endian_) {
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Determine type width and first/last character
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  size_t width = 0;
  const char *type = CHAR(STRING_ELT(type_, 0));
  unsigned long len = strlen(type);
  char last_char  = type[len - 1];
  char first_char = type[0];
  
  switch(last_char) {
  
  case '8': //  8 bits
    width = 1;
    break;
  case '6': // 16 bits
    width = 2;
    break;
  case '2': // 32 bits
    width = 4;
    break;
  case '4': // 64 bits
    width = 8;
    break;
  default:
    error("type not understood: %s", type);
  }
  
  size_t Nbytes = (size_t)length(x_);
  
  if (Nbytes % width != 0) {
    error("Nbytes (%zu) is not a multiple of width %zu\n", Nbytes, width);
  }
  
  size_t N = Nbytes / width;
  
  SEXP res_ = PROTECT(allocVector(REALSXP, (R_xlen_t)N));
  double *res = REAL(res_);
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // If big_endian
  // Reverse bytes in groups of 'width'
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (asLogical(big_endian_)) {
    error("convert_cint_to_rdbl_(): Big endian conversion not done yet");
  }
  
  if (last_char == '4') {
    // 64 bit
    if (first_char == 'u') {
      uint64_t *v = (uint64_t *)RAW(x_);
      for (int i = 0; i < N; i++) {
        res[i] = v[i];
      }
    } else {
      int64_t *v = (int64_t *)RAW(x_);
      for (int i = 0; i < N; i++) {
        res[i] = v[i];
      }
    }
  } else if (last_char == '2' && first_char == 'u') {
    uint32_t *v = (uint32_t *)RAW(x_);
    for (int i = 0; i < N; i++) {
      res[i] = v[i];
    }
  } else {
    error("convert_cint_to_rdbl_(): type not handled here: %s", type);
  }
  
  
  UNPROTECT(1);
  return(res_);
}




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Convert R double to another integer type.  
// 
// .Call(convert_int32_, x, type, endian == 'big') 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP convert_rdbl_to_cint_(SEXP x_, SEXP type_, SEXP big_endian_) {
  
  size_t N  = (size_t)length(x_);
  double *x = REAL(x_);
  
  size_t width = 0;
  const char *type = CHAR(STRING_ELT(type_, 0));
  unsigned long len = strlen(type);
  char last_char  = type[len - 1];
  char first_char = type[0];
  
  switch(last_char) {
  
  case '8': //  8 bits
    width = 1;
    break;
  case '6': // 16 bits
    width = 2;
    break;
  case '2': // 32 bits
    width = 4;
    break;
  case '4': // 64 bits
    width = 8;
    break;
  default:
    error("type not understood: %s", type);
  }
  
  SEXP res_ = PROTECT(allocVector(RAWSXP, (R_xlen_t)(N * width)));
  uint8_t *res = RAW(res_);
  
  bool big_endian = asLogical(big_endian_);
  
  if (last_char == '8') {
    // 8 bit
    if (first_char == 'u') {
      for (size_t i = 0; i < N; i++) {
        res[i] = (uint8_t)x[i];
      }
    } else {
      int8_t *res2 = (int8_t*)res; // to avoid a Wconversion issue
      for (size_t i = 0; i < N; i++) {
        res2[i] = (int8_t)x[i];
      }
    }
  } else if (last_char == '6') {
    // 16 bit
    if (first_char == 'u') {
      for (size_t i = 0; i < N; i++) {
        uint16_t val = (uint16_t)x[i];
        if (big_endian) {
          rev_memcpy(res + i * width, (uint8_t *)&val, width);
        } else {  
          memcpy(res + i * width, &val, width);
        }
      }
    } else {
      for (size_t i = 0; i < N; i++) {
        int16_t val = (int16_t)x[i];
        if (big_endian) {
          rev_memcpy(res + i * width, (uint8_t *)&val, width);
        } else {  
          memcpy(res + i * width, &val, width);
        }
      }
    }
  } else if (last_char == '2') {
    // 32 bit
    if (first_char == 'u') {
      for (size_t i = 0; i < N; i++) {
        uint32_t val = (uint32_t)x[i];
        if (big_endian) {
          rev_memcpy(res + i * width, (uint8_t *)&val, width);
        } else {  
          memcpy(res + i * width, &val, width);
        }
      }
    } else {
      for (size_t i = 0; i < N; i++) {
        int32_t val = (int32_t)x[i];
        if (big_endian) {
          rev_memcpy(res + i * width, (uint8_t *)&val, width);
        } else {  
          memcpy(res + i * width, &val, width);
        }
      }
    }
  } else if (last_char == '4') {
    // 64 bit
    if (first_char == 'u') {
      for (size_t i = 0; i < N; i++) {
        uint64_t val = (uint64_t)x[i];
        if (big_endian) {
          rev_memcpy(res + i * width, (uint8_t *)&val, width);
        } else {  
          memcpy(res + i * width, &val, width);
        }
      }
    } else {
      for (size_t i = 0; i < N; i++) {
        int64_t val = (int64_t)x[i];
        if (big_endian) {
          rev_memcpy(res + i * width, (uint8_t *)&val, width);
        } else {  
          memcpy(res + i * width, &val, width);
        }
      }
    }
  } else {
    error("type not understood :: %s", type);
  }

  
  UNPROTECT(1);
  return res_;
}





