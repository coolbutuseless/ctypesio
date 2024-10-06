
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Interpret a raw vector as a vector of integer64
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP raw_to_integer64_(SEXP raw_vec_, SEXP big_endian_) {
  
  int nprotect = 0;
  int input_len = length(raw_vec_);
  
  if (input_len % 8 != 0 || TYPEOF(raw_vec_) != RAWSXP) {
    error("Input must be a raw vector with length a multiple of 8");
  }
  
  if (asLogical(big_endian_)) {
    error("Converting raw vec from big endian to integer64 not currently supported");
  }
  
  int N = input_len / 8;
  SEXP res_ = PROTECT(allocVector(REALSXP, N)); nprotect++;
  
  memcpy(REAL(res_), RAW(raw_vec_), input_len);
  
  setAttrib(res_, R_ClassSymbol, PROTECT(mkString("integer64"))); nprotect++;
  
  UNPROTECT(nprotect);
  return res_;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Convert integer64 vector to a raw vector
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP integer64_to_raw_(SEXP vec_, SEXP big_endian_) {
  
  int nprotect = 0;
  int nbytes = length(vec_) * 8;
  
  SEXP res_ = PROTECT(allocVector(RAWSXP, nbytes)); nprotect++;
  
  if (asLogical(big_endian_)) {
    error("Converting raw vec from big endian to integer64 not currently supported");
  }
  
  memcpy(RAW(res_), REAL(vec_), nbytes);
  
  UNPROTECT(nprotect);
  return res_;
}


