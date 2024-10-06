
// #define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP uint64_to_dbl_(SEXP vec_);
SEXP convert_rdbl_to_cint_(SEXP x_, SEXP type_, SEXP big_endian_);
SEXP convert_cint_to_rdbl_(SEXP x_, SEXP type_, SEXP big_endian_);
SEXP raw_to_integer64_(SEXP raw_vec_, SEXP big_endian_);
SEXP integer64_to_raw_(SEXP vec_    , SEXP big_endian_);

static const R_CallMethodDef CEntries[] = {
  {"uint64_to_dbl_", (DL_FUNC) &uint64_to_dbl_, 1},
  {"convert_rdbl_to_cint_", (DL_FUNC) &convert_rdbl_to_cint_, 3},
  {"convert_cint_to_rdbl_", (DL_FUNC) &convert_cint_to_rdbl_, 3},
  {"raw_to_integer64_", (DL_FUNC) &raw_to_integer64_, 2},
  {"integer64_to_raw_", (DL_FUNC) &integer64_to_raw_, 2},
  {NULL , NULL, 0}
};


void R_init_ctypesio(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}



