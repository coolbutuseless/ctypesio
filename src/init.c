
// #define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP uint64_to_dbl_(SEXP vec_);
SEXP convert_rdbl_to_cint_(SEXP x_, SEXP type_, SEXP big_endian_);
SEXP convert_cint_to_rdbl_(SEXP x_, SEXP type_, SEXP big_endian_);

static const R_CallMethodDef CEntries[] = {
  {"uint64_to_dbl_", (DL_FUNC) &uint64_to_dbl_, 1},
  {"convert_rdbl_to_cint_", (DL_FUNC) &convert_rdbl_to_cint_, 3},
  {"convert_cint_to_rdbl_", (DL_FUNC) &convert_cint_to_rdbl_, 3},
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



