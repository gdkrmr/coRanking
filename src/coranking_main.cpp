#include "coranking.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern "C" {

  SEXP C_coranking(SEXP Ro, SEXP R){

    if(!Rf_isInteger(Ro) || !Rf_isInteger(R))
      Rf_error("input must be of type integer");
    if(LENGTH(Rf_getAttrib(Ro, R_DimSymbol)) != 2 ||
       LENGTH(Rf_getAttrib(Ro, R_DimSymbol)) != 2)
      Rf_error("input must be a matrices");
    if( INTEGER( Rf_getAttrib(Ro, R_DimSymbol) )[0] !=
        INTEGER( Rf_getAttrib(Ro, R_DimSymbol) )[1] ||
        INTEGER( Rf_getAttrib(Ro, R_DimSymbol) )[0] !=
        INTEGER( Rf_getAttrib(Ro, R_DimSymbol) )[1] )
      Rf_error("input must be square");
    if(INTEGER( Rf_getAttrib(Ro, R_DimSymbol))[0] !=
       INTEGER( Rf_getAttrib(R,  R_DimSymbol))[0])
      Rf_error("input matrices must have the same size");

    int * cRo = INTEGER(Ro);
    int * cR  = INTEGER(R);
    int N = INTEGER( Rf_getAttrib(Ro, R_DimSymbol) )[0];

    SEXP rQ = PROTECT(Rf_allocMatrix(INTSXP, N-1, N-1));
    int * cQ = INTEGER(rQ);

    CORANKING::coranking(cRo, cR, N, cQ);

    // set dimensions nicely

    //the "" seems to be important, without it
    // we get a segfault
    const char *rQcornms[] = {"Ro", "R", ""};
    SEXP rQdimnames = PROTECT(Rf_mkNamed(VECSXP, rQcornms));

    // 1:N-1
    SEXP corIdx1 = PROTECT(Rf_allocVector(INTSXP, N-1));
    int * ci = INTEGER(corIdx1);
    for(int i = 0; i < N-1; i++) ci[i] = i+1;
    SEXP corIdx2 = PROTECT(Rf_duplicate(corIdx1));

    SET_VECTOR_ELT(rQdimnames, 0, Ro = corIdx1);
    SET_VECTOR_ELT(rQdimnames, 1, R  = corIdx2);
    Rf_setAttrib(rQ, R_DimNamesSymbol, rQdimnames);

    // set class
    SEXP rQclass = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(rQclass, 0, Rf_mkChar("coranking"));
    Rf_setAttrib(rQ, R_ClassSymbol, rQclass);

    UNPROTECT(5);
    return rQ;
  }


  SEXP C_rankmatrix(SEXP DD){

    if(!Rf_isReal(DD))
      Rf_error("input distance matrix must be of type real");
    if(LENGTH(Rf_getAttrib(DD, R_DimSymbol)) != 2)
      Rf_error("input must be a matrix");
    if(INTEGER( Rf_getAttrib(DD, R_DimSymbol) )[0] !=
       INTEGER( Rf_getAttrib(DD, R_DimSymbol) )[1])
      Rf_error("input must be square");

    double * cDD = REAL(DD);
    int N = INTEGER( Rf_getAttrib(DD, R_DimSymbol) )[0];

    SEXP rR = PROTECT(Rf_allocMatrix(INTSXP, N, N));
    int * cR = INTEGER(rR);

    CORANKING::rankmatrix(cDD, N, cR);

    SEXP Rclass = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(Rclass, 0, Rf_mkChar("rankmatrix"));
    Rf_setAttrib(rR, R_ClassSymbol, Rclass);

    UNPROTECT(2);
    return rR;
  }


  SEXP C_euclidean(SEXP data){

    if(!Rf_isReal(data))
      Rf_error("data must be of type real");
    if(LENGTH(Rf_getAttrib(data, R_DimSymbol)) != 2)
      Rf_error("data must be a matrix");

    double * cdata = REAL(data);
    int N = INTEGER( Rf_getAttrib(data, R_DimSymbol) )[0];
    int D = INTEGER( Rf_getAttrib(data, R_DimSymbol) )[1];

    SEXP rDD = PROTECT(Rf_allocMatrix(REALSXP, N, N));
    double * cDD = REAL(rDD);

    CORANKING::euclidean(cdata, N, D, cDD);

    UNPROTECT(1);
    return rDD;
  }

  // registering R routines

  static const R_CallMethodDef callMethods[] = {
    {"C_coranking",  (DL_FUNC) &C_coranking,  2},
    {"C_rankmatrix", (DL_FUNC) &C_rankmatrix, 1},
    {"C_euclidean",  (DL_FUNC) &C_euclidean,  1},
    {NULL, NULL, 0}
  };

  void R_init_coRanking(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
  }
} // end extern "C"
