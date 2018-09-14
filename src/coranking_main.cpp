#include "coranking.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern "C" {

  SEXP C_coranking(SEXP Ro, SEXP R){

    if(!isInteger(Ro) || !isInteger(R))
      error("input must be of type integer");
    if(LENGTH(getAttrib(Ro, R_DimSymbol)) != 2 ||
       LENGTH(getAttrib(Ro, R_DimSymbol)) != 2)
      error("input must be a matrices");
    if( INTEGER( getAttrib(Ro, R_DimSymbol) )[0] !=
        INTEGER( getAttrib(Ro, R_DimSymbol) )[1] ||
        INTEGER( getAttrib(Ro, R_DimSymbol) )[0] !=
        INTEGER( getAttrib(Ro, R_DimSymbol) )[1] )
      error("input must be square");
    if(INTEGER( getAttrib(Ro, R_DimSymbol))[0] !=
       INTEGER( getAttrib(R,  R_DimSymbol))[0])
      error("input matrices must have the same size");

    int * cRo = INTEGER(Ro);
    int * cR  = INTEGER(R);
    int N = INTEGER( getAttrib(Ro, R_DimSymbol) )[0];

    SEXP rQ = PROTECT(allocMatrix(INTSXP, N-1, N-1));
    int * cQ = INTEGER(rQ);

    CORANKING::coranking(cRo, cR, N, cQ);

    // set dimensions nicely

    //the "" seems to be important, without it
    // we get a segfault
    const char *rQcornms[] = {"Ro", "R", ""};
    SEXP rQdimnames = PROTECT(mkNamed(VECSXP, rQcornms));

    // 1:N-1
    SEXP corIdx1 = PROTECT(allocVector(INTSXP, N-1));
    int * ci = INTEGER(corIdx1);
    for(int i = 0; i < N-1; i++) ci[i] = i+1;
    SEXP corIdx2 = PROTECT(duplicate(corIdx1));

    SET_VECTOR_ELT(rQdimnames, 0, Ro = corIdx1);
    SET_VECTOR_ELT(rQdimnames, 1, R  = corIdx2);
    setAttrib(rQ, R_DimNamesSymbol, rQdimnames);

    // set class
    SEXP rQclass = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(rQclass, 0, mkChar("coranking"));
    setAttrib(rQ, R_ClassSymbol, rQclass);

    UNPROTECT(5);
    return rQ;
  }


  SEXP C_rankmatrix(SEXP DD){

    if(!isReal(DD))
      error("input distance matrix must be of type real");
    if(LENGTH(getAttrib(DD, R_DimSymbol)) != 2)
      error("input must be a matrix");
    if(INTEGER( getAttrib(DD, R_DimSymbol) )[0] !=
       INTEGER( getAttrib(DD, R_DimSymbol) )[1])
      error("input must be square");

    double * cDD = REAL(DD);
    int N = INTEGER( getAttrib(DD, R_DimSymbol) )[0];

    SEXP rR = PROTECT(allocMatrix(INTSXP, N, N));
    int * cR = INTEGER(rR);

    CORANKING::rankmatrix(cDD, N, cR);

    SEXP Rclass = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(Rclass, 0, mkChar("rankmatrix"));
    setAttrib(rR, R_ClassSymbol, Rclass);

    UNPROTECT(2);
    return rR;
  }


  SEXP C_euclidean(SEXP data){

    if(!isReal(data))
      error("data must be of type real");
    if(LENGTH(getAttrib(data, R_DimSymbol)) != 2)
      error("data must be a matrix");

    double * cdata = REAL(data);
    int N = INTEGER( getAttrib(data, R_DimSymbol) )[0];
    int D = INTEGER( getAttrib(data, R_DimSymbol) )[1];

    SEXP rDD = PROTECT(allocMatrix(REALSXP, N, N));
    double * cDD = REAL(rDD);

    CORANKING::euclidean(cdata, N, D, cDD);

    UNPROTECT(1);
    return rDD;
  }

  // registering R routines

  static R_NativePrimitiveArgType S[] = {REALSXP};
  static R_NativePrimitiveArgType SS[] = {REALSXP, REALSXP};

  static const
  R_CMethodDef cMethods[] = {
                             {"C_coranking",  (DL_FUNC) &C_coranking,  2, SS},
                             {"C_rankmatrix", (DL_FUNC) &C_rankmatrix, 1, S},
                             {"C_euclidean",  (DL_FUNC) &C_euclidean,  1, S},
                             {NULL, NULL, 0, NULL}};

  static const
  R_CallMethodDef callMethods[] = {
                                   {"C_coranking",  (DL_FUNC) &C_coranking,  2},
                                   {"C_rankmatrix", (DL_FUNC) &C_rankmatrix, 1},
                                   {"C_euclidean",  (DL_FUNC) &C_euclidean,  1},
                                   {NULL, NULL, 0}};

  void R_init_coRanking(DllInfo *info) {
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
  }
} // end extern "C"
