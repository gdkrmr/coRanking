#include "coranking.h"
#include <R.h>
#include <Rinternals.h>
#include "/usr/share/R/include/R.h"
#include "/usr/share/R/include/Rinternals.h"

extern "C" {

 
  SEXP coranking(SEXP Ro, SEXP R){

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

    
    
    UNPROTECT(1);
    return rQ;
  }

  
  SEXP rankmatrix(SEXP DD){

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

    UNPROTECT(1);
    return rR;
  }


  SEXP euclidean(SEXP data){

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
  
}
