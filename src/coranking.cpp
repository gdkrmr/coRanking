#include <cmath>
#include <algorithm>
#include <cstdio>
#include <iostream>
#include "coranking.h"
#include <R.h>
#include <Rinternals.h>


void CORANKING::coranking(const int* Ro, const int* R,
			  const int N, int* Q) {
  for(int i = 0; i < (N-1)*(N-1); i++) Q[i] = 0;
  
  int ind;
  int Qind;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < N; j++) {
      ind = i*N + j;
      if(R[ind] > 0 && Ro[ind] > 0) {
	Qind = (R[ind] - 1) * (N-1) + Ro[ind] - 1;
	Q[ Qind ] += 1;
      }
    }
  }
}

void CORANKING::rankmatrix(const double* DD,
				  const int N, int* R) {
  
  int* inds  = (int*) malloc(N * sizeof(int));
  int* ranks = (int*) malloc(N * sizeof(int));
  const double* ptr;
  for(int i = 0; i < N; i++) {
    ptr = &DD[i*N];
    // init ranks as 0:N
    for(int j = 0; j < N; j++) {
      ranks[j] = j;
      inds[j] = j;
    }

    // sort ranks with respect to DD[_,i]
    std::sort(&inds[0], &inds[N],
	      [ptr] (const int l, const int r) {
		return ptr[l] < ptr[r];
	      });
    
    // we are in row i so inds[0] must be i
    //std::cout << "inds[0] = " << inds[0] << "; i = " << i << std::endl;
    if(inds[0] != i) {
      // inds == {5, 6, i, ...}
      int iind = 0;
      while ( inds[iind] != i ) {
	//std::cout << "iind = " << iind << "; N = " << N << std::endl;
	iind++;
	if(iind >= N) error("Error in C code: index out of range"); // can we guarantee that this will never happen?
      }

      for(int j = iind; j > 0; j--) {
	//std::cout << "j = " << j << std::endl;
	inds[j] = inds[j-1];
      }

      inds[0] = i;	
    }

    for(int j = 0; j < N; j++) {
      ranks[inds[j]] = j;
    }

    // std::cout << "DD:" << std::endl;
    // for(int j = 0; j < N; j++) {
    //   std::cout << ptr[j] << ' ';
    // }
    // std::cout << std::endl;
    // std::cout << "inds:" << std::endl;
    // for(int j = 0; j < N; j++) {
    //   std::cout << inds[j] << ' ';
    // }
    // std::cout << std::endl;
    // std::cout << "ranks:" << std::endl;
    // for(int j = 0; j < N; j++) {
    //   std::cout << ranks[j] << ' ';
    // }
    // std::cout << std::endl;

    // std::sort(&ranks[0], &ranks[N],
    // 		     [inds] (const int l, const int r) {
    // 		       return inds[l] < inds[r];
    // 		     });
    
    // put ranks in results

    for(int j = 0; j < N; j++) {
      R[i*N + j] = ranks[j];      
    }   
  }
  free(inds);
  inds = NULL;
  free(ranks);
  ranks = NULL;
}
 
void CORANKING::euclidean(const double* X, const int N,
				 const int D, double* DD) {
  //d_ij = sqrt(||x_i||^2 + ||x_j||^2 - 2pq)

  //vector of square norms ||x_i||^2:
  double* sqnorms = (double*) calloc(N, sizeof(double));
  //if(sqnorms == NULL) { printf("Memory allocation failed!\n"); exit(1); }
  if(sqnorms == NULL) throw 1;
  
  for(int d = 0; d < D; d++) {
    for(int n = 0; n < N; n++) {
      sqnorms[n] += (X[d*N + n] * X[d*N + n]);
    }
  }
  
  for(int n = 0; n < N; n++) {
    for(int m = 0; m < N; m++){
      DD[n*N + m] = sqnorms[n] + sqnorms[m];
    }
  }
  
  double qp;
  for(int i = 0; i < N; i++) {
    DD[i*N + i] = 0;
    for(int j = i+1; j < N; j++) {
      qp = 0.0;
      for(int d = 0; d < D; d++) {
	qp += X[d*N + i] * X[d*N + j];
      }
      DD[i*N + j] -= 2.0 * qp;
      DD[j*N + i] = DD[i*N + j];
    }
  }
  
  for(int i = 0; i < N*N; i++) {
    DD[i] = ( DD[i] < 0 ? 0 : std::sqrt(DD[i]) );
  }

  free(sqnorms);
  sqnorms = NULL;
}
/*
int main () {
  int D = 2;
  int N = 4;


  double data1[] = { 2., 3., 4., 5.,
		     1., 2., 3., 4. };
  double data2[] = { 2., 3., 4., 5.,
		     3., 2., 4., 2. };
  double* dist1 = (double*) calloc( N*N,  sizeof(double));
  int*    rank1 = (int*)    calloc( N*N,    sizeof(int) );
  double* dist2 = (double*) calloc( N*N,  sizeof(double));
  int*    rank2 = (int*)    calloc( N*N,    sizeof(int) );
  int* corank = (int*) calloc( (N-1)*(N-1), sizeof(int) );

  
  CORANKING* cr = new CORANKING();
  
  
  std::cout << "data1:" << std::endl;
  for(int j = 0; j < D; j++){
    for(int i = 0; i < N; i++){
      std::cout << data1[j*N + i] << ' ';
    }
    std::cout << std::endl;
  }
  
  std::cout << "data2:" << std::endl;
  for(int j = 0; j < D; j++){
    for(int i = 0; i < N; i++){
      std::cout << data2[j*N + i] << ' ';
    }
    std::cout << std::endl;
  }

  cr->euclidean(&data1[0], N, D, dist1);

  std::cout << "euclidean 1:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << dist1[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }
  
  cr->euclidean(&data2[0], N, D, dist2);

  std::cout << "euclidean 2:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << dist2[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }
  
  cr->rankmatrix(dist1, N, rank1);

  std::cout << "rank 1:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << rank1[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }
  
  cr->rankmatrix(dist2, N, rank2);

  std::cout << "rank 2:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << rank2[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }

  cr->coranking(rank1, rank2, N, corank);

  std::cout << "coranking:" << std::endl;
  for(int i = 0; i < (N-1); i++){
    for(int j = 0; j < (N-1); j++){
      std::cout << corank[i*(N-1) + j] << ' ';
    }
    std::cout << std::endl;
  }

  
  delete(cr);
  free(dist1); dist1 = NULL;
  free(dist2); dist2 = NULL;
  free(rank1); rank1 = NULL;
  free(rank2); rank2 = NULL;
  free(corank); corank = NULL;

  std::cout << "checking class functions DONE" << std::endl;

  dist1 = (double*) calloc( N*N,  sizeof(double));
  rank1 = (int*)    calloc( N*N,    sizeof(int) );
  dist2 = (double*) calloc( N*N,  sizeof(double));
  rank2 = (int*)    calloc( N*N,    sizeof(int) );
  corank = (int*)   calloc( (N-1)*(N-1), sizeof(int) );

  std::cout << "data1:" << std::endl;
  for(int j = 0; j < D; j++){
    for(int i = 0; i < N; i++){
      std::cout << data1[j*N + i] << ' ';
    }
    std::cout << std::endl;
  }
  
  std::cout << "data2:" << std::endl;
  for(int j = 0; j < D; j++){
    for(int i = 0; i < N; i++){
      std::cout << data2[j*N + i] << ' ';
    }
    std::cout << std::endl;
  }

  CORANKING::euclidean(&data1[0], N, D, dist1);

  std::cout << "euclidean 1:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << dist1[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }
  
  CORANKING::euclidean(&data2[0], N, D, dist2);

  std::cout << "euclidean 2:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << dist2[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }
  
  CORANKING::rankmatrix(dist1, N, rank1);

  std::cout << "rank 1:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << rank1[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }
  
  CORANKING::rankmatrix(dist2, N, rank2);

  std::cout << "rank 2:" << std::endl;
  for(int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      std::cout << rank2[i*N + j] << ' ';
    }
    std::cout << std::endl;
  }

  CORANKING::coranking(rank1, rank2, N, corank);

  std::cout << "coranking:" << std::endl;
  for(int i = 0; i < (N-1); i++){
    for(int j = 0; j < (N-1); j++){
      std::cout << corank[i*(N-1) + j] << ' ';
    }
    std::cout << std::endl;
  }


  free(dist1); dist1 = NULL;
  free(dist2); dist2 = NULL;
  free(rank1); rank1 = NULL;
  free(rank2); rank2 = NULL;
  free(corank); corank = NULL;

  std::cout << "DONE" << std::endl;

}
*/

/*
calcDmat(numericMatrix X) {
  // allocate memory for result
  NumericMatrix dmat(X.nrow(), X.nrow());
  NumericVector p1(X.ncol());
  NumericVector p2(X.ncol());
  // calulate distance between points
  for(int i = 0; i < X.nrow() - 1 ; i++) {
    p1 = X(_, i);
    dmat(i, i) = 0.0;
    for(int j = i + 1; j < X.nrow(); j++) {
      p2 = X(_, j);
      int tmp = _dist(p1, p2);
      dmat[i,j] = tmp;
      dmat[j,i] = tmp;
    }
  }
};

double _dist(NumericVector x1, NumericVector x2) {
  double rr = 0.0;
  for(int i = 0; i < x1.length(); i++) {
    rr += (x1[i] - x2[i])^2;
  }
  rr = math.sqrt(rr);
  return(rr);
}

// distance matrix -> rank matrix
IntegerMatrix rankmatrixCpp(NumericMatrix D) {
  IntegerVector ss(D.nrow());
  IntegerMatrix res(D.nrow(), D.nrow());
  
  for(int j = 0; j < N.nrow(); j++){
    for(int i = 0; i < D.nrow(); i++) {
      ss[i] = i;
    }
    
    sort(ss.begin(), ss.end(),
	 [&](const int& a, const int& b) {
	   return (D[a + D.nrow() * j] < D[b + D.nrow() * j]);
	 }
    );
    res(_, j) = ss;    
  }
  return res;
}



//[[Rcpp::export]]
IntegerMatrix corankingCpp(IntegerMatrix R, IntegerMatrix Ro) {
  IntegerMatrix res(R.nrow(), R.ncol() , 0);
  for(int i = 0; i < R.col(); i++){
    for(int j = 0; j < R.row(); j++){
      res[ R[i,j], Ro[i,j] ] += 1
    }
  }
  rse
}
*/
