#ifndef CORANKING_H
#define CORANKING_H

class CORANKING {
 public:
  static void coranking(const int* Ro, const int* R,
			const int N, int* Q);
  static void rankmatrix(const double* DD,
			 const int N, int* R);
  static void euclidean(const double* X, const int N,
			const int D, double* DD);
 private:

};

#endif
