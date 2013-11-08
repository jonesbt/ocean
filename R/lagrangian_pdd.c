#include <R.h>
#include <Rinternals.h>

int n_in_box(double *x, double *y, int npts, 
	     double x0, double x1, double y0, double y1) {
  int sum = 0;
  for(int i=0; i < npts; ++i)
    if((x[i] > x0) & (x[i] < x1) & (y[i] > y0) & (y[i] < y1))
      ++sum;
  return sum;
}

void bin_data(double *x, double *y, int npts, 
	      double *gridx, double *gridy, int ngridx, int ngridy,
	      int *grid) {
  for(int j=0; j < ngridy; ++j)
    for(int i=0; i < ngridx; ++i)
      grid[(j * ngridx) + i] = n_in_box(x, y, npts,
					    gridx[i], gridx[i + 1],
					    gridy[j], gridy[j + 1]);
}

void R_bin_data(double *x, double *y, int *npts,
		double *gridx, double *gridy, int *ngridx, int *ngridy,
		int *grid) {
  bin_data(x, y, npts[0], gridx, gridy, ngridx[0], ngridy[0], grid);
}

void R_filter_data() {}

