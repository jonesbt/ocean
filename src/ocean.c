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

double sign(double x1, double x2, double x3,
	      double y1, double y2, double y3) {
  return((x1 - x2) * (y3 - y2) - (x3 - x2) * (y1 - y2));
}

int is_in_triangle(double x_pt, double y_pt,
		   double* x, double* y) {

  double b1 = sign(x_pt, x[0], x[1], y_pt, y[0], y[1]);
  double b2 = sign(x_pt, x[1], x[2], y_pt, y[1], y[2]);
  double b3 = sign(x_pt, x[2], x[0], y_pt, y[2], y[0]);
  return 
    ((b1 < 0.0) && (b2 < 0.0) && (b3 < 0.0)) ||
    ((b1 > 0.0) && (b2 > 0.0) && (b3 > 0.0));
}

int find_element(double x_pt, double y_pt,
		  double* x, double* y, int n_pts,
		  int* tri1, int* tri2, int* tri3) {
  for(int i=0; i<n_pts; i++)
    if(is_in_triangle(x_pt, y_pt,
		      (double[]) {x[tri1[i]], x[tri2[i]], x[tri3[i]]},
		      (double[]) {y[tri1[i]], y[tri2[i]], y[tri3[i]]})) {
      return i;
    }
  return -1;

}

void R_find_element(double* x_pts, double* y_pts, int* n_pts,
		double* x, double* y, int* n_grid_pts,
		  int* tri1, int* tri2, int* tri3,
		  int* elements) {
  for(int i=0; i<n_pts[0]; i++)
    elements[i] = find_element(x_pts[i], y_pts[i],
				x, y, n_grid_pts[0],
				tri1, tri2, tri3);
}
