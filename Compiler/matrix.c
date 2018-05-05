#include <stdio.h>
#include <math.h>
struct mat {
	double* d;
	int width;
	int height;
};

int _mat_index(int width, int i, int j) {
	return i*width + j;
}

void _mat_zero_out(struct mat* m) {
	printf("height: %d width: %d \n", m->height, m->width);
	int size = m->height*m->width;
	for (int i = 0; i < size; ++i)
	{
		m->d[i] = 0.0;
	}
}

void _mat_print(struct mat* m) {
	printf("%d x %d [", m->height, m->width);
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			printf("%.4f ", m->d[idx]);
		}
		if (i != (m->height-1)) printf("|\n");
	}
	printf(" ]\n");
}

void _mat_scalar_add(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] + a;
		}
	}
}

void _mat_scalar_subtract(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] - a;
		}
	}
}

void _mat_scalar_multiply(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] * a;
		}
	}
}

void _mat_scalar_divide(struct mat* m, double a, struct mat* out) {
	for (int i = 0; i < m->height; ++i)
	{
		for (int j = 0; j < m->width; ++j)
		{
			int idx = _mat_index(m->width,i,j);
			out->d[idx] = m->d[idx] / a;
		}
	}
}

void _mat_mat_add(struct mat* l, struct mat* r, struct mat* out) {
	if ((l->width != r->width) || (l->height != r->height)) {
		printf("ERROR: Illegal attempt to add %dx%d matrix and %dx%d matrix.", l->height, l->width, r->height, r->width);
	}
	else {
		for (int i = 0; i < l->height; ++i)
		{
			for (int j = 0; j < l->width; ++j)
			{
				int idx = _mat_index(l->width,i,j);
				out->d[idx] = l->d[idx] + r->d[idx];
			}
		}
	}
}
void _mat_mat_subtract(struct mat* l, struct mat* r, struct mat* out) {
	if ((l->width != r->width) || (l->height != r->height)) {
		printf("ERROR: Illegal attempt to add %dx%d matrix and %dx%d matrix.", l->height, l->width, r->height, r->width);
	}
	else {
		for (int i = 0; i < l->height; ++i)
		{
			for (int j = 0; j < l->width; ++j)
			{
				int idx = _mat_index(l->width,i,j);
				out->d[idx] = l->d[idx] - r->d[idx];
			}
		}
	}
}
void _mat_mat_multiply(struct mat* l, struct mat* r, struct mat* out) {
	for (int i = 0; i < l->height; ++i)
	{
		for (int j = 0; j < r->width; ++j)
		{
			double sum = 0;
			for (int k = 0; k < r->height; ++k) {
				int l_idx = _mat_index(l->width,i,k);
				int r_idx = _mat_index(r->width,k,j);
				sum +=  l->d[l_idx] * r->d[r_idx];
			}
			int idx = _mat_index(out->width, i, j);
			out->d[idx] = sum;
		}
	}
}

void _mat_mat_divide(struct mat* l, struct mat* r, struct mat* out) {}
void _mat_mat_convolute(struct mat* l, struct mat* r, struct mat* out) {}