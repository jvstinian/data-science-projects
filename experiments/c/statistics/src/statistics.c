#include <statistics.h>
#include <mtwister/mtwister.h>
#include <stdio.h>
#include <stdlib.h>

/* DGESVD prototype */
extern void dgesvd_( char* jobu, char* jobvt, int* m, int* n, double* a,
                int* lda, double* s, double* u, int* ldu, double* vt, int* ldvt,
                double* work, int* lwork, int* info );

double mean(double* data, size_t size) {
    size_t i;
    double ret;
    for (i = 0; i < size; i++) {
        ret += data[i];
    }
    return ret / (double) size;
}

int imin(int a, int b) {
    return (a < b) ? a : b;
}

void print_fortran_matrix(char* desc, int m, int n, double* a, int lda) {
    int i, j;
    printf( "%s\n", desc );
    for(i = 0; i < m; i++) {
            for(j = 0; j < n; j++ ) printf( " %6.2f", a[i+j*lda]);
            printf( "\n" );
    }
}

void simple_lm(unsigned int nobs, unsigned int nvars, double f_arr[nvars][nobs]) {
    int m = nobs, n = nvars;
    int lda = m, ldu = m, ldvt = imin(m, n);
    int info, lwork;
    double wkopt;
    double* work;
    /* Local arrays */
    double s[ldvt]; /* dimension of s is min(m, n) */
    double u[ldu * ldvt], vt[ldvt * n];

    /* Calculate optimal size of work array */
    lwork = -1;
    dgesvd_("S", "S", &m, &n, (double*) f_arr, &lda, s, u, &ldu, vt, &ldvt, &wkopt, &lwork, &info);
    lwork = (int)wkopt;
    work = (double*)malloc(lwork*sizeof(double));
    /* Compute SVD */
    dgesvd_( "S", "S", &m, &n, (double*) f_arr, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, &info );
    /* Check for convergence */
    if( info != 0 ) {
        printf("The algorithm computing SVD failed to converge.  ");
        if (info < 0) {
            printf("Argument %d had an illegal value\n", -info);
        } else { /* info > 0 */
            printf("DBDSQR did not converge");
        }
        free(work);
        exit(1);
    }
    free(work);
    printf("ldvt: %d", ldvt);
    double check_matrix[m * n];
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            double temp = 0.0;
            for (int x = 0; x < ldvt; x++) {
                temp += u[i + ldu * x] * s[x] * vt[x + ldvt * j];
            }
            check_matrix[i + m * j] = temp;
        }
    }
    print_fortran_matrix("u * s * vt", m, n, check_matrix, m);
    /* u * s * vt 
     */
    /* new matrix
     *  4.81   9.23  -6.80   5.02  -1.07
     *  1.21  -7.93   9.76   6.09   7.04
     *  5.95  -7.02   8.40   0.18   0.07
     * -6.98  -2.55  -5.36   0.31   4.19
     *  9.82  -6.41  -6.00  -9.83  -1.43
     * -5.04   3.83  -8.56   2.34   4.53
     */
}

/* Taking ldvt = min(rows, cols),
 * f_u_out is an (fortran) array with dimensions (m, ldvt),
 * f_vt_out is an (fortran) array with dimensions (ldvt, n),
 * and s_out is an array of length ldvt. */
int thin_svd(size_t rows, size_t cols, double f_arr[cols][rows], double* f_u_out, double* s_out, double* f_vt_out) {
    int m = rows, n = cols;
    int lda = m, ldu = m, ldvt = imin(m, n);
    int info, lwork;
    double wkopt;
    double* work;
    /* Local arrays */
    double s[ldvt]; /* dimension of s is min(m, n) */
    double u[ldu * ldvt], vt[ldvt * n];

    /* Calculate optimal size of work array */
    lwork = -1;
    dgesvd_("S", "S", &m, &n, (double*) f_arr, &lda, s_out, f_u_out, &ldu, f_vt_out, &ldvt, &wkopt, &lwork, &info);
    lwork = (int)wkopt;
    work = (double*)malloc( lwork*sizeof(double) );
    if (work == NULL) {
        fprintf(stderr, "thin_svd: could not allocate work array");
        return 1;
    }
    /* Compute SVD */
    dgesvd_( "S", "S", &m, &n, (double*) f_arr, &lda, s_out, f_u_out, &ldu, f_vt_out, &ldvt, work, &lwork, &info );
    /* Check for convergence */
    if( info != 0 ) {
        fprintf(stderr, "thin_svd: the algorithm computing SVD failed to converge\n");
        if (info < 0) {
            fprintf(stderr, "  argument %d had an illegal value\n", -info);
        } else { /* info > 0 */
            fprintf(stderr, "  DBDSQR did not converge\n");
        }
        free(work);
        return 2;
    }
    free(work);
    printf("ldvt: %d", ldvt);
    /*
    double check_matrix[m * n];
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            double temp = 0.0;
            for (int x = 0; x < ldvt; x++) {
                temp += u[i + ldu * x] * s[x] * vt[x + ldvt * j];
            }
            check_matrix[i + m * j] = temp;
        }
    }
    print_fortran_matrix("u * s * vt", m, n, check_matrix, m);
    */
    return 0;
}

void pinv_extended(size_t rows, size_t cols, double f_arr[cols][rows], double rcond, double f_arr_inv_out[rows][cols], double * s_out) {
    size_t minrc = (rows < cols) ? rows : cols;
    double f_u_out[minrc][rows];
    /*double s_out[minrc]; */
    double f_vt_out[cols][minrc];
    double s_recip[minrc];
    size_t i, j, k;
    double max_s, cutoff;

    /* TODO: return if the following returns a non-zero value */
    thin_svd(rows, cols, f_arr, (double*) f_u_out, s_out, (double*) f_vt_out);
    for (i = 0; i < minrc; i++) {
        if (s_out[i] > max_s) {
            max_s = s_out[i];
        }
    }
    cutoff = rcond * max_s;

    for (i = 0; i < minrc; i++) {
        if (s_out[i] > cutoff) {
            s_recip[i] = 1.0 / s_out[i];
        } else {
            s_recip[i] = 0.0;
        }
    }
   
    /* We calculate v * (diag srecip with dim (minrc, minrc)) * u^T, 
     * noting though that in the following we use fortran arrays which
     * are transposed relative to the indexing used in mathematical 
     * notation as well as C arrays */
    for (i = 0; i < cols; i++) {
        for (j = 0; j < rows; j++) {
            f_arr_inv_out[j][i] = 0.0;
            for (k = 0; k < minrc; k++) {
                f_arr_inv_out[j][i] += f_vt_out[i][k] * s_recip[k] * f_u_out[k][j];
            }
            
        }
    }
    /*
     * pinvExtended m rcond = (v <> diagRect 0 srecip rrow rcol <> tr u, s)
           where (u, s, v) = thinSVD $ conj x0\n",
                 rcol = cols u
                 rrow = cols v
                 urow = rows u
                 vrow = rows v
                 cutoff = rcond * maxElement s
                 srecip = cmap (\\x -> if x > cutoff then 1.0 / x else 0.0) (subVector 0 (min urow vrow) s)
     */
}

int main() {
    double data[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    double res = mean(data, 5);
    printf("Mean: %f\n", res);
 
    size_t i;
    MTRand rng = seedRand(123);
    double data2[10000];
    for (i = 0; i < 10000; i++) {
        double urand = genRand(&rng);
        /*
        printf("Random number %lu: %f\n", i + 1, urand);
        */
        data2[i] = urand;
    }
    printf("Mean of uniformly randomly sampled data: %f\n", mean(data2, 10000));
        
    double a[6*5] = {
         4.81,  1.21,  5.95, -6.98,  9.82, -5.04,
         9.23, -7.93, -7.02, -2.55, -6.41,  3.83,
        -6.80,  9.76,  8.40, -5.36, -6.00, -8.56,
         5.02,  6.09,  0.18,  0.31, -9.83,  2.34,
        -1.07,  7.04,  0.07,  4.19, -1.43,  4.53
    };
    simple_lm(6, 5, (double (*)[6]) a);

    double coefs[5] = { 1.0, 1.0, 1.0, 1.0, 1.0 };
    double xs[5][100];
    double ys[100];
    for (i = 0; i < 100; i++) {
        xs[0][i] = genRand(&rng);
        xs[1][i] = genRand(&rng);
        xs[2][i] = genRand(&rng);
        xs[3][i] = genRand(&rng);
        xs[4][i] = 1.0; /* const */
        ys[i] = xs[0][i] * coefs[0] +
                xs[1][i] * coefs[1] +
                xs[2][i] * coefs[2] +
                xs[3][i] * coefs[3] +
                xs[4][i] * coefs[4] +
                (genRand(&rng) - 0.5) / 10.0;
    }
    double pinv[100][5];
    double sv[5];
    pinv_extended(100, 5, xs, 1.0e-8, pinv, sv);
    double coefs_est[5] = { 0.0, 0.0, 0.0, 0.0, 0.0 };
    for (i = 0; i < 100; i++) {
        coefs_est[0] += pinv[i][0] * ys[i];
        coefs_est[1] += pinv[i][1] * ys[i];
        coefs_est[2] += pinv[i][2] * ys[i];
        coefs_est[3] += pinv[i][3] * ys[i];
        coefs_est[4] += pinv[i][4] * ys[i];
    }
    printf("Coefficient estimates: \n");
    for (i = 0; i < 5; i++) {
        printf("%lu: %f\n", i, coefs_est[i]);
    }

    return 0;
}

