#include <statistics.h>
#include <mtwister/mtwister.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* memcpy */

/* DGESVD prototype */
extern void dgesvd_( char* jobu, char* jobvt, int* m, int* n, double* a,
                int* lda, double* s, double* u, int* ldu, double* vt, int* ldvt,
                double* work, int* lwork, int* info );

double mean(size_t size, double data[size]) {
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

/*
pinvExtended :: Matrix Double -> Double -> (Matrix Double, Vector Double)
pinvExtended m rcond = (v <> diagRect 0 srecip rrow rcol <> tr u, s)
    where (u, s, v) = thinSVD $ conj x0
          rcol = cols u
          rrow = cols v
          urow = rows u
          vrow = rows v
          cutoff = rcond * maxElement s
          srecip = cmap (\x -> if x > cutoff then 1.0 / x else 0.0) (subVector 0 (min urow vrow) s)
*/

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

enum covariance_kind {
    NON_ROBUST_COVARIANCE
};

typedef enum Boolean {
    FALSE,
    TRUE
} Boolean;

struct ols_results {
    size_t rows;
    size_t cols;
    double* exog_mat;
    double* endog;
    double* beta;
    double* normalized_cov_params_mat;
    enum covariance_kind cov_kind;
    Boolean use_t;
};

struct ols_results* fit_ols(size_t rows, size_t cols, double f_exog_mat[cols][rows], double endog[rows]) {
    size_t r, c, c2;
    struct ols_results* res = malloc(sizeof(struct ols_results));
    if (res == NULL) {
        fprintf(stderr, "fit_ols: could not allocate ols results");
        return NULL;
    }
    res->rows = rows;
    res->cols = cols;
    res->exog_mat = malloc(rows * cols * sizeof(double));
    if (res->exog_mat == NULL) {
        fprintf(stderr, "fit_ols: could not allocate exogenous matrix");
        free(res);
        return NULL;
    }
    res->endog = malloc(rows * sizeof(double));
    if (res->endog == NULL) {
        fprintf(stderr, "fit_ols: could not allocate endogenous vector");
        free(res->exog_mat);
        free(res);
        return NULL;
    }
    res->beta = malloc(rows * sizeof(double));
    if (res->beta == NULL) {
        fprintf(stderr, "fit_ols: could not allocate beta");
        free(res->endog);
        free(res->exog_mat);
        free(res);
        return NULL;
    }
    res->normalized_cov_params_mat = malloc(rows * rows * sizeof(double));
    if (res->normalized_cov_params_mat == NULL) {
        fprintf(stderr, "fit_ols: could not allocate beta");
        free(res->beta);
        free(res->endog);
        free(res->exog_mat);
        free(res);
        return NULL;
    }
    res->cov_kind = NON_ROBUST_COVARIANCE;
    res->use_t = FALSE;

    for (r = 0; r < rows; r++) {
        for (c = 0; c < cols; c++) {
            res->exog_mat[r * cols + c] = f_exog_mat[c][r];
        }
    }
    memcpy(res->endog, endog, rows * sizeof(double));
    double (*f_pinv)[cols] = malloc(rows * sizeof(double[cols]));
    double* sv = malloc(cols * sizeof(double));
    /* Try 1.0e-15 for rcond in the following */
    pinv_extended(rows, cols, f_exog_mat, 1.0e-8, f_pinv, sv);
    for (c = 0; c < cols; c++) {
        res->beta[c] = 0.0;
        for (r = 0; r < rows; r++) {
            res->beta[c] += f_pinv[r][c] * endog[r];
        }
    }
    /* normalized_cov_params is pinv * transpose(pinv) = transpose(f_pinv) * f_pinv */
    for (c = 0; c < cols; c++) {
        for (c2 = 0; c2 < cols; c2++) {
            res->normalized_cov_params_mat[c * cols + c2] = 0.0;
            for (r = 0; r < rows; r++) {
                res->normalized_cov_params_mat[c * cols + c2] += f_pinv[r][c] * f_pinv[r][c2];
            }
        }
    }

    size_t rank = 0;
    /* TODO: sv actually has dimension min(rows, cols) */
    for (c = 0; c < cols; c++) {
        if (sv[c] != 0.0) {
            rank++;
        }
    }
    printf("Rank: %lu", rank);
    /*
    wexog = x
    wendog = y
    (pinvWexog, singularValues) = pinvExtended wexog 1e-15
    normalized_cov_params = pinvWexog <> tr pinvWexog -- TODO: Should this be tr'?
    -- Cache these singular values for use later.
    wexogSingularValues = singularValues
    dataRank = rank $ diag singularValues -- following statsmodels, but is this just the number of non-zero singular values, possibly with a cutoff
    beta = pinvWexog `app` wendog
    in OLSResults beta normalized_cov_params NonRobustCovariance True (wexog, wendog)
    */
}

double lm_predict(struct ols_results* fit, double x[fit->cols]) {
    double predict_out;
    size_t c;

    predict_out = 0.0;
    for(c = 0; c < fit->cols; c++) {
        predict_out += x[c] * fit->beta[c];
    }
    return predict_out;
}

void lm_predict_batch(const struct ols_results* fit, size_t batch_obs, double x[batch_obs][fit->cols], double predict_out[batch_obs]) {
    size_t r, c;

    for(r = 0; r < batch_obs; r++) {
        predict_out[r] = 0.0;
        for(c = 0; c < fit->cols; c++) {
            predict_out[r] += x[r][c] * fit->beta[c];
        }
    }
}

size_t lm_nobs(const struct ols_results* fit) {
    return fit->rows;
}

void lm_predict_fittedvalues(const struct ols_results* fit, double fittedvalues[fit->rows]) {
    /* TODO: Does the cast in the following work? */
    lm_predict_batch(fit, fit->rows, (double (*)[fit->cols]) fit->exog_mat, fittedvalues);
}

int lm_resid(const struct ols_results* fit, double resid[fit->rows]) {
    double* fvs = malloc(fit->rows * sizeof(double));
    if (fvs == NULL) {
        fprintf(stderr, "lm_predict_resid: could not allocate fitted values");
        return 1;
    }
    size_t r;

    lm_predict_fittedvalues(fit, fvs);
    for(r = 0; r < fit->rows; r++) {
        resid[r] = fit->endog[r] - fvs[r];
    }
    free(fvs);
    return 0;
}

inline double norm_2_sq(size_t len, double x[len]) {
    size_t r;
    double ret = 0.0;

    for(r = 0; r < len; r++) {
        ret += x[r] * x[r];
    }
    return ret;
}

static inline double shifted_norm_2_sq(size_t len, double x[len], double shift) {
    size_t r;
    double ret = 0.0;

    for(r = 0; r < len; r++) {
        ret += (x[r] - shift) * (x[r] - shift);
    }
    return ret;
}

int lm_ssr(const struct ols_results* fit, double* ssr) {
    double* fvs = malloc(fit->rows * sizeof(double));
    if (fvs == NULL) {
        fprintf(stderr, "lm_predict_resid: could not allocate fitted values");
        return 1;
    }
    
    lm_resid(fit, fvs);
    *ssr = norm_2_sq(fit->rows, fvs);

    free(fvs);
    return 0;
}

double lm_centered_tss(const struct ols_results* fit) {
    double m = mean(fit->rows, fit->endog);
    return shifted_norm_2_sq(fit->rows, fit->endog, m);
}

double lm_uncentered_tss(const struct ols_results* fit) {
    return norm_2_sq(fit->rows, fit->endog);
}

int lm_rsquared(const struct ols_results* fit, Boolean hasconst, double* rsquared_out) {
    double ssr;
    if (lm_ssr(fit, &ssr)) {
        fprintf(stderr, "lm_rsquared: error in lm_ssr");
        return 1;
    }

    if (hasconst) {
        *rsquared_out = 1.0 - ssr / lm_centered_tss(fit);
    } else {
        *rsquared_out = 1.0 - ssr / lm_uncentered_tss(fit);
    }
    return 0;
}

/* The explained sum of squares. 
 * If a constant is present, the centered total sum of squares minus the 
 * sum of squared residuals. If there is no constant, the uncentered total 
 * sum of squares is used. */
int lm_ess(const struct ols_results* fit, Boolean hasconst, double* ess) {
    double ssr;
    if (lm_ssr(fit, &ssr)) {
        fprintf(stderr, "lm_ess: error in lm_ssr");
        return 1;
    }

    if (hasconst) {
        return lm_centered_tss(fit) - ssr;
    } else {
        return lm_uncentered_tss(fit) - ssr;
    }
    return 0;
}

/*
-- TODO: Some additional work is needed here.
-- Adjusted R-squared.
-- This is defined here as 1 - (`nobs`-1)/`df_resid` * (1-`rsquared`)
-- if a constant is included and 1 - `nobs`/`df_resid` * (1-`rsquared`) if
-- no constant is included.
-- rsquared_adj :: OLSResults -> Bool -> Double
-- rsquared_adj ols hasconst = 1 - adj * (1 - rsquared ols hasconst)
--     where k_constant = if hasconst then 1 else 0
--           adj = (fromIntegral (nobs ols - k_constant) / fromIntegral df_resid)
--           dataRank = rank $ diag singularValues -- TODO
--           df_resid = nobs ols - dataRank

-- let k_constant = 1 -- based on the way we defined x0
--     nobs = rows wexog
--     df_model = dataRank - k_constant
--     df_resid = nobs - dataRank
*/

int main() {
    double data[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    double res = mean(5, data);
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
    printf("Mean of uniformly randomly sampled data: %f\n", mean(10000, data2));
 
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

