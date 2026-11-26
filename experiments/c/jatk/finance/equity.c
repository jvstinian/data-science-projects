#include <equity.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>

/* Estimate the number of days between the two dates */
int datecmp(const struct Date* d2, const struct Date* d1) {
    struct tm date2 = {0};
    struct tm date1 = {0};
    time_t t2, t1;

    /* End date */
    date2.tm_year = d2->year - 1900;
    date2.tm_mon = d2->month - 1;
    date2.tm_mday = d2->day;
    date2.tm_isdst = 0;

    /* Start date */
    date1.tm_year = d1->year - 1900;
    date1.tm_mon = d1->month - 1;
    date1.tm_mday = d1->day;
    date1.tm_isdst = 0;

    /* Convert to epoch */
    t2 = mktime(&date2);
    t1 = mktime(&date1);

    /* Failures are treated a 0 seconds */
    if (t2 == -1) t2 = 0;
    if (t1 == -1) t1 = 0;

    /* Calculate the difference in seconds and convert seconds to days */
    return (int) difftime(t2, t1) / 86400;
}

static int datecmp_void(const void* dv2, const void* dv1) {
    return datecmp((const struct Date*) dv2, (const struct Date*) dv1);
}

static int equityprice_cmp_void(const void* epv2, const void* epv1) {
    const struct EquityPrice* ep2 = (struct EquityPrice*) epv2;
    const struct EquityPrice* ep1 = (struct EquityPrice*) epv1;

    return datecmp(&ep2->date, &ep1->date);
}

static int stocksplit_cmp_void(const void* sv2, const void* sv1) {
    const struct StockSplit* s2 = (struct StockSplit*) sv2;
    const struct StockSplit* s1 = (struct StockSplit*) sv1;

    return datecmp(&s2->split_date, &s1->split_date);
}

static int stocksplit_date_cmp_void(const void* dtv2, const void* sv1) {
    const struct Date* dt2 = (struct Date*) dtv2;
    const struct StockSplit* s1 = (struct StockSplit*) sv1;

    return datecmp(dt2, &s1->split_date);
}

static int dividend_cmp_void(const void* dv2, const void* dv1) {
    const struct Dividend* d2 = (struct Dividend*) dv2;
    const struct Dividend* d1 = (struct Dividend*) dv1;

    return datecmp(&d2->ex_div_date, &d1->ex_div_date);
}

static int dividend_date_cmp_void(const void* dtv2, const void* dv1) {
    const struct Date* dt2 = (struct Date*) dtv2;
    const struct Dividend* d1 = (struct Dividend*) dv1;

    return datecmp(dt2, &d1->ex_div_date);
}

struct IndexedDividend {
    size_t ex_div_date_index;
    size_t payment_date_index;
    float unadj_dividend;
    float adj_dividend_mult;
};

struct IndexedStockSplit {
    size_t split_date_index;
    float split_ratio;
    float adj_mult;
};

struct EquityPriceHistory {
    size_t ndates;
    struct Date* dates;
    float* prices;
    size_t nsplits;
    struct IndexedStockSplit* splits;
    size_t ndivs;
    struct IndexedDividend* dividends;
};

/* Function to implement lower_bound */
int lower_bound(const void* arr, size_t n, size_t size, const void* val, int (*compare_fn)(const void*, const void*))
{
    int mid;
    const void* elem;

    /* Initialise starting index and ending index */
    int low = 0;
    int high = n;

    while (low < high) {
        mid = low + (high - low) / 2;

        /* If *val  is less than or equal to arr[mid], then find in
           left subarray */
        elem = (const void*) ((const uint8_t*) arr + size * mid);
        if (compare_fn(val, /*&arr[mid]*/elem) <= 0) {
            high = mid;
        } else {
            /* If *val is greater arr[mid] then find in right subarray */
            low = mid + 1;
        }
    }
  
    // if X is greater than arr[n-1]
    /*
    if(low < N && arr[low] < X) {
       low++;
    }
    */
      
    // Return the lower_bound index
    return low;
}

/* Function to implement upper_bound */
int upper_bound_void(const void* arr, size_t n, size_t size, const void* val, int (*compare_fn)(const void*, const void*))
{
    int mid;
    const void* elem;

    /* Initialise starting index and ending index */
    int low = 0;
    int high = n;

    while (low < high) {
        /* Find the middle index */
        mid = low + (high - low) / 2;

        /* If *val is greater than or equal to arr[mid] then find
           in right subarray */
        elem = (const void*) ((const uint8_t*) arr + size * mid);
        if (compare_fn(val, /*&arr[mid]*/elem) >= 0) {
            low = mid + 1;
        } else {
            /* If *val is less than arr[mid] then find in left subarray */
            high = mid;
        }
    }
  
    /* if *val is greater than arr[n-1] 
       NOTE: I don't think the following is needed
    if(low < N && arr[low] <= X) {
       low++;
    }
    */

    return low;
}

struct EquityPriceHistory* equity_history_create(
        size_t plen, struct EquityPrice phist[plen],
        size_t slen, struct StockSplit splithist[slen],
        size_t dlen, struct Dividend dhist[dlen]
) {
    struct Date first_date, last_date;
    size_t split_start, split_end, div_start, div_end;
    size_t i;
    size_t date_idx;
    float split_mult;

    if (plen == 0) {
        return NULL;
    }

    struct EquityPriceHistory* eh = malloc(sizeof(struct EquityPriceHistory));
    if (eh == NULL) {
        fprintf(stderr, "equity_history_create: unable to allocate equity price history");
        return NULL;
    }

    /* Sort by date */
    qsort((void*) phist, plen, sizeof(struct EquityPrice), &equityprice_cmp_void);
    qsort((void*) splithist, slen, sizeof(struct StockSplit), &stocksplit_cmp_void);
    qsort((void*) dhist, dlen, sizeof(struct Dividend), &dividend_cmp_void);
    /* Only keep splits and dividends relevant to the provided price history */
    first_date = phist[0].date;
    last_date = phist[plen - 1].date;  /* Note plen > 0 here */
    split_start = upper_bound_void((void*) splithist, slen, sizeof(struct StockSplit), (const void*) &first_date, &stocksplit_date_cmp_void);
    split_end = upper_bound_void((void*) splithist, slen, sizeof(struct StockSplit), (const void*) &last_date, &stocksplit_date_cmp_void);
    /* The fact we use upper bound for both the start and end dates for the allowable range
     * of ex dividend dates requires some explanation.
     * A dividend adjustment requires the previous day's closing price to calculate the
     * dividend adjustment.  Hence we require the ex dividend date of the earliest dividend
     * to be after the first stock price observation date.
     * Hence we find the first dividend to consider using the "upper bound" index
     * for the first stock price observation date.
     * Next, to apply a dividend adjustment to the series, we've decided to require
     * that the ex dividend date of a dividend fall on or before the last price observation
     * date.  The reason for this is to avoid business day logic for establishing date
     * bounds.  For instance, if we had announced dividends falling days after the last stock
     * price observation, then we couldn't determine the dividend adjustment as we don't
     * have the stock price for the date immediately preceding the ex dividend date.
     * We could probably tighten this up to adjust for a dividend with an ex dividend date
     * that immediately follows the last stock price observation (so that we could
     * calculate the dividend adjustment), but we choose to avoid this so as
     * avoid the business day logic needed to determine the business day following
     * the last stock price observation (which could be complicated by a holiday schedule
     * for instance).  As such, we identify the index for the first
     * dividend record whose ex dividend date falls after the last stock price observation,
     * which again is found using the "upper bound" index. */
    div_start = upper_bound_void((void*) dhist, dlen, sizeof(struct Dividend), (const void*) &first_date, &dividend_date_cmp_void);
    div_end = upper_bound_void((void*) dhist, dlen, sizeof(struct Dividend), (const void*) &last_date, &dividend_date_cmp_void);

    eh->ndates = plen;
    eh->dates = malloc(plen * sizeof(struct Date));
    if (eh->dates == NULL) {
        fprintf(stderr, "equity_history_create: unable to allocate list for dates");
        free(eh);
        return NULL;
    }

    eh->prices = malloc(plen * sizeof(float));
    if (eh->prices == NULL) {
        fprintf(stderr, "equity_history_create: unable to allocate list for prices");
        free(eh->dates);
        free(eh);
        return NULL;
    }

    /* Fill in the date and price series */
    for (i = 0; i < plen; i++) {
        eh->dates[i] = phist[i].date;
        eh->prices[i] = phist[i].price;
    }
    
    if (split_end > split_start) {
        eh->splits = malloc((split_end - split_start) * sizeof(struct IndexedStockSplit));
        if (eh->splits == NULL) {
            fprintf(stderr, "equity_history_create: unable to allocate list for splits");
            free(eh->prices);
            free(eh->dates);
            free(eh);
            return NULL;
        }
        eh->nsplits = split_end - split_start;
    } else {
        eh->splits = NULL;
        eh->nsplits = 0;
    }
    
    for (i = split_start; i < split_end; i++) {
        eh->splits[i - split_start].split_ratio = (float) splithist[i].shares_out / splithist[i].shares_in;
        date_idx = lower_bound((void*) eh->dates, plen, sizeof(struct Date), (void*) &splithist[i].split_date, &datecmp_void);
        /* From the lower bound calculation, we must have
         * datecmp(&splithist[i].split_date, &eh->dates[date_idx]) >= 0)
         * Moreover, we expect the split date to be a market date, as so the date index given by the
         * lower bound should correspond to a date in the historical series, i.e.
         * datecmp(&splithist[i].split_date, &eh->dates[date_idx]) == 0)
         * We use this for the condition in the if statement below,
         * except that we generalize to <= 0 so that the condition is exhaustive
         * (to reiterate, we shouldn't encounter < 0). */
        if (datecmp(&splithist[i].split_date, &eh->dates[date_idx]) <= 0) {
            eh->splits[i - split_start].split_date_index = date_idx;
        } else {
            /* This is the unexpected case in which datecmp(&splithist[i].split_date, eh->dates[date_idx]) > 0.
             * In practice this means the date index falls between two market dates.
             * We take the following date to be the split date, i.e. we assign date_idx + 1 here. */
            eh->splits[i - split_start].split_date_index = date_idx + 1;
        }
    }
   
    /* We calculate the reverse accumulated split adjustment multipler */
    i = split_end - split_start;
    split_mult = 1.0;
    while (i > 0) {
        i--;
        split_mult /= eh->splits[i].split_ratio;
        eh->splits[i].adj_mult = split_mult;
    }

    if (div_end > div_start) {
        eh->dividends = malloc((div_end - div_start) * sizeof(struct IndexedDividend));
        if (eh->dividends == NULL) {
            fprintf(stderr, "equity_history_create: unable to allocate list for dividends");
            free(eh->splits);
            free(eh->prices);
            free(eh->dates);
            free(eh);
            return NULL;
        }
        eh->ndivs = div_end - div_start;
    } else {
        eh->dividends = NULL;
        eh->ndivs = 0;
    }

    for (i = div_start; i < div_end; i++) {
        eh->dividends[i - div_start].unadj_dividend = dhist[i].dividend;
        date_idx = lower_bound((void*) eh->dates, plen, sizeof(struct Date), (void*) &dhist[i].ex_div_date, &datecmp_void);
        /* The comments about the values of datecmp(&dhist[i].ex_div_date, &eh->dates[date_idx])
         * and the conditions we use below are analagous to those for the split data. */
        if (datecmp(&dhist[i].ex_div_date, &eh->dates[date_idx]) <= 0) {
            /* The date index corresponds to the ex dividend date */
            eh->dividends[i - div_start].ex_div_date_index = date_idx;
        } else {
            /* This is the unexpected case in which the ex dividend date falls between
             * two market dates. */
            eh->dividends[i - div_start].ex_div_date_index = date_idx + 1;
        }
        /* We calculate the dividend multiplier using the price from the previous day. */
        eh->dividends[i - div_start].adj_dividend_mult = (
                1.0 - dhist[i].dividend / eh->prices[eh->dividends[i - div_start].ex_div_date_index - 1]
        );
        
        /* We now calculate the payment_date_index.  Note that there is no requirement that
         * the payment date is on or before the last day for which we have a stock price.
         * This means that payment_date_index can be >= ndates */
        eh->dividends[i - div_start].payment_date_index = lower_bound(
                (void*) eh->dates, plen, sizeof(struct Date), (void*) &dhist[i].payment_date, &datecmp_void
        );
        /* TODO: Remove the following when ready */
        if (eh->dividends[i - div_start].payment_date_index >= eh->ndates) {
            printf("The dividend payment date falls after the last price observation\n");
        }
            
    }
    
    /* We calculate the reverse accumulated split adjustment multipler */
    i = div_end - div_start;
    /* This avoids the need to track the reverse accumulated multipler */
    /* If i == 0 or i == 1, we do nothing.  If i > 1, then we perform
     * the reverse accumulation. */
    if (i >= 1) i--; 
    while (i > 0) {
        i--;
        eh->dividends[i].adj_dividend_mult *= eh->dividends[i+1].adj_dividend_mult;
    }
    
    return eh;
}

void equity_history_destroy(struct EquityPriceHistory* eh) {
    if (eh->dividends != NULL) {
        free(eh->dividends);
    }
    if (eh->splits != NULL) {
        free(eh->splits);
    }
    free(eh->prices);
    free(eh->dates);
    free(eh);
}

size_t equity_dividend_count(const struct EquityPriceHistory* eh) {
    return eh->ndivs;
}

size_t equity_date_count(const struct EquityPriceHistory* eh) {
    return eh->ndates;
}

size_t equity_adjusted_prices(const struct EquityPriceHistory* eh, size_t n, float adj_prices[n]) {
    size_t plen = (eh->ndates < n) ? eh->ndates : n;
    size_t i, si, di;
    float split_mult, div_mult;

    si = 0;
    di = 0;

    for (i = 0; i < plen; i++) {
        while (si < eh->nsplits &&    eh->splits[si].split_date_index  <= i) si++;
        while (di < eh->ndivs   && eh->dividends[di].ex_div_date_index <= i) di++;
        if (si < eh->nsplits) {
            split_mult = eh->splits[si].adj_mult;
        } else {
            split_mult = 1.0;
        }
        if (di < eh->ndivs) {
            div_mult = eh->dividends[di].adj_dividend_mult;
        } else {
            div_mult = 1.0;
        }

        adj_prices[i] = div_mult * split_mult * eh->prices[i];
    }

    return plen;
}

float equity_total_return(const struct EquityPriceHistory* eh, float start_amount) {
    float shares = start_amount / eh->prices[0];
    size_t plen = eh->ndates;
    size_t i, si, di;

    i = 0;
    si = 0;
    di = 0;
    while (i < plen) {
        if (si < eh->nsplits && di < eh->ndivs) {
            if (eh->dividends[di].payment_date_index <= eh->splits[si].split_date_index) {
                i = eh->dividends[di].payment_date_index;
                shares += shares * eh->dividends[di].unadj_dividend / eh->prices[i];
                /* The following takes into account a pecularity of dividends,
                 * in that there's an ex dividend date preceding the actual payment date.
                 * We consider all dividends with an ex dividend date after the first
                 * observation date and on or before the last observation date.
                 * However, the payment date can fall after the last observation date.
                 * Since for total returns we consider the dividend payment date rather
                 * than the ex dividend date, this means that we have to ignore
                 * those dividend payments falling after the last observation date.
                 * For this reason we increment the index until we find another dividend
                 * with payment date within the observation price window or until we reach
                 * the end of the dividend list. */
                do {
                    di++;
                } while (di < eh->ndivs && eh->dividends[di].payment_date_index >= eh->ndates);
            } else {
                i = eh->splits[si].split_date_index;
                shares *= eh->splits[si].split_ratio;
                si++;
            }
        } else if (di < eh->ndivs) {
            /* There are no more splits. */
            i = eh->dividends[di].payment_date_index;
            shares += shares * eh->dividends[di].unadj_dividend / eh->prices[i];
            /* See the comments about the dividend payment date above. */
            do {
                di++;
            } while (di < eh->ndivs && eh->dividends[di].payment_date_index >= eh->ndates);
        } else if (si < eh->nsplits) {
            /* There are no more dividends */
            i = eh->splits[si].split_date_index;
            shares *= eh->splits[si].split_ratio;
            si++;
        } else {
            /* We've processed all the splits and dividends */
            i = plen;
        }
    }

    return shares * eh->prices[eh->ndates - 1];
}


struct EquityPriceHistory* equity_aprc_adiv_history_create(
        size_t plen, struct EquityPrice aphist[plen],
        size_t slen, struct StockSplit splithist[slen],
        size_t dlen, struct Dividend adhist[dlen]
) {
    struct Date first_date, last_date;
    size_t split_start, split_end, div_start, div_end;
    size_t i;
    size_t date_idx;
    float split_mult;
    size_t ex_div_date_idx, prior_ex_div_date_idx, pi; /* For reversing the dividend adjustment */
    size_t di; /* For iterating over dividends */
    size_t si; /* Split index, used when reversing the split adjustments */
    float revacc_div_mult; /* The reverse accumulated dividend adjustment */
    float adj_dividend;
    float div_adj_prior_close_price, div_unadj_prior_close_price;

    if (plen == 0) {
        return NULL;
    }

    struct EquityPriceHistory* eh = malloc(sizeof(struct EquityPriceHistory));
    if (eh == NULL) {
        fprintf(stderr, "equity_aprc_adiv_history_create: unable to allocate equity price history");
        return NULL;
    }

    /* Sort by date */
    qsort((void*) aphist, plen, sizeof(struct EquityPrice), &equityprice_cmp_void);
    qsort((void*) splithist, slen, sizeof(struct StockSplit), &stocksplit_cmp_void);
    qsort((void*) adhist, dlen, sizeof(struct Dividend), &dividend_cmp_void);
    /* Only keep splits and dividends relevant to the provided price history */
    first_date = aphist[0].date;
    last_date = aphist[plen - 1].date;  /* Note plen > 0 here */
    split_start = upper_bound_void((void*) splithist, slen, sizeof(struct StockSplit), (const void*) &first_date, &stocksplit_date_cmp_void);
    split_end = upper_bound_void((void*) splithist, slen, sizeof(struct StockSplit), (const void*) &last_date, &stocksplit_date_cmp_void);
    /* The fact we use upper bound for both the start and end dates for the allowable range
     * of ex dividend dates requires some explanation.
     * A dividend adjustment requires the previous day's closing price to calculate the
     * dividend adjustment.  Hence we require the ex dividend date of the earliest dividend
     * to be after the first stock price observation date,
     * So we identify the first dividend to consider using the "upper bound" index
     * for the first stock price observation date.
     * Next, to apply a dividend adjustment to the series, we've decided to require
     * that the ex dividend date of a dividend fall on or before the last price observation
     * date.  The reason for this is to avoid business day logic for establishing date
     * bounds.  For instance, if we had announced dividends falling days after the last stock
     * price observation, then we couldn't determine the dividend adjustment as we don't
     * have the stock price for the date immediately preceding the ex dividend date.
     * We could probably tighten this up to adjust for a dividend with an ex dividend date
     * that immediately follows the last stock price observation (so that we could
     * calculate the dividend adjustment), but we choose to avoid this so as
     * avoid the business day logic needed to determine the business day following
     * the last stock price observation (which could be complicated by a holiday schedule
     * for instance).  As such, we identify the index for the first
     * dividend record whose ex dividend date falls after the last stock price observation,
     * which again is found using the "upper bound" index. */
    div_start = upper_bound_void((void*) adhist, dlen, sizeof(struct Dividend), (const void*) &first_date, &dividend_date_cmp_void);
    div_end = upper_bound_void((void*) adhist, dlen, sizeof(struct Dividend), (const void*) &last_date, &dividend_date_cmp_void);

    eh->ndates = plen;
    eh->dates = malloc(plen * sizeof(struct Date));
    if (eh->dates == NULL) {
        fprintf(stderr, "equity_aprc_adiv_history_create: unable to allocate list for dates");
        free(eh);
        return NULL;
    }

    eh->prices = malloc(plen * sizeof(float));
    if (eh->prices == NULL) {
        fprintf(stderr, "equity_aprc_adiv_history_create: unable to allocate list for prices");
        free(eh->dates);
        free(eh);
        return NULL;
    }

    /* Fill in the date and price series */
    /* Note here that we are assigning the adjusted price to the
     * unadjusted price series.  We will overwrite the value later with
     * the unadjusted price. */
    for (i = 0; i < plen; i++) {
        eh->dates[i] = aphist[i].date;
        eh->prices[i] = aphist[i].price;
    }
    
    if (split_end > split_start) {
        eh->splits = malloc((split_end - split_start) * sizeof(struct IndexedStockSplit));
        if (eh->splits == NULL) {
            fprintf(stderr, "equity_aprc_adiv_history_create: unable to allocate list for splits");
            free(eh->prices);
            free(eh->dates);
            free(eh);
            return NULL;
        }
        eh->nsplits = split_end - split_start;
    } else {
        eh->splits = NULL;
        eh->nsplits = 0;
    }
    
    for (i = split_start; i < split_end; i++) {
        eh->splits[i - split_start].split_ratio = (float) splithist[i].shares_out / splithist[i].shares_in;
        date_idx = lower_bound((void*) eh->dates, plen, sizeof(struct Date), (void*) &splithist[i].split_date, &datecmp_void);
        /* From the lower bound calculation, we must have
         * datecmp(&splithist[i].split_date, &eh->dates[date_idx]) >= 0)
         * Moreover, we expect the split date to be a market date, so the date index given by the
         * lower bound should correspond to a date in the historical series, i.e.
         * datecmp(&splithist[i].split_date, &eh->dates[date_idx]) == 0 .
         * We use this for the condition in the if statement below,
         * except that we generalize to <= 0 so that the condition is exhaustive
         * (to reiterate, we shouldn't encounter < 0). */
        if (datecmp(&splithist[i].split_date, &eh->dates[date_idx]) <= 0) {
            eh->splits[i - split_start].split_date_index = date_idx;
        } else {
            /* This is the unexpected case in which datecmp(&splithist[i].split_date, eh->dates[date_idx]) > 0.
             * In practice this means the date index falls between two market dates.
             * We take the following date to be the split date, i.e. we assign date_idx + 1 here. */
            eh->splits[i - split_start].split_date_index = date_idx + 1;
        }
    }
   
    /* We calculate the reverse accumulated split adjustment multipler */
    i = split_end - split_start;
    split_mult = 1.0;
    while (i > 0) {
        i--;
        split_mult /= eh->splits[i].split_ratio;
        eh->splits[i].adj_mult = split_mult;
    }
   
    /* TODO: This is handled below instead.
     * We reverse the split adjustment to the price series.
     * After this the prices will only be dividend adjusted.
    si = 0;
    for (i = 0; i < plen; i++) {
        while (si < eh->nsplits && eh->splits[si].split_date_index  <= i) si++;
        if (si < eh->nsplits) {
            split_mult = eh->splits[si].adj_mult;
        } else {
            split_mult = 1.0;
        }
        eh->prices[i] /= split_mult;
    }
    */

    if (div_end > div_start) {
        eh->dividends = malloc((div_end - div_start) * sizeof(struct IndexedDividend));
        if (eh->dividends == NULL) {
            fprintf(stderr, "equity_aprc_adiv_history_create: unable to allocate list for dividends");
            free(eh->splits);
            free(eh->prices);
            free(eh->dates);
            free(eh);
            return NULL;
        }
        eh->ndivs = div_end - div_start;
    } else {
        eh->dividends = NULL;
        eh->ndivs = 0;
    }

    i = div_end;
    revacc_div_mult = 1.0;
    /* for (i = div_start; i < div_end; i++) */
    while (i > div_start) {
        i--;
        adj_dividend = adhist[i].dividend;

        /* Note that we assign the adjusted dividend here.  The adjustment will be reversed below. */
        eh->dividends[i - div_start].unadj_dividend = adj_dividend;
        date_idx = lower_bound((void*) eh->dates, plen, sizeof(struct Date), (void*) &adhist[i].ex_div_date, &datecmp_void);
        /* The comments about the values of datecmp(&adhist[i].ex_div_date, &eh->dates[date_idx])
         * and the conditions we use below are analagous to those for the split data. */
        if (datecmp(&adhist[i].ex_div_date, &eh->dates[date_idx]) <= 0) {
            /* The date index corresponds to the ex dividend date */
            eh->dividends[i - div_start].ex_div_date_index = date_idx;
        } else {
            /* This is the unexpected case in which the ex dividend date falls between
             * two market dates. */
            eh->dividends[i - div_start].ex_div_date_index = date_idx + 1;
        }
        /* TODO: We calculate the dividend multiplier using the price from the previous day.
         * NOTE: We will calculate the adjusted dividend multiplier 
        eh->dividends[i - div_start].adj_dividend_mult = (
                1.0 - adhist[i].dividend / eh->prices[eh->dividends[i - div_start].ex_div_date_index - 1]
        );
        */
        
        /* We now calculate the payment_date_index.  Note that there is no requirement that
         * the payment date is on or before the last day for which we have a stock price.
         * This means that payment_date_index can be >= ndates */
        eh->dividends[i - div_start].payment_date_index = lower_bound(
                (void*) eh->dates, plen, sizeof(struct Date), (void*) &adhist[i].payment_date, &datecmp_void
        );
        /* TODO: Remove the following when ready */
        if (eh->dividends[i - div_start].payment_date_index >= eh->ndates) {
            printf("The dividend payment date falls after the last price observation\n");
        }
        
        /* We calculate the dividend multiplier adjustment here using
         * the adjusted dividend and adjusted price. */
        ex_div_date_idx = eh->dividends[i - div_start].ex_div_date_index;
        /* The use of the reverse accumulated dividend multiplier in the following might
         * be confusing.  At this point, prices for dates before ex_div_date_idx
         * have not been modified.  These prices are both split and dividend adjusted,
         * including the current dividend considered as well as all the following dividends.
         * At this pointn the reverse accumulated dividend multiplier represents the
         * multiplier for all the following dividends (not including the current dividend
         * under consideration).  After dividing the preceding price by this multiplier,
         * the resulting value should only reflect the split adjustment
         * and the adjustment for the current dividend. */
        div_adj_prior_close_price = eh->prices[ex_div_date_idx - 1] / revacc_div_mult;
        /* As we know the adjusted dividend and the price only adjusted for the
         * current dividend, we can calculate the price for the prior close
         * (that is only adjusted for splits, which we indicate by "div_unadj" here). */
        div_unadj_prior_close_price = div_adj_prior_close_price + adj_dividend;
        /* Now that we know both the split adjusted dividend and the split (but not dividend)
         * adjusted preceding price, we can update the
         * reverse accumulated dividend multiplier,
         * and use this to reverse the dividend adjustment for prices preceding the
         * current dividend. */
        revacc_div_mult *= (1.0 - adj_dividend / div_unadj_prior_close_price);
        eh->dividends[i - div_start].adj_dividend_mult = revacc_div_mult;
        /* We now go through the prices preceding the current dividend ex dividend date,
         * down to the ex dividend date for the preceding dividend, and use
         * the reverse accumulate dividend multiplier to determine the
         * dividend unadjusted (but still split adjusted) price. */
        if (i > div_start) {
            prior_ex_div_date_idx = eh->dividends[i - div_start - 1].ex_div_date_index;
        } else {
            /* The current dividend under consideration is the earliest
             * relevant dividend, and so we modify prices back to the start
             * of the series. */
            prior_ex_div_date_idx = 0;
        }
        for (pi = prior_ex_div_date_idx; pi < ex_div_date_idx; pi++) {
            eh->prices[pi] /= revacc_div_mult;
        }
    }

    /* At this point the prices are split adjusted but no longer dividend adjusted.
     * The dividend amounts are also split adjusted. */

    /* We reverse the split adjustment to the price series.
     * After this the prices will only be dividend adjusted. */
    si = 0;
    for (i = 0; i < plen; i++) {
        while (si < eh->nsplits && eh->splits[si].split_date_index  <= i) si++;
        if (si < eh->nsplits) {
            split_mult = eh->splits[si].adj_mult;
        } else {
            split_mult = 1.0;
        }
        eh->prices[i] /= split_mult;
    }

    /* We reverse the split adjustment to the dividend series. */
    si = 0;
    for (di = 0; di < eh->ndivs; di++) {
        /* Get the ex dividend date index */
        i = eh->dividends[di].ex_div_date_index;
        /* Precede as before by finding the split that applies to this
         * date index */
        while (si < eh->nsplits && eh->splits[si].split_date_index  <= i) si++;
        if (si < eh->nsplits) {
            split_mult = eh->splits[si].adj_mult;
        } else {
            split_mult = 1.0;
        }
        eh->dividends[di].unadj_dividend /= split_mult;
    }
    
   
    /* TODO: Remove the following when ready */
    /* We calculate the reverse accumulated split adjustment multipler
    i = div_end - div_start;
    */
    /* This avoids the need to track the reverse accumulated multipler */
    /* If i == 0 or i == 1, we do nothing.  If i > 1, then we perform
     * the reverse accumulation.
    if (i >= 1) i--; 
    while (i > 0) {
        i--;
        eh->dividends[i].adj_dividend_mult *= eh->dividends[i+1].adj_dividend_mult;
    }
    */
    
    return eh;
}
