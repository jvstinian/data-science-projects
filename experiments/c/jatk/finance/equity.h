#ifndef INC_FINANCE_EQUITY_H
#define INC_FINANCE_EQUITY_H
#include <stdint.h>
#include <stddef.h>

struct Date {
    uint16_t year;
    uint8_t month;
    uint8_t day;
};

int datecmp(const struct Date* d2, const struct Date* d1);

struct EquityPrice {
    struct Date date;
    float price;
};

struct Dividend {
    struct Date ex_div_date;
    struct Date payment_date;
    float dividend;
};

struct StockSplit {
    struct Date split_date;
    unsigned int shares_in;
    unsigned int shares_out;
};

/*
struct EquityTotalReturn {
    struct Date start_date;
    struct Date end_date;
    float end_shares;
};
*/

struct EquityPriceHistory;

struct EquityPriceHistory* equity_history_create(
        size_t plen, struct EquityPrice phist[plen],
        size_t slen, struct StockSplit splithist[slen],
        size_t dlen, struct Dividend dhist[dlen]
);
void equity_history_destroy(struct EquityPriceHistory* eh);
size_t equity_dividend_count(const struct EquityPriceHistory* eh);
size_t equity_date_count(const struct EquityPriceHistory* eh);
size_t equity_adjusted_prices(const struct EquityPriceHistory* eh, size_t n, float adj_prices[n]);
float equity_total_return(const struct EquityPriceHistory* eh, float start_amount);

#endif
