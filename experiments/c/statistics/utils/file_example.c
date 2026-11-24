#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

enum DataType {
    STRING,
    UINT32,
    INT32,
    UINT64,
    INT64,
    FLOAT,
    DOUBLE
};

size_t datatype_sizeof(enum DataType dtype) {
    switch (dtype) {
        case STRING:
            return sizeof(char*);
        case UINT32:
        case INT32:
            return 4;
        case UINT64:
        case INT64:
            return 8;
        case FLOAT:
            return sizeof(float);
        case DOUBLE:
            return sizeof(double);
    }
}


struct DataField {
    char* field_name;
    enum DataType dtype;
};

struct DataSchema {
    size_t capacity;
    size_t length;
    struct DataField fields[1];
};

/* Data Schema */
struct DataSchema* schema_create(size_t cpty){
    if (cpty < 1) {
        cpty = 1;
    }
	struct DataSchema* ret = malloc(sizeof(struct DataSchema) + (cpty-1)*sizeof(struct DataField));
	if (ret != NULL) {
		ret->capacity = cpty;
		ret->length = 0;
	}
	return ret;
}

int schema_realloc(struct DataSchema** spp, size_t new_capacity){
    *spp = realloc(*spp, sizeof(struct DataSchema) + (new_capacity-1)*sizeof(struct DataField));
    if (*spp == NULL) {
        return -1;
    }
    (*spp)->capacity = new_capacity;
	return 0;
}

int schema_push(struct DataSchema** spp, struct DataField val){
	(*spp)->fields[(*spp)->length] = val;
	(*spp)->length++;

	if ((*spp)->length >= (*spp)->capacity) {
		/* realloc */
		int newcap = 2 * (*spp)->capacity;
		*spp = realloc(*spp, sizeof(struct DataSchema) + (newcap-1)*sizeof(struct DataField));
		if (*spp == NULL) {
			return -1;
		}
		(*spp)->capacity = newcap;
	}
	return 0;
}

size_t schema_number_of_fields(const struct DataSchema* sp){
    return sp->length;
}

enum DataType schema_get_type(const struct DataSchema* sp, size_t idx){
    assert(idx < sp->length);
    return sp->fields[idx].dtype;
}

struct DataSchema* schema_dup(struct DataSchema* sp){
    size_t cpty = sp->capacity;
    if (cpty < 1) {
        cpty = 1;
    }
	struct DataSchema* ret = malloc(sizeof(struct DataSchema) + (cpty-1)*sizeof(struct DataField));
	if (ret != NULL) {
		ret->capacity = cpty;
		ret->length = sp->length;
        memcpy(ret->fields, sp->fields, sp->length*sizeof(struct DataField));
	}
	return ret;
}


void schema_destroy(struct DataSchema* sp) {
	free(sp);
}

struct DataFrame {
    size_t rowcapacity;
    size_t rmult;
    size_t nrows;
    size_t ncols;
    struct DataSchema* schema;
    size_t* offsets;
    size_t* field_sizes;
    size_t total_record_size;
    unsigned char* data;
};

struct DataFrame* dataframe_create(size_t rcpty, struct DataSchema* sp){
    struct DataFrame* ret = malloc(sizeof(struct DataFrame));
    if (ret == NULL) {
        return NULL;
    }

    /* Duplicate the schema */
    ret->schema = schema_dup(sp);
    if (ret->schema == NULL) {
        free(ret);
        return NULL;
    }

    size_t fields = schema_number_of_fields(sp);
    ret->rmult = 2u;
    ret->ncols = fields;
    ret->offsets = malloc(fields * sizeof(size_t));
    if (ret->offsets == NULL) {
        free(ret->schema);
        free(ret);
        return NULL;
    }
    ret->field_sizes = malloc(fields * sizeof(size_t));
    if (ret->field_sizes == NULL) {
        free(ret->offsets);
        free(ret->schema);
        free(ret);
        return NULL;
    }
    size_t required_rmult;
    size_t cum_offset = 0;
    size_t field_size;
    size_t max_field_size = 1;
    size_t i;
    for(i = 0; i < fields; i++) {
        ret->offsets[i] = cum_offset;
        field_size = datatype_sizeof(schema_get_type(sp, i));
        ret->field_sizes[i] = field_size;
        /* For i > 0, we find the value j such that
         * \sum_{i <= j} s_i <= j * \sum_{i < j}
         * which is equivalent to 
         * s_j <= (j - 1) * \sum_{i < j} .
         * We can then find the mininum value for (j - 1) by looking at the
         * quotient (s_j + \sum_{i < j} - 1) / \sum_{i < j} .
         * We add 1 to get the minimum required value for j. */
        if (i > 0) {
            required_rmult = 1u + (field_size + cum_offset - 1) / cum_offset;
            if (required_rmult > ret->rmult) {
                ret->rmult = required_rmult;
            }
        }
        /* Update cum_offset for the next iteration */
        cum_offset += field_size;
        max_field_size = field_size > max_field_size ? field_size : max_field_size;
    }

    /* cum_offset is now the size of a record.  We store the value as the
     * total_record_size in the data frame struct. */
    ret->total_record_size = cum_offset;

    /* While not necessary and a bit arbitrary, we set the initial row capacity be at least
     * as large as the number of columns. */
    if (rcpty < ret->ncols) {
        rcpty = ret->ncols;
    }
    ret->rowcapacity = rcpty;

    /* We determine the the total initial capacity of the data set 
     * using the record size and initial row capacity */
	ret->data = malloc(ret->rowcapacity * ret->total_record_size * sizeof(unsigned char));
    if (ret->data == NULL) {
        free(ret->field_sizes);
        free(ret->offsets);
        free(ret->schema);
        free(ret);
        return NULL;
    }
    ret->nrows = 0;

	return ret;
}

int dataframe_data_realloc(struct DataFrame* dfp){
    size_t old_row_capacity = dfp->rowcapacity;
    size_t new_row_capacity = dfp->rowcapacity * dfp->rmult;
    size_t new_data_capacity = new_row_capacity * dfp->total_record_size;
    size_t i, offset, field_size;

    printf("Diagnostic: calling realloc for dataframe data\n");
    dfp->data = realloc(dfp->data, new_data_capacity * sizeof(unsigned char));
    if (dfp->data == NULL) {
        return -1;
    }
    dfp->rowcapacity = new_row_capacity;

    /* TODO: The following use of the for loop is incorrect.
     *       One must instead descend from dfp->ncols - 1 to 0.
     *       Use 
     *       i = dfp->ncols; while (i > 1) { i--; ... }
     *       instead. */
    for (i = 1; i < dfp->ncols; i++) {
        offset = dfp->offsets[i];
        field_size = dfp->field_sizes[i];
        memcpy(dfp->data + new_row_capacity * offset, dfp->data + old_row_capacity * offset, old_row_capacity * field_size);
    }
	return 0;
}

void dataframe_print(struct DataFrame* dfp){
    size_t r, c;
    char** strs;
    float* flts;
    double* dbls;

    for (c = 0; c < dfp->ncols; c++) {
        /* To get here we must have 0 < dfp->ncols */
        if (c < (dfp->ncols - 1)) {
            printf("%s,", dfp->schema->fields[c].field_name);
        } else {
            printf("%s\n", dfp->schema->fields[c].field_name);
        }
    }
    for (r = 0; r < dfp->nrows; r++) {
        for (c = 0; c < dfp->ncols; c++) {
            /* To get here we must have 0 < dfp->ncols */
            /* TODO: Declare enum DataType before the loop */
            enum DataType dtype = schema_get_type(dfp->schema, c);
            char term = ',';
            if (c == (dfp->ncols - 1)) {
                term = '\n';
            }
            switch (dtype) {
                case STRING:
                    /*
                    *((char**) (dfp->data + dfp->rowcapacity * dfp->offsets[i] + dfp->nrows * dfp->field_sizes[i])) = strdup(fldstr);
                    */
                    strs = (char**) (dfp->data + dfp->rowcapacity * dfp->offsets[c]);
                    printf("%s%c", strs[r], term);
                    break;
                case FLOAT:
                    flts = (float*) (dfp->data + dfp->rowcapacity * dfp->offsets[c]);
                    printf("%f%c", flts[r], term);
                    break;
                case DOUBLE:
                    dbls = (double*) (dfp->data + dfp->rowcapacity * dfp->offsets[c]);
                    printf("%f%c", dbls[r], term);
                    break;
                default:
                    printf("Cannot process field type %d at this time\n", dtype);
            }
        }
    }
}

void dataframe_destroy(struct DataFrame* dfp){
    /* String values are duplicated, so we free these here. */
    size_t r, c;
    char** strs;
    enum DataType dtype;

    for (c = 0; c < dfp->ncols; c++) {
        dtype = schema_get_type(dfp->schema, c);
        if (dtype == STRING) {
            strs = (char**) (dfp->data + dfp->rowcapacity * dfp->offsets[c]);
            for (r = 0; r < dfp->nrows; r++) {
                free(strs[r]);
            }
        }
    }

    /* Free the data structures */
    free(dfp->data);
    free(dfp->field_sizes);
    free(dfp->offsets);
    free(dfp->schema);
    free(dfp);
}


/* NOTE (JS): Removing const from input (p) and output. */
char* str_find_quote(char* p, char quote_char, char escape_char){
    char* ret = p;
    printf("Char: %c", *ret);
    while ((ret != NULL) && (*ret != '\0')) {
        ret = strchr(ret, quote_char);
        if( ret != NULL ){
            /* Then *ret == quote_char */
            if (ret == p) {
                break;
            } else if( *(ret - 1) != escape_char) {
                break;
            } else {
                ret++;
            }
        }
    }
    /* TODO: Consider if ret == NULL */
    if (*ret == '\0') {
        ret = NULL;
    }
    return ret;
}


int parse_delimited_string(char* input, char** outputs, size_t* length, size_t max_length, char delim_char, char quote_char, char escape_char) {
    size_t idx = 0;

    char* start = input;
    char* end;

    while( (start != NULL) && (idx < max_length) ) {
        if (quote_char && (*start == quote_char)) {
            start += 1;
            end = str_find_quote(start, quote_char, escape_char);
            if(end == NULL) {
                return 2;
            } else {
                /* Then *end == quote_char */
                *end = '\0';
                end++; /* Have to increment to perform next search */
                end = strchr(end, delim_char);
                if(end != NULL) {
                    /* Then *end == delim_char */
                    end++;
                }
            }
        } else {
            end = strchr(start, delim_char);
            if(end != NULL) {
                /* Then *end == delim_char */
                *end = '\0';
                end++;
            }
        }
        outputs[idx] = start;
        idx++;
        start = end;
    }
    *length = idx;
    return 0;
}

typedef struct DelimitedStringParser {
    char* copy;
    char delim_char;
    char quote_char;
    char escape_char;
    size_t capacity;
    size_t length;
    char** fields;
} DelimitedStringParser;

/* NOTE: In this version, added the max_length argument */
DelimitedStringParser* delimited_string_parser_create(const char* input, char delim_char, char quote_char, char escape_char, size_t max_length){
    size_t nchars = strlen(input);
    char* copy = (char*) malloc(nchars + 1);
    if (copy == NULL) {
        return NULL;
    }
    strcpy(copy, input);

    size_t capacity = max_length;
    /* NOTE: In this version, we simply set the capacity using the max_length argument
    size_t idx;
    for (idx = 0; idx < nchars; idx++){
        if (input[idx] == delim_char) {
            capacity += 1;
        }
    }
    */

    char** fields = (char**) malloc(capacity * sizeof(char*));
    if (fields == NULL) {
        free(copy);
        return NULL;
    }

    DelimitedStringParser* ret;
    ret = (DelimitedStringParser*) malloc(sizeof(*ret));
    *ret = (DelimitedStringParser) { copy, delim_char, quote_char, escape_char, capacity, 0u, fields };
    return ret;
}

/* NOTE: This method was created for this version */
DelimitedStringParser* delimited_string_parser_empty(char delim_char, char quote_char, char escape_char, size_t capacity)
{
    assert(capacity > 0);
    char** fields = (char**) malloc(capacity * sizeof(char*));
    if (fields == NULL) {
        return NULL;
    }

    DelimitedStringParser* ret;
    ret = (DelimitedStringParser*) malloc(sizeof(*ret));
    *ret = (DelimitedStringParser) { NULL, delim_char, quote_char, escape_char, capacity, 0u, fields };
    return ret;
}

size_t delimited_string_parser_get_field_count(const DelimitedStringParser* string_parser) {
    return string_parser->length;
}

const char* delimited_string_parser_get_field(const DelimitedStringParser* string_parser, size_t idx) {
    return string_parser->fields[idx];
}

/* NOTE: This method was created for this version */
int delimited_string_parser_set_line(DelimitedStringParser* string_parser, const char* line)
{
    if (string_parser->copy != NULL) {
        free(string_parser->copy);
        string_parser->length = 0;
    }
    string_parser->copy = strdup(line);
    string_parser->length = 0;
    if (string_parser->copy == NULL) {
        return 3;
    }
    return 0;
}

void delimited_string_parser_destroy(DelimitedStringParser* string_parser) {
    if (string_parser->copy != NULL) {
        free(string_parser->copy);
    }
    free(string_parser->fields);
    free(string_parser);
}

int delimited_string_parser_execute(DelimitedStringParser* string_parser) {
    int ret;
    ret = parse_delimited_string(
        string_parser->copy,
        string_parser->fields,
        &string_parser->length,
        string_parser->capacity,
        string_parser->delim_char, 
        string_parser->quote_char, 
        string_parser->escape_char
    );
    return ret;
}

int dataframe_header_check(struct DataFrame* dfp, DelimitedStringParser* sp) {
    int status = 0;
    size_t c;
    const char *headerstr, *fldstr;
    // const char* fldstr;

    for (c = 0; c < dfp->ncols; c++) {
        headerstr = dfp->schema->fields[c].field_name;
        fldstr = delimited_string_parser_get_field(sp, c);
        if (strcmp(headerstr, fldstr)) {
            fprintf(stderr, "dataframe_parse_line: expected field %s, found %s", headerstr, fldstr);
            status = 4;
            break;
        }
    }
    return status;
}

int dataframe_parse_header(struct DataFrame* dfp, DelimitedStringParser* sp, const char* line) {
    int status = 0;
    size_t i;
    if ((status = delimited_string_parser_set_line(sp, line)) != 0) {
        fprintf(stderr, "dataframe_parse_line: failed to set line");
        return status;
    }
    if ((status = delimited_string_parser_execute(sp)) != 0) {
        fprintf(stderr, "dataframe_parse_line: parser execution failed");
        return status;
    }

    status = dataframe_header_check(dfp, sp);
    return status;
}

int dataframe_parse_line(struct DataFrame* dfp, DelimitedStringParser* sp, const char* line) {
    /* TODO: We might want to add the line number as an input
     *       and use this in any error messages below. */
    int status = 0;
    size_t i;
    const char* fldstr;
    if ((status = delimited_string_parser_set_line(sp, line)) != 0) {
        fprintf(stderr, "dataframe_parse_line: failed to set line");
        return status;
    }
    if ((status = delimited_string_parser_execute(sp)) != 0) {
        fprintf(stderr, "dataframe_parse_line: parser execution failed");
        return status;
    }

    if (dfp->nrows >= dfp->rowcapacity) {
        if ((status = dataframe_data_realloc(dfp)) != 0) {
            fprintf(stderr, "dataframe_parse_line: could not increase the size of the dataframe\n");
            return status;
        }
    }
    /* TODO: On parsing, we might need to make sure the number of parsed fields is
     *       as expected.  This would likely be logic added to another method. */
    for (i = 0; i < dfp->ncols; i++) {
        fldstr = delimited_string_parser_get_field(sp, i);
        
        enum DataType dtype = schema_get_type(dfp->schema, i);
        switch (dtype) {
            case STRING:
                /*
                *((char**) (dfp->data + dfp->rowcapacity * dfp->offsets[i] + dfp->nrows * dfp->field_sizes[i])) = strdup(fldstr);
                */
                ((char**) (dfp->data + dfp->rowcapacity * dfp->offsets[i]))[dfp->nrows] = strdup(fldstr);
                break;
            case FLOAT:
                ((float*) (dfp->data + dfp->rowcapacity * dfp->offsets[i]))[dfp->nrows] = strtof(fldstr, NULL);
                break;
            default:
                printf("Cannot process field type %d at this time\n", dtype);
        }
        /*
        dataframe_set_field_value(dfp, i, fldstr);
        */
    }
    dfp->nrows++; /* Increment the number of rows */
}

int main(void) {
    FILE *file = fopen("example.txt", "r");
    if (file == NULL) {
        perror("Failed to open file");
        fprintf(stderr, "main: %s\n", strerror(errno));
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t nread;
    size_t i = 0;

    struct DataField fld1 = { "Date", STRING };
    struct DataField fld2 = { "Price", FLOAT };
    struct DataSchema* schema = schema_create(2u);
    if (schema == NULL) {
        fprintf(stderr, "main: unable to create schema");
        return 1;
    }
    schema_push(&schema, fld1);
    schema_push(&schema, fld2);

    struct DataFrame* dfp = dataframe_create(/*initial rows*/ 1u, schema);
    if (dfp == NULL) {
        fprintf(stderr, "main: unable to create dataframe");
        return 1;
    }

    DelimitedStringParser* dsp = delimited_string_parser_empty(',', '\"', '\\', 2u);

    while ((nread = getline(&line, &len, file)) != -1) {
        // Remove trailing newline if present
        if (nread > 0 && line[nread - 1] == '\n') {
            line[nread - 1] = '\0';
        }
        printf("Len: %lu, nread: %lu, Line: %s\n", len, nread, line);

        /* DelimitedStringParser* dsp = delimited_string_parser_create(line, ',', '\"', '\\', 2u); */
        if (delimited_string_parser_set_line(dsp, line) != 0) {
            fprintf(stderr, "main: unable to set line");
            break;
        }
        if (delimited_string_parser_execute(dsp) != 0) {
            fprintf(stderr, "main: unable to parse line");
            break;
        }
        printf("\tField 0: %s\n", dsp->fields[0]);
        printf("\tField 1: %s\n", dsp->fields[1]);

        if (i > 0) {
            dataframe_parse_line(dfp, dsp, line);
        } else {
            dataframe_parse_header(dfp, dsp, line);
        }
        i++;
    }
    delimited_string_parser_destroy(dsp);
    printf("Printing dataframe: \n");
    dataframe_print(dfp);
    dataframe_destroy(dfp);
    schema_destroy(schema);

    if (feof(file)) {
        fprintf(stdout, "main: reached end-of-file - %s\n", strerror(errno));
    }

    if (ferror(file)) {
        fprintf(stderr, "main: %s\n", strerror(errno));
    }

    free(line); // Must free the dynamically allocated buffer
    fclose(file);
    return 0;
}

