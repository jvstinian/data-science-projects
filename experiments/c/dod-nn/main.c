#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>  /* memcpy */
#include <assert.h>

float rand_float() {
    return (float)rand() / (float)RAND_MAX;
};

float identity_function(float x) {
    return x;
}

/*  One option to remove warning
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
float const_one_function(float x) {
    return 1.0f;
}
#pragma GCC diagnostic pop
*/
float const_one_function(float) {
    return 1.0f;
}

float (*activator_function)(float) = &identity_function;
float (*activator_derivative)(float) = &const_one_function;

unsigned int number_of_elements(const unsigned int* shape, unsigned int dim){
    unsigned int i;
    unsigned int ret = 1;
    for (i = 0; i < dim; i++){
        ret *= shape[i];
    }
    return ret;
}

unsigned int max_uint(unsigned int a, unsigned int b) {
    return (a > b) ? a : b;
}

struct InputShape {
    unsigned int dims;
    unsigned int shape[6];
};

struct Inputs {
    unsigned int num_inputs;
    struct InputShape input_shapes[1];
};

struct Inputs* inputs_new(unsigned int num_inputs) {
    unsigned int adj_num_inputs = max_uint(1, num_inputs);
    struct Inputs* ret = malloc(sizeof(unsigned int) + adj_num_inputs * sizeof(struct InputShape));
    if (ret != NULL) {
        ret->num_inputs = num_inputs;
    }
    return ret;
}

void inputs_free(struct Inputs* inputs) {
    free(inputs);
}

void inputs_print(struct Inputs* inputs) {
    unsigned int num_inputs = inputs->num_inputs;
    size_t i, j;
    unsigned int dims;
    for (i = 0; i < num_inputs; i++) {
        printf("Input %lu shape: ", i);
        printf("(");
        dims = inputs->input_shapes[i].dims;
        for (j = 0; j < dims; j++) {
            printf("%u", inputs->input_shapes[i].shape[j]);
            if (j + 1 < dims) {
                printf(", ");
            } else {
                printf(")");
            }
        }
        printf("\n");
    }
}

size_t inputs_size(struct Inputs* inputs) {
    unsigned int num_inputs = inputs->num_inputs;
    size_t ret = 0;
   
    size_t i, j;
    unsigned int dims;
    size_t s;
    for (i = 0; i < num_inputs; i++) {
        /* Initialize size for input to the first dimension, and iterate
         * and multiply */
        /* TODO: Use number_of_elements here? */
        s = inputs->input_shapes[i].shape[0];
        dims = inputs->input_shapes[i].dims;
        for (j = 1; j < dims; j++) {
            s *= inputs->input_shapes[i].shape[j];
        }
        ret += s;
    }
    return ret;
}

enum LayerType {
    WEIGHT_ONLY,
    WEIGHT_BIAS_ACTIVATOR,
    LAYER_TYPE_COUNT
};

struct LayerDimensions {
    unsigned int output_dims;
    unsigned int output_shape[6];
};

struct LayerKinds {
    unsigned int num_layers;
    struct LayerDimensions layer_specs[1];
};

struct LayerKinds* layer_kinds_new(unsigned int num_layers) {
    unsigned int adj_num_layers = max_uint(1, num_layers);
    struct LayerKinds* ret = malloc(sizeof(unsigned int) + adj_num_layers * sizeof(struct LayerKinds));
    if (ret != NULL) {
        ret->num_layers = num_layers;
    }
    return ret;
}

void layer_kinds_free(struct LayerKinds* layer_kinds) {
    free(layer_kinds);
}

struct SequentialModel {
    struct Inputs *inputs;
    struct LayerKinds *layer_kinds;
};
   
/* TODO: Is the following used? */
struct Tensor {
    unsigned int dims;
    unsigned int shape[6];
    float* weights;
    /* float bias[2]; */
    /* float x[2]; */
};

/* NOTE: Will use a list of ValueInfo instead
struct InputInfo {
    unsigned int offset;
    unsigned int individual_size;
    unsigned int batch_size;
    unsigned int dims;
    unsigned int shape[7];
};
*/

struct ValueInfo {
    unsigned int offset;
    /*unsigned int layer_id;*/
    unsigned int individual_size;
    unsigned int batch_size;
    unsigned int dims;
    unsigned int shape[7];
};

struct DerivativeInfo {
    unsigned int layer_id;
    unsigned int offset;
    unsigned int individual_size;
    unsigned int batch_size;
    unsigned int dims;
    unsigned int shape[7];
};

struct TensorInfo {
    unsigned int layer_id;
    unsigned int component_id;
    unsigned int offset;
    unsigned int noe;
    unsigned int dims;
    unsigned int shape[6];
};

struct LayerInfo {
    unsigned int layer_id;
    /* TODO: Add layer type field */
    enum LayerType layer_type;
    /* unsigned int input_offset; TODO: Change to instance_idx */
    unsigned int instance_idx;
    /*
    unsigned int num_inputs;
    */
};

/*
struct LayerValueInfo {
    unsigned int layer_id;
    unsigned int value_id;
};
*/

struct LayerTypeCounter {
    unsigned int wo_count;
    unsigned int wba_count;
    /* unsigned int concat_count; */
};

struct LayerTypeCounter get_layer_type_counts(const struct LayerKinds* layer_kinds) {
    struct LayerTypeCounter ret = {0, 0};
    ret.wo_count = layer_kinds->num_layers;
    return ret;
}

unsigned int get_tensor_count(struct LayerTypeCounter ltc) {
    return ltc.wo_count + (2*ltc.wba_count);
};

struct WOLayerInfo {
    unsigned int instance_idx; /* TODO: Remove when ready */
    unsigned int layer_id;
    unsigned int input_value_id;
    unsigned int output_value_id;
    unsigned int weights_tensor_id;
    /*
    int (*layer_output_derivative)(struct Network*, unsigned int);
    float (*activator_func)(float);
    float (*activator_deriv_func)(float);
    */
};

struct WBALayerInfo {
    unsigned int instance_idx; /* TODO: Remove when ready */
    unsigned int layer_id;
    unsigned int input_value_id;
    unsigned int weights_tensor_id;
    unsigned int bias_tensor_id;
    /*
    int (*layer_output_derivative)(struct Network*, unsigned int);
    float (*activator_func)(float);
    float (*activator_deriv_func)(float);
    */
};

struct Network {
    unsigned int batchsize;
    /*unsigned int inputs_size;*/
    unsigned int values_size;
    unsigned int derivatives_size;
    unsigned int tensors_size;
    unsigned int num_inputs;
    /*
    struct InputInfo* ii;
    */
    unsigned int num_values;
    struct ValueInfo* vi;
    unsigned int num_derivatives;
    struct ValueInfo* di;
    unsigned int num_tensors;
    struct TensorInfo* ti;
    unsigned int num_layers;
    struct LayerInfo* li;
    /*
    unsigned int layer_inputs_length;
    struct LayerValueInfo* layer_inputs;
    */
    /* Layer Instances */
    unsigned int woli_length;
    struct WOLayerInfo* woli;
    unsigned int wbali_length;
    struct WBALayerInfo* wbali;
    /*
    unsigned int layer_outputs_length;
    struct LayerValueInfo* layer_outputs;
    */
    /*
    float* inputs_ptr;
    */
    float* values_ptr;
    float* derivatives_ptr;
    float* tensors_ptr;
};

/* TODO: Can we use restrict on out? */
void dot_product(float* a, float* b, size_t len, float* out) {
    size_t i;

    *out = 0.0f;
    for(i = 0; i < len; i++) {
        *out += a[i] * b[i];
    }
}

void vector_multiply_diff(float c, float* a, float* b, size_t len, float* out) {
    size_t i;

    for(i = 0; i < len; i++) {
        out[i] = c * (a[i] - b[i]);
    }
}

/* TODO: The following is not used yet.  It is intended to replace layer_compute. */
int weight_only_layer_eval(struct Network* network, unsigned int instance_idx) {
    size_t i;
    unsigned int batch_id = 0;

    size_t input_length;
    float* input;
    size_t output_length;
    float* output;
#ifndef NDEBUG
    size_t tensor_length;
#endif
    float* tensor;

    /* Multiple inputs are possible, hence the offset to the layer_inputs array.
     * However for the SequentialModel we have one input for each layer, so
     * we just extract the value_id in layer_inputs at the offset index. */
    unsigned int layer_id = network->woli[instance_idx].layer_id;
    size_t input_value_idx = network->woli[instance_idx].input_value_id;
    size_t output_value_idx = network->woli[instance_idx].output_value_id;

    /* The following doesn't depend on batch ID */
#ifndef NDEBUG
    tensor_length = network->ti[layer_id].noe;
#endif
    tensor = network->tensors_ptr + network->ti[layer_id].offset;

    input_length = network->vi[input_value_idx].individual_size;
    output_length = network->vi[output_value_idx].individual_size;
    assert(tensor_length == input_length * output_length);

    for(batch_id = 0; batch_id < network->batchsize; batch_id++) {
        input = network->values_ptr + network->vi[input_value_idx].offset + batch_id * input_length;
        output = network->values_ptr + network->vi[output_value_idx].offset + batch_id * output_length;

        for(i = 0; i < output_length; i++) {
            dot_product(
                tensor + i * input_length,
                input,
                input_length,
                output + i
            );
        }
        /* TODO: Add call to activator function */
    }
    return 0;
}

int layer_compute(struct Network* network, unsigned int layer_id) {
    size_t i;
    unsigned int batch_id = 0;

    size_t input_length;
    float* input;
    size_t output_length;
    float* output;
#ifndef NDEBUG
    size_t tensor_length;
#endif
    float* tensor;

    /* Multiple inputs are possible, hence the offset to the layer_inputs array.
     * However for the SequentialModel we have one input for each layer, so
     * we just extract the value_id in layer_inputs at the offset index. */
    /*
    size_t layer_instance_offset = network->li[layer_id].input_offset;
    size_t input_value_idx = network->layer_inputs[layer_input_offset].value_id;
    */
    size_t layer_instance_offset = network->li[layer_id].instance_idx;
    size_t input_value_idx = network->woli[layer_instance_offset].input_value_id;

    size_t output_value_idx = network->num_inputs + layer_id; /* TODO: Need a more general approach here. */

    /* The following doesn't depend on batch ID */
#ifndef NDEBUG
    tensor_length = network->ti[layer_id].noe;
#endif
    tensor = network->tensors_ptr + network->ti[layer_id].offset;

    for(batch_id = 0; batch_id < network->batchsize; batch_id++) {
        /*
        if (layer_id == 0) {
            input_length = network->ii[0].individual_size;
            input = network->inputs_ptr + network->ii[0].offset + batch_id * input_length;
        } else {
            input_length = network->vi[layer_id - 1].individual_size;
            input = network->values_ptr + network->vi[layer_id - 1].offset + batch_id * input_length;
        }
        */
        /* TODO: input and output length don't depend on batch_id, so can be moved out of the loop */
        input_length = network->vi[input_value_idx].individual_size;
        input = network->values_ptr + network->vi[input_value_idx].offset + batch_id * input_length;

        output_length = network->vi[output_value_idx].individual_size;
        output = network->values_ptr + network->vi[output_value_idx].offset + batch_id * output_length;

        assert(tensor_length == input_length * output_length);

        for(i = 0; i < output_length; i++) {
            dot_product(
                tensor + i * input_length,
                input,
                input_length,
                output + i
            );
        }
    }
    return 0;
}

void last_layer_output_derivative(struct Network* network) {
    unsigned int layer_id = network->num_layers - 1;
    unsigned int output_value_idx = network->num_values - 1;
    unsigned int response_value_idx = 1;
    float mult = -1.0 / ((float) network->batchsize);

    unsigned int output_value_length = network->vi[output_value_idx].individual_size;
    unsigned int response_value_length = network->vi[response_value_idx].individual_size;
    unsigned int deriv_length = network->di[layer_id].individual_size;
    assert(output_value_length == response_value_length);
    assert(output_value_length == deriv_length);

    float *output_value, *response_value, *deriv;

    unsigned int batch_id = 0;
    
    for(batch_id = 0; batch_id < network->batchsize; batch_id++) {
        output_value = network->values_ptr + network->vi[output_value_idx].offset + batch_id * output_value_length;
        response_value = network->values_ptr + network->vi[response_value_idx].offset + batch_id * response_value_length;
        deriv = network->derivatives_ptr + network->di[layer_id].offset + batch_id * deriv_length;

        vector_multiply_diff(
            mult,
            output_value,
            response_value,
            response_value_length,
            deriv
        );
    }
}

void layer_output_derivative(struct Network* network, unsigned int layer_id) {
    assert(layer_id < network->num_layers - 1);
    printf("Stub for derivative of layer_id %u (%u layers)\n", layer_id, network->num_layers);
    size_t i, j;
    unsigned int batch_id;

    size_t backprop_deriv_length;
    float* backprop_deriv;
    size_t layer_deriv_length;
    float* layer_deriv;
    size_t tensor_length;
    float* tensor;

    /* TODO: How would we handle multiple inputs? */
    backprop_deriv_length = network->di[layer_id+1].individual_size;
    layer_deriv_length = network->di[layer_id].individual_size;

    /* The following doesn't depend on batch ID */
    tensor_length = network->ti[layer_id+1].noe;
    tensor = network->tensors_ptr + network->ti[layer_id+1].offset;

    assert(tensor_length == backprop_deriv_length * layer_deriv_length);

    for(batch_id = 0; batch_id < network->batchsize; batch_id++) {
        backprop_deriv = network->derivatives_ptr + network->di[layer_id+1].offset + batch_id * backprop_deriv_length;
        layer_deriv = network->derivatives_ptr + network->di[layer_id].offset + batch_id * layer_deriv_length;

        printf("Performing manual tensor left-multiplication for batch_id=%u in layer_id=%u\n", batch_id, layer_id);
        printf("backprop_deriv_length=%lu, layer_deriv_length=%lu, tensor_length=%lu\n",
                backprop_deriv_length,
                layer_deriv_length,
                tensor_length);
        for(j = 0; j < layer_deriv_length; j++) {
            layer_deriv[j] = 0.0f;
            for(i = 0; i < backprop_deriv_length; i++) {
                layer_deriv[j] += tensor[i * layer_deriv_length + j] * backprop_deriv[i];
            }
        }
    }
}

int layer_update_weights(struct Network* network, unsigned int layer_id) {
    size_t i, j;
    unsigned int batch_id = 0;

    size_t input_length;
    float* input;
    size_t output_length;
    size_t backprop_deriv_length;
    float* backprop_deriv;
    size_t tensor_length;
    float* tensor;

    /* Multiple inputs are possible, hence the offset to the layer_inputs array.
     * However for the SequentialModel we have one input for each layer, so
     * we just extract the value_id in layer_inputs at the offset index. */
    /*
    size_t layer_instance_offset = network->li[layer_id].input_offset;
    size_t input_value_idx = network->layer_inputs[layer_input_offset].value_id;
    */
    size_t layer_instance_offset = network->li[layer_id].instance_idx;
    size_t input_value_idx = network->woli[layer_instance_offset].input_value_id;

    /* NOTE: We don't use output but we do use the output length for
     *       the size of the activator derivative terms. */
    size_t output_value_idx = network->num_inputs + layer_id; /* TODO: Need a more general approach here. */
 
    input_length = network->vi[input_value_idx].individual_size;
    output_length = network->vi[output_value_idx].individual_size;
    backprop_deriv_length = network->di[layer_id].individual_size;
    /* total_deriv_length = network->di[layer_id].individual_size; */

    /* The following doesn't depend on batch ID */
    tensor_length = network->ti[layer_id].noe;
    tensor = network->tensors_ptr + network->ti[layer_id].offset;

    assert(backprop_deriv_length == output_length);
    assert(tensor_length == input_length * output_length);

    /* We allocate a temporary array for activator function part of the derivative.
     * We also allocate for the derivative which has the same shape as the layer tensor,
     * and initialize to 0 */
    /* TODO: Error handling needed */
    float* temp_output = malloc(output_length * sizeof(float));
    if (temp_output == NULL) {
        fprintf(stderr, "layer_update_weight: could not allocate activator function derivative array");
        return 1;
    }
    float* total_agg_deriv = calloc(tensor_length, sizeof(float));
    if (total_agg_deriv == NULL) {
        fprintf(stderr, "layer_update_weight: could not allocate total aggregate derivative array");
        free(temp_output);
        return 1;
    }

    for(batch_id = 0; batch_id < network->batchsize; batch_id++) {
        input  = network->values_ptr + network->vi[input_value_idx].offset + batch_id * input_length;
        backprop_deriv = network->derivatives_ptr + network->di[layer_id].offset + batch_id * backprop_deriv_length;

        /* As with the layer output value, we apply the tensor */
        /* temp_output overwritten for each batch_id */
        for(i = 0; i < output_length; i++) {
            dot_product(
                tensor + i * input_length,
                input,
                input_length,
                temp_output + i
            );
        }
        /* TODO: Apply activator derivative
        array_map(temp_output, output_length, activator_derivative);
        */
        /* TODO: We manually set the output to the derivative of the identity function (i.e., constant 1),
         *       which undoes the previous loop */
        for(i = 0; i < output_length; i++) {
            temp_output[i] = 1.0f;
        }
        
        for(i = 0; i < output_length; i++) {
            for(j = 0; j < input_length; j++) {
                total_agg_deriv[i * input_length + j] += backprop_deriv[i] * temp_output[i] * input[j];
            }
        }
    }
    /* Adjust tensor */
    /* TODO: Replace 0.05 with a parameter */
    for(i = 0; i < tensor_length; i++) {
        tensor[i] += 0.05 * total_agg_deriv[i];
    }

    /* Clean up */
    free(total_agg_deriv);
    free(temp_output);
    return 0;
}

void eval(struct Network* network) {
    /* TODO: We use the tensors data as a proxy for the layers.
     *       We should probably have dedicated layer data. */
    size_t i;
    unsigned int layer_id;

    for(i = 0; i < network->num_tensors; i++) {
        layer_id = network->ti[i].layer_id;  /* TODO: Maybe li instead of ti? */
        layer_compute(network, layer_id);
    }
    /* Calculate the derivatives */
    last_layer_output_derivative(network);
    layer_id = network->num_layers - 1;
    if (layer_id > 0) {
        while (layer_id > 0) {
            layer_id--;
            layer_output_derivative(network, layer_id);
        }
    }
}

int update_weights(struct Network* network) {
    unsigned int layer_id = network->num_layers;
    int status = 0;
    while (layer_id > 0) {
        layer_id--;
        status = layer_update_weights(network, layer_id);
        if (status) break;
    }
    return status;
}

void fill_value(struct Network* network, unsigned int value_id, float val) {
    size_t i;
    /*
    unsigned int batch_id;
    */

    size_t input_length;
    float* input;
    
    /* No need for iteration
    for(batch_id = 0; batch_id < network->batchsize; batch_id++) {
        input_length = network->vi[value_id].individual_size;
        input = network->values_ptr + network->vi[value_id].offset + batch_id * input_length;

        for(i = 0; i < input_length; i++) {
            input[i] = val;
        }
    }
    */
    input_length = network->vi[value_id].batch_size;
    input = network->values_ptr + network->vi[value_id].offset;
    for(i = 0; i < input_length; i++) {
        input[i] = val;
    }
}

void fill_rand(struct Network* network, unsigned int value_id) {
    size_t i;
    size_t input_length;
    float* input;
    
    input_length = network->vi[value_id].batch_size;
    input = network->values_ptr + network->vi[value_id].offset;
    for(i = 0; i < input_length; i++) {
        input[i] = rand_float();
    }
}

int network_init(const struct Inputs* inputs, const struct LayerKinds* layer_kinds, unsigned int batchsize, struct Network* network){
    /* unsigned int batchsize = 1;  TODO: Remove when ready */
    assert(inputs->num_inputs > 0);
    size_t i;
    size_t layer_idx;
    unsigned int last_offset;
    struct LayerTypeCounter ltc_sizes;
    struct LayerTypeCounter ltc_indices = { 0 };

    /* batchsize */
    network->batchsize = batchsize;

    /* Input Info */
    network->num_inputs = inputs->num_inputs;
    /*
    network->ii = malloc(network->num_inputs * sizeof(struct InputInfo));
    if (network->ii == NULL) {
        fprintf(stderr, "network_init: could not allocate InputInfo");
        return 1;
    }
    last_offset = 0;
    for (i = 0; i < inputs->num_inputs; i++) {
        network->ii[i].dims = inputs->input_shapes[i].dims + 1;
        network->ii[i].shape[0] = batchsize; // TODO: use batchsize
        memcpy(
            ((unsigned int*) network->ii[i].shape) + 1,
            inputs->input_shapes[i].shape,
            inputs->input_shapes[i].dims * sizeof(unsigned int)
        );

        network->ii[i].individual_size = number_of_elements(inputs->input_shapes[i].shape, inputs->input_shapes[i].dims);
        network->ii[i].batch_size = batchsize * network->ii[i].individual_size;
        network->ii[i].offset = last_offset;
        last_offset += network->ii[i].batch_size;
    };
    network->inputs_size = last_offset;
    */
    
    /* Value Info
     * This includes both the inputs and the layer output values */
    network->num_values = layer_kinds->num_layers + network->num_inputs;
    network->vi = malloc(network->num_values * sizeof(struct ValueInfo));
    if (network->vi == NULL) {
        fprintf(stderr, "network_init: could not allocate ValueInfo");
        /* free(network->ii); network->ii =NULL; */
        return 1;
    }
    last_offset = 0;
    /* Fill in inputs */
    for (i = 0; i < inputs->num_inputs; i++) {
        network->vi[i].dims = inputs->input_shapes[i].dims + 1;
        network->vi[i].shape[0] = batchsize;
        memcpy(
            ((unsigned int*) network->vi[i].shape) + 1,
            inputs->input_shapes[i].shape,
            inputs->input_shapes[i].dims * sizeof(unsigned int)
        );

        network->vi[i].individual_size = number_of_elements(inputs->input_shapes[i].shape, inputs->input_shapes[i].dims);
        network->vi[i].batch_size = batchsize * network->vi[i].individual_size;
        network->vi[i].offset = last_offset;
        last_offset += network->vi[i].batch_size;
    };
    /*network->inputs_size = last_offset;*/
    /* Fill in layer output information */
    for (i = inputs->num_inputs; i < inputs->num_inputs + layer_kinds->num_layers ; i++) {
        layer_idx = i - inputs->num_inputs;
        /* network->vi[i].layer_id = layer_idx;  IN PROGRESS: Remove layer_id */
        network->vi[i].dims = layer_kinds->layer_specs[layer_idx].output_dims + 1;
        network->vi[i].shape[0] = batchsize; /* TODO: use batchsize */
        memcpy(
            ((unsigned int*) network->vi[i].shape) + 1,
            layer_kinds->layer_specs[layer_idx].output_shape,
            layer_kinds->layer_specs[layer_idx].output_dims * sizeof(unsigned int)
        );
        network->vi[i].individual_size = number_of_elements(
            layer_kinds->layer_specs[layer_idx].output_shape,
            layer_kinds->layer_specs[layer_idx].output_dims
        );
        network->vi[i].batch_size = batchsize * network->vi[i].individual_size;
        network->vi[i].offset = last_offset;
        last_offset += network->vi[i].batch_size;
    }
    network->values_size = last_offset;

    /* Derivative Info */
    network->num_derivatives = layer_kinds->num_layers;
    network->di = malloc(network->num_derivatives * sizeof(struct DerivativeInfo));
    if (network->di == NULL) {
        fprintf(stderr, "network_init: could not allocate DerivativeInfo");
        /*free(network->ii); network->ii =NULL;*/
        free(network->vi); network->vi =NULL;
        return 1;
    }
    /* Derivatives actually have a value for each layer.  We reverse over the
     * derivatives by layer though. */
    last_offset = 0;
    for (i = 0; i < layer_kinds->num_layers; i++) {
        /* Originally we iterated up to num_layers but reversed vi starting from the
         * last element num_values - 1 */
        /* network->di[i] = network->vi[network->num_values - 1 - i]; */
        /* Now we just iterate starting with the output values of the layers */
        network->di[i] = network->vi[inputs->num_inputs + i];
        /* Reset the offset */
        network->di[i].offset = last_offset;
        last_offset += network->di[i].batch_size;
    }
    network->derivatives_size = last_offset;

    /* Calculate the number of each layer type */
    ltc_sizes = get_layer_type_counts(layer_kinds);
    /* Tensor Info */
    /* TODO: Need to define a method for calculating the number of tensors */
    /* network->num_tensors = layer_kinds->num_layers; */
    network->num_tensors = get_tensor_count(ltc_sizes);
    assert(network->num_tensors > 0); /* TODO: Move an assert for layer_kinds->num_layers up */

    network->ti = malloc(network->num_tensors * sizeof(struct TensorInfo));
    if (network->ti == NULL) {
        fprintf(stderr, "network_init: could not allocate TensorInfo");
        /*free(network->ii); network->ii =NULL;*/
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        return 1;
    }
    /* WOLayerInfo */
    network->woli_length = ltc_sizes.wo_count;
    if (ltc_sizes.wo_count > 0) {
        network->woli = malloc(ltc_sizes.wo_count * sizeof(struct WOLayerInfo));
        if (network->woli == NULL) {
            fprintf(stderr, "network_init: could not allocate WOLayerInfo");
            /*free(network->ii); network->ii =NULL;*/
            free(network->vi); network->vi =NULL;
            free(network->di); network->di =NULL;
            free(network->ti); network->ti =NULL;
            return 1;
        }
    }
    /* WBALayerInfo */
    network->wbali_length = ltc_sizes.wba_count;
    if (ltc_sizes.wba_count > 0) {
        network->wbali = malloc(ltc_sizes.wba_count * sizeof(struct WBALayerInfo));
        if (network->wbali == NULL) {
            fprintf(stderr, "network_init: could not allocate WOLayerInfo");
            /*free(network->ii); network->ii =NULL;*/
            free(network->vi); network->vi =NULL;
            free(network->di); network->di =NULL;
            free(network->ti); network->ti =NULL;
            if (network->woli != NULL) {
                free(network->woli); 
            }
            network->woli = NULL;
            return 1;
        }
    }
    
    /* Layer Info */
    network->num_layers = layer_kinds->num_layers;
    network->li = malloc(network->num_layers * sizeof(struct LayerInfo));
    if (network->li == NULL) {
        fprintf(stderr, "network_init: could not allocate LayerInfo");
        /*free(network->ii); network->ii =NULL;*/
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        free(network->ti); network->ti =NULL;
        if (network->woli != NULL) {
            free(network->woli); 
        }
        network->woli = NULL;
        if (network->wbali != NULL) {
            free(network->wbali); 
        }
        network->wbali = NULL;
        return 1;
    }
    
    unsigned int layer_input_dims;
    unsigned int layer_input_shape[6];

    /* Initialize layer input shape and dimension to the values for the first input */
    /* NOTE: Other inputs are ignored at this point */
    layer_input_dims = inputs->input_shapes[0].dims;
    memcpy(layer_input_shape, inputs->input_shapes[0].shape, layer_input_dims*sizeof(unsigned int));

    last_offset = 0;
    for (i = 0; i < layer_kinds->num_layers ; i++) {
        /* Set layer info */
        network->li[i].layer_id = i;
        /*network->li[i].input_offset = i; */
        network->li[i].layer_type = WEIGHT_ONLY;
        network->li[i].instance_idx = ltc_indices.wo_count;

        /* Set layer instance info */
        network->woli[ltc_indices.wo_count].instance_idx = ltc_indices.wo_count;
        network->woli[ltc_indices.wo_count].layer_id = i;
        if (i == 0) {
            network->woli[ltc_indices.wo_count].input_value_id = 0;
        } else {
            network->woli[ltc_indices.wo_count].input_value_id = network->num_inputs + (i - 1);
        }
        network->woli[ltc_indices.wo_count].output_value_id = network->num_inputs + i;
        network->woli[ltc_indices.wo_count].weights_tensor_id = i;
        ltc_indices.wo_count++;

        /* Set tensor info */
        network->ti[i].layer_id = i;
        network->ti[i].component_id = 0;  /* NOTE: Currently always setting to 1 */
        network->ti[i].dims = layer_kinds->layer_specs[i].output_dims;
        memcpy(
            network->ti[i].shape,
            layer_kinds->layer_specs[i].output_shape,
            layer_kinds->layer_specs[i].output_dims * sizeof(unsigned int)
        );

        memcpy(
            ((unsigned int*) network->ti[i].shape) + network->ti[i].dims,
            layer_input_shape,
            layer_input_dims * sizeof(unsigned int)
        );
        network->ti[i].dims += layer_input_dims;
        
        network->ti[i].noe = number_of_elements(
            network->ti[i].shape,
            network->ti[i].dims
        );
        network->ti[i].offset = last_offset;
        last_offset += network->ti[i].noe;

        /* Layer output dimensions become input dimensions for next layer */
        layer_input_dims = layer_kinds->layer_specs[i].output_dims;
        memcpy(layer_input_shape, layer_kinds->layer_specs[i].output_shape, layer_input_dims*sizeof(unsigned int));
    }
    network->tensors_size = last_offset;

    /* 
    // last_offset = 0;
    for (i = 0; i < network->num_layers ; i++) {
        network->li[i].layer_id = i;
        network->li[i].input_offset = i;
        // network->li[i].input_offset = last_offset;
        // network->li[i].num_inputs = 1;
        // last_offset += network->li[i].num_inputs;
    }
    */

    /*
    network->layer_inputs_length = last_offset;
    network->layer_inputs = malloc(network->layer_inputs_length * sizeof(struct LayerValueInfo));
    if (network->layer_inputs == NULL) {
        fprintf(stderr, "network_init: could not allocate LayerValueInfo");
        // free(network->ii); network->ii =NULL;
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        free(network->ti); network->ti =NULL;
        if (network->woli != NULL) {
            free(network->woli); 
        }
        network->woli = NULL;
        if (network->wbali != NULL) {
            free(network->wbali); 
        }
        network->wbali = NULL;
        free(network->li); network->li =NULL;
        return 1;
    }
    for (i = 0; i < network->num_layers ; i++) {
        network->layer_inputs[i].layer_id = i;
        if (i == 0) {
            network->layer_inputs[i].value_id = 0;
        } else {
            // The first layer output corresponds to the value_id = network->num_inputs
            network->layer_inputs[i].value_id = network->num_inputs + (i - 1);
        }
    }
    */

    /* Create arrays for data */
    /*
    network->inputs_ptr = malloc(network->inputs_size* sizeof(float));
    if (network->inputs_ptr == NULL) {
        fprintf(stderr, "network_init: could not allocate inputs array");
        // free(network->ii); network->ii =NULL;
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        free(network->ti); network->ti =NULL;
    }
    */
    network->values_ptr = malloc(network->values_size * sizeof(float));
    if (network->values_ptr == NULL) {
        fprintf(stderr, "network_init: could not allocate values array");
        /*free(network->ii); network->ii =NULL;*/
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        free(network->ti); network->ti =NULL;
        /* free(network->inputs_ptr); network->inputs_ptr =NULL; */
        if (network->woli != NULL) {
            free(network->woli); 
        }
        network->woli = NULL;
        if (network->wbali != NULL) {
            free(network->wbali); 
        }
        network->wbali = NULL;
        free(network->li); network->li =NULL;
    }
    network->derivatives_ptr = malloc(network->derivatives_size * sizeof(float));
    if (network->derivatives_ptr == NULL) {
        fprintf(stderr, "network_init: could not allocate derivatives array");
        /*free(network->ii); network->ii =NULL;*/
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        free(network->ti); network->ti =NULL;
        /* free(network->inputs_ptr); network->inputs_ptr =NULL; */
        if (network->woli != NULL) {
            free(network->woli); 
        }
        network->woli = NULL;
        if (network->wbali != NULL) {
            free(network->wbali); 
        }
        network->wbali = NULL;
        free(network->li); network->li =NULL;
        free(network->values_ptr); network->values_ptr =NULL;
    }
    network->tensors_ptr = malloc(network->tensors_size * sizeof(float));
    if (network->tensors_ptr == NULL) {
        fprintf(stderr, "network_init: could not allocate tensors array");
        /*free(network->ii); network->ii =NULL;*/
        free(network->vi); network->vi =NULL;
        free(network->di); network->di =NULL;
        free(network->ti); network->ti =NULL;
        /* free(network->inputs_ptr); network->inputs_ptr =NULL; */
        if (network->woli != NULL) {
            free(network->woli); 
        }
        network->woli = NULL;
        if (network->wbali != NULL) {
            free(network->wbali); 
        }
        network->wbali = NULL;
        free(network->li); network->li =NULL;
        free(network->values_ptr); network->values_ptr =NULL;
        free(network->derivatives_ptr); network->derivatives_ptr =NULL;
    }
    return 0;
}

void network_destroy(struct Network network){
    /*free(network.ii);*/
    free(network.vi);
    free(network.di);
    free(network.ti);
    if (network.woli != NULL) free(network.woli); 
    if (network.wbali != NULL) free(network.wbali); 
    free(network.li);
    /* free(network.layer_inputs) */
    /* free(network.inputs_ptr); */
    free(network.values_ptr);
    free(network.derivatives_ptr);
    free(network.tensors_ptr);
}

/* TODO: Remove
struct TensorInfo* tensor_infos_new(const struct Inputs* inputs, const struct LayerKinds* layer_kinds, unsigned int* num_tensors) {
    unsigned int layer_input_dims;
    unsigned int layer_input_shape[6];
    size_t i;

    assert(inputs->num_inputs > 0);
    layer_input_dims = inputs->input_shapes[0].dims;
    memcpy(layer_input_shape, inputs->input_shapes[0].shape, layer_input_dims*sizeof(unsigned int));

    // TODO: Need to define a method for calculating the number of tensors 
    *num_tensors = layer_kinds->num_layers;

    assert(*num_tensors > 0);
    struct TensorInfo* ret = malloc(*num_tensors * sizeof(struct TensorInfo));

    for (i = 0; i < layer_kinds->num_layers ; i++) {
        ret[i].layer_id = i;
        ret[i].component_id = 0;
        ret[i].dims = layer_kinds->layer_specs[i].output_dims;
        memcpy(
            ret[i].shape,
            layer_kinds->layer_specs[i].output_shape,
            layer_kinds->layer_specs[i].output_dims * sizeof(unsigned int)
        );

        //
        for (j = 0; j < layer_input_shape ; j++) {
            ret[ret[i].shape + j] = layer_input_shape[j];
        }
        //
        memcpy(
            ((unsigned int*) ret[i].shape) + ret[i].dims,
            layer_input_shape,
            layer_input_dims * sizeof(unsigned int)
        );
        ret[i].dims += layer_input_dims;
        
        // Layer output dimensions become input dimensions for next layer //
        layer_input_dims = layer_kinds->layer_specs[i].output_dims;
        //
        layer_input_shape = layer_kinds->layer_specs[i].output_shape;
        //
        memcpy(layer_input_shape, layer_kinds->layer_specs[i].output_shape, layer_input_dims*sizeof(unsigned int));
    }
    return ret;
}
*/

/* TODO: Remove
void tensor_infos_free(struct TensorInfo* tensor_info_ptr) {
    free(tensor_info_ptr);
}
*/


/*
struct Deriv {
    float grad[2];
};

float layer_objective(struct Layer layer, float xs[3][2], float ys[3]) {
    int i;
    float res = 0.0f;

    for (i = 0; i < 3; i++) {
        float v = xs[i][0] * layer.weights[0] + xs[i][1] * layer.weights[1];
        res += (1/3.0f) * (v - ys[i]) * (v - ys[i]);
    }
    return res;
}

struct Deriv layer_deriv(struct Layer layer, float xs[3][2], float ys[3]) {
    struct Deriv deriv = { { 0.0f, 0.0f } };

    int i;
    for (i = 0; i < 3; i++) {
        float v = xs[i][0] * layer.weights[0] + xs[i][1] * layer.weights[1];
        deriv.grad[0] += (1/3.0f) * 2.0f * (v - ys[i]) * xs[i][0];
        deriv.grad[1] += (1/3.0f) * 2.0f * (v - ys[i]) * xs[i][1];
    }
    return deriv;
}

float inputs[3][2] = { { 0.0f, 1.0f }, { 2.0f, 3.0f }, { 4.0f, 5.0f } };

const float a = 10.0f, b = 1.0f;

float outputs[3];

int main(void) {
    outputs[0] = a * inputs[0][0] + b * inputs[0][1];
    outputs[1] = a * inputs[1][0] + b * inputs[1][1];
    outputs[2] = a * inputs[2][0] + b * inputs[2][1];

    srand(137);
    printf("Random float: %f\n", rand_float());
    printf("Random float: %f\n", rand_float());
    srand(137);
    printf("Random float: %f\n", rand_float());
    printf("Random float: %f\n", rand_float());

    int i, j;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 2; j++) {
            printf(" %.4f", inputs[i][j]);
        }
        printf("\n");
    }

    // struct Layer layer = { { 0.0f, 0.0f } }; 
    // Start layer with random weights 
    struct Layer layer = { { 20 * rand_float() - 10.0f, 20 * rand_float() - 10.0f } };
    struct Deriv deriv;

    for (i = 0; i < 500; i++) {
        printf("layer weights = (%.4f, %.4f)\n", layer.weights[0], layer.weights[1]);
        deriv = layer_deriv(layer, inputs, (float*) outputs);
        printf("negative grad = (%.4f, %.4f)\n", -deriv.grad[0], -deriv.grad[1]);
        layer.weights[0] -= 0.05*deriv.grad[0];
        layer.weights[1] -= 0.05*deriv.grad[1];
    }

    float err = layer_objective(layer, inputs, outputs);
    printf("Objective function given current weights: %.6f", err);
    return 0;
}
*/
int main(void) {
    struct Inputs* inputs = inputs_new(2);
    if (inputs == NULL) {
        fprintf(stderr, "Error allocating input dimensions.");
        return 1;
    }
    inputs->input_shapes[0].dims = 1;
    inputs->input_shapes[0].shape[0] = 5;
    inputs->input_shapes[1].dims = 1;
    inputs->input_shapes[1].dims = 1;
    inputs->input_shapes[1].shape[0] = 3;
    inputs_print(inputs);
    printf("Inputs size: %lu\n", inputs_size(inputs));

    struct LayerKinds* layer_kinds = layer_kinds_new(1);
    if (layer_kinds == NULL) {
        fprintf(stderr, "Error allocating layer information.");
        goto defer_free_inputs;
        /* TODO: Set status for exit */
    }
    layer_kinds[0].layer_specs[0].output_dims = 1;
    layer_kinds[0].layer_specs[0].output_shape[0] = 3;
    /*
    layer_kinds[0].layer_specs[0].output_dims = 2;
    layer_kinds[0].layer_specs[0].output_shape[0] = 10;
    layer_kinds[0].layer_specs[0].output_shape[1] = 8;
    layer_kinds[0].layer_specs[1].output_dims = 1;
    layer_kinds[0].layer_specs[1].output_shape[0] = 3;
    */

    /*
    unsigned int num_tensors;
    struct TensorInfo* tis = tensor_infos_new(inputs, layer_kinds, &num_tensors);
    if (tis == NULL) {
        fprintf(stderr, "Error allocating tensor info.");
        goto defer_layer_kinds;
    }
    */

    unsigned int batchsize = 64;

    struct Network network;
    if (network_init(inputs, layer_kinds, batchsize, &network)) {
        /* TODO: Assumes tis will be removed */
        goto defer_layer_kinds;
    }
    printf("Number of elements in values array: %u\n", network.values_size);
    
    /* TODO: The following is just to obtain some non-trivial values */
    fill_rand(&network, 0);  /* Fill input with random data */
    fill_value(&network, 1, 0.0f);
    /* for(idx = 0; idx < 5; idx++) network.values_ptr[idx] = rand_float(); */
    /* Fill tensor with random values */
    size_t idx;
    for(idx = 0; idx < network.tensors_size; idx++) network.tensors_ptr[idx] = rand_float();

    eval(&network);
    update_weights(&network);
    size_t iter_count = 0;
    for (iter_count = 0; iter_count < 10000; iter_count++) {
        eval(&network);
        update_weights(&network);
        float max_tensor_val = 0.0;
        for(idx = 0; idx < network.tensors_size; idx++) {
            max_tensor_val = fabs(network.tensors_ptr[idx]) > max_tensor_val ? fabs(network.tensors_ptr[idx]) : max_tensor_val;
        }
        if (iter_count % 1000 == 0) {
            printf("Max weight in iteration %lu is %f\n", iter_count, max_tensor_val);
        }
        if (max_tensor_val < 1e-4) break;
    }

    
    network_destroy(network);
    /* tensor_infos_free(tis); */

    /* TODO: Remove the following */
    printf("Activator function applied to 2.71: %.2f\n", activator_function(2.71f));
    printf("Activator function derivative applied to 2.71: %.2f\n", activator_derivative(2.71f));
defer_layer_kinds:
    layer_kinds_free(layer_kinds);
defer_free_inputs:
    inputs_free(inputs);
    return 0;
}

