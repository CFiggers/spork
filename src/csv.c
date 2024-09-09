/*
 * Wrapper around zpl.h for csv functionality.
 */

#include <janet.h>

#define ZPL_IMPLEMENTATION
#define ZPL_NANO
#define ZPL_ENABLE_PARSER
#define ZPL_CSV_DEBUG
#include "../deps/zpl/zpl.h"

void mangle_key_if_existing (JanetTable *ret, JanetBuffer *key, bool keywordize) {
    JanetString key_string = janet_string(key->data, key->count);
    Janet key_test;
    if (keywordize) {
        key_test = janet_keywordv(key_string, janet_string_length(key_string));
    } else {
        key_test = janet_wrap_string(key_string);
    }

    if (janet_truthy(janet_table_get(ret, key_test))) {
        janet_buffer_push_cstring(key, "_");
        mangle_key_if_existing(ret, key, keywordize);
    }
}

JANET_FN(cfun_csv_parse,
         "(csv/parse bytes &opt has-header keywords)",
         "Returns a janet data structure after parsing csv. If `has-header` is truthy, "
         "will return a table. Otherwise, returns an array of arrays. If `keywords` is truthy, "
         "table keys will be converted to keywords (only matters when `has-header` is "
         "truthy also)."
         "\n\n"
         "NOTE: Duplicate headers in the source .csv will be mangled to avoid loss of data.")
{
    janet_arity(argc, 1, 3);

    /* Get buffer or string from slot 0 */
    char *start;
    if (janet_checktype(argv[0], JANET_BUFFER)) {
        JanetBuffer *buffer = janet_unwrap_buffer(argv[0]);
        janet_buffer_push_u8(buffer, 0);
        buffer->count--;
        start = (const char *)buffer->data;
    } else {
        JanetByteView bytes = janet_getbytes(argv, 0);
        start = bytes.bytes;
    }
    char *copy[strlen(start)+1]; 
    memcpy(copy, start, strlen(start)+1);

    int has_header = (argc > 1 && janet_truthy(argv[1]));

    /* Invoke ZPL csv parsing */
    zpl_csv_object r = {0};
    zpl_u8 err = zpl_csv_parse(&r, (const char *)copy, zpl_heap_allocator(), has_header);
    if (err) janet_panicf("parsing error code: %d\n", err);

    if (has_header) {
        /* Convert ZPL array into Janet table */
        JanetTable *ret = janet_table(0);

        for (int i = 0; i < zpl_array_count(r.nodes); i++) {
            JanetArray *column = janet_array(0);

            for (int j = 0; j < zpl_array_count(r.nodes[0].nodes); j++) {
                janet_array_push(column, janet_wrap_string(janet_cstring(r.nodes[i].nodes[j].string)));
            };

            bool keywordize = argc > 2 && janet_truthy(argv[2]);

            /* Mangle keys by appending "_" to avoid duplicate */
            JanetBuffer *str = janet_buffer(0);
            janet_buffer_push_cstring(str, r.nodes[i].name);
            mangle_key_if_existing(ret, str, keywordize);
            JanetString key_string = janet_string(str->data, str->count);
            
            /* Keywordize if slot 2 is truthy */
            Janet key;
            if (keywordize) {
                key = janet_keywordv(key_string, janet_string_length(key_string));
            } else {
                key = janet_wrap_string(key_string);
            }
            
            janet_table_put(ret, key, janet_wrap_array(column));
        };

        zpl_csv_free(&r);
        return janet_wrap_table(ret);
    } else {
        /* Convert ZPL array into Janet array */
        JanetArray *ret = janet_array(0);

        for (int i = 0; i < zpl_array_count(r.nodes); i++) {
            JanetArray *column = janet_array(0);

            for (int j = 0; j < zpl_array_count(r.nodes[0].nodes); j++) {
                janet_array_push(column, janet_wrap_string(janet_cstring(r.nodes[i].nodes[j].string)));
            };

            janet_array_push(ret, janet_wrap_array(column));
        };

        zpl_csv_free(&r);
        return janet_wrap_array(ret);
    }
}

JANET_MODULE_ENTRY(JanetTable *env)
{
    JanetRegExt cfuns[] = {
        JANET_REG("parse", cfun_csv_parse),
        JANET_REG_END};
    janet_cfuns_ext(env, "csv", cfuns);
}