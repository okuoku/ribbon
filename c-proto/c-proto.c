/* Ribbon Scheme proto */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Heap */

#include "c-proto.h"

void
RnValueLink(RnCtx* ctx, Value* target){
    Value* last_root;
    last_root = ctx->current_frame->root;
    ctx->current_frame->root = target;
    target->next = last_root;
    target->prev = 0;
    if(last_root){
        last_root->prev = target;
    }
    target->type = VT_EMPTY;
}

void
RnValueUnlink(RnCtx* ctx, Value* target){
    if(ctx->current_frame->root == target){
        ctx->current_frame->root = target->next;
    }
    if(target->next){
        target->next->prev = target->prev;
    }
    if(target->prev){
        target->prev->next = target->next;
    }
}

void
RnEnter(RnCtx* ctx, Value* out){
    out->next = 0;
    if(ctx->current_frame){
        ctx->current_frame->next = out;
    }
    out->prev = ctx->current_frame;
    out->root = 0; /* No object allocated */
    out->type = VT_ROOT;
    ctx->current_frame = out;
}

void
RnLeave(RnCtx* ctx, Value* current_frame){
    Value* cur;
    if(ctx->current_frame != current_frame){
        abort();
    }
    cur = current_frame->root;
    while(cur){
        RnValueUnlink(ctx, cur);
        cur = current_frame->root;
    }

    ctx->current_frame = current_frame->prev;
}

void
RnRef(RnCtx* ctx, ValueContainer obj, ValueType type){
    (void)ctx;
    (void)obj;
    (void)type;
}

void
RnUnref(RnCtx* ctx, ValueContainer* obj, ValueType* type){
    (void)ctx;
    (void)obj;
    *type = VT_EMPTY;
}

void
RnValueUnref(RnCtx* ctx, Value* target){
    RnUnref(ctx, &target->value, &target->type);
}


void
RnValueRef(RnCtx* ctx, Value* target, ValueContainer obj, ValueType type){
    RnValueUnref(ctx, target);
    target->value = obj;
    target->type = type;
    RnRef(ctx, obj, type);
}

void
RnObjHeaderInit(RnCtx* ctx, ObjHeader* header){
    (void)ctx;
    (void)header;
}

void
RnRibSet(RnCtx* ctx, Value* target, Value* obj, int field){
    ObjRib* r;
    if(target->type != VT_RIB){
        abort();
    }
    r = target->value.as_rib;
    RnUnref(ctx, &r->field[field], &r->type[field]);
    r->field[field] = obj->value;
    r->type[field] = obj->type;
    RnRef(ctx, r->field[field], r->type[field]);
}

void
RnRibRef(RnCtx* ctx, Value* out, Value* obj, int field){
    ObjRib* r;
    if(obj->type != VT_RIB){
        abort();
    }
    r = obj->value.as_rib;
    RnValueRef(ctx, out, r->field[field], r->type[field]);
}

void 
RnRib(RnCtx* ctx, Value* out, Value* field0, Value* field1, Value* field2){
    ObjRib* r;
    ValueContainer vc;
    r = (ObjRib*)malloc(sizeof(ObjRib));
    if(!r){
        abort();
    }
    RnObjHeaderInit(ctx, (ObjHeader*)r);
    r->type[0] = r->type[1] = r->type[2] = VT_EMPTY;
    vc.as_rib = r;
    RnValueRef(ctx, out, vc, VT_RIB);
    RnRibSet(ctx, out, field0, 0);
    RnRibSet(ctx, out, field1, 1);
    RnRibSet(ctx, out, field2, 2);
}

void
RnInt64(RnCtx* ctx, Value* out, int64_t i64){
    ValueContainer v;
    v.as_int64 = i64;
    RnValueRef(ctx, out, v, VT_INT64);
}

void
RnDouble(RnCtx* ctx, Value* out, double d){
    ValueContainer v;
    v.as_double = d;
    RnValueRef(ctx, out, v, VT_DOUBLE);
}

void
RnChar(RnCtx* ctx, Value* out, int c){
    ValueContainer v;
    v.as_char = c;
    RnValueRef(ctx, out, v, VT_CHAR);
}

void
RnZone0(RnCtx* ctx, Value* out, ValueZone0 z){
    ValueContainer v;
    v.as_zone0 = z;
    RnValueRef(ctx, out, v, VT_ZONE0);
}

void
RnCons(RnCtx* ctx, Value* out, Value* car, Value* cdr){
    Value id;
    RnValueLink(ctx, &id);
    RnInt64(ctx, &id, 0 /* Pair */);
    RnRib(ctx, out, car, cdr, &id);
    RnValueUnlink(ctx, &id);
}

void
RnVector(RnCtx* ctx, Value* out, size_t len){
    size_t i;
    ObjVector* r;
    ValueContainer v;
    r = (ObjVector*)malloc(sizeof(ObjVector));
    if(!r){
        abort();
    }
    r->values = (ValueContainer*)malloc(sizeof(ValueContainer) * len);
    if(! r->values){
        abort();
    }
    r->types = (ValueType*)malloc(sizeof(ValueType) * len);
    if(! r->types){
        abort();
    }
    RnObjHeaderInit(ctx, (ObjHeader*)r);
    for(i = 0; i!=len; i++){
        r->types[i] = VT_EMPTY;
    }
    r->length = len;
    v.as_vector = r;
    RnValueRef(ctx, out, v, VT_VECTOR);
}

void
RnVectorRef(RnCtx* ctx, Value* out, Value* target, size_t idx){
    ObjVector* vec;
    if(target->type != VT_VECTOR && target->type != VT_SIMPLE_STRUCT){
        abort();
    }
    vec = target->value.as_vector;
    if(idx >= vec->length){
        abort();
    }
    RnValueRef(ctx, out, vec->values[idx], vec->types[idx]);
}

void
RnVectorSet(RnCtx* ctx, Value* target, Value* obj, size_t idx){
    ObjVector* vec;
    if(target->type != VT_VECTOR && target->type != VT_SIMPLE_STRUCT){
        abort();
    }
    vec = target->value.as_vector;
    if(idx >= vec->length){
        abort();
    }
    RnUnref(ctx, &vec->values[idx], &vec->types[idx]);
    vec->values[idx] = obj->value;
    vec->types[idx] = obj->type;
    RnRef(ctx, vec->values[idx], vec->types[idx]);
}

void
RnBytevector(RnCtx* ctx, Value* out, size_t len){
    ValueContainer v;
    ObjBytevector* bv;

    bv = (ObjBytevector*)malloc(sizeof(ObjBytevector));
    if(! bv){
        abort();
    }
    bv->buf = (uint8_t*)malloc(len);
    if(! bv->buf){
        abort();
    }
    bv->len = len;
    bv->refcnt = 0;

    v.as_bytevector = bv;
    RnValueRef(ctx, out, v, VT_BYTEVECTOR);
}

void
RnString(RnCtx* ctx, Value* out, const char* name, size_t len){
    char* buf;
    ObjString* str;
    ValueContainer v;
    str = (ObjString*)malloc(sizeof(ObjString));
    if(! str){
        abort();
    }
    str->refcnt = 0;
    buf = (char*)malloc(len + 1);
    if(! buf){
        abort();
    }
    memcpy(buf, name, len);
    buf[len] = 0;
    str->str = buf;
    str->len = len;
    v.as_string = str;
    RnValueRef(ctx, out, v, VT_STRING);
}

void
RnUninternedSymbol(RnCtx* ctx, Value* out, Value* name){
    Value id;
    RnValueLink(ctx, &id);
    RnInt64(ctx, &id, 2 /* Symbol */);
    RnRib(ctx, out, name, name, &id);
    RnValueUnlink(ctx, &id);
}

/* Hashtable */
/* FIXME: Uses -1 id for hashtable collision */
static int /* bool */
ht_eqv_blob(Value* x /* String */, Value* y /* String */){
    if(x->type != VT_STRING || y->type != VT_STRING){
        return 0;
    }
    if(x->type != y->type){
        return 0;
    }
    /* fast-path: eq? */
    if(x->value.as_string == y->value.as_string){
        return 1;
    }
    if(x->value.as_string->len != y->value.as_string->len){
        return 0;
    }
    if(0 == memcmp(x->value.as_string->str, y->value.as_string->str,
                   x->value.as_string->len)){
        return 1;
    }
    return 0;
}

static uint64_t
ht_hash_blob(Value* blob){
    size_t off;
    uint64_t reg;
    ObjString* str;

    if(blob->type == VT_STRING){
        /* At this moment, don't assume string is not movable... */
        /* Because we don't deduplicate strings */
        /* Maybe revisited later */
        reg = 0;
        str = blob->value.as_string;
        for(off = 0; off != str->len; off++){
            reg *= 11;
            reg += str->str[off];
        }
        return reg;
    }else{
        abort();
    }
}

static int /* bool */
ht_eqv_int(Value* x, Value* y){
    if(x->type != VT_INT64 || y->type != VT_INT64){
        return 0;
    }
    if(x->type != y->type){
        return 0;
    }
    if(x->value.as_int64 == y->value.as_int64){
        return 1;
    }
    return 0;
}

static uint64_t
ht_hash_int(Value* obj){
    if(obj->type != VT_INT64){
        abort();
    }
    return obj->value.as_int64;
}

static int /* bool */
ht_eqv_eqv(Value* x, Value* y){
    // FIXME: Move this logic to actual eqv?
    if(x->type != y->type){
        return 0;
    }
    switch(x->type){
        case VT_ZONE0:
            if(x->value.as_zone0 == y->value.as_zone0){
                return 1;
            }
            break;
        case VT_INT64:
            if(x->value.as_int64 == y->value.as_int64){
                return 1;
            }
            break;
        case VT_DOUBLE:
            if(x->value.as_double == y->value.as_double){
                return 1;
            }
            break;
        case VT_CHAR:
            if(x->value.as_char == y->value.as_char){
                return 1;
            }
            break;
        case VT_STRING:
            if(x->value.as_string == y->value.as_string){
                return 1;
            }
            break;
        case VT_BYTEVECTOR:
            if(x->value.as_bytevector == y->value.as_bytevector){
                return 1;
            }
            break;
        case VT_RIB:
            if(x->value.as_rib == y->value.as_rib){
                return 1;
            }
            break;
        case VT_VECTOR: /* UNIMPL */
        case VT_SIMPLE_STRUCT: /* UNIMPL */
        case VT_HASHTABLE: /* UNIMPL */
        default:
            abort();
    }
    return 0;
}

static uint64_t
ht_hash_eqv(Value* x){
    switch(x->type){
        case VT_ZONE0:
            return 181 * (int)x->value.as_zone0;
        case VT_INT64:
            return x->value.as_int64;
        case VT_DOUBLE:
            /* Read the value as int64: We don't have to guarantee Nan
             * hash value */
            return x->value.as_int64;
        case VT_CHAR:
            return 781 * x->value.as_char;
        case VT_STRING:
            return ht_hash_blob(x);
        case VT_BYTEVECTOR:
            return (uintptr_t)x->value.as_bytevector;
        case VT_RIB:
            return (uintptr_t)x->value.as_rib;
        case VT_VECTOR: /* UNIMPL */
        case VT_SIMPLE_STRUCT: /* UNIMPL */
        case VT_HASHTABLE: /* UNIMPL */
        default:
            abort();
    }
}

static ssize_t /* loc or -1 */
ht_lookup(RnCtx* ctx, ObjHashtable* ht, size_t start, Value* key){
    ObjRib* r;
    Value v;
    ssize_t loc;
    int e;
    RnValueLink(ctx, &v);
    r = ht->table[start].as_rib;
    while(1){
        if(! r){
            loc = -1; /* Not found */
            break;
        }
        loc = r->field[0].as_int64;
        if(loc < 0){
            abort();
        }
        if((size_t)loc >= ht->containercount){
            abort();
        }
        switch(ht->hashtable_type){
            case HT_BLOBKEY:
                RnValueRef(ctx, &v, ht->keys[loc], VT_STRING);
                e = ht_eqv_blob(&v, key);
                break;
            case HT_INTKEY:
                RnValueRef(ctx, &v, ht->keys[loc], VT_INT64);
                e = ht_eqv_int(&v, key);
                break;
            case HT_EQV:
                RnValueRef(ctx, &v, ht->keys[loc], ht->keytypes[loc]);
                e = ht_eqv_eqv(&v, key);
                break;
            default:
                abort();
                break;
        }
        if(e){
            break;
        }
        r = r->field[1].as_rib;
    }
    RnValueUnlink(ctx, &v);
    return loc;
}

static size_t
ht_add_value(RnCtx* ctx, ObjHashtable* ht, Value* key, Value* obj){
    size_t i;
    uintptr_t target, newsize;
    /* Pass1: Search for empty slot(value type is VT_EMPTY) */
    for(i=0; i!=ht->containercount; i++){
        if(ht->valuetypes[i] == VT_EMPTY){
            break;
        }
    }
    target = i;
    if(target == ht->containercount){
        /* Resize hashtable */
        newsize = ht->containercount * 2;
        ht->keys = (ValueContainer*)realloc(ht->keys, 
                                            sizeof(ValueContainer) * newsize);
        if(! ht->keys){
            abort();
        }
        ht->values = (ValueContainer*)realloc(ht->values, 
                                              sizeof(ValueContainer) * newsize);
        if(! ht->values){
            abort();
        }
        ht->valuetypes = (ValueType*)realloc(ht->valuetypes, 
                                             sizeof(ValueType) * newsize);
        if(! ht->valuetypes){
            abort();
        }
        if(ht->keytypes){
            ht->keytypes = (ValueType*)realloc(ht->keytypes,
                                               sizeof(ValueType) * newsize);
            if(! ht->keytypes){
                abort();
            }

        }
        for(i=ht->containercount; i!=newsize; i++){
            ht->valuetypes[i] = VT_EMPTY;
            if(ht->keytypes){
                ht->keytypes[i] = VT_EMPTY;
            }
        }
        ht->containercount = newsize;
    }

    ht->keys[target] = key->value;
    if(ht->keytypes){
        ht->keytypes[target] = key->type;
    }
    ht->values[target] = obj->value;
    ht->valuetypes[target] = obj->type;

    switch(ht->hashtable_type){
        case HT_BLOBKEY:
            RnRef(ctx, ht->keys[target], VT_STRING);
            break;
        case HT_INTKEY:
            RnRef(ctx, ht->keys[target], VT_INT64);
            break;
        case HT_EQV:
            RnRef(ctx, ht->keys[target], ht->keytypes[target]);
            break;
        default:
            abort();
            break;
    }

    RnRef(ctx, ht->values[target], ht->valuetypes[target]);
    ht->keycount++;

    return target;
}

static size_t /* hash */
ht_hash(ObjHashtable* ht, Value* key){
    uint64_t hashk;
    switch(ht->hashtable_type){
        case HT_BLOBKEY:
            hashk = ht_hash_blob(key);
            break;
        case HT_INTKEY:
            hashk = ht_hash_int(key);
            break;
        case HT_EQV:
            hashk = ht_hash_eqv(key);
            break;
        default:
            abort();
            break;
    }
    return hashk % ht->tablesize;
}


void
RnHashtableRef(RnCtx* ctx, Value* out, Value* ht, Value* key, Value* def){
    size_t hashk;
    ssize_t loc;
    ObjHashtable* hto;
    Value frame;
    RnEnter(ctx, &frame);
    if(ht->type != VT_HASHTABLE){
        abort();
    }

    hto = ht->value.as_hashtable;
    hashk = ht_hash(hto, key);
    loc = ht_lookup(ctx, hto, hashk, key);
    if(loc == -1){
        RnValueRef(ctx, out, def->value, def->type);
    }else{
        RnValueRef(ctx, out, hto->values[loc], hto->valuetypes[loc]);
    }
    RnLeave(ctx, &frame);
}

void
RnHashtableSet(RnCtx* ctx, Value* ht, Value* key, Value* obj){
    size_t hashk;
    ssize_t loc;
    Value frame;
    Value r;
    Value me;
    Value zero;
    Value next;
    ObjHashtable* hto;
    RnEnter(ctx, &frame);
    RnValueLink(ctx, &r);
    RnValueLink(ctx, &me);
    RnValueLink(ctx, &zero);
    RnValueLink(ctx, &next);

    if(ht->type != VT_HASHTABLE){
        abort();
    }
    hto = ht->value.as_hashtable;
    hashk = ht_hash(hto, key);

    if(! hto->table[hashk].as_rib){
        loc = ht_add_value(ctx, hto, key, obj);
        RnInt64(ctx, &me, loc);
        RnInt64(ctx, &zero, 0);
        RnRib(ctx, &r, &me, &zero, &zero);
        hto->table[hashk] = r.value;
        RnRef(ctx, hto->table[hashk], VT_RIB);
    }else{
        loc = ht_lookup(ctx, hto, hashk, key);
        if(loc != -1){
            /* Replace value on loc */
            RnUnref(ctx, &hto->values[loc], &hto->valuetypes[loc]);
            hto->values[loc] = obj->value;
            hto->valuetypes[loc] = obj->type;
            RnRef(ctx, hto->values[loc], hto->valuetypes[loc]);
        }else{
            /* Hash collision, add new loc and chain it as rib */
            loc = ht_add_value(ctx, hto, key, obj);
            RnInt64(ctx, &me, loc);
            RnInt64(ctx, &zero, 0);
            RnValueRef(ctx, &next, hto->table[hashk], VT_RIB);
            RnRib(ctx, &r, &me, &next, &zero);
            hto->table[hashk] = r.value;
            RnRef(ctx, hto->table[hashk], VT_RIB);
        }
    }
    RnLeave(ctx, &frame);
}

void
RnHashtable(RnCtx* ctx, Value* out, HashtableClass htc){
    ValueContainer ptrv;
    ObjHashtable* ht;
    size_t i;
    enum HashtableType_e type;

    switch(htc){
        case HTC_EQ_HASHTABLE:
        case HTC_EQV_HASHTABLE:
            type = HT_EQV;
            break;
        case HTC_INTEGER_HASHTABLE:
            type = HT_INTKEY;
            break;
        case HTC_STRING_HASHTABLE:
        case HTC_SYMBOL_HASHTABLE:
            type = HT_BLOBKEY;
            break;
        default:
            abort();
            break;
    }
    ht = (ObjHashtable*)malloc(sizeof(ObjHashtable));
    if(! ht){
        abort();
    }
    RnObjHeaderInit(ctx, (ObjHeader*)ht);

    ht->tablesize = 57;
    ht->table = (ValueContainer*)malloc(sizeof(ValueContainer) * ht->tablesize);
    if(! ht->table){
        abort();
    }

    ht->containercount = 16;
    ht->keycount = 0;
    ht->hashtable_type = type;
    ht->hashtable_class = htc;

    ht->keys = (ValueContainer*)malloc(sizeof(ValueContainer) * ht->containercount);
    if(! ht->keys){
        abort();
    }
    ht->valuetypes = (ValueType*)malloc(sizeof(ValueType) * ht->containercount);
    if(! ht->valuetypes){
        abort();
    }
    ht->values = (ValueContainer*)malloc(sizeof(ValueContainer) * ht->containercount);
    if(! ht->values){
        abort();
    }
    if(type == HT_EQV){
        ht->keytypes = (ValueType*)malloc(sizeof(ValueType) * ht->containercount);
        if(! ht->keytypes){
            abort();
        }
    }else{
        ht->keytypes = 0;
    }

    for(i=0;i != ht->tablesize; i++){
        ht->table[i].as_rib = 0;
    }

    for(i=0; i!= ht->containercount; i++){
        ht->valuetypes[i] = VT_EMPTY;
        if(type == HT_EQV){
            ht->keytypes[i] = VT_EMPTY;
        }
    }

    ptrv.as_hashtable = ht;
    RnValueRef(ctx, out, ptrv, VT_HASHTABLE);
}

/* Context */
void
RnCtxInit(RnCtx* newctx){
    newctx->current_frame = 0;
    RnEnter(newctx, &newctx->ctx_root);
    RnValueLink(newctx, &newctx->ht_global);
    RnValueLink(newctx, &newctx->ht_libcode);
    RnValueLink(newctx, &newctx->ht_macro);
    RnValueLink(newctx, &newctx->bootstrap);

    RnHashtable(newctx, &newctx->ht_global, HTC_STRING_HASHTABLE);
}

/* dryPack loader */
static uint64_t
get_leb128(const uint8_t** cur){
    const uint8_t* m;
    uint8_t b;
    uint64_t acc = 0;
    uint64_t mult = 1;
    m = *cur;
    while(1){
        b = *m;
        m++;
        if(b < 128){ /* Term */
            acc += (b * mult);
            break;
        }else{
            b -= 128;
            acc += (b * mult);
        }
        mult *= 128;
    }
    *cur = m;
    return acc;
}

static uint8_t
get_b8(const uint8_t** cur){
    uint8_t r;
    const uint8_t* m;
    m = *cur;
    r = *m;
    m++;
    *cur = m;
    return r;

}

static char*
get_cstr(const uint8_t** cur){
    size_t i, len;
    char* p;
    len = get_leb128(cur);
    p = (char*)malloc(len+1);
    if(! p){
        abort();
    }
    for(i=0; i!=len; i++){
        p[i] = get_b8(cur);
    }
    p[len] = 0;
    return p;
}

static void
load_bootstrap(RnCtx* ctx, const uint8_t* bin){
    const uint8_t* p;
    size_t i,j;
    size_t cur;
    size_t total;
    size_t output;
    size_t zone0_count;
    size_t chars;
    size_t numbers;
    size_t symbols;
    size_t strings;
    size_t bytevectors;
    size_t pairs;
    size_t vectors;
    size_t pairoff;
    size_t vectoroff;
    uint64_t proto;
    uint64_t val;
    double d;
    char* num;
    char* nam;
    size_t namlen;
    Value frame;
    Value* v;
    Value zero;
    Value tmp;
    Value sym;
    Value symnam;

    p = bin;

    total = get_leb128(&p);
    output = get_leb128(&p);
    zone0_count = get_leb128(&p);
    chars = get_leb128(&p);
    numbers = get_leb128(&p);
    symbols = get_leb128(&p);
    strings = get_leb128(&p);
    bytevectors = get_leb128(&p);
    pairs = get_leb128(&p);
    vectors = get_leb128(&p);

    if(4 != zone0_count){
        abort();
    }

    RnEnter(ctx, &frame);
    v = (Value*)malloc(sizeof(Value)*total);
    if(! v){
        abort();
    }
    for(cur = 0; cur != total; cur++){
        RnValueLink(ctx, &v[cur]);
    }
    RnValueLink(ctx, &zero);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &sym);
    RnValueLink(ctx, &symnam);
    RnInt64(ctx, &zero, 0);

    /* Zone0 */
    RnZone0(ctx, &v[0], ZZ_NIL);
    RnZone0(ctx, &v[1], ZZ_TRUE);
    RnZone0(ctx, &v[2], ZZ_FALSE);
    RnZone0(ctx, &v[3], ZZ_EOF_OBJECT);

    cur = 4;
    pairoff = cur + chars + numbers + symbols + strings + bytevectors;
    vectoroff = pairoff + pairs;

    /* Chars */
    for(i = 0; i != chars; i++){
        RnChar(ctx, &v[cur+i], get_leb128(&p));
    }
    cur += chars;

    /* Numbers */
    for(i = 0; i != numbers; i++){
        proto = get_leb128(&p);
        switch(proto){
            case 1: /* Positive integer */
                val = get_leb128(&p);
                RnInt64(ctx, &v[cur+i], val);
                break;
            case 2: /* Negative integer */
                val = (int64_t)0 - get_leb128(&p);
                RnInt64(ctx, &v[cur+i], val);
                break;
            case 3: /* Scheme number */
                num = get_cstr(&p);
                d = strtod(num, NULL);
                free(num);
                RnDouble(ctx, &v[cur+i], d);
                break;
            default:
                abort();
                break;
        }
    }
    cur += numbers;

    /* Symbols */
    for(i = 0; i != symbols; i++){
        nam = get_cstr(&p); /* FIXME: use dedicated string function */
        namlen = strlen(nam);
        RnString(ctx, &symnam, nam, namlen);
        free(nam);
        RnUninternedSymbol(ctx, &sym, &symnam);
        RnHashtableRef(ctx, &tmp, &ctx->ht_global, &symnam, &sym);
        if(tmp.value.as_rib == sym.value.as_rib){
            RnHashtableSet(ctx, &ctx->ht_global, &symnam, &sym);
        }
        RnValueRef(ctx, &v[cur+i], sym.value, sym.type);
    }
    cur += symbols;

    /* Strings */
    for(i = 0; i != strings; i++){
        nam = get_cstr(&p); /* FIXME: use dedicated string function */
        namlen = strlen(nam);
        RnString(ctx, &v[cur+i], nam, namlen);
        free(nam);
    }
    cur += strings;

    /* Bytevectors */
    for(i = 0; i != bytevectors; i++){
        val = get_leb128(&p);
        RnBytevector(ctx, &v[cur+i], val);
        for(j = 0; j != val; j++){
            v[cur+i].value.as_bytevector->buf[j] = get_b8(&p);
        }
    }

    /* Pass1 create idx pair/vectors */
    cur = pairoff;
    for(i = 0; i != pairs; i++){
        RnCons(ctx, &v[cur+i], &zero, &zero);
        RnInt64(ctx, &tmp, get_leb128(&p));
        RnRibSet(ctx, &v[cur+i], &tmp, 0);
        RnInt64(ctx, &tmp, get_leb128(&p));
        RnRibSet(ctx, &v[cur+i], &tmp, 1);
    }

    cur = vectoroff;
    for(i = 0; i != vectors; i++){
        val = get_leb128(&p);
        if(val == 3){
            /* Create Rib first */
            /* FIXME: Decode to vector later */
            RnRib(ctx, &v[cur+i], &zero, &zero, &zero);
            RnInt64(ctx, &tmp, get_leb128(&p));
            RnRibSet(ctx, &v[cur+i], &tmp, 0);
            RnInt64(ctx, &tmp, get_leb128(&p));
            RnRibSet(ctx, &v[cur+i], &tmp, 1);
            RnInt64(ctx, &tmp, get_leb128(&p));
            RnRibSet(ctx, &v[cur+i], &tmp, 2);
        }else{
            RnVector(ctx, &v[cur+i], val);
            for(j = 0; j != val; j++){
                RnInt64(ctx, &tmp, get_leb128(&p));
                RnVectorSet(ctx, &v[cur+i], &tmp, j);
            }
        }
    }

    /* Pass2 replace address into actual objects */
    cur = pairoff;
    for(i = 0; i != pairs; i++){
        val = v[cur+i].value.as_rib->field[0].as_int64;
        RnRibSet(ctx, &v[cur+i], &v[val], 0);
        val = v[cur+i].value.as_rib->field[1].as_int64;
        RnRibSet(ctx, &v[cur+i], &v[val], 1);
    }

    cur = vectoroff;
    for(i = 0; i != vectors; i++){
        if(v[cur+i].type == VT_RIB){
            val = v[cur+i].value.as_rib->field[0].as_int64;
            RnRibSet(ctx, &v[cur+i], &v[val], 0);
            val = v[cur+i].value.as_rib->field[1].as_int64;
            RnRibSet(ctx, &v[cur+i], &v[val], 1);
            val = v[cur+i].value.as_rib->field[2].as_int64;
            RnRibSet(ctx, &v[cur+i], &v[val], 2);
        }else{
            for(j = 0; j != v[cur+i].value.as_vector->length; j++){
                val = v[cur+i].value.as_vector->values[j].as_int64;
                RnVectorSet(ctx, &v[cur+i], &v[val], j);
            }
        }
    }


    RnValueRef(ctx, &ctx->bootstrap, v[output].value, v[output].type);
    RnLeave(ctx, &frame);
}

/* VM */

/* main */
int
main(int ac, char** av){
    FILE* bin;
    uint8_t* bootstrap;
    long binsize;

    RnCtx ctx;

    (void)ac;
    (void)av;

    bin = fopen("c:/cygwin64/home/oku/repos/yuniribbit-proto/dump.bin", "rb");
    if(! bin){
        abort();
    }
    fseek(bin, 0, SEEK_END);
    binsize = ftell(bin);
    fseek(bin, 0, SEEK_SET);
    bootstrap = (uint8_t*)malloc(binsize);
    if(! bootstrap){
        abort();
    }
    fread(bootstrap, binsize, 1, bin);
    fclose(bin);

    RnCtxInit(&ctx);
    load_bootstrap(&ctx, bootstrap);

    return 0;
}

