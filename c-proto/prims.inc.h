typedef void (*ExArg_0_1)(RnCtx* ctx, Value* out);
typedef void (*ExArg_1_1)(RnCtx* ctx, Value* out, Value* x);
typedef void (*ExArg_2_1)(RnCtx* ctx, Value* out, Value* x, Value* y);
typedef void (*ExArg_2_2)(RnCtx* ctx, Value* out, Value* out2,
                          Value* x, Value* y);
typedef void (*ExArg_3_1)(RnCtx* ctx, Value* out, Value* x, Value* y, Value* z);
typedef void (*ExArg_4_1)(RnCtx* ctx, Value* out, Value* x, Value* y, Value* Z,
                          Value* w);
typedef void (*ExArg_5_1)(RnCtx* ctx, Value* out, Value* x, Value* y, Value* z,
                          Value* w, Value* u);

static void
ExCall_0_1(ExArg_0_1 func, RnCtx* ctx, int argc, Value* stack){
    Value out[1];
    if(argc != 0){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 0);
        abort();
    }
    RnValueLink(ctx, &out[0]);
    func(ctx, &out[0]);
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
}

static void
ExCall_1_1(ExArg_1_1 func, RnCtx* ctx, int argc, Value* stack){
    Value arg[1];
    Value out[1];
    if(argc != 1){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 1);
        abort();
    }
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[0]);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    func(ctx, &out[0], &arg[0]);
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
}

static void
ExCall_2_1(ExArg_2_1 func, RnCtx* ctx, int argc, Value* stack){
    Value arg[2];
    Value out[1];
    if(argc != 2){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 2);
        abort();
    }
    RnValueLink(ctx, &arg[1]);
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[0]);
    RnRibRef(ctx, &arg[1], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    func(ctx, &out[0], &arg[0], &arg[1]);
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
}

static void
ExCall_2_2(ExArg_2_2 func, RnCtx* ctx, int argc, Value* stack){
    Value tmp;
    Value six;
    Value zero;
    Value nil;
    Value arg[2];
    Value out[2];
    if(argc != 2){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 2);
        abort();
    }
    RnValueLink(ctx, &arg[1]);
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[1]);
    RnValueLink(ctx, &out[0]);
    RnValueLink(ctx, &nil);
    RnValueLink(ctx, &zero);
    RnValueLink(ctx, &six);
    RnValueLink(ctx, &tmp);
    RnInt64(ctx, &six, 6);
    RnInt64(ctx, &zero, 0);
    RnZone0(ctx, &nil, ZZ_NIL);
    RnRibRef(ctx, &arg[1], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    func(ctx, &out[0], &out[1], &arg[0], &arg[1]);
    RnCons(ctx, &tmp, &out[1], &nil);
    RnCons(ctx, &tmp, &out[0], &tmp);
    RnRib(ctx, &tmp, &tmp, &zero, &six);
    RnCons(ctx, stack, &tmp, stack);
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &six);
    RnValueUnlink(ctx, &zero);
    RnValueUnlink(ctx, &nil);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &out[1]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
}

static void
ExCall_3_1(ExArg_3_1 func, RnCtx* ctx, int argc, Value* stack){
    Value arg[3];
    Value out[1];
    if(argc != 3){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 3);
        abort();
    }
    RnValueLink(ctx, &arg[2]);
    RnValueLink(ctx, &arg[1]);
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[0]);
    RnRibRef(ctx, &arg[2], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[1], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    func(ctx, &out[0], &arg[0], &arg[1], &arg[2]);
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RnValueUnlink(ctx, &arg[2]);
}

static void
ExCall_4_1(ExArg_4_1 func, RnCtx* ctx, int argc, Value* stack){
    Value arg[4];
    Value out[1];
    if(argc != 4){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 4);
        abort();
    }
    RnValueLink(ctx, &arg[3]);
    RnValueLink(ctx, &arg[2]);
    RnValueLink(ctx, &arg[1]);
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[0]);
    RnRibRef(ctx, &arg[3], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[2], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[1], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    func(ctx, &out[0], &arg[0], &arg[1], &arg[2], &arg[3]);
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RnValueUnlink(ctx, &arg[2]);
    RnValueUnlink(ctx, &arg[3]);
}

static void
ExCall_5_1(ExArg_5_1 func, RnCtx* ctx, int argc, Value* stack){
    Value arg[5];
    Value out[1];
    if(argc != 5){
        fprintf(stderr, "Invalid argument count %d (expected %d)\n",
                argc, 5);
        abort();
    }
    RnValueLink(ctx, &arg[4]);
    RnValueLink(ctx, &arg[3]);
    RnValueLink(ctx, &arg[2]);
    RnValueLink(ctx, &arg[1]);
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[0]);
    RnRibRef(ctx, &arg[4], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[3], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[2], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[1], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    func(ctx, &out[0], &arg[0], &arg[1], &arg[2], &arg[3], &arg[4]);
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RnValueUnlink(ctx, &arg[2]);
    RnValueUnlink(ctx, &arg[3]);
    RnValueUnlink(ctx, &arg[4]);
}


#define VMBRIDGENAME_0_1(call) Bridge ## call
#define VMBRIDGENAME_1_1(call) Bridge ## call
#define VMBRIDGENAME_2_1(call) Bridge ## call
#define VMBRIDGENAME_2_2(call) Bridge ## call
#define VMBRIDGENAME_3_1(call) Bridge ## call
#define VMBRIDGENAME_4_1(call) Bridge ## call
#define VMBRIDGENAME_5_1(call) Bridge ## call
#define VMBRIDGENAME_N(call) call

#define VMBRIDGE_N(call)
#define VMBRIDGE_0_1(call) VMBRIDGEGEN(_0_1, call)
#define VMBRIDGE_1_1(call) VMBRIDGEGEN(_1_1, call)
#define VMBRIDGE_2_1(call) VMBRIDGEGEN(_2_1, call)
#define VMBRIDGE_2_2(call) VMBRIDGEGEN(_2_2, call)
#define VMBRIDGE_3_1(call) VMBRIDGEGEN(_3_1, call)
#define VMBRIDGE_4_1(call) VMBRIDGEGEN(_4_1, call)
#define VMBRIDGE_5_1(call) VMBRIDGEGEN(_5_1, call)

#define VMBRIDGEGEN(f,call) \
    static void \
    VMBRIDGENAME ## f(call)(RnCtx* ctx, int argc, Value* stack){ \
        ExCall ## f(call, ctx, argc, stack); \
    } 

#define GEN_FILD(nam, func, proto) {nam, sizeof(nam), VMBRIDGENAME ## proto(func)},
#define GEN_BRIDGE(_, func, proto) VMBRIDGE ## proto(func)

static void
to_bool(RnCtx* ctx, Value* out, int x){
    if(x){
        RnZone0(ctx, out, ZZ_TRUE);
    }else{
        RnZone0(ctx, out, ZZ_FALSE);
    }
}

static void
ExCharP(RnCtx* ctx, Value* out, Value* x){
    to_bool(ctx, out, x->type == VT_CHAR);
}

static void
ExFixnumP(RnCtx* ctx, Value* out, Value* x){
    to_bool(ctx, out, x->type == VT_INT64);
}

static void
ExFlonumP(RnCtx* ctx, Value* out, Value* x){
    to_bool(ctx, out, x->type == VT_DOUBLE);
}

static void
ExRibP(RnCtx* ctx, Value* out, Value* x){
    to_bool(ctx, out, x->type == VT_RIB);
}

static void
ExEqvP(RnCtx* ctx, Value* out, Value* x, Value* y){
    int r;
    if(x->type != y->type){
        to_bool(ctx, out, 0);
        return;
    }
    switch(x->type){
        case VT_DOUBLE:
            r = (x->value.as_double == y->value.as_double);
            break;
        case VT_INT64:
            r = (x->value.as_int64 == y->value.as_int64);
            break;
        case VT_ZONE0:
            r = (x->value.as_zone0 == y->value.as_zone0);
            break;
        case VT_CHAR:
            r = (x->value.as_char == y->value.as_char);
            break;
        case VT_RIB:
            r = (x->value.as_rib == y->value.as_rib);
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            r = (x->value.as_vector == y->value.as_vector);
            break;
        case VT_HASHTABLE:
            r = (x->value.as_hashtable == y->value.as_hashtable);
            break;
        case VT_STRING:
            r = (x->value.as_string == y->value.as_string);
            break;
        case VT_BYTEVECTOR:
            r = (x->value.as_bytevector == y->value.as_bytevector);
            break;
        default:
            r = x == y;
            break;
    }
    to_bool(ctx, out, r);
}

static void
ExProcedureP(RnCtx* ctx, Value* out, Value* x){
    int r;
    Value tmp;
    RnValueLink(ctx, &tmp);
    if(x->type != VT_RIB){
        r = 0;
    }else{
        RnRibRef(ctx, &tmp, x, 2);
        if(tmp.type == VT_INT64 && tmp.value.as_int64 == 1){
            r = 1;
        }else{
            r = 0;
        }
        to_bool(ctx, out, r);
        RnValueUnlink(ctx, &tmp);
    }
}

static void
ExValues(RnCtx* ctx, int argc, Value* stack){
    int i;
    Value six;
    Value tmp;
    Value tmp2;
    if(argc == 1){
        /* Do nothing */
    }else{
        RnValueLink(ctx, &tmp2);
        RnValueLink(ctx, &tmp);
        RnValueLink(ctx, &six);
        RnInt64(ctx, &six, 6);
        RnZone0(ctx, &tmp, ZZ_NIL);
        for(i=0;i!=argc;i++){
            RnRibRef(ctx, &tmp2, stack, 0);
            RnCons(ctx, &tmp, &tmp2, &tmp);
            RnRibRef(ctx, stack, stack, 1);
        }
        RnCons(ctx, stack, &tmp, stack);
        RnValueUnlink(ctx, &six);
        RnValueUnlink(ctx, &tmp);
        RnValueUnlink(ctx, &tmp2);
    }
}

static void
ExListToValues(RnCtx* ctx, Value* out, Value* x){
    Value zero;
    Value six;
    RnValueLink(ctx, &zero);
    RnValueLink(ctx, &six);
    RnInt64(ctx, &zero, 0);
    RnInt64(ctx, &six, 6);
    RnRib(ctx, out, x, &zero, &six);
    RnValueUnlink(ctx, &six);
    RnValueUnlink(ctx, &zero);
}

static void
ExStringToSymbol(RnCtx* ctx, Value* out, Value* str){
    Value sym;
    if(str->type != VT_STRING){
        abort();
    }
    RnValueLink(ctx, &sym);
    RnUninternedSymbol(ctx, &sym, str);
    RnHashtableRef(ctx, out, &ctx->ht_global, str, &sym);
    if(out->value.as_rib == sym.value.as_rib){
        RnHashtableSet(ctx, &ctx->ht_global, str, &sym);
    }
    RnValueUnlink(ctx, &sym);
}

static void
ExSymbolToString(RnCtx* ctx, Value* out, Value* sym){
    Value tmp;
    RnValueLink(ctx, &tmp);
    RnRibRef(ctx, &tmp, sym, 2);
    if(tmp.type != VT_INT64 || tmp.value.as_int64 != 2){
        abort();
    }
    RnRibRef(ctx, out, sym, 1);
    RnValueUnlink(ctx, &tmp);
}

static void
ExCharToInteger(RnCtx* ctx, Value* out, Value* c){
    if(c->type != VT_CHAR){
        abort();
    }
    RnInt64(ctx, out, c->value.as_char);
}

static void
ExIntegerToChar(RnCtx* ctx, Value* out, Value* i){
    if(i->type != VT_INT64){
        abort();
    }
    RnChar(ctx, out, i->value.as_int64);
}

static void
ExUtf8ToString_3_1(RnCtx* ctx, Value* out, Value* bv, Value* start, Value* end){
    ssize_t istart;
    ssize_t iend;
    ssize_t ilen;
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    istart = start->value.as_int64;
    iend = start->value.as_int64;
    if(istart < 0){
        abort();
    }
    if(istart > iend){
        abort();
    }
    ilen = iend - istart;

    RnString(ctx, out, (char*)bv->value.as_bytevector->buf + istart, ilen);
}

static void
ExUtf8ToString_2_1(RnCtx* ctx, Value* out, Value* bv, Value* start){
    ssize_t istart;
    ssize_t iend;
    ssize_t ilen;
    if(start->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    istart = start->value.as_int64;
    iend = bv->value.as_bytevector->len;
    if(istart < 0){
        abort();
    }
    if(istart > iend){
        abort();
    }
    ilen = iend - istart;

    RnString(ctx, out, (char*)bv->value.as_bytevector->buf + istart, ilen);
}

static void
ExUtf8ToString_1_1(RnCtx* ctx, Value* out, Value* bv){
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    RnString(ctx, out, (char*)bv->value.as_bytevector->buf, bv->value.as_bytevector->len);
}

static void
ExUtf8ToString(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExUtf8ToString_1_1, ctx, argc, stack);
            return;
        case 2:
            ExCall_2_1(ExUtf8ToString_2_1, ctx, argc, stack);
            return;
        case 3:
            ExCall_3_1(ExUtf8ToString_3_1, ctx, argc, stack);
            return;
        default:
            abort();
            return;
    }
}

static int /* -1 for broken */
utf8_ebytes(int c){
    if(c < 0x80){
        return 1;
    }else if(c < 0x800){
        return 2;
    }else if(c < 0x10000){
        return 3;
    }else if(c < 0x110000){
        return 4;
    }else{
        return -1;
    }
}

static int /* -1 for broken */
utf8_dbytes(const char* start){
    unsigned char c;
    c = *(const unsigned char*)start;
    if(c < 0x80){
        return 1;
    }else if(c < 0xe0){
        return 2;
    }else if(c < 0xf0){
        return 3;
    }else if(c < 0xf5){
        return 4;
    }else{
        return -1;
    }
}

static int
utf8_decode(const char* start, const char* stop){
    int r;
    int reg;
    int acc;
    r = utf8_dbytes(start);
    if(r<=0){
        abort();
    }
    if(r>4){
        abort();
    }
    if(start + r >= stop){
        abort();
    }
    switch(r){
        case 1:
            reg = *(const unsigned char*)start;
            return reg;
        case 2:
            reg = *(const unsigned char*)start;
            acc = reg & 0x1f;
            acc <<= 6;
            start++;
            reg = *(const unsigned char*)start;
            acc += reg & 0x3f;
            return acc;
        case 3:
            reg = *(const unsigned char*)start;
            acc = reg & 0xf;
            acc <<= 6;
            start++;
            reg = *(const unsigned char*)start;
            acc += reg & 0x3f;
            acc <<= 6;
            start++;
            reg = *(const unsigned char*)start;
            acc += reg & 0x3f;
            return acc;
        case 4:
            reg = *(const unsigned char*)start;
            acc = reg & 7;
            acc <<= 6;
            start++;
            reg = *(const unsigned char*)start;
            acc += reg & 0x3f;
            acc <<= 6;
            start++;
            reg = *(const unsigned char*)start;
            acc += reg & 0x3f;
            acc <<= 6;
            start++;
            reg = *(const unsigned char*)start;
            acc += reg & 0x3f;
            return acc;
    }
    abort();
    return 0;
}

static void
utf8_encode(char* start, const char* stop, int c){
    int r;
    r = utf8_ebytes(c);
    if(r<=0){
        abort();
    }
    if(r>4){
        abort();
    }
    if(start + r >= stop){
        abort();
    }
    switch(r){
        case 1:
            *(unsigned char*)start = c;
            return;
        case 2:
            start += 1;
            *(unsigned char*)start = ((c & 0x3f) | 0x80);
            start--;
            c >>= 6;
            *(unsigned char*)start = ((c & 0x1f) | 0xc0);
            return;
        case 3:
            start += 2;
            *(unsigned char*)start = ((c & 0x3f) | 0x80);
            start--;
            c >>= 6;
            *(unsigned char*)start = ((c & 0x3f) | 0x80);
            start--;
            c >>= 6;
            *(unsigned char*)start = ((c & 0xf) | 0xe0);
            return;
        case 4:
            start += 3;
            *(unsigned char*)start = ((c & 0x3f) | 0x80);
            start--;
            c >>= 6;
            *(unsigned char*)start = ((c & 0x3f) | 0x80);
            start--;
            c >>= 6;
            *(unsigned char*)start = ((c & 0x3f) | 0x80);
            start--;
            c >>= 6;
            *(unsigned char*)start = ((c & 7) | 0xf0);
            return;
    }
}

static void
vector_to_string(RnCtx* ctx, Value* out, ObjVector* vec, 
                 size_t start, size_t end){
    int r,c;
    size_t siz;
    size_t i;
    char* s;
    char* p;
    char* pend;
    ValueContainer v;
    ObjString* str;
    /* Pass1: Calc total string size */
    siz = 0;
    if(start > end){
        abort();
    }
    for(i=start; i!= end; i++){
        if(vec->types[i] != VT_CHAR){
            abort();
        }
        r = utf8_ebytes(vec->values[i].as_char);
        if(r <= 0){
            abort();
        }
        siz += r;
    }

    str = (ObjString*)malloc(sizeof(ObjString));
    if(! str){
        abort();
    }
    s = (char*)malloc(siz + 1);
    s[siz] = 0;

    /* Pass2: Convert to utf8 blob */
    p = s;
    pend = s + siz;
    for(i=start; i!= end; i++){
        c = vec->values[i].as_char;
        utf8_encode(p, pend, c);
        p += utf8_ebytes(c);
    }
    str->str = (const char*)p;
    str->len = siz;
    str->refcnt = 0;
    v.as_string = str;

    RnValueRef(ctx, out, v, VT_STRING);
}

static void
ExVectorToString_3_1(RnCtx* ctx, Value* out, Value* vec, Value* start,
                     Value* end){
    if(vec->type != VT_VECTOR){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    vector_to_string(ctx, out, vec->value.as_vector,
                     start->value.as_int64, end->value.as_int64);
}

static void
ExVectorToString_2_1(RnCtx* ctx, Value* out, Value* vec, Value* start){
    if(vec->type != VT_VECTOR){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    vector_to_string(ctx, out, vec->value.as_vector,
                     start->value.as_int64, vec->value.as_vector->length);
}

static void
ExVectorToString_1_1(RnCtx* ctx, Value* out, Value* vec){
    if(vec->type != VT_VECTOR){
        abort();
    }
    vector_to_string(ctx, out, vec->value.as_vector,
                     0, vec->value.as_vector->length);
}

static void
ExVectorToString(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExVectorToString_1_1, ctx, argc, stack);
            break;
        case 2:
            ExCall_2_1(ExVectorToString_2_1, ctx, argc, stack);
            break;
        case 3:
            ExCall_3_1(ExVectorToString_3_1, ctx, argc, stack);
            break;
        default:
            abort();
            break;
    }
}

static void
list_to_vector(RnCtx* ctx, Value* out, Value* lis, int skip, int limit){
    Value input;
    Value tmp;
    ObjString* str;
    ValueContainer v;
    int i,c;
    size_t siz;
    char* s;
    char* p;
    char* pend;
    RnValueLink(ctx, &input);
    RnValueLink(ctx, &tmp);
    /* Skip */
    for(i=0;i!=skip;i++){
        if(lis->type != VT_RIB){
            abort();
        }
        RnRibRef(ctx, lis, lis, 1);
    }
    RnValueRef(ctx, &input, lis->value, lis->type);
    /* Pass1: calc string size */
    RnValueRef(ctx, &tmp, input.value, input.type);
    i = skip;
    siz = 0;
    while(1){
        if(limit == i){
            break;
        }
        if(tmp.type == VT_ZONE0 && tmp.value.as_zone0 == ZZ_NIL){
            break;
        }
        if(tmp.type == VT_RIB){
            if(tmp.value.as_rib->type[0] != VT_CHAR){
                abort();
            }
            c = tmp.value.as_rib->field[0].as_char;
            siz += utf8_ebytes(c);
            RnRibRef(ctx, &tmp, &tmp, 1);
        }else{
            abort();
        }
        i++;
    }

    /* Pass2: Construct string */
    s = (char*)malloc(siz + 1);
    if(! s){
        abort();
    }
    p = s;
    pend = s + siz;
    *pend = 0;

    RnValueRef(ctx, &tmp, input.value, input.type);
    while(1){
        if(limit == i){
            break;
        }
        if(tmp.type == VT_ZONE0 && tmp.value.as_zone0 == ZZ_NIL){
            break;
        }
        if(tmp.type == VT_RIB){
            if(tmp.value.as_rib->type[0] != VT_CHAR){
                abort();
            }
            c = tmp.value.as_rib->field[0].as_char;
            utf8_encode(p, pend, c);
            p += utf8_ebytes(c);
            RnRibRef(ctx, &tmp, &tmp, 1);
        }else{
            abort();
        }
        i++;
    }

    str = (ObjString*)malloc(sizeof(ObjString));
    str->str = (const char*)p;
    str->refcnt = 0;
    str->len = siz;
    v.as_string = str;
    RnValueRef(ctx, out, v, VT_STRING);
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &input);
}

static void
ExListToString_3_1(RnCtx* ctx, Value* out, Value* lis, 
                   Value* start, Value* end){
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    list_to_vector(ctx, out, lis, start->value.as_int64,
                   end->value.as_int64);
}

static void
ExListToString_2_1(RnCtx* ctx, Value* out, Value* lis, Value* start){
    if(start->type != VT_INT64){
        abort();
    }
    list_to_vector(ctx, out, lis, start->value.as_int64, -1);
}

static void
ExListToString_1_1(RnCtx* ctx, Value* out, Value* lis){
    list_to_vector(ctx, out, lis, 0, -1);
}

static void
ExListToString(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExListToString_1_1, ctx, argc, stack);
            break;
        case 2:
            ExCall_2_1(ExListToString_2_1, ctx, argc, stack);
            break;
        case 3:
            ExCall_3_1(ExListToString_3_1, ctx, argc, stack);
            break;
        default:
            abort();
            break;
    }
}

static void
number_to_string(RnCtx* ctx, Value* out, Value* z, int radix){
    char buf[128];
    switch(z->type){
        case VT_INT64:
            switch(radix){
                case 10:
                    snprintf(buf, sizeof(buf), PRId64, z->value.as_int64);
                    break;
                case 16:
                    snprintf(buf, sizeof(buf), PRIx64, z->value.as_int64);
                    break;
                case 8:
                    snprintf(buf, sizeof(buf), PRIo64, z->value.as_int64);
                    break;
                default:
                    abort();
                    break;
            }
            break;
        case VT_DOUBLE:
            snprintf(buf, sizeof(buf), "%f", z->value.as_double);
            break;
        default:
            abort();
            break;
    }
    RnString(ctx, out, buf, strnlen(buf, sizeof(buf)));
}

static void
ExNumberToString_2_1(RnCtx* ctx, Value* out, Value* z, Value* radix){
    if(radix->type != VT_INT64){
        abort();
    }
    number_to_string(ctx, out, z, radix->value.as_int64);
}

static void
ExNumberToString_1_1(RnCtx* ctx, Value* out, Value* z){
    number_to_string(ctx, out, z, 10);
}

static void
ExNumberToString(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExNumberToString_1_1, ctx, argc, stack);
            break;
        case 2:
            ExCall_2_1(ExNumberToString_2_1, ctx, argc, stack);
            break;
        default:
            abort();
            break;
    }
}

static void
string_to_number(RnCtx* ctx, Value* out, Value* s, int radix){
    // FIXME: Implement this seriously
    size_t i;
    ObjString* str;
    int is_inexact;
    double d;
    int64_t reg;
    if(s->type != VT_STRING){
        abort();
    }
    str = s->value.as_string;
    switch(radix){
        case 2:
        case 8:
        case 16:
            reg = strtoimax(str->str, NULL, radix);
            RnInt64(ctx, out, reg);
            break;
        case 10:
            /* Pass1: Scan for a period */
            is_inexact = 0;
            for(i=0; i != str->len; i++){
                if(str->str[i] == '.'){
                    is_inexact = 1;
                    break;
                }
            }

            /* Pass2: Actual conversion */
            d = strtod(str->str, NULL);
            if(round(d) == d && (!is_inexact)){
                RnInt64(ctx, out, (int64_t)d);
            }else{
                RnDouble(ctx, out, d);
            }
            break;
    }
}

static void
ExStringToNumber_2_1(RnCtx* ctx, Value* out, Value* s, Value* radix){
    if(radix->type != VT_INT64){
        abort();
    }
    string_to_number(ctx, out, s, radix->value.as_int64);
}

static void
ExStringToNumber_1_1(RnCtx* ctx, Value* out, Value* s){
    string_to_number(ctx, out, s, 10);
}

static void
ExStringToNumber(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExStringToNumber_1_1, ctx, argc, stack);
            break;
        case 2:
            ExCall_2_1(ExStringToNumber_2_1, ctx, argc, stack);
            break;
        default:
            abort();
            break;
    }
}

static const char*
utf8_skip(const char* start, const char* end, size_t count){
    int r;
    while(start <= end){
        if(count == 0){
            return start;
        }
        r = utf8_dbytes(start);
        if(start + r > end){
            return 0;
        }
        start += r;
        count--;
    }
    if(count == 0){
        return end;
    }else{
        return 0; /* Fail */
    }
}

static void
ExStringToUtf8_3_1(RnCtx* ctx, Value* out, Value* s, Value* start, Value* end){
    const char* begin;
    const char* tail;
    const char* term;
    size_t outlen;
    if(s->type != VT_STRING){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }

    term = s->value.as_string->str + s->value.as_string->len;
    begin = utf8_skip(s->value.as_string->str, term, start->value.as_int64);
    tail = utf8_skip(begin, term, end->value.as_int64 - start->value.as_int64);
    outlen = tail - begin;
    RnBytevector(ctx, out, outlen);
    memcpy(out->value.as_bytevector->buf, begin, outlen);
}

static void
ExStringToUtf8_2_1(RnCtx* ctx, Value* out, Value* s, Value* start){
    const char* begin;
    const char* end;
    size_t outlen;
    if(s->type != VT_STRING){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }

    end = s->value.as_string->str + s->value.as_string->len;
    begin = utf8_skip(s->value.as_string->str, end, start->value.as_int64);
    outlen = end - begin;
    RnBytevector(ctx, out, outlen);
    memcpy(out->value.as_bytevector->buf, begin, outlen);
}

static void
ExStringToUtf8_1_1(RnCtx* ctx, Value* out, Value* s){
    if(s->type != VT_STRING){
        abort();
    }
    RnBytevector(ctx, out, s->value.as_string->len);
    memcpy(out->value.as_bytevector->buf,
           s->value.as_string->str,
           s->value.as_string->len);
}

static void
ExStringToUtf8(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExStringToUtf8_1_1, ctx, argc, stack);
            break;
        case 2:
            ExCall_2_1(ExStringToUtf8_2_1, ctx, argc, stack);
            break;
        case 3:
            ExCall_3_1(ExStringToUtf8_3_1, ctx, argc, stack);
            break;
        default:
            abort();
            break;
    }
}


#define EXLIB_MISC(x) \
    x("char?", ExCharP, _1_1) \
    x("$fixnum?", ExFixnumP, _1_1) \
    x("$flonum?", ExFlonumP, _1_1) \
    x("rib?", ExRibP, _1_1) \
    x("eqv?", ExEqvP, _2_1) \
    x("procedure?", ExProcedureP, _1_1) \
    x("values", ExValues, _N) \
    x("list->values", ExListToValues, _1_1) \
    x("string->symbol", ExStringToSymbol, _1_1) \
    x("symbol->string", ExSymbolToString, _1_1) \
    x("char->integer", ExCharToInteger, _1_1) \
    x("integer->char", ExIntegerToChar, _1_1) \
    x("utf8->string", ExUtf8ToString, _N) \
    x("vector->string", ExVectorToString, _N) \
    x("list->string", ExListToString, _N) \
    x("number->string", ExNumberToString, _N) \
    x("string->number", ExStringToNumber, _N) \
    x("string->utf8", ExStringToUtf8, _N)


/* (char? obj) => <BOOL> */
/* ($fixnum? num) => <BOOL> */
/* ($flonum? num) => <BOOL> */
/* (rib? x) => <BOOL> */
/* (eqv? x y) => <BOOL> */
/* (procedure? x) => <BOOL> */
/* values (VM) */
/* list->values (VM) */
/* string->symbol */
/* (symbol->string s) => str */
/* (char->integer c) => int */
/* (integer->char int) => c */
/* (utf8->string ...) => str */
/* (vector->string ...) => str */
/* (list->string lis) => str */
/* (number->string ...) => str */
/* (string->number ...) => num */
/* (string->utf8 ...) => bv */


/* (rib x y z) => rib */
/* (field0 r) => x */
/* (field1 r) => x */
/* (field2 r) => x */
/* (field0-set! r v) => v */
/* (field1-set! r v) => v */
/* (field2-set! r v) => v */


static void
emergency_print(RnCtx* ctx, Value* v){
    Value tmp;
    size_t i;
    if(! v){
        return;
    }
    RnValueLink(ctx, &tmp);
    switch(v->type){
        case VT_EMPTY:
            fprintf(stderr, "[EMPTY]");
            break;
        case VT_ZONE0:
            switch(v->value.as_zone0){
                case ZZ_NIL:
                    fprintf(stderr, "[NIL]");
                    break;
                case ZZ_TRUE:
                    fprintf(stderr, "#t");
                    break;
                case ZZ_FALSE:
                    fprintf(stderr, "#f");
                    break;
                case ZZ_EOF_OBJECT:
                    fprintf(stderr, "#<eof-object>");
                    break;
                default:
                    fprintf(stderr, "[ZZ:%d]",(int)v->value.as_zone0);
                    break;
            }
            break;
        case VT_INT64:
            fprintf(stderr, PRId64, v->value.as_int64);
            break;
        case VT_DOUBLE:
            fprintf(stderr, "%f", v->value.as_double);
            break;
        case VT_CHAR:
            // FIXME: UTF8
            fprintf(stderr, "%c", v->value.as_char);
            break;
        case VT_RIB:
            fprintf(stderr, "#<");
            RnRibRef(ctx, &tmp, v, 0);
            emergency_print(ctx, &tmp);
            fprintf(stderr, " ");
            RnRibRef(ctx, &tmp, v, 1);
            emergency_print(ctx, &tmp);
            fprintf(stderr, " ");
            RnRibRef(ctx, &tmp, v, 2);
            emergency_print(ctx, &tmp);
            fprintf(stderr, ">");
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            for(i=0;i!=v->value.as_vector->length;i++){
                if(i==0){
                    fprintf(stderr, "#(");
                }else{
                    fprintf(stderr, " ");
                }
                RnVectorRef(ctx, &tmp, v, i);
                emergency_print(ctx, &tmp);
            }
            fprintf(stderr, ")");
            break;
        case VT_HASHTABLE:
            fprintf(stderr, "#<HASHTABLE>");
            break;
        case VT_STRING:
            fprintf(stderr, "\"%s\"", v->value.as_string->str);
            break;
        case VT_BYTEVECTOR:
            fprintf(stderr, "#<BYTEVECTOR>");
            break;
        case VT_ROOT:
            fprintf(stderr, "#<ROOT>");
            break;
        default:
            fprintf(stderr, "#<UNKNOWN:%d>", (int)v->type);
            break;

    }
    RnValueUnlink(ctx, &tmp);
}

/* $error/core (VM) */
static void
ExErrorCore(RnCtx* ctx, int argc, Value* stack){
    int i;
    Value tmp, tmp2;
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &tmp2);
    fprintf(stderr, "Error:\n");
    if(argc<0){
        argc=0;
    }
    if(stack->type == VT_RIB){
        RnValueRef(ctx, &tmp, stack->value, stack->type);
        for(i=0;i!=argc;i++){
            fprintf(stderr, "  %d: ", i);
            if(tmp.type != VT_RIB){
                fprintf(stderr, "BROKEN\n");
                break;
            }
            RnRibRef(ctx, &tmp2, &tmp, 0);
            emergency_print(ctx, &tmp2);
            fprintf(stderr, "\n");
            RnRibRef(ctx, &tmp, &tmp, 1);
        }
    }else{
        fprintf(stderr, "BROKEN STACK!\n");
        emergency_print(ctx, stack);
    }
    abort();
}


/* (id x) => x */
/* arg1 -- drop a value on stack */
/* (arg2 y x) => x */
/* (vminject x) => y */
/* (vmfetchcode x) => y */
/* (vmfetch x) => y */
/* close (VM) */
/* $vm-exit (VM) */
/* apply-values (VM) */


/* ($$runvm code) => out */
/* ($$command-line bogus) => vector */
/* ($$lookup-cached-libinfo/encoded sym) => obj */
/* ($$lookup-cached-code sym) => VM-code */
/* ($$lookup-cached-macro sym) => VM-code */
/* ($$macro-runtime-mode bogus) => 0 */

/* (+ ...) => num */
/* (- ...) => num */
/* (* ...) => num */

static void
ExAdd(RnCtx* ctx, int argc, Value* stack){
    Value tmp;
    Value out;
    ValueContainer v;
    int64_t i64,ii64;
    double d64,dd64;
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    v.as_int64 = 0;
    RnValueRef(ctx, &out, v, VT_INT64);
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;
        if(out.type == VT_INT64){
            i64 = out.value.as_int64;
            if(tmp.type == VT_INT64){
                ii64 = tmp.value.as_int64;
                v.as_int64 = i64 + ii64;
                RnValueRef(ctx, &out, v, VT_INT64);
            }else if(tmp.type == VT_DOUBLE){
                d64 = i64;
                dd64 = tmp.value.as_double;
                v.as_double = d64 + dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else{
                abort();
            }
        }else if(out.type == VT_DOUBLE){
            d64 = out.value.as_double;
            if(tmp.type == VT_INT64){
                dd64 = tmp.value.as_int64;
                v.as_double = d64 + dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else if(tmp.type == VT_DOUBLE){
                dd64 = tmp.value.as_double;
                v.as_double = d64 + dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else{
                abort();
            }
        }else{
            abort();
        }
    }
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

static void
ExSub(RnCtx* ctx, int argc, Value* stack){
    Value tmp;
    Value out;
    ValueContainer v;
    int64_t i64,ii64;
    double d64,dd64;
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }else if(argc == 1){
        RnRibRef(ctx, &out, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        if(out.type == VT_INT64){
            v = out.value;
            v.as_int64 = (int64_t)0 - v.as_int64;
            RnValueRef(ctx, &out, v, VT_INT64);
        }else if(out.type == VT_DOUBLE){
            v = out.value;
            v.as_double = (double)0.0 - v.as_double;
            RnValueRef(ctx, &out, v, VT_DOUBLE);
        }else{
            abort();
        }
    }else{
        RnRibRef(ctx, &out, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;
        while(argc){
            RnRibRef(ctx, &tmp, stack, 0);
            RnRibRef(ctx, stack, stack, 1);
            argc--;
            if(out.type == VT_INT64){
                i64 = out.value.as_int64;
                if(tmp.type == VT_INT64){
                    ii64 = tmp.value.as_int64;
                    v.as_int64 = i64 - ii64;
                    RnValueRef(ctx, &out, v, VT_INT64);
                }else if(tmp.type == VT_DOUBLE){
                    d64 = i64;
                    dd64 = tmp.value.as_double;
                    v.as_double = d64 - dd64;
                    RnValueRef(ctx, &out, v, VT_DOUBLE);
                }else{
                    abort();
                }
            }else if(out.type == VT_DOUBLE){
                d64 = out.value.as_double;
                if(tmp.type == VT_INT64){
                    dd64 = tmp.value.as_int64;
                    v.as_double = d64 - dd64;
                    RnValueRef(ctx, &out, v, VT_DOUBLE);
                }else if(tmp.type == VT_DOUBLE){
                    dd64 = tmp.value.as_double;
                    v.as_double = d64 - dd64;
                    RnValueRef(ctx, &out, v, VT_DOUBLE);
                }else{
                    abort();
                }
            }else{
                abort();
            }
        }
    }
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

static void
ExMul(RnCtx* ctx, int argc, Value* stack){
    Value tmp;
    Value out;
    ValueContainer v;
    int64_t i64,ii64;
    double d64,dd64;
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    v.as_int64 = 1;
    RnValueRef(ctx, &out, v, VT_INT64);
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;
        if(out.type == VT_INT64){
            i64 = out.value.as_int64;
            if(tmp.type == VT_INT64){
                ii64 = tmp.value.as_int64;
                v.as_int64 = i64 * ii64;
                RnValueRef(ctx, &out, v, VT_INT64);
            }else if(tmp.type == VT_DOUBLE){
                d64 = i64;
                dd64 = tmp.value.as_double;
                v.as_double = d64 * dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else{
                abort();
            }
        }else if(out.type == VT_DOUBLE){
            d64 = out.value.as_double;
            if(tmp.type == VT_INT64){
                dd64 = tmp.value.as_int64;
                v.as_double = d64 * dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else if(tmp.type == VT_DOUBLE){
                dd64 = tmp.value.as_double;
                v.as_double = d64 * dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else{
                abort();
            }
        }else{
            abort();
        }
    }
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

/* (= ...) => <BOOL> */
/* (< ...) => <BOOL> */
/* (<= ...) => <BOOL> */
/* (> ...) => <BOOL> */
/* (>= ...) => <BOOL> */

static void
ExEq(RnCtx* ctx, int argc, Value* stack){
    int r;
    Value tmp;
    ValueContainer v;
    ValueType t;
    Value out;

    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }
    RnRibRef(ctx, &tmp, stack, 0);
    if(tmp.type == VT_INT64){
        v.as_int64 = tmp.value.as_int64;
        t = VT_INT64;
    }else if(tmp.type == VT_DOUBLE){
        v.as_double = tmp.value.as_double;
        t = VT_DOUBLE;
    }else{
        abort();
    }
    r = 1;
    RnRibRef(ctx, stack, stack, 1);
    argc--;
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;

        if(tmp.type == VT_INT64){
            if(t == VT_INT64){
                if(v.as_int64 != tmp.value.as_int64){
                    r = 0;
                    break;
                }
            }else if(t == VT_DOUBLE){
                if(v.as_double != tmp.value.as_int64){
                    r = 0;
                    break;
                }
            }else{
                abort();
            }
        }else if(tmp.type == VT_DOUBLE){
            if(t == VT_INT64){
                if(v.as_int64 != tmp.value.as_double){
                    r = 0;
                    break;
                }
            }else if(t == VT_DOUBLE){
                if(v.as_double != tmp.value.as_double){
                    r = 0;
                    break;
                }
            }else{
                abort();
            }
        }else{
            abort();
        }

    }
    to_bool(ctx, &out, r);
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

static void
ExLt(RnCtx* ctx, int argc, Value* stack){
    int r;
    Value tmp;
    ValueContainer v;
    ValueType t;
    Value out;

    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }
    RnRibRef(ctx, &tmp, stack, 0);
    if(tmp.type == VT_INT64){
        v.as_int64 = tmp.value.as_int64;
        t = VT_INT64;
    }else if(tmp.type == VT_DOUBLE){
        v.as_double = tmp.value.as_double;
        t = VT_DOUBLE;
    }else{
        abort();
    }
    r = 1;
    RnRibRef(ctx, stack, stack, 1);
    argc--;
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;

        if(tmp.type == VT_INT64){
            if(t == VT_INT64){
                if(v.as_int64 >= tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_int64 = tmp.value.as_int64;
            }else if(t == VT_DOUBLE){
                if(v.as_double >= tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_int64;
            }else{
                abort();
            }
        }else if(tmp.type == VT_DOUBLE){
            if(t == VT_INT64){
                if(v.as_int64 >= tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
                t = VT_DOUBLE;
            }else if(t == VT_DOUBLE){
                if(v.as_double >= tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
            }else{
                abort();
            }
        }else{
            abort();
        }

    }
    to_bool(ctx, &out, r);
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

static void
ExLtEq(RnCtx* ctx, int argc, Value* stack){
    int r;
    Value tmp;
    ValueContainer v;
    ValueType t;
    Value out;

    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }
    RnRibRef(ctx, &tmp, stack, 0);
    if(tmp.type == VT_INT64){
        v.as_int64 = tmp.value.as_int64;
        t = VT_INT64;
    }else if(tmp.type == VT_DOUBLE){
        v.as_double = tmp.value.as_double;
        t = VT_DOUBLE;
    }else{
        abort();
    }
    r = 1;
    RnRibRef(ctx, stack, stack, 1);
    argc--;
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;

        if(tmp.type == VT_INT64){
            if(t == VT_INT64){
                if(v.as_int64 > tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_int64 = tmp.value.as_int64;
            }else if(t == VT_DOUBLE){
                if(v.as_double > tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_int64;
            }else{
                abort();
            }
        }else if(tmp.type == VT_DOUBLE){
            if(t == VT_INT64){
                if(v.as_int64 > tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
                t = VT_DOUBLE;
            }else if(t == VT_DOUBLE){
                if(v.as_double > tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
            }else{
                abort();
            }
        }else{
            abort();
        }

    }
    to_bool(ctx, &out, r);
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

static void
ExGt(RnCtx* ctx, int argc, Value* stack){
    int r;
    Value tmp;
    ValueContainer v;
    ValueType t;
    Value out;

    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }
    RnRibRef(ctx, &tmp, stack, 0);
    if(tmp.type == VT_INT64){
        v.as_int64 = tmp.value.as_int64;
        t = VT_INT64;
    }else if(tmp.type == VT_DOUBLE){
        v.as_double = tmp.value.as_double;
        t = VT_DOUBLE;
    }else{
        abort();
    }
    r = 1;
    RnRibRef(ctx, stack, stack, 1);
    argc--;
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;

        if(tmp.type == VT_INT64){
            if(t == VT_INT64){
                if(v.as_int64 <= tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_int64 = tmp.value.as_int64;
            }else if(t == VT_DOUBLE){
                if(v.as_double <= tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_int64;
            }else{
                abort();
            }
        }else if(tmp.type == VT_DOUBLE){
            if(t == VT_INT64){
                if(v.as_int64 <= tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
                t = VT_DOUBLE;
            }else if(t == VT_DOUBLE){
                if(v.as_double <= tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
            }else{
                abort();
            }
        }else{
            abort();
        }

    }
    to_bool(ctx, &out, r);
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

static void
ExGtEq(RnCtx* ctx, int argc, Value* stack){
    int r;
    Value tmp;
    ValueContainer v;
    ValueType t;
    Value out;

    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }
    RnRibRef(ctx, &tmp, stack, 0);
    if(tmp.type == VT_INT64){
        v.as_int64 = tmp.value.as_int64;
        t = VT_INT64;
    }else if(tmp.type == VT_DOUBLE){
        v.as_double = tmp.value.as_double;
        t = VT_DOUBLE;
    }else{
        abort();
    }
    r = 1;
    RnRibRef(ctx, stack, stack, 1);
    argc--;
    while(argc){
        RnRibRef(ctx, &tmp, stack, 0);
        RnRibRef(ctx, stack, stack, 1);
        argc--;

        if(tmp.type == VT_INT64){
            if(t == VT_INT64){
                if(v.as_int64 < tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_int64 = tmp.value.as_int64;
            }else if(t == VT_DOUBLE){
                if(v.as_double < tmp.value.as_int64){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_int64;
            }else{
                abort();
            }
        }else if(tmp.type == VT_DOUBLE){
            if(t == VT_INT64){
                if(v.as_int64 < tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
                t = VT_DOUBLE;
            }else if(t == VT_DOUBLE){
                if(v.as_double < tmp.value.as_double){
                    r = 0;
                    break;
                }
                v.as_double = tmp.value.as_double;
            }else{
                abort();
            }
        }else{
            abort();
        }

    }
    to_bool(ctx, &out, r);
    RnCons(ctx, stack, &out, stack);
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
}

/* ($fx/ x y) => <FIXNUM> */
/* ($fl/ x y) => <FLONUM> */
/* ($fx->fl num) => num */
/* ($fx-expt x y) => num */
/* ($fx-floor/ x y) => num+num */
/* ($fx-truncate/ x y) => num+num */
/* ($fl-nan? num) => <BOOL> */
/* ($fl-finite? num) => <BOOL> */
/* ($fl-infinite? num) => <BOOL> */
/* ($fl->fx num) => num */
/* ($fl-expt x y) => num */
/* ($fl-floor x) => num */
/* ($fl-ceiling x) => num */
/* ($fl-truncate x) => num */
/* ($fl-round x) => num */
/* ($fl-acos x) => num */
/* ($fl-asin x) => num */
/* ($fl-atan x) => num */
/* ($fl-atan2 y x) => num */
/* ($fl-cos x) => num */
/* ($fl-sin x) => num */
/* ($fl-tan x) => num */
/* ($fl-exp x) => num */
/* ($fl-log ...) => num */
/* ($fl-sqrt x) => num */
/* ($fl-floor/ x y) => num+num */
/* ($fl-truncate/ x y) => num+num */


static void
ExFxDiv(RnCtx* ctx, Value* out, Value* x, Value* y){
    ValueContainer v;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    v.as_int64 = x->value.as_int64 / y->value.as_int64;
    RnValueRef(ctx, out, v, VT_INT64);
}

static void
ExFlDiv(RnCtx* ctx, Value* out, Value* x, Value* y){
    ValueContainer v;
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    v.as_double = x->value.as_double / y->value.as_double;
    RnValueRef(ctx, out, v, VT_DOUBLE);
}

static void
ExFxToFl(RnCtx* ctx, Value* out, Value* x){
    ValueContainer v;
    if(x->type != VT_INT64){
        abort();
    }
    v.as_double = x->value.as_int64;
    RnValueRef(ctx, out, v, VT_DOUBLE);
}

static void
ExFxExpt(RnCtx* ctx, Value* out, Value* x, Value* y){
    // FIXME: Implement this
    double a,b;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }

    a = x->value.as_int64;
    b = y->value.as_int64;
    RnDouble(ctx, out, pow(a,b));
}

static void
ExFxFloorDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    int64_t r1;
    int64_t r2;
    // FIXME: Implement this
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    r1 = x->value.as_int64 / y->value.as_int64;
    r2 = x->value.as_int64 % y->value.as_int64;
    RnInt64(ctx, out1, r1);
    RnInt64(ctx, out2, r2);
}

static void
ExFxTruncateDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    int64_t r1;
    int64_t r2;
    // FIXME: Implement this
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    r1 = x->value.as_int64 / y->value.as_int64;
    r2 = x->value.as_int64 % y->value.as_int64;
    RnInt64(ctx, out1, r1);
    RnInt64(ctx, out2, r2);

}

static void
ExFlNanP(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    to_bool(ctx, out, isnan(x->value.as_double));
}

static void
ExFlFiniteP(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    to_bool(ctx, out, isfinite(x->value.as_double));
}

static void
ExFlInfiniteP(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    to_bool(ctx, out, ! isfinite(x->value.as_double));
}

static void
ExFlToFx(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnInt64(ctx, out, x->value.as_double);
}

static void
ExFlExpt(RnCtx* ctx, Value* out, Value* x, Value* y){
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, pow(x->value.as_double, y->value.as_double));
}

static void
ExFlFloor(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, floor(x->value.as_double));
}

static void
ExFlCeiling(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, ceil(x->value.as_double));
}

static void
ExFlTruncate(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, trunc(x->value.as_double));
}

static void
ExFlRound(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, round(x->value.as_double));
}

static void
ExFlAcos(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, acos(x->value.as_double));
}

static void
ExFlAsin(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, asin(x->value.as_double));
}

static void
ExFlAtan(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, atan(x->value.as_double));
}

static void
ExFlAtan2(RnCtx* ctx, Value* out, Value* x, Value* y){
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, atan2(x->value.as_double, y->value.as_double));
}

static void
ExFlCos(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, cos(x->value.as_double));
}

static void
ExFlSin(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, sin(x->value.as_double));
}

static void
ExFlTan(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, tan(x->value.as_double));
}

static void
ExFlExp(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, exp(x->value.as_double));
}


static void
ExFlLog_2_1(RnCtx* ctx, Value* out, Value* x, Value* y){
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, log(x->value.as_double) / log(y->value.as_double));
}

static void
ExFlLog_1_1(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, log(x->value.as_double));
}

static void
ExFlLog(RnCtx* ctx, int argc, Value* stack){
    switch(argc){
        case 1:
            ExCall_1_1(ExFlLog_1_1, ctx, argc, stack);
            break;
        case 2:
            ExCall_2_1(ExFlLog_2_1, ctx, argc, stack);
            break;
        default:
            abort();
    }
}

static void
ExFlSqrt(RnCtx* ctx, Value* out, Value* x){
    if(x->type != VT_DOUBLE){
        abort();
    }
    RnDouble(ctx, out, sqrt(x->value.as_double));
}

static void
ExFlFloorDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    double r1, r2;
    // FIXME: Implement this
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    r1 = floor(x->value.as_double / y->value.as_double);
    r2 = x->value.as_double - (r1 * y->value.as_double);
    RnDouble(ctx, out1, r1);
    RnDouble(ctx, out2, r2);
}

static void
ExFlTruncateDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    double r1, r2;
    // FIXME: Implement this
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }

    r1 = trunc(x->value.as_double / y->value.as_double);
    r2 = x->value.as_double - (r1 * y->value.as_double);
    RnDouble(ctx, out1, r1);
    RnDouble(ctx, out2, r2);

}

#define EXLIB_MATH(x) \
    x("+", ExAdd, _N) \
    x("-", ExSub, _N) \
    x("*", ExMul, _N) \
    x("=", ExEq, _N) \
    x("<", ExLt, _N) \
    x("<=", ExLtEq, _N) \
    x(">", ExGt, _N) \
    x(">=", ExGtEq, _N) \
    x("$fx/", ExFxDiv, _2_1) \
    x("$fl/", ExFlDiv, _2_1) \
    x("$fx->fl", ExFxToFl, _1_1) \
    x("$fx-expt", ExFxExpt, _2_1) \
    x("$fx-floor/", ExFxFloorDiv, _2_2) \
    x("$fx-truncate/", ExFxTruncateDiv, _2_2) \
    x("$fl-nan?", ExFlNanP, _1_1) \
    x("$fl-finite?", ExFlFiniteP, _1_1) \
    x("$fl-infinite?", ExFlInfiniteP, _1_1) \
    x("$fl->fx", ExFlToFx, _1_1) \
    x("$fl-expt", ExFlExpt, _2_1) \
    x("$fl-floor", ExFlFloor, _1_1) \
    x("$fl-ceiling", ExFlCeiling, _1_1) \
    x("$fl-truncate", ExFlTruncate, _1_1) \
    x("$fl-round", ExFlRound, _1_1) \
    x("$fl-acos", ExFlAcos, _1_1) \
    x("$fl-asin", ExFlAsin, _1_1) \
    x("$fl-atan", ExFlAtan, _1_1) \
    x("$fl-atan2", ExFlAtan2, _2_1) \
    x("$fl-cos", ExFlCos, _1_1) \
    x("$fl-sin", ExFlSin, _1_1) \
    x("$fl-tan", ExFlTan, _1_1) \
    x("$fl-exp", ExFlExp, _1_1) \
    x("$fl-log", ExFlLog, _N) \
    x("$fl-sqrt", ExFlSqrt, _1_1) \
    x("$fl-floor/", ExFlFloorDiv, _2_2) \
    x("$fl-truncate/", ExFlTruncateDiv, _2_2)


/* (file-exists? str) => <BOOL> */
/* (delete-file str) => bogus */
/* (filehandle-open/input file) => fh */
/* (filehandle-open/output file) => fh */
/* (filehandle-close fh) => bogus */
/* (filehandle-read! fh bv offs len) => len */
/* (filehandle-write fh bv offs len) => len */
/* (filehandle-flush fh) => bogus */
/* (filehandle-stdin) => fh */
/* (filehandle-stdout) => fh */
/* (filehandle-stderr) => fh */

/* (vec-copy v start end) => vec */
/* (vec-copy! tgt loc src start end) => bogus */
/* (vec-ref vec idx) => obj */
/* (vec-set! vec idx obj) => bogus */
/* (vec-new tag k) => vec */
/* (vec-length vec) => num */
/* (vec-fill! vec obj from to) => bogus */
/* (vec= v1 v2) => <BOOL> */
/* (vec-append ...) => vec */
/* (vec-subvec vec start end) => vec */
/* (ht-new x) => ht */
/* (ht-set! ht key obj) => bogus */
/* (ht-entries ht) => key+value */
/* (ht-ref ht key default) => obj */
/* (ht-keys ht) => key */
/* (ht-size ht) => num */

#define EXFUN(nam, fn) {nam, sizeof(nam), fn}

EXLIB_MISC(GEN_BRIDGE)
EXLIB_MATH(GEN_BRIDGE)

RnVmEx vm_externals[] = {
    EXLIB_MISC(GEN_FILD)
    EXLIB_MATH(GEN_FILD)
    EXFUN("$error/core", ExErrorCore),
    {0, 0, 0}
};
