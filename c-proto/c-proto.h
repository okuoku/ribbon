
struct ObjHeader_s {
    struct ObjHeader_s* gc_next;
    struct ObjHeader_s* gc_prev;
    uintptr_t refcnt;
    uintptr_t gc_refinfo; /* refcnt * 8 + GC_TYPE + walked? */
};

static const uintptr_t GC_TYPE_RIB = 1 * 2;
static const uintptr_t GC_TYPE_VECTOR = 2 * 2;
static const uintptr_t GC_TYPE_HASHTABLE = 3 * 2;

typedef struct ObjHeader_s ObjHeader;

enum ValueType_e {
    VT_EMPTY,
    /* integers */
    VT_ZONE0,
    VT_INT64,
    VT_DOUBLE,
    VT_CHAR,
    /* Container pointers */
    VT_RIB,
    VT_VECTOR, /* NB: Unwrapped (unlike on-Scheme) */
    VT_SIMPLE_STRUCT, /* NB: Unwrapped */
    VT_HASHTABLE, /* NB: Unwrapped */
    /* Heap pointers */
    VT_STRING,
    VT_BYTEVECTOR,
    /* root */
    VT_ROOT
};

typedef enum ValueType_e ValueType;

enum ValueZone0_e {
    ZZ_NIL,
    ZZ_TRUE,
    ZZ_FALSE,
    ZZ_EOF_OBJECT
};

typedef enum ValueZone0_e ValueZone0;

struct ObjRib_s;
struct ObjVector_s;
struct ObjString_s;
struct ObjBytevector_s;
struct ObjHashtable_s;
union ValueContainer_u {
    int64_t as_int64;
    double as_double;
    int as_char;
    ValueZone0 as_zone0;
    struct ObjRib_s* as_rib;
    struct ObjVector_s* as_vector; /* And, SimpleStruct */
    struct ObjString_s* as_string;
    struct ObjBytevector_s* as_bytevector;
    struct ObjHashtable_s* as_hashtable;
};

typedef union ValueContainer_u ValueContainer;

struct ObjRib_s {
    ObjHeader header;
    ValueContainer field[3];
    ValueType type[3];
};

typedef struct ObjRib_s ObjRib;

struct ObjVector_s {
    ObjHeader header;
    uintptr_t length;
    ValueContainer* values;
    ValueType* types;
};

typedef struct ObjVector_s ObjVector;

struct ObjBytevector_s {
    uintptr_t refcnt;
    uintptr_t typeinfo; /* FIXME: Tentative */
    uint8_t* buf;
    size_t len;
};

typedef struct ObjBytevector_s ObjBytevector;

static inline int
RnBytevectorIsExternal(ObjBytevector* bv){
    return bv->typeinfo & 1;
}

static inline int
RnBytevectorIsUnlimitedRange(ObjBytevector* bv){
    return bv->typeinfo & 2;
}

struct ObjString_s {
    uintptr_t refcnt;
    const char* str;
    size_t len;
};

typedef struct ObjString_s ObjString;

enum HashtableClass_e {
    HTC_EQ_HASHTABLE = 0,
    HTC_EQV_HASHTABLE = 1,
    HTC_INTEGER_HASHTABLE = 2,
    HTC_STRING_HASHTABLE = 3,
    HTC_SYMBOL_HASHTABLE = 4
};

typedef enum HashtableClass_e HashtableClass;

enum HashtableType_e {
    HT_BLOBKEY, /* STRING */
    HT_INTKEY, /* INTEGER */
    HT_EQV /* EQ, EQV and SYMBOL */
};


struct ObjHashtable_s {
    ObjHeader header;
    uintptr_t keycount; /* Precise count of valid keys */
    uintptr_t containercount; /* Upper bound of keys/values */
    uintptr_t tablesize;
    ValueContainer* table; /* Rib or Null */
    ValueContainer* keys;
    ValueContainer* values;
    enum ValueType_e* valuetypes;
    enum ValueType_e* keytypes; /* Null on int/string/symbol */
    enum HashtableType_e hashtable_type;
    HashtableClass hashtable_class;
};

typedef struct ObjHashtable_s ObjHashtable;

struct Value_s {
    /* Allocation chain */
    struct Value_s* prev;
    struct Value_s* next;
    /* Stack frame chain */
    struct Value_s* root;
    ValueContainer value;
    ValueType type;
};

typedef struct Value_s Value;

struct RnCtx_s { /* Ribbon Context */
    Value ctx_root;
    Value raise_proc;
    Value* current_frame;
    ObjHeader gcroot;

    /* Globals (referred in ctx_root) */
    Value ht_global; /* String hashtable */
    Value ht_libinfo; /* libsym => info */
    Value ht_libcode; /* libsym => code */
    Value ht_macro; /* sym => code */
    Value bootstrap; /* Scheme object */
    Value args; /* command-line arguments (vector) */
};


typedef struct RnCtx_s RnCtx;

enum RnResult_e {
    RN_SUCCESS = 0,
    RN_EXCEPTION
};

typedef enum RnResult_e RnResult;

typedef RnResult (*RnVmExFunc)(RnCtx* ctx, int argc, Value* stack);

#ifdef _MSC_VER
#define RNFUNC_LABEL_ATTRIBUTE
#else
#define RNFUNC_LABEL_ATTRIBUTE __attribute__((unused))
#endif

#define RNFUNC_BEGIN RnResult rnresult = RN_SUCCESS;
#define RNFUNC_END rnresult_return: RNFUNC_LABEL_ATTRIBUTE; return rnresult;
#define RNFUNC_CALL(ctx,x) do { \
    rnresult = x; \
    if(rnresult != RN_SUCCESS){ \
        abort(); \
    } \
} while(0)

struct RnVmEx_s {
    const char* symname;
    size_t namelen;
    RnVmExFunc func;
};

typedef struct RnVmEx_s RnVmEx;

/* Interface APIs */
void RnCtxInit(RnCtx* newctx);
void RnCtxRunBootstrap(RnCtx* ctx, const uint8_t* bootstrap);
void RnValueLink(RnCtx* ctx, Value* target);
void RnValueUnlink(RnCtx* ctx, Value* target);
RnResult RnVector(RnCtx* ctx, Value* out, size_t len);
RnResult RnVectorSet(RnCtx* ctx, Value* target, Value* obj, size_t idx);
RnResult RnString(RnCtx* ctx, Value* out, const char* name, size_t len);

