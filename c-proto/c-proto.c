#define GCCYCLE 10000000
//#define TRACE
#define DEBUG_FILLFREED

/* Ribbon Scheme proto */
#include <stdint.h>
#include <stdlib.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <string.h>
#include <math.h>

#include "c-proto.h"

/* Panic */

#if defined(_MSC_VER)
#define NORETURN __declspec(noreturn) 
#else
#define NORETURN __attribute__ ((noreturn))
#endif

NORETURN static void
RnPanic(void){
    abort();
}

NORETURN static void
RnLowMemory(void){
    abort();
}

#include <stdarg.h>
#include <stdio.h> /* FIXME: We still have sprintf in number_to_string */
#define debugprintf BsDebugPrintf

/* Standard Bootstrap handler implementation */
void* BsFileOpenForRead(const char* path);
void* BsFileOpenForReadWrite(const char* path);
void* BsFileReadAll(const char* path); /* => malloc'ed region */
void BsFileClose(void* handle);
int BsFileRead(void* handle, void* buf, size_t buflen, size_t* outlen);
int BsFileWrite(void* handle, void* buf, size_t buflen, size_t* outlen);
void BsFileFlush(void* handle);
void* BsFileGetStdin(void);
void* BsFileGetStdout(void);
void* BsFileGetStderr(void);
#ifdef __GNUC__
#define PRINTF __attribute__((format(printf,1,2)))
#else
#define PRINTF
#endif
void BsDebugPrintf(const char* fmt, ...) PRINTF;

#ifndef RN_EMBEDDING
void*
BsFileOpenForRead(const char* path){
    return (void*)fopen(path, "rb");
}

void*
BsFileOpenForReadWrite(const char* path){
    return (void*)fopen(path, "w+b");
}

void*
BsFileReadAll(const char* path){
    FILE* fp;
    size_t binsize, readsize;
    void* out;

    fp = fopen(path, "rb");

    /* FIXME: This isn't portable */
    fseek(fp, 0, SEEK_END);
    binsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    out = malloc(binsize);
    if(! out){
        RnLowMemory();
    }

    readsize = fread(out, 1, binsize, fp);
    if(readsize != binsize){
        RnPanic();
    }
    fclose(fp);
    return out;
}

void
BsFileClose(void* handle){
    FILE* fp = (FILE*)handle;
    fclose(fp);
}

int
BsFileRead(void* handle, void* buf, size_t buflen, size_t* outlen){
    FILE* fp = (FILE*)handle;
    size_t readlen;

    readlen = fread(buf, 1, buflen, fp);
    *outlen = readlen;
    return 0;
}

int
BsFileWrite(void* handle, void* buf, size_t buflen, size_t* outlen){
    FILE* fp = (FILE*)handle;
    size_t writelen;
    writelen = fwrite(buf, 1, buflen, fp);
    *outlen = writelen;
    return 0;
}

void
BsFileFlush(void* handle){
    FILE* fp = (FILE*)handle;
    fflush(fp);
}

void*
BsFileGetStdin(void){
    return (void*)stdin;
}

void*
BsFileGetStdout(void){
    return (void*)stdout;
}

void*
BsFileGetStderr(void){
    return (void*)stderr;
}

void
BsDebugPrintf(const char* fmt, ...){
    va_list ap;
    va_start(ap, fmt);
    (void)vfprintf(stderr, fmt, ap);
    va_end(ap);
}
#endif


/* GC */

static void RnDestroyRib(RnCtx* ctx, ObjRib* rib);
static void RnDestroyVector(RnCtx* ctx, ObjVector* vector);
static void RnDestroyHashtable(RnCtx* ctx, ObjHashtable* hashtable);

#define REF_INDELETE UINTPTR_MAX

static int gccounter = 0;

const uintptr_t ON_GARBAGE_LIST_MIN = UINTPTR_MAX - 7;
const uintptr_t ITS_REACHABLE = 8;

static ValueType
gettype(ObjHeader* header){
    uintptr_t x;
    x = header->gc_refinfo;
    x &= 6;
    if(x == GC_TYPE_RIB){
        return VT_RIB;
    }else if(x == GC_TYPE_VECTOR){
        return VT_VECTOR;
    }else if(x == GC_TYPE_HASHTABLE){
        return VT_HASHTABLE;
    }else{
        RnPanic();
    }
}

static void
gcref_value(ValueContainer vc, ValueType t){
    switch(t){
        case VT_EMPTY:
        case VT_ZONE0:
        case VT_INT64:
        case VT_DOUBLE:
        case VT_CHAR:
        case VT_STRING:
        case VT_BYTEVECTOR:
            break;
        case VT_RIB:
            vc.as_rib->header.gc_refinfo -= 8;
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            vc.as_vector->header.gc_refinfo -= 8;
            break;
        case VT_HASHTABLE:
            vc.as_hashtable->header.gc_refinfo -= 8;
            break;
        default:
            RnPanic();
            break;
    }
}

static void
gcwalk_gcref(ObjHeader* h){
    size_t i;
    ObjRib* rib;
    ObjVector* vector;
    ObjHashtable* hashtable;
    ValueType t;
    t = gettype(h);

    switch(t){
        case VT_RIB:
            rib = (ObjRib*)h;
            for(i=0;i!=3;i++){
                gcref_value(rib->field[i], rib->type[i]);
            }
            break;
        case VT_VECTOR:
            vector = (ObjVector*)h;
            for(i=0;i!=vector->length;i++){
                gcref_value(vector->values[i], vector->types[i]);
            }
            break;
        case VT_HASHTABLE:
            hashtable = (ObjHashtable*)h;
            /* NB: We can safely ignore table since it won't make
             * any loop */
            for(i=0;i!=hashtable->containercount;i++){
                gcref_value(hashtable->values[i], hashtable->valuetypes[i]);
            }
            if(hashtable->keytypes){
                for(i=0;i!=hashtable->containercount;i++){
                    gcref_value(hashtable->keys[i], hashtable->keytypes[i]);
                }
            }
            break;
        default:
            RnPanic();
            break;
    }
}

static void
rescue_value(ObjHeader* last, ValueContainer vc, ValueType t){
    ObjHeader* h;
    uintptr_t typetag;
    switch(t){
        case VT_EMPTY:
        case VT_ZONE0:
        case VT_INT64:
        case VT_DOUBLE:
        case VT_CHAR:
        case VT_STRING:
        case VT_BYTEVECTOR:
            return;
        case VT_RIB:
            h = &vc.as_rib->header;
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            h = &vc.as_vector->header;
            break;
        case VT_HASHTABLE:
            h = &vc.as_hashtable->header;
            break;
        default:
            RnPanic();
            break;
    }

    typetag = h->gc_refinfo & 6;

    if(h->gc_refinfo >= ON_GARBAGE_LIST_MIN){
        if(h->gc_prev){
            h->gc_prev->gc_next = h->gc_next;
        }
        if(h->gc_next){
            h->gc_next->gc_prev = h->gc_prev;
        }
        if(last->gc_prev){
            last->gc_prev->gc_next = h;
        }
        h->gc_prev = last->gc_prev;
        h->gc_next = last;
        last->gc_prev = h;
    }

    h->gc_refinfo = typetag + ITS_REACHABLE;
}

static void
gcwalk_rescue(ObjHeader* last, ObjHeader* h){
    size_t i;
    ObjRib* rib;
    ObjVector* vector;
    ObjHashtable* hashtable;
    ValueType t;
    t = gettype(h);

    switch(t){
        case VT_RIB:
            rib = (ObjRib*)h;
            for(i=0;i!=3;i++){
                rescue_value(last, rib->field[i], rib->type[i]);
            }
            break;
        case VT_VECTOR:
            vector = (ObjVector*)h;
            for(i=0;i!=vector->length;i++){
                rescue_value(last, vector->values[i], vector->types[i]);
            }
            break;
        case VT_HASHTABLE:
            hashtable = (ObjHashtable*)h;
            for(i=0;i!=hashtable->containercount;i++){
                rescue_value(last, hashtable->values[i], 
                             hashtable->valuetypes[i]);
            }
            if(hashtable->keytypes){
                for(i=0;i!=hashtable->containercount;i++){
                    rescue_value(last, hashtable->keys[i], 
                                 hashtable->keytypes[i]);
                }
            }
            break;
        default:
            RnPanic();
            break;
    }
}

static void
RnGc(RnCtx* ctx){
    ObjHeader* cur;
    ObjHeader* curnext;
    ObjHeader lastlive;
    ObjHeader garbages;
    ValueType t;
    uintptr_t gcrefcnt;
    uintptr_t typetag;
    garbages.gc_prev = garbages.gc_next = 0;
    lastlive.gc_prev = lastlive.gc_next = 0;

    /* First, Initialize gc_refinfo field + setup lastlive */
    cur = ctx->gcroot.gc_next;
    while(cur){
        cur->gc_refinfo &= 6;
        if(cur->refcnt == 0){
            RnPanic();
        }
        if(cur->refcnt == REF_INDELETE){
            RnPanic();
        }
        cur->gc_refinfo += cur->refcnt * 8;
        lastlive.gc_prev = cur;
        cur = cur->gc_next;
    }

    /* Link lastlive */
    if(lastlive.gc_prev){
        if(lastlive.gc_prev->gc_next != 0){
            RnPanic();
        }
        lastlive.gc_prev->gc_next = &lastlive;
    }

    /* Adjust gcref */
    cur = ctx->gcroot.gc_next;
    while(cur && cur != &lastlive){
        gcwalk_gcref(cur);
        cur = cur->gc_next;
    }

    /* Detect unreachable-candidates */
    cur = ctx->gcroot.gc_next;
    while(cur && cur != &lastlive){
        gcrefcnt = cur->gc_refinfo / 8;
        typetag = cur->gc_refinfo & 6;
        curnext = cur->gc_next;
        if(cur->gc_refinfo >= ON_GARBAGE_LIST_MIN){
            RnPanic();
        }
        if(gcrefcnt != 0){
            /* It's reachable object, walk and move its contents
             * to live list */
            gcwalk_rescue(&lastlive, cur);
            curnext = cur->gc_next;
        }else{
            /* Maybe a garbage, move to garbage list for now */
            if(cur->gc_prev){
                cur->gc_prev->gc_next = cur->gc_next;
            }
            if(cur->gc_next){
                cur->gc_next->gc_prev = cur->gc_prev;
            }
            if(garbages.gc_next){
                garbages.gc_next->gc_prev = cur;
            }
            cur->gc_next = garbages.gc_next;
            cur->gc_prev = &garbages;
            garbages.gc_next = cur;
            cur->gc_refinfo = ON_GARBAGE_LIST_MIN + typetag;
        }
        cur = curnext;
    }

    /* Unlink lastlive */
    if(lastlive.gc_prev){
        if(lastlive.gc_prev->gc_next != &lastlive){
            RnPanic();
        }
        lastlive.gc_prev->gc_next = 0;
    }

    /* Do GC */
    cur = garbages.gc_next;
    while(cur){
        curnext = cur->gc_next;
        t = gettype(cur);
        switch(t){
            case VT_RIB:
                RnDestroyRib(ctx, (ObjRib*)cur);
                break;
            case VT_VECTOR:
                RnDestroyVector(ctx, (ObjVector*)cur);
                break;
            case VT_HASHTABLE:
                RnDestroyHashtable(ctx, (ObjHashtable*)cur);
                break;
            default:
                RnPanic();
                break;
        }
        cur = curnext;
    }
}

static void 
RnGcTick(RnCtx* ctx){
    gccounter++;
    if(gccounter > GCCYCLE){
        gccounter = 0;
        RnGc(ctx);
    }
}

/* Heap */


static void RnUnref(RnCtx* ctx, ValueContainer* obj, ValueType* type);
static void RnUnrefInDestroy(RnCtx* ctx, ValueContainer obj, ValueType type);

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
    RnUnref(ctx, &target->value, &target->type);
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
        RnPanic();
    }
    cur = current_frame->root;
    while(cur){
        RnValueUnlink(ctx, cur);
        cur = current_frame->root;
    }

    ctx->current_frame = current_frame->prev;
}

static void RnObjHeaderUnlink(RnCtx* ctx, ObjHeader* header);


static void
RnRefRib(RnCtx* ctx, ObjRib* rib){
    (void) ctx;
    if(rib->header.refcnt == REF_INDELETE){
        RnPanic();
    }
    rib->header.refcnt++;
}

static void
RnDestroyRib(RnCtx* ctx, ObjRib* rib){
    int i;
    for(i=0;i!=3;i++){
        RnUnrefInDestroy(ctx, rib->field[i], rib->type[i]);
    }
#ifdef DEBUG_FILLFREED
    memset(rib, 0xcc, sizeof(ObjRib));
#endif
    free(rib);
}

static void
RnUnrefRib(RnCtx* ctx, ObjRib* rib){
    int i;
    if(rib->header.refcnt == REF_INDELETE){
        /* Do nothing, in loop */
        return;
    }
    if(rib->header.refcnt == 0){
        RnPanic();
    }
    rib->header.refcnt--;
    if(rib->header.refcnt == 0){
        rib->header.refcnt = REF_INDELETE;
        RnObjHeaderUnlink(ctx, &rib->header);
        for(i=0;i!=3;i++){
            RnUnref(ctx, &rib->field[i], &rib->type[i]);
        }
#ifdef DEBUG_FILLFREED
        memset(rib, 0xcc, sizeof(ObjRib));
#endif
        free(rib);
    }
}

static void
RnRefVector(RnCtx* ctx, ObjVector* vector){
    (void) ctx;
    if(vector->header.refcnt == REF_INDELETE){
        RnPanic();
    }
    vector->header.refcnt++;
}

static void
RnDestroyVector(RnCtx* ctx, ObjVector* vector){
    size_t i;
    for(i=0;i!=vector->length;i++){
        RnUnrefInDestroy(ctx, vector->values[i], vector->types[i]);
    }
#ifdef DEBUG_FILLFREED
    memset(vector->values, 0xcc, sizeof(ValueContainer) * vector->length);
    memset(vector->types, 0xcc, sizeof(ValueType) * vector->length);
#endif
    free(vector->values);
    free(vector->types);
#ifdef DEBUG_FILLFREED
    memset(vector, 0xcc, sizeof(ObjVector));
#endif
    free(vector);
}

static void
RnUnrefVector(RnCtx* ctx, ObjVector* vector){
    size_t i;
    if(vector->header.refcnt == REF_INDELETE){
        /* Do nothing, in loop */
        return;
    }
    if(vector->header.refcnt == 0){
        RnPanic();
    }
    vector->header.refcnt--;
    if(vector->header.refcnt == 0){
        vector->header.refcnt = REF_INDELETE;
        RnObjHeaderUnlink(ctx, &vector->header);
        for(i=0;i!=vector->length;i++){
            RnUnref(ctx, &vector->values[i], &vector->types[i]);
        }
#ifdef DEBUG_FILLFREED
        memset(vector->values, 0xcc, sizeof(ValueContainer) * vector->length);
        memset(vector->types, 0xcc, sizeof(ValueType) * vector->length);
#endif
        free(vector->values);
        free(vector->types);
#ifdef DEBUG_FILLFREED
        memset(vector, 0xcc, sizeof(ObjVector));
#endif
        free(vector);
    }
}

static void
RnRefString(RnCtx* ctx, ObjString* string){
    (void) ctx;
    if(string->refcnt == REF_INDELETE){
        RnPanic();
    }
    string->refcnt++;
}

static void
RnUnrefString(RnCtx* ctx, ObjString* string){
    (void) ctx;
    if(string->refcnt == 0){
        RnPanic();
    }
    string->refcnt--;
    if(string->refcnt == 0){
#ifdef DEBUG_FILLFREED
        memset((void*)string->str, 0xcc, string->len);
#endif
        free((void*)string->str);
#ifdef DEBUG_FILLFREED
        memset(string, 0xcc, sizeof(ObjString));
#endif
        free(string);
    }
}

static void
RnRefBytevector(RnCtx* ctx, ObjBytevector* bytevector){
    (void) ctx;
    if(bytevector->refcnt == REF_INDELETE){
        RnPanic();
    }
    bytevector->refcnt++;
}

static void
RnUnrefBytevector(RnCtx* ctx, ObjBytevector* bytevector){
    (void) ctx;
    if(bytevector->refcnt == 0){
        RnPanic();
    }
    bytevector->refcnt--;
    if(bytevector->refcnt == 0){
        if(! RnBytevectorIsExternal(bytevector)){
#ifdef DEBUG_FILLFREED
            memset(bytevector->buf, 0xcc, bytevector->len);
#endif
            free(bytevector->buf);
        }
#ifdef DEBUG_FILLFREED
        memset(bytevector, 0xcc, sizeof(ObjBytevector));
#endif
        free(bytevector);
    }
}

static void
RnRefHashtable(RnCtx* ctx, ObjHashtable* hashtable){
    (void) ctx;
    if(hashtable->header.refcnt == REF_INDELETE){
        RnPanic();
    }
    hashtable->header.refcnt++;
}

static void
RnDestroyHashtable(RnCtx* ctx, ObjHashtable* hashtable){
    size_t i;
    ValueType vt;
    for(i=0;i!=hashtable->tablesize;i++){
        if(hashtable->table[i].as_rib){
            vt = VT_RIB;
            RnUnref(ctx, &hashtable->table[i], &vt);
        }
    }
#ifdef DEBUG_FILLFREED
    memset(hashtable->table, 0xcc, sizeof(ValueContainer) * 
           hashtable->tablesize);
#endif
    free(hashtable->table);

    for(i=0;i!=hashtable->containercount;i++){
        RnUnrefInDestroy(ctx, hashtable->values[i], hashtable->valuetypes[i]);
        switch(hashtable->hashtable_type){
            case HT_BLOBKEY:
                vt = VT_STRING;
                RnUnrefInDestroy(ctx, hashtable->keys[i], VT_STRING);
                break;
            case HT_INTKEY:
                vt = VT_INT64;
                RnUnrefInDestroy(ctx, hashtable->keys[i], VT_INT64);
                break;
            case HT_EQV:
                RnUnrefInDestroy(ctx, hashtable->keys[i], 
                                 hashtable->keytypes[i]);
                break;
            default:
                RnPanic();
                break;
        }
    }

#ifdef DEBUG_FILLFREED
    memset(hashtable->values, 0xcc, sizeof(ValueContainer) *
           hashtable->containercount);
    memset(hashtable->valuetypes, 0xcc, sizeof(ValueType) *
           hashtable->containercount);
    memset(hashtable->keys, 0xcc, sizeof(ValueContainer) *
           hashtable->containercount);
    if(hashtable->keytypes){
        memset(hashtable->keytypes, 0xcc, sizeof(ValueType) *
               hashtable->containercount);
    }
#endif
    free(hashtable->values);
    free(hashtable->valuetypes);
    free(hashtable->keys);
    if(hashtable->keytypes){
        free(hashtable->keytypes);
    }
#ifdef DEBUG_FILLFREED
    memset(hashtable, 0xcc, sizeof(ObjHashtable));
#endif
    free(hashtable);
}

static void
RnUnrefHashtable(RnCtx* ctx, ObjHashtable* hashtable){
    size_t i;
    (void) ctx;
    ValueType vt;
    if(hashtable->header.refcnt == REF_INDELETE){
        /* Do nothing, in loop */
        return;
    }
    if(hashtable->header.refcnt == 0){
        RnPanic();
    }
    hashtable->header.refcnt--;
    if(hashtable->header.refcnt == 0){
        hashtable->header.refcnt = REF_INDELETE;
        RnObjHeaderUnlink(ctx, &hashtable->header);
        for(i=0;i!=hashtable->tablesize;i++){
            if(hashtable->table[i].as_rib){
                vt = VT_RIB;
                RnUnref(ctx, &hashtable->table[i], &vt);
            }
        }
#ifdef DEBUG_FILLFREED
        memset(hashtable->table, 0xcc, sizeof(ValueContainer) * 
               hashtable->tablesize);
#endif
        free(hashtable->table);

        for(i=0;i!=hashtable->containercount;i++){
            RnUnref(ctx, &hashtable->values[i], &hashtable->valuetypes[i]);
            switch(hashtable->hashtable_type){
                case HT_BLOBKEY:
                    vt = VT_STRING;
                    RnUnref(ctx, &hashtable->keys[i], &vt);
                    break;
                case HT_INTKEY:
                    vt = VT_INT64;
                    RnUnref(ctx, &hashtable->keys[i], &vt);
                    break;
                case HT_EQV:
                    RnUnref(ctx, &hashtable->keys[i], 
                            &hashtable->keytypes[i]);
                    break;
                default:
                    RnPanic();
                    break;
            }
        }

#ifdef DEBUG_FILLFREED
        memset(hashtable->values, 0xcc, sizeof(ValueContainer) *
               hashtable->containercount);
        memset(hashtable->valuetypes, 0xcc, sizeof(ValueType) *
               hashtable->containercount);
        memset(hashtable->keys, 0xcc, sizeof(ValueContainer) *
               hashtable->containercount);
        if(hashtable->keytypes){
            memset(hashtable->keytypes, 0xcc, sizeof(ValueType) *
                   hashtable->containercount);
        }
#endif
        free(hashtable->values);
        free(hashtable->valuetypes);
        free(hashtable->keys);
        if(hashtable->keytypes){
            free(hashtable->keytypes);
        }
#ifdef DEBUG_FILLFREED
        memset(hashtable, 0xcc, sizeof(ObjHashtable));
#endif
        free(hashtable);
    }
}

static void
RnRef(RnCtx* ctx, ValueContainer obj, ValueType type){
    switch(type){
        case VT_EMPTY:
            //abort();
            break;
        case VT_ZONE0:
        case VT_INT64:
        case VT_DOUBLE:
        case VT_CHAR:
            /* Do nothing */
            break;
        case VT_RIB:
            RnRefRib(ctx, obj.as_rib);
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            RnRefVector(ctx, obj.as_vector);
            break;
        case VT_HASHTABLE:
            RnRefHashtable(ctx, obj.as_hashtable);
            break;
        case VT_STRING:
            RnRefString(ctx, obj.as_string);
            break;
        case VT_BYTEVECTOR:
            RnRefBytevector(ctx, obj.as_bytevector);
            break;
        case VT_ROOT:
            /* Do nothing, ctx always refs it */
            break;
        default:
            RnPanic();
            break;
    }
}

static void
RnUnrefInDestroy(RnCtx* ctx, ValueContainer obj, ValueType type){
    switch(type){
        case VT_EMPTY:
        case VT_ZONE0:
        case VT_INT64:
        case VT_DOUBLE:
        case VT_CHAR:
            /* Do nothing */
            break;
        case VT_RIB:
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
        case VT_HASHTABLE:
            /* Container objects are already doomed to be freed */
            break;
        case VT_STRING:
            RnUnrefString(ctx, obj.as_string);
            break;
        case VT_BYTEVECTOR:
            RnUnrefBytevector(ctx, obj.as_bytevector);
            break;
        case VT_ROOT:
            /* Do nothing? Should be freed with ctx */
            break;
        default:
            RnPanic();
            break;
    }
}

static void
RnUnref(RnCtx* ctx, ValueContainer* obj, ValueType* type){
    switch(*type){
        case VT_EMPTY:
        case VT_ZONE0:
        case VT_INT64:
        case VT_DOUBLE:
        case VT_CHAR:
            /* Do nothing */
            break;
        case VT_RIB:
            RnUnrefRib(ctx, obj->as_rib);
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            RnUnrefVector(ctx, obj->as_vector);
            break;
        case VT_HASHTABLE:
            RnUnrefHashtable(ctx, obj->as_hashtable);
            break;
        case VT_STRING:
            RnUnrefString(ctx, obj->as_string);
            break;
        case VT_BYTEVECTOR:
            RnUnrefBytevector(ctx, obj->as_bytevector);
            break;
        case VT_ROOT:
            /* Do nothing? Should be freed with ctx */
            break;
        default:
            RnPanic();
            break;
    }
    *type = VT_EMPTY;
}

void
RnValueUnref(RnCtx* ctx, Value* target){
    RnUnref(ctx, &target->value, &target->type);
}


void
RnValueRef(RnCtx* ctx, Value* target, ValueContainer obj, ValueType type){
    RnRef(ctx, obj, type);
    RnValueUnref(ctx, target);
    target->value = obj;
    target->type = type;
}

static void
RnObjHeaderInit(RnCtx* ctx, ObjHeader* header, ValueType t){
    header->refcnt = 0;
    switch(t){
        case VT_RIB:
            header->gc_refinfo = GC_TYPE_RIB;
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            header->gc_refinfo = GC_TYPE_VECTOR;
            break;
        case VT_HASHTABLE:
            header->gc_refinfo = GC_TYPE_HASHTABLE;
            break;
        default:
            RnPanic();
            break;
    }
    header->gc_prev = &ctx->gcroot;
    if(ctx->gcroot.gc_next){
        ctx->gcroot.gc_next->gc_prev = header;
    }
    header->gc_next = ctx->gcroot.gc_next;
    ctx->gcroot.gc_next = header;
}

static void
RnObjHeaderUnlink(RnCtx* ctx, ObjHeader* header){
    (void)ctx;
    if(header->gc_prev){
        header->gc_prev->gc_next = header->gc_next;
    }
    if(header->gc_next){
        header->gc_next->gc_prev = header->gc_prev;
    }
}

RnResult
RnRibSet(RnCtx* ctx, Value* target, Value* obj, int field){
    RNFUNC_BEGIN;
    ObjRib* r;
    if(target->type != VT_RIB){
        abort();
    }
    r = target->value.as_rib;
    RnUnref(ctx, &r->field[field], &r->type[field]);
    r->field[field] = obj->value;
    r->type[field] = obj->type;
    RnRef(ctx, r->field[field], r->type[field]);
    RNFUNC_END;
}

RnResult
RnRibRef(RnCtx* ctx, Value* out, Value* obj, int field){
    RNFUNC_BEGIN;
    ObjRib* r;
    if(obj->type != VT_RIB){
        abort();
    }
    r = obj->value.as_rib;
    RnValueRef(ctx, out, r->field[field], r->type[field]);
    RNFUNC_END;
}

RnResult
RnRib(RnCtx* ctx, Value* out, Value* field0, Value* field1, Value* field2){
    RNFUNC_BEGIN;
    ObjRib* r;
    Value v;
    ValueContainer vc;
    RnValueLink(ctx, &v);
    r = (ObjRib*)malloc(sizeof(ObjRib));
    if(!r){
        RnLowMemory();
    }
    RnObjHeaderInit(ctx, (ObjHeader*)r, VT_RIB);
    r->type[0] = r->type[1] = r->type[2] = VT_EMPTY;
    vc.as_rib = r;
    RnValueRef(ctx, &v, vc, VT_RIB);
    RNFUNC_CALL(ctx, RnRibSet(ctx, &v, field0, 0));
    RNFUNC_CALL(ctx, RnRibSet(ctx, &v, field1, 1));
    RNFUNC_CALL(ctx, RnRibSet(ctx, &v, field2, 2));
    RnValueRef(ctx, out, v.value, VT_RIB);
    RnValueUnlink(ctx, &v);
    RNFUNC_END;
}

RnResult
RnInt64(RnCtx* ctx, Value* out, int64_t i64){
    RNFUNC_BEGIN;
    ValueContainer v;
    v.as_int64 = i64;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

RnResult
RnDouble(RnCtx* ctx, Value* out, double d){
    RNFUNC_BEGIN;
    ValueContainer v;
    v.as_double = d;
    RnValueRef(ctx, out, v, VT_DOUBLE);
    RNFUNC_END;
}

RnResult
RnChar(RnCtx* ctx, Value* out, int c){
    RNFUNC_BEGIN;
    ValueContainer v;
    v.as_char = c;
    RnValueRef(ctx, out, v, VT_CHAR);
    RNFUNC_END;
}

RnResult
RnZone0(RnCtx* ctx, Value* out, ValueZone0 z){
    RNFUNC_BEGIN;
    ValueContainer v;
    v.as_zone0 = z;
    RnValueRef(ctx, out, v, VT_ZONE0);
    RNFUNC_END;
}

RnResult
RnCons(RnCtx* ctx, Value* out, Value* car, Value* cdr){
    RNFUNC_BEGIN;
    Value id;
    RnValueLink(ctx, &id);
    RNFUNC_CALL(ctx, RnInt64(ctx, &id, 0 /* Pair */));
    RNFUNC_CALL(ctx, RnRib(ctx, out, car, cdr, &id));
    RnValueUnlink(ctx, &id);
    RNFUNC_END;
}

RnResult
RnVector(RnCtx* ctx, Value* out, size_t len){
    RNFUNC_BEGIN;
    size_t i;
    ObjVector* r;
    ValueContainer v;
    r = (ObjVector*)malloc(sizeof(ObjVector));
    if(!r){
        RnLowMemory();
    }
    r->values = (ValueContainer*)malloc(sizeof(ValueContainer) * len);
    if(! r->values){
        RnLowMemory();
    }
    r->types = (ValueType*)malloc(sizeof(ValueType) * len);
    if(! r->types){
        RnLowMemory();
    }
    RnObjHeaderInit(ctx, (ObjHeader*)r, VT_VECTOR);
    for(i = 0; i!=len; i++){
        r->types[i] = VT_EMPTY;
    }
    r->length = len;
    v.as_vector = r;
    RnValueRef(ctx, out, v, VT_VECTOR);
    RNFUNC_END;
}

RnResult
RnVectorRef(RnCtx* ctx, Value* out, Value* target, size_t idx){
    RNFUNC_BEGIN;
    ObjVector* vec;
    if(target->type != VT_VECTOR && target->type != VT_SIMPLE_STRUCT){
        abort();
    }
    vec = target->value.as_vector;
    if(idx >= vec->length){
        abort();
    }
    RnValueRef(ctx, out, vec->values[idx], vec->types[idx]);
    RNFUNC_END;
}

RnResult
RnVectorSet(RnCtx* ctx, Value* target, Value* obj, size_t idx){
    RNFUNC_BEGIN;
    ObjVector* vec;
    if(target->type != VT_VECTOR && target->type != VT_SIMPLE_STRUCT){
        abort();
    }
    vec = target->value.as_vector;
    if(idx >= vec->length){
        abort();
    }
    RnRef(ctx, obj->value, obj->type);
    RnUnref(ctx, &vec->values[idx], &vec->types[idx]);
    vec->values[idx] = obj->value;
    vec->types[idx] = obj->type;
    RNFUNC_END;
}

RnResult
RnBytevector(RnCtx* ctx, Value* out, size_t len){
    RNFUNC_BEGIN;
    ValueContainer v;
    ObjBytevector* bv;

    bv = (ObjBytevector*)malloc(sizeof(ObjBytevector));
    if(! bv){
        RnLowMemory();
    }
    bv->buf = (uint8_t*)malloc(len);
    if(! bv->buf){
        RnLowMemory();
    }
    bv->len = len;
    bv->typeinfo = 0;
    bv->refcnt = 0;

    v.as_bytevector = bv;
    RnValueRef(ctx, out, v, VT_BYTEVECTOR);
    RNFUNC_END;
}

RnResult
RnBytevectorExternal(RnCtx* ctx, Value* out, void* ptr, 
                     int has_len, size_t len){
    RNFUNC_BEGIN;
    ValueContainer v;
    ObjBytevector* bv;

    bv = (ObjBytevector*)malloc(sizeof(ObjBytevector));
    if(! bv){
        RnLowMemory();
    }
    bv->buf = (uint8_t*)ptr;
    if(! bv->buf){
        RnLowMemory();
    }
    bv->len = len;
    if(has_len){
        bv->typeinfo = 1; /* Is external */
    }else{
        bv->typeinfo = 3; /* Is external and has unlimited range */
    }
    bv->refcnt = 0;

    v.as_bytevector = bv;
    RnValueRef(ctx, out, v, VT_BYTEVECTOR);
    RNFUNC_END;
}

RnResult
RnString(RnCtx* ctx, Value* out, const char* name, size_t len){
    RNFUNC_BEGIN;
    char* buf;
    ObjString* str;
    ValueContainer v;
    str = (ObjString*)malloc(sizeof(ObjString));
    if(! str){
        RnLowMemory();
    }
    str->refcnt = 0;
    buf = (char*)malloc(len + 1);
    if(! buf){
        RnLowMemory();
    }
    memcpy(buf, name, len);
    buf[len] = 0;
    str->str = buf;
    str->len = len;
    v.as_string = str;
    RnValueRef(ctx, out, v, VT_STRING);
    RNFUNC_END;
}

RnResult
RnUninternedSymbol(RnCtx* ctx, Value* out, Value* name){
    RNFUNC_BEGIN;
    Value id;
    RnValueLink(ctx, &id);
    RNFUNC_CALL(ctx, RnInt64(ctx, &id, 2 /* Symbol */));
    RNFUNC_CALL(ctx, RnRib(ctx, out, name, name, &id));
    RnValueUnlink(ctx, &id);
    RNFUNC_END;
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
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            if(x->value.as_vector == y->value.as_vector){
                return 1;
            }
            break;
        case VT_HASHTABLE: /* UNIMPL */
        default:
            RnPanic();
    }
    return 0;
}

static uint64_t
ht_hash_eqv(Value* x){
    switch(x->type){
        case VT_ZONE0:
            return (uint64_t)181 * (int)x->value.as_zone0;
        case VT_INT64:
            return x->value.as_int64;
        case VT_DOUBLE:
            /* Read the value as int64: We don't have to guarantee Nan
             * hash value */
            return x->value.as_int64;
        case VT_CHAR:
            return (uint64_t)781 * x->value.as_char;
        case VT_STRING:
            return ht_hash_blob(x);
        case VT_BYTEVECTOR:
            return (uintptr_t)x->value.as_bytevector;
        case VT_RIB:
            return (uintptr_t)x->value.as_rib;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            return (uintptr_t)x->value.as_vector;
        case VT_HASHTABLE: /* UNIMPL */
        default:
            RnPanic();
    }
}

static size_t /* loc or SIZE_MAX */
ht_lookup(RnCtx* ctx, ObjHashtable* ht, size_t start, Value* key){
    ObjRib* r;
    Value v;
    size_t loc;
    int e;
    RnValueLink(ctx, &v);
    r = ht->table[start].as_rib;
    while(1){
        if(! r){
            loc = SIZE_MAX; /* Not found */
            break;
        }
        loc = r->field[0].as_int64;
        if((size_t)loc >= ht->containercount){
            RnPanic();
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
                RnPanic();
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
            RnLowMemory();
        }
        ht->values = (ValueContainer*)realloc(ht->values, 
                                              sizeof(ValueContainer) * newsize);
        if(! ht->values){
            RnLowMemory();
        }
        ht->valuetypes = (ValueType*)realloc(ht->valuetypes, 
                                             sizeof(ValueType) * newsize);
        if(! ht->valuetypes){
            RnLowMemory();
        }
        if(ht->keytypes){
            ht->keytypes = (ValueType*)realloc(ht->keytypes,
                                               sizeof(ValueType) * newsize);
            if(! ht->keytypes){
                RnLowMemory();
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
            RnPanic();
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
            RnPanic();
            break;
    }
    return hashk % ht->tablesize;
}


RnResult
RnHashtableRef(RnCtx* ctx, Value* out, Value* ht, Value* key, Value* def){
    RNFUNC_BEGIN;
    size_t hashk;
    size_t loc;
    ObjHashtable* hto;
    Value frame;
    RnEnter(ctx, &frame);
    if(ht->type != VT_HASHTABLE){
        abort();
    }

    hto = ht->value.as_hashtable;
    hashk = ht_hash(hto, key);
    loc = ht_lookup(ctx, hto, hashk, key);
    if(loc == SIZE_MAX){
        RnValueRef(ctx, out, def->value, def->type);
    }else{
        RnValueRef(ctx, out, hto->values[loc], hto->valuetypes[loc]);
    }
    RnLeave(ctx, &frame);
    RNFUNC_END;
}

RnResult
RnHashtableSet(RnCtx* ctx, Value* ht, Value* key, Value* obj){
    RNFUNC_BEGIN;
    size_t hashk;
    size_t loc;
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
        RNFUNC_CALL(ctx, RnInt64(ctx, &me, loc));
        RNFUNC_CALL(ctx, RnInt64(ctx, &zero, 0));
        RNFUNC_CALL(ctx, RnRib(ctx, &r, &me, &zero, &zero));
        hto->table[hashk] = r.value;
        RnRef(ctx, hto->table[hashk], VT_RIB);
    }else{
        loc = ht_lookup(ctx, hto, hashk, key);
        if(loc != SIZE_MAX){
            /* Replace value on loc */
            RnUnref(ctx, &hto->values[loc], &hto->valuetypes[loc]);
            hto->values[loc] = obj->value;
            hto->valuetypes[loc] = obj->type;
            RnRef(ctx, hto->values[loc], hto->valuetypes[loc]);
        }else{
            /* Hash collision, add new loc and chain it as rib */
            loc = ht_add_value(ctx, hto, key, obj);
            RNFUNC_CALL(ctx, RnInt64(ctx, &me, loc));
            RNFUNC_CALL(ctx, RnInt64(ctx, &zero, 0));
            RnValueRef(ctx, &next, hto->table[hashk], VT_RIB);
            RNFUNC_CALL(ctx, RnRib(ctx, &r, &me, &next, &zero));
            hto->table[hashk] = r.value;
            RnRef(ctx, hto->table[hashk], VT_RIB);
        }
    }
    RnLeave(ctx, &frame);
    RNFUNC_END;
}

RnResult
RnHashtable(RnCtx* ctx, Value* out, HashtableClass htc){
    RNFUNC_BEGIN;
    ValueContainer ptrv;
    ObjHashtable* ht;
    size_t i;
    enum HashtableType_e type;

    switch(htc){
        case HTC_EQ_HASHTABLE:
        case HTC_EQV_HASHTABLE:
        case HTC_SYMBOL_HASHTABLE:
            type = HT_EQV;
            break;
        case HTC_INTEGER_HASHTABLE:
            type = HT_INTKEY;
            break;
        case HTC_STRING_HASHTABLE:
            type = HT_BLOBKEY;
            break;
        default:
            abort();
            break;
    }
    ht = (ObjHashtable*)malloc(sizeof(ObjHashtable));
    if(! ht){
        RnLowMemory();
    }
    RnObjHeaderInit(ctx, (ObjHeader*)ht, VT_HASHTABLE);

    ht->tablesize = 57;
    ht->table = (ValueContainer*)malloc(sizeof(ValueContainer) * ht->tablesize);
    if(! ht->table){
        RnLowMemory();
    }

    ht->containercount = 16;
    ht->keycount = 0;
    ht->hashtable_type = type;
    ht->hashtable_class = htc;

    ht->keys = (ValueContainer*)malloc(sizeof(ValueContainer) * ht->containercount);
    if(! ht->keys){
        RnLowMemory();
    }
    ht->valuetypes = (ValueType*)malloc(sizeof(ValueType) * ht->containercount);
    if(! ht->valuetypes){
        RnLowMemory();
    }
    ht->values = (ValueContainer*)malloc(sizeof(ValueContainer) * ht->containercount);
    if(! ht->values){
        RnLowMemory();
    }
    if(type == HT_EQV){
        ht->keytypes = (ValueType*)malloc(sizeof(ValueType) * ht->containercount);
        if(! ht->keytypes){
            RnLowMemory();
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
    RNFUNC_END;
}

/* Context */
void
RnCtxInit(RnCtx* newctx){
    newctx->current_frame = 0;
    newctx->gcroot.gc_prev = newctx->gcroot.gc_next = 0;
    RnEnter(newctx, &newctx->ctx_root);
    RnValueLink(newctx, &newctx->ht_global);
    RnValueLink(newctx, &newctx->ht_libinfo);
    RnValueLink(newctx, &newctx->ht_libcode);
    RnValueLink(newctx, &newctx->ht_macro);
    RnValueLink(newctx, &newctx->bootstrap);
    RnValueLink(newctx, &newctx->args);
    RnValueLink(newctx, &newctx->raise_proc);

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
        RnLowMemory();
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
        RnPanic();
    }

    RnEnter(ctx, &frame);
    v = (Value*)malloc(sizeof(Value)*total);
    if(! v){
        RnLowMemory();
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
        RnChar(ctx, &v[cur+i], (int)get_leb128(&p));
    }
    cur += chars;

    /* Numbers */
    for(i = 0; i != numbers; i++){
        proto = get_leb128(&p);
        switch(proto){
            case 1: /* Positive integer */
                val = get_leb128(&p);
                //debugprintf("Exact: [%ld]\n", val);
                RnInt64(ctx, &v[cur+i], val);
                break;
            case 2: /* Negative integer */
                val = (int64_t)0 - get_leb128(&p);
                //debugprintf("Exact: [%ld]\n", val);
                RnInt64(ctx, &v[cur+i], val);
                break;
            case 3: /* Scheme number */
                num = get_cstr(&p);
                d = strtod(num, NULL);
                if(d == 0){
                    debugprintf("FIXME: [%s] yielded exact zero?\n", num);
                    RnInt64(ctx, &v[cur+i], 0);
                }else{
                    //debugprintf("Inexact: [%s]\n", num);
                    RnDouble(ctx, &v[cur+i], d);
                }
                free(num);
                break;
            default:
                RnPanic();
                break;
        }
    }
    cur += numbers;

    /* Symbols */
    for(i = 0; i != symbols; i++){
        nam = get_cstr(&p); /* FIXME: use dedicated string function */
        namlen = strlen(nam);
        RnString(ctx, &symnam, nam, namlen);
        RnUninternedSymbol(ctx, &sym, &symnam);
        RnHashtableRef(ctx, &tmp, &ctx->ht_global, &symnam, &sym);
        if(tmp.value.as_rib == sym.value.as_rib){
            //debugprintf("Symbol = [%s] => %p\n", nam, (void*)sym.value.as_rib);
            RnHashtableSet(ctx, &ctx->ht_global, &symnam, &sym);
        }
        free(nam);
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
        RnVector(ctx, &v[cur+i], val);
        for(j = 0; j != val; j++){
            RnInt64(ctx, &tmp, get_leb128(&p));
            RnVectorSet(ctx, &v[cur+i], &tmp, j);
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
            debugprintf("[EMPTY]");
            break;
        case VT_ZONE0:
            switch(v->value.as_zone0){
                case ZZ_NIL:
                    debugprintf("[NIL]");
                    break;
                case ZZ_TRUE:
                    debugprintf("#t");
                    break;
                case ZZ_FALSE:
                    debugprintf("#f");
                    break;
                case ZZ_EOF_OBJECT:
                    debugprintf("#<eof-object>");
                    break;
                default:
                    debugprintf("[ZZ:%d]",(int)v->value.as_zone0);
                    break;
            }
            break;
        case VT_INT64:
            debugprintf("%" PRId64, v->value.as_int64);
            break;
        case VT_DOUBLE:
            debugprintf("%f", v->value.as_double);
            break;
        case VT_CHAR:
            // FIXME: UTF8
            debugprintf("%c", v->value.as_char);
            break;
        case VT_RIB:
            if(v->value.as_rib->type[2] == VT_INT64 &&
               v->value.as_rib->field[2].as_int64 == 1){
                debugprintf("#<procedure>");
            }else{
                debugprintf("#<");
                RnRibRef(ctx, &tmp, v, 0);
                emergency_print(ctx, &tmp);
                debugprintf(" ");
                RnRibRef(ctx, &tmp, v, 1);
                emergency_print(ctx, &tmp);
                debugprintf(" ");
                RnRibRef(ctx, &tmp, v, 2);
                emergency_print(ctx, &tmp);
                debugprintf(">");
            }
            break;
        case VT_VECTOR:
        case VT_SIMPLE_STRUCT:
            for(i=0;i!=v->value.as_vector->length;i++){
                if(i==0){
                    debugprintf("#(");
                }else{
                    debugprintf(" ");
                }
                RnVectorRef(ctx, &tmp, v, i);
                emergency_print(ctx, &tmp);
            }
            debugprintf(")");
            break;
        case VT_HASHTABLE:
            debugprintf("#<HASHTABLE>");
            break;
        case VT_STRING:
            debugprintf("\"%s\"", v->value.as_string->str);
            break;
        case VT_BYTEVECTOR:
            debugprintf("#<BYTEVECTOR>");
            break;
        case VT_ROOT:
            debugprintf("#<ROOT>");
            break;
        default:
            debugprintf("#<UNKNOWN:%d>", (int)v->type);
            break;

    }
    RnValueUnlink(ctx, &tmp);
}

struct vmstate_s {
    Value stack;
    Value pc;
    Value result;
    int exit_mode;
    int vals;
    /* temps */
    Value reg;
    Value opnd;
    Value zero;
};

static void
list_tail(RnCtx* ctx, Value* v, size_t d){
    size_t i;
    for(i=0;i!=d;i++){
        RnRibRef(ctx, v, v, 1);
    }
}

static void
get_cont(RnCtx* ctx, struct vmstate_s* state){
    while(1){
        if(state->stack.type != VT_RIB){
            break;
        }
        if(state->stack.value.as_rib->type[2] == VT_RIB){
            return;
        }
        RnRibRef(ctx, &state->stack, &state->stack, 1);
    }
}

static void /* => lookup(opnd) to reg */
get_var(RnCtx* ctx, struct vmstate_s* state){
    if(state->opnd.type == VT_INT64){
        RnValueRef(ctx, &state->reg, state->stack.value, state->stack.type);
        list_tail(ctx, &state->reg, state->opnd.value.as_int64);
        RnRibRef(ctx, &state->reg, &state->reg, 0);
    }else{
#ifdef TRACE
        if(state->opnd.value.as_rib->type[1] == VT_INT64){
            printf("GLBL trampoline\n");
        }else{
            printf("GLBL: %s\n", state->opnd.value.as_rib->field[1].as_string->str);
        }
#endif
        RnRibRef(ctx, &state->reg, &state->opnd, 0);
    }
}

static void
call_lambda(RnCtx* ctx, struct vmstate_s* state){
    Value tmp;
    Value acc;
    int argnc, layout, nargs, res;
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &acc);
    /* reg = proc */
    /* opnd <= code(field0 of proc) */
    RnRibRef(ctx, &state->opnd, &state->reg, 0);
    /* reg = newcont */
    RnRib(ctx, &state->reg, &state->zero, &state->reg, &state->zero);
    /* Construct Stack */
    if(state->opnd.type != VT_RIB){
        abort();
    }
    if(state->opnd.value.as_rib->type[0] != VT_INT64){
        abort();
    }
    layout = (int)state->opnd.value.as_rib->field[0].as_int64;
    argnc = (state->vals >= 0 && layout < 0) ? state->vals + 1 + layout : 0;
    nargs = (layout < 0) ? -layout : layout;
    if((state->vals >= 0) && argnc < 0 && state->vals < nargs){
        abort();
    }
    /* Construct rest argument on acc */
    RnZone0(ctx, &acc, ZZ_NIL);
#ifdef TRACE
    debugprintf("vals = %d, layout = %d, argnc = %d, nargs = %d\n", 
            state->vals, layout, argnc, nargs);
#endif
    for(res = argnc; res != 0; res--){
        RnRibRef(ctx, &tmp, &state->stack, 0);
        /*
        debugprintf("PUSHr:\n");
        emergency_print(ctx, &acc);
        debugprintf("\n");
        */
        RnRibRef(ctx, &state->stack, &state->stack, 1);
        RnCons(ctx, &acc, &tmp, &acc);
    }
    if(layout < 0){
#ifdef TRACE
        debugprintf("PUSHc:\n");
        emergency_print(ctx, &acc);
        debugprintf("\n");
#endif
        /* Push rest aguments as a list if required */
        RnCons(ctx, &state->stack, &acc, &state->stack);
    }
    /* acc = new stack, init as newcont */
    RnValueRef(ctx, &acc, state->reg.value, state->reg.type);
    for(res = nargs; res != 0; res--){
        RnRibRef(ctx, &tmp, &state->stack, 0);
#ifdef TRACE
        debugprintf("PUSHa:\n");
        emergency_print(ctx, &tmp);
        debugprintf("\n");
#endif
        RnRibRef(ctx, &state->stack, &state->stack, 1);
        RnCons(ctx, &acc, &tmp, &acc);
    }
#ifdef TRACE
    debugprintf("STA:\n");
    emergency_print(ctx, &acc);
    debugprintf("\n");
#endif
    /* Check if tail call */
    if(state->pc.type == VT_RIB){
        /* has rib as cont., so save stack and continuation on new stack */
        RnRibSet(ctx, &state->reg, &state->stack, 0);
        RnRibSet(ctx, &state->reg, &state->pc, 2);
    }else{
        get_cont(ctx, state);
        /* Concat old stack frame on new cont */
        RnRibRef(ctx, &tmp, &state->stack, 0);
        RnRibSet(ctx, &state->reg, &tmp, 0);
        RnRibRef(ctx, &tmp, &state->stack, 2);
        RnRibSet(ctx, &state->reg, &tmp, 2);
    }
    /* Set new pc and stack */
    state->vals = -1;
    RnRibRef(ctx, &state->pc, &state->opnd, 2);
    RnValueRef(ctx, &state->stack, acc.value, acc.type);
    /*
    debugprintf("ST:\n");
    emergency_print(ctx, &state->stack);
    debugprintf("\n");
    */
    RnValueUnlink(ctx, &acc);
    RnValueUnlink(ctx, &tmp);
}

/* 1: $vm-exit (VM) */
/* 2: apply-values (VM) */

static void
call_apply_values(RnCtx* ctx, struct vmstate_s* state){
    int vals;
    Value values;
    Value consumer;
    Value trampoline;
    RnValueLink(ctx, &trampoline);
    RnValueLink(ctx, &consumer);
    RnValueLink(ctx, &values);
    /* Receive args */
    RnRibRef(ctx, &values, &state->stack, 0);
    RnRibRef(ctx, &state->stack, &state->stack, 1);
    RnRibRef(ctx, &consumer, &state->stack, 0);
    RnRibRef(ctx, &state->stack, &state->stack, 1);

    /* Construct trampoline */
    RnRib(ctx, &trampoline, &consumer, &state->zero, &state->zero);
    RnRib(ctx, &trampoline, &state->zero, &trampoline, &state->pc);

    if(values.type == VT_RIB && 
       values.value.as_rib->type[2] == VT_INT64 &&
       values.value.as_rib->field[2].as_int64 == 6){
        /* Receive values from values object */
        vals = 0;
        RnRibRef(ctx, &values, &values, 0);
        while(1){
            if(values.type == VT_ZONE0 && values.value.as_zone0 == ZZ_NIL){
                break;
            }
            RnRibRef(ctx, &state->reg, &values, 0);
            RnRibRef(ctx, &values, &values, 1);
#ifdef TRACE
            debugprintf("ARG(%d):\n", vals);
            emergency_print(ctx, &state->reg);
            debugprintf("\n");
#endif
            RnCons(ctx, &state->stack, &state->reg, &state->stack);
            vals ++;
        }
        state->vals = vals;
    }else{
        /* Standard apply */
        state->vals = 1;
        RnCons(ctx, &state->stack, &values, &state->stack);
    }
    /* Jump to trampoline */
    RnValueRef(ctx, &state->pc, trampoline.value, trampoline.type);
    RnValueUnlink(ctx, &values);
    RnValueUnlink(ctx, &consumer);
    RnValueUnlink(ctx, &trampoline);
}

static int /* Continue? */
call_vm_exit(RnCtx* ctx, struct vmstate_s* state){
    int mode;
    int r = 0;
    Value arg;
    Value modeval;
    RnValueLink(ctx, &arg);
    RnValueLink(ctx, &modeval);
    RnRibRef(ctx, &arg, &state->stack, 0);
    RnRibRef(ctx, &state->stack, &state->stack, 1);
    RnRibRef(ctx, &modeval, &state->stack, 0);
    RnRibRef(ctx, &state->stack, &state->stack, 1);
    if(modeval.type != VT_INT64){
        abort();
    }
    mode = (int)modeval.value.as_int64;
    if(mode == 1){ /* Scheme exit */
        state->exit_mode = mode;
        // FIXME: Raise it instead
        exit((int)arg.value.as_int64);
    }else if(mode == 3){ /* Set raise proc */
        r = 1; /* continue */
        RnValueRef(ctx, &ctx->raise_proc, arg.value, arg.type);
    }else{ /* eval exit */
        RnValueRef(ctx, &state->result, arg.value, arg.type);
    }
    RnValueUnlink(ctx, &arg);
    RnValueUnlink(ctx, &modeval);
    return r;
}

static void
call_vmex(RnCtx* ctx, struct vmstate_s* state, RnVmExFunc func){
    func(ctx, state->vals, &state->stack);
}

static int
call_primitive(RnCtx* ctx, struct vmstate_s* state, 
               int64_t protocol, int64_t ident){
    int r;
    r = 1; /* Continue by default */
    switch(protocol){
        case 2: /* internal primitives */
            switch(ident){
                case 1: /* $vm-exit */
                    r = call_vm_exit(ctx, state);
                    break;
                case 2: /* apply-values */
                    call_apply_values(ctx, state);
                    break;
                default:
                    abort();
                    break;
            }
            break;
        case 1: /* external primitives */
            call_vmex(ctx, state, (RnVmExFunc)ident);
            break;
        default:
            abort();
            break;
    }
    /* For vmex, handle tail call here */
    if(protocol == 1){
        /* Check if tail call */
        if(state->pc.type != VT_RIB){
            /* Save primitive-returned stack */
            RnValueRef(ctx, &state->reg, 
                       state->stack.value, state->stack.type);
            /* Find stack bottom */
            get_cont(ctx, state);
            /* Patch stack */
            RnRibRef(ctx, &state->opnd, &state->stack, 0);
            RnRibSet(ctx, &state->reg, &state->opnd, 1);
            /* Replace pc and restore stack */
            RnRibRef(ctx, &state->pc, &state->stack, 2);
            RnValueRef(ctx, &state->stack, state->reg.value, state->reg.type);
        }
    }
    return r;
}

static int /* Continue? */
vmstep(RnCtx* ctx, struct vmstate_s* state){
    int r;
    int64_t inst;
    RnRibRef(ctx, &state->reg, &state->pc, 0);
    if(state->reg.type != VT_INT64){
        abort();
    }
    inst = state->reg.value.as_int64;
    RnRibRef(ctx, &state->opnd, &state->pc, 1);
    RnRibRef(ctx, &state->pc, &state->pc, 2);
    r = 1; /* Continue by default */

#ifdef TRACE
    debugprintf("op = %ld\n", inst);
#endif
    
    switch(inst){
        case 0: /* Jump/Call */
            get_var(ctx, state);
            if(state->reg.type != VT_RIB){
                abort();
            }
            if(state->reg.value.as_rib->type[1] == VT_ZONE0
               && (state->reg.value.as_rib->field[1].as_zone0 == ZZ_FALSE ||
                   state->reg.value.as_rib->field[1].as_zone0 == ZZ_TRUE)){
                r = call_primitive(ctx, state,
                                   state->reg.value.as_rib->field[1].as_zone0
                                   == ZZ_FALSE ? 1 : 2,
                                   state->reg.value.as_rib->field[0].as_int64);
            }else{
                /* Calling a lambda */
                call_lambda(ctx, state);
            }

            break;
        case 1: /* Set */
            state->vals = -1;
            if(state->opnd.type == VT_INT64){
                RnValueRef(ctx, &state->reg, 
                           state->stack.value, state->stack.type);
                list_tail(ctx, &state->reg, state->opnd.value.as_int64);
                RnRibRef(ctx, &state->opnd, &state->stack, 0);
                RnRibSet(ctx, &state->reg, &state->opnd, 0);
            }else{
                RnRibRef(ctx, &state->reg, &state->stack, 0);
                RnRibSet(ctx, &state->opnd, &state->reg, 0);
            }
            RnRibRef(ctx, &state->stack, &state->stack, 1);
            break;
        case 2: /* Get */
            state->vals = -1;
            get_var(ctx, state);
            RnCons(ctx, &state->stack, &state->reg, &state->stack);
            break;
        case 3: /* Const(verb) */
            state->vals = -1;
            RnCons(ctx, &state->stack, &state->opnd, &state->stack);
            break;
        case 4: /* if */
            state->vals = -1;
            RnRibRef(ctx, &state->reg, &state->stack, 0);
            RnRibRef(ctx, &state->stack, &state->stack, 1);
            if(state->reg.type == VT_ZONE0 
               && state->reg.value.as_zone0 == ZZ_FALSE){
                /* False case, do nothing */
            }else{
                /* True case, alter next with opnd */
                RnValueRef(ctx, &state->pc, 
                           state->opnd.value, state->opnd.type);
            }
            break;
        case 5: /* Enter(Yuni) */
            if(state->opnd.type != VT_INT64){
                abort();
            }
            state->vals = (int)state->opnd.value.as_int64;
            break;
        case 6: /* Const(Obj) */
            state->vals = -1;
            RnCons(ctx, &state->stack, &state->opnd, &state->stack);
#ifdef TRACE
            debugprintf("Const: ");
            emergency_print(ctx, &state->opnd);
            debugprintf("\n");
#endif
            break;
        default: /* Term */
            return 0;
    }
    return r;
}

static RnResult
RnVmRun(RnCtx* ctx, Value* out, Value* code){
    RNFUNC_BEGIN;
    int cont;
    Value frame;
    Value seven;
    Value zero;
    struct vmstate_s state;
    RnEnter(ctx, &frame);
    RnValueLink(ctx, &state.stack);
    RnValueLink(ctx, &state.pc);
    RnValueLink(ctx, &state.result);
    RnValueLink(ctx, &state.reg);
    RnValueLink(ctx, &state.opnd);
    RnValueLink(ctx, &state.zero);
    RnValueLink(ctx, &seven);
    RnValueLink(ctx, &zero);
    RNFUNC_CALL(ctx, RnInt64(ctx, &seven, 7));
    RNFUNC_CALL(ctx, RnInt64(ctx, &zero, 0));
    RNFUNC_CALL(ctx, RnInt64(ctx, &state.zero, 0));

    RNFUNC_CALL(ctx, RnRibRef(ctx, &state.pc, code, 0));
    RNFUNC_CALL(ctx, RnRibRef(ctx, &state.pc, &state.pc, 2));

    /* Something default */
    RNFUNC_CALL(ctx, RnInt64(ctx, &state.result, -1234));
    /* Initial instruction #<7 0 0> */
    RNFUNC_CALL(ctx, RnRib(ctx, &state.stack, &seven, &zero, &zero));
    /* Initial stack frame */
    RNFUNC_CALL(ctx, RnRib(ctx, &state.stack, &zero, &zero, &state.stack));
    state.vals = -1;

    while(1){
        cont = vmstep(ctx, &state);
        RnGcTick(ctx);
        if(! cont){
            break;
        }
    }


    RnValueRef(ctx, out, state.result.value, state.result.type);
    RnLeave(ctx, &frame);
    RNFUNC_END;
}

static RnResult
RnVmRun_list(RnCtx* ctx, Value* code){
    RNFUNC_BEGIN;
    Value tmp;
    Value queue;
    Value bogus; /* We don't need actual VMrun result */
    RnValueLink(ctx, &bogus);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &queue);

    RnValueRef(ctx, &queue, code->value, code->type);
    while(!(queue.type == VT_ZONE0 && queue.value.as_zone0 == ZZ_NIL)){
        RnRibRef(ctx, &tmp, &queue, 0);
        RnRibRef(ctx, &queue, &queue, 1);
        RnVmRun(ctx, &bogus, &tmp);
    }

    RnValueUnlink(ctx, &queue);
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &bogus);
    RNFUNC_END;
}

static size_t
read_vec_offset(RnCtx* ctx, Value* vec, size_t offs){
    Value tmp;
    size_t r;
    RnValueLink(ctx, &tmp);
    RnVectorRef(ctx, &tmp, vec, offs);
    if(tmp.type != VT_INT64){
        RnPanic();
    }
    r = (size_t)tmp.value.as_int64;
    RnValueUnlink(ctx, &tmp);
    return r;
}

static void
parse_ribcode(RnCtx* ctx, Value* code, Value* vec){
    Value frame;
    Value tbl;
    size_t out;
    size_t offs_rib;
    size_t offs_rosym;
    size_t ribcount;
    Value ribs;
    Value r;
    Value v;
    size_t vidx;
    Value zero;
    size_t idx;
    int i;

    RnEnter(ctx, &frame);
    RnValueLink(ctx, &tbl);
    RnValueLink(ctx, &ribs);
    RnValueLink(ctx, &r);
    RnValueLink(ctx, &v);
    RnValueLink(ctx, &zero);

    /* Init */
    RnVectorRef(ctx, &tbl, vec, 4);
    out = read_vec_offset(ctx, vec, 0);
    offs_rib = read_vec_offset(ctx, vec, 1);
    offs_rosym = read_vec_offset(ctx, vec, 2);
    ribcount = offs_rosym - offs_rib;
    RnVector(ctx, &ribs, ribcount);
    RnInt64(ctx, &zero, 0);

    /* Generate output ribs */
    idx = 0;
    while(idx != ribcount){
        RnRib(ctx, &r, &zero, &zero, &zero);
        RnVectorSet(ctx, &ribs, &r, idx);
        idx++;
    }

    /* Fill rib content */
    idx = offs_rib;
    while(idx != offs_rosym){
        RnVectorRef(ctx, &r, &ribs, (idx - offs_rib) / 3);
        for(i = 0; i != 3; i++){
            vidx = read_vec_offset(ctx, &tbl, idx + i);
            if((offs_rib <= vidx) && (offs_rosym > vidx)){
                RnVectorRef(ctx, &v, &ribs, (vidx - offs_rib) / 3);
            }else{
                RnVectorRef(ctx, &v, &tbl, vidx);
            }
            if(v.type == VT_EMPTY){
                RnPanic();
            }
            RnRibSet(ctx, &r, &v, i);
        }
        idx += 3;
    }
    /* Don't access `vec` to support code == vec case */
    RnVectorRef(ctx, code, &ribs, (out - offs_rib) / 3);
    if(code->type != VT_RIB){
        RnPanic();
    }
    RnLeave(ctx, &frame);
}

static void
parse_ribcode_list(RnCtx* ctx, Value* out, Value* lis){
    Value frame;
    Value tmp;
    Value obj;
    Value acc;
    RnEnter(ctx, &frame);
    RnValueLink(ctx, &acc);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &obj);
    RnValueRef(ctx, &tmp, lis->value, lis->type);
    RnZone0(ctx, &acc, ZZ_NIL);

    while(!(tmp.type == VT_ZONE0 && tmp.value.as_zone0 == ZZ_NIL)){
        RnRibRef(ctx, &obj, &tmp, 0);
        RnRibRef(ctx, &tmp, &tmp, 1);
        parse_ribcode(ctx, &obj, &obj);
        RnCons(ctx, &acc, &obj, &acc);
    }
    /* Reverse */
    RnZone0(ctx, out, ZZ_NIL);
    while(!(acc.type == VT_ZONE0 && acc.value.as_zone0 == ZZ_NIL)){
        RnRibRef(ctx, &obj, &acc, 0);
        RnRibRef(ctx, &acc, &acc, 1);
        RnCons(ctx, out, &obj, out);
    }
    RnLeave(ctx, &frame);
}

static void
parse_bootstrap(RnCtx* ctx){
    Value frame;
    Value cur;
    Value lib;
    Value libsym;
    Value sym;
    Value code;
    Value tmp;
    Value tmp2;
    // Library = (<lib> ...)
    // lib = #(libname libsym import* imports exports VMseq mac* VMmac)
    RnEnter(ctx, &frame);
    RnValueLink(ctx, &cur);
    RnValueLink(ctx, &lib);
    RnValueLink(ctx, &libsym);
    RnValueLink(ctx, &sym);
    RnValueLink(ctx, &code);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &tmp2);
    RnHashtable(ctx, &ctx->ht_libinfo, HTC_SYMBOL_HASHTABLE);
    RnHashtable(ctx, &ctx->ht_libcode, HTC_SYMBOL_HASHTABLE);
    RnHashtable(ctx, &ctx->ht_macro, HTC_SYMBOL_HASHTABLE);

    RnValueRef(ctx, &cur, ctx->bootstrap.value, ctx->bootstrap.type);
    while(1){
        if(cur.type == VT_ZONE0 && cur.value.as_zone0 == ZZ_NIL){
            break;
        }
        RnRibRef(ctx, &lib, &cur, 0);
        RnRibRef(ctx, &cur, &cur, 1);
        RnVectorRef(ctx, &libsym, &lib, 1);
        RnRibRef(ctx, &tmp, &libsym, 1);
        //printf("Loading %s ...\n", tmp.value.as_string->str);
        /* Libinfo */
        RnZone0(ctx, &tmp, ZZ_NIL);
        RnVectorRef(ctx, &tmp2, &lib, 6);
        RnCons(ctx, &tmp, &tmp2, &tmp);
        RnVectorRef(ctx, &tmp2, &lib, 4);
        RnCons(ctx, &tmp, &tmp2, &tmp);
        RnVectorRef(ctx, &tmp2, &lib, 3);
        RnCons(ctx, &tmp, &tmp2, &tmp);
        RnHashtableSet(ctx, &ctx->ht_libinfo, &libsym, &tmp);
        /* libcode */
        RnVectorRef(ctx, &tmp, &lib, 5);
        parse_ribcode_list(ctx, &tmp, &tmp);
        RnVectorSet(ctx, &lib, &tmp, 5); /* Write back unpacked ribcode */
        RnHashtableSet(ctx, &ctx->ht_libcode, &libsym, &tmp);
        /* mac* */
        RnVectorRef(ctx, &tmp, &lib, 7);
        parse_ribcode_list(ctx, &tmp, &tmp);
        RnVmRun_list(ctx, &tmp);
        RnVectorRef(ctx, &tmp, &lib, 6);
        while(1){
            if(tmp.type == VT_ZONE0 && tmp.value.as_zone0 == ZZ_NIL){
                break;
            }
            RnRibRef(ctx, &sym, &tmp, 0);
            RnRibRef(ctx, &tmp, &tmp, 1);
            RnRibRef(ctx, &tmp2, &sym, 0);
            RnHashtableSet(ctx, &ctx->ht_macro, &sym, &tmp2);
        }

    }
    RnLeave(ctx, &frame);
}

static void
run_bootstrap(RnCtx* ctx){
    Value frame;
    Value cur;
    Value lib;
    Value code;
    Value out;
    RnEnter(ctx, &frame);
    RnValueLink(ctx, &cur);
    RnValueLink(ctx, &lib);
    RnValueLink(ctx, &code);
    RnValueLink(ctx, &out);

    RnValueRef(ctx, &cur, ctx->bootstrap.value, ctx->bootstrap.type);
    while(1){
        if(cur.type == VT_ZONE0 && cur.value.as_zone0 == ZZ_NIL){
            break;
        }
        RnRibRef(ctx, &lib, &cur, 0);
        RnRibRef(ctx, &cur, &cur, 1);
        RnVectorRef(ctx, &code, &lib, 5);
        RnVmRun_list(ctx, &code);
    }

    RnLeave(ctx, &frame);
}

#include "prims.inc.h"

static void
enter_externals(RnCtx* ctx){
    /* Externals: #<addr #f 1> ro #<num #t 1> */
    Value frame;
    Value symnamestr;
    Value sym;
    Value zero;
    Value one;
    Value two;
    Value addr;
    Value obj;
    Value tmp;
    Value t;
    Value f;
    size_t i;
    i = 0;
    RnEnter(ctx, &frame);
    RnValueLink(ctx, &symnamestr);
    RnValueLink(ctx, &sym);
    RnValueLink(ctx, &addr);
    RnValueLink(ctx, &zero);
    RnValueLink(ctx, &one);
    RnValueLink(ctx, &two);
    RnValueLink(ctx, &obj);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &t);
    RnValueLink(ctx, &f);
    RnInt64(ctx, &zero, 0);
    RnInt64(ctx, &one, 1);
    RnInt64(ctx, &two, 2);
    RnZone0(ctx, &t, ZZ_TRUE);
    RnZone0(ctx, &f, ZZ_FALSE);
    while(vm_externals[i].symname){
        /* Construct VM function */
        RnInt64(ctx, &addr, (uintptr_t)vm_externals[i].func);
        RnRib(ctx, &obj, &addr, &f, &one);
        RnString(ctx, &symnamestr, 
                 vm_externals[i].symname, vm_externals[i].namelen);
        if(strlen(vm_externals[i].symname) != vm_externals[i].namelen){
            RnPanic();
        }
        RnUninternedSymbol(ctx, &sym, &symnamestr);
        RnHashtableRef(ctx, &tmp, &ctx->ht_global, &symnamestr, &sym);
        //printf("[%s] = %p\n", vm_externals[i].symname, (void*)tmp.value.as_rib);
        if(tmp.value.as_rib == sym.value.as_rib){
            printf("New addition?? [%s]\n", vm_externals[i].symname);
            RnHashtableSet(ctx, &ctx->ht_global, &symnamestr, &sym);
        }
        RnRibSet(ctx, &tmp, &obj, 0);
        i++;
    }
    /* 1: $vm-exit */
    RnRib(ctx, &obj, &one, &t, &one);
    RnString(ctx, &symnamestr, "$vm-exit", sizeof("$vm-exit") - 1);
    RnUninternedSymbol(ctx, &sym, &symnamestr);
    RnHashtableRef(ctx, &tmp, &ctx->ht_global, &symnamestr, &sym);
    if(tmp.value.as_rib == sym.value.as_rib){
        RnHashtableSet(ctx, &ctx->ht_global, &symnamestr, &sym);
    }
    RnRibSet(ctx, &tmp, &obj, 0);

    /* 2: apply-values */
    RnRib(ctx, &obj, &two, &t, &one);
    RnString(ctx, &symnamestr, "apply-values", sizeof("apply-values") - 1);
    RnUninternedSymbol(ctx, &sym, &symnamestr);
    RnHashtableRef(ctx, &tmp, &ctx->ht_global, &symnamestr, &sym);
    if(tmp.value.as_rib == sym.value.as_rib){
        RnHashtableSet(ctx, &ctx->ht_global, &symnamestr, &sym);
    }
    RnRibSet(ctx, &tmp, &obj, 0);

    RnLeave(ctx, &frame);
}

void
RnCtxRunBootstrap(RnCtx* ctx, const uint8_t* bootstrap){
    load_bootstrap(ctx, bootstrap);
    enter_externals(ctx);
    parse_bootstrap(ctx);
    run_bootstrap(ctx);
}

#ifndef RN_EMBEDDING
/* main */
static const char* bootfile = BUILDROOT "/" BOOTNAME;

int
main(int ac, char** av){
    int i,argstart;
    uint8_t* bootstrap;
    Value str;

    RnCtx ctx;
    RnCtxInit(&ctx);

    RnValueLink(&ctx, &str);
    /* Parse arguments */
    if(ac > 0){
        argstart = 1; // TEMP
        RnVector(&ctx, &ctx.args, ac - argstart + 4);
        /* default arguments */
        RnString(&ctx, &str, "-yuniroot", sizeof("-yuniroot") - 1);
        RnVectorSet(&ctx, &ctx.args, &str, 0);
        RnString(&ctx, &str, YUNIROOT, sizeof(YUNIROOT) - 1);
        RnVectorSet(&ctx, &ctx.args, &str, 1);
        RnString(&ctx, &str, "-runtimeroot", sizeof("-runtimeroot") - 1);
        RnVectorSet(&ctx, &ctx.args, &str, 2);
        RnString(&ctx, &str, RUNTIMEROOT, sizeof(RUNTIMEROOT) - 1);
        RnVectorSet(&ctx, &ctx.args, &str, 3);
        for(i=argstart;i!=ac;i++){
            /* pack rest arguments into a vector */
            // FIXME: Use ARG_MAX on posix
            RnString(&ctx, &str, av[i], strnlen(av[i], 4096));
            RnVectorSet(&ctx, &ctx.args, &str, i - argstart + 4);
        }
    }else{
        /* AC should never be 0 on POSIX */
        abort();
    }
    RnValueUnlink(&ctx, &str);

    /* Load bootfile */
    bootstrap = (uint8_t*)BsFileReadAll(bootfile);

    /* Run bootstrap */
    RnCtxRunBootstrap(&ctx, bootstrap);
    free(bootstrap);
    // FIXME: Deinit context here

    return 0;
}
#endif
