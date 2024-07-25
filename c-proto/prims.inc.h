typedef RnResult (*ExArg_0_1)(RnCtx* ctx, Value* out);
typedef RnResult (*ExArg_1_1)(RnCtx* ctx, Value* out, Value* x);
typedef RnResult (*ExArg_1_2)(RnCtx* ctx, Value* out, Value* out2, Value* x);
typedef RnResult (*ExArg_2_1)(RnCtx* ctx, Value* out, Value* x, Value* y);
typedef RnResult (*ExArg_2_2)(RnCtx* ctx, Value* out, Value* out2,
                          Value* x, Value* y);
typedef RnResult (*ExArg_3_1)(RnCtx* ctx, Value* out, Value* x, Value* y, 
                              Value* z);
typedef RnResult (*ExArg_4_1)(RnCtx* ctx, Value* out, Value* x, Value* y, 
                              Value* Z, Value* w);
typedef RnResult (*ExArg_5_1)(RnCtx* ctx, Value* out, Value* x, Value* y, 
                              Value* z, Value* w, Value* u);

static RnResult
ExCall_0_1(ExArg_0_1 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value out[1];
    if(argc != 0){
        debugprintf("Invalid argument count %d (expected %d)\n",
                argc, 0);
        abort();
    }
    RnValueLink(ctx, &out[0]);
    RNFUNC_CALL(ctx, func(ctx, &out[0]));
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RNFUNC_END;
}

static RnResult
ExCall_1_1(ExArg_1_1 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value arg[1];
    Value out[1];
    if(argc != 1){
        debugprintf("Invalid argument count %d (expected %d)\n",
                argc, 1);
        abort();
    }
    RnValueLink(ctx, &arg[0]);
    RnValueLink(ctx, &out[0]);
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RNFUNC_CALL(ctx, func(ctx, &out[0], &arg[0]));
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RNFUNC_END;
}

static RnResult
ExCall_1_2(ExArg_1_2 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value tmp;
    Value six;
    Value zero;
    Value nil;
    Value arg[1];
    Value out[2];
    if(argc != 1){
        debugprintf("Invalid argument count %d (expected %d)\n",
                argc, 1);
        abort();
    }
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
    RnRibRef(ctx, &arg[0], stack, 0);
    RnRibRef(ctx, stack, stack, 1);
    RNFUNC_CALL(ctx,func(ctx, &out[0], &out[1], &arg[0]));
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
    RNFUNC_END;
}

static RnResult
ExCall_2_1(ExArg_2_1 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value arg[2];
    Value out[1];
    if(argc != 2){
        debugprintf("Invalid argument count %d (expected %d)\n",
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
    RNFUNC_CALL(ctx, func(ctx, &out[0], &arg[0], &arg[1]));
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RNFUNC_END;
}

static RnResult
ExCall_2_2(ExArg_2_2 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value tmp;
    Value six;
    Value zero;
    Value nil;
    Value arg[2];
    Value out[2];
    if(argc != 2){
        debugprintf("Invalid argument count %d (expected %d)\n",
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
    RNFUNC_CALL(ctx,func(ctx, &out[0], &out[1], &arg[0], &arg[1]));
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
    RNFUNC_END;
}

static RnResult
ExCall_3_1(ExArg_3_1 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value arg[3];
    Value out[1];
    if(argc != 3){
        debugprintf("Invalid argument count %d (expected %d)\n",
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
    RNFUNC_CALL(ctx,func(ctx, &out[0], &arg[0], &arg[1], &arg[2]));
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RnValueUnlink(ctx, &arg[2]);
    RNFUNC_END;
}

static RnResult
ExCall_4_1(ExArg_4_1 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value arg[4];
    Value out[1];
    if(argc != 4){
        debugprintf("Invalid argument count %d (expected %d)\n",
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
    RNFUNC_CALL(ctx, func(ctx, &out[0], &arg[0], &arg[1], &arg[2], &arg[3]));
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RnValueUnlink(ctx, &arg[2]);
    RnValueUnlink(ctx, &arg[3]);
    RNFUNC_END;
}

static RnResult
ExCall_5_1(ExArg_5_1 func, RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value arg[5];
    Value out[1];
    if(argc != 5){
        debugprintf("Invalid argument count %d (expected %d)\n",
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
    RNFUNC_CALL(ctx, func(ctx, &out[0], &arg[0], &arg[1], &arg[2], &arg[3], 
                          &arg[4]));
    RnCons(ctx, stack, &out[0], stack);
    RnValueUnlink(ctx, &out[0]);
    RnValueUnlink(ctx, &arg[0]);
    RnValueUnlink(ctx, &arg[1]);
    RnValueUnlink(ctx, &arg[2]);
    RnValueUnlink(ctx, &arg[3]);
    RnValueUnlink(ctx, &arg[4]);
    RNFUNC_END;
}


#define VMBRIDGENAME_0_1(call) Bridge ## call
#define VMBRIDGENAME_1_1(call) Bridge ## call
#define VMBRIDGENAME_1_2(call) Bridge ## call
#define VMBRIDGENAME_2_1(call) Bridge ## call
#define VMBRIDGENAME_2_2(call) Bridge ## call
#define VMBRIDGENAME_3_1(call) Bridge ## call
#define VMBRIDGENAME_4_1(call) Bridge ## call
#define VMBRIDGENAME_5_1(call) Bridge ## call
#define VMBRIDGENAME_N(call) call

#define VMBRIDGE_N(call)
#define VMBRIDGE_0_1(call) VMBRIDGEGEN(_0_1, call)
#define VMBRIDGE_1_1(call) VMBRIDGEGEN(_1_1, call)
#define VMBRIDGE_1_2(call) VMBRIDGEGEN(_1_2, call)
#define VMBRIDGE_2_1(call) VMBRIDGEGEN(_2_1, call)
#define VMBRIDGE_2_2(call) VMBRIDGEGEN(_2_2, call)
#define VMBRIDGE_3_1(call) VMBRIDGEGEN(_3_1, call)
#define VMBRIDGE_4_1(call) VMBRIDGEGEN(_4_1, call)
#define VMBRIDGE_5_1(call) VMBRIDGEGEN(_5_1, call)

#define VMBRIDGEGEN(f,call) \
    static RnResult \
    VMBRIDGENAME ## f(call)(RnCtx* ctx, int argc, Value* stack){ \
        RNFUNC_BEGIN; \
        RNFUNC_CALL(ctx, ExCall ## f(call, ctx, argc, stack)); \
        RNFUNC_END; \
    } 

#define GEN_FILD(nam, func, proto) {nam, sizeof(nam) - 1, VMBRIDGENAME ## proto(func)},
#define GEN_BRIDGE(_, func, proto) VMBRIDGE ## proto(func)

static RnResult
to_bool(RnCtx* ctx, Value* out, int x){
    RNFUNC_BEGIN;
    if(x){
        RNFUNC_CALL(ctx, RnZone0(ctx, out, ZZ_TRUE));
    }else{
        RNFUNC_CALL(ctx, RnZone0(ctx, out, ZZ_FALSE));
    }
    RNFUNC_END;
}
/* (rib x y z) => rib */
/* (field0 r) => x */
/* (field1 r) => x */
/* (field2 r) => x */
/* (field0-set! r v) => v */
/* (field1-set! r v) => v */
/* (field2-set! r v) => v */

#define EXLIB_RIB(x) \
    x("rib", ExRib, _3_1) \
    x("field0", ExField0, _1_1) \
    x("field1", ExField1, _1_1) \
    x("field2", ExField2, _1_1) \
    x("field0-set!", ExField0SetEx, _2_1) \
    x("field1-set!", ExField1SetEx, _2_1) \
    x("field2-set!", ExField2SetEx, _2_1) 


static RnResult
ExRib(RnCtx* ctx, Value* out, Value* x, Value* y, Value* z){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRib(ctx, out, x, y, z));
    RNFUNC_END;
}

static RnResult
ExField0(RnCtx* ctx, Value* out, Value* r){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRibRef(ctx, out, r, 0));
    RNFUNC_END;
}

static RnResult
ExField1(RnCtx* ctx, Value* out, Value* r){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRibRef(ctx, out, r, 1));
    RNFUNC_END;
}

static RnResult
ExField2(RnCtx* ctx, Value* out, Value* r){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRibRef(ctx, out, r, 2));
    RNFUNC_END;
}

static RnResult
ExField0SetEx(RnCtx* ctx, Value* out, Value* r, Value* obj){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRibSet(ctx, r, obj, 0));
    RnValueRef(ctx, out, obj->value, obj->type);
    RNFUNC_END;
}

static RnResult
ExField1SetEx(RnCtx* ctx, Value* out, Value* r, Value* obj){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRibSet(ctx, r, obj, 1));
    RnValueRef(ctx, out, obj->value, obj->type);
    RNFUNC_END;
}

static RnResult
ExField2SetEx(RnCtx* ctx, Value* out, Value* r, Value* obj){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnRibSet(ctx, r, obj, 2));
    RnValueRef(ctx, out, obj->value, obj->type);
    RNFUNC_END;
}

/* (id x) => x */
/* arg1 -- drop a value on stack */
/* (arg2 y x) => x */
/* (vminject x) => y */
/* (vmfetchcode x) => y */
/* (vmfetch x) => y */
/* ($$command-line bogus) => vector */
/* ($$lookup-cached-libinfo sym) => obj */
/* ($$lookup-cached-code sym) => VM-code */
/* ($$lookup-cached-macro sym) => VM-code */
/* ($$macro-runtime-mode bogus) => 0 */
/* close (VM) */
/* ($$runvm code) => out */

#define EXLIB_VMSUP(x) \
    x("id", ExId, _N) \
    x("arg1", ExArg1, _N) \
    x("arg2", ExArg2, _N) \
    x("vminject", ExVminject, _N) \
    x("vmfetchcode", ExVmfetchcode, _N) \
    x("vmfetch", ExVmfetch, _N) \
    x("$$command-line", ExCommandLine, _1_1) \
    x("$$lookup-cached-libinfo", ExLookupCachedLibinfo, _1_1) \
    x("$$lookup-cached-code", ExLookupCachedCode, _1_1) \
    x("$$lookup-cached-macro", ExLookupCachedMacro, _1_1) \
    x("$$macro-runtime-mode", ExMacroRuntimeMode, _1_1) \
    x("close", ExClose, _N) \
    x("$$runvm", ExRunVm, _1_1)

static RnResult
ExId(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    (void) stack;
    (void) argc;
    (void) ctx;
    RNFUNC_END;
}

static RnResult
ExArg1(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    (void) argc;
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    RNFUNC_END;
}

static RnResult
ExArg2(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value tmp;
    (void) argc;
    RnValueLink(ctx, &tmp);
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &tmp, stack));
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExVminject(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    (void) ctx;
    (void) stack;
    if(argc != 1){
        abort();
    }
    RNFUNC_END;
}

static RnResult
ExVmfetchcode(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    (void) ctx;
    (void) stack;
    if(argc != 1){
        abort();
    }
    RNFUNC_END;
}

static RnResult
ExVmfetch(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    (void) ctx;
    (void) stack;
    if(argc != 1){
        abort();
    }
    RNFUNC_END;
}

static RnResult
ExCommandLine(RnCtx* ctx, Value* out, Value* bogus){
    RNFUNC_BEGIN;
    (void) bogus;
    RnValueRef(ctx, out, ctx->args.value, ctx->args.type);
    RNFUNC_END;
}

static RnResult
ExLookupCachedLibinfo(RnCtx* ctx, Value* out, Value* libsym){
    RNFUNC_BEGIN;
    Value fail;
    RnValueLink(ctx, &fail);
    RNFUNC_CALL(ctx, to_bool(ctx, &fail, 0));
    RNFUNC_CALL(ctx, RnHashtableRef(ctx, out, &ctx->ht_libinfo, libsym, &fail));
    RnValueUnlink(ctx, &fail);
    RNFUNC_END;
}

static RnResult
ExLookupCachedCode(RnCtx* ctx, Value* out, Value* libsym){
    RNFUNC_BEGIN;
    Value fail;
    RnValueLink(ctx, &fail);
    RNFUNC_CALL(ctx, to_bool(ctx, &fail, 0));
    RNFUNC_CALL(ctx, RnHashtableRef(ctx, out, &ctx->ht_libcode, libsym, &fail));
    RnValueUnlink(ctx, &fail);
    RNFUNC_END;
}

static RnResult
ExLookupCachedMacro(RnCtx* ctx, Value* out, Value* sym){
    RNFUNC_BEGIN;
    Value fail;
    RnValueLink(ctx, &fail);
    RNFUNC_CALL(ctx, to_bool(ctx, &fail, 0));
    RNFUNC_CALL(ctx, RnHashtableRef(ctx, out, &ctx->ht_macro, sym, &fail));
    RnValueUnlink(ctx, &fail);
    RNFUNC_END;
}

static RnResult
ExMacroRuntimeMode(RnCtx* ctx, Value* out, Value* bogus){
    RNFUNC_BEGIN;
    (void) bogus;
    RNFUNC_CALL(ctx, RnInt64(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExClose(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value cur;
    Value one;
    (void) argc;
    RnValueLink(ctx, &cur);
    RnValueLink(ctx, &one);
    RNFUNC_CALL(ctx, RnInt64(ctx, &one, 1));
    RNFUNC_CALL(ctx, RnRibRef(ctx, &cur, stack, 0));
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    RNFUNC_CALL(ctx, RnRibRef(ctx, &cur, &cur, 0));
    RNFUNC_CALL(ctx, RnRib(ctx, &cur, &cur, stack, &one));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &cur, stack));
    RnValueUnlink(ctx, &one);
    RnValueUnlink(ctx, &cur);
    RNFUNC_END;
}

static RnResult
ExRunVm(RnCtx* ctx, Value* out, Value* code){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnVmRun(ctx, out, code));
    RNFUNC_END;
}



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
/* (string? obj) => <BOOL> */
/* (bytevector? obj) => <BOOL> */
/* (pair? obj) => <BOOL> */
/* (symbol? obj) => <BOOL> */
/* (vector? obj) => <BOOL> */
/* (simple-struct? obj) => <BOOL> */

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
    x("string->utf8", ExStringToUtf8, _N) \
    x("string?", ExStringP, _1_1) \
    x("bytevector?", ExBytevectorP, _1_1) \
    x("pair?", ExPairP, _1_1) \
    x("symbol?", ExSymbolP, _1_1) \
    x("vector?", ExVectorP, _1_1) \
    x("simple-struct?", ExSimpleStructP, _1_1)

static RnResult
ExStringP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_STRING));
    RNFUNC_END;
}

static RnResult
ExBytevectorP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_BYTEVECTOR));
    RNFUNC_END;
}

static RnResult
ExPairP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx,
                to_bool(ctx, out, x->type == VT_RIB &&
                        x->value.as_rib->type[2] == VT_INT64 &&
                        x->value.as_rib->field[2].as_int64 == 0));
    RNFUNC_END;
}

static RnResult
ExSymbolP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx,
                to_bool(ctx, out, x->type == VT_RIB &&
                        x->value.as_rib->type[2] == VT_INT64 &&
                        x->value.as_rib->field[2].as_int64 == 2));
    RNFUNC_END;
}

static RnResult
ExVectorP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_VECTOR));
    RNFUNC_END;
}

static RnResult
ExSimpleStructP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_SIMPLE_STRUCT));
    RNFUNC_END;
}

static RnResult
ExCharP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_CHAR));
    RNFUNC_END;
}

static RnResult
ExFixnumP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_INT64));
    RNFUNC_END;
}

static RnResult
ExFlonumP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_DOUBLE));
    RNFUNC_END;
}

static RnResult
ExRibP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->type == VT_RIB));
    RNFUNC_END;
}

static RnResult
ExEqvP(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    int r;

    if(x->type != y->type){
        RNFUNC_CALL(ctx, to_bool(ctx, out, 0));
        goto finish;
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
    RNFUNC_CALL(ctx, to_bool(ctx, out, r));
finish:
    RNFUNC_END;
}

static RnResult
ExProcedureP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    int r;
    Value tmp;
    RnValueLink(ctx, &tmp);
    if(x->type != VT_RIB){
        r = 0;
    }else{
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, x, 2));
        if(tmp.type == VT_INT64 && tmp.value.as_int64 == 1){
            r = 1;
        }else{
            r = 0;
        }
        RNFUNC_CALL(ctx, to_bool(ctx, out, r));
        RnValueUnlink(ctx, &tmp);
    }
    RNFUNC_END;
}

static RnResult
ExValues(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    int i;
    Value zero;
    Value six;
    Value tmp;
    Value tmp2;
    if(argc == 1){
        /* Do nothing */
    }else{
        RnValueLink(ctx, &tmp2);
        RnValueLink(ctx, &tmp);
        RnValueLink(ctx, &zero);
        RnValueLink(ctx, &six);
        RNFUNC_CALL(ctx, RnInt64(ctx, &zero, 0));
        RNFUNC_CALL(ctx, RnInt64(ctx, &six, 6));
        RNFUNC_CALL(ctx, RnZone0(ctx, &tmp, ZZ_NIL));
        for(i=0;i!=argc;i++){
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp2, stack, 0));
            RNFUNC_CALL(ctx, RnCons(ctx, &tmp, &tmp2, &tmp));
            RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        }
        RNFUNC_CALL(ctx, RnRib(ctx, &tmp, &tmp, &zero, &six));
        RNFUNC_CALL(ctx, RnCons(ctx, stack, &tmp, stack));
        RnValueUnlink(ctx, &six);
        RnValueUnlink(ctx, &zero);
        RnValueUnlink(ctx, &tmp);
        RnValueUnlink(ctx, &tmp2);
    }
    RNFUNC_END;
}

static RnResult
ExListToValues(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    Value zero;
    Value six;
    RnValueLink(ctx, &zero);
    RnValueLink(ctx, &six);
    RNFUNC_CALL(ctx, RnInt64(ctx, &zero, 0));
    RNFUNC_CALL(ctx, RnInt64(ctx, &six, 6));
    RNFUNC_CALL(ctx, RnRib(ctx, out, x, &zero, &six));
    RnValueUnlink(ctx, &six);
    RnValueUnlink(ctx, &zero);
    RNFUNC_END;
}

static RnResult
ExStringToSymbol(RnCtx* ctx, Value* out, Value* str){
    RNFUNC_BEGIN;
    Value sym;
    if(str->type != VT_STRING){
        abort();
    }
    RnValueLink(ctx, &sym);
    RNFUNC_CALL(ctx, RnUninternedSymbol(ctx, &sym, str));
    RNFUNC_CALL(ctx, RnHashtableRef(ctx, out, &ctx->ht_global, str, &sym));
    if(out->value.as_rib == sym.value.as_rib){
        RNFUNC_CALL(ctx, RnHashtableSet(ctx, &ctx->ht_global, str, &sym));
    }
    RnValueUnlink(ctx, &sym);
    RNFUNC_END;
}

static RnResult
ExSymbolToString(RnCtx* ctx, Value* out, Value* sym){
    RNFUNC_BEGIN;
    Value tmp;
    RnValueLink(ctx, &tmp);
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, sym, 2));
    if(tmp.type != VT_INT64 || tmp.value.as_int64 != 2){
        abort();
    }
    RNFUNC_CALL(ctx, RnRibRef(ctx, out, sym, 1));
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExCharToInteger(RnCtx* ctx, Value* out, Value* c){
    RNFUNC_BEGIN;
    if(c->type != VT_CHAR){
        abort();
    }
    RNFUNC_CALL(ctx, RnInt64(ctx, out, c->value.as_char));
    RNFUNC_END;
}

static RnResult
ExIntegerToChar(RnCtx* ctx, Value* out, Value* i){
    RNFUNC_BEGIN;
    if(i->type != VT_INT64){
        abort();
    }
    if(i->value.as_int64 < 0){
        abort();
    }
    RNFUNC_CALL(ctx, RnChar(ctx, out, (int)i->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExUtf8ToString_3_1(RnCtx* ctx, Value* out, Value* bv, Value* start, Value* end){
    RNFUNC_BEGIN;
    size_t istart;
    size_t iend;
    size_t ilen;
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
    iend = end->value.as_int64;
    if(istart > iend){
        abort();
    }
    ilen = iend - istart;

    RNFUNC_CALL(ctx, RnString(ctx, out, 
                              (char*)bv->value.as_bytevector->buf + istart, 
                              ilen));
    RNFUNC_END;
}

static RnResult
ExUtf8ToString_2_1(RnCtx* ctx, Value* out, Value* bv, Value* start){
    RNFUNC_BEGIN;
    size_t istart;
    size_t iend;
    size_t ilen;
    if(start->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    istart = start->value.as_int64;
    iend = bv->value.as_bytevector->len;
    if(istart > iend){
        abort();
    }
    ilen = iend - istart;

    RNFUNC_CALL(ctx, RnString(ctx, out, 
                              (char*)bv->value.as_bytevector->buf + istart, 
                              ilen));
    RNFUNC_END;
}

static RnResult
ExUtf8ToString_1_1(RnCtx* ctx, Value* out, Value* bv){
    RNFUNC_BEGIN;
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    RNFUNC_CALL(ctx, RnString(ctx, out, 
                              (char*)bv->value.as_bytevector->buf, 
                              bv->value.as_bytevector->len));
    RNFUNC_END;
}

static RnResult
ExUtf8ToString(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, ExCall_1_1(ExUtf8ToString_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx, ExCall_2_1(ExUtf8ToString_2_1, ctx, argc, stack));
            break;
        case 3:
            RNFUNC_CALL(ctx, ExCall_3_1(ExUtf8ToString_3_1, ctx, argc, stack));
            break;
        default:
            abort();
    }
    RNFUNC_END;
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
    if(start + r > stop){
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
    if(start + r > stop){
        abort();
    }
    switch(r){
        case 1:
            *(unsigned char*)start = (unsigned char)c;
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

static RnResult
vector_to_string(RnCtx* ctx, Value* out, ObjVector* vec, 
                 size_t start, size_t end){
    RNFUNC_BEGIN;
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
        RnLowMemory();
    }
    s = (char*)malloc(siz + 1);
    if(! s){
        RnLowMemory();
    }
    s[siz] = 0;

    /* Pass2: Convert to utf8 blob */
    p = s;
    pend = s + siz;
    for(i=start; i!= end; i++){
        c = vec->values[i].as_char;
        utf8_encode(p, pend, c);
        p += utf8_ebytes(c);
    }
    str->str = (const char*)s;
    str->len = siz;
    str->refcnt = 0;
    v.as_string = str;

    RnValueRef(ctx, out, v, VT_STRING);
    RNFUNC_END;
}

static RnResult
ExVectorToString_3_1(RnCtx* ctx, Value* out, Value* vec, Value* start,
                     Value* end){
    RNFUNC_BEGIN;
    if(vec->type != VT_VECTOR){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    if(start->value.as_int64 < 0){
        abort();
    }
    if(end->value.as_int64 < 0){
        abort();
    }
    RNFUNC_CALL(ctx, vector_to_string(ctx, out, vec->value.as_vector,
                                      start->value.as_int64, 
                                      end->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExVectorToString_2_1(RnCtx* ctx, Value* out, Value* vec, Value* start){
    RNFUNC_BEGIN;
    if(vec->type != VT_VECTOR){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    if(start->value.as_int64 < 0){
        abort();
    }
    RNFUNC_CALL(ctx, vector_to_string(ctx, out, vec->value.as_vector,
                                      start->value.as_int64, 
                                      vec->value.as_vector->length));
    RNFUNC_END;
}

static RnResult
ExVectorToString_1_1(RnCtx* ctx, Value* out, Value* vec){
    RNFUNC_BEGIN;
    if(vec->type != VT_VECTOR){
        abort();
    }
    RNFUNC_CALL(ctx, vector_to_string(ctx, out, vec->value.as_vector,
                                      0, vec->value.as_vector->length));
    RNFUNC_END;
}

static RnResult
ExVectorToString(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, 
                        ExCall_1_1(ExVectorToString_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx,
                        ExCall_2_1(ExVectorToString_2_1, ctx, argc, stack));
            break;
        case 3:
            RNFUNC_CALL(ctx,
                        ExCall_3_1(ExVectorToString_3_1, ctx, argc, stack));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
list_to_vector(RnCtx* ctx, Value* out, Value* lis, size_t skip, size_t limit){
    RNFUNC_BEGIN;
    Value input;
    Value tmp;
    ObjString* str;
    ValueContainer v;
    size_t i;
    int c;
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
        RNFUNC_CALL(ctx, RnRibRef(ctx, lis, lis, 1));
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
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &tmp, 1));
        }else{
            abort();
        }
        i++;
    }

    /* Pass2: Construct string */
    s = (char*)malloc(siz + 1);
    if(! s){
        RnLowMemory();
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
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &tmp, 1));
        }else{
            abort();
        }
        i++;
    }

    str = (ObjString*)malloc(sizeof(ObjString));
    if(! str){
        RnLowMemory();
    }
    str->str = (const char*)s;
    str->refcnt = 0;
    str->len = siz;
    v.as_string = str;
    RnValueRef(ctx, out, v, VT_STRING);
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &input);
    RNFUNC_END;
}

static RnResult
ExListToString_3_1(RnCtx* ctx, Value* out, Value* lis, 
                   Value* start, Value* end){
    RNFUNC_BEGIN;
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    if(start->value.as_int64 < 0){
        abort();
    }
    if(end->value.as_int64 < 0){
        abort();
    }
    RNFUNC_CALL(ctx, list_to_vector(ctx, out, lis, start->value.as_int64,
                                    end->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExListToString_2_1(RnCtx* ctx, Value* out, Value* lis, Value* start){
    RNFUNC_BEGIN;
    if(start->type != VT_INT64){
        abort();
    }
    if(start->value.as_int64 < 0){
        abort();
    }
    RNFUNC_CALL(ctx, list_to_vector(ctx, out, lis, start->value.as_int64, -1));
    RNFUNC_END;
}

static RnResult
ExListToString_1_1(RnCtx* ctx, Value* out, Value* lis){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, list_to_vector(ctx, out, lis, 0, -1));
    RNFUNC_END;
}

static RnResult
ExListToString(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, ExCall_1_1(ExListToString_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx, ExCall_2_1(ExListToString_2_1, ctx, argc, stack));
            break;
        case 3:
            RNFUNC_CALL(ctx, ExCall_3_1(ExListToString_3_1, ctx, argc, stack));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
number_to_string(RnCtx* ctx, Value* out, Value* z, int radix){
    RNFUNC_BEGIN;
    char buf[128];
    switch(z->type){
        case VT_INT64:
            switch(radix){
                case 10:
                    snprintf(buf, sizeof(buf), "%" PRId64, z->value.as_int64);
                    break;
                case 16:
                    snprintf(buf, sizeof(buf), "%" PRIx64, z->value.as_int64);
                    break;
                case 8:
                    snprintf(buf, sizeof(buf), "%" PRIo64, z->value.as_int64);
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
    RNFUNC_CALL(ctx, RnString(ctx, out, buf, strnlen(buf, sizeof(buf))));
    RNFUNC_END;
}

static RnResult
ExNumberToString_2_1(RnCtx* ctx, Value* out, Value* z, Value* radix){
    RNFUNC_BEGIN;
    if(radix->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, number_to_string(ctx, out, z, (int)radix->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExNumberToString_1_1(RnCtx* ctx, Value* out, Value* z){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, number_to_string(ctx, out, z, 10));
    RNFUNC_END;
}

static RnResult
ExNumberToString(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, 
                        ExCall_1_1(ExNumberToString_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx,
                        ExCall_2_1(ExNumberToString_2_1, ctx, argc, stack));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
string_to_number(RnCtx* ctx, Value* out, Value* s, int radix){
    RNFUNC_BEGIN;
    // FIXME: Implement this seriously
    size_t i;
    ObjString* str;
    int is_inexact;
    double d;
    int64_t reg;
    const char* t;
    if(s->type != VT_STRING){
        abort();
    }
    str = s->value.as_string;
    t = str->str;
    /* Check for explicit radix */
    if(str->len > 2){
        if(t[0] == '#'){
            switch(t[1]){
                case 'x':
                    radix = 16;
                    t = &t[2];
                    break;
                case 'b':
                    radix = 2;
                    t = &t[2];
                    break;
                case 'd':
                    radix = 10;
                    t = &t[2];
                    break;
                case 'o':
                    radix = 8;
                    t = &t[2];
                    break;
                default:
                    break;
            }
        }
    }
    switch(radix){
        case 2:
        case 8:
        case 16:
            reg = strtoimax(t, NULL, radix);
            RNFUNC_CALL(ctx, RnInt64(ctx, out, reg));
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
                RNFUNC_CALL(ctx, RnInt64(ctx, out, (int64_t)d));
            }else{
                RNFUNC_CALL(ctx, RnDouble(ctx, out, d));
            }
            break;
    }
    RNFUNC_END;
}

static RnResult
ExStringToNumber_2_1(RnCtx* ctx, Value* out, Value* s, Value* radix){
    RNFUNC_BEGIN;
    if(radix->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, string_to_number(ctx, out, s, (int)radix->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExStringToNumber_1_1(RnCtx* ctx, Value* out, Value* s){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, string_to_number(ctx, out, s, 10));
    RNFUNC_END;
}

static RnResult
ExStringToNumber(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, 
                        ExCall_1_1(ExStringToNumber_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx,
                        ExCall_2_1(ExStringToNumber_2_1, ctx, argc, stack));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
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

static RnResult
ExStringToUtf8_3_1(RnCtx* ctx, Value* out, Value* s, Value* start, Value* end){
    RNFUNC_BEGIN;
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
    if(start->value.as_int64 < 0){
        abort();
    }
    if(end->value.as_int64 < 0){
        abort();
    }
    if(start->value.as_int64 > end->value.as_int64){
        abort();
    }

    term = s->value.as_string->str + s->value.as_string->len;
    begin = utf8_skip(s->value.as_string->str, term, start->value.as_int64);
    tail = utf8_skip(begin, term, end->value.as_int64 - start->value.as_int64);
    outlen = tail - begin;
    RNFUNC_CALL(ctx, RnBytevector(ctx, out, outlen));
    memcpy(out->value.as_bytevector->buf, begin, outlen);
    RNFUNC_END;
}

static RnResult
ExStringToUtf8_2_1(RnCtx* ctx, Value* out, Value* s, Value* start){
    RNFUNC_BEGIN;
    const char* begin;
    const char* end;
    size_t outlen;
    if(s->type != VT_STRING){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    if(start->value.as_int64 < 0){
        abort();
    }

    end = s->value.as_string->str + s->value.as_string->len;
    begin = utf8_skip(s->value.as_string->str, end, start->value.as_int64);
    outlen = end - begin;
    RNFUNC_CALL(ctx, RnBytevector(ctx, out, outlen));
    memcpy(out->value.as_bytevector->buf, begin, outlen);
    RNFUNC_END;
}

static RnResult
ExStringToUtf8_1_1(RnCtx* ctx, Value* out, Value* s){
    RNFUNC_BEGIN;
    if(s->type != VT_STRING){
        abort();
    }
    RNFUNC_CALL(ctx, RnBytevector(ctx, out, s->value.as_string->len));
    memcpy(out->value.as_bytevector->buf,
           s->value.as_string->str,
           s->value.as_string->len);
    RNFUNC_END;
}

static RnResult
ExStringToUtf8(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, ExCall_1_1(ExStringToUtf8_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx, ExCall_2_1(ExStringToUtf8_2_1, ctx, argc, stack));
            break;
        case 3:
            RNFUNC_CALL(ctx, ExCall_3_1(ExStringToUtf8_3_1, ctx, argc, stack));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

/* $error/core (VM) */
static RnResult
ExErrorCore(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    int i;
    Value tmp, tmp2;
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &tmp2);
    debugprintf("Error:\n");
    if(argc<0){
        argc=0;
    }
    if(stack->type == VT_RIB){
        RnValueRef(ctx, &tmp, stack->value, stack->type);
        for(i=0;i!=argc;i++){
            debugprintf("  %d: ", i);
            if(tmp.type != VT_RIB){
                debugprintf("BROKEN\n");
                break;
            }
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp2, &tmp, 0));
            emergency_print(ctx, &tmp2);
            debugprintf("\n");
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &tmp, 1));
        }
    }else{
        debugprintf("BROKEN STACK!\n");
        emergency_print(ctx, stack);
    }
    exit(1);
    RNFUNC_END;
}



/* (+ ...) => num */
/* (- ...) => num */
/* (* ...) => num */
/* (= ...) => <BOOL> */
/* (< ...) => <BOOL> */
/* (<= ...) => <BOOL> */
/* (> ...) => <BOOL> */
/* (>= ...) => <BOOL> */
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
/* ($fx+ x y) => num */

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
    x("$fl-loge", ExFlLoge, _1_1) \
    x("$fl-log", ExFlLog, _N) \
    x("$fl-sqrt", ExFlSqrt, _1_1) \
    x("$fl-floor/", ExFlFloorDiv, _2_2) \
    x("$fl-truncate/", ExFlTruncateDiv, _2_2) \
    x("$fx+", ExFxAdd, _2_1) \
    x("$fx-", ExFxSub, _2_1) \
    x("$fx=", ExFxEq, _2_1) \
    x("$fx<", ExFxLt, _2_1) \
    x("$fx<=", ExFxLtEq, _2_1) \
    x("$fx>", ExFxGt, _2_1) \
    x("$fx>=", ExFxGtEq, _2_1)

static RnResult
ExFxGtEq(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, 
                             x->value.as_int64 >= y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExFxGt(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->value.as_int64 > y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExFxLtEq(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->value.as_int64 <= y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExFxLt(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->value.as_int64 < y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExFxEq(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, x->value.as_int64 == y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExFxSub(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, RnInt64(ctx, out, x->value.as_int64 - y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExFxAdd(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, RnInt64(ctx, out, x->value.as_int64 + y->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExAdd(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
        if(out.type == VT_INT64){
            i64 = out.value.as_int64;
            if(tmp.type == VT_INT64){
                ii64 = tmp.value.as_int64;
                v.as_int64 = i64 + ii64;
                RnValueRef(ctx, &out, v, VT_INT64);
            }else if(tmp.type == VT_DOUBLE){
                d64 = (double)i64;
                dd64 = tmp.value.as_double;
                v.as_double = d64 + dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else{
                abort();
            }
        }else if(out.type == VT_DOUBLE){
            d64 = out.value.as_double;
            if(tmp.type == VT_INT64){
                dd64 = (double)tmp.value.as_int64;
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
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
sub_itr(RnCtx* ctx, int rest, Value* out, Value* stack){
    RNFUNC_BEGIN;
    Value acc;
    if(rest == 0){
        RNFUNC_CALL(ctx, RnRibRef(ctx, out, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    }else{
        RnValueLink(ctx, &acc);
        RNFUNC_CALL(ctx, RnRibRef(ctx, &acc, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        RNFUNC_CALL(ctx, sub_itr(ctx, rest - 1, out, stack));
        if(out->type == VT_INT64){
            if(acc.type == VT_INT64){
                RNFUNC_CALL(ctx, 
                            RnInt64(ctx, out, 
                                    out->value.as_int64 - acc.value.as_int64));
            }else if(acc.type == VT_DOUBLE){
                RNFUNC_CALL(ctx, RnDouble(ctx, out, 
                                          (double)out->value.as_int64
                                          - acc.value.as_double));
            }else{
                abort();
            }
        }else if(out->type == VT_DOUBLE){
            if(acc.type == VT_INT64){
                RNFUNC_CALL(ctx, RnDouble(ctx, out, out->value.as_double
                                          - (double)acc.value.as_int64));
            }else if(acc.type == VT_DOUBLE){
                RNFUNC_CALL(ctx, RnDouble(ctx, out, out->value.as_double
                                          - acc.value.as_double));
            }else{
                abort();
            }
        }else{
            abort();
        }
        RnValueUnlink(ctx, &acc);
    }
    RNFUNC_END;
}

static RnResult
ExSub(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value out;
    ValueContainer v;
    RnValueLink(ctx, &out);
    if(argc == 0){
        abort();
    }else if(argc == 1){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &out, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
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
        RNFUNC_CALL(ctx, sub_itr(ctx, argc - 1, &out, stack));
    }
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RNFUNC_END;
}

static RnResult
ExMul(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
        if(out.type == VT_INT64){
            i64 = out.value.as_int64;
            if(tmp.type == VT_INT64){
                ii64 = tmp.value.as_int64;
                v.as_int64 = i64 * ii64;
                RnValueRef(ctx, &out, v, VT_INT64);
            }else if(tmp.type == VT_DOUBLE){
                d64 = (double)i64;
                dd64 = tmp.value.as_double;
                v.as_double = d64 * dd64;
                RnValueRef(ctx, &out, v, VT_DOUBLE);
            }else{
                abort();
            }
        }else if(out.type == VT_DOUBLE){
            d64 = out.value.as_double;
            if(tmp.type == VT_INT64){
                dd64 = (double)tmp.value.as_int64;
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
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExEq(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    argc--;
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
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
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, &out, r));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExLt(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    argc--;
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
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
                v.as_double = (double)tmp.value.as_int64;
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
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, &out, r));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExLtEq(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    argc--;
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
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
                v.as_double = (double)tmp.value.as_int64;
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
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, &out, r));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExGt(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    argc--;
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
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
                v.as_double = (double)tmp.value.as_int64;
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
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, &out, r));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExGtEq(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
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
    RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
    argc--;
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
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
                v.as_double = (double)tmp.value.as_int64;
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
    while(argc){
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        argc--;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, &out, r));
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &out, stack));
    RnValueUnlink(ctx, &out);
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}


static RnResult
ExFxDiv(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    ValueContainer v;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    v.as_int64 = x->value.as_int64 / y->value.as_int64;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExFlDiv(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    ValueContainer v;
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    v.as_double = x->value.as_double / y->value.as_double;
    RnValueRef(ctx, out, v, VT_DOUBLE);
    RNFUNC_END;
}

static RnResult
ExFxToFl(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    // FIXME: Allows double
    if(x->type == VT_DOUBLE){
        RNFUNC_CALL(ctx, RnDouble(ctx, out, x->value.as_double));
    }else{
        if(x->type != VT_INT64){
            abort();
        }
        RNFUNC_CALL(ctx, RnDouble(ctx, out, (double)x->value.as_int64));
    }
    RNFUNC_END;
}

static RnResult
ExFxExpt(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    // FIXME: Implement this
    double a,b;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }

    a = (double)x->value.as_int64;
    b = (double)y->value.as_int64;
    RNFUNC_CALL(ctx, RnInt64(ctx, out, (int64_t)pow(a,b)));
    RNFUNC_END;
}

static RnResult
ExFxFloorDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    RNFUNC_BEGIN;
    int64_t v1,v2;
    int64_t r1,r2;
    if(x->type != VT_INT64){
        abort();
    }
    if(y->type != VT_INT64){
        abort();
    }
    v1 = x->value.as_int64;
    v2 = y->value.as_int64;
    if((v1 >= 0 && v2 >= 0) || (v1 < 0 && v2 < 0)){
        r1 = v1 / v2;
        r2 = v1 % v2;
    }else{
        /* Calc modulus first, adjust its sign */
        r2 = v1 % v2;
        if(r2 < 0){
            if(v2 >= 0){
                r2 = r2 * -1;
            }
        }else{
            if(v2 < 0){
                r2 = r2 * -1;
            }
        }
        /* Adjust v1 and calc truncate-quotient against it */
        v1 = v1 - r2;
        r1 = v1 / v2;
    }
    RNFUNC_CALL(ctx, RnInt64(ctx, out1, r1));
    RNFUNC_CALL(ctx, RnInt64(ctx, out2, r2));
    RNFUNC_END;
}

static RnResult
ExFxTruncateDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnInt64(ctx, out1, r1));
    RNFUNC_CALL(ctx, RnInt64(ctx, out2, r2));
    RNFUNC_END;
}

static RnResult
ExFlNanP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    // FIXME: Allows int64
    if(x->type == VT_INT64){
        RNFUNC_CALL(ctx, to_bool(ctx, out, 0));
    }else{
        if(x->type != VT_DOUBLE){
            abort();
        }
        RNFUNC_CALL(ctx, to_bool(ctx, out, isnan(x->value.as_double)));
    }
    RNFUNC_END;
}

static RnResult
ExFlFiniteP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    // FIXME: Allows int64
    if(x->type == VT_INT64){
        RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    }else{
        if(x->type != VT_DOUBLE){
            abort();
        }
        RNFUNC_CALL(ctx, to_bool(ctx, out, isfinite(x->value.as_double)));
    }
    RNFUNC_END;
}

static RnResult
ExFlInfiniteP(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    // FIXME: Allows int64
    if(x->type == VT_INT64){
        RNFUNC_CALL(ctx, to_bool(ctx, out, 0));
    }else{
        if(x->type != VT_DOUBLE){
            abort();
        }
        RNFUNC_CALL(ctx, to_bool(ctx, out, ! isfinite(x->value.as_double)));
    }
    RNFUNC_END;
}

static RnResult
ExFlToFx(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    // FIXME: Allow exact since it should work as `exact` as well..
    if(x->type == VT_INT64){
        RNFUNC_CALL(ctx, RnInt64(ctx, out, x->value.as_int64));
    }else{
        if(x->type != VT_DOUBLE){
            abort();
        }
        RNFUNC_CALL(ctx, RnInt64(ctx, out, (int64_t)x->value.as_double));
    }
    RNFUNC_END;
}

static RnResult
ExFlExpt(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, 
                              pow(x->value.as_double, y->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlFloor(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, floor(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlCeiling(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, ceil(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlTruncate(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, trunc(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlRound(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, round(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlAcos(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, acos(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlAsin(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, asin(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlAtan(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, atan(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlAtan2(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, 
                              atan2(x->value.as_double, y->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlCos(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, cos(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlSin(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, sin(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlTan(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, tan(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlExp(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, exp(x->value.as_double)));
    RNFUNC_END;
}


static RnResult
ExFlLog_2_1(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    if(y->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, log(x->value.as_double) 
                              / log(y->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlLog_1_1(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, log(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlLog(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    switch(argc){
        case 1:
            RNFUNC_CALL(ctx, ExCall_1_1(ExFlLog_1_1, ctx, argc, stack));
            break;
        case 2:
            RNFUNC_CALL(ctx, ExCall_2_1(ExFlLog_2_1, ctx, argc, stack));
            break;
        default:
            abort();
    }
    RNFUNC_END;
}

static RnResult
ExFlLoge(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    // FIXME: Tentative.
    RNFUNC_CALL(ctx, ExFlLog_1_1(ctx, out, x));
    RNFUNC_END;
}

static RnResult
ExFlSqrt(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_DOUBLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnDouble(ctx, out, sqrt(x->value.as_double)));
    RNFUNC_END;
}

static RnResult
ExFlFloorDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnDouble(ctx, out1, r1));
    RNFUNC_CALL(ctx, RnDouble(ctx, out2, r2));
    RNFUNC_END;
}

static RnResult
ExFlTruncateDiv(RnCtx* ctx, Value* out1, Value* out2, Value* x, Value* y){
    RNFUNC_BEGIN;
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
    RNFUNC_CALL(ctx, RnDouble(ctx, out1, r1));
    RNFUNC_CALL(ctx, RnDouble(ctx, out2, r2));
    RNFUNC_END;
}



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

#define EXLIB_FILE(x) \
    x("file-exists?", ExFileExistsP, _1_1) \
    x("delete-file", ExDeleteFile, _1_1) \
    x("filehandle-open/input", ExFilehandleOpenInput, _1_1) \
    x("filehandle-open/output", ExFilehandleOpenOutput, _1_1) \
    x("filehandle-close", ExFilehandleClose, _1_1) \
    x("filehandle-read!", ExFilehandleReadEx, _4_1) \
    x("filehandle-write", ExFilehandleWrite, _4_1) \
    x("filehandle-flush", ExFilehandleFlush, _1_1) \
    x("filehandle-stdin", ExFilehandleStdin, _0_1) \
    x("filehandle-stdout", ExFilehandleStdout, _0_1) \
    x("filehandle-stderr", ExFilehandleStderr, _0_1)

static RnResult
ExFileExistsP(RnCtx* ctx, Value* out, Value* str){
    RNFUNC_BEGIN;
    void* handle;
    int r;
    if(str->type != VT_STRING){
        abort();
    }
    handle = BsFileOpenForRead(str->value.as_string->str);

    if(handle){
        r = 1;
        BsFileClose(handle);
    }else{
        r = 0;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, r));
    RNFUNC_END;
}

static RnResult
ExDeleteFile(RnCtx* ctx, Value* out, Value* str){
    RNFUNC_BEGIN;
    // FIXME: Implement this
    (void)ctx;
    (void)out;
    (void)str;

    abort();
    RNFUNC_END;
}

static RnResult
ExFilehandleOpenInput(RnCtx* ctx, Value* out, Value* str){
    RNFUNC_BEGIN;
    // FIXME: Provide some destructor
    void* handle;
    if(str->type != VT_STRING){
        abort();
    }
    handle = BsFileOpenForRead(str->value.as_string->str);
    if(handle){
        RNFUNC_CALL(ctx, RnInt64(ctx, out, (uintptr_t)handle));
    }else{
        RNFUNC_CALL(ctx, to_bool(ctx, out, 0));
    }
    RNFUNC_END;
}

static RnResult
ExFilehandleOpenOutput(RnCtx* ctx, Value* out, Value* str){
    RNFUNC_BEGIN;
    // FIXME: Provide some destructor
    void* handle;
    if(str->type != VT_STRING){
        abort();
    }
    handle = BsFileOpenForReadWrite(str->value.as_string->str);
    if(handle){
        RNFUNC_CALL(ctx, RnInt64(ctx, out, (uintptr_t)handle));
    }else{
        RNFUNC_CALL(ctx, to_bool(ctx, out, 0));
    }
    RNFUNC_END;
}

static RnResult
ExFilehandleClose(RnCtx* ctx, Value* out, Value* fh){
    RNFUNC_BEGIN;
    (void)ctx;
    if(fh->type != VT_INT64){
        abort();
    }
    BsFileClose((void*)(uintptr_t)fh->value.as_int64);
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExFilehandleReadEx(RnCtx* ctx, Value* out, Value* fh, Value* bv, Value* offs,
                   Value* len){
    RNFUNC_BEGIN;
    void* handle;
    size_t reqlen;
    size_t readlen;
    size_t poffs;
    if(fh->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(offs->type != VT_INT64){
        abort();
    }
    if(len->type != VT_INT64){
        abort();
    }
    reqlen = len->value.as_int64;
    poffs = offs->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(bv->value.as_bytevector->len < reqlen + poffs){
            abort();
        }
    }

    handle = (FILE*)(uintptr_t)fh->value.as_int64;
    (void)BsFileRead(handle, bv->value.as_bytevector->buf + poffs, reqlen,
                     &readlen);
    RNFUNC_CALL(ctx, RnInt64(ctx, out, readlen));
    RNFUNC_END;
}

static RnResult
ExFilehandleWrite(RnCtx* ctx, Value* out, Value* fh, Value* bv, Value* offs,
                  Value* len){
    RNFUNC_BEGIN;
    void* handle;
    size_t reqlen;
    size_t writelen;
    size_t poffs;
    if(fh->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(offs->type != VT_INT64){
        abort();
    }
    if(len->type != VT_INT64){
        abort();
    }
    reqlen = len->value.as_int64;
    poffs = offs->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(bv->value.as_bytevector->len < reqlen + poffs){
            abort();
        }
    }

    handle = (void*)(uintptr_t)fh->value.as_int64;
    (void)BsFileWrite(handle, bv->value.as_bytevector->buf + poffs, reqlen,
                      &writelen);
    RNFUNC_CALL(ctx, RnInt64(ctx, out, writelen));
    RNFUNC_END;
}

static RnResult
ExFilehandleFlush(RnCtx* ctx, Value* out, Value* fh){
    RNFUNC_BEGIN;
    void* handle;
    if(fh->type != VT_INT64){
        abort();
    }
    handle = (void*)(uintptr_t)fh->value.as_int64;
    BsFileFlush(handle);
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExFilehandleStdin(RnCtx* ctx, Value* out){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnInt64(ctx, out, (uintptr_t)BsFileGetStdin()));
    RNFUNC_END;
}

static RnResult
ExFilehandleStdout(RnCtx* ctx, Value* out){
    RNFUNC_BEGIN
    RNFUNC_CALL(ctx, RnInt64(ctx, out, (uintptr_t)BsFileGetStdout()));
    RNFUNC_END;
}

static RnResult
ExFilehandleStderr(RnCtx* ctx, Value* out){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnInt64(ctx, out, (uintptr_t)BsFileGetStderr()));
    RNFUNC_END;
}

/* (vec-copy v start end) => vec */
/* (vec-copy! tgt loc src start end) => bogus */
/* (vec-ref vec idx) => obj */
/* (vec-set! vec idx obj) => bogus */
/* (vec-new tag k) => vec */
/* (vec-length vec) => num */
/* (vec-fill! vec obj from to) => bogus */
/* (vec= v1 v2) => <BOOL> */
/* (vec-append ...) => vec */
/* (vec-subvec vec start end) => vec */ // FIXME: Remove this

#define EXLIB_VEC(x) \
    x("vec-copy", ExVecCopy, _3_1) \
    x("vec-copy!", ExVecCopyEx, _5_1) \
    x("vec-ref", ExVecRef, _2_1) \
    x("vec-set!", ExVecSetEx, _3_1) \
    x("vec-new", ExVecNew, _2_1) \
    x("vec-length", ExVecLength, _1_1) \
    x("vec-fill!", ExVecFillEx, _4_1) \
    x("vec=", ExVecEq, _2_1) \
    x("vec-append", ExVecAppend, _N) \
    x("vec-subvec", ExVecSubvec, _3_1) // FIXME: Remove this

static RnResult
sub_string(RnCtx* ctx, Value* out, ObjString* str, size_t start, size_t end){
    RNFUNC_BEGIN;
    const char* begin;
    const char* tail;
    const char* term;

    term = str->str + str->len;
    begin = utf8_skip(str->str, term, start);
    tail = utf8_skip(begin, term, end - start);
    RNFUNC_CALL(ctx, RnString(ctx, out, begin, tail - begin));
    RNFUNC_END;
}

static RnResult
ExVecCopy(RnCtx* ctx, Value* out, Value* v, Value* start, Value* end){
    RNFUNC_BEGIN;
    size_t s;
    size_t e;
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    s = start->value.as_int64;
    e = end->value.as_int64;

    switch(v->type){
        case VT_STRING:
            RNFUNC_CALL(ctx, sub_string(ctx, out, v->value.as_string, s, e));
            break;
        case VT_BYTEVECTOR:
            RNFUNC_CALL(ctx, RnBytevector(ctx, out, e - s));
            memcpy(out->value.as_bytevector->buf, 
                   v->value.as_bytevector->buf + s,
                   e - s);
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
ExVecCopyEx(RnCtx* ctx, Value* out, Value* tgt, Value* loc, 
            Value* src, Value* start, Value* end){
    RNFUNC_BEGIN;
    size_t i,l,s,e;
    uint8_t* f;
    uint8_t* t;
    Value tmp;
    if(tgt->type != src->type){
        abort();
    }
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    if(loc->type != VT_INT64){
        abort();
    }
    l = loc->value.as_int64;
    s = start->value.as_int64;
    e = end->value.as_int64;
    switch(tgt->type){
        case VT_VECTOR:
            RnValueLink(ctx, &tmp);
            for(i=0;i!=(e - s);i++){
                RNFUNC_CALL(ctx, RnVectorRef(ctx, &tmp, src, s + i));
                RNFUNC_CALL(ctx, RnVectorSet(ctx, tgt, &tmp, l + i));
            }
            RnValueUnlink(ctx, &tmp);
            break;
        case VT_BYTEVECTOR:
            if(! RnBytevectorIsUnlimitedRange(src->value.as_bytevector)){
                if(e > src->value.as_bytevector->len){
                    abort();
                }
            }
            if(! RnBytevectorIsUnlimitedRange(tgt->value.as_bytevector)){
                if((l + (e - s)) > tgt->value.as_bytevector->len){
                    abort();
                }
            }
            f = src->value.as_bytevector->buf + s;
            t = tgt->value.as_bytevector->buf + l;
            memmove(t, f, (e - s));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExVecRef(RnCtx* ctx, Value* out, Value* vec, Value* idx){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    const char* ref;
    const char* s;
    const char* t;
    if(idx->type != VT_INT64){
        abort();
    }
    if(vec->type == VT_SIMPLE_STRUCT){
        if(idx->value.as_int64 < -1){
            abort();
        }
    }else{
        if(idx->value.as_int64 < 0){
            abort();
        }
    }
    e = idx->value.as_int64;
    switch(vec->type){
        case VT_STRING:
            s = vec->value.as_string->str;
            t = vec->value.as_string->str + vec->value.as_string->len;
            ref = utf8_skip(s, t, e);
            if(ref){
                v.as_char = utf8_decode(ref, t);
                RnValueRef(ctx, out, v, VT_CHAR);
            }else{
                abort();
            }
            break;
        case VT_SIMPLE_STRUCT:
            RNFUNC_CALL(ctx, RnVectorRef(ctx, out, vec, e + 1));
            break;
        case VT_VECTOR:
            RNFUNC_CALL(ctx, RnVectorRef(ctx, out, vec, e));
            break;
        case VT_BYTEVECTOR:
            if(! RnBytevectorIsUnlimitedRange(vec->value.as_bytevector)){
                if(e > vec->value.as_bytevector->len){
                    abort();
                }
            }
            v.as_int64 = vec->value.as_bytevector->buf[e];
            RnValueRef(ctx, out, v, VT_INT64);
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
ExVecSetEx(RnCtx* ctx, Value* out, Value* vec, Value* idx, Value* obj){
    RNFUNC_BEGIN;
    size_t e;
    if(idx->type != VT_INT64){
        abort();
    }
    if(vec->type == VT_SIMPLE_STRUCT){
        if(idx->value.as_int64 < -1){
            abort();
        }
    }else{
        if(idx->value.as_int64 < 0){
            abort();
        }
    }
    e = idx->value.as_int64;
    switch(vec->type){
        case VT_SIMPLE_STRUCT:
            RNFUNC_CALL(ctx, RnVectorSet(ctx, vec, obj, e + 1));
            break;
        case VT_VECTOR:
            RNFUNC_CALL(ctx, RnVectorSet(ctx, vec, obj, e));
            break;
        case VT_BYTEVECTOR:
            if(obj->type != VT_INT64){
                abort();
            }
            if(obj->value.as_int64 < 0){
                abort();
            }
            if(obj->value.as_int64 > 256){
                abort();
            }
            if(! RnBytevectorIsUnlimitedRange(vec->value.as_bytevector)){
                if(e > vec->value.as_bytevector->len){
                    abort();
                }
            }
            vec->value.as_bytevector->buf[e] = 
                (unsigned char)obj->value.as_int64;
            break;
        default:
            abort();
            break;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExVecNew(RnCtx* ctx, Value* out, Value* tag, Value* k){
    RNFUNC_BEGIN;
    uint64_t siz;
    if(tag->type != VT_INT64){
        abort();
    }
    if(k->type != VT_INT64){
        abort();
    }
    siz = k->value.as_int64;

    switch(tag->value.as_int64){
        case 3:
            if(siz == 0){
                RNFUNC_CALL(ctx, RnString(ctx, out, "", 0));
            }else{
                abort();
            }
            break;
        case 4:
            RNFUNC_CALL(ctx, RnVector(ctx, out, siz));
            break;
        case 9:
            RNFUNC_CALL(ctx, RnVector(ctx, out, siz + 1));
            out->type = VT_SIMPLE_STRUCT;
            break;
        case 8:
            RNFUNC_CALL(ctx, RnBytevector(ctx, out, siz));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
ExVecLength(RnCtx* ctx, Value* out, Value* vec){
    RNFUNC_BEGIN;
    size_t len;
    const char* begin;
    const char* end;
    const char* cur;
    switch(vec->type){
        case VT_STRING:
            len = 0;
            begin = vec->value.as_string->str;
            end = vec->value.as_string->str + vec->value.as_string->len;
            cur = begin;
            while(cur != end){
                cur = utf8_skip(cur, end, 1);
                len++;
            }
            RNFUNC_CALL(ctx, RnInt64(ctx, out, len));
            break;
        case VT_VECTOR:
            RNFUNC_CALL(ctx, RnInt64(ctx, out, vec->value.as_vector->length));
            break;
        case VT_BYTEVECTOR:
            RNFUNC_CALL(ctx, RnInt64(ctx, out, vec->value.as_bytevector->len));
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}

static RnResult
ExVecFillEx(RnCtx* ctx, Value* out, Value* vec, Value* obj, 
            Value* from, Value* to){
    RNFUNC_BEGIN;
    size_t f, t, i;
    if(from->type != VT_INT64){
        abort();
    }
    if(from->value.as_int64 < 0){
        abort();
    }
    if(to->type != VT_INT64){
        abort();
    }
    if(to->value.as_int64 < 0){
        abort();
    }
    f = from->value.as_int64;
    t = to->value.as_int64;
    if(f > t){
        abort();
    }
    switch(vec->type){
        case VT_VECTOR:
            for(i = f; i != t; i++){
                RNFUNC_CALL(ctx, RnVectorSet(ctx, vec, obj, i));
            }
            break;
        case VT_BYTEVECTOR:
            if(obj->type != VT_INT64){
                abort();
            }
            if(obj->value.as_int64 < 0){
                abort();
            }
            if(obj->value.as_int64 > 256){
                abort();
            }
            if(! RnBytevectorIsUnlimitedRange(vec->value.as_bytevector)){
                if(t > vec->value.as_bytevector->len){
                    abort();
                }
            }
            for(i = f; i != t; i++){
                vec->value.as_bytevector->buf[i] = 
                    (unsigned char)obj->value.as_int64;
            }
            break;
        default:
            abort();
            break;
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExVecEq(RnCtx* ctx, Value* out, Value* x, Value* y){
    RNFUNC_BEGIN;
    if(x->type != y->type){
        abort();
    }
    if(x->type != VT_STRING){
        abort();
    }
    if(x->value.as_string == y->value.as_string){
        RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
        goto finish;
    }
    if(x->value.as_string->len == y->value.as_string->len){
        if(0 == memcmp(x->value.as_string->str,
                       y->value.as_string->str,
                       x->value.as_string->len)){
            RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
            goto finish;
        }
    }
    RNFUNC_CALL(ctx, to_bool(ctx, out, 0));
finish:
    RNFUNC_END;
}

static RnResult
ExVecAppend(RnCtx* ctx, int argc, Value* stack){
    RNFUNC_BEGIN;
    Value tmp;
    Value tmp2;
    char* s;
    size_t total;
    size_t loc;
    int i;
    RnValueLink(ctx, &tmp2);
    RnValueLink(ctx, &tmp);

    /* Pass1: Calc total bytesize */
    total = 0;
    RnValueRef(ctx, &tmp, stack->value, stack->type);
    for(i = 0; i != argc; i++){
        RnRibRef(ctx, &tmp2, &tmp, 0);
        RnRibRef(ctx, &tmp, &tmp, 1);
        if(tmp2.type != VT_STRING){
            abort();
        }
        total += tmp2.value.as_string->len;
    }

    s = (char*)malloc(total);
    if(! s){
        RnLowMemory();
    }

    /* Pass2: Pop args and generate total string */
    loc = total;
    for(i = 0; i != argc; i++){
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, stack, 0));
        RNFUNC_CALL(ctx, RnRibRef(ctx, stack, stack, 1));
        loc -= tmp.value.as_string->len;
        memcpy(&s[loc], tmp.value.as_string->str, tmp.value.as_string->len);
    }
    RNFUNC_CALL(ctx, RnString(ctx, &tmp, s, total));
    free(s);
    RNFUNC_CALL(ctx, RnCons(ctx, stack, &tmp, stack));
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &tmp2);
    RNFUNC_END;
}

// FIXME: Remove this
static RnResult
ExVecSubvec(RnCtx* ctx, Value* out, Value* v, Value* start, Value* end){
    RNFUNC_BEGIN;
    size_t s;
    size_t e;
    if(start->type != VT_INT64){
        abort();
    }
    if(end->type != VT_INT64){
        abort();
    }
    s = start->value.as_int64;
    e = end->value.as_int64;

    switch(v->type){
        case VT_STRING:
            RNFUNC_CALL(ctx, sub_string(ctx, out, v->value.as_string, s, e));
            break;
        case VT_BYTEVECTOR:
            RNFUNC_CALL(ctx, RnBytevector(ctx, out, e - s));
            memcpy(out->value.as_bytevector->buf, 
                   v->value.as_bytevector->buf + s,
                   e - s);
            break;
        default:
            abort();
            break;
    }
    RNFUNC_END;
}



/* (ht-new x) => ht */
/* (ht-set! ht key obj) => bogus */
/* (ht-entries ht) => key+value */
/* (ht-ref ht key default) => obj */
/* (ht-keys ht) => key */
/* (ht-size ht) => num */

#define EXLIB_HT(x) \
    x("ht-new", ExHtNew, _1_1) \
    x("hashtable-set!", ExHtSetEx, _3_1) \
    x("hashtable-entries", ExHtEntries, _1_2) \
    x("hashtable-ref", ExHtRef, _3_1) \
    x("hashtable-keys", ExHtKeys, _1_1) \
    x("hashtable-size", ExHtSize, _1_1)

static RnResult
ExHtNew(RnCtx* ctx, Value* out, Value* x){
    RNFUNC_BEGIN;
    if(x->type != VT_INT64){
        abort();
    }
    RNFUNC_CALL(ctx, RnHashtable(ctx, out, (HashtableClass)x->value.as_int64));
    RNFUNC_END;
}

static RnResult
ExHtSetEx(RnCtx* ctx, Value* out, Value* ht, Value* key, Value* obj){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnHashtableSet(ctx, ht, key, obj));
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExHtEntries(RnCtx* ctx, Value* out1, Value* out2, Value* ht){
    RNFUNC_BEGIN;
    size_t k,i,loc;
    Value tmp;
    ValueType t;
    ObjHashtable* hto;
    if(ht->type != VT_HASHTABLE){
        abort();
    }
    RnValueLink(ctx, &tmp);
    hto = ht->value.as_hashtable;
    k = hto->keycount;

    /* Keys */
    RNFUNC_CALL(ctx, RnVector(ctx, out1, k));
    loc = 0;
    for(i=0;i!=ht->value.as_hashtable->containercount;i++){
        if(hto->valuetypes[i] != VT_EMPTY){
            switch(hto->hashtable_type){
                case HT_BLOBKEY:
                    t = VT_STRING;
                    break;
                case HT_INTKEY:
                    t = VT_INT64;
                    break;
                case HT_EQV:
                    t = hto->keytypes[i];
                    break;
                default:
                    abort();
                    break;
            }
            RnValueRef(ctx, &tmp, hto->keys[i], t);
            RNFUNC_CALL(ctx, RnVectorSet(ctx, out1, &tmp, loc));
            loc++;
        }
    }
    if(loc != k){
        abort();
    }

    /* Values */
    RNFUNC_CALL(ctx, RnVector(ctx, out2, k));
    loc = 0;
    for(i=0;i!=ht->value.as_hashtable->containercount;i++){
        if(hto->valuetypes[i] != VT_EMPTY){
            RnValueRef(ctx, &tmp, hto->values[i], hto->valuetypes[i]);
            RNFUNC_CALL(ctx, RnVectorSet(ctx, out2, &tmp, loc));
            loc++;
        }
    }
    if(loc != k){
        abort();
    }
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExHtRef(RnCtx* ctx, Value* out, Value* ht, Value* key, Value* def){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnHashtableRef(ctx, out, ht, key, def));
    RNFUNC_END;
}

static RnResult
ExHtKeys(RnCtx* ctx, Value* out, Value* ht){
    RNFUNC_BEGIN;
    size_t k,i,loc;
    Value tmp;
    ObjHashtable* hto;
    ValueType t;
    if(ht->type != VT_HASHTABLE){
        abort();
    }
    RnValueLink(ctx, &tmp);
    hto = ht->value.as_hashtable;
    k = hto->keycount;
    RNFUNC_CALL(ctx, RnVector(ctx, out, k));
    loc = 0;
    for(i=0;i!=ht->value.as_hashtable->containercount;i++){
        if(hto->valuetypes[i] != VT_EMPTY){
            switch(hto->hashtable_type){
                case HT_BLOBKEY:
                    t = VT_STRING;
                    break;
                case HT_INTKEY:
                    t = VT_INT64;
                    break;
                case HT_EQV:
                    t = hto->keytypes[i];
                    break;
                default:
                    abort();
                    break;
            }
            RnValueRef(ctx, &tmp, hto->keys[i], t);
            RNFUNC_CALL(ctx, RnVectorSet(ctx, out, &tmp, loc));
            loc++;
        }
    }
    if(loc != k){
        abort();
    }
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
ExHtSize(RnCtx* ctx, Value* out, Value* ht){
    RNFUNC_BEGIN;
    if(ht->type != VT_HASHTABLE){
        abort();
    }
    RNFUNC_CALL(ctx, RnInt64(ctx, out, ht->value.as_hashtable->keycount));
    RNFUNC_END;
}

/* FFI Related */
/* (bv-ref/s8 bv byteoff) */
/* (bv-ref/u8 bv byteoff) */
/* (bv-ref/u16 bv byteoff) */
/* (bv-ref/s16 bv byteoff) */
/* (bv-ref/u32 bv byteoff) */
/* (bv-ref/s32 bv byteoff) */
/* (bv-ref/u64 bv byteoff) */
/* (bv-ref/s64 bv byteoff) */
/* (bv-ref/ptr0 bv byteoff) */
/* (bv-set!/s8 bv byteoff v) */
/* (bv-set!/u8 bv byteoff v) */
/* (bv-set!/u16 bv byteoff v) */
/* (bv-set!/s16 bv byteoff v) */
/* (bv-set!/u32 bv byteoff v) */
/* (bv-set!/s32 bv byteoff v) */
/* (bv-set!/u64 bv byteoff v) */
/* (bv-set!/s64 bv byteoff v) */
/* (bv-set!/ptr0 bv byteoff v) */
/* (address->bytevector v len?) */
/* (bytevector->address bv) */
/* (nccc-call ptr in inoff out outoff) */

static RnResult
ExBvRef_s8(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    int8_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int8_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_s16(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    int16_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int16_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_s32(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    int32_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int32_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_s64(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    int64_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int64_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_u8(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    uint8_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint8_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_u16(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    uint16_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint16_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_u32(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    uint32_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint32_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_u64(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    uint64_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint64_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvRef_ptr(RnCtx* ctx, Value* out, Value* bv, Value* off){
    RNFUNC_BEGIN;
    ValueContainer v;
    size_t e;
    uintptr_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uintptr_t*)&bv->value.as_bytevector->buf[e];
    v.as_int64 = *vp;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

static RnResult
ExBvSetEx_s8(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    int8_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int8_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_s16(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    int16_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int16_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_s32(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    int32_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int32_t*)&bv->value.as_bytevector->buf[e];
    *vp = (int32_t)d->value.as_int64; /* FIXME: Test overflow */
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_s64(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    int64_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (int64_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_u8(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    uint8_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint8_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_u16(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    uint16_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint16_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_u32(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    uint32_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint32_t*)&bv->value.as_bytevector->buf[e];
    *vp = (uint32_t) d->value.as_int64; /* FIXME: Test overflow */
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_u64(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    uint64_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uint64_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExBvSetEx_ptr(RnCtx* ctx, Value* out, Value* bv, Value* off, Value* d){
    RNFUNC_BEGIN;
    size_t e;
    uintptr_t* vp;
    if(off->type != VT_INT64){
        abort();
    }
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    if(d->type != VT_INT64){
        abort();
    }
    e = off->value.as_int64;
    if(! RnBytevectorIsUnlimitedRange(bv->value.as_bytevector)){
        if(e > bv->value.as_bytevector->len){
            abort();
        }
    }
    vp = (uintptr_t*)&bv->value.as_bytevector->buf[e];
    *vp = d->value.as_int64;
    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

static RnResult
ExAddressToBytevector(RnCtx* ctx, Value* out, Value* addr, Value* len){
    RNFUNC_BEGIN;
    int has_len;
    size_t len_value;
    void* addr_value;
    if(addr->type != VT_INT64){
        abort();
    }
    if(len->type == VT_INT64){
        len_value = len->value.as_int64;
        has_len = 1;
    }else if(len->type == VT_ZONE0 && len->value.as_zone0 == ZZ_FALSE){
        len_value = 0;
        has_len = 0;
    }else{
        abort();
    }

    addr_value = (void*)(uintptr_t)addr->value.as_int64;

    RNFUNC_CALL(ctx, RnBytevectorExternal(ctx, out, addr_value, has_len,
                                          len_value));
    RNFUNC_END;
}

static RnResult
ExBytevectorToAddress(RnCtx* ctx, Value* out, Value* bv){
    RNFUNC_BEGIN;
    ValueContainer v;
    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    v.as_int64 = (uint64_t)(uintptr_t)bv->value.as_bytevector->buf;
    RnValueRef(ctx, out, v, VT_INT64);
    RNFUNC_END;
}

typedef void (*NcccFunc)(const uint64_t* in, uint64_t* out);

static RnResult
ExNcccCall(RnCtx* ctx, Value* out, Value* funcptr,
           Value* inbv, Value* inoff,
           Value* outbv, Value* outoff){
    RNFUNC_BEGIN;
    NcccFunc func;
    const uint64_t* argin;
    uint64_t* argout;
    /* FIXME: No validation done against bv offsets */
    /* FIXME: Check alignment for in/out buffers */
    if(funcptr->type != VT_INT64){
        abort();
    }
    if(inbv->type != VT_BYTEVECTOR){
        abort();
    }
    if(outbv->type != VT_BYTEVECTOR){
        abort();
    }
    if(inoff->type != VT_INT64){
        abort();
    }
    if(outoff->type != VT_INT64){
        abort();
    }

    func = (NcccFunc) funcptr->value.as_int64;
    argin = (uint64_t*)(inbv->value.as_bytevector->buf + 
                        inoff->value.as_int64);
    argout = (uint64_t*)(outbv->value.as_bytevector->buf + 
                         outoff->value.as_int64);

    /* Call nccc func */
    func(argin, argout);

    RNFUNC_CALL(ctx, to_bool(ctx, out, 1));
    RNFUNC_END;
}

#ifdef __cplusplus
extern "C"
#endif
uintptr_t ncccinteg_get_dispatch(uintptr_t idx);

static RnResult
ExNcccSizeOfPointer(RnCtx* ctx, Value* out){
    RNFUNC_BEGIN;
    RNFUNC_CALL(ctx, RnInt64(ctx, out, sizeof(uintptr_t)));
    RNFUNC_END;
}

static RnResult
ExNcccGetDispatch(RnCtx* ctx, Value* out, Value* idx){
    RNFUNC_BEGIN;
    uintptr_t dispatch;
    if(idx->type != VT_INT64){
        abort();
    }

    dispatch = ncccinteg_get_dispatch(idx->value.as_int64);
    RNFUNC_CALL(ctx, RnInt64(ctx, out, dispatch));
    RNFUNC_END;
}

#define EXLIB_NCCC(x) \
    x("bv-ref/s8", ExBvRef_s8, _2_1) \
    x("bv-ref/u8", ExBvRef_u8, _2_1) \
    x("bv-ref/s16", ExBvRef_s16, _2_1) \
    x("bv-ref/u16", ExBvRef_u16, _2_1) \
    x("bv-ref/s32", ExBvRef_s32, _2_1) \
    x("bv-ref/u32", ExBvRef_u32, _2_1) \
    x("bv-ref/s64", ExBvRef_s64, _2_1) \
    x("bv-ref/u64", ExBvRef_u64, _2_1) \
    x("bv-ref/ptr0", ExBvRef_ptr, _2_1) \
    x("bv-set!/s8", ExBvSetEx_s8, _3_1) \
    x("bv-set!/u8", ExBvSetEx_u8, _3_1) \
    x("bv-set!/s16", ExBvSetEx_s16, _3_1) \
    x("bv-set!/u16", ExBvSetEx_u16, _3_1) \
    x("bv-set!/s32", ExBvSetEx_s32, _3_1) \
    x("bv-set!/u32", ExBvSetEx_u32, _3_1) \
    x("bv-set!/s64", ExBvSetEx_s64, _3_1) \
    x("bv-set!/u64", ExBvSetEx_u64, _3_1) \
    x("bv-set!/ptr0", ExBvSetEx_ptr, _3_1) \
    x("address->bytevector", ExAddressToBytevector, _2_1) \
    x("bytevector->address", ExBytevectorToAddress, _1_1) \
    x("nccc-call0", ExNcccCall, _5_1) \
    x("nccc-sizeof-ptr", ExNcccSizeOfPointer, _0_1) \
    x("nccc-get-dispatch0", ExNcccGetDispatch, _1_1)

#define EXFUN(nam, fn) {nam, sizeof(nam) - 1, fn}

#include "ssplit.inc.h"
#include "reader.inc.h"

#define EXLIB_MINIREAD(x) \
    x("miniread-utf8-read", ExUtf8Read, _1_1)

EXLIB_RIB(GEN_BRIDGE)
EXLIB_MISC(GEN_BRIDGE)
EXLIB_MATH(GEN_BRIDGE)
EXLIB_FILE(GEN_BRIDGE)
EXLIB_VEC(GEN_BRIDGE)
EXLIB_HT(GEN_BRIDGE)
EXLIB_VMSUP(GEN_BRIDGE)
EXLIB_NCCC(GEN_BRIDGE)
EXLIB_MINIREAD(GEN_BRIDGE)

RnVmEx vm_externals[] = {
    EXLIB_RIB(GEN_FILD)
    EXLIB_MISC(GEN_FILD)
    EXLIB_MATH(GEN_FILD)
    EXLIB_FILE(GEN_FILD)
    EXLIB_VEC(GEN_FILD)
    EXLIB_HT(GEN_FILD)
    EXLIB_VMSUP(GEN_FILD)
    EXLIB_NCCC(GEN_FILD)
    EXLIB_MINIREAD(GEN_FILD)
    EXFUN("$error/core", ExErrorCore),
    {0, 0, 0}
};
