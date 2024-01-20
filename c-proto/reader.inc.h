#define MR_SECTOR_SIZE 128

static RnResult
mr_sector_new(RnCtx* ctx, Value* out){
    RNFUNC_BEGIN;
    Value tmp;
    RnValueLink(ctx, &tmp);
    RNFUNC_CALL(ctx, RnBytevector(ctx, &tmp, sizeof(mrtoken) * MR_SECTOR_SIZE));
    RNFUNC_CALL(ctx, RnCons(ctx, out, &tmp, out));
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

enum mr_realize_mode_e {
    MM_FIRST, /* = LIST, but terminted with terminator */
    MM_LIST,
    MM_VECTOR,
    MM_BYTEVECTOR,
    MM_ONE
};

typedef enum mr_realize_mode_e mr_realize_mode;

#define MRSTRCMP(str) \
    strncmp(buf + me->start_index, str, me->end_index - me->start_index)

static RnResult
mr_realize_string(RnCtx* ctx, Value* out, mrtoken* me, const char* buf){
    int i,j;
    int can_fastpath = 1;
    char* tmpbuf;
    RNFUNC_BEGIN;
    /* Pass1: Check for fastpath = no need to conversion */
    for(i=me->start_index;i!=me->end_index;i++){
        if(buf[i] == 92){
            can_fastpath = 0;
            break;
        }
    }
    /* Pass2: fastpath/slowpath */
    if(can_fastpath){
        /* Drop double quote on the both ends */
        RNFUNC_CALL(ctx, RnString(ctx, out, &buf[me->start_index + 1],
                                  me->end_index - me->start_index - 2));
    }else{
        tmpbuf = (char*)malloc(me->end_index - me->start_index);
        j = 0;
        for(i=me->start_index+1;i!=(me->end_index-1);i++){
            if(buf[i] == 92){
                if(i+1 == (me->end_index-1)){
                    abort();
                }
                switch(buf[i+1]){
                    case 97:
                        tmpbuf[j] = 7;
                        break;
                    case 98:
                        tmpbuf[j] = 8;
                        break;
                    case 116:
                        tmpbuf[j] = 9;
                        break;
                    case 110:
                        tmpbuf[j] = 10;
                        break;
                    case 100:
                        tmpbuf[j] = 0xd; /* ??? */
                        break;
                    case 34:
                        tmpbuf[j] = 34;
                        break;
                    case 92:
                        tmpbuf[j] = 92;
                        break;
                    default:
                        abort();
                        break;
                }
                i++;
                j++;
            }else{
                tmpbuf[j] = buf[i];
                j++;
            }
        }
        /* FIXME: Can cause leak */
        RNFUNC_CALL(ctx, RnString(ctx, out, tmpbuf, j));
        free(tmpbuf);
    }
    RNFUNC_END;
}

static RnResult
mr_realize_char(RnCtx* ctx, Value* out, mrtoken* me, const char* buf){
    RNFUNC_BEGIN;
    int c;
    if((me->end_index - me->start_index) == 3){
        c = buf[me->start_index + 2];
    }else if(!MRSTRCMP("#\\alarm")){
        c = 7;
    }else if(!MRSTRCMP("#\\backspace")){
        c = 8;
    }else if(!MRSTRCMP("#\\delete")){
        c = 0x7f;
    }else if(!MRSTRCMP("#\\escape")){
        c = 0x1b;
    }else if(!MRSTRCMP("#\\newline")){
        c = 0xa;
    }else if(!MRSTRCMP("#\\null")){
        c = 0;
    }else if(!MRSTRCMP("#\\return")){
        c = 0xd;
    }else if(!MRSTRCMP("#\\space")){
        c = 0x20;
    }else if(!MRSTRCMP("#\\tab")){
        c = 9;
    }else{
        abort();
    }
    RNFUNC_CALL(ctx, RnChar(ctx, out, c));
    RNFUNC_END;
}

enum mr_objtype_e {
    MO_IGNORE,
    MO_CHARLIT,
    MO_NUMBER,
    MO_BYTEVECTOR_BEGIN,
    MO_VECTOR_BEGIN,
    MO_SYMBOL
};

typedef enum mr_objtype_e mr_objtype;

#define MRCMP(k, n) (((me->end_index - me->start_index) >= n) \
&& (memcmp(&buf[me->start_index], k, n) == 0))

#define MRCMP1(k) (buf[me->start_index] == k)

static mr_objtype
mr_realize_checkobjtype(const char* buf, mrtoken* me){
    if(MRCMP("#!", 2)){
        return MO_IGNORE;
    }else if(MRCMP("#\\", 2)){
        // FIXME: Do we need this ??
        return MO_CHARLIT;
    }else if(MRCMP("#x", 2)){
        return MO_NUMBER;
    }else if(MRCMP("#e", 2)){
        return MO_NUMBER;
    }else if(MRCMP("#i", 2)){
        return MO_NUMBER;
    }else if(MRCMP("#b", 2)){
        return MO_NUMBER;
    }else if(MRCMP("#d", 2)){
        return MO_NUMBER;
    }else if(MRCMP("#o", 2)){
        return MO_NUMBER;
    }else if(MRCMP1('0') || MRCMP1('1') || MRCMP1('2') || 
             MRCMP1('3') || MRCMP1('4') || MRCMP1('5') || 
             MRCMP1('6') || MRCMP1('7') || MRCMP1('8') ||
             MRCMP1('9')){
        return MO_NUMBER;
    }else if(MRCMP1('+') || MRCMP1('-')){
        if(me->end_index - me->start_index == 1){
            return MO_SYMBOL;
        }else{
            return MO_NUMBER;
        }
    }else if(MRCMP("#vu8(", 5)){
        return MO_BYTEVECTOR_BEGIN;
    }else if(MRCMP("#u8(", 4)){
        return MO_BYTEVECTOR_BEGIN;
    }else if(MRCMP("#(", 2)){
        return MO_VECTOR_BEGIN;
    }else{
        return MO_SYMBOL;
    }
}

static RnResult
mr_realize_number(RnCtx* ctx, Value* out, mrtoken* me, const char* buf){
    RNFUNC_BEGIN;
    int radix = 10;
    int has_flag = 0;
    Value tmp;
    RnValueLink(ctx, &tmp);
    if(buf[me->start_index] == '#'){
        has_flag = 1;
        switch(buf[me->start_index+1]){
            case 'o':
                radix = 8;
                break;
            case 'b':
                radix = 2;
                break;
            case 'x':
                radix = 16;
                break;
            case 'e':
                radix = 10;
                break;
            default:
            case 'i':
                abort();
                break;
        }
    }
    if(has_flag){
        RNFUNC_CALL(ctx, RnString(ctx, &tmp, &buf[me->start_index + 2],
                                  me->end_index - me->start_index - 2));
    }else{
        RNFUNC_CALL(ctx, RnString(ctx, &tmp, &buf[me->start_index],
                                  me->end_index - me->start_index));
    }
    RNFUNC_CALL(ctx, string_to_number(ctx, out, &tmp, radix));
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
mr_realize_symbol(RnCtx* ctx, Value* out, mrtoken* me, const char* buf){
    RNFUNC_BEGIN;
    Value tmp;
    RnValueLink(ctx, &tmp);
    RNFUNC_CALL(ctx, RnString(ctx, &tmp, &buf[me->start_index],
                              me->end_index - me->start_index));
    RNFUNC_CALL(ctx, ExStringToSymbol(ctx, out, &tmp));
    RnValueUnlink(ctx, &tmp);
    RNFUNC_END;
}

static RnResult
mr_realize_wrap(RnCtx* ctx, Value* out, ssplit_token type){
    RNFUNC_BEGIN;
    const char* symname;
    Value sym;
    Value nil;
    Value obj;
    switch(type){
        case TOK_NEXT_QUOTE:
            symname = "quote";
            break;
        case TOK_NEXT_QUASIQUOTE:
            symname = "quasiquote";
            break;
        case TOK_NEXT_UNQUOTE:
            symname = "unquote";
            break;
        case TOK_NEXT_UNQUOTE_SPLICING:
            symname = "unquote-splicing";
            break;
        case TOK_NEXT_SYNTAX_QUOTE:
            symname = "syntax";
            break;
        case TOK_NEXT_SYNTAX_UNQUOTE:
            symname = "unsyntax";
            break;
        case TOK_NEXT_SYNTAX_QUASIQUOTE:
            symname = "quasisyntax";
            break;
        case TOK_NEXT_SYNTAX_UNQUOTE_SPLICING:
            symname = "unsyntax-splicing";
            break;
        default:
            abort();
    }
    RnValueLink(ctx, &sym);
    RnValueLink(ctx, &nil);
    RnValueLink(ctx, &obj);
    RnValueRef(ctx, &obj, out->value, out->type);
    RNFUNC_CALL(ctx, RnString(ctx, &sym, symname, strlen(symname)));
    RNFUNC_CALL(ctx, ExStringToSymbol(ctx, &sym, &sym));
    RNFUNC_CALL(ctx, RnZone0(ctx, &nil, ZZ_NIL));
    RNFUNC_CALL(ctx, RnCons(ctx, out, &obj, &nil));
    RNFUNC_CALL(ctx, RnCons(ctx, out, &sym, out));
    RnValueUnlink(ctx, &sym);
    RnValueUnlink(ctx, &nil);
    RnValueUnlink(ctx, &obj);
    RNFUNC_END;
}

static RnResult
mr_realize_itr(RnCtx* ctx, Value* out, mr_realize_mode mode, 
               mrtoken** curvec, int* curidx, int* totalidx, int total_tokens,
               Value* tokens, const char* buf){
    RNFUNC_BEGIN;
    Value acc;
    Value tmp;
    Value tmp2;
    mrtoken* me;
    mrtoken* vec;
    mr_objtype objtype;
    int i;
    int objcnt = 0;
    int turn_to_pair = 0;
    RnValueLink(ctx, &acc);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &tmp2);
    RNFUNC_CALL(ctx, RnZone0(ctx, &acc, ZZ_NIL));
    for(;;){
        if(*totalidx == total_tokens){
            if(mode != MM_FIRST){
                abort();
            }
            goto endobj;
        }
        if(*curidx == MR_SECTOR_SIZE){
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, tokens, 0));
            RNFUNC_CALL(ctx, RnRibRef(ctx, tokens, tokens, 1));
            vec = (mrtoken*)tmp.value.as_bytevector->buf;
            *curvec = vec;
            *curidx = 0;
        }
        vec = *curvec;
        me = &vec[*curidx];
        *curidx += 1;
        *totalidx += 1;


#define MMITR(next) \
        RNFUNC_CALL(ctx, mr_realize_itr(ctx, &tmp, next, curvec, curidx, \
                                        totalidx, total_tokens, tokens, \
                                        buf))

        switch(me->type){
            case TOK_COMMENT:
                /* Do nothing */
                continue;
            case TOK_TRUE:
                RNFUNC_CALL(ctx, to_bool(ctx, &tmp, 1));
                break;
            case TOK_FALSE:
                RNFUNC_CALL(ctx, to_bool(ctx, &tmp, 0));
                break;
            case TOK_STRING:
                RNFUNC_CALL(ctx, mr_realize_string(ctx, &tmp, me, buf));
                break;
            case TOK_OBJ:
                objtype = mr_realize_checkobjtype(buf, me);
                switch(objtype){
                    case MO_IGNORE:
                        continue;
                    case MO_CHARLIT:
                        RNFUNC_CALL(ctx, mr_realize_char(ctx, &tmp, me, buf));
                        break;
                    case MO_NUMBER:
                        RNFUNC_CALL(ctx,
                                    mr_realize_number(ctx, &tmp, me, buf));
                        break;
                    case MO_BYTEVECTOR_BEGIN:
                        MMITR(MM_BYTEVECTOR);
                        break;
                    case MO_VECTOR_BEGIN:
                        MMITR(MM_VECTOR);
                        break;
                    case MO_SYMBOL:
                        RNFUNC_CALL(ctx, 
                                    mr_realize_symbol(ctx, &tmp, me, buf));
                        break;
                    default:
                        abort();
                }
                break;
            case TOK_LIST_BEGIN_PAREN:
            case TOK_LIST_BEGIN_SQ:
                MMITR(MM_LIST);
                break;
            case TOK_TURN_TO_PAIR:
                if(objcnt == 0){
                    abort();
                }
                turn_to_pair = objcnt + 1;
                continue;
            case TOK_LIST_END_PAREN:
            case TOK_LIST_END_SQ:
                goto endobj;
            case TOK_NEXT_QUOTE:
            case TOK_NEXT_QUASIQUOTE:
            case TOK_NEXT_UNQUOTE:
            case TOK_NEXT_UNQUOTE_SPLICING:
            case TOK_NEXT_SYNTAX_QUOTE:
            case TOK_NEXT_SYNTAX_UNQUOTE:
            case TOK_NEXT_SYNTAX_QUASIQUOTE:
            case TOK_NEXT_SYNTAX_UNQUOTE_SPLICING:
                MMITR(MM_ONE);
                RNFUNC_CALL(ctx, mr_realize_wrap(ctx, &tmp, me->type));
                break;
            case TOK_NEXT_DATUM_COMMENT:
                MMITR(MM_ONE);
                continue;
            default:
                abort();
                break;
        }
        if(mode == MM_ONE){
            break;
        }else{
            /* push to acc */
            RNFUNC_CALL(ctx, RnCons(ctx, &acc, &tmp, &acc));
            objcnt++;
        }
    }
endobj:
    /* Output acc (or tmp) */
    if(mode == MM_ONE){
        RnValueRef(ctx, out, tmp.value, tmp.type);
    }else{
        /* Convert acc */
        switch(mode){
            case MM_FIRST:
            case MM_LIST:
                /* Reverse acc */
                if(turn_to_pair){
                    if(turn_to_pair != objcnt){
                        abort();
                    }
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &acc, 0));
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &acc, &acc, 1));
                    objcnt--;
                }else{
                    RNFUNC_CALL(ctx, RnZone0(ctx, &tmp, ZZ_NIL));
                }
                for(i=0;i!=objcnt;i++){
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp2, &acc, 0));
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &acc, &acc, 1));
                    RNFUNC_CALL(ctx, RnCons(ctx, &tmp, &tmp2, &tmp));
                }
                RnValueRef(ctx, out, tmp.value, tmp.type);
                break;
            case MM_VECTOR:
                RNFUNC_CALL(ctx, RnVector(ctx, out, objcnt));
                for(i=0;i!=objcnt;i++){
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp2, &acc, 0));
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &acc, &acc, 1));
                    RNFUNC_CALL(ctx, RnVectorSet(ctx, out, &tmp2, 
                                                 objcnt - 1 - i));
                }
                break;
            case MM_BYTEVECTOR:
                RNFUNC_CALL(ctx, RnBytevector(ctx, out, objcnt));
                for(i=0;i!=objcnt;i++){
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp2, &acc, 0));
                    RNFUNC_CALL(ctx, RnRibRef(ctx, &acc, &acc, 1));
                    if(tmp2.type != VT_INT64){
                        abort();
                    }
                    out->value.as_bytevector->buf[objcnt - 1 - i] = 
                        tmp2.value.as_int64;
                }
                break;
            default:
                abort();
                break;
        }
    }
    RnValueUnlink(ctx, &tmp2);
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &acc);
    RNFUNC_END;
}

static RnResult
ExUtf8Read(RnCtx* ctx, Value* out, Value* bv){
    RNFUNC_BEGIN;
    mrctx mctx;
    Value cur;
    Value tokens;
    Value tokens_bak;
    Value tmp;
    int readidx;
    int tokencnt;
    int total_tokens;
    int has_hold;
    mrtoken* firstvec;
    int curidx;
    int totalidx;
    RnValueLink(ctx, &tokens_bak);
    RnValueLink(ctx, &tokens);
    RnValueLink(ctx, &tmp);
    RnValueLink(ctx, &cur);

    if(bv->type != VT_BYTEVECTOR){
        abort();
    }
    RNFUNC_CALL(ctx, RnZone0(ctx, &cur, ZZ_NIL));

    /* Pass1: Convert string into tokens */
    ribbon_mr_init(&mctx);
    readidx = 0;
    for(;;){
        RNFUNC_CALL(ctx, mr_sector_new(ctx, &cur));
        RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &cur, 0));
        (void) ribbon_mr_input(&mctx, 
                               (mrtoken*)tmp.value.as_bytevector->buf,
                               MR_SECTOR_SIZE,
                               &readidx, &tokencnt, &has_hold,
                               (char*)bv->value.as_bytevector->buf,
                               bv->value.as_bytevector->len,
                               1 /* terminate */);
        if(tokencnt == MR_SECTOR_SIZE){
            continue;
        }else{
            break;
        }
    }

    /* Pass2: (reverse cur) and count tokens */
    total_tokens = 0;
    RNFUNC_CALL(ctx, RnZone0(ctx, &tokens, ZZ_NIL));
    for(;;){
        if(cur.type == VT_ZONE0 && cur.value.as_zone0 == ZZ_NIL){
            total_tokens -= MR_SECTOR_SIZE;
            total_tokens += tokencnt;
            break;
        }else{
            RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &cur, 0));
            RNFUNC_CALL(ctx, RnRibRef(ctx, &cur, &cur, 1));
            RNFUNC_CALL(ctx, RnCons(ctx, &tokens, &tmp, &tokens));
            total_tokens += MR_SECTOR_SIZE;
        }
    }
    /* Backup tokens (to keep reference during mr_realize_itr) */
    RnValueRef(ctx, &tokens_bak, tokens.value, tokens.type);

    /* Pass3: Realize tokens */
    totalidx = 0;
    curidx = 0;
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tmp, &tokens, 0));
    RNFUNC_CALL(ctx, RnRibRef(ctx, &tokens, &tokens, 1));
    firstvec = (mrtoken*)tmp.value.as_bytevector->buf;
    RNFUNC_CALL(ctx, mr_realize_itr(ctx, out, MM_FIRST, 
                                    &firstvec, &curidx, &totalidx, total_tokens,
                                    &tokens,
                                    (char*)bv->value.as_bytevector->buf));

    RnValueUnlink(ctx, &cur);
    RnValueUnlink(ctx, &tmp);
    RnValueUnlink(ctx, &tokens);
    RnValueUnlink(ctx, &tokens_bak);
    RNFUNC_END;
}
