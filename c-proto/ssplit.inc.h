#include <stdint.h>
#include <string.h>
#include <stdlib.h>

enum ssplit_byte_class_e {
    BC_NONE,
    BC_TAB,
    BC_LF,
    BC_CR,
    BC_SPACE,
    BC_DQUOTE,
    BC_SHARP,
    BC_FQUOTE,
    BC_PAREN_L,
    BC_PAREN_R,
    BC_COMMA,
    BC_DOT,
    BC_SEMICOLON,
    BC_AT,
    BC_LARGE_F,
    BC_LARGE_T,
    BC_SQ_L,
    BC_BACKSLASH,
    BC_SQ_R,
    BC_BQUOTE,
    BC_SMALL_F,
    BC_SMALL_T,
    BC_PIPE
};

typedef enum ssplit_byte_class_e ssplit_byte_class;

static ssplit_byte_class
to_ssplit_byte_class(int c){
    switch(c){
        case 9:
            return BC_TAB;
        case 10:
            return BC_LF;
        case 13:
            return BC_CR;
        case 32:
            return BC_SPACE;
        case 34:
            return BC_DQUOTE;
        case 35:
            return BC_SHARP;
        case 39:
            return BC_FQUOTE;
        case 40:
            return BC_PAREN_L;
        case 41:
            return BC_PAREN_R;
        case 44:
            return BC_COMMA;
        case 46:
            return BC_DOT;
        case 59:
            return BC_SEMICOLON;
        case 64:
            return BC_AT;
        case 70:
            return BC_LARGE_F;
        case 84:
            return BC_LARGE_T;
        case 91:
            return BC_SQ_L;
        case 92:
            return BC_BACKSLASH;
        case 93:
            return BC_SQ_R;
        case 96:
            return BC_BQUOTE;
        case 102:
            return BC_SMALL_F;
        case 116:
            return BC_SMALL_T;
        case 124:
            return BC_PIPE;
        default:
            return BC_NONE;
    }
}

static int
ssplit_byte_whitespace_p(int c){
    ssplit_byte_class bc;
    bc = to_ssplit_byte_class(c);
    switch(bc){
        case BC_SPACE:
        case BC_TAB:
        case BC_CR:
        case BC_LF:
            return 1;
        default:
            return 0;
    }
}

static int
ssplit_byte_delimiter_p(int c){
    ssplit_byte_class bc;
    if(ssplit_byte_whitespace_p(c)){
        return 1;
    }
    bc = to_ssplit_byte_class(c);
    switch(bc){
        case BC_PAREN_L:
        case BC_PAREN_R:
        case BC_SQ_L:
        case BC_SQ_R:
        case BC_SEMICOLON:
        case BC_SHARP:
        case BC_DQUOTE:
            return 1;
        default:
            return 0;
    }
}

enum ssplit_token_e {
    /* newline */
    TOK_CR,
    TOK_LF,
    TOK_CRLF,
    /* objects/chars */
    TOK_SPACE,
    TOK_DOT,
    TOK_SHARP,
    TOK_SEMICOLON,
    TOK_DQUOTE,
    TOK_TRUE,
    TOK_FALSE,
    TOK_PIPE,
    TOK_BACKSLASH,
    /* string escape */
    TOK_ESCAPE_DQUOTE,
    /* begin/end */
    TOK_LIST_BEGIN_PAREN,
    TOK_LIST_END_PAREN,
    TOK_LIST_BEGIN_SQ,
    TOK_LIST_END_SQ,
    TOK_BLOCK_COMMENT_BEGIN,
    TOK_BLOCK_COMMENT_END,
    /* states */
    TOK_NEXT_QUOTE,
    TOK_NEXT_QUASIQUOTE,
    TOK_NEXT_UNQUOTE,
    TOK_NEXT_SYNTAX_QUOTE,
    TOK_NEXT_SYNTAX_QUASIQUOTE,
    TOK_NEXT_SYNTAX_UNQUOTE,
    TOK_NEXT_UNQUOTE_SPLICING,
    TOK_NEXT_CHAR_LITERAL,
    TOK_NEXT_DATUM_COMMENT,
    TOK_NEXT_SYNTAX_UNQUOTE_SPLICING,
    /* Special */
    TOK_CHARLIT,
    TOK_TURN_TO_PAIR,
    TOK_OBJ,
    TOK_STRING,
    TOK_COMMENT,
    /* meta */
    TOK_others,
    TOK_t,
    TOK_f
};

typedef enum ssplit_token_e ssplit_token;
typedef ssplit_token ssplit_pair[2];

/* Normal context */
static void
ssplit_parse_byte0(ssplit_pair out, int c){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(bc){
        /* Paren */
        case BC_PAREN_L:
            *a = TOK_LIST_BEGIN_PAREN;
            break;
        case BC_PAREN_R:
            *a = TOK_LIST_END_PAREN;
            break;
        case BC_SQ_L:
            *a = TOK_LIST_BEGIN_SQ;
            break;
        case BC_SQ_R:
            *a = TOK_LIST_END_SQ;
            break;
        /* Quotes */
        case BC_FQUOTE:
            *a = TOK_NEXT_QUOTE;
            break;
        case BC_BQUOTE:
            *a = TOK_NEXT_QUASIQUOTE;
            break;
        case BC_COMMA:
            *a = TOK_NEXT_UNQUOTE; /* Unquote splicing */
            *b = TOK_t;
            break;
        /* Spaces */
        case BC_SPACE:
            *a = TOK_SPACE;
            break;
        case BC_CR:
            *a = TOK_CR; /* CRLF */
            *b = TOK_t;
            break;
        case BC_LF:
            *a = TOK_LF;
            break;
        /* Specials */
        case BC_DOT:
            *a = TOK_DOT;
            break;
        case BC_SHARP: /* #t #f ... */
            *a = TOK_SHARP;
            *b = TOK_t;
            break;
        case BC_SEMICOLON:
            *a = TOK_SEMICOLON;
            break;
        case BC_DQUOTE:
            *a = TOK_DQUOTE;
            break;
        default:
            break;
    }
}

static void
ssplit_parse_byte1(ssplit_pair out, int c, ssplit_token prev_sym){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(prev_sym){
        case TOK_SHARP:
            switch(bc){
                case BC_FQUOTE:
                    *a = TOK_NEXT_SYNTAX_QUOTE;
                    break;
                case BC_BQUOTE:
                    *a = TOK_NEXT_SYNTAX_QUASIQUOTE;
                    break;
                case BC_COMMA:
                    *a = TOK_NEXT_SYNTAX_UNQUOTE; /* splicing */
                    *b = TOK_t;
                    break;
                case BC_BACKSLASH:
                    *a = TOK_NEXT_CHAR_LITERAL;
                    break;
                case BC_PIPE:
                    *a = TOK_BLOCK_COMMENT_BEGIN;
                    break;
                case BC_SEMICOLON:
                    *a = TOK_NEXT_DATUM_COMMENT;
                    break;
                case BC_SMALL_T:
                    *a = TOK_TRUE;
                    break;
                case BC_SMALL_F:
                    *a = TOK_FALSE;
                    break;
                case BC_LARGE_T:
                    *a = TOK_TRUE;
                    break;
                case BC_LARGE_F:
                    *a = TOK_FALSE;
                    break;
                default:
                    break;
            }
            break;
        case TOK_NEXT_UNQUOTE:
            if(bc == BC_AT){
                *a = TOK_NEXT_UNQUOTE_SPLICING;
                *b = TOK_t;
            }
            break;
        case TOK_CR:
            if(bc == BC_LF){
                *a = TOK_CRLF;
            }
            break;
        default:
            break;
    }
}

static void
ssplit_parse_byte2(ssplit_pair out, int c, ssplit_token prev_sym){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    if(prev_sym == TOK_NEXT_SYNTAX_UNQUOTE){
        if(bc == BC_AT){
            *a = TOK_NEXT_SYNTAX_UNQUOTE_SPLICING;
        }
    }
}

/* String context */
static void
ssplit_instring_parse_byte0(ssplit_pair out, int c){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(bc){
        case BC_BACKSLASH:
            *a = TOK_BACKSLASH;
            *b = TOK_t;
            break;
        case BC_DQUOTE:
            *a = TOK_DQUOTE;
            break;
        case BC_CR:
            *a = TOK_CR;
            *b = TOK_t;
            break;
        case BC_LF:
            *a = TOK_LF;
            break;
        default:
            break;
    }
}

static void
ssplit_instring_parse_byte1(ssplit_pair out, int c, ssplit_token prev_sym){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(prev_sym){
        case TOK_CR:
            if(bc == BC_LF){
                *a = TOK_CRLF;
            }
            break;
        case TOK_BACKSLASH:
            if(bc == BC_DQUOTE){
                *a = TOK_ESCAPE_DQUOTE;
            }
            break;
        default:
            break;
    }
}

/* Line comment context */
static void
ssplit_incomment_parse_byte0(ssplit_pair out, int c){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(bc){
        case BC_CR:
            *a = TOK_CR;
            *b = TOK_t;
            break;
        case BC_LF:
            *a = TOK_LF;
            break;
        case BC_SEMICOLON:
            *a = TOK_SEMICOLON;
        default:
            break;
    }
}

static void
ssplit_incomment_parse_byte1(ssplit_pair out, int c, ssplit_token prev_sym){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(prev_sym){
        case TOK_CR:
            if(bc == BC_LF){
                *a = TOK_CRLF;
            }
            break;
        default:
            break;
    }
}

/* Block comment context */
static void
ssplit_inblockcomment_parse_byte0(ssplit_pair out, int c){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(bc){
        case BC_PIPE:
            *a = TOK_PIPE;
            *b = TOK_t;
            break;
        case BC_SHARP:
            *a = TOK_SHARP;
            *b = TOK_t;
            break;
        case BC_CR:
            *a = TOK_CR;
            *b = TOK_t;
            break;
        case BC_LF:
            *a = TOK_LF;
            break;
        default:
            break;
    }
}

static void
ssplit_inblockcomment_parse_byte1(ssplit_pair out, 
                                  int c, ssplit_token prev_sym){
    ssplit_byte_class bc;
    ssplit_token* a;
    ssplit_token* b;
    a = &out[0];
    b = &out[1];
    *a = TOK_others;
    *b = TOK_f;
    bc = to_ssplit_byte_class(c);
    switch(prev_sym){
        case TOK_PIPE:
            if(bc == BC_SHARP){
                *a = TOK_BLOCK_COMMENT_END;
            }
            break;
        case TOK_SHARP:
            if(bc == BC_PIPE){
                *a = TOK_BLOCK_COMMENT_BEGIN;
            }
            break;
        case TOK_CR:
            if(bc == BC_LF){
                *a = TOK_CRLF;
            }
            break;
        default:
            break;
    }
}

/* State machine(mr: miniread) */

enum mrstate_e {
    MR_Initial,
    MR_CHARLIT,
    MR_OBJ0, MR_OBJ0xSHARP, MR_OBJ0xDOT,
    MR_OBJ1, MR_OBJ1xSHARP,
    MR_OBJ2,
    MR_STRING0,
    MR_STRING1,
    MR_LINECOMMENT0,
    MR_LINECOMMENT1,
    MR_BLOCKCOMMENT0,
    MR_BLOCKCOMMENT1
};

typedef enum mrstate_e mrstate;

struct mrctx_s {
    mrstate state;
    ssplit_token reg;
    int hold;
    void* hold_stream;
    int hold_index;
    int lineno;
    int column;
    int blockcomment_depth;
    void* start_stream;
    int start_index;
    int start_lineno;
    int start_column;
    ssplit_token prev_type;
    void* prev_stream;
    int prev_index;
    int prev_lineno;
    int prev_column;
};

typedef struct mrctx_s mrctx;

struct mrtoken_s {
    int start_index;
    int start_lineno;
    int start_column;
    int end_index;
    int end_lineno;
    int end_column;
    ssplit_token type;
    void* start_stream;
    void* end_stream;
};

typedef struct mrtoken_s mrtoken;

static void
ribbon_mr_init(mrctx* ctx){
    memset(ctx, 0, sizeof(mrctx));
    ctx->state = MR_Initial;
}

static int /* has error? */
ribbon_mr_input(mrctx* ctx, mrtoken* tkn, size_t tkncnt,
                int* out_readidx, int* out_outtkncnt, int* out_hold,
                const char* buf, int len, int terminate){

    size_t curidx = 0;
    int step_b;
    void* step_stream;
    int step_index;
    ssplit_token step_prev_type;

    int do_terminate = terminate;
    int read_index = 0;
    ssplit_pair p;


    /* Main loop */
    for(;;){
        if(curidx == tkncnt){
            break;
        }
        if(ctx->hold){ /* Consume holded byte */
            step_b = ctx->hold;
            ctx->hold = 0;
            step_stream = ctx->hold_stream;
            step_index = ctx->hold_index;
        }else{ /* Consume buffer */
            if(len == read_index){
                if(do_terminate){
                    /* feed whitespace as a terminal character */
                    do_terminate = 0;
                    step_b = 32;
                    step_index = 0;
                    step_stream = 0;
                }else{
                    break;
                }
            }else{
                step_b = buf[read_index];
                step_index = read_index;
                step_stream = (void*)buf;
                read_index++;
            }
        }

        /* callstep */
        switch(ctx->state){
            case MR_CHARLIT:
                /* Special: Character literal */
                p[0] = TOK_CHARLIT;
                p[1] = TOK_f;
                break;
            case MR_Initial:
            case MR_OBJ0:
            case MR_OBJ0xSHARP:
            case MR_OBJ0xDOT:
                ssplit_parse_byte0(p, step_b);
                break;
            case MR_OBJ1:
            case MR_OBJ1xSHARP:
                ssplit_parse_byte1(p, step_b, ctx->reg);
                break;
            case MR_OBJ2:
                ssplit_parse_byte2(p, step_b, ctx->reg);
                break;
            case MR_STRING0:
                ssplit_instring_parse_byte0(p, step_b);
                break;
            case MR_STRING1:
                ssplit_instring_parse_byte1(p, step_b, ctx->reg);
                break;
            case MR_LINECOMMENT0:
                ssplit_incomment_parse_byte0(p, step_b);
                break;
            case MR_LINECOMMENT1:
                ssplit_incomment_parse_byte1(p, step_b, ctx->reg);
                break;
            case MR_BLOCKCOMMENT0:
                ssplit_inblockcomment_parse_byte0(p, step_b);
                break;
            case MR_BLOCKCOMMENT1:
                ssplit_inblockcomment_parse_byte1(p, step_b, ctx->reg);
                break;
            default:
                abort();
                break;
        }

        step_prev_type = ctx->reg;
        ctx->reg = p[0];
        
#define MR_HOLD() \
        ctx->hold = step_b; \
        ctx->hold_index = step_index; \
        ctx->hold_stream = step_stream

#define MR_SET_PREV_HERE(type) \
        ctx->prev_type = type; \
        ctx->prev_stream = step_stream; \
        ctx->prev_index = step_index; \
        ctx->prev_lineno = ctx->lineno; \
        ctx->prev_column = ctx->column

#define MR_BEGIN_HERE(next_state) \
        ctx->state = next_state; \
        ctx->start_stream = step_stream; \
        ctx->start_index = step_index; \
        ctx->start_lineno = ctx->lineno; \
        ctx->start_column = ctx->column

#define MR_TKN_SET_START(typ) \
        tkn[curidx].type = typ; \
        tkn[curidx].start_stream = ctx->start_stream; \
        tkn[curidx].start_index = ctx->start_index; \
        tkn[curidx].start_lineno = ctx->lineno; \
        tkn[curidx].start_column = ctx->column

#define MR_EMIT_TKN() \
        ctx->state = MR_Initial; \
        curidx ++;

#define MR_END_HERE(tkn_type) \
        MR_TKN_SET_START(tkn_type); \
        tkn[curidx].end_stream = step_stream; \
        tkn[curidx].end_index = step_index; \
        tkn[curidx].end_lineno = ctx->lineno; \
        tkn[curidx].end_column = ctx->column; \
        MR_EMIT_TKN()

#define MR_END_PREV() \
        MR_TKN_SET_START(ctx->prev_type); \
        tkn[curidx].end_stream = ctx->prev_stream; \
        tkn[curidx].end_index = ctx->prev_index; \
        tkn[curidx].end_lineno = ctx->prev_lineno; \
        tkn[curidx].end_column = ctx->prev_column; \
        MR_EMIT_TKN()

#define MR_TKN_SINGLE(tkn_type) \
        MR_BEGIN_HERE(MR_Initial); \
        MR_END_HERE(tkn_type)

        /* char */
        switch(ctx->state){
            case MR_CHARLIT:
                MR_SET_PREV_HERE(TOK_OBJ);
                ctx->state = MR_OBJ0;
                break;
            case MR_Initial:
                switch(p[0]){
                    /* Lists */
                    case TOK_LIST_BEGIN_PAREN:
                    case TOK_LIST_END_PAREN:
                    case TOK_LIST_BEGIN_SQ:
                    case TOK_LIST_END_SQ:
                    /* Quotes */
                    case TOK_NEXT_QUOTE:
                    case TOK_NEXT_QUASIQUOTE:
                        MR_TKN_SINGLE(p[0]);
                        break;
                    case TOK_SEMICOLON:
                        MR_BEGIN_HERE(MR_LINECOMMENT0);
                        break;
                    case TOK_DQUOTE:
                        MR_BEGIN_HERE(MR_STRING0);
                        break;
                    case TOK_NEXT_UNQUOTE:
                        MR_SET_PREV_HERE(TOK_NEXT_UNQUOTE);
                        MR_BEGIN_HERE(MR_OBJ1);
                        break;
                    case TOK_SPACE:
                    case TOK_CR:
                    case TOK_LF:
                        /* Do nothing */
                        break;
                    case TOK_SHARP:
                        MR_SET_PREV_HERE(TOK_OBJ);
                        MR_BEGIN_HERE(MR_OBJ1);
                        break;
                    case TOK_DOT:
                        MR_SET_PREV_HERE(TOK_TURN_TO_PAIR);
                        MR_BEGIN_HERE(MR_OBJ0xDOT);
                        break;
                    default: /* Some object */
                        MR_SET_PREV_HERE(TOK_OBJ);
                        if(p[1] != TOK_f){
                            MR_BEGIN_HERE(MR_OBJ1);
                        }else{
                            MR_BEGIN_HERE(MR_OBJ0);
                        }
                        break;
                }
                break;
            case MR_OBJ0:
            case MR_OBJ0xSHARP:
            case MR_OBJ0xDOT:
                if(p[0] == TOK_LIST_BEGIN_PAREN){
                    /* Include L paren into token */
                    if(ctx->state == MR_OBJ0xSHARP){
                        MR_END_HERE(TOK_OBJ);
                    }else{
                        MR_END_PREV();
                        MR_HOLD();
                        ctx->state = MR_Initial;
                    }
                }else{
                    if(ssplit_byte_whitespace_p(step_b) ||
                       ssplit_byte_delimiter_p(step_b)){
                        MR_END_PREV();
                        MR_HOLD();
                        ctx->state = MR_Initial;
                    }else{
                        MR_SET_PREV_HERE(TOK_OBJ);
                        if(p[1] != TOK_f){
                            ctx->state = MR_OBJ1;
                        }
                    }
                }
                break;
            case MR_OBJ1:
            case MR_OBJ1xSHARP:
                switch(p[0]){
                    case TOK_NEXT_CHAR_LITERAL:
                        /* FIXME: We never receive CRLF here. */
                        MR_SET_PREV_HERE(TOK_OBJ);
                        ctx->state = MR_CHARLIT;
                        break;
                    case TOK_NEXT_SYNTAX_QUOTE:
                    case TOK_NEXT_SYNTAX_QUASIQUOTE:
                    case TOK_NEXT_UNQUOTE_SPLICING:
                    case TOK_NEXT_DATUM_COMMENT:
                    case TOK_TRUE:
                    case TOK_FALSE:
                        MR_END_HERE(p[0]);
                        break;
                    case TOK_BLOCK_COMMENT_BEGIN:
                        ctx->blockcomment_depth++;
                        ctx->state = MR_BLOCKCOMMENT0;
                        break;
                    default:
                        /* Follow-up OBJ0 */
                        if(step_prev_type == TOK_NEXT_UNQUOTE){
                            MR_END_PREV();
                            MR_HOLD();
                            ctx->state = MR_Initial;
                        }else{
                            /* Hold a byte and restart with Initial/OBJ0 */
                            if(ctx->state == MR_OBJ1xSHARP &&
                               to_ssplit_byte_class(step_b) == BC_PAREN_L){
                            }else if(ssplit_byte_delimiter_p(step_b)){
                                MR_END_PREV();
                                MR_HOLD();
                                ctx->state = MR_Initial;
                            }else{
                                MR_HOLD();
                                if(ctx->state == MR_OBJ1xSHARP){
                                    ctx->state = MR_OBJ0xSHARP;
                                }else{
                                    ctx->state = MR_OBJ0;
                                }
                            }
                        }
                        break;
                }
                break;
            case MR_OBJ2:
                if(p[0] == TOK_NEXT_SYNTAX_UNQUOTE_SPLICING){
                    MR_END_HERE(TOK_NEXT_SYNTAX_UNQUOTE_SPLICING);
                }else if(step_prev_type == TOK_NEXT_SYNTAX_UNQUOTE){
                    MR_END_PREV();
                    MR_HOLD();
                    ctx->state = MR_Initial;
                }else{
                    abort();
                }
                break;
            case MR_STRING0:
                if(p[0] == TOK_BACKSLASH){
                    ctx->state = MR_STRING1;
                }else if(p[0] == TOK_DQUOTE){
                    MR_END_HERE(TOK_STRING);
                }else{
                    /* Do nothing */
                }
                break;
            case MR_STRING1:
                ctx->state = MR_STRING0;
                break;
            case MR_LINECOMMENT0:
                if(p[0] == TOK_LF){
                    MR_END_HERE(TOK_COMMENT);
                }else if(p[0] == TOK_CR){
                    MR_SET_PREV_HERE(TOK_COMMENT);
                    ctx->state = MR_LINECOMMENT1;
                }
                break;
            case MR_LINECOMMENT1:
                if(p[0] == TOK_CRLF){
                    MR_END_HERE(TOK_COMMENT);
                }else{
                    MR_END_PREV();
                    MR_HOLD();
                    ctx->state = MR_Initial;
                }
                break;
            case MR_BLOCKCOMMENT0:
                if(p[0] == TOK_PIPE || p[0] == TOK_SHARP){
                    ctx->state = MR_BLOCKCOMMENT1;
                }else{
                    /* Do nothing */
                }
                break;
            case MR_BLOCKCOMMENT1:
                if(p[0] == TOK_BLOCK_COMMENT_BEGIN){
                    ctx->blockcomment_depth++;
                }else if(p[0] == TOK_BLOCK_COMMENT_END){
                    ctx->blockcomment_depth--;
                    if(ctx->blockcomment_depth == 0){
                        MR_END_HERE(TOK_COMMENT);
                        ctx->state = MR_Initial;
                    }else{
                        ctx->state = MR_BLOCKCOMMENT0;
                    }
                }else{
                    ctx->state = MR_BLOCKCOMMENT0;
                }
                break;
            default:
                abort();
                break;
        }

#undef MR_TKN_SINGLE
#undef MR_END_PREV
#undef MR_END_HERE
#undef MR_EMIT_TKN
#undef MR_TKN_SET_START
#undef MR_BEGIN_HERE
#undef MR_SET_PREV_HERE
#undef MR_HOLD

    }

    /* output */
    *out_readidx = read_index;
    *out_outtkncnt = curidx;
    *out_hold = ctx->hold ? 1 : 0;

    return 0;
}
