//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "LexerContext.hpp"
//=============================================================================
/* A Bison parser, made by GNU Bison 1.875c.  */
//=============================================================================
/* Tokens.  */
#ifndef YYTOKENTYPE
#define YYTOKENTYPE
/* Put the tokens into the symbol table, so that GDB and other debuggers
   know about them.  */
enum yytokentype
{
    YYEMPTY = -2,
    YYEOF = 0, /* "end of file"  */
    YYerror = 256, /* error  */
    YYUNDEF = 257, /* "invalid token"  */
    IDENT = 258, /* IDENT  */
    NUMERIC = 259, /* NUMERIC  */
    ENDQSTMNT = 260, /* ENDQSTMNT  */
    ENDSTMNT = 261, /* ENDSTMNT  */
    LE = 262, /* LE  */
    GE = 263, /* GE  */
    EQ = 264, /* EQ  */
    DOTTIMES = 265, /* DOTTIMES  */
    DOTRDIV = 266, /* DOTRDIV  */
    DOTLDIV = 267, /* DOTLDIV  */
    DOTPOWER = 268, /* DOTPOWER  */
    DOTTRANSPOSE = 269, /* DOTTRANSPOSE  */
    CHARACTER = 270, /* CHARACTER  */
    STRING = 271, /* STRING  */
    SPECIALCALL = 272, /* SPECIALCALL  */
    END = 273, /* END  */
    IF = 274, /* IF  */
    FUNCTION = 275, /* FUNCTION  */
    FOR = 276, /* FOR  */
    BREAK = 277, /* BREAK  */
    MAGICEND = 278, /* MAGICEND  */
    WHILE = 279, /* WHILE  */
    ELSE = 280, /* ELSE  */
    ELSEIF = 281, /* ELSEIF  */
    SWITCH = 282, /* SWITCH  */
    CASE = 283, /* CASE  */
    OTHERWISE = 284, /* OTHERWISE  */
    CONTINUE = 285, /* CONTINUE  */
    TRY = 286, /* TRY  */
    CATCH = 287, /* CATCH  */
    FIELD = 288, /* FIELD  */
    REFLPAREN = 289, /* REFLPAREN  */
    REFRPAREN = 290, /* REFRPAREN  */
    KEYBOARD = 291, /* KEYBOARD  */
    RETURN = 292, /* RETURN  */
    VARARGIN = 293, /* VARARGIN  */
    VARARGOUT = 294, /* VARARGOUT  */
    ABORT = 296, /* ABORT  */
    ENDFUNCTION = 297, /* ENDFUNCTION  */
    SOR = 298, /* SOR  */
    SAND = 299, /* SAND  */
    NE = 300, /* NE  */
    POS = 301, /* POS  */
    NEG = 302, /* NEG  */
    NOT = 303 /* NOT  */
};
#endif
//=============================================================================
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define IDENT 258
#define NUMERIC 259
#define ENDQSTMNT 260
#define ENDSTMNT 261
#define LE 262
#define GE 263
#define EQ 264
#define DOTTIMES 265
#define DOTRDIV 266
#define DOTLDIV 267
#define DOTPOWER 268
#define DOTTRANSPOSE 269
#define CHARACTER 270
#define STRING 271
#define SPECIALCALL 272
#define END 273
#define IF 274
#define FUNCTION 275
#define FOR 276
#define BREAK 277
#define MAGICEND 278
#define WHILE 279
#define ELSE 280
#define ELSEIF 281
#define SWITCH 282
#define CASE 283
#define OTHERWISE 284
#define CONTINUE 285
#define TRY 286
#define CATCH 287
#define FIELD 288
#define REFLPAREN 289
#define REFRPAREN 290
#define KEYBOARD 291
#define RETURN 292
#define VARARGIN 293
#define VARARGOUT 294
#define ABORT 295
#define ENDFUNCTION 296
#define SOR 297
#define SAND 298
#define NE 299
#define POS 300
#define NEG 301
#define NOT 302
//=============================================================================
#if !defined(YYSTYPE) && !defined(YYSTYPE_IS_DECLARED)
using YYSTYPE = int;
#define yystype YYSTYPE /* obsolescent; will be withdrawn */
#define YYSTYPE_IS_DECLARED 1
#define YYSTYPE_IS_TRIVIAL 1
#endif
//=============================================================================
extern YYSTYPE yylval;
//=============================================================================
void
clearTextBufferLexer(Nelson::LexerContext& lexerContext);
//=============================================================================
