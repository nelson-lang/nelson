//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
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
    IDENT = 258,
    NUMERIC = 259,
    ENDQSTMNT = 260,
    ENDSTMNT = 261,
    LE = 262,
    GE = 263,
    EQ = 264,
    DOTTIMES = 265,
    DOTRDIV = 266,
    DOTLDIV = 267,
    DOTPOWER = 268,
    DOTTRANSPOSE = 269,
    CHARACTER = 270,
    STRING = 271,
    SPECIALCALL = 272,
    END = 273,
    IF = 274,
    FUNCTION = 275,
    FOR = 276,
    BREAK = 277,
    MAGICEND = 278,
    WHILE = 279,
    ELSE = 280,
    ELSEIF = 281,
    SWITCH = 282,
    CASE = 283,
    OTHERWISE = 284,
    CONTINUE = 285,
    TRY = 286,
    CATCH = 287,
    FIELD = 288,
    REFLPAREN = 289,
    REFRPAREN = 290,
    KEYBOARD = 291,
    RETURN = 292,
    VARARGIN = 293,
    VARARGOUT = 294,
    QUIT = 295,
    ABORT = 296,
    ENDFUNCTION = 297,
    SOR = 298,
    SAND = 299,
    NE = 300,
    POS = 301,
    NEG = 302,
    NOT = 303
};
#endif
//=============================================================================
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
#define QUIT 295
#define ABORT 296
#define ENDFUNCTION 297
#define SOR 298
#define SAND 299
#define NE 300
#define POS 301
#define NEG 302
#define NOT 303
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
clearTextBufferLexer();
//=============================================================================
