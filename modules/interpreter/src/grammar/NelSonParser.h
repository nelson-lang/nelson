//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    STRING = 270,
    SPECIALCALL = 271,
    END = 272,
    IF = 273,
    FUNCTION = 274,
    FOR = 275,
    BREAK = 276,
    MAGICEND = 277,
    WHILE = 278,
    ELSE = 279,
    ELSEIF = 280,
    SWITCH = 281,
    CASE = 282,
    OTHERWISE = 283,
    CONTINUE = 284,
    TRY = 285,
    CATCH = 286,
    FIELD = 287,
    REFLPAREN = 288,
    REFRPAREN = 289,
    KEYBOARD = 290,
    RETURN = 291,
    VARARGIN = 292,
    VARARGOUT = 293,
    QUIT = 294,
    ABORT = 295,
    ENDFUNCTION = 296,
    SOR = 297,
    SAND = 298,
    NE = 299,
    NOT = 300,
    NEG = 301,
    POS = 302
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
typedef int YYSTYPE;
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
