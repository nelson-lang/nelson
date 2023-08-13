/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"

// clang-format off
//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// clang-format off
//bison -L C -k -o NelSonParser.cpp NelSonParser.yxx
//=============================================================================
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include "NelsonParserHelpers.hpp"
#include "AbstractSyntaxTree.hpp"
#include "i18n.hpp"
//=============================================================================
#define YYSTYPE ParseRHS
//=============================================================================
extern int yylex(void);
extern int yydebug;
//=============================================================================
static std::mutex parseMutex;
//=============================================================================
namespace Nelson {
  void yyerror(const char *s) {
     return;
  }
}
//=============================================================================
using namespace Nelson;
//=============================================================================

#line 110 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    IDENT = 258,                   /* IDENT  */
    NUMERIC = 259,                 /* NUMERIC  */
    ENDQSTMNT = 260,               /* ENDQSTMNT  */
    ENDSTMNT = 261,                /* ENDSTMNT  */
    LE = 262,                      /* LE  */
    GE = 263,                      /* GE  */
    EQ = 264,                      /* EQ  */
    DOTTIMES = 265,                /* DOTTIMES  */
    DOTRDIV = 266,                 /* DOTRDIV  */
    DOTLDIV = 267,                 /* DOTLDIV  */
    DOTPOWER = 268,                /* DOTPOWER  */
    DOTTRANSPOSE = 269,            /* DOTTRANSPOSE  */
    CHARACTER = 270,               /* CHARACTER  */
    STRING = 271,                  /* STRING  */
    SPECIALCALL = 272,             /* SPECIALCALL  */
    END = 273,                     /* END  */
    IF = 274,                      /* IF  */
    FUNCTION = 275,                /* FUNCTION  */
    FOR = 276,                     /* FOR  */
    BREAK = 277,                   /* BREAK  */
    MAGICEND = 278,                /* MAGICEND  */
    WHILE = 279,                   /* WHILE  */
    ELSE = 280,                    /* ELSE  */
    ELSEIF = 281,                  /* ELSEIF  */
    SWITCH = 282,                  /* SWITCH  */
    CASE = 283,                    /* CASE  */
    OTHERWISE = 284,               /* OTHERWISE  */
    CONTINUE = 285,                /* CONTINUE  */
    TRY = 286,                     /* TRY  */
    CATCH = 287,                   /* CATCH  */
    FIELD = 288,                   /* FIELD  */
    REFLPAREN = 289,               /* REFLPAREN  */
    REFRPAREN = 290,               /* REFRPAREN  */
    KEYBOARD = 291,                /* KEYBOARD  */
    RETURN = 292,                  /* RETURN  */
    VARARGIN = 293,                /* VARARGIN  */
    VARARGOUT = 294,               /* VARARGOUT  */
    QUIT = 295,                    /* QUIT  */
    ABORT = 296,                   /* ABORT  */
    ENDFUNCTION = 297,             /* ENDFUNCTION  */
    SOR = 298,                     /* SOR  */
    SAND = 299,                    /* SAND  */
    NE = 300,                      /* NE  */
    POS = 301,                     /* POS  */
    NEG = 302,                     /* NEG  */
    NOT = 303                      /* NOT  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_IDENT = 3,                      /* IDENT  */
  YYSYMBOL_NUMERIC = 4,                    /* NUMERIC  */
  YYSYMBOL_ENDQSTMNT = 5,                  /* ENDQSTMNT  */
  YYSYMBOL_ENDSTMNT = 6,                   /* ENDSTMNT  */
  YYSYMBOL_LE = 7,                         /* LE  */
  YYSYMBOL_GE = 8,                         /* GE  */
  YYSYMBOL_EQ = 9,                         /* EQ  */
  YYSYMBOL_DOTTIMES = 10,                  /* DOTTIMES  */
  YYSYMBOL_DOTRDIV = 11,                   /* DOTRDIV  */
  YYSYMBOL_DOTLDIV = 12,                   /* DOTLDIV  */
  YYSYMBOL_DOTPOWER = 13,                  /* DOTPOWER  */
  YYSYMBOL_DOTTRANSPOSE = 14,              /* DOTTRANSPOSE  */
  YYSYMBOL_CHARACTER = 15,                 /* CHARACTER  */
  YYSYMBOL_STRING = 16,                    /* STRING  */
  YYSYMBOL_SPECIALCALL = 17,               /* SPECIALCALL  */
  YYSYMBOL_END = 18,                       /* END  */
  YYSYMBOL_IF = 19,                        /* IF  */
  YYSYMBOL_FUNCTION = 20,                  /* FUNCTION  */
  YYSYMBOL_FOR = 21,                       /* FOR  */
  YYSYMBOL_BREAK = 22,                     /* BREAK  */
  YYSYMBOL_MAGICEND = 23,                  /* MAGICEND  */
  YYSYMBOL_WHILE = 24,                     /* WHILE  */
  YYSYMBOL_ELSE = 25,                      /* ELSE  */
  YYSYMBOL_ELSEIF = 26,                    /* ELSEIF  */
  YYSYMBOL_SWITCH = 27,                    /* SWITCH  */
  YYSYMBOL_CASE = 28,                      /* CASE  */
  YYSYMBOL_OTHERWISE = 29,                 /* OTHERWISE  */
  YYSYMBOL_CONTINUE = 30,                  /* CONTINUE  */
  YYSYMBOL_TRY = 31,                       /* TRY  */
  YYSYMBOL_CATCH = 32,                     /* CATCH  */
  YYSYMBOL_FIELD = 33,                     /* FIELD  */
  YYSYMBOL_REFLPAREN = 34,                 /* REFLPAREN  */
  YYSYMBOL_REFRPAREN = 35,                 /* REFRPAREN  */
  YYSYMBOL_KEYBOARD = 36,                  /* KEYBOARD  */
  YYSYMBOL_RETURN = 37,                    /* RETURN  */
  YYSYMBOL_VARARGIN = 38,                  /* VARARGIN  */
  YYSYMBOL_VARARGOUT = 39,                 /* VARARGOUT  */
  YYSYMBOL_QUIT = 40,                      /* QUIT  */
  YYSYMBOL_ABORT = 41,                     /* ABORT  */
  YYSYMBOL_ENDFUNCTION = 42,               /* ENDFUNCTION  */
  YYSYMBOL_SOR = 43,                       /* SOR  */
  YYSYMBOL_SAND = 44,                      /* SAND  */
  YYSYMBOL_45_ = 45,                       /* '|'  */
  YYSYMBOL_46_ = 46,                       /* '&'  */
  YYSYMBOL_47_ = 47,                       /* '<'  */
  YYSYMBOL_48_ = 48,                       /* '>'  */
  YYSYMBOL_NE = 49,                        /* NE  */
  YYSYMBOL_50_ = 50,                       /* ':'  */
  YYSYMBOL_51_ = 51,                       /* '+'  */
  YYSYMBOL_52_ = 52,                       /* '-'  */
  YYSYMBOL_53_ = 53,                       /* '*'  */
  YYSYMBOL_54_ = 54,                       /* '/'  */
  YYSYMBOL_55_ = 55,                       /* '\\'  */
  YYSYMBOL_POS = 56,                       /* POS  */
  YYSYMBOL_NEG = 57,                       /* NEG  */
  YYSYMBOL_NOT = 58,                       /* NOT  */
  YYSYMBOL_59_ = 59,                       /* '^'  */
  YYSYMBOL_60_ = 60,                       /* '\''  */
  YYSYMBOL_61_ = 61,                       /* '('  */
  YYSYMBOL_62_ = 62,                       /* ')'  */
  YYSYMBOL_63_ = 63,                       /* '='  */
  YYSYMBOL_64_ = 64,                       /* '['  */
  YYSYMBOL_65_ = 65,                       /* ']'  */
  YYSYMBOL_66_ = 66,                       /* ','  */
  YYSYMBOL_67_ = 67,                       /* ';'  */
  YYSYMBOL_68_ = 68,                       /* '{'  */
  YYSYMBOL_69_ = 69,                       /* '}'  */
  YYSYMBOL_70_ = 70,                       /* '~'  */
  YYSYMBOL_71_ = 71,                       /* '.'  */
  YYSYMBOL_YYACCEPT = 72,                  /* $accept  */
  YYSYMBOL_program = 73,                   /* program  */
  YYSYMBOL_functionDef = 74,               /* functionDef  */
  YYSYMBOL_functionDefList = 75,           /* functionDefList  */
  YYSYMBOL_returnDeclaration = 76,         /* returnDeclaration  */
  YYSYMBOL_argumentList = 77,              /* argumentList  */
  YYSYMBOL_argument = 78,                  /* argument  */
  YYSYMBOL_statementList = 79,             /* statementList  */
  YYSYMBOL_statement = 80,                 /* statement  */
  YYSYMBOL_statementType = 81,             /* statementType  */
  YYSYMBOL_endfunctionStatement = 82,      /* endfunctionStatement  */
  YYSYMBOL_specialSyntaxStatement = 83,    /* specialSyntaxStatement  */
  YYSYMBOL_returnStatement = 84,           /* returnStatement  */
  YYSYMBOL_pauseStatement = 85,            /* pauseStatement  */
  YYSYMBOL_continueStatement = 86,         /* continueStatement  */
  YYSYMBOL_breakStatement = 87,            /* breakStatement  */
  YYSYMBOL_tryStatement = 88,              /* tryStatement  */
  YYSYMBOL_optionalCatch = 89,             /* optionalCatch  */
  YYSYMBOL_switchStatement = 90,           /* switchStatement  */
  YYSYMBOL_optionalEndStatement = 91,      /* optionalEndStatement  */
  YYSYMBOL_newLine = 92,                   /* newLine  */
  YYSYMBOL_caseBlock = 93,                 /* caseBlock  */
  YYSYMBOL_caseList = 94,                  /* caseList  */
  YYSYMBOL_caseStatement = 95,             /* caseStatement  */
  YYSYMBOL_otherwiseClause = 96,           /* otherwiseClause  */
  YYSYMBOL_forStatement = 97,              /* forStatement  */
  YYSYMBOL_forIndexExpression = 98,        /* forIndexExpression  */
  YYSYMBOL_whileStatement = 99,            /* whileStatement  */
  YYSYMBOL_ifStatement = 100,              /* ifStatement  */
  YYSYMBOL_conditionedStatement = 101,     /* conditionedStatement  */
  YYSYMBOL_elseIfBlock = 102,              /* elseIfBlock  */
  YYSYMBOL_elseIfStatementList = 103,      /* elseIfStatementList  */
  YYSYMBOL_elseIfStatement = 104,          /* elseIfStatement  */
  YYSYMBOL_elseStatement = 105,            /* elseStatement  */
  YYSYMBOL_assignmentStatement = 106,      /* assignmentStatement  */
  YYSYMBOL_multiFunctionCall = 107,        /* multiFunctionCall  */
  YYSYMBOL_expr = 108,                     /* expr  */
  YYSYMBOL_terminal = 109,                 /* terminal  */
  YYSYMBOL_symbRefList = 110,              /* symbRefList  */
  YYSYMBOL_symbRef = 111,                  /* symbRef  */
  YYSYMBOL_indexElement = 112,             /* indexElement  */
  YYSYMBOL_indexList = 113,                /* indexList  */
  YYSYMBOL_cellDef = 114,                  /* cellDef  */
  YYSYMBOL_matrixDef = 115,                /* matrixDef  */
  YYSYMBOL_rowSeperator = 116,             /* rowSeperator  */
  YYSYMBOL_columnSep = 117,                /* columnSep  */
  YYSYMBOL_rowDef = 118                    /* rowDef  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  94
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3033

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  72
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  47
/* YYNRULES -- Number of rules.  */
#define YYNRULES  227
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  343

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   303


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    46,    60,
      61,    62,    53,    51,    66,    52,    71,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    50,    67,
      47,    63,    48,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    64,    55,    65,    59,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    68,    45,    69,    70,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      49,    56,    57,    58
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    71,    71,    72,    72,    73,    77,    84,    92,   100,
     109,   117,   126,   127,   129,   130,   131,   132,   134,   135,
     139,   140,   144,   145,   146,   147,   148,   149,   150,   151,
     155,   156,   160,   165,   166,   170,   174,   178,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,   205,   208,   209,   210,   211,
     212,   213,   214,   215,   216,   217,   220,   224,   228,   231,
     235,   241,   246,   250,   251,   255,   261,   267,   267,   267,
     267,   271,   271,   276,   276,   280,   283,   289,   295,   298,
     304,   307,   312,   317,   318,   319,   321,   322,   323,   324,
     325,   326,   330,   333,   338,   339,   344,   350,   351,   355,
     358,   361,   365,   366,   370,   373,   379,   382,   385,   388,
     389,   393,   394,   398,   401,   405,   409,   413,   414,   415,
     416,   417,   418,   422,   423,   424,   425,   426,   427,   428,
     429,   430,   431,   432,   433,   434,   435,   436,   437,   438,
     439,   440,   441,   442,   443,   444,   445,   446,   447,   448,
     449,   450,   451,   452,   453,   454,   455,   456,   457,   458,
     459,   460,   461,   462,   463,   464,   465,   466,   467,   468,
     469,   470,   471,   472,   473,   477,   478,   479,   480,   481,
     482,   483,   484,   485,   486,   487,   488,   489,   490,   491,
     492,   493,   496,   497,   500,   501,   502,   503,   504,   505,
     506,   509,   510,   511,   512,   513,   514,   518,   519,   523,
     524,   528,   529,   533,   533,   537,   541,   542
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "IDENT", "NUMERIC",
  "ENDQSTMNT", "ENDSTMNT", "LE", "GE", "EQ", "DOTTIMES", "DOTRDIV",
  "DOTLDIV", "DOTPOWER", "DOTTRANSPOSE", "CHARACTER", "STRING",
  "SPECIALCALL", "END", "IF", "FUNCTION", "FOR", "BREAK", "MAGICEND",
  "WHILE", "ELSE", "ELSEIF", "SWITCH", "CASE", "OTHERWISE", "CONTINUE",
  "TRY", "CATCH", "FIELD", "REFLPAREN", "REFRPAREN", "KEYBOARD", "RETURN",
  "VARARGIN", "VARARGOUT", "QUIT", "ABORT", "ENDFUNCTION", "SOR", "SAND",
  "'|'", "'&'", "'<'", "'>'", "NE", "':'", "'+'", "'-'", "'*'", "'/'",
  "'\\\\'", "POS", "NEG", "NOT", "'^'", "'\\''", "'('", "')'", "'='",
  "'['", "']'", "','", "';'", "'{'", "'}'", "'~'", "'.'", "$accept",
  "program", "functionDef", "functionDefList", "returnDeclaration",
  "argumentList", "argument", "statementList", "statement",
  "statementType", "endfunctionStatement", "specialSyntaxStatement",
  "returnStatement", "pauseStatement", "continueStatement",
  "breakStatement", "tryStatement", "optionalCatch", "switchStatement",
  "optionalEndStatement", "newLine", "caseBlock", "caseList",
  "caseStatement", "otherwiseClause", "forStatement", "forIndexExpression",
  "whileStatement", "ifStatement", "conditionedStatement", "elseIfBlock",
  "elseIfStatementList", "elseIfStatement", "elseStatement",
  "assignmentStatement", "multiFunctionCall", "expr", "terminal",
  "symbRefList", "symbRef", "indexElement", "indexList", "cellDef",
  "matrixDef", "rowSeperator", "columnSep", "rowDef", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-128)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-127)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     397,  -128,   220,  -128,    27,    12,   188,   137,   156,  -128,
    -128,  1391,  2587,  -128,  2387,  -128,  -128,  -128,  -128,  -128,
    2587,  2587,  1421,  1309,  1335,  1447,    46,  -128,    34,   783,
    -128,   159,  -128,   267,  -128,  -128,  -128,  -128,  -128,  -128,
    -128,  -128,  -128,  -128,  -128,  2835,  -128,   -13,  -128,  -128,
    -128,  -128,  -128,  -128,  -128,  -128,  -128,  -128,  1309,    42,
    2653,    75,  -128,   146,   -10,    62,   215,  -128,    -3,   218,
      89,  -128,  2678,  2678,  1043,   201,   201,  -128,  2739,  -128,
    -128,  -128,  -128,  2835,   165,  2587,    21,  -128,  -128,    13,
    2587,    21,  -128,   201,  -128,  -128,  -128,  -128,  -128,  -128,
    -128,  -128,  -128,  -128,  1477,  1503,  1533,  1559,  1589,  1615,
    1645,  -128,  1671,  1701,  1727,  1757,  1783,  1813,  1839,  1869,
    1895,  1925,  1951,  1981,  2007,  2037,  -128,  2467,  2063,  2488,
      28,  -128,   177,  2093,    79,    42,  -128,  -128,  -128,  -128,
    -128,  -128,  2225,   226,  -128,  -128,     8,  -128,  2387,  -128,
    -128,  -128,    74,    19,  -128,  -128,   161,  2119,  -128,    14,
    2281,  2337,   116,  2437,    16,  -128,  -128,   105,  2144,   181,
    -128,  2587,  -128,  2493,    66,  -128,   778,  -128,   778,  -128,
     778,  -128,   201,  -128,   201,  -128,   201,  -128,   201,  -128,
    2889,  -128,  2905,  -128,  2959,  -128,  2973,  -128,   778,  -128,
     778,  -128,   778,  -128,  2704,  -128,   432,  -128,   432,  -128,
     201,  -128,   201,  -128,   201,  -128,   201,  -128,   262,  -128,
    2835,  -128,    26,  -128,  2835,     7,  -128,  2587,  -128,  -128,
    -128,   987,    38,  -128,  2225,  -128,   173,   122,   453,  -128,
    -128,    37,   170,  -128,   138,  2387,  -128,  2835,  -128,  2149,
    -128,   851,  -128,   919,  2587,   176,   116,  -128,   270,  1099,
    -128,  -128,   265,  -128,    21,  -128,  2544,  2835,  -128,    21,
    -128,  2565,  -128,   127,  -128,  -128,  2488,  -128,  -128,  2815,
    -128,  1155,  -128,  -128,  2387,    56,  -128,  -128,  -128,  -128,
     173,   133,   509,  -128,  2759,  -128,  -128,  -128,  -128,  2678,
    2387,    48,  -128,  2387,  -128,    35,  -128,  -128,  2175,  -128,
    -128,   565,  -128,  2387,  2387,   207,  -128,  -128,  2387,  1211,
    -128,  -128,  1267,  -128,  1365,    29,  -128,  2835,   621,   677,
    -128,  2387,  2225,  -128,  -128,    36,  -128,     9,   733,  -128,
    -128,  -128,  -128
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     5,   202,   185,   186,   187,     0,     0,     0,    69,
     188,     0,     0,    68,    40,    67,    66,    52,    53,    55,
       0,     0,     0,     0,     0,     0,     0,    20,     3,    40,
      33,     0,    54,    51,    50,    49,    44,    43,    48,    47,
      42,    45,    46,    38,    41,    39,   135,   189,    61,    56,
      60,    59,    58,    57,   107,   202,   186,   187,     0,   112,
       0,   189,    12,     0,     0,     0,     0,   101,    95,     0,
       0,   104,     0,     0,    40,   173,   172,   184,     0,   191,
     224,   223,   195,   226,     0,     0,   221,   201,   200,     0,
       0,   219,   175,   174,     1,    21,    34,    35,    36,    37,
      64,    65,    63,    62,     0,     0,     0,     0,     0,     0,
       0,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,     0,     0,     0,
       0,   203,     0,     0,   119,   113,   114,   111,    79,    78,
      77,    80,   110,    13,    82,    81,     0,    23,    40,    22,
      27,    32,     0,     0,    30,    16,     0,     0,    99,     0,
      40,    40,    83,    40,     0,   183,   182,   190,     0,     0,
     225,     0,   196,     0,     0,   157,   156,   161,   160,   163,
     162,   167,   166,   169,   168,   171,   170,   179,   178,   151,
     150,   153,   152,   147,   146,   149,   148,   155,   154,   159,
     158,   165,   164,   134,   133,   137,   136,   139,   138,   141,
     140,   143,   142,   145,   144,   177,   176,   212,     0,   205,
     211,   217,     0,   122,   121,     0,   209,     0,   190,   117,
     116,     0,     0,   115,   109,    14,     0,     0,    40,    25,
      28,     0,     0,    17,     0,    40,   100,    94,    98,     0,
      90,     0,   102,     0,     0,    89,    84,    85,   202,    40,
      71,    70,     0,   193,   222,   192,     0,   227,   198,   220,
     197,     0,   216,   215,   206,   204,     0,   208,   207,     0,
     120,    40,   108,   106,    40,     0,    29,    24,    31,    18,
       0,     0,    40,    97,     0,    92,    91,   105,   103,     0,
      40,     0,    86,    40,   132,     0,   194,   199,     0,   218,
     210,    40,    15,    40,    40,     0,    96,    93,    40,    40,
      76,    75,    40,   131,     0,     0,   214,   213,    40,    40,
      19,    40,    87,   129,   123,     0,   130,     0,    40,   128,
     124,   127,   125
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -128,  -128,   174,  -128,  -128,  -111,   -36,    33,   103,  -128,
    -128,  -128,  -128,  -128,  -128,  -128,  -128,  -128,  -128,   -67,
    -105,  -128,  -128,   -30,  -128,  -128,  -128,  -128,  -128,    87,
    -128,  -128,    98,  -128,  -128,  -128,     1,  -128,     0,  -128,
     -39,  -127,   151,   -42,   -20,  -128,   -23
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    26,    27,    28,    66,   153,   154,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,   164,    39,   142,
     148,   255,   256,   257,   301,    40,    70,    41,    42,    59,
     134,   135,   136,   232,    43,    44,    45,    46,    61,   131,
     221,   222,    89,    84,    85,   171,    86
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      47,    91,   225,   160,    90,   161,   162,    60,   277,   235,
     341,   151,    72,    73,    47,   248,   132,   260,    80,    81,
     240,    75,    76,    78,    83,    83,    93,   274,    53,    47,
     336,   226,    55,     3,   261,   237,   323,   339,   286,   282,
    -126,  -126,    52,   169,    56,    57,    94,    74,   127,   320,
     128,   245,    10,   149,     7,   129,   283,   312,   130,    83,
     157,   144,   145,   150,   168,   151,   321,    91,   133,   173,
     236,    80,    81,   276,    47,   276,   278,   249,   342,   217,
      20,    21,   172,   218,   241,   242,    83,   170,   275,   227,
      22,    83,   276,    58,   138,   139,   324,    24,   340,    25,
     287,  -126,   276,   325,   231,   176,   178,   180,   182,   184,
     186,   188,   168,   190,   192,   194,   196,   198,   200,   202,
     204,   206,   208,   210,   212,   214,   216,   152,   220,   224,
     220,   284,    96,   291,    60,   270,   127,   239,    62,   289,
      63,   151,    47,   129,   254,   264,   130,   143,    47,   266,
     269,   144,   145,   303,   271,   140,   141,    67,   247,    68,
      47,    47,   243,    47,    97,    98,   144,   145,   262,    83,
      80,    81,   267,   151,    83,   234,    64,    96,   144,   145,
     313,   238,    80,    81,   285,   314,    80,    81,   242,    54,
     308,    55,     3,   251,   253,   315,   259,   335,   337,   242,
     290,    65,    95,    56,    57,   300,   288,   146,   330,   147,
     331,    10,   144,   145,   110,   111,   155,    69,   156,   158,
     230,   159,   244,    48,    49,    99,   302,   -26,   279,   -26,
     167,    47,   318,   233,    47,    50,    51,   309,    47,    20,
      21,   174,   228,   264,     0,    47,   265,     0,   269,    22,
     294,    47,    58,    47,     0,   299,    24,     0,    25,    47,
     125,   126,     0,   272,   281,   273,   304,    83,   305,     0,
     100,   101,    83,    48,    49,   144,   145,   220,   292,     0,
       0,    47,   102,   103,    47,    50,    51,     0,     0,     0,
       0,     0,    47,     0,     0,     0,     0,     0,     0,     0,
      47,     0,     0,    47,     0,     0,     0,     0,     0,   327,
       0,    47,     0,    47,    47,     0,     0,   311,    47,    47,
       0,     0,    47,     0,     0,   220,   220,     0,    47,    47,
       0,    47,    47,   319,     0,     0,   322,    96,    47,     0,
       0,    96,     0,     0,     0,     0,   328,   329,     0,     0,
       0,   332,     0,     0,    96,     0,    96,     0,     0,     0,
       0,     0,    96,     0,   338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    96,     0,    -4,     1,     0,
       2,     3,   -40,   -40,     0,     0,     0,     0,     0,     0,
       0,     0,     4,     5,    96,     0,     6,     7,     8,     9,
      10,    11,    96,     0,    12,    96,     0,    13,    14,     0,
       0,    96,    96,    15,    16,    96,     0,    17,    18,    19,
       0,    96,   107,   108,   109,   110,   111,     0,    20,    21,
       0,     0,     0,    -9,     0,     0,     2,     3,    22,     0,
       0,    23,     0,   -40,     0,    24,     0,    25,     4,     5,
       0,     0,     6,    -9,     8,     9,    10,    11,     0,     0,
      12,     0,     0,    13,    14,   122,   123,   124,     0,    15,
      16,   125,   126,    17,    18,    19,     0,     0,     0,     0,
       0,     0,     0,     0,    20,    21,     0,     0,     0,    -8,
       0,     0,     2,     3,    22,     0,     0,    23,     0,     0,
       0,    24,     0,    25,     4,     5,     0,     0,     6,    -8,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,   -11,     0,     0,     2,     3,
      22,     0,     0,    23,     0,     0,     0,    24,     0,    25,
       4,     5,     0,     0,     6,   -11,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,     0,     0,    20,    21,     0,     0,
       0,    -7,     0,     0,     2,     3,    22,     0,     0,    23,
       0,     0,     0,    24,     0,    25,     4,     5,     0,     0,
       6,    -7,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,    19,     0,     0,     0,     0,     0,     0,
       0,     0,    20,    21,     0,     0,     0,   -10,     0,     0,
       2,     3,    22,     0,     0,    23,     0,     0,     0,    24,
       0,    25,     4,     5,     0,     0,     6,   -10,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,    -6,     0,     0,     2,     3,    22,     0,
       0,    23,     0,     0,     0,    24,     0,    25,     4,     5,
       0,     0,     6,    -6,     8,     9,    10,    11,     0,     0,
      12,     0,     0,    13,    14,     0,     0,     0,     0,    15,
      16,     0,     0,    17,    18,    19,     0,     0,     0,     0,
       0,     0,     0,    -2,    20,    21,     2,     3,   107,   108,
     109,   110,   111,     0,    22,     0,     0,    23,     4,     5,
       0,    24,     6,    25,     8,     9,    10,    11,     0,     0,
      12,     0,     0,    13,    14,     0,     0,     0,     0,    15,
      16,     0,     0,    17,    18,    19,     0,     0,   119,   120,
     121,   122,   123,   124,    20,    21,     0,   125,   126,     0,
       0,     0,     0,     0,    22,     0,     0,    23,     0,     0,
       0,    24,   295,    25,     2,     3,   -40,   -40,     0,     0,
       0,     0,     0,     0,     0,     0,     4,     5,     0,   296,
       6,     0,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,    19,     0,     0,     0,     0,     0,     0,
       0,     0,    20,    21,     0,     0,     0,     0,     0,     0,
       0,     0,    22,     0,     0,    23,     0,   -40,     0,    24,
     297,    25,     2,     3,   -40,   -40,     0,     0,     0,     0,
       0,     0,     0,     0,     4,     5,     0,   298,     6,     0,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,     0,     0,     0,
      22,     0,     0,    23,     0,   -40,     0,    24,   280,    25,
       2,     3,   -40,   -40,     0,     0,     0,     0,     0,     0,
       0,     0,     4,     5,     0,     0,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,     0,   -74,     0,     2,     3,    22,     0,
       0,    23,     0,   -40,     0,    24,     0,    25,     4,     5,
       0,   -74,     6,     0,     8,     9,    10,    11,     0,     0,
      12,     0,     0,    13,    14,   163,     0,     0,     0,    15,
      16,     0,     0,    17,    18,    19,     0,     0,     0,     0,
       0,     0,     0,     0,    20,    21,     0,     0,     0,     0,
     -73,     0,     2,     3,    22,     0,     0,    23,     0,     0,
       0,    24,     0,    25,     4,     5,     0,   -73,     6,     0,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,  -118,     0,     2,     3,
      22,     0,     0,    23,     0,     0,     0,    24,     0,    25,
       4,     5,     0,  -118,     6,     0,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,     0,     0,    20,    21,     0,     0,
       0,     0,   -88,     0,     2,     3,    22,     0,     0,    23,
       0,     0,     0,    24,     0,    25,     4,     5,     0,   -88,
       6,     0,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,    19,     0,     0,     0,     0,     0,     0,
       0,     0,    20,    21,     0,     0,     0,     0,   -72,     0,
       2,     3,    22,     0,     0,    23,     0,     0,     0,    24,
       0,    25,     4,     5,     0,   -72,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,    19,
      79,     0,    55,     3,    80,    81,     0,     0,    20,    21,
       0,     0,     0,     0,    56,    57,     0,     0,    22,     0,
       0,    23,    10,     0,     0,    24,    87,    25,    55,     3,
      80,    81,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,     0,     0,     0,     0,     0,     0,    10,     0,
      20,    21,     0,     0,     0,     0,   333,     0,    55,     3,
      22,     0,     0,    58,    82,     0,     0,    24,     0,    25,
      56,    57,     0,     0,     0,     0,    20,    21,    10,     0,
       0,     0,    71,     0,    55,     3,    22,     0,     0,    58,
       0,     0,     0,    24,    88,    25,    56,    57,     0,     0,
       0,     0,     0,     0,    10,   217,    20,    21,     0,   218,
       0,     0,    77,     0,    55,     3,    22,   334,     0,    58,
       0,     0,     0,    24,     0,    25,    56,    57,     0,     0,
       0,     0,    20,    21,    10,     0,     0,     0,    92,     0,
      55,     3,    22,     0,     0,    58,     0,     0,     0,    24,
       0,    25,    56,    57,     0,     0,     0,     0,     0,     0,
      10,     0,    20,    21,     0,     0,     0,     0,   175,     0,
      55,     3,    22,     0,     0,    58,     0,     0,     0,    24,
       0,    25,    56,    57,     0,     0,     0,     0,    20,    21,
      10,     0,     0,     0,   177,     0,    55,     3,    22,     0,
       0,    58,     0,     0,     0,    24,     0,    25,    56,    57,
       0,     0,     0,     0,     0,     0,    10,     0,    20,    21,
       0,     0,     0,     0,   179,     0,    55,     3,    22,     0,
       0,    58,     0,     0,     0,    24,     0,    25,    56,    57,
       0,     0,     0,     0,    20,    21,    10,     0,     0,     0,
     181,     0,    55,     3,    22,     0,     0,    58,     0,     0,
       0,    24,     0,    25,    56,    57,     0,     0,     0,     0,
       0,     0,    10,     0,    20,    21,     0,     0,     0,     0,
     183,     0,    55,     3,    22,     0,     0,    58,     0,     0,
       0,    24,     0,    25,    56,    57,     0,     0,     0,     0,
      20,    21,    10,     0,     0,     0,   185,     0,    55,     3,
      22,     0,     0,    58,     0,     0,     0,    24,     0,    25,
      56,    57,     0,     0,     0,     0,     0,     0,    10,     0,
      20,    21,     0,     0,     0,     0,   187,     0,    55,     3,
      22,     0,     0,    58,     0,     0,     0,    24,     0,    25,
      56,    57,     0,     0,     0,     0,    20,    21,    10,     0,
       0,     0,   189,     0,    55,     3,    22,     0,     0,    58,
       0,     0,     0,    24,     0,    25,    56,    57,     0,     0,
       0,     0,     0,     0,    10,     0,    20,    21,     0,     0,
       0,     0,   191,     0,    55,     3,    22,     0,     0,    58,
       0,     0,     0,    24,     0,    25,    56,    57,     0,     0,
       0,     0,    20,    21,    10,     0,     0,     0,   193,     0,
      55,     3,    22,     0,     0,    58,     0,     0,     0,    24,
       0,    25,    56,    57,     0,     0,     0,     0,     0,     0,
      10,     0,    20,    21,     0,     0,     0,     0,   195,     0,
      55,     3,    22,     0,     0,    58,     0,     0,     0,    24,
       0,    25,    56,    57,     0,     0,     0,     0,    20,    21,
      10,     0,     0,     0,   197,     0,    55,     3,    22,     0,
       0,    58,     0,     0,     0,    24,     0,    25,    56,    57,
       0,     0,     0,     0,     0,     0,    10,     0,    20,    21,
       0,     0,     0,     0,   199,     0,    55,     3,    22,     0,
       0,    58,     0,     0,     0,    24,     0,    25,    56,    57,
       0,     0,     0,     0,    20,    21,    10,     0,     0,     0,
     201,     0,    55,     3,    22,     0,     0,    58,     0,     0,
       0,    24,     0,    25,    56,    57,     0,     0,     0,     0,
       0,     0,    10,     0,    20,    21,     0,     0,     0,     0,
     203,     0,    55,     3,    22,     0,     0,    58,     0,     0,
       0,    24,     0,    25,    56,    57,     0,     0,     0,     0,
      20,    21,    10,     0,     0,     0,   205,     0,    55,     3,
      22,     0,     0,    58,     0,     0,     0,    24,     0,    25,
      56,    57,     0,     0,     0,     0,     0,     0,    10,     0,
      20,    21,     0,     0,     0,     0,   207,     0,    55,     3,
      22,     0,     0,    58,     0,     0,     0,    24,     0,    25,
      56,    57,     0,     0,     0,     0,    20,    21,    10,     0,
       0,     0,   209,     0,    55,     3,    22,     0,     0,    58,
       0,     0,     0,    24,     0,    25,    56,    57,     0,     0,
       0,     0,     0,     0,    10,     0,    20,    21,     0,     0,
       0,     0,   211,     0,    55,     3,    22,     0,     0,    58,
       0,     0,     0,    24,     0,    25,    56,    57,     0,     0,
       0,     0,    20,    21,    10,     0,     0,     0,   213,     0,
      55,     3,    22,     0,     0,    58,     0,     0,     0,    24,
       0,    25,    56,    57,     0,     0,     0,     0,     0,     0,
      10,     0,    20,    21,     0,     0,     0,     0,   215,     0,
      55,     3,    22,     0,     0,    58,     0,     0,     0,    24,
       0,    25,    56,    57,     0,     0,     0,     0,    20,    21,
      10,     0,     0,     0,   223,     0,    55,     3,    22,     0,
       0,    58,     0,     0,     0,    24,     0,    25,    56,    57,
       0,     0,     0,     0,     0,     0,    10,     0,    20,    21,
       0,     0,     0,     0,   229,     0,    55,     3,    22,     0,
       0,    58,     0,     0,     0,    24,     0,    25,    56,    57,
       0,     0,     0,     0,    20,    21,    10,     0,     0,     0,
     246,     0,    55,     3,    22,     0,     0,    58,     0,     0,
       0,    24,     0,    25,    56,    57,     0,     0,     0,     0,
       0,     0,    10,     0,    20,    21,     0,    55,     3,     0,
     293,     0,    55,     3,    22,     0,     0,    58,     0,    56,
      57,    24,     0,    25,    56,    57,     0,    10,     0,     0,
      20,    21,    10,     0,     0,     0,   326,     0,    55,     3,
      22,     0,     0,    58,     0,     0,     0,    24,     0,    25,
      56,    57,     0,     0,     0,    20,    21,     0,    10,     0,
      20,    21,     0,     0,     0,    22,     0,     0,    58,   263,
      22,     0,    24,    58,    25,     0,     0,    24,     0,    25,
       0,     0,     0,     0,     0,     0,    20,    21,     2,     3,
     -40,   -40,     0,     0,     0,     0,    22,     0,     0,    58,
       4,     5,     0,    24,     6,    25,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,    19,     0,     0,
       0,     0,     0,     0,     0,     0,    20,    21,     0,     0,
       0,     0,     0,     0,     2,     3,    22,     0,     0,    23,
       0,   -40,     0,    24,     0,    25,     4,     5,     0,   250,
       6,     0,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,    19,     0,     0,     0,     0,     0,     0,
       0,     0,    20,    21,     0,     0,     0,     0,     0,     0,
       2,     3,    22,     0,     0,    23,     0,     0,     0,    24,
       0,    25,     4,     5,     0,   252,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       2,     3,     0,     0,     0,     0,     0,     0,    22,     0,
       0,    23,     4,     5,     0,    24,     6,    25,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
     258,     3,     0,     0,     0,     0,     0,     0,    22,     0,
       0,    23,     4,     5,     0,    24,     6,    25,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
      55,     3,     0,    15,    16,     0,     0,    17,    18,    19,
       0,     0,    56,    57,     0,     0,     0,     0,    20,    21,
      10,    55,     3,     0,     0,     0,    55,     3,    22,     0,
       0,    23,     0,    56,    57,    24,     0,    25,    56,    57,
       0,    10,     0,     0,     0,     0,    10,   217,    20,    21,
       0,   218,     0,     0,     0,     0,     0,     0,    22,   219,
       0,    58,     0,     0,     0,    24,     0,    25,   217,    20,
      21,     0,   218,     0,    20,    21,     0,    55,     3,    22,
       0,     0,    58,     0,    22,     0,    24,    58,    25,    56,
      57,    24,   268,    25,     0,     0,     0,    10,    55,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,    57,     0,     0,     0,     0,     0,     0,    10,     0,
      55,     3,     0,     0,     0,    20,    21,     0,     0,     0,
       0,     0,    56,    57,     0,    22,     0,     0,    58,   306,
      10,     0,    24,     0,    25,     0,    20,    21,     0,     0,
       0,     0,     0,     0,     0,     0,    22,     0,     0,    58,
       0,     0,     0,    24,   307,    25,     0,     0,    20,    21,
       0,     0,     0,     0,     0,     0,     0,     0,    22,     0,
       0,    58,     0,     0,   137,    24,     0,    25,   138,   139,
     104,   105,   106,   107,   108,   109,   110,   111,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   138,   139,   104,   105,   106,   107,   108,
     109,   110,   111,     0,     0,     0,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,     0,
       0,     0,   125,   126,   107,   108,   109,   110,   111,   140,
     141,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,     0,     0,     0,   125,   126,     0,
     165,     0,     0,     0,   140,   141,   104,   105,   106,   107,
     108,   109,   110,   111,     0,   120,   121,   122,   123,   124,
     316,     0,     0,   125,   126,     0,   104,   105,   106,   107,
     108,   109,   110,   111,     0,     0,     0,     0,     0,     0,
       0,     0,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,     0,     0,     0,   125,   126,
       0,   166,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,     0,     0,     0,   125,   126,
       0,   317,   104,   105,   106,   107,   108,   109,   110,   111,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   104,   105,   106,   107,   108,   109,   110,   111,
       0,     0,     0,     0,     0,     0,     0,     0,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,     0,     0,     0,   125,   126,     0,   310,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,     0,     0,     0,   125,   126,   104,   105,   106,   107,
     108,   109,   110,   111,     0,     0,     0,     0,     0,     0,
       0,     0,   104,   105,   106,   107,   108,   109,   110,   111,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,     0,     0,     0,   125,   126,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
     124,     0,     0,     0,   125,   126,   104,   105,   106,   107,
     108,   109,   110,   111,     0,     0,     0,     0,     0,     0,
     104,   105,   106,   107,   108,   109,   110,   111,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,     0,     0,     0,   125,   126,
     116,   117,   118,   119,   120,   121,   122,   123,   124,     0,
       0,     0,   125,   126
};

static const yytype_int16 yycheck[] =
{
       0,    24,   129,    70,    24,    72,    73,     6,     1,     1,
       1,     3,    11,    12,    14,     1,    58,     1,     5,     6,
       1,    20,    21,    22,    23,    24,    25,     1,    16,    29,
       1,     3,     3,     4,    18,   146,     1,     1,     1,     1,
       5,     6,    15,    85,    15,    16,     0,    14,    61,     1,
      63,   156,    23,    63,    20,    68,    18,     1,    71,    58,
      63,     5,     6,     1,    84,     3,    18,    90,    26,    89,
      62,     5,     6,    66,    74,    66,    69,    63,    69,    50,
      51,    52,    69,    54,    65,    66,    85,    66,    62,    61,
      61,    90,    66,    64,     5,     6,    61,    68,    62,    70,
      63,    66,    66,    68,    25,   104,   105,   106,   107,   108,
     109,   110,   132,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,    65,   127,   128,
     129,   236,    29,   244,   133,    69,    61,    63,     1,     1,
       3,     3,   142,    68,    28,   168,    71,     1,   148,   169,
     173,     5,     6,   258,   174,    66,    67,     1,   157,     3,
     160,   161,     1,   163,     5,     6,     5,     6,    63,   168,
       5,     6,   171,     3,   173,   142,    39,    74,     5,     6,
     285,   148,     5,     6,    62,   290,     5,     6,    66,     1,
      63,     3,     4,   160,   161,    62,   163,   324,   325,    66,
      62,    64,    28,    15,    16,    29,   242,    61,     1,    63,
     315,    23,     5,     6,    13,    14,     1,    61,     3,     1,
     133,     3,    61,     3,     4,    66,   256,     1,   227,     3,
      65,   231,   299,   135,   234,    15,    16,   276,   238,    51,
      52,    90,    65,   266,    -1,   245,    65,    -1,   271,    61,
     249,   251,    64,   253,    -1,   254,    68,    -1,    70,   259,
      59,    60,    -1,     1,   231,     3,     1,   266,     3,    -1,
       3,     4,   271,     3,     4,     5,     6,   276,   245,    -1,
      -1,   281,    15,    16,   284,    15,    16,    -1,    -1,    -1,
      -1,    -1,   292,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,    -1,   303,    -1,    -1,    -1,    -1,    -1,   308,
      -1,   311,    -1,   313,   314,    -1,    -1,   284,   318,   319,
      -1,    -1,   322,    -1,    -1,   324,   325,    -1,   328,   329,
      -1,   331,   332,   300,    -1,    -1,   303,   234,   338,    -1,
      -1,   238,    -1,    -1,    -1,    -1,   313,   314,    -1,    -1,
      -1,   318,    -1,    -1,   251,    -1,   253,    -1,    -1,    -1,
      -1,    -1,   259,    -1,   331,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   281,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   292,    -1,     0,     1,    -1,
       3,     4,     5,     6,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,   311,    -1,    19,    20,    21,    22,
      23,    24,   319,    -1,    27,   322,    -1,    30,    31,    -1,
      -1,   328,   329,    36,    37,   332,    -1,    40,    41,    42,
      -1,   338,    10,    11,    12,    13,    14,    -1,    51,    52,
      -1,    -1,    -1,     0,    -1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    66,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    19,    20,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    53,    54,    55,    -1,    36,
      37,    59,    60,    40,    41,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,     0,
      -1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    -1,    19,    20,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,     0,    -1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    19,    20,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,     0,    -1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    -1,
      19,    20,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,     0,    -1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    19,    20,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,     0,    -1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    19,    20,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     0,    51,    52,     3,     4,    10,    11,
      12,    13,    14,    -1,    61,    -1,    -1,    64,    15,    16,
      -1,    68,    19,    70,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    42,    -1,    -1,    50,    51,
      52,    53,    54,    55,    51,    52,    -1,    59,    60,    -1,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,     1,    70,     3,     4,     5,     6,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    66,    -1,    68,
       1,    70,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      61,    -1,    -1,    64,    -1,    66,    -1,    68,     1,    70,
       3,     4,     5,     6,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    66,    -1,    68,    -1,    70,    15,    16,
      -1,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    32,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
       1,    -1,     3,     4,     5,     6,    -1,    -1,    51,    52,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    61,    -1,
      -1,    64,    23,    -1,    -1,    68,     1,    70,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      51,    52,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    65,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    -1,    -1,    51,    52,    23,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    69,    70,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    50,    51,    52,    -1,    54,
      -1,    -1,     1,    -1,     3,     4,    61,    62,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    -1,
      -1,    -1,    51,    52,    23,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    -1,    -1,    51,    52,
      23,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    51,    52,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    -1,    -1,    51,    52,    23,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    -1,    51,    52,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    -1,    -1,    -1,
      51,    52,    23,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      51,    52,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    -1,    -1,    51,    52,    23,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    51,    52,    -1,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    -1,
      -1,    -1,    51,    52,    23,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    -1,    -1,    51,    52,
      23,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    51,    52,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    -1,    -1,    51,    52,    23,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    -1,    51,    52,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    -1,    -1,    -1,
      51,    52,    23,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      51,    52,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    -1,    -1,    51,    52,    23,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    23,    -1,    51,    52,    -1,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    -1,    70,    15,    16,    -1,    -1,
      -1,    -1,    51,    52,    23,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    -1,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    -1,    -1,    -1,    51,    52,
      23,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    51,    52,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    15,    16,
      -1,    -1,    -1,    -1,    51,    52,    23,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    -1,
      -1,    68,    -1,    70,    15,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    23,    -1,    51,    52,    -1,     3,     4,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    15,
      16,    68,    -1,    70,    15,    16,    -1,    23,    -1,    -1,
      51,    52,    23,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      15,    16,    -1,    -1,    -1,    51,    52,    -1,    23,    -1,
      51,    52,    -1,    -1,    -1,    61,    -1,    -1,    64,    65,
      61,    -1,    68,    64,    70,    -1,    -1,    68,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
      15,    16,    -1,    68,    19,    70,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    66,    -1,    68,    -1,    70,    15,    16,    -1,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    -1,    -1,    68,
      -1,    70,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
       3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    64,    15,    16,    -1,    68,    19,    70,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
       3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    64,    15,    16,    -1,    68,    19,    70,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
       3,     4,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    15,    16,    -1,    -1,    -1,    -1,    51,    52,
      23,     3,     4,    -1,    -1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    15,    16,    68,    -1,    70,    15,    16,
      -1,    23,    -1,    -1,    -1,    -1,    23,    50,    51,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    61,    62,
      -1,    64,    -1,    -1,    -1,    68,    -1,    70,    50,    51,
      52,    -1,    54,    -1,    51,    52,    -1,     3,     4,    61,
      -1,    -1,    64,    -1,    61,    -1,    68,    64,    70,    15,
      16,    68,    69,    70,    -1,    -1,    -1,    23,     3,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
       3,     4,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
      -1,    -1,    15,    16,    -1,    61,    -1,    -1,    64,    65,
      23,    -1,    68,    -1,    70,    -1,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
      -1,    -1,    -1,    68,    69,    70,    -1,    -1,    51,    52,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    64,    -1,    -1,     1,    68,    -1,    70,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    -1,    -1,    -1,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    -1,
      -1,    -1,    59,    60,    10,    11,    12,    13,    14,    66,
      67,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    -1,    -1,    -1,    59,    60,    -1,
       1,    -1,    -1,    -1,    66,    67,     7,     8,     9,    10,
      11,    12,    13,    14,    -1,    51,    52,    53,    54,    55,
       1,    -1,    -1,    59,    60,    -1,     7,     8,     9,    10,
      11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    -1,    -1,    59,    60,
      -1,    62,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    -1,    -1,    59,    60,
      -1,    62,     7,     8,     9,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     7,     8,     9,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    59,    60,    -1,    62,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    59,    60,     7,     8,     9,    10,
      11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     7,     8,     9,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    -1,    -1,    59,    60,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    59,    60,     7,     8,     9,    10,
      11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    -1,    -1,    59,    60,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    -1,
      -1,    -1,    59,    60
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,     4,    15,    16,    19,    20,    21,    22,
      23,    24,    27,    30,    31,    36,    37,    40,    41,    42,
      51,    52,    61,    64,    68,    70,    73,    74,    75,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    90,
      97,    99,   100,   106,   107,   108,   109,   110,     3,     4,
      15,    16,    15,    16,     1,     3,    15,    16,    64,   101,
     108,   110,     1,     3,    39,    64,    76,     1,     3,    61,
      98,     1,   108,   108,    79,   108,   108,     1,   108,     1,
       5,     6,    65,   108,   115,   116,   118,     1,    69,   114,
     116,   118,     1,   108,     0,    74,    80,     5,     6,    66,
       3,     4,    15,    16,     7,     8,     9,    10,    11,    12,
      13,    14,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    59,    60,    61,    63,    68,
      71,   111,   115,    26,   102,   103,   104,     1,     5,     6,
      66,    67,    91,     1,     5,     6,    61,    63,    92,    63,
       1,     3,    65,    77,    78,     1,     3,    63,     1,     3,
      91,    91,    91,    32,    89,     1,    62,    65,   116,   115,
      66,   117,    69,   116,   114,     1,   108,     1,   108,     1,
     108,     1,   108,     1,   108,     1,   108,     1,   108,     1,
     108,     1,   108,     1,   108,     1,   108,     1,   108,     1,
     108,     1,   108,     1,   108,     1,   108,     1,   108,     1,
     108,     1,   108,     1,   108,     1,   108,    50,    54,    62,
     108,   112,   113,     1,   108,   113,     3,    61,    65,     1,
     101,    25,   105,   104,    79,     1,    62,    77,    79,    63,
       1,    65,    66,     1,    61,    92,     1,   108,     1,    63,
      18,    79,    18,    79,    28,    93,    94,    95,     3,    79,
       1,    18,    63,    65,   118,    65,   116,   108,    69,   118,
      69,   116,     1,     3,     1,    62,    66,     1,    69,   108,
       1,    79,     1,    18,    92,    62,     1,    63,    78,     1,
      62,    77,    79,     1,   108,     1,    18,     1,    18,   108,
      29,    96,    95,    92,     1,     3,    65,    69,    63,   112,
      62,    79,     1,    92,    92,    62,     1,    62,    91,    79,
       1,    18,    79,     1,    61,    68,     1,   108,    79,    79,
       1,    92,    79,     1,    62,   113,     1,   113,    79,     1,
      62,     1,    69
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    72,    73,    73,    73,    73,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      75,    75,    76,    76,    76,    76,    76,    76,    76,    76,
      77,    77,    78,    79,    79,    80,    80,    80,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    82,    83,    83,    83,    83,
      83,    83,    83,    83,    83,    83,    84,    85,    86,    87,
      88,    88,    89,    89,    89,    90,    90,    91,    91,    91,
      91,    92,    92,    93,    93,    94,    94,    95,    96,    96,
      97,    97,    97,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    99,    99,    99,    99,   100,   100,   100,   101,
     101,   101,   102,   102,   103,   103,   104,   104,   105,   105,
     105,   106,   106,   107,   107,   107,   107,   107,   107,   107,
     107,   107,   107,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   109,   109,   109,   109,   109,
     109,   109,   109,   109,   109,   109,   109,   109,   109,   109,
     109,   109,   110,   110,   111,   111,   111,   111,   111,   111,
     111,   112,   112,   112,   112,   112,   112,   113,   113,   114,
     114,   115,   115,   116,   116,   117,   118,   118
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     0,     1,     8,     7,     5,     4,
       7,     6,     2,     3,     4,     6,     3,     4,     5,     7,
       1,     2,     2,     2,     4,     3,     2,     2,     3,     4,
       1,     3,     1,     1,     2,     2,     2,     2,     1,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     1,     1,     1,
       4,     4,     4,     2,     0,     6,     6,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     4,     2,     0,
       4,     5,     5,     5,     3,     1,     5,     4,     3,     2,
       3,     1,     4,     5,     2,     5,     5,     2,     5,     3,
       2,     2,     0,     1,     1,     2,     2,     2,     2,     0,
       2,     3,     3,     7,     8,     8,     5,     8,     8,     7,
       7,     6,     5,     3,     3,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     2,     2,     3,     3,     3,     3,
       2,     2,     3,     3,     2,     1,     1,     1,     1,     1,
       3,     2,     4,     4,     5,     2,     3,     4,     4,     5,
       2,     2,     1,     2,     3,     2,     3,     3,     3,     2,
       4,     1,     1,     4,     4,     2,     2,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     3
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: statementList  */
#line 71 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                { setParsedScriptBlock(yyvsp[0].v.p);}
#line 2090 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 5: /* program: error  */
#line 73 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
          {yyxpt(_("statement list or function definition"),yyvsp[0]);}
#line 2096 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 6: /* functionDef: FUNCTION returnDeclaration IDENT '(' argumentList ')' newLine statementList  */
#line 77 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                                {
   ParseRHS lhsRhs = yyvsp[-6];
   ParseRHS nameRhs = yyvsp[-5];
   ParseRHS rhsRhs = yyvsp[-3];
   ParseRHS codeRhs = yyvsp[0];
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
  }
#line 2108 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 7: /* functionDef: FUNCTION IDENT '(' argumentList ')' newLine statementList  */
#line 84 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                              {
   ParseRHS lhsRhs;
   lhsRhs.v.p = nullptr;
   ParseRHS nameRhs = yyvsp[-5];
   ParseRHS rhsRhs = yyvsp[-3];
   ParseRHS codeRhs = yyvsp[0];
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 2121 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 8: /* functionDef: FUNCTION returnDeclaration IDENT newLine statementList  */
#line 92 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                           {
    ParseRHS lhsRhs = yyvsp[-3];
    ParseRHS nameRhs = yyvsp[-2];
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yyvsp[0];
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 2134 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 9: /* functionDef: FUNCTION IDENT newLine statementList  */
#line 100 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                         {
    ParseRHS lhsRhs;
    lhsRhs.v.p = nullptr;
    ParseRHS nameRhs = yyvsp[-2];
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yyvsp[0];
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 2148 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 10: /* functionDef: FUNCTION returnDeclaration IDENT '(' ')' newLine statementList  */
#line 109 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                   {
    ParseRHS lhsRhs = yyvsp[-5];
    ParseRHS nameRhs = yyvsp[-4];
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yyvsp[0];
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 2161 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 11: /* functionDef: FUNCTION IDENT '(' ')' newLine statementList  */
#line 117 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                 {
    ParseRHS lhsRhs;
    lhsRhs.v.p = nullptr;
    ParseRHS nameRhs = yyvsp[-4];
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yyvsp[0];
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 2175 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 12: /* functionDef: FUNCTION error  */
#line 126 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("legal function name or return declaration after 'function'"), yyvsp[-1]);}
#line 2181 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 13: /* functionDef: FUNCTION IDENT error  */
#line 127 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {yyxpt(_("argument list or statement list after identifier '") +
  yyvsp[-1].v.p->text.c_str() + "'",yyvsp[-1]);}
#line 2188 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 14: /* functionDef: FUNCTION IDENT '(' error  */
#line 129 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("(possibly empty) argument list after '('"),yyvsp[-1]);}
#line 2194 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 15: /* functionDef: FUNCTION IDENT '(' argumentList ')' error  */
#line 130 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                              {yyxpt(_("statement list after ')'"),yyvsp[-1]);}
#line 2200 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 16: /* functionDef: FUNCTION returnDeclaration error  */
#line 131 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {yyxpt(_("function name for function declared"),yyvsp[-2]);}
#line 2206 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 17: /* functionDef: FUNCTION returnDeclaration IDENT error  */
#line 132 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                           {yyxpt(_("argument list or statement list following function name :") +
  yyvsp[-1].v.p->text.c_str(), yyvsp[-1]);}
#line 2213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 18: /* functionDef: FUNCTION returnDeclaration IDENT '(' error  */
#line 134 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                {yyxpt(_("(possibly empty) argument list after '('"),yyvsp[-1]);}
#line 2219 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 19: /* functionDef: FUNCTION returnDeclaration IDENT '(' argumentList ')' error  */
#line 135 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                {yyxpt(_("statement list after ')'"),yyvsp[-1]);}
#line 2225 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 22: /* returnDeclaration: VARARGOUT '='  */
#line 144 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = yyvsp[-1].v.p;}
#line 2231 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 23: /* returnDeclaration: IDENT '='  */
#line 145 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = yyvsp[-1].v.p;}
#line 2237 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 24: /* returnDeclaration: '[' argumentList ']' '='  */
#line 146 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                             {yyval.v.p = yyvsp[-2].v.p;}
#line 2243 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 25: /* returnDeclaration: '[' ']' '='  */
#line 147 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = nullptr;}
#line 2249 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 26: /* returnDeclaration: IDENT error  */
#line 148 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyxpt(_("an '=' symbol after identifier in return declaration"),yyvsp[-1]);}
#line 2255 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 27: /* returnDeclaration: '[' error  */
#line 149 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("a valid list of return arguments in return declaration"),yyvsp[-1]);}
#line 2261 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 28: /* returnDeclaration: '[' argumentList error  */
#line 150 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyxpt(_("matching ']' in return declaration for '['"),yyvsp[-2]);}
#line 2267 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 29: /* returnDeclaration: '[' argumentList ']' error  */
#line 151 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                               {yyxpt(_("an '=' symbol after return declaration"),yyvsp[-1]);}
#line 2273 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 30: /* argumentList: argument  */
#line 155 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyval.v.p = yyvsp[0].v.p;}
#line 2279 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 31: /* argumentList: argumentList ',' argument  */
#line 156 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2285 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 33: /* statementList: statement  */
#line 165 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyval.v.p = AbstractSyntaxTree::createNode(OP_BLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 2291 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 34: /* statementList: statementList statement  */
#line 166 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2297 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 35: /* statement: statementType ENDQSTMNT  */
#line 170 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {
        yyval.v.p = AbstractSyntaxTree::createNode(OP_QSTATEMENT,NULL,yyvsp[0].v.i);
      yyval.v.p->down = yyvsp[-1].v.p;
   }
#line 2306 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 36: /* statement: statementType ENDSTMNT  */
#line 174 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                            {
      yyval.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yyvsp[0].v.i);
            yyval.v.p->down = yyvsp[-1].v.p;
   }
#line 2315 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 37: /* statement: statementType ','  */
#line 178 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {
      yyval.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yyvsp[0].v.i);
      yyval.v.p->down = yyvsp[-1].v.p;
   }
#line 2324 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 40: /* statementType: %empty  */
#line 187 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
     {yyval.v.p = AbstractSyntaxTree::createNode(null_node,"",-1);}
#line 2330 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 56: /* specialSyntaxStatement: IDENT NUMERIC  */
#line 208 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2336 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 57: /* specialSyntaxStatement: STRING STRING  */
#line 209 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2342 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 58: /* specialSyntaxStatement: CHARACTER CHARACTER  */
#line 210 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2348 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 59: /* specialSyntaxStatement: IDENT STRING  */
#line 211 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2354 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 60: /* specialSyntaxStatement: IDENT CHARACTER  */
#line 212 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2360 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 61: /* specialSyntaxStatement: IDENT IDENT  */
#line 213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext()); }
#line 2366 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 62: /* specialSyntaxStatement: specialSyntaxStatement STRING  */
#line 214 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2372 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 63: /* specialSyntaxStatement: specialSyntaxStatement CHARACTER  */
#line 215 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2378 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 64: /* specialSyntaxStatement: specialSyntaxStatement IDENT  */
#line 216 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                               {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2384 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 65: /* specialSyntaxStatement: specialSyntaxStatement NUMERIC  */
#line 217 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2390 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 70: /* tryStatement: TRY statementList optionalCatch END  */
#line 236 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {
    yyval.v.p = yyvsp[-3].v.p;
    yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2400 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 71: /* tryStatement: TRY statementList optionalCatch error  */
#line 242 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyxpt(_("matching 'end' to 'try' clause from line ") + decodeline(yyvsp[-3]),yyvsp[0]);}
#line 2406 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 72: /* optionalCatch: CATCH IDENT newLine statementList  */
#line 246 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                    {
    yyval.v.p = yyvsp[-2].v.p;
    yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2415 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 73: /* optionalCatch: CATCH statementList  */
#line 250 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyval.v.p = yyvsp[0].v.p;}
#line 2421 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 74: /* optionalCatch: %empty  */
#line 251 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
    {yyval.v.p = nullptr;}
#line 2427 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 75: /* switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause END  */
#line 255 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                  {
    yyval.v.p = yyvsp[-5].v.p;
    yyval.v.p->addChild(yyvsp[-4].v.p);
    if (yyvsp[-2].v.p != nullptr) yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2438 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 76: /* switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause error  */
#line 261 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                    {
          yyxpt(_("matching 'end' to 'switch' clause from line ") + decodeline(yyvsp[-5]),yyvsp[0]);
        }
#line 2446 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 83: /* caseBlock: %empty  */
#line 276 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyval.v.p = nullptr;}
#line 2452 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 85: /* caseList: caseStatement  */
#line 280 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_CASEBLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());
  }
#line 2460 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 86: /* caseList: caseList caseStatement  */
#line 283 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {
    yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2468 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 87: /* caseStatement: CASE expr optionalEndStatement statementList  */
#line 289 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                               {
    yyval.v.p = yyvsp[-3].v.p; yyval.v.p->addChild(yyvsp[-2].v.p); yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2476 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 88: /* otherwiseClause: OTHERWISE statementList  */
#line 295 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2484 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 89: /* otherwiseClause: %empty  */
#line 298 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {
    yyval.v.p = nullptr;
  }
#line 2492 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 90: /* forStatement: FOR forIndexExpression optionalEndStatement END  */
#line 304 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                  {
   yyval.v.p = nullptr;
  }
#line 2500 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 91: /* forStatement: FOR forIndexExpression optionalEndStatement statementList END  */
#line 307 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2510 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 92: /* forStatement: FOR forIndexExpression optionalEndStatement statementList error  */
#line 313 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyxpt(_("'end' to match 'for' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2516 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 93: /* forIndexExpression: '(' IDENT '=' expr ')'  */
#line 317 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {yyval.v.p = yyvsp[-3].v.p; yyval.v.p->addChild(yyvsp[-1].v.p);}
#line 2522 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 94: /* forIndexExpression: IDENT '=' expr  */
#line 318 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                 {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2528 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 95: /* forIndexExpression: IDENT  */
#line 319 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyval.v.p = yyvsp[0].v.p;
        yyval.v.p->addChild(AbstractSyntaxTree::createNode(OP_RHS, AbstractSyntaxTree::createNode(id_node,yyvsp[0].v.p->text.c_str(), yyvsp[0].v.p->getContext()),yyvsp[0].v.p->getContext())); }
#line 2535 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 96: /* forIndexExpression: '(' IDENT '=' expr error  */
#line 321 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyxpt(_("matching right parenthesis"),yyvsp[-4]);}
#line 2541 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 97: /* forIndexExpression: '(' IDENT '=' error  */
#line 322 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyxpt(_("indexing expression"),yyvsp[-1]);}
#line 2547 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 98: /* forIndexExpression: '(' IDENT error  */
#line 323 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyxpt(_("equals operator after loop index"),yyvsp[-1]);}
#line 2553 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 99: /* forIndexExpression: '(' error  */
#line 324 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyxpt(_("identifier that is the loop variable"),yyvsp[-1]);}
#line 2559 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 100: /* forIndexExpression: IDENT '=' error  */
#line 325 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyxpt(_("indexing expression"),yyvsp[-1]);}
#line 2565 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 101: /* forIndexExpression: error  */
#line 326 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyxpt(_("identifier or assignment (id = expr) after 'for' "),yyvsp[0]);}
#line 2571 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 102: /* whileStatement: WHILE expr optionalEndStatement END  */
#line 330 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {
    yyval.v.p = nullptr;
  }
#line 2579 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 103: /* whileStatement: WHILE expr optionalEndStatement statementList END  */
#line 333 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2589 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 104: /* whileStatement: WHILE error  */
#line 338 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("test expression after 'while'"),yyvsp[-1]);}
#line 2595 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 105: /* whileStatement: WHILE expr optionalEndStatement statementList error  */
#line 340 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyxpt(_("'end' to match 'while' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2601 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 106: /* ifStatement: IF conditionedStatement elseIfBlock elseStatement END  */
#line 344 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                        {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    if (yyvsp[-2].v.p != nullptr) yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2612 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 107: /* ifStatement: IF error  */
#line 350 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyxpt(_("condition expression for 'if'"),yyvsp[-1]);}
#line 2618 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 108: /* ifStatement: IF conditionedStatement elseIfBlock elseStatement error  */
#line 351 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                          {yyxpt(_("'end' to match 'if' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2624 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 109: /* conditionedStatement: expr optionalEndStatement statementList  */
#line 355 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                          {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-2].v.p->getContext());
  }
#line 2632 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 110: /* conditionedStatement: expr optionalEndStatement  */
#line 358 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                            {
	  yyval.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yyvsp[-1].v.p,yyvsp[-1].v.p->getContext());
	}
#line 2640 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 111: /* conditionedStatement: expr error  */
#line 361 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyxpt("valid list of statements after condition",yyvsp[0]);}
#line 2646 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 112: /* elseIfBlock: %empty  */
#line 365 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = nullptr;}
#line 2652 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 114: /* elseIfStatementList: elseIfStatement  */
#line 370 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_ELSEIFBLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());
  }
#line 2660 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 115: /* elseIfStatementList: elseIfStatementList elseIfStatement  */
#line 373 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {
    yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2668 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 116: /* elseIfStatement: ELSEIF conditionedStatement  */
#line 379 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2676 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 117: /* elseIfStatement: ELSEIF error  */
#line 382 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt(_("test condition for 'elseif' clause"),yyvsp[-1]);}
#line 2682 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 118: /* elseStatement: ELSE statementList  */
#line 385 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                     {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2690 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 119: /* elseStatement: %empty  */
#line 388 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = nullptr;}
#line 2696 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 120: /* elseStatement: ELSE error  */
#line 389 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyxpt(_("statement list for 'else' clause"),yyvsp[-1]);}
#line 2702 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 121: /* assignmentStatement: symbRefList '=' expr  */
#line 393 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_ASSIGN,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2708 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 122: /* assignmentStatement: symbRefList '=' error  */
#line 394 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyxpt(_("expression in assignment"),yyvsp[-1]);}
#line 2714 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 123: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' ')'  */
#line 398 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                        {
  yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-5].v.p,yyvsp[-2].v.p,yyvsp[-6].v.i);
  }
#line 2722 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 124: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList ')'  */
#line 401 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyvsp[-3].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-6].v.p,yyvsp[-3].v.p,yyvsp[-7].v.i);
  }
#line 2731 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 125: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList '}'  */
#line 405 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyvsp[-3].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-6].v.p,yyvsp[-3].v.p,yyvsp[-7].v.i);
  }
#line 2740 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 126: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT  */
#line 409 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {
    yyvsp[0].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,NULL,-1));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-3].v.p,yyvsp[0].v.p,yyvsp[-4].v.i);
  }
#line 2749 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 127: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList error  */
#line 413 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {yyxpt(_("matching right bracket"), yyvsp[-2]);}
#line 2755 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 128: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList error  */
#line 414 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {yyxpt(_("matching right parenthesis"), yyvsp[-2]);}
#line 2761 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 129: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' error  */
#line 415 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                            {yyxpt(_("indexing list"), yyvsp[-1]);}
#line 2767 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 130: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' error  */
#line 416 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                            {yyxpt(_("indexing list"), yyvsp[-1]);}
#line 2773 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 131: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT error  */
#line 417 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                       {yyxpt(_("left parenthesis"),yyvsp[-1]);}
#line 2779 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 132: /* multiFunctionCall: '[' matrixDef ']' '=' error  */
#line 418 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyxpt("identifier",yyvsp[-1]);}
#line 2785 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 133: /* expr: expr ':' expr  */
#line 422 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_COLON,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2791 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 134: /* expr: expr ':' error  */
#line 423 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after ':'"), yyvsp[-1]);}
#line 2797 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 136: /* expr: expr '+' expr  */
#line 425 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_PLUS,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2803 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 137: /* expr: expr '+' error  */
#line 426 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '+'"), yyvsp[-1]);}
#line 2809 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 138: /* expr: expr '-' expr  */
#line 427 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SUBTRACT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2815 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 139: /* expr: expr '-' error  */
#line 428 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '-'"), yyvsp[-1]);}
#line 2821 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 140: /* expr: expr '*' expr  */
#line 429 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_TIMES,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2827 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 141: /* expr: expr '*' error  */
#line 430 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '*'"),yyvsp[-1]);}
#line 2833 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 142: /* expr: expr '/' expr  */
#line 431 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_RDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2839 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 143: /* expr: expr '/' error  */
#line 432 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '/'"),yyvsp[-1]);}
#line 2845 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 144: /* expr: expr '\\' expr  */
#line 433 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_LDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2851 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 145: /* expr: expr '\\' error  */
#line 434 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '\\'"),yyvsp[-1]);}
#line 2857 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 146: /* expr: expr '|' expr  */
#line 435 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_OR,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2863 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 147: /* expr: expr '|' error  */
#line 436 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '|'"),yyvsp[-1]);}
#line 2869 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 148: /* expr: expr '&' expr  */
#line 437 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_AND,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2875 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 149: /* expr: expr '&' error  */
#line 438 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '&'"),yyvsp[-1]);}
#line 2881 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 150: /* expr: expr SOR expr  */
#line 439 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SOR,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2887 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 151: /* expr: expr SOR error  */
#line 440 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '||'"),yyvsp[-1]);}
#line 2893 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 152: /* expr: expr SAND expr  */
#line 441 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_SAND,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2899 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 153: /* expr: expr SAND error  */
#line 442 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '&&'"),yyvsp[-1]);}
#line 2905 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 154: /* expr: expr '<' expr  */
#line 443 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_LT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2911 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 155: /* expr: expr '<' error  */
#line 444 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '<'"),yyvsp[-1]);}
#line 2917 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 156: /* expr: expr LE expr  */
#line 445 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_LEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2923 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 157: /* expr: expr LE error  */
#line 446 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '<='"),yyvsp[-1]);}
#line 2929 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 158: /* expr: expr '>' expr  */
#line 447 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_GT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2935 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 159: /* expr: expr '>' error  */
#line 448 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '>'"),yyvsp[-1]);}
#line 2941 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 160: /* expr: expr GE expr  */
#line 449 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_GEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2947 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 161: /* expr: expr GE error  */
#line 450 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '>='"),yyvsp[-1]);}
#line 2953 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 162: /* expr: expr EQ expr  */
#line 451 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2959 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 163: /* expr: expr EQ error  */
#line 452 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '=='"),yyvsp[-1]);}
#line 2965 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 164: /* expr: expr NE expr  */
#line 453 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_NEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2971 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 165: /* expr: expr NE error  */
#line 454 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '~='"),yyvsp[-1]);}
#line 2977 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 166: /* expr: expr DOTTIMES expr  */
#line 455 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_TIMES,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2983 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 167: /* expr: expr DOTTIMES error  */
#line 456 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after '.*'"), yyvsp[-1]);}
#line 2989 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 168: /* expr: expr DOTRDIV expr  */
#line 457 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_RDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2995 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 169: /* expr: expr DOTRDIV error  */
#line 458 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after './'"),yyvsp[-1]);}
#line 3001 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 170: /* expr: expr DOTLDIV expr  */
#line 459 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_LDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3007 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 171: /* expr: expr DOTLDIV error  */
#line 460 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyxpt(_("an expression after '.\\'"),yyvsp[-1]);}
#line 3013 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 172: /* expr: '-' expr  */
#line 461 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_UMINUS,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3019 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 173: /* expr: '+' expr  */
#line 462 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_UPLUS, yyvsp[0].v.p, yyvsp[-1].v.i);}
#line 3025 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 174: /* expr: '~' expr  */
#line 463 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_NOT,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3031 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 175: /* expr: '~' error  */
#line 464 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after logical not"),yyvsp[0]);}
#line 3037 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 176: /* expr: expr '^' expr  */
#line 465 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_MPOWER,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3043 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 177: /* expr: expr '^' error  */
#line 466 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '^'"),yyvsp[-1]);}
#line 3049 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 178: /* expr: expr DOTPOWER expr  */
#line 467 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_POWER,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3055 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 179: /* expr: expr DOTPOWER error  */
#line 468 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after '.^'"),yyvsp[-1]);}
#line 3061 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 180: /* expr: expr '\''  */
#line 469 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_TRANSPOSE,yyvsp[-1].v.p,yyvsp[0].v.i);}
#line 3067 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 181: /* expr: expr DOTTRANSPOSE  */
#line 470 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_TRANSPOSE,yyvsp[-1].v.p,yyvsp[0].v.i);}
#line 3073 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 182: /* expr: '(' expr ')'  */
#line 471 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = yyvsp[-1].v.p;}
#line 3079 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 183: /* expr: '(' expr error  */
#line 472 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("a right parenthesis after expression to match this one"),yyvsp[-2]);}
#line 3085 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 184: /* expr: '(' error  */
#line 473 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt(_("an expression after left parenthesis"),yyvsp[-1]);}
#line 3091 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 189: /* terminal: symbRefList  */
#line 481 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_RHS,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3097 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 190: /* terminal: '[' matrixDef ']'  */
#line 482 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = yyvsp[-1].v.p;}
#line 3103 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 191: /* terminal: '[' error  */
#line 483 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("a matrix definition followed by a right bracket"),yyvsp[-1]);}
#line 3109 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 192: /* terminal: '[' rowSeperator matrixDef ']'  */
#line 484 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-1].v.p;}
#line 3115 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 193: /* terminal: '[' matrixDef rowSeperator ']'  */
#line 485 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p;}
#line 3121 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 194: /* terminal: '[' rowSeperator matrixDef rowSeperator ']'  */
#line 486 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                {yyval.v.p = yyvsp[-2].v.p;}
#line 3127 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 195: /* terminal: '[' ']'  */
#line 487 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yyvsp[-1].v.i);}
#line 3133 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 196: /* terminal: '{' cellDef '}'  */
#line 488 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = yyvsp[-1].v.p;}
#line 3139 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 197: /* terminal: '{' rowSeperator cellDef '}'  */
#line 489 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyval.v.p = yyvsp[-1].v.p;}
#line 3145 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 198: /* terminal: '{' cellDef rowSeperator '}'  */
#line 490 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyval.v.p = yyvsp[-2].v.p;}
#line 3151 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 199: /* terminal: '{' rowSeperator cellDef rowSeperator '}'  */
#line 491 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                              {yyval.v.p = yyvsp[-2].v.p;}
#line 3157 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 200: /* terminal: '{' '}'  */
#line 492 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yyvsp[-1].v.i);}
#line 3163 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 201: /* terminal: '{' error  */
#line 493 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("a cell-array definition followed by a right brace"),yyvsp[-1]);}
#line 3169 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 203: /* symbRefList: symbRefList symbRef  */
#line 497 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3175 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 204: /* symbRef: '(' indexList ')'  */
#line 500 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i); }
#line 3181 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 205: /* symbRef: '(' ')'  */
#line 501 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyval.v.p = AbstractSyntaxTree::createNode(OP_PARENS,NULL,yyvsp[-1].v.i); }
#line 3187 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 206: /* symbRef: '(' indexList error  */
#line 502 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("matching right parenthesis"),yyvsp[-2]);}
#line 3193 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 207: /* symbRef: '{' indexList '}'  */
#line 503 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yyvsp[-1].v.p,yyvsp[-2].v.i); }
#line 3199 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 208: /* symbRef: '{' indexList error  */
#line 504 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("matching right brace"),yyvsp[-2]);}
#line 3205 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 209: /* symbRef: '.' IDENT  */
#line 505 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT,yyvsp[0].v.p,yyvsp[-1].v.i); }
#line 3211 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 210: /* symbRef: '.' '(' expr ')'  */
#line 506 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOTDYN,yyvsp[-1].v.p,yyvsp[-3].v.i);}
#line 3217 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 212: /* indexElement: ':'  */
#line 510 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyval.v.p = AbstractSyntaxTree::createNode(OP_ALL,NULL,yyvsp[0].v.i);}
#line 3223 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 213: /* indexElement: '/' IDENT '=' expr  */
#line 511 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_KEYWORD,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-3].v.i);}
#line 3229 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 214: /* indexElement: '/' IDENT '=' error  */
#line 512 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("expecting expression after '=' in keyword assignment"),yyvsp[-1]);}
#line 3235 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 215: /* indexElement: '/' IDENT  */
#line 513 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_KEYWORD,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3241 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 216: /* indexElement: '/' error  */
#line 514 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("expecting keyword identifier after '/' in keyword assignment"),yyvsp[-1]);}
#line 3247 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 218: /* indexList: indexList ',' indexElement  */
#line 519 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addPeer(yyvsp[0].v.p);}
#line 3253 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 219: /* cellDef: rowDef  */
#line 523 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3259 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 220: /* cellDef: cellDef rowSeperator rowDef  */
#line 524 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3265 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 221: /* matrixDef: rowDef  */
#line 528 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACKETS,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3271 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 222: /* matrixDef: matrixDef rowSeperator rowDef  */
#line 529 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                     {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3277 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 226: /* rowDef: expr  */
#line 541 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyval.v.p = AbstractSyntaxTree::createNode(OP_SEMICOLON,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3283 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 227: /* rowDef: rowDef columnSep expr  */
#line 542 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                             {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3289 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;


#line 3293 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 544 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"

//=============================================================================
namespace Nelson {
//=============================================================================
  void callyyparse() {
    std::scoped_lock<std::mutex> lock{parseMutex};
    yyparse();
  }
//=============================================================================
}
//=============================================================================
// clang-format on
