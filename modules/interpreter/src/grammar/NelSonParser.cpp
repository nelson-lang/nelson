/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C */

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
#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
#endif
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
    ABORT = 295,                   /* ABORT  */
    ENDFUNCTION = 296,             /* ENDFUNCTION  */
    SOR = 297,                     /* SOR  */
    SAND = 298,                    /* SAND  */
    NE = 299,                      /* NE  */
    POS = 300,                     /* POS  */
    NEG = 301,                     /* NEG  */
    NOT = 302                      /* NOT  */
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
  YYSYMBOL_ABORT = 40,                     /* ABORT  */
  YYSYMBOL_ENDFUNCTION = 41,               /* ENDFUNCTION  */
  YYSYMBOL_SOR = 42,                       /* SOR  */
  YYSYMBOL_SAND = 43,                      /* SAND  */
  YYSYMBOL_44_ = 44,                       /* '|'  */
  YYSYMBOL_45_ = 45,                       /* '&'  */
  YYSYMBOL_46_ = 46,                       /* '<'  */
  YYSYMBOL_47_ = 47,                       /* '>'  */
  YYSYMBOL_NE = 48,                        /* NE  */
  YYSYMBOL_49_ = 49,                       /* ':'  */
  YYSYMBOL_50_ = 50,                       /* '+'  */
  YYSYMBOL_51_ = 51,                       /* '-'  */
  YYSYMBOL_52_ = 52,                       /* '*'  */
  YYSYMBOL_53_ = 53,                       /* '/'  */
  YYSYMBOL_54_ = 54,                       /* '\\'  */
  YYSYMBOL_POS = 55,                       /* POS  */
  YYSYMBOL_NEG = 56,                       /* NEG  */
  YYSYMBOL_NOT = 57,                       /* NOT  */
  YYSYMBOL_58_ = 58,                       /* '^'  */
  YYSYMBOL_59_ = 59,                       /* '\''  */
  YYSYMBOL_60_ = 60,                       /* '('  */
  YYSYMBOL_61_ = 61,                       /* ')'  */
  YYSYMBOL_62_ = 62,                       /* '='  */
  YYSYMBOL_63_ = 63,                       /* '['  */
  YYSYMBOL_64_ = 64,                       /* ']'  */
  YYSYMBOL_65_ = 65,                       /* ';'  */
  YYSYMBOL_66_ = 66,                       /* '{'  */
  YYSYMBOL_67_ = 67,                       /* '}'  */
  YYSYMBOL_68_ = 68,                       /* '~'  */
  YYSYMBOL_69_ = 69,                       /* '@'  */
  YYSYMBOL_70_ = 70,                       /* '.'  */
  YYSYMBOL_71_ = 71,                       /* ','  */
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
  YYSYMBOL_rowDef = 118,                   /* rowDef  */
  YYSYMBOL_parenExpr = 119                 /* parenExpr  */
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
#define YYFINAL  97
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3021

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  72
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  48
/* YYNRULES -- Number of rules.  */
#define YYNRULES  230
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  351

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   302


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
       2,     2,     2,     2,     2,     2,     2,     2,    45,    59,
      60,    61,    52,    50,    71,    51,    70,    53,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    49,    65,
      46,    62,    47,     2,    69,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    63,    54,    64,    58,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    66,    44,    67,    68,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    42,    43,    48,
      55,    56,    57
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    71,    71,    72,    72,    73,    77,    84,    92,   100,
     109,   117,   126,   127,   128,   129,   130,   131,   132,   133,
     137,   138,   142,   143,   144,   145,   146,   147,   148,   149,
     153,   154,   158,   162,   163,   167,   171,   175,   182,   183,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   196,   197,   201,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,   216,   220,   224,   227,   231,
     237,   242,   246,   247,   251,   257,   263,   264,   265,   266,
     270,   271,   276,   277,   281,   284,   290,   296,   299,   305,
     308,   313,   318,   319,   320,   322,   323,   324,   325,   326,
     327,   331,   334,   339,   340,   344,   350,   351,   355,   358,
     361,   365,   366,   370,   373,   379,   382,   386,   389,   390,
     394,   395,   399,   402,   406,   410,   414,   415,   416,   417,
     418,   419,   423,   424,   425,   426,   427,   428,   429,   430,
     431,   432,   433,   434,   435,   436,   437,   438,   439,   440,
     441,   442,   443,   444,   445,   446,   447,   448,   449,   450,
     451,   452,   453,   454,   455,   456,   457,   458,   459,   460,
     461,   462,   463,   464,   465,   466,   467,   468,   469,   470,
     471,   472,   476,   477,   478,   479,   480,   481,   482,   483,
     484,   485,   486,   487,   488,   489,   490,   491,   492,   493,
     494,   495,   499,   500,   504,   505,   506,   507,   508,   509,
     510,   514,   515,   516,   517,   518,   519,   523,   524,   528,
     529,   533,   534,   538,   539,   543,   547,   548,   552,   553,
     554
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
  "VARARGIN", "VARARGOUT", "ABORT", "ENDFUNCTION", "SOR", "SAND", "'|'",
  "'&'", "'<'", "'>'", "NE", "':'", "'+'", "'-'", "'*'", "'/'", "'\\\\'",
  "POS", "NEG", "NOT", "'^'", "'\\''", "'('", "')'", "'='", "'['", "']'",
  "';'", "'{'", "'}'", "'~'", "'@'", "'.'", "','", "$accept", "program",
  "functionDef", "functionDefList", "returnDeclaration", "argumentList",
  "argument", "statementList", "statement", "statementType",
  "endfunctionStatement", "specialSyntaxStatement", "returnStatement",
  "pauseStatement", "continueStatement", "breakStatement", "tryStatement",
  "optionalCatch", "switchStatement", "optionalEndStatement", "newLine",
  "caseBlock", "caseList", "caseStatement", "otherwiseClause",
  "forStatement", "forIndexExpression", "whileStatement", "ifStatement",
  "conditionedStatement", "elseIfBlock", "elseIfStatementList",
  "elseIfStatement", "elseStatement", "assignmentStatement",
  "multiFunctionCall", "expr", "terminal", "symbRefList", "symbRef",
  "indexElement", "indexList", "cellDef", "matrixDef", "rowSeperator",
  "columnSep", "rowDef", "parenExpr", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-133)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-126)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     349,  -133,   202,  -133,    12,    16,  1426,   145,   168,  -133,
    -133,  1506,  2642,  -133,  2387,  -133,  -133,  -133,  -133,  2642,
    2642,  1527,   390,  1393,  1531,    -1,    61,  -133,    44,  1007,
    -133,    33,  -133,   208,  -133,  -133,  -133,  -133,  -133,  -133,
    -133,  -133,  -133,  -133,  -133,  2803,  -133,   159,  -133,  -133,
    -133,  -133,  -133,  -133,  -133,  -133,  -133,  -133,  -133,   390,
      57,   418,   108,  -133,   135,     8,    41,   180,  -133,    88,
     231,    13,  -133,  2220,  2220,  1062,   172,   172,  -133,  2673,
    -133,  -133,  -133,  -133,  2803,   138,  2642,    24,  -133,  -133,
       4,  2642,    24,  -133,   172,  -133,    86,  -133,  -133,  -133,
    -133,  -133,  -133,  -133,  -133,  -133,  -133,  -133,  1600,  1604,
    1625,  1629,  1698,  1702,  1723,  -133,  1727,  1796,  1800,  1821,
    1825,  1894,  1898,  1919,  1923,  1992,  1996,  2017,  2021,  2090,
    -133,  2471,  2094,  2500,    40,  -133,   183,  2115,   147,    57,
    -133,  -133,  -133,  -133,  -133,  1129,  -133,   234,  -133,  -133,
     148,  -133,  2387,  -133,  -133,  -133,    98,    15,  -133,  -133,
      48,  2119,  -133,    14,  2277,  2332,   162,  2442,    47,  -133,
    -133,   101,  2532,   187,  2642,  -133,  2555,    35,  2642,   -16,
    -133,  2952,  -133,  2952,  -133,  2952,  -133,   172,  -133,   172,
    -133,   172,  -133,   172,  -133,  2820,  -133,  2873,  -133,  2888,
    -133,  2902,  -133,  2952,  -133,  2952,  -133,  2952,  -133,  2962,
    -133,   712,  -133,   712,  -133,   172,  -133,   172,  -133,   172,
    -133,   172,  -133,   256,  -133,  2803,  -133,    36,  -133,  2803,
      27,  -133,  2642,  -133,  -133,  -133,   619,    50,  -133,  1129,
    -133,    52,    -5,   677,  -133,  -133,    29,   188,  -133,   155,
    2387,  -133,  2803,  -133,  2188,  -133,   481,  -133,   550,  2642,
     167,   162,  -133,   270,  1187,  -133,  -133,   265,  -133,    24,
    -133,  2587,  2803,  -133,    24,  -133,  2610,  2803,  2642,  -133,
     141,  -133,  -133,  2500,  -133,  -133,  2748,  -133,  1242,  -133,
    -133,  2387,    76,  -133,  -133,  -133,  -133,    52,     2,   732,
    -133,  2728,  -133,  -133,  -133,  -133,  2220,  2387,    51,  -133,
    2387,  -133,    30,  -133,  -133,  2803,  2192,  -133,  -133,   787,
    -133,  2387,  2387,   193,  -133,  -133,  2387,  1297,  -133,  -133,
    1352,  -133,  1422,  1451,  -133,  2803,   842,   897,  -133,  2387,
    1129,  -133,  -133,    45,  -133,    32,   952,  -133,  -133,  -133,
    -133
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     5,   202,   182,   183,   184,     0,     0,     0,    68,
     185,     0,     0,    67,    40,    66,    65,    52,    54,     0,
       0,     0,     0,     0,     0,     0,     0,    20,     3,    40,
      33,     0,    53,    51,    50,    49,    44,    43,    48,    47,
      42,    45,    46,    38,    41,    39,   134,   189,   181,    60,
      55,    59,    58,    57,    56,   106,   202,   183,   184,     0,
     111,     0,   189,    12,     0,     0,     0,     0,   100,    94,
       0,     0,   103,     0,     0,    40,   172,   171,   230,     0,
     191,   224,   223,   195,   226,     0,     0,   221,   201,   200,
       0,     0,   219,   174,   173,   186,     0,     1,    21,    34,
      35,    36,   225,    37,    63,    64,    62,    61,     0,     0,
       0,     0,     0,     0,     0,   180,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,   203,     0,     0,   118,   112,
     113,   110,    78,    77,    79,   109,    76,    13,    81,    80,
       0,    23,    40,    22,    27,    32,     0,     0,    30,    16,
       0,     0,    98,     0,    40,    40,    82,    40,     0,   229,
     228,   190,     0,     0,     0,   196,     0,     0,     0,     0,
     156,   155,   160,   159,   162,   161,   166,   165,   168,   167,
     170,   169,   178,   177,   150,   149,   152,   151,   146,   145,
     148,   147,   154,   153,   158,   157,   164,   163,   133,   132,
     136,   135,   138,   137,   140,   139,   142,   141,   144,   143,
     176,   175,   212,     0,   205,   211,   217,     0,   121,   120,
       0,   209,     0,   190,   116,   115,     0,     0,   114,   108,
      14,     0,     0,    40,    25,    28,     0,     0,    17,     0,
      40,    99,    93,    97,     0,    89,     0,   101,     0,     0,
      88,    83,    84,   202,    40,    70,    69,     0,   193,   222,
     192,     0,   227,   198,   220,   197,     0,   188,     0,   216,
     215,   206,   204,     0,   208,   207,     0,   119,    40,   107,
     105,    40,     0,    29,    24,    31,    18,     0,     0,    40,
      96,     0,    91,    90,   104,   102,     0,    40,     0,    85,
      40,   131,     0,   194,   199,   187,     0,   218,   210,    40,
      15,    40,    40,     0,    95,    92,    40,    40,    75,    74,
      40,   130,     0,     0,   214,   213,    40,    40,    19,    40,
      86,   128,   122,     0,   129,     0,    40,   127,   123,   126,
     124
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -133,  -133,   176,  -133,  -133,   -88,   -33,   -10,     5,  -133,
    -133,  -133,  -133,  -133,  -133,  -133,  -133,  -133,  -133,   -68,
    -110,  -133,  -133,   -46,  -133,  -133,  -133,  -133,  -133,    83,
    -133,  -133,   102,  -133,  -133,  -133,     1,  -133,     0,  -133,
     -56,  -132,   151,   -12,     3,   -20,    -6,  -133
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    26,    27,    28,    67,   157,   158,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,   168,    39,   145,
     152,   260,   261,   262,   308,    40,    71,    41,    42,    60,
     138,   139,   140,   237,    43,    44,    45,    46,    62,   135,
     226,   227,    90,    85,    86,   146,    87,    48
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      47,   230,    95,   164,    75,   165,   166,    61,   179,    81,
      82,   103,    73,    74,    47,   253,   245,    92,   142,   143,
      76,    77,    79,    84,    84,    94,    91,    53,   284,    47,
     293,   331,    54,   349,    99,  -125,  -125,   281,   100,   101,
      81,    82,   154,   231,   155,   278,   347,   136,   265,   248,
     250,   289,   328,   148,   149,   102,   292,   148,   149,    96,
      84,    97,   242,   323,     7,   266,   102,   174,   290,   329,
     153,   175,   174,   102,   173,    47,   254,   320,   144,   246,
      99,   148,   149,   137,   102,    92,   102,    84,   172,   155,
     332,   294,    84,   176,   285,   102,   333,   282,   102,   350,
     232,  -125,   275,   102,   102,   156,   348,   102,   249,   181,
     183,   185,   187,   189,   191,   193,   102,   195,   197,   199,
     201,   203,   205,   207,   209,   211,   213,   215,   217,   219,
     221,   291,   225,   229,   225,   239,   147,   247,    61,   172,
     148,   149,   243,    81,    82,    47,    63,   178,    64,   240,
     161,   155,    47,   310,   256,   258,   296,   264,   155,   247,
     244,   298,   252,   267,    47,    47,   269,    47,   131,    68,
     274,    69,   236,    84,   133,   272,   271,    84,   134,   277,
     276,   159,   321,   160,    65,   114,   115,   322,    81,    82,
     259,   155,    81,    82,   338,   150,   307,   151,   148,   149,
     343,   345,   171,   316,    98,    49,    50,   283,    66,   241,
     283,   104,   105,   339,   295,   309,   297,    51,    52,   131,
     235,   132,   247,   106,   107,   133,   288,   317,    70,   134,
     129,   130,   162,   286,   163,   -26,    47,   -26,   326,    47,
     299,   238,   177,    47,    99,     0,     0,   233,    99,   174,
      47,   270,     0,     0,   174,   301,    47,   279,    47,   280,
     306,    99,     0,    99,    47,   269,   311,     0,   312,    99,
     274,     0,    84,    49,    50,   148,   149,    84,   247,   315,
       0,   319,     0,     0,   225,    51,    52,     0,    47,     0,
       0,    47,     0,    99,     0,     0,     0,   327,     0,    47,
     330,     0,     0,     0,    99,     0,     0,    47,     0,     0,
      47,   336,   337,     0,     0,     0,   340,   335,     0,    47,
       0,    47,    47,   283,    99,   283,    47,    47,     0,   346,
      47,     0,    99,   225,   225,    99,    47,    47,     0,    47,
      47,    99,    99,     0,     0,    99,    47,     0,     0,    -4,
       1,    99,     2,     3,   -40,   -40,     0,     0,     0,     0,
       0,     0,     0,     0,     4,     5,     0,     0,     6,     7,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,    80,     0,    56,     3,    81,    82,     0,     0,    19,
      20,     0,     0,     0,     0,    57,    58,     0,     0,    21,
       0,     0,    22,    10,     0,    23,     0,    24,    25,   141,
     -40,     0,     0,   142,   143,   108,   109,   110,   111,   112,
     113,   114,   115,     0,     0,     0,     0,     0,     0,     0,
      19,    20,     0,     0,     0,     0,     0,     0,     0,     0,
      21,     0,     0,    59,    83,     0,    23,     0,    24,    25,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,     0,     0,     0,   129,   130,     0,     0,
       0,     0,   302,   144,     2,     3,   -40,   -40,     0,   102,
       0,     0,     0,     0,     0,     0,     4,     5,     0,   303,
       6,     0,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,    19,    20,     0,     0,     0,     0,     0,     0,     0,
       0,    21,     0,     0,    22,     0,     0,    23,     0,    24,
      25,   304,   -40,     2,     3,   -40,   -40,     0,     0,     0,
       0,     0,     0,     0,     0,     4,     5,     0,   305,     6,
       0,     8,     9,    10,    11,     0,     0,    12,     0,     0,
      13,    14,     0,     0,     0,     0,    15,    16,     0,     0,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
      19,    20,     0,     0,     0,     0,     0,     0,     0,     0,
      21,     0,     0,    22,     0,     0,    23,     0,    24,    25,
     287,   -40,     2,     3,   -40,   -40,     0,     0,     0,     0,
       0,     0,     0,     0,     4,     5,     0,     0,     6,     0,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,    19,
      20,     0,     0,     0,     0,     0,     0,    -9,     0,    21,
       2,     3,    22,     0,     0,    23,     0,    24,    25,     0,
     -40,     0,     4,     5,     0,     0,     6,    -9,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,   111,   112,   113,   114,   115,    19,    20,     0,
       0,     0,    -8,     0,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
       0,     6,    -8,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,   126,   127,   128,     0,    15,    16,
     129,   130,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,     0,     0,     0,   -11,     0,     0,
       2,     3,    21,     0,     0,    22,     0,     0,    23,     0,
      24,    25,     4,     5,     0,     0,     6,   -11,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,    -7,     0,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
       0,     6,    -7,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,     0,     0,     0,     0,    15,    16,
       0,     0,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,     0,     0,     0,   -10,     0,     0,
       2,     3,    21,     0,     0,    22,     0,     0,    23,     0,
      24,    25,     4,     5,     0,     0,     6,   -10,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,    -6,     0,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
       0,     6,    -6,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,     0,     0,     0,     0,    15,    16,
       0,     0,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,     0,     0,     0,    -2,     0,     0,
       2,     3,    21,     0,     0,    22,     0,     0,    23,     0,
      24,    25,     4,     5,     0,     0,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,   -73,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
     -73,     6,     0,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,   167,     0,     0,     0,    15,    16,
       0,     0,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,     0,     0,     0,     0,     0,     0,
       0,     0,    21,     0,     0,    22,     0,     0,    23,     0,
      24,    25,     2,     3,   -40,   -40,     0,     0,     0,     0,
       0,     0,     0,     0,     4,     5,     0,     0,     6,     0,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,    19,
      20,     0,     0,     0,     0,     0,     0,     0,   -72,    21,
       2,     3,    22,     0,     0,    23,     0,    24,    25,     0,
     -40,     0,     4,     5,     0,   -72,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,  -117,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
    -117,     6,     0,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,     0,     0,     0,     0,    15,    16,
       0,     0,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,     0,     0,     0,     0,   -87,     0,
       2,     3,    21,     0,     0,    22,     0,     0,    23,     0,
      24,    25,     4,     5,     0,   -87,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,   -71,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
     -71,     6,     0,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,     0,     0,     0,     0,    15,    16,
       0,     0,    17,    18,    88,     0,    56,     3,    81,    82,
       0,     0,    19,    20,     0,     0,     0,     0,    57,    58,
       0,     0,    21,     0,     0,    22,    10,     0,    23,     0,
      24,    25,     0,   341,     0,    56,     3,    55,     0,    56,
       3,     0,     0,     0,     0,     0,     0,    57,    58,     0,
       0,    57,    58,    19,    20,    10,     0,     0,     0,    10,
       0,     0,   344,    21,    56,     3,    59,     0,     0,    23,
      89,    24,    25,     0,     0,     0,    57,    58,     0,     0,
       0,   222,    19,    20,    10,   223,    19,    20,     0,     0,
       0,     0,    21,   342,     0,    59,    21,     0,    23,    59,
      24,    25,    23,     0,    24,    25,     0,     0,     0,     0,
     222,    19,    20,     0,   223,     0,     0,    72,     0,    56,
       3,    21,     0,     0,    59,     0,     0,    23,     0,    24,
      25,    57,    58,     0,     0,     0,     0,     0,    78,    10,
      56,     3,    93,     0,    56,     3,     0,     0,     0,     0,
       0,     0,    57,    58,     0,     0,    57,    58,     0,     0,
      10,     0,     0,     0,    10,     0,    19,    20,     0,     0,
       0,     0,     0,     0,     0,     0,    21,     0,     0,    59,
       0,     0,    23,     0,    24,    25,     0,    19,    20,     0,
       0,    19,    20,     0,     0,     0,     0,    21,     0,     0,
      59,    21,     0,    23,    59,    24,    25,    23,     0,    24,
      25,   180,     0,    56,     3,   182,     0,    56,     3,     0,
       0,     0,     0,     0,     0,    57,    58,     0,     0,    57,
      58,     0,     0,    10,     0,     0,   184,    10,    56,     3,
     186,     0,    56,     3,     0,     0,     0,     0,     0,     0,
      57,    58,     0,     0,    57,    58,     0,     0,    10,     0,
      19,    20,    10,     0,    19,    20,     0,     0,     0,     0,
      21,     0,     0,    59,    21,     0,    23,    59,    24,    25,
      23,     0,    24,    25,     0,    19,    20,     0,     0,    19,
      20,     0,     0,     0,     0,    21,     0,     0,    59,    21,
       0,    23,    59,    24,    25,    23,     0,    24,    25,   188,
       0,    56,     3,   190,     0,    56,     3,     0,     0,     0,
       0,     0,     0,    57,    58,     0,     0,    57,    58,     0,
       0,    10,     0,     0,   192,    10,    56,     3,   194,     0,
      56,     3,     0,     0,     0,     0,     0,     0,    57,    58,
       0,     0,    57,    58,     0,     0,    10,     0,    19,    20,
      10,     0,    19,    20,     0,     0,     0,     0,    21,     0,
       0,    59,    21,     0,    23,    59,    24,    25,    23,     0,
      24,    25,     0,    19,    20,     0,     0,    19,    20,     0,
       0,     0,     0,    21,     0,     0,    59,    21,     0,    23,
      59,    24,    25,    23,     0,    24,    25,   196,     0,    56,
       3,   198,     0,    56,     3,     0,     0,     0,     0,     0,
       0,    57,    58,     0,     0,    57,    58,     0,     0,    10,
       0,     0,   200,    10,    56,     3,   202,     0,    56,     3,
       0,     0,     0,     0,     0,     0,    57,    58,     0,     0,
      57,    58,     0,     0,    10,     0,    19,    20,    10,     0,
      19,    20,     0,     0,     0,     0,    21,     0,     0,    59,
      21,     0,    23,    59,    24,    25,    23,     0,    24,    25,
       0,    19,    20,     0,     0,    19,    20,     0,     0,     0,
       0,    21,     0,     0,    59,    21,     0,    23,    59,    24,
      25,    23,     0,    24,    25,   204,     0,    56,     3,   206,
       0,    56,     3,     0,     0,     0,     0,     0,     0,    57,
      58,     0,     0,    57,    58,     0,     0,    10,     0,     0,
     208,    10,    56,     3,   210,     0,    56,     3,     0,     0,
       0,     0,     0,     0,    57,    58,     0,     0,    57,    58,
       0,     0,    10,     0,    19,    20,    10,     0,    19,    20,
       0,     0,     0,     0,    21,     0,     0,    59,    21,     0,
      23,    59,    24,    25,    23,     0,    24,    25,     0,    19,
      20,     0,     0,    19,    20,     0,     0,     0,     0,    21,
       0,     0,    59,    21,     0,    23,    59,    24,    25,    23,
       0,    24,    25,   212,     0,    56,     3,   214,     0,    56,
       3,     0,     0,     0,     0,     0,     0,    57,    58,     0,
       0,    57,    58,     0,     0,    10,     0,     0,   216,    10,
      56,     3,   218,     0,    56,     3,     0,     0,     0,     0,
       0,     0,    57,    58,     0,     0,    57,    58,     0,     0,
      10,     0,    19,    20,    10,     0,    19,    20,     0,     0,
       0,     0,    21,     0,     0,    59,    21,     0,    23,    59,
      24,    25,    23,     0,    24,    25,     0,    19,    20,     0,
       0,    19,    20,     0,     0,     0,     0,    21,     0,     0,
      59,    21,     0,    23,    59,    24,    25,    23,     0,    24,
      25,   220,     0,    56,     3,   228,     0,    56,     3,     0,
       0,     0,     0,     0,     0,    57,    58,     0,     0,    57,
      58,     0,     0,    10,     0,     0,   234,    10,    56,     3,
     251,     0,    56,     3,     0,     0,     0,     0,     0,     0,
      57,    58,     0,     0,    57,    58,     0,     0,    10,     0,
      19,    20,    10,     0,    19,    20,     0,     0,     0,     0,
      21,     0,     0,    59,    21,     0,    23,    59,    24,    25,
      23,     0,    24,    25,     0,    19,    20,     0,     0,    19,
      20,     0,     0,     0,     0,    21,     0,     0,    59,    21,
       0,    23,    59,    24,    25,    23,     0,    24,    25,   300,
       0,    56,     3,   334,     0,    56,     3,     0,     0,     0,
       0,     0,     0,    57,    58,     0,     0,    57,    58,     0,
       0,    10,     0,     0,     0,    10,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   142,   143,   108,   109,   110,
     111,   112,   113,   114,   115,     0,     0,     0,    19,    20,
       0,     0,    19,    20,     0,     0,     0,     0,    21,     0,
       0,    59,    21,     0,    23,    59,    24,    25,    23,     0,
      24,    25,   116,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,     0,     0,     0,   129,   130,
       2,     3,     0,     0,     0,   144,     0,     0,     0,     0,
       0,   102,     4,     5,     0,   255,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,     0,     0,     2,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
     257,     6,     0,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,     0,     0,     0,     0,    15,    16,
       0,     0,    17,    18,     0,     0,     0,     0,     0,     0,
       0,     0,    19,    20,     0,     0,     0,     0,     0,     0,
       2,     3,    21,     0,     0,    22,     0,     0,    23,     0,
      24,    25,     4,     5,     0,     0,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,     0,     0,   263,     3,    21,     0,     0,
      22,     0,     0,    23,     0,    24,    25,     4,     5,     0,
       0,     6,     0,     8,     9,    10,    11,     0,     0,    12,
       0,     0,    13,    14,    56,     3,     0,     0,    15,    16,
       0,     0,    17,    18,     0,     0,    57,    58,     0,     0,
       0,     0,    19,    20,    10,     0,     0,     0,     0,     0,
       0,     0,    21,    56,     3,    22,     0,     0,    23,     0,
      24,    25,     0,     0,     0,    57,    58,     0,     0,     0,
     222,    19,    20,    10,   223,     0,     0,     0,     0,     0,
       0,    21,   224,     0,    59,    56,     3,    23,     0,    24,
      25,     0,     0,     0,     0,     0,     0,    57,    58,   222,
      19,    20,     0,   223,     0,    10,     0,     0,    56,     3,
      21,     0,     0,    59,     0,     0,    23,     0,    24,    25,
      57,    58,     0,     0,     0,     0,     0,     0,    10,     0,
       0,     0,    19,    20,     0,     0,     0,     0,     0,     0,
      56,     3,    21,     0,     0,    59,   268,     0,    23,     0,
      24,    25,    57,    58,     0,    19,    20,     0,     0,     0,
      10,     0,     0,    56,     3,    21,     0,     0,    59,     0,
       0,    23,   273,    24,    25,    57,    58,     0,     0,     0,
       0,     0,     0,    10,     0,     0,     0,    19,    20,     0,
       0,     0,     0,     0,     0,    56,     3,    21,     0,     0,
      59,   313,     0,    23,     0,    24,    25,    57,    58,     0,
      19,    20,     0,     0,     0,    10,     0,     0,     0,     0,
      21,     0,     0,    59,   169,     0,    23,   314,    24,    25,
     108,   109,   110,   111,   112,   113,   114,   115,     0,     0,
       0,     0,    19,    20,     0,     0,     0,     0,     0,     0,
       0,     0,    21,     0,     0,    59,     0,     0,    23,     0,
      24,    25,     0,     0,     0,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,     0,   324,
       0,   129,   130,     0,   170,   108,   109,   110,   111,   112,
     113,   114,   115,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,   109,   110,   111,   112,
     113,   114,   115,     0,     0,     0,     0,     0,     0,     0,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,     0,     0,     0,   129,   130,     0,   325,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,     0,     0,     0,   129,   130,     0,   318,
     108,   109,   110,   111,   112,   113,   114,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,   109,   110,
     111,   112,   113,   114,   115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,     0,     0,
       0,   129,   130,   117,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,     0,     0,     0,   129,   130,
     108,   109,   110,   111,   112,   113,   114,   115,     0,     0,
       0,     0,     0,     0,     0,   108,   109,   110,   111,   112,
     113,   114,   115,     0,     0,     0,     0,     0,     0,   108,
     109,   110,   111,   112,   113,   114,   115,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,     0,     0,
       0,   129,   130,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,     0,     0,     0,   129,   130,   120,   121,
     122,   123,   124,   125,   126,   127,   128,     0,     0,     0,
     129,   130,   111,   112,   113,   114,   115,     0,     0,     0,
       0,     0,   111,   112,   113,   114,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,   124,   125,   126,   127,   128,     0,     0,     0,
     129,   130,   124,   125,   126,   127,   128,     0,     0,     0,
     129,   130
};

static const yytype_int16 yycheck[] =
{
       0,   133,     3,    71,    14,    73,    74,     6,    96,     5,
       6,    31,    11,    12,    14,     1,     1,    23,     5,     6,
      19,    20,    21,    22,    23,    24,    23,    15,     1,    29,
       1,     1,    16,     1,    29,     5,     6,     1,     5,     6,
       5,     6,     1,     3,     3,    61,     1,    59,     1,     1,
     160,     1,     1,     5,     6,    71,    61,     5,     6,    60,
      59,     0,   150,    61,    20,    18,    71,    87,    18,    18,
      62,    67,    92,    71,    86,    75,    62,     1,    65,    64,
      75,     5,     6,    26,    71,    91,    71,    86,    85,     3,
      60,    62,    91,    90,    67,    71,    66,    61,    71,    67,
      60,    71,    67,    71,    71,    64,    61,    71,    60,   108,
     109,   110,   111,   112,   113,   114,    71,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   241,   131,   132,   133,   145,     1,   157,   137,   136,
       5,     6,   152,     5,     6,   145,     1,    61,     3,     1,
      62,     3,   152,   263,   164,   165,     1,   167,     3,   179,
      62,   249,   161,    62,   164,   165,   172,   167,    60,     1,
     176,     3,    25,   172,    66,   174,   173,   176,    70,   178,
     177,     1,   292,     3,    39,    13,    14,   297,     5,     6,
      28,     3,     5,     6,     1,    60,    29,    62,     5,     6,
     332,   333,    64,    62,    28,     3,     4,   227,    63,    61,
     230,     3,     4,   323,   247,   261,    61,    15,    16,    60,
     137,    62,   242,    15,    16,    66,   236,   283,    60,    70,
      58,    59,     1,   232,     3,     1,   236,     3,   306,   239,
     250,   139,    91,   243,   239,    -1,    -1,    64,   243,   269,
     250,    64,    -1,    -1,   274,   254,   256,     1,   258,     3,
     259,   256,    -1,   258,   264,   271,     1,    -1,     3,   264,
     276,    -1,   271,     3,     4,     5,     6,   276,   298,   278,
      -1,   291,    -1,    -1,   283,    15,    16,    -1,   288,    -1,
      -1,   291,    -1,   288,    -1,    -1,    -1,   307,    -1,   299,
     310,    -1,    -1,    -1,   299,    -1,    -1,   307,    -1,    -1,
     310,   321,   322,    -1,    -1,    -1,   326,   316,    -1,   319,
      -1,   321,   322,   343,   319,   345,   326,   327,    -1,   339,
     330,    -1,   327,   332,   333,   330,   336,   337,    -1,   339,
     340,   336,   337,    -1,    -1,   340,   346,    -1,    -1,     0,
       1,   346,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    19,    20,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,     1,    -1,     3,     4,     5,     6,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    60,
      -1,    -1,    63,    23,    -1,    66,    -1,    68,    69,     1,
      71,    -1,    -1,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,    64,    -1,    66,    -1,    68,    69,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    -1,    -1,    58,    59,    -1,    -1,
      -1,    -1,     1,    65,     3,     4,     5,     6,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,    68,
      69,     1,    71,     3,     4,     5,     6,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    18,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,    -1,    -1,    66,    -1,    68,    69,
       1,    71,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,    60,
       3,     4,    63,    -1,    -1,    66,    -1,    68,    69,    -1,
      71,    -1,    15,    16,    -1,    -1,    19,    20,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    10,    11,    12,    13,    14,    50,    51,    -1,
      -1,    -1,     0,    -1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      -1,    19,    20,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    52,    53,    54,    -1,    36,    37,
      58,    59,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,     0,    -1,    -1,
       3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    -1,    19,    20,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,     0,    -1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      -1,    19,    20,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,     0,    -1,    -1,
       3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    -1,    19,    20,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,     0,    -1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      -1,    19,    20,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,     0,    -1,    -1,
       3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    32,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    60,
       3,     4,    63,    -1,    -1,    66,    -1,    68,    69,    -1,
      71,    -1,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,     1,    -1,     3,     4,     5,     6,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    15,    16,
      -1,    -1,    60,    -1,    -1,    63,    23,    -1,    66,    -1,
      68,    69,    -1,     1,    -1,     3,     4,     1,    -1,     3,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,
      -1,    15,    16,    50,    51,    23,    -1,    -1,    -1,    23,
      -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,    66,
      67,    68,    69,    -1,    -1,    -1,    15,    16,    -1,    -1,
      -1,    49,    50,    51,    23,    53,    50,    51,    -1,    -1,
      -1,    -1,    60,    61,    -1,    63,    60,    -1,    66,    63,
      68,    69,    66,    -1,    68,    69,    -1,    -1,    -1,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,     1,    -1,     3,
       4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,    68,
      69,    15,    16,    -1,    -1,    -1,    -1,    -1,     1,    23,
       3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,    -1,
      23,    -1,    -1,    -1,    23,    -1,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
      -1,    -1,    66,    -1,    68,    69,    -1,    50,    51,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,    -1,
      63,    60,    -1,    66,    63,    68,    69,    66,    -1,    68,
      69,     1,    -1,     3,     4,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    15,
      16,    -1,    -1,    23,    -1,    -1,     1,    23,     3,     4,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    15,    16,    -1,    -1,    23,    -1,
      50,    51,    23,    -1,    50,    51,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,    60,    -1,    66,    63,    68,    69,
      66,    -1,    68,    69,    -1,    50,    51,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    60,
      -1,    66,    63,    68,    69,    66,    -1,    68,    69,     1,
      -1,     3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,
      -1,    23,    -1,    -1,     1,    23,     3,     4,     1,    -1,
       3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,
      -1,    -1,    15,    16,    -1,    -1,    23,    -1,    50,    51,
      23,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,
      -1,    63,    60,    -1,    66,    63,    68,    69,    66,    -1,
      68,    69,    -1,    50,    51,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    60,    -1,    -1,    63,    60,    -1,    66,
      63,    68,    69,    66,    -1,    68,    69,     1,    -1,     3,
       4,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    16,    -1,    -1,    15,    16,    -1,    -1,    23,
      -1,    -1,     1,    23,     3,     4,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,
      15,    16,    -1,    -1,    23,    -1,    50,    51,    23,    -1,
      50,    51,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
      60,    -1,    66,    63,    68,    69,    66,    -1,    68,    69,
      -1,    50,    51,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    60,    -1,    -1,    63,    60,    -1,    66,    63,    68,
      69,    66,    -1,    68,    69,     1,    -1,     3,     4,     1,
      -1,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      16,    -1,    -1,    15,    16,    -1,    -1,    23,    -1,    -1,
       1,    23,     3,     4,     1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,
      -1,    -1,    23,    -1,    50,    51,    23,    -1,    50,    51,
      -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    60,    -1,
      66,    63,    68,    69,    66,    -1,    68,    69,    -1,    50,
      51,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    63,    60,    -1,    66,    63,    68,    69,    66,
      -1,    68,    69,     1,    -1,     3,     4,     1,    -1,     3,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,
      -1,    15,    16,    -1,    -1,    23,    -1,    -1,     1,    23,
       3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,    -1,
      23,    -1,    50,    51,    23,    -1,    50,    51,    -1,    -1,
      -1,    -1,    60,    -1,    -1,    63,    60,    -1,    66,    63,
      68,    69,    66,    -1,    68,    69,    -1,    50,    51,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,    -1,
      63,    60,    -1,    66,    63,    68,    69,    66,    -1,    68,
      69,     1,    -1,     3,     4,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    15,
      16,    -1,    -1,    23,    -1,    -1,     1,    23,     3,     4,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    15,    16,    -1,    -1,    23,    -1,
      50,    51,    23,    -1,    50,    51,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,    60,    -1,    66,    63,    68,    69,
      66,    -1,    68,    69,    -1,    50,    51,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    60,
      -1,    66,    63,    68,    69,    66,    -1,    68,    69,     1,
      -1,     3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,
      -1,    23,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    -1,    -1,    -1,    50,    51,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,
      -1,    63,    60,    -1,    66,    63,    68,    69,    66,    -1,
      68,    69,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    -1,    -1,    58,    59,
       3,     4,    -1,    -1,    -1,    65,    -1,    -1,    -1,    -1,
      -1,    71,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,     3,     4,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    -1,    -1,    15,    16,    -1,    -1,
      -1,    -1,    50,    51,    23,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    60,     3,     4,    63,    -1,    -1,    66,    -1,
      68,    69,    -1,    -1,    -1,    15,    16,    -1,    -1,    -1,
      49,    50,    51,    23,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    60,    61,    -1,    63,     3,     4,    66,    -1,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    49,
      50,    51,    -1,    53,    -1,    23,    -1,    -1,     3,     4,
      60,    -1,    -1,    63,    -1,    -1,    66,    -1,    68,    69,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,    60,    -1,    -1,    63,    64,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    50,    51,    -1,    -1,    -1,
      23,    -1,    -1,     3,     4,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    68,    69,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,    60,    -1,    -1,
      63,    64,    -1,    66,    -1,    68,    69,    15,    16,    -1,
      50,    51,    -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,     1,    -1,    66,    67,    68,    69,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,     1,
      -1,    58,    59,    -1,    61,     7,     8,     9,    10,    11,
      12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     7,     8,     9,    10,    11,
      12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    -1,    -1,    58,    59,    -1,    61,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    -1,    -1,    58,    59,    -1,    61,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     7,     8,     9,
      10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    58,    59,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    -1,    -1,    58,    59,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     7,     8,     9,    10,    11,
      12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,     7,
       8,     9,    10,    11,    12,    13,    14,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    58,    59,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    -1,    -1,    -1,    58,    59,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      58,    59,    10,    11,    12,    13,    14,    -1,    -1,    -1,
      -1,    -1,    10,    11,    12,    13,    14,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      58,    59,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      58,    59
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,     4,    15,    16,    19,    20,    21,    22,
      23,    24,    27,    30,    31,    36,    37,    40,    41,    50,
      51,    60,    63,    66,    68,    69,    73,    74,    75,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    90,
      97,    99,   100,   106,   107,   108,   109,   110,   119,     3,
       4,    15,    16,    15,    16,     1,     3,    15,    16,    63,
     101,   108,   110,     1,     3,    39,    63,    76,     1,     3,
      60,    98,     1,   108,   108,    79,   108,   108,     1,   108,
       1,     5,     6,    64,   108,   115,   116,   118,     1,    67,
     114,   116,   118,     1,   108,     3,    60,     0,    74,    80,
       5,     6,    71,   117,     3,     4,    15,    16,     7,     8,
       9,    10,    11,    12,    13,    14,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    58,
      59,    60,    62,    66,    70,   111,   115,    26,   102,   103,
     104,     1,     5,     6,    65,    91,   117,     1,     5,     6,
      60,    62,    92,    62,     1,     3,    64,    77,    78,     1,
       3,    62,     1,     3,    91,    91,    91,    32,    89,     1,
      61,    64,   116,   115,   117,    67,   116,   114,    61,    77,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,    49,    53,    61,   108,   112,   113,     1,   108,
     113,     3,    60,    64,     1,   101,    25,   105,   104,    79,
       1,    61,    77,    79,    62,     1,    64,   117,     1,    60,
      92,     1,   108,     1,    62,    18,    79,    18,    79,    28,
      93,    94,    95,     3,    79,     1,    18,    62,    64,   118,
      64,   116,   108,    67,   118,    67,   116,   108,    61,     1,
       3,     1,    61,   117,     1,    67,   108,     1,    79,     1,
      18,    92,    61,     1,    62,    78,     1,    61,    77,    79,
       1,   108,     1,    18,     1,    18,   108,    29,    96,    95,
      92,     1,     3,    64,    67,   108,    62,   112,    61,    79,
       1,    92,    92,    61,     1,    61,    91,    79,     1,    18,
      79,     1,    60,    66,     1,   108,    79,    79,     1,    92,
      79,     1,    61,   113,     1,   113,    79,     1,    61,     1,
      67
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    72,    73,    73,    73,    73,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      75,    75,    76,    76,    76,    76,    76,    76,    76,    76,
      77,    77,    78,    79,    79,    80,    80,    80,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    82,    83,    83,    83,    83,    83,
      83,    83,    83,    83,    83,    84,    85,    86,    87,    88,
      88,    89,    89,    89,    90,    90,    91,    91,    91,    91,
      92,    92,    93,    93,    94,    94,    95,    96,    96,    97,
      97,    97,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    99,    99,    99,    99,   100,   100,   100,   101,   101,
     101,   102,   102,   103,   103,   104,   104,   105,   105,   105,
     106,   106,   107,   107,   107,   107,   107,   107,   107,   107,
     107,   107,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   109,   109,   109,   109,   109,   109,   109,   109,
     109,   109,   109,   109,   109,   109,   109,   109,   109,   109,
     109,   109,   110,   110,   111,   111,   111,   111,   111,   111,
     111,   112,   112,   112,   112,   112,   112,   113,   113,   114,
     114,   115,   115,   116,   116,   117,   118,   118,   119,   119,
     119
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     0,     1,     8,     7,     5,     4,
       7,     6,     2,     3,     4,     6,     3,     4,     5,     7,
       1,     2,     2,     2,     4,     3,     2,     2,     3,     4,
       1,     3,     1,     1,     2,     2,     2,     2,     1,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     1,     1,     1,     1,     4,
       4,     4,     2,     0,     6,     6,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     4,     2,     0,     4,
       5,     5,     5,     3,     1,     5,     4,     3,     2,     3,
       1,     4,     5,     2,     5,     5,     2,     5,     3,     2,
       2,     0,     1,     1,     2,     2,     2,     2,     0,     2,
       3,     3,     7,     8,     8,     5,     8,     8,     7,     7,
       6,     5,     3,     3,     1,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     2,     2,     2,     2,     3,     3,     3,     3,     2,
       2,     1,     1,     1,     1,     1,     2,     5,     4,     1,
       3,     2,     4,     4,     5,     2,     3,     4,     4,     5,
       2,     2,     1,     2,     3,     2,     3,     3,     3,     2,
       4,     1,     1,     4,     4,     2,     2,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     3,     3,     3,
       2
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
#line 2094 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 5: /* program: error  */
#line 73 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
          {yyxpt(_("statement list or function definition"),yyvsp[0]);}
#line 2100 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2112 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2125 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2138 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2152 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2165 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2179 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 12: /* functionDef: FUNCTION error  */
#line 126 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                     {yyxpt(_("legal function name or return declaration after 'function'"), yyvsp[-1]);}
#line 2185 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 13: /* functionDef: FUNCTION IDENT error  */
#line 127 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyxpt(_("argument list or statement list after identifier '") + yyvsp[-1].v.p->text.c_str() + "'",yyvsp[-1]);}
#line 2191 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 14: /* functionDef: FUNCTION IDENT '(' error  */
#line 128 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyxpt(_("(possibly empty) argument list after '('"),yyvsp[-1]);}
#line 2197 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 15: /* functionDef: FUNCTION IDENT '(' argumentList ')' error  */
#line 129 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                {yyxpt(_("statement list after ')'"),yyvsp[-1]);}
#line 2203 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 16: /* functionDef: FUNCTION returnDeclaration error  */
#line 130 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                        {yyxpt(_("function name for function declared"),yyvsp[-2]);}
#line 2209 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 17: /* functionDef: FUNCTION returnDeclaration IDENT error  */
#line 131 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                             {yyxpt(_("argument list or statement list following function name :") + yyvsp[-1].v.p->text.c_str(), yyvsp[-1]);}
#line 2215 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 18: /* functionDef: FUNCTION returnDeclaration IDENT '(' error  */
#line 132 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                  {yyxpt(_("(possibly empty) argument list after '('"),yyvsp[-1]);}
#line 2221 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 19: /* functionDef: FUNCTION returnDeclaration IDENT '(' argumentList ')' error  */
#line 133 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                  {yyxpt(_("statement list after ')'"),yyvsp[-1]);}
#line 2227 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 22: /* returnDeclaration: VARARGOUT '='  */
#line 142 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = yyvsp[-1].v.p;}
#line 2233 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 23: /* returnDeclaration: IDENT '='  */
#line 143 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = yyvsp[-1].v.p;}
#line 2239 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 24: /* returnDeclaration: '[' argumentList ']' '='  */
#line 144 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                             {yyval.v.p = yyvsp[-2].v.p;}
#line 2245 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 25: /* returnDeclaration: '[' ']' '='  */
#line 145 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = nullptr;}
#line 2251 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 26: /* returnDeclaration: IDENT error  */
#line 146 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyxpt(_("an '=' symbol after identifier in return declaration"),yyvsp[-1]);}
#line 2257 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 27: /* returnDeclaration: '[' error  */
#line 147 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("a valid list of return arguments in return declaration"),yyvsp[-1]);}
#line 2263 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 28: /* returnDeclaration: '[' argumentList error  */
#line 148 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyxpt(_("matching ']' in return declaration for '['"),yyvsp[-2]);}
#line 2269 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 29: /* returnDeclaration: '[' argumentList ']' error  */
#line 149 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                               {yyxpt(_("an '=' symbol after return declaration"),yyvsp[-1]);}
#line 2275 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 30: /* argumentList: argument  */
#line 153 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyval.v.p = yyvsp[0].v.p;}
#line 2281 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 31: /* argumentList: argumentList columnSep argument  */
#line 154 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                    {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2287 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 33: /* statementList: statement  */
#line 162 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyval.v.p = AbstractSyntaxTree::createNode(OP_BLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 2293 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 34: /* statementList: statementList statement  */
#line 163 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2299 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 35: /* statement: statementType ENDQSTMNT  */
#line 167 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {
      yyval.v.p = AbstractSyntaxTree::createNode(OP_QSTATEMENT,NULL,yyvsp[0].v.i);
      yyval.v.p->down = yyvsp[-1].v.p;
  }
#line 2308 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 36: /* statement: statementType ENDSTMNT  */
#line 171 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {
     yyval.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yyvsp[0].v.i);
     yyval.v.p->down = yyvsp[-1].v.p;
  }
#line 2317 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 37: /* statement: statementType columnSep  */
#line 175 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                            {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yyvsp[0].v.i);
    yyval.v.p->down = yyvsp[-1].v.p;
  }
#line 2326 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 40: /* statementType: %empty  */
#line 184 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
    {yyval.v.p = AbstractSyntaxTree::createNode(null_node,"",-1);}
#line 2332 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 55: /* specialSyntaxStatement: IDENT NUMERIC  */
#line 204 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2338 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 56: /* specialSyntaxStatement: STRING STRING  */
#line 205 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2344 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 57: /* specialSyntaxStatement: CHARACTER CHARACTER  */
#line 206 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2350 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 58: /* specialSyntaxStatement: IDENT STRING  */
#line 207 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                 {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2356 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 59: /* specialSyntaxStatement: IDENT CHARACTER  */
#line 208 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2362 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 60: /* specialSyntaxStatement: IDENT IDENT  */
#line 209 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext()); }
#line 2368 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 61: /* specialSyntaxStatement: specialSyntaxStatement STRING  */
#line 210 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                  {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2374 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 62: /* specialSyntaxStatement: specialSyntaxStatement CHARACTER  */
#line 211 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                     {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2380 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 63: /* specialSyntaxStatement: specialSyntaxStatement IDENT  */
#line 212 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2386 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 64: /* specialSyntaxStatement: specialSyntaxStatement NUMERIC  */
#line 213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2392 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 69: /* tryStatement: TRY statementList optionalCatch END  */
#line 232 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {
    yyval.v.p = yyvsp[-3].v.p;
    yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2402 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 70: /* tryStatement: TRY statementList optionalCatch error  */
#line 238 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyxpt(_("matching 'end' to 'try' clause from line ") + decodeline(yyvsp[-3]),yyvsp[0]);}
#line 2408 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 71: /* optionalCatch: CATCH IDENT newLine statementList  */
#line 242 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                    {
    yyval.v.p = yyvsp[-2].v.p;
    yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2417 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 72: /* optionalCatch: CATCH statementList  */
#line 246 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyval.v.p = yyvsp[0].v.p;}
#line 2423 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 73: /* optionalCatch: %empty  */
#line 247 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
    {yyval.v.p = nullptr;}
#line 2429 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 74: /* switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause END  */
#line 251 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                  {
    yyval.v.p = yyvsp[-5].v.p;
    yyval.v.p->addChild(yyvsp[-4].v.p);
    if (yyvsp[-2].v.p != nullptr) yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2440 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 75: /* switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause error  */
#line 257 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                      {
    yyxpt(_("matching 'end' to 'switch' clause from line ") + decodeline(yyvsp[-5]),yyvsp[0]);
  }
#line 2448 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 82: /* caseBlock: %empty  */
#line 276 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyval.v.p = nullptr;}
#line 2454 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 84: /* caseList: caseStatement  */
#line 281 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_CASEBLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());
  }
#line 2462 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 85: /* caseList: caseList caseStatement  */
#line 284 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {
    yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2470 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 86: /* caseStatement: CASE expr optionalEndStatement statementList  */
#line 290 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                               {
    yyval.v.p = yyvsp[-3].v.p; yyval.v.p->addChild(yyvsp[-2].v.p); yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2478 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 87: /* otherwiseClause: OTHERWISE statementList  */
#line 296 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2486 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 88: /* otherwiseClause: %empty  */
#line 299 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
    {
    yyval.v.p = nullptr;
  }
#line 2494 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 89: /* forStatement: FOR forIndexExpression optionalEndStatement END  */
#line 305 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                  {
   yyval.v.p = nullptr;
  }
#line 2502 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 90: /* forStatement: FOR forIndexExpression optionalEndStatement statementList END  */
#line 308 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                  {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2512 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 91: /* forStatement: FOR forIndexExpression optionalEndStatement statementList error  */
#line 314 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyxpt(_("'end' to match 'for' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2518 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 92: /* forIndexExpression: '(' IDENT '=' expr ')'  */
#line 318 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {yyval.v.p = yyvsp[-3].v.p; yyval.v.p->addChild(yyvsp[-1].v.p);}
#line 2524 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 93: /* forIndexExpression: IDENT '=' expr  */
#line 319 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2530 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 94: /* forIndexExpression: IDENT  */
#line 320 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
          {yyval.v.p = yyvsp[0].v.p;
        yyval.v.p->addChild(AbstractSyntaxTree::createNode(OP_RHS, AbstractSyntaxTree::createNode(id_node,yyvsp[0].v.p->text.c_str(), yyvsp[0].v.p->getContext()),yyvsp[0].v.p->getContext())); }
#line 2537 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 95: /* forIndexExpression: '(' IDENT '=' expr error  */
#line 322 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                             {yyxpt(_("matching right parenthesis"),yyvsp[-4]);}
#line 2543 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 96: /* forIndexExpression: '(' IDENT '=' error  */
#line 323 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("indexing expression"),yyvsp[-1]);}
#line 2549 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 97: /* forIndexExpression: '(' IDENT error  */
#line 324 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("equals operator after loop index"),yyvsp[-1]);}
#line 2555 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 98: /* forIndexExpression: '(' error  */
#line 325 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("identifier that is the loop variable"),yyvsp[-1]);}
#line 2561 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 99: /* forIndexExpression: IDENT '=' error  */
#line 326 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("indexing expression"),yyvsp[-1]);}
#line 2567 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 100: /* forIndexExpression: error  */
#line 327 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
          {yyxpt(_("identifier or assignment (id = expr) after 'for' "),yyvsp[0]);}
#line 2573 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 101: /* whileStatement: WHILE expr optionalEndStatement END  */
#line 331 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {
    yyval.v.p = nullptr;
  }
#line 2581 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 102: /* whileStatement: WHILE expr optionalEndStatement statementList END  */
#line 334 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2591 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 103: /* whileStatement: WHILE error  */
#line 339 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyxpt(_("test expression after 'while'"),yyvsp[-1]);}
#line 2597 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 104: /* whileStatement: WHILE expr optionalEndStatement statementList error  */
#line 340 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                        {yyxpt(_("'end' to match 'while' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2603 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 105: /* ifStatement: IF conditionedStatement elseIfBlock elseStatement END  */
#line 344 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                        {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    if (yyvsp[-2].v.p != nullptr) yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2614 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 106: /* ifStatement: IF error  */
#line 350 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyxpt(_("condition expression for 'if'"),yyvsp[-1]);}
#line 2620 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 107: /* ifStatement: IF conditionedStatement elseIfBlock elseStatement error  */
#line 351 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                            {yyxpt(_("'end' to match 'if' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2626 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 108: /* conditionedStatement: expr optionalEndStatement statementList  */
#line 355 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                          {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-2].v.p->getContext());
  }
#line 2634 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 109: /* conditionedStatement: expr optionalEndStatement  */
#line 358 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {
	  yyval.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yyvsp[-1].v.p,yyvsp[-1].v.p->getContext());
	}
#line 2642 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 110: /* conditionedStatement: expr error  */
#line 361 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt("valid list of statements after condition",yyvsp[0]);}
#line 2648 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 111: /* elseIfBlock: %empty  */
#line 365 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = nullptr;}
#line 2654 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 113: /* elseIfStatementList: elseIfStatement  */
#line 370 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_ELSEIFBLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());
  }
#line 2662 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 114: /* elseIfStatementList: elseIfStatementList elseIfStatement  */
#line 373 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                        {
    yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2670 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 115: /* elseIfStatement: ELSEIF conditionedStatement  */
#line 379 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2678 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 116: /* elseIfStatement: ELSEIF error  */
#line 382 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                 {yyxpt(_("test condition for 'elseif' clause"),yyvsp[-1]);}
#line 2684 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 117: /* elseStatement: ELSE statementList  */
#line 386 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                     {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2692 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 118: /* elseStatement: %empty  */
#line 389 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyval.v.p = nullptr;}
#line 2698 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 119: /* elseStatement: ELSE error  */
#line 390 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt(_("statement list for 'else' clause"),yyvsp[-1]);}
#line 2704 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 120: /* assignmentStatement: symbRefList '=' expr  */
#line 394 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_ASSIGN,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2710 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 121: /* assignmentStatement: symbRefList '=' error  */
#line 395 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyxpt(_("expression in assignment"),yyvsp[-1]);}
#line 2716 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 122: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' ')'  */
#line 399 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                        {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-5].v.p,yyvsp[-2].v.p,yyvsp[-6].v.i);
  }
#line 2724 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 123: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList ')'  */
#line 402 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyvsp[-3].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-6].v.p,yyvsp[-3].v.p,yyvsp[-7].v.i);
  }
#line 2733 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 124: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList '}'  */
#line 406 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyvsp[-3].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-6].v.p,yyvsp[-3].v.p,yyvsp[-7].v.i);
  }
#line 2742 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 125: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT  */
#line 410 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {
    yyvsp[0].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,NULL,-1));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-3].v.p,yyvsp[0].v.p,yyvsp[-4].v.i);
  }
#line 2751 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 126: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList error  */
#line 414 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {yyxpt(_("matching right bracket"), yyvsp[-2]);}
#line 2757 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 127: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList error  */
#line 415 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {yyxpt(_("matching right parenthesis"), yyvsp[-2]);}
#line 2763 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 128: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' error  */
#line 416 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                            {yyxpt(_("indexing list"), yyvsp[-1]);}
#line 2769 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 129: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' error  */
#line 417 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                            {yyxpt(_("indexing list"), yyvsp[-1]);}
#line 2775 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 130: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT error  */
#line 418 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                       {yyxpt(_("left parenthesis"),yyvsp[-1]);}
#line 2781 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 131: /* multiFunctionCall: '[' matrixDef ']' '=' error  */
#line 419 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyxpt("identifier",yyvsp[-1]);}
#line 2787 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 132: /* expr: expr ':' expr  */
#line 423 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_COLON,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2793 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 133: /* expr: expr ':' error  */
#line 424 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after ':'"), yyvsp[-1]);}
#line 2799 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 135: /* expr: expr '+' expr  */
#line 426 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_PLUS,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2805 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 136: /* expr: expr '+' error  */
#line 427 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '+'"), yyvsp[-1]);}
#line 2811 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 137: /* expr: expr '-' expr  */
#line 428 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SUBTRACT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2817 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 138: /* expr: expr '-' error  */
#line 429 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '-'"), yyvsp[-1]);}
#line 2823 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 139: /* expr: expr '*' expr  */
#line 430 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_TIMES,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2829 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 140: /* expr: expr '*' error  */
#line 431 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '*'"),yyvsp[-1]);}
#line 2835 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 141: /* expr: expr '/' expr  */
#line 432 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_RDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2841 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 142: /* expr: expr '/' error  */
#line 433 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '/'"),yyvsp[-1]);}
#line 2847 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 143: /* expr: expr '\\' expr  */
#line 434 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_LDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2853 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 144: /* expr: expr '\\' error  */
#line 435 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '\\'"),yyvsp[-1]);}
#line 2859 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 145: /* expr: expr '|' expr  */
#line 436 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_OR,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2865 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 146: /* expr: expr '|' error  */
#line 437 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '|'"),yyvsp[-1]);}
#line 2871 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 147: /* expr: expr '&' expr  */
#line 438 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_AND,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2877 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 148: /* expr: expr '&' error  */
#line 439 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '&'"),yyvsp[-1]);}
#line 2883 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 149: /* expr: expr SOR expr  */
#line 440 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SOR,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2889 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 150: /* expr: expr SOR error  */
#line 441 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '||'"),yyvsp[-1]);}
#line 2895 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 151: /* expr: expr SAND expr  */
#line 442 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_SAND,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2901 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 152: /* expr: expr SAND error  */
#line 443 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '&&'"),yyvsp[-1]);}
#line 2907 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 153: /* expr: expr '<' expr  */
#line 444 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_LT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2913 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 154: /* expr: expr '<' error  */
#line 445 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '<'"),yyvsp[-1]);}
#line 2919 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 155: /* expr: expr LE expr  */
#line 446 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_LEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2925 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 156: /* expr: expr LE error  */
#line 447 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '<='"),yyvsp[-1]);}
#line 2931 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 157: /* expr: expr '>' expr  */
#line 448 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_GT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2937 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 158: /* expr: expr '>' error  */
#line 449 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '>'"),yyvsp[-1]);}
#line 2943 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 159: /* expr: expr GE expr  */
#line 450 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_GEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2949 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 160: /* expr: expr GE error  */
#line 451 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '>='"),yyvsp[-1]);}
#line 2955 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 161: /* expr: expr EQ expr  */
#line 452 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2961 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 162: /* expr: expr EQ error  */
#line 453 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '=='"),yyvsp[-1]);}
#line 2967 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 163: /* expr: expr NE expr  */
#line 454 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_NEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2973 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 164: /* expr: expr NE error  */
#line 455 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '~='"),yyvsp[-1]);}
#line 2979 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 165: /* expr: expr DOTTIMES expr  */
#line 456 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_TIMES,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2985 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 166: /* expr: expr DOTTIMES error  */
#line 457 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after '.*'"), yyvsp[-1]);}
#line 2991 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 167: /* expr: expr DOTRDIV expr  */
#line 458 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_RDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2997 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 168: /* expr: expr DOTRDIV error  */
#line 459 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after './'"),yyvsp[-1]);}
#line 3003 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 169: /* expr: expr DOTLDIV expr  */
#line 460 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_LDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3009 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 170: /* expr: expr DOTLDIV error  */
#line 461 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyxpt(_("an expression after '.\\'"),yyvsp[-1]);}
#line 3015 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 171: /* expr: '-' expr  */
#line 462 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_UMINUS,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3021 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 172: /* expr: '+' expr  */
#line 463 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_UPLUS, yyvsp[0].v.p, yyvsp[-1].v.i);}
#line 3027 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 173: /* expr: '~' expr  */
#line 464 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_NOT,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3033 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 174: /* expr: '~' error  */
#line 465 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after logical not"),yyvsp[0]);}
#line 3039 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 175: /* expr: expr '^' expr  */
#line 466 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_MPOWER,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3045 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 176: /* expr: expr '^' error  */
#line 467 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '^'"),yyvsp[-1]);}
#line 3051 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 177: /* expr: expr DOTPOWER expr  */
#line 468 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_POWER,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3057 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 178: /* expr: expr DOTPOWER error  */
#line 469 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after '.^'"),yyvsp[-1]);}
#line 3063 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 179: /* expr: expr '\''  */
#line 470 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_TRANSPOSE,yyvsp[-1].v.p,yyvsp[0].v.i);}
#line 3069 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 180: /* expr: expr DOTTRANSPOSE  */
#line 471 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_TRANSPOSE,yyvsp[-1].v.p,yyvsp[0].v.i);}
#line 3075 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 186: /* terminal: '@' IDENT  */
#line 480 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p =  AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_NAMED, yyvsp[0].v.p, yyvsp[0].v.i);}
#line 3081 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 187: /* terminal: '@' '(' argumentList ')' expr  */
#line 481 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yyvsp[-2].v.p,yyvsp[0].v.p, yyvsp[-2].v.p->getContext());}
#line 3087 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 188: /* terminal: '@' '(' ')' expr  */
#line 482 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                     {yyval.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yyvsp[0].v.p, yyvsp[0].v.p->getContext());}
#line 3093 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 189: /* terminal: symbRefList  */
#line 483 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_RHS,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3099 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 190: /* terminal: '[' matrixDef ']'  */
#line 484 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = yyvsp[-1].v.p;}
#line 3105 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 191: /* terminal: '[' error  */
#line 485 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("a matrix definition followed by a right bracket"),yyvsp[-1]);}
#line 3111 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 192: /* terminal: '[' rowSeperator matrixDef ']'  */
#line 486 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-1].v.p;}
#line 3117 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 193: /* terminal: '[' matrixDef rowSeperator ']'  */
#line 487 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p;}
#line 3123 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 194: /* terminal: '[' rowSeperator matrixDef rowSeperator ']'  */
#line 488 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                {yyval.v.p = yyvsp[-2].v.p;}
#line 3129 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 195: /* terminal: '[' ']'  */
#line 489 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yyvsp[-1].v.i);}
#line 3135 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 196: /* terminal: '{' cellDef '}'  */
#line 490 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = yyvsp[-1].v.p;}
#line 3141 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 197: /* terminal: '{' rowSeperator cellDef '}'  */
#line 491 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyval.v.p = yyvsp[-1].v.p;}
#line 3147 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 198: /* terminal: '{' cellDef rowSeperator '}'  */
#line 492 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyval.v.p = yyvsp[-2].v.p;}
#line 3153 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 199: /* terminal: '{' rowSeperator cellDef rowSeperator '}'  */
#line 493 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                              {yyval.v.p = yyvsp[-2].v.p;}
#line 3159 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 200: /* terminal: '{' '}'  */
#line 494 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yyvsp[-1].v.i);}
#line 3165 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 201: /* terminal: '{' error  */
#line 495 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("a cell-array definition followed by a right brace"),yyvsp[-1]);}
#line 3171 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 203: /* symbRefList: symbRefList symbRef  */
#line 500 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3177 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 204: /* symbRef: '(' indexList ')'  */
#line 504 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i); }
#line 3183 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 205: /* symbRef: '(' ')'  */
#line 505 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyval.v.p = AbstractSyntaxTree::createNode(OP_PARENS,NULL,yyvsp[-1].v.i); }
#line 3189 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 206: /* symbRef: '(' indexList error  */
#line 506 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("matching right parenthesis"),yyvsp[-2]);}
#line 3195 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 207: /* symbRef: '{' indexList '}'  */
#line 507 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yyvsp[-1].v.p,yyvsp[-2].v.i); }
#line 3201 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 208: /* symbRef: '{' indexList error  */
#line 508 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("matching right brace"),yyvsp[-2]);}
#line 3207 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 209: /* symbRef: '.' IDENT  */
#line 509 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT,yyvsp[0].v.p,yyvsp[-1].v.i); }
#line 3213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 210: /* symbRef: '.' '(' expr ')'  */
#line 510 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOTDYN,yyvsp[-1].v.p,yyvsp[-3].v.i);}
#line 3219 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 212: /* indexElement: ':'  */
#line 515 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyval.v.p = AbstractSyntaxTree::createNode(OP_ALL,NULL,yyvsp[0].v.i);}
#line 3225 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 213: /* indexElement: '/' IDENT '=' expr  */
#line 516 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_KEYWORD,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-3].v.i);}
#line 3231 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 214: /* indexElement: '/' IDENT '=' error  */
#line 517 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("expecting expression after '=' in keyword assignment"),yyvsp[-1]);}
#line 3237 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 215: /* indexElement: '/' IDENT  */
#line 518 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_KEYWORD,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3243 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 216: /* indexElement: '/' error  */
#line 519 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("expecting keyword identifier after '/' in keyword assignment"),yyvsp[-1]);}
#line 3249 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 218: /* indexList: indexList columnSep indexElement  */
#line 524 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                     {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addPeer(yyvsp[0].v.p);}
#line 3255 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 219: /* cellDef: rowDef  */
#line 528 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
         {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3261 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 220: /* cellDef: cellDef rowSeperator rowDef  */
#line 529 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3267 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 221: /* matrixDef: rowDef  */
#line 533 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
         {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACKETS,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3273 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 222: /* matrixDef: matrixDef rowSeperator rowDef  */
#line 534 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                  {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3279 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 226: /* rowDef: expr  */
#line 547 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
       {yyval.v.p = AbstractSyntaxTree::createNode(OP_SEMICOLON,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3285 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 227: /* rowDef: rowDef columnSep expr  */
#line 548 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3291 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 228: /* parenExpr: '(' expr ')'  */
#line 552 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyval.v.p = yyvsp[-1].v.p;}
#line 3297 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 229: /* parenExpr: '(' expr error  */
#line 553 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("a right parenthesis after expression to match this one"),yyvsp[-2]);}
#line 3303 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 230: /* parenExpr: '(' error  */
#line 554 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt(_("an expression after left parenthesis"),yyvsp[-1]);}
#line 3309 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;


#line 3313 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"

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

#line 556 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"

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
