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
  YYSYMBOL_65_ = 65,                       /* ','  */
  YYSYMBOL_66_ = 66,                       /* ';'  */
  YYSYMBOL_67_ = 67,                       /* '{'  */
  YYSYMBOL_68_ = 68,                       /* '}'  */
  YYSYMBOL_69_ = 69,                       /* '~'  */
  YYSYMBOL_70_ = 70,                       /* '@'  */
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
#define YYLAST   3093

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
      60,    61,    52,    50,    65,    51,    71,    53,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    49,    66,
      46,    62,    47,     2,    70,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    63,    54,    64,    58,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    67,    44,    68,    69,     2,     2,     2,
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
     109,   117,   126,   127,   129,   130,   131,   132,   134,   135,
     139,   140,   144,   145,   146,   147,   148,   149,   150,   151,
     155,   156,   160,   164,   165,   169,   173,   177,   184,   185,
     186,   187,   188,   189,   190,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   203,   206,   207,   208,   209,   210,
     211,   212,   213,   214,   215,   218,   222,   226,   229,   233,
     239,   244,   248,   249,   253,   259,   265,   265,   265,   265,
     269,   269,   274,   274,   278,   281,   287,   293,   296,   302,
     305,   310,   315,   316,   317,   319,   320,   321,   322,   323,
     324,   328,   331,   336,   337,   342,   348,   349,   353,   356,
     359,   363,   364,   368,   371,   377,   380,   383,   386,   387,
     391,   392,   396,   399,   403,   407,   411,   412,   413,   414,
     415,   416,   420,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,   431,   432,   433,   434,   435,   436,   437,
     438,   439,   440,   441,   442,   443,   444,   445,   446,   447,
     448,   449,   450,   451,   452,   453,   454,   455,   456,   457,
     458,   459,   460,   461,   462,   463,   464,   465,   466,   467,
     468,   469,   473,   474,   475,   476,   477,   478,   479,   480,
     481,   482,   483,   484,   485,   486,   487,   488,   489,   490,
     491,   492,   495,   496,   499,   500,   501,   502,   503,   504,
     505,   508,   509,   510,   511,   512,   513,   517,   518,   522,
     523,   527,   528,   532,   532,   536,   540,   541,   544,   545,
     546
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
  "','", "';'", "'{'", "'}'", "'~'", "'@'", "'.'", "$accept", "program",
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

#define YYPACT_NINF (-132)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-126)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     349,  -132,   267,  -132,    -4,    37,  1386,    15,    55,  -132,
    -132,  1412,  2640,  -132,  2466,  -132,  -132,  -132,  -132,  2640,
    2640,  1444,  1296,    34,  1470,    39,    17,  -132,    42,   755,
    -132,   177,  -132,   359,  -132,  -132,  -132,  -132,  -132,  -132,
    -132,  -132,  -132,  -132,  -132,  2887,  -132,   218,  -132,  -132,
    -132,  -132,  -132,  -132,  -132,  -132,  -132,  -132,  -132,  1296,
      33,  2707,   227,  -132,   168,    11,    25,   141,  -132,    19,
     175,   133,  -132,  2732,  2732,  1023,   289,   289,  -132,  2792,
    -132,  -132,  -132,  -132,  2887,   165,  2640,     2,  -132,  -132,
      27,  2640,     2,  -132,   289,  -132,   131,  -132,  -132,  -132,
    -132,  -132,  -132,  -132,  -132,  -132,  -132,  1502,  1528,  1560,
    1586,  1618,  1644,  1676,  -132,  1702,  1734,  1760,  1792,  1818,
    1850,  1876,  1908,  1934,  1966,  1992,  2024,  2050,  2082,  -132,
     144,  2108,  2553,   143,  -132,   185,  2140,    65,    33,  -132,
    -132,  -132,  -132,  -132,  -132,  2292,   215,  -132,  -132,   186,
    -132,  2466,  -132,  -132,  -132,    87,     7,  -132,  -132,   214,
    2166,  -132,    26,  2350,  2408,    68,  2524,    46,  -132,  -132,
      88,  2267,   204,  -132,  2640,  -132,  2567,    38,  2640,   123,
    -132,  3034,  -132,  3034,  -132,  3034,  -132,   289,  -132,   289,
    -132,   289,  -132,   289,  -132,  2940,  -132,  2956,  -132,  3009,
    -132,  3023,  -132,  3034,  -132,  3034,  -132,  3034,  -132,  2757,
    -132,  2670,  -132,  2670,  -132,   289,  -132,   289,  -132,   289,
    -132,   289,  -132,   250,  -132,  2887,  -132,     9,  -132,  2887,
      14,  -132,  2640,  -132,  -132,  -132,   965,    50,  -132,  2292,
    -132,   240,   135,   407,  -132,  -132,    29,   155,  -132,   205,
    2466,  -132,  2887,  -132,  2198,  -132,   825,  -132,   895,  2640,
     136,    68,  -132,   219,  1081,  -132,  -132,   251,  -132,     2,
    -132,  2582,  2887,  -132,     2,  -132,  2625,  2887,  2640,  -132,
     106,  -132,  -132,  2553,  -132,  -132,  2867,  -132,  1139,  -132,
    -132,  2466,    60,  -132,  -132,  -132,  -132,   240,   156,   465,
    -132,  2812,  -132,  -132,  -132,  -132,  2732,  2466,    51,  -132,
    2466,  -132,    40,  -132,  -132,  2887,  2224,  -132,  -132,   523,
    -132,  2466,  2466,   226,  -132,  -132,  2466,  1197,  -132,  -132,
    1255,  -132,  1325,  1354,  -132,  2887,   581,   639,  -132,  2466,
    2292,  -132,  -132,    75,  -132,    18,   697,  -132,  -132,  -132,
    -132
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
      35,    36,    37,    63,    64,    62,    61,     0,     0,     0,
       0,     0,     0,     0,   180,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
       0,     0,     0,     0,   203,     0,     0,   118,   112,   113,
     110,    78,    77,    76,    79,   109,    13,    81,    80,     0,
      23,    40,    22,    27,    32,     0,     0,    30,    16,     0,
       0,    98,     0,    40,    40,    82,    40,     0,   229,   228,
     190,     0,     0,   225,     0,   196,     0,     0,     0,     0,
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
    -132,  -132,   209,  -132,  -132,   -94,    -6,   -10,     5,  -132,
    -132,  -132,  -132,  -132,  -132,  -132,  -132,  -132,  -132,   -68,
    -111,  -132,  -132,    -2,  -132,  -132,  -132,  -132,  -132,   129,
    -132,  -132,   137,  -132,  -132,  -132,     1,  -132,     0,  -132,
     -16,  -131,   182,   -23,     8,  -132,   -14,  -132
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    26,    27,    28,    67,   156,   157,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,   167,    39,   145,
     151,   260,   261,   262,   308,    40,    71,    41,    42,    60,
     137,   138,   139,   237,    43,    44,    45,    46,    62,   134,
     226,   227,    90,    85,    86,   174,    87,    48
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      47,   230,   179,   163,    75,   164,   165,    61,   245,    92,
     281,    53,    73,    74,    47,   284,    63,    97,    64,   349,
      76,    77,    79,    84,    84,    94,   153,   253,   154,    47,
     293,    91,    81,    82,    99,    88,   135,    56,     3,    81,
      82,   331,    95,    81,    82,  -125,  -125,   265,   250,    57,
      58,   289,   328,    54,    65,   242,    68,    10,    69,   136,
      84,   320,     7,   172,   266,   147,   148,   173,   290,   329,
     282,   246,   247,   152,   283,    47,   347,    92,    66,   283,
      99,   160,   285,   283,    19,    20,   350,    84,   254,   155,
     236,   294,    84,   171,    21,   175,   259,    59,   176,    96,
     332,    23,    89,    24,    25,  -125,   275,   333,   181,   183,
     185,   187,   189,   191,   193,    70,   195,   197,   199,   201,
     203,   205,   207,   209,   211,   213,   215,   217,   219,   221,
     291,   225,   229,   225,   154,   239,   348,    61,   141,   142,
     283,   243,   158,   171,   159,    47,   231,    56,     3,   244,
     267,    47,   310,   256,   258,   298,   264,   269,   154,    57,
      58,   252,   274,    47,    47,   307,    47,    10,   316,   146,
      81,    82,    84,   147,   148,   272,   161,    84,   162,   277,
     271,   321,   100,   101,   278,   276,   322,   240,   247,   154,
      81,    82,   178,   222,    19,    20,   292,   223,   143,   144,
     247,   343,   345,   232,    21,   224,   296,    59,   154,    81,
      82,    23,   339,    24,    25,   248,   -26,   323,   -26,   147,
     148,   247,    49,    50,   147,   148,   288,   338,   149,   170,
     150,   147,   148,   286,    51,    52,    47,    98,   326,    47,
     299,   295,   102,    47,    99,   147,   148,   241,    99,   233,
      47,   279,   311,   280,   312,   301,    47,   269,    47,   309,
     306,    99,   274,    99,    47,   235,   297,   317,   270,    99,
      49,    50,    84,   177,   249,   238,     0,    84,   130,   315,
     131,   319,    51,    52,   225,   132,     0,   130,    47,   133,
       0,    47,     0,    99,   132,     0,     0,   327,   133,    47,
     330,     0,   113,   114,    99,     0,     0,    47,     0,     0,
      47,   336,   337,     0,     0,     0,   340,   335,     0,    47,
       0,    47,    47,     0,    99,     0,    47,    47,     0,   346,
      47,     0,    99,   225,   225,    99,    47,    47,     0,    47,
      47,    99,    99,     0,     0,    99,    47,   128,   129,    -4,
       1,    99,     2,     3,   -40,   -40,     0,     0,     0,     0,
       0,     0,   103,   104,     4,     5,     0,     0,     6,     7,
       8,     9,    10,    11,   105,   106,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,    19,
      20,     0,     0,     0,     0,     0,     0,    -9,     0,    21,
       2,     3,    22,     0,   -40,     0,    23,     0,    24,    25,
       0,     0,     4,     5,     0,     0,     6,    -9,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,     0,     0,    -8,     0,    21,     2,     3,
      22,     0,     0,     0,    23,     0,    24,    25,     0,     0,
       4,     5,     0,     0,     6,    -8,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,    19,    20,     0,     0,     0,
       0,     0,     0,   -11,     0,    21,     2,     3,    22,     0,
       0,     0,    23,     0,    24,    25,     0,     0,     4,     5,
       0,     0,     6,   -11,     8,     9,    10,    11,     0,     0,
      12,     0,     0,    13,    14,     0,     0,     0,     0,    15,
      16,     0,     0,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,    19,    20,     0,     0,     0,     0,     0,
       0,    -7,     0,    21,     2,     3,    22,     0,     0,     0,
      23,     0,    24,    25,     0,     0,     4,     5,     0,     0,
       6,    -7,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,    19,    20,     0,     0,     0,     0,     0,     0,   -10,
       0,    21,     2,     3,    22,     0,     0,     0,    23,     0,
      24,    25,     0,     0,     4,     5,     0,     0,     6,   -10,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,    19,
      20,     0,     0,     0,     0,     0,     0,    -6,     0,    21,
       2,     3,    22,     0,     0,     0,    23,     0,    24,    25,
       0,     0,     4,     5,     0,     0,     6,    -6,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,     0,     0,    -2,     0,    21,     2,     3,
      22,     0,     0,     0,    23,     0,    24,    25,     0,     0,
       4,     5,     0,     0,     6,     0,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    21,     0,     0,    22,     0,
       0,     0,    23,     0,    24,    25,   302,     0,     2,     3,
     -40,   -40,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     0,   303,     6,     0,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    21,     0,     0,    22,     0,
     -40,     0,    23,     0,    24,    25,   304,     0,     2,     3,
     -40,   -40,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     0,   305,     6,     0,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    21,     0,     0,    22,     0,
     -40,     0,    23,     0,    24,    25,   287,     0,     2,     3,
     -40,   -40,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     0,     0,     6,     0,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,     0,     0,     0,
       0,     0,     0,     0,     0,    19,    20,     0,     0,     0,
       0,     0,     0,     0,   -73,    21,     2,     3,    22,     0,
     -40,     0,    23,     0,    24,    25,     0,     0,     4,     5,
       0,   -73,     6,     0,     8,     9,    10,    11,     0,     0,
      12,     0,     0,    13,    14,   166,     0,     0,     0,    15,
      16,     0,     0,    17,    18,     0,     0,     0,     0,     0,
       0,     0,     0,    19,    20,     0,     0,     0,     0,     0,
       0,     0,   -72,    21,     2,     3,    22,     0,     0,     0,
      23,     0,    24,    25,     0,     0,     4,     5,     0,   -72,
       6,     0,     8,     9,    10,    11,     0,     0,    12,     0,
       0,    13,    14,     0,     0,     0,     0,    15,    16,     0,
       0,    17,    18,     0,     0,     0,     0,     0,     0,     0,
       0,    19,    20,     0,     0,     0,     0,     0,     0,     0,
    -117,    21,     2,     3,    22,     0,     0,     0,    23,     0,
      24,    25,     0,     0,     4,     5,     0,  -117,     6,     0,
       8,     9,    10,    11,     0,     0,    12,     0,     0,    13,
      14,     0,     0,     0,     0,    15,    16,     0,     0,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,    19,
      20,     0,     0,     0,     0,     0,     0,     0,   -87,    21,
       2,     3,    22,     0,     0,     0,    23,     0,    24,    25,
       0,     0,     4,     5,     0,   -87,     6,     0,     8,     9,
      10,    11,     0,     0,    12,     0,     0,    13,    14,     0,
       0,     0,     0,    15,    16,     0,     0,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
       0,     0,     0,     0,     0,     0,   -71,    21,     2,     3,
      22,     0,     0,     0,    23,     0,    24,    25,     0,     0,
       4,     5,     0,   -71,     6,     0,     8,     9,    10,    11,
       0,     0,    12,     0,     0,    13,    14,     0,     0,     0,
       0,    15,    16,     0,     0,    17,    18,    80,     0,    56,
       3,    81,    82,     0,     0,    19,    20,     0,     0,     0,
       0,    57,    58,     0,     0,    21,     0,     0,    22,    10,
       0,     0,    23,     0,    24,    25,   341,     0,    56,     3,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,    58,     0,     0,     0,     0,    19,    20,    10,     0,
       0,     0,     0,     0,     0,   344,    21,    56,     3,    59,
      83,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,     0,     0,   222,    19,    20,    10,   223,     0,
       0,     0,     0,     0,     0,    21,   342,    55,    59,    56,
       3,     0,    23,     0,    24,    25,     0,     0,     0,     0,
       0,    57,    58,   222,    19,    20,     0,   223,     0,    10,
       0,     0,     0,    72,    21,    56,     3,    59,     0,     0,
       0,    23,     0,    24,    25,     0,     0,    57,    58,     0,
       0,     0,     0,     0,     0,    10,    19,    20,     0,     0,
       0,     0,     0,     0,     0,    78,    21,    56,     3,    59,
       0,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,    19,    20,     0,     0,     0,    10,     0,     0,
       0,    93,    21,    56,     3,    59,     0,     0,     0,    23,
       0,    24,    25,     0,     0,    57,    58,     0,     0,     0,
       0,     0,     0,    10,    19,    20,     0,     0,     0,     0,
       0,     0,     0,   180,    21,    56,     3,    59,     0,     0,
       0,    23,     0,    24,    25,     0,     0,    57,    58,     0,
      19,    20,     0,     0,     0,    10,     0,     0,     0,   182,
      21,    56,     3,    59,     0,     0,     0,    23,     0,    24,
      25,     0,     0,    57,    58,     0,     0,     0,     0,     0,
       0,    10,    19,    20,     0,     0,     0,     0,     0,     0,
       0,   184,    21,    56,     3,    59,     0,     0,     0,    23,
       0,    24,    25,     0,     0,    57,    58,     0,    19,    20,
       0,     0,     0,    10,     0,     0,     0,   186,    21,    56,
       3,    59,     0,     0,     0,    23,     0,    24,    25,     0,
       0,    57,    58,     0,     0,     0,     0,     0,     0,    10,
      19,    20,     0,     0,     0,     0,     0,     0,     0,   188,
      21,    56,     3,    59,     0,     0,     0,    23,     0,    24,
      25,     0,     0,    57,    58,     0,    19,    20,     0,     0,
       0,    10,     0,     0,     0,   190,    21,    56,     3,    59,
       0,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,     0,     0,     0,     0,     0,    10,    19,    20,
       0,     0,     0,     0,     0,     0,     0,   192,    21,    56,
       3,    59,     0,     0,     0,    23,     0,    24,    25,     0,
       0,    57,    58,     0,    19,    20,     0,     0,     0,    10,
       0,     0,     0,   194,    21,    56,     3,    59,     0,     0,
       0,    23,     0,    24,    25,     0,     0,    57,    58,     0,
       0,     0,     0,     0,     0,    10,    19,    20,     0,     0,
       0,     0,     0,     0,     0,   196,    21,    56,     3,    59,
       0,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,    19,    20,     0,     0,     0,    10,     0,     0,
       0,   198,    21,    56,     3,    59,     0,     0,     0,    23,
       0,    24,    25,     0,     0,    57,    58,     0,     0,     0,
       0,     0,     0,    10,    19,    20,     0,     0,     0,     0,
       0,     0,     0,   200,    21,    56,     3,    59,     0,     0,
       0,    23,     0,    24,    25,     0,     0,    57,    58,     0,
      19,    20,     0,     0,     0,    10,     0,     0,     0,   202,
      21,    56,     3,    59,     0,     0,     0,    23,     0,    24,
      25,     0,     0,    57,    58,     0,     0,     0,     0,     0,
       0,    10,    19,    20,     0,     0,     0,     0,     0,     0,
       0,   204,    21,    56,     3,    59,     0,     0,     0,    23,
       0,    24,    25,     0,     0,    57,    58,     0,    19,    20,
       0,     0,     0,    10,     0,     0,     0,   206,    21,    56,
       3,    59,     0,     0,     0,    23,     0,    24,    25,     0,
       0,    57,    58,     0,     0,     0,     0,     0,     0,    10,
      19,    20,     0,     0,     0,     0,     0,     0,     0,   208,
      21,    56,     3,    59,     0,     0,     0,    23,     0,    24,
      25,     0,     0,    57,    58,     0,    19,    20,     0,     0,
       0,    10,     0,     0,     0,   210,    21,    56,     3,    59,
       0,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,     0,     0,     0,     0,     0,    10,    19,    20,
       0,     0,     0,     0,     0,     0,     0,   212,    21,    56,
       3,    59,     0,     0,     0,    23,     0,    24,    25,     0,
       0,    57,    58,     0,    19,    20,     0,     0,     0,    10,
       0,     0,     0,   214,    21,    56,     3,    59,     0,     0,
       0,    23,     0,    24,    25,     0,     0,    57,    58,     0,
       0,     0,     0,     0,     0,    10,    19,    20,     0,     0,
       0,     0,     0,     0,     0,   216,    21,    56,     3,    59,
       0,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,    19,    20,     0,     0,     0,    10,     0,     0,
       0,   218,    21,    56,     3,    59,     0,     0,     0,    23,
       0,    24,    25,     0,     0,    57,    58,     0,     0,     0,
       0,     0,     0,    10,    19,    20,     0,     0,     0,     0,
       0,     0,     0,   220,    21,    56,     3,    59,     0,     0,
       0,    23,     0,    24,    25,     0,     0,    57,    58,     0,
      19,    20,     0,     0,     0,    10,     0,     0,     0,   228,
      21,    56,     3,    59,     0,     0,     0,    23,     0,    24,
      25,     0,     0,    57,    58,     0,     0,     0,     0,     0,
       0,    10,    19,    20,     0,     0,     0,     0,     0,     0,
       0,   234,    21,    56,     3,    59,     0,     0,     0,    23,
       0,    24,    25,     0,     0,    57,    58,     0,    19,    20,
       0,     0,     0,    10,     0,     0,     0,   251,    21,    56,
       3,    59,     0,     0,     0,    23,     0,    24,    25,     0,
       0,    57,    58,     0,     0,     0,     0,     0,     0,    10,
      19,    20,     0,     0,     0,     0,     0,     0,     0,   300,
      21,    56,     3,    59,     0,     0,     0,    23,     0,    24,
      25,     0,     0,    57,    58,     0,    19,    20,     0,     0,
       0,    10,     0,     0,     0,   334,    21,    56,     3,    59,
       0,     0,     0,    23,     0,    24,    25,     0,     0,    57,
      58,     0,     0,     0,     0,     0,     0,    10,    19,    20,
       0,     0,     0,     0,     0,     0,     0,     0,    21,     0,
       0,    59,     0,     0,     0,    23,     0,    24,    25,     0,
      56,     3,     0,     0,    19,    20,     0,     0,     0,     0,
       0,     0,    57,    58,    21,     0,     0,    59,     0,     0,
      10,    23,     0,    24,    25,     2,     3,   -40,   -40,     0,
       0,     0,     0,     0,     0,     0,     0,     4,     5,     0,
       0,     6,     0,     8,     9,    10,    11,    19,    20,    12,
       0,     0,    13,    14,     0,     0,     0,    21,    15,    16,
      59,   268,    17,    18,    23,     0,    24,    25,     0,     0,
       0,     0,    19,    20,     0,     0,     0,     0,     0,     0,
       0,     0,    21,     2,     3,    22,     0,   -40,     0,    23,
       0,    24,    25,     0,     0,     4,     5,     0,   255,     6,
       0,     8,     9,    10,    11,     0,     0,    12,     0,     0,
      13,    14,     0,     0,     0,     0,    15,    16,     0,     0,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
      19,    20,     0,     0,     0,     0,     0,     0,     0,     0,
      21,     2,     3,    22,     0,     0,     0,    23,     0,    24,
      25,     0,     0,     4,     5,     0,   257,     6,     0,     8,
       9,    10,    11,     0,     0,    12,     0,     0,    13,    14,
       0,     0,     0,     0,    15,    16,     0,     0,    17,    18,
       0,     0,     0,     0,     0,     0,     0,     0,    19,    20,
       0,     0,     0,     0,     0,     0,     0,     0,    21,     2,
       3,    22,     0,     0,     0,    23,     0,    24,    25,     0,
       0,     4,     5,     0,     0,     6,     0,     8,     9,    10,
      11,     0,     0,    12,     0,     0,    13,    14,     0,     0,
       0,     0,    15,    16,     0,     0,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,    19,    20,     0,     0,
       0,     0,     0,     0,     0,     0,    21,   263,     3,    22,
       0,     0,     0,    23,     0,    24,    25,     0,     0,     4,
       5,     0,     0,     6,     0,     8,     9,    10,    11,     0,
       0,    12,     0,     0,    13,    14,    56,     3,     0,     0,
      15,    16,     0,     0,    17,    18,     0,     0,    57,    58,
      56,     3,     0,     0,    19,    20,    10,     0,     0,     0,
       0,     0,    57,    58,    21,    56,     3,    22,     0,     0,
      10,    23,     0,    24,    25,     0,     0,    57,    58,     0,
       0,     0,   222,    19,    20,    10,   223,     0,     0,     0,
       0,     0,     0,    21,     0,     0,    59,    19,    20,     0,
      23,     0,    24,    25,     0,     0,     0,    21,    56,     3,
      59,     0,    19,    20,    23,   273,    24,    25,     0,     0,
      57,    58,    21,    56,     3,    59,   313,     0,    10,    23,
       0,    24,    25,     0,     0,    57,    58,     0,     0,     0,
       0,     0,     0,    10,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    19,    20,     0,     0,     0,
     110,   111,   112,   113,   114,    21,     0,     0,    59,     0,
      19,    20,    23,   314,    24,    25,     0,     0,     0,     0,
      21,     0,     0,    59,     0,     0,     0,    23,   140,    24,
      25,     0,   141,   142,   107,   108,   109,   110,   111,   112,
     113,   114,   125,   126,   127,     0,     0,     0,   128,   129,
       0,     0,     0,     0,     0,     0,     0,   141,   142,   107,
     108,   109,   110,   111,   112,   113,   114,     0,     0,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,     0,     0,     0,   128,   129,   110,   111,   112,
     113,   114,   143,   144,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,     0,     0,     0,
     128,   129,     0,   168,     0,     0,     0,   143,   144,   107,
     108,   109,   110,   111,   112,   113,   114,   123,   124,   125,
     126,   127,     0,   324,     0,   128,   129,     0,     0,   107,
     108,   109,   110,   111,   112,   113,   114,     0,     0,     0,
       0,     0,     0,     0,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,     0,     0,     0,
     128,   129,     0,   169,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,     0,     0,     0,
     128,   129,     0,   325,   107,   108,   109,   110,   111,   112,
     113,   114,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   107,   108,   109,   110,   111,   112,
     113,   114,     0,     0,     0,     0,     0,     0,     0,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,     0,     0,     0,   128,   129,     0,   318,   115,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,     0,     0,     0,   128,   129,   107,   108,   109,
     110,   111,   112,   113,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   108,   109,   110,   111,   112,   113,
     114,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,     0,     0,     0,   128,   129,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,     0,     0,     0,   128,   129,   107,   108,   109,   110,
     111,   112,   113,   114,     0,     0,     0,     0,     0,     0,
     107,   108,   109,   110,   111,   112,   113,   114,     0,     0,
       0,     0,     0,     0,   110,   111,   112,   113,   114,     0,
       0,     0,     0,     0,   118,   119,   120,   121,   122,   123,
     124,   125,   126,   127,     0,     0,     0,   128,   129,   119,
     120,   121,   122,   123,   124,   125,   126,   127,     0,     0,
       0,   128,   129,   122,   123,   124,   125,   126,   127,     0,
       0,     0,   128,   129
};

static const yytype_int16 yycheck[] =
{
       0,   132,    96,    71,    14,    73,    74,     6,     1,    23,
       1,    15,    11,    12,    14,     1,     1,     0,     3,     1,
      19,    20,    21,    22,    23,    24,     1,     1,     3,    29,
       1,    23,     5,     6,    29,     1,    59,     3,     4,     5,
       6,     1,     3,     5,     6,     5,     6,     1,   159,    15,
      16,     1,     1,    16,    39,   149,     1,    23,     3,    26,
      59,     1,    20,    86,    18,     5,     6,    65,    18,    18,
      61,    64,    65,    62,    65,    75,     1,    91,    63,    65,
      75,    62,    68,    65,    50,    51,    68,    86,    62,    64,
      25,    62,    91,    85,    60,    68,    28,    63,    90,    60,
      60,    67,    68,    69,    70,    65,    68,    67,   107,   108,
     109,   110,   111,   112,   113,    60,   115,   116,   117,   118,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     241,   130,   131,   132,     3,   145,    61,   136,     5,     6,
      65,   151,     1,   135,     3,   145,     3,     3,     4,    62,
      62,   151,   263,   163,   164,   249,   166,   171,     3,    15,
      16,   160,   176,   163,   164,    29,   166,    23,    62,     1,
       5,     6,   171,     5,     6,   174,     1,   176,     3,   178,
     172,   292,     5,     6,    61,   177,   297,     1,    65,     3,
       5,     6,    61,    49,    50,    51,    61,    53,    65,    66,
      65,   332,   333,    60,    60,    61,     1,    63,     3,     5,
       6,    67,   323,    69,    70,     1,     1,    61,     3,     5,
       6,    65,     3,     4,     5,     6,   236,     1,    60,    64,
      62,     5,     6,   232,    15,    16,   236,    28,   306,   239,
     250,   247,    65,   243,   239,     5,     6,    61,   243,    64,
     250,     1,     1,     3,     3,   254,   256,   271,   258,   261,
     259,   256,   276,   258,   264,   136,    61,   283,    64,   264,
       3,     4,   271,    91,    60,   138,    -1,   276,    60,   278,
      62,   291,    15,    16,   283,    67,    -1,    60,   288,    71,
      -1,   291,    -1,   288,    67,    -1,    -1,   307,    71,   299,
     310,    -1,    13,    14,   299,    -1,    -1,   307,    -1,    -1,
     310,   321,   322,    -1,    -1,    -1,   326,   316,    -1,   319,
      -1,   321,   322,    -1,   319,    -1,   326,   327,    -1,   339,
     330,    -1,   327,   332,   333,   330,   336,   337,    -1,   339,
     340,   336,   337,    -1,    -1,   340,   346,    58,    59,     0,
       1,   346,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,    15,    16,    -1,    -1,    19,    20,
      21,    22,    23,    24,    15,    16,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,    60,
       3,     4,    63,    -1,    65,    -1,    67,    -1,    69,    70,
      -1,    -1,    15,    16,    -1,    -1,    19,    20,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,     0,    -1,    60,     3,     4,
      63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,
      15,    16,    -1,    -1,    19,    20,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,     0,    -1,    60,     3,     4,    63,    -1,
      -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,
      -1,    -1,    19,    20,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,
      -1,     0,    -1,    60,     3,     4,    63,    -1,    -1,    -1,
      67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,    -1,
      19,    20,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,     0,
      -1,    60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,
      69,    70,    -1,    -1,    15,    16,    -1,    -1,    19,    20,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,    60,
       3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,
      -1,    -1,    15,    16,    -1,    -1,    19,    20,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,     0,    -1,    60,     3,     4,
      63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
      -1,    -1,    67,    -1,    69,    70,     1,    -1,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
      65,    -1,    67,    -1,    69,    70,     1,    -1,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
      65,    -1,    67,    -1,    69,    70,     1,    -1,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,
      65,    -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,
      -1,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    32,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,
      67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,
      69,    70,    -1,    -1,    15,    16,    -1,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    60,
       3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,
      -1,    -1,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    60,     3,     4,
      63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,
      15,    16,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,     1,    -1,     3,
       4,     5,     6,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    15,    16,    -1,    -1,    60,    -1,    -1,    63,    23,
      -1,    -1,    67,    -1,    69,    70,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    -1,    -1,    50,    51,    23,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      64,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    -1,    -1,    49,    50,    51,    23,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    60,    61,     1,    63,     3,
       4,    -1,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    15,    16,    49,    50,    51,    -1,    53,    -1,    23,
      -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    50,    51,    -1,    -1,    -1,    23,    -1,    -1,
      -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      50,    51,    -1,    -1,    -1,    23,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    50,    51,
      -1,    -1,    -1,    23,    -1,    -1,    -1,     1,    60,     3,
       4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
      -1,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    50,    51,    -1,    -1,
      -1,    23,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    50,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    60,     3,
       4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
      -1,    15,    16,    -1,    50,    51,    -1,    -1,    -1,    23,
      -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    50,    51,    -1,    -1,    -1,    23,    -1,    -1,
      -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      50,    51,    -1,    -1,    -1,    23,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    50,    51,
      -1,    -1,    -1,    23,    -1,    -1,    -1,     1,    60,     3,
       4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
      -1,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    50,    51,    -1,    -1,
      -1,    23,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    50,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    60,     3,
       4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
      -1,    15,    16,    -1,    50,    51,    -1,    -1,    -1,    23,
      -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    50,    51,    -1,    -1,    -1,    23,    -1,    -1,
      -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      -1,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      50,    51,    -1,    -1,    -1,    23,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    60,     3,     4,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    50,    51,
      -1,    -1,    -1,    23,    -1,    -1,    -1,     1,    60,     3,
       4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
      -1,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    23,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    50,    51,    -1,    -1,
      -1,    23,    -1,    -1,    -1,     1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    50,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,    -1,
      -1,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
       3,     4,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,    60,    -1,    -1,    63,    -1,    -1,
      23,    67,    -1,    69,    70,     3,     4,     5,     6,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    50,    51,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    60,    36,    37,
      63,    64,    40,    41,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    60,     3,     4,    63,    -1,    65,    -1,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    18,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      60,     3,     4,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    -1,    -1,    15,    16,    -1,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,     3,
       4,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    -1,
      -1,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,
      -1,    -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    60,     3,     4,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    -1,    -1,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    27,    -1,    -1,    30,    31,     3,     4,    -1,    -1,
      36,    37,    -1,    -1,    40,    41,    -1,    -1,    15,    16,
       3,     4,    -1,    -1,    50,    51,    23,    -1,    -1,    -1,
      -1,    -1,    15,    16,    60,     3,     4,    63,    -1,    -1,
      23,    67,    -1,    69,    70,    -1,    -1,    15,    16,    -1,
      -1,    -1,    49,    50,    51,    23,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    60,    -1,    -1,    63,    50,    51,    -1,
      67,    -1,    69,    70,    -1,    -1,    -1,    60,     3,     4,
      63,    -1,    50,    51,    67,    68,    69,    70,    -1,    -1,
      15,    16,    60,     3,     4,    63,    64,    -1,    23,    67,
      -1,    69,    70,    -1,    -1,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      10,    11,    12,    13,    14,    60,    -1,    -1,    63,    -1,
      50,    51,    67,    68,    69,    70,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,    -1,    -1,    -1,    67,     1,    69,
      70,    -1,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    52,    53,    54,    -1,    -1,    -1,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    -1,    -1,    58,    59,    10,    11,    12,
      13,    14,    65,    66,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      58,    59,    -1,     1,    -1,    -1,    -1,    65,    66,     7,
       8,     9,    10,    11,    12,    13,    14,    50,    51,    52,
      53,    54,    -1,     1,    -1,    58,    59,    -1,    -1,     7,
       8,     9,    10,    11,    12,    13,    14,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      58,    59,    -1,    61,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
      58,    59,    -1,    61,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    -1,    -1,    58,    59,    -1,    61,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    -1,    -1,    58,    59,     7,     8,     9,
      10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     7,     8,     9,    10,    11,    12,    13,
      14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    -1,    -1,    -1,    58,    59,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    -1,    -1,    58,    59,     7,     8,     9,    10,
      11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    10,    11,    12,    13,    14,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    -1,    -1,    -1,    58,    59,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    58,    59,    49,    50,    51,    52,    53,    54,    -1,
      -1,    -1,    58,    59
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,     4,    15,    16,    19,    20,    21,    22,
      23,    24,    27,    30,    31,    36,    37,    40,    41,    50,
      51,    60,    63,    67,    69,    70,    73,    74,    75,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    90,
      97,    99,   100,   106,   107,   108,   109,   110,   119,     3,
       4,    15,    16,    15,    16,     1,     3,    15,    16,    63,
     101,   108,   110,     1,     3,    39,    63,    76,     1,     3,
      60,    98,     1,   108,   108,    79,   108,   108,     1,   108,
       1,     5,     6,    64,   108,   115,   116,   118,     1,    68,
     114,   116,   118,     1,   108,     3,    60,     0,    74,    80,
       5,     6,    65,     3,     4,    15,    16,     7,     8,     9,
      10,    11,    12,    13,    14,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    58,    59,
      60,    62,    67,    71,   111,   115,    26,   102,   103,   104,
       1,     5,     6,    65,    66,    91,     1,     5,     6,    60,
      62,    92,    62,     1,     3,    64,    77,    78,     1,     3,
      62,     1,     3,    91,    91,    91,    32,    89,     1,    61,
      64,   116,   115,    65,   117,    68,   116,   114,    61,    77,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,    49,    53,    61,   108,   112,   113,     1,   108,
     113,     3,    60,    64,     1,   101,    25,   105,   104,    79,
       1,    61,    77,    79,    62,     1,    64,    65,     1,    60,
      92,     1,   108,     1,    62,    18,    79,    18,    79,    28,
      93,    94,    95,     3,    79,     1,    18,    62,    64,   118,
      64,   116,   108,    68,   118,    68,   116,   108,    61,     1,
       3,     1,    61,    65,     1,    68,   108,     1,    79,     1,
      18,    92,    61,     1,    62,    78,     1,    61,    77,    79,
       1,   108,     1,    18,     1,    18,   108,    29,    96,    95,
      92,     1,     3,    64,    68,   108,    62,   112,    61,    79,
       1,    92,    92,    61,     1,    61,    91,    79,     1,    18,
      79,     1,    60,    67,     1,   108,    79,    79,     1,    92,
      79,     1,    61,   113,     1,   113,    79,     1,    61,     1,
      68
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
#line 2108 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 5: /* program: error  */
#line 73 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
          {yyxpt(_("statement list or function definition"),yyvsp[0]);}
#line 2114 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2126 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2139 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2152 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2166 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2179 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
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
#line 2193 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 12: /* functionDef: FUNCTION error  */
#line 126 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("legal function name or return declaration after 'function'"), yyvsp[-1]);}
#line 2199 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 13: /* functionDef: FUNCTION IDENT error  */
#line 127 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {yyxpt(_("argument list or statement list after identifier '") +
  yyvsp[-1].v.p->text.c_str() + "'",yyvsp[-1]);}
#line 2206 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 14: /* functionDef: FUNCTION IDENT '(' error  */
#line 129 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("(possibly empty) argument list after '('"),yyvsp[-1]);}
#line 2212 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 15: /* functionDef: FUNCTION IDENT '(' argumentList ')' error  */
#line 130 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                              {yyxpt(_("statement list after ')'"),yyvsp[-1]);}
#line 2218 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 16: /* functionDef: FUNCTION returnDeclaration error  */
#line 131 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {yyxpt(_("function name for function declared"),yyvsp[-2]);}
#line 2224 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 17: /* functionDef: FUNCTION returnDeclaration IDENT error  */
#line 132 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                           {yyxpt(_("argument list or statement list following function name :") +
  yyvsp[-1].v.p->text.c_str(), yyvsp[-1]);}
#line 2231 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 18: /* functionDef: FUNCTION returnDeclaration IDENT '(' error  */
#line 134 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                {yyxpt(_("(possibly empty) argument list after '('"),yyvsp[-1]);}
#line 2237 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 19: /* functionDef: FUNCTION returnDeclaration IDENT '(' argumentList ')' error  */
#line 135 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                {yyxpt(_("statement list after ')'"),yyvsp[-1]);}
#line 2243 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 22: /* returnDeclaration: VARARGOUT '='  */
#line 144 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = yyvsp[-1].v.p;}
#line 2249 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 23: /* returnDeclaration: IDENT '='  */
#line 145 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = yyvsp[-1].v.p;}
#line 2255 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 24: /* returnDeclaration: '[' argumentList ']' '='  */
#line 146 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                             {yyval.v.p = yyvsp[-2].v.p;}
#line 2261 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 25: /* returnDeclaration: '[' ']' '='  */
#line 147 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = nullptr;}
#line 2267 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 26: /* returnDeclaration: IDENT error  */
#line 148 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyxpt(_("an '=' symbol after identifier in return declaration"),yyvsp[-1]);}
#line 2273 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 27: /* returnDeclaration: '[' error  */
#line 149 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("a valid list of return arguments in return declaration"),yyvsp[-1]);}
#line 2279 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 28: /* returnDeclaration: '[' argumentList error  */
#line 150 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyxpt(_("matching ']' in return declaration for '['"),yyvsp[-2]);}
#line 2285 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 29: /* returnDeclaration: '[' argumentList ']' error  */
#line 151 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                               {yyxpt(_("an '=' symbol after return declaration"),yyvsp[-1]);}
#line 2291 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 30: /* argumentList: argument  */
#line 155 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyval.v.p = yyvsp[0].v.p;}
#line 2297 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 31: /* argumentList: argumentList ',' argument  */
#line 156 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2303 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 33: /* statementList: statement  */
#line 164 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyval.v.p = AbstractSyntaxTree::createNode(OP_BLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 2309 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 34: /* statementList: statementList statement  */
#line 165 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2315 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 35: /* statement: statementType ENDQSTMNT  */
#line 169 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {
        yyval.v.p = AbstractSyntaxTree::createNode(OP_QSTATEMENT,NULL,yyvsp[0].v.i);
      yyval.v.p->down = yyvsp[-1].v.p;
   }
#line 2324 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 36: /* statement: statementType ENDSTMNT  */
#line 173 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                            {
      yyval.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yyvsp[0].v.i);
            yyval.v.p->down = yyvsp[-1].v.p;
   }
#line 2333 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 37: /* statement: statementType ','  */
#line 177 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {
      yyval.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yyvsp[0].v.i);
      yyval.v.p->down = yyvsp[-1].v.p;
   }
#line 2342 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 40: /* statementType: %empty  */
#line 186 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
     {yyval.v.p = AbstractSyntaxTree::createNode(null_node,"",-1);}
#line 2348 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 55: /* specialSyntaxStatement: IDENT NUMERIC  */
#line 206 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2354 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 56: /* specialSyntaxStatement: STRING STRING  */
#line 207 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2360 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 57: /* specialSyntaxStatement: CHARACTER CHARACTER  */
#line 208 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2366 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 58: /* specialSyntaxStatement: IDENT STRING  */
#line 209 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2372 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 59: /* specialSyntaxStatement: IDENT CHARACTER  */
#line 210 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext());}
#line 2378 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 60: /* specialSyntaxStatement: IDENT IDENT  */
#line 211 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yyvsp[-1].v.p,yyvsp[0].v.p,yyvsp[-1].v.p->getContext()); }
#line 2384 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 61: /* specialSyntaxStatement: specialSyntaxStatement STRING  */
#line 212 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2390 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 62: /* specialSyntaxStatement: specialSyntaxStatement CHARACTER  */
#line 213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2396 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 63: /* specialSyntaxStatement: specialSyntaxStatement IDENT  */
#line 214 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                               {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2402 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 64: /* specialSyntaxStatement: specialSyntaxStatement NUMERIC  */
#line 215 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyvsp[-1].v.p->addChild(yyvsp[0].v.p);}
#line 2408 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 69: /* tryStatement: TRY statementList optionalCatch END  */
#line 234 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {
    yyval.v.p = yyvsp[-3].v.p;
    yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2418 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 70: /* tryStatement: TRY statementList optionalCatch error  */
#line 240 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyxpt(_("matching 'end' to 'try' clause from line ") + decodeline(yyvsp[-3]),yyvsp[0]);}
#line 2424 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 71: /* optionalCatch: CATCH IDENT newLine statementList  */
#line 244 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                    {
    yyval.v.p = yyvsp[-2].v.p;
    yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2433 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 72: /* optionalCatch: CATCH statementList  */
#line 248 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyval.v.p = yyvsp[0].v.p;}
#line 2439 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 73: /* optionalCatch: %empty  */
#line 249 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
    {yyval.v.p = nullptr;}
#line 2445 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 74: /* switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause END  */
#line 253 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                  {
    yyval.v.p = yyvsp[-5].v.p;
    yyval.v.p->addChild(yyvsp[-4].v.p);
    if (yyvsp[-2].v.p != nullptr) yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2456 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 75: /* switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause error  */
#line 259 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                    {
          yyxpt(_("matching 'end' to 'switch' clause from line ") + decodeline(yyvsp[-5]),yyvsp[0]);
        }
#line 2464 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 82: /* caseBlock: %empty  */
#line 274 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyval.v.p = nullptr;}
#line 2470 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 84: /* caseList: caseStatement  */
#line 278 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_CASEBLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());
  }
#line 2478 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 85: /* caseList: caseList caseStatement  */
#line 281 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {
    yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2486 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 86: /* caseStatement: CASE expr optionalEndStatement statementList  */
#line 287 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                               {
    yyval.v.p = yyvsp[-3].v.p; yyval.v.p->addChild(yyvsp[-2].v.p); yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2494 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 87: /* otherwiseClause: OTHERWISE statementList  */
#line 293 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2502 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 88: /* otherwiseClause: %empty  */
#line 296 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {
    yyval.v.p = nullptr;
  }
#line 2510 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 89: /* forStatement: FOR forIndexExpression optionalEndStatement END  */
#line 302 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                  {
   yyval.v.p = nullptr;
  }
#line 2518 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 90: /* forStatement: FOR forIndexExpression optionalEndStatement statementList END  */
#line 305 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                                {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2528 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 91: /* forStatement: FOR forIndexExpression optionalEndStatement statementList error  */
#line 311 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyxpt(_("'end' to match 'for' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2534 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 92: /* forIndexExpression: '(' IDENT '=' expr ')'  */
#line 315 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                         {yyval.v.p = yyvsp[-3].v.p; yyval.v.p->addChild(yyvsp[-1].v.p);}
#line 2540 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 93: /* forIndexExpression: IDENT '=' expr  */
#line 316 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                 {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 2546 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 94: /* forIndexExpression: IDENT  */
#line 317 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyval.v.p = yyvsp[0].v.p;
        yyval.v.p->addChild(AbstractSyntaxTree::createNode(OP_RHS, AbstractSyntaxTree::createNode(id_node,yyvsp[0].v.p->text.c_str(), yyvsp[0].v.p->getContext()),yyvsp[0].v.p->getContext())); }
#line 2553 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 95: /* forIndexExpression: '(' IDENT '=' expr error  */
#line 319 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyxpt(_("matching right parenthesis"),yyvsp[-4]);}
#line 2559 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 96: /* forIndexExpression: '(' IDENT '=' error  */
#line 320 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyxpt(_("indexing expression"),yyvsp[-1]);}
#line 2565 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 97: /* forIndexExpression: '(' IDENT error  */
#line 321 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyxpt(_("equals operator after loop index"),yyvsp[-1]);}
#line 2571 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 98: /* forIndexExpression: '(' error  */
#line 322 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyxpt(_("identifier that is the loop variable"),yyvsp[-1]);}
#line 2577 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 99: /* forIndexExpression: IDENT '=' error  */
#line 323 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyxpt(_("indexing expression"),yyvsp[-1]);}
#line 2583 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 100: /* forIndexExpression: error  */
#line 324 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyxpt(_("identifier or assignment (id = expr) after 'for' "),yyvsp[0]);}
#line 2589 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 101: /* whileStatement: WHILE expr optionalEndStatement END  */
#line 328 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {
    yyval.v.p = nullptr;
  }
#line 2597 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 102: /* whileStatement: WHILE expr optionalEndStatement statementList END  */
#line 331 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2607 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 103: /* whileStatement: WHILE error  */
#line 336 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("test expression after 'while'"),yyvsp[-1]);}
#line 2613 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 104: /* whileStatement: WHILE expr optionalEndStatement statementList error  */
#line 338 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
  {yyxpt(_("'end' to match 'while' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2619 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 105: /* ifStatement: IF conditionedStatement elseIfBlock elseStatement END  */
#line 342 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                        {
    yyval.v.p = yyvsp[-4].v.p;
    yyval.v.p->addChild(yyvsp[-3].v.p);
    if (yyvsp[-2].v.p != nullptr) yyval.v.p->addChild(yyvsp[-2].v.p);
    if (yyvsp[-1].v.p != nullptr) yyval.v.p->addChild(yyvsp[-1].v.p);
  }
#line 2630 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 106: /* ifStatement: IF error  */
#line 348 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
           {yyxpt(_("condition expression for 'if'"),yyvsp[-1]);}
#line 2636 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 107: /* ifStatement: IF conditionedStatement elseIfBlock elseStatement error  */
#line 349 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                          {yyxpt(_("'end' to match 'if' statement from line ") + decodeline(yyvsp[-4]),yyvsp[0]);}
#line 2642 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 108: /* conditionedStatement: expr optionalEndStatement statementList  */
#line 353 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                          {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-2].v.p->getContext());
  }
#line 2650 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 109: /* conditionedStatement: expr optionalEndStatement  */
#line 356 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                            {
	  yyval.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yyvsp[-1].v.p,yyvsp[-1].v.p->getContext());
	}
#line 2658 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 110: /* conditionedStatement: expr error  */
#line 359 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyxpt("valid list of statements after condition",yyvsp[0]);}
#line 2664 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 111: /* elseIfBlock: %empty  */
#line 363 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = nullptr;}
#line 2670 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 113: /* elseIfStatementList: elseIfStatement  */
#line 368 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {
    yyval.v.p = AbstractSyntaxTree::createNode(OP_ELSEIFBLOCK,yyvsp[0].v.p,yyvsp[0].v.p->getContext());
  }
#line 2678 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 114: /* elseIfStatementList: elseIfStatementList elseIfStatement  */
#line 371 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                      {
    yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);
  }
#line 2686 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 115: /* elseIfStatement: ELSEIF conditionedStatement  */
#line 377 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2694 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 116: /* elseIfStatement: ELSEIF error  */
#line 380 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt(_("test condition for 'elseif' clause"),yyvsp[-1]);}
#line 2700 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 117: /* elseStatement: ELSE statementList  */
#line 383 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                     {
    yyval.v.p = yyvsp[0].v.p;
  }
#line 2708 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 118: /* elseStatement: %empty  */
#line 386 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = nullptr;}
#line 2714 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 119: /* elseStatement: ELSE error  */
#line 387 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
             {yyxpt(_("statement list for 'else' clause"),yyvsp[-1]);}
#line 2720 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 120: /* assignmentStatement: symbRefList '=' expr  */
#line 391 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_ASSIGN,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2726 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 121: /* assignmentStatement: symbRefList '=' error  */
#line 392 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyxpt(_("expression in assignment"),yyvsp[-1]);}
#line 2732 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 122: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' ')'  */
#line 396 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                        {
  yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-5].v.p,yyvsp[-2].v.p,yyvsp[-6].v.i);
  }
#line 2740 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 123: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList ')'  */
#line 399 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyvsp[-3].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-6].v.p,yyvsp[-3].v.p,yyvsp[-7].v.i);
  }
#line 2749 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 124: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList '}'  */
#line 403 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                    {
    yyvsp[-3].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-6].v.p,yyvsp[-3].v.p,yyvsp[-7].v.i);
  }
#line 2758 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 125: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT  */
#line 407 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {
    yyvsp[0].v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,NULL,-1));
    yyval.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yyvsp[-3].v.p,yyvsp[0].v.p,yyvsp[-4].v.i);
  }
#line 2767 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 126: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList error  */
#line 411 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {yyxpt(_("matching right bracket"), yyvsp[-2]);}
#line 2773 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 127: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList error  */
#line 412 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                      {yyxpt(_("matching right parenthesis"), yyvsp[-2]);}
#line 2779 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 128: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' error  */
#line 413 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                            {yyxpt(_("indexing list"), yyvsp[-1]);}
#line 2785 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 129: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' error  */
#line 414 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                            {yyxpt(_("indexing list"), yyvsp[-1]);}
#line 2791 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 130: /* multiFunctionCall: '[' matrixDef ']' '=' IDENT error  */
#line 415 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                       {yyxpt(_("left parenthesis"),yyvsp[-1]);}
#line 2797 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 131: /* multiFunctionCall: '[' matrixDef ']' '=' error  */
#line 416 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                {yyxpt("identifier",yyvsp[-1]);}
#line 2803 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 132: /* expr: expr ':' expr  */
#line 420 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_COLON,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2809 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 133: /* expr: expr ':' error  */
#line 421 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after ':'"), yyvsp[-1]);}
#line 2815 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 135: /* expr: expr '+' expr  */
#line 423 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_PLUS,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2821 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 136: /* expr: expr '+' error  */
#line 424 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '+'"), yyvsp[-1]);}
#line 2827 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 137: /* expr: expr '-' expr  */
#line 425 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SUBTRACT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2833 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 138: /* expr: expr '-' error  */
#line 426 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '-'"), yyvsp[-1]);}
#line 2839 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 139: /* expr: expr '*' expr  */
#line 427 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_TIMES,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2845 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 140: /* expr: expr '*' error  */
#line 428 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '*'"),yyvsp[-1]);}
#line 2851 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 141: /* expr: expr '/' expr  */
#line 429 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_RDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2857 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 142: /* expr: expr '/' error  */
#line 430 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '/'"),yyvsp[-1]);}
#line 2863 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 143: /* expr: expr '\\' expr  */
#line 431 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_LDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2869 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 144: /* expr: expr '\\' error  */
#line 432 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '\\'"),yyvsp[-1]);}
#line 2875 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 145: /* expr: expr '|' expr  */
#line 433 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_OR,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2881 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 146: /* expr: expr '|' error  */
#line 434 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '|'"),yyvsp[-1]);}
#line 2887 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 147: /* expr: expr '&' expr  */
#line 435 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_AND,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2893 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 148: /* expr: expr '&' error  */
#line 436 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '&'"),yyvsp[-1]);}
#line 2899 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 149: /* expr: expr SOR expr  */
#line 437 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_SOR,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2905 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 150: /* expr: expr SOR error  */
#line 438 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '||'"),yyvsp[-1]);}
#line 2911 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 151: /* expr: expr SAND expr  */
#line 439 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_SAND,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2917 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 152: /* expr: expr SAND error  */
#line 440 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '&&'"),yyvsp[-1]);}
#line 2923 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 153: /* expr: expr '<' expr  */
#line 441 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_LT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2929 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 154: /* expr: expr '<' error  */
#line 442 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '<'"),yyvsp[-1]);}
#line 2935 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 155: /* expr: expr LE expr  */
#line 443 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_LEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2941 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 156: /* expr: expr LE error  */
#line 444 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '<='"),yyvsp[-1]);}
#line 2947 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 157: /* expr: expr '>' expr  */
#line 445 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_GT,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2953 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 158: /* expr: expr '>' error  */
#line 446 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '>'"),yyvsp[-1]);}
#line 2959 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 159: /* expr: expr GE expr  */
#line 447 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_GEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2965 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 160: /* expr: expr GE error  */
#line 448 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '>='"),yyvsp[-1]);}
#line 2971 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 161: /* expr: expr EQ expr  */
#line 449 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2977 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 162: /* expr: expr EQ error  */
#line 450 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '=='"),yyvsp[-1]);}
#line 2983 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 163: /* expr: expr NE expr  */
#line 451 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_NEQ,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 2989 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 164: /* expr: expr NE error  */
#line 452 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyxpt(_("an expression after '~='"),yyvsp[-1]);}
#line 2995 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 165: /* expr: expr DOTTIMES expr  */
#line 453 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_TIMES,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3001 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 166: /* expr: expr DOTTIMES error  */
#line 454 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after '.*'"), yyvsp[-1]);}
#line 3007 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 167: /* expr: expr DOTRDIV expr  */
#line 455 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_RDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3013 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 168: /* expr: expr DOTRDIV error  */
#line 456 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after './'"),yyvsp[-1]);}
#line 3019 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 169: /* expr: expr DOTLDIV expr  */
#line 457 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_LDIV,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3025 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 170: /* expr: expr DOTLDIV error  */
#line 458 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyxpt(_("an expression after '.\\'"),yyvsp[-1]);}
#line 3031 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 171: /* expr: '-' expr  */
#line 459 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_UMINUS,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3037 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 172: /* expr: '+' expr  */
#line 460 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_UPLUS, yyvsp[0].v.p, yyvsp[-1].v.i);}
#line 3043 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 173: /* expr: '~' expr  */
#line 461 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_NOT,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3049 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 174: /* expr: '~' error  */
#line 462 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after logical not"),yyvsp[0]);}
#line 3055 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 175: /* expr: expr '^' expr  */
#line 463 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                   {yyval.v.p = AbstractSyntaxTree::createNode(OP_MPOWER,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3061 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 176: /* expr: expr '^' error  */
#line 464 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("an expression after '^'"),yyvsp[-1]);}
#line 3067 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 177: /* expr: expr DOTPOWER expr  */
#line 465 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_POWER,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3073 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 178: /* expr: expr DOTPOWER error  */
#line 466 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("an expression after '.^'"),yyvsp[-1]);}
#line 3079 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 179: /* expr: expr '\''  */
#line 467 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_TRANSPOSE,yyvsp[-1].v.p,yyvsp[0].v.i);}
#line 3085 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 180: /* expr: expr DOTTRANSPOSE  */
#line 468 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT_TRANSPOSE,yyvsp[-1].v.p,yyvsp[0].v.i);}
#line 3091 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 186: /* terminal: '@' IDENT  */
#line 477 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p =  AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_NAMED, yyvsp[0].v.p, yyvsp[0].v.i);}
#line 3097 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 187: /* terminal: '@' '(' argumentList ')' expr  */
#line 478 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yyvsp[-2].v.p,yyvsp[0].v.p, yyvsp[-2].v.p->getContext());}
#line 3103 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 188: /* terminal: '@' '(' ')' expr  */
#line 479 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                     {yyval.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yyvsp[0].v.p, yyvsp[0].v.p->getContext());}
#line 3109 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 189: /* terminal: symbRefList  */
#line 480 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                {yyval.v.p = AbstractSyntaxTree::createNode(OP_RHS,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3115 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 190: /* terminal: '[' matrixDef ']'  */
#line 481 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = yyvsp[-1].v.p;}
#line 3121 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 191: /* terminal: '[' error  */
#line 482 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("a matrix definition followed by a right bracket"),yyvsp[-1]);}
#line 3127 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 192: /* terminal: '[' rowSeperator matrixDef ']'  */
#line 483 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-1].v.p;}
#line 3133 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 193: /* terminal: '[' matrixDef rowSeperator ']'  */
#line 484 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p;}
#line 3139 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 194: /* terminal: '[' rowSeperator matrixDef rowSeperator ']'  */
#line 485 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                                {yyval.v.p = yyvsp[-2].v.p;}
#line 3145 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 195: /* terminal: '[' ']'  */
#line 486 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yyvsp[-1].v.i);}
#line 3151 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 196: /* terminal: '{' cellDef '}'  */
#line 487 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = yyvsp[-1].v.p;}
#line 3157 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 197: /* terminal: '{' rowSeperator cellDef '}'  */
#line 488 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyval.v.p = yyvsp[-1].v.p;}
#line 3163 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 198: /* terminal: '{' cellDef rowSeperator '}'  */
#line 489 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                 {yyval.v.p = yyvsp[-2].v.p;}
#line 3169 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 199: /* terminal: '{' rowSeperator cellDef rowSeperator '}'  */
#line 490 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                              {yyval.v.p = yyvsp[-2].v.p;}
#line 3175 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 200: /* terminal: '{' '}'  */
#line 491 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                  {yyval.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yyvsp[-1].v.i);}
#line 3181 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 201: /* terminal: '{' error  */
#line 492 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("a cell-array definition followed by a right brace"),yyvsp[-1]);}
#line 3187 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 203: /* symbRefList: symbRefList symbRef  */
#line 496 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyval.v.p = yyvsp[-1].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3193 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 204: /* symbRef: '(' indexList ')'  */
#line 499 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyval.v.p = AbstractSyntaxTree::createNode(OP_PARENS,yyvsp[-1].v.p,yyvsp[-2].v.i); }
#line 3199 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 205: /* symbRef: '(' ')'  */
#line 500 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
            {yyval.v.p = AbstractSyntaxTree::createNode(OP_PARENS,NULL,yyvsp[-1].v.i); }
#line 3205 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 206: /* symbRef: '(' indexList error  */
#line 501 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("matching right parenthesis"),yyvsp[-2]);}
#line 3211 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 207: /* symbRef: '{' indexList '}'  */
#line 502 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                      {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yyvsp[-1].v.p,yyvsp[-2].v.i); }
#line 3217 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 208: /* symbRef: '{' indexList error  */
#line 503 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                              {yyxpt(_("matching right brace"),yyvsp[-2]);}
#line 3223 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 209: /* symbRef: '.' IDENT  */
#line 504 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOT,yyvsp[0].v.p,yyvsp[-1].v.i); }
#line 3229 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 210: /* symbRef: '.' '(' expr ')'  */
#line 505 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                           {yyval.v.p = AbstractSyntaxTree::createNode(OP_DOTDYN,yyvsp[-1].v.p,yyvsp[-3].v.i);}
#line 3235 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 212: /* indexElement: ':'  */
#line 509 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
        {yyval.v.p = AbstractSyntaxTree::createNode(OP_ALL,NULL,yyvsp[0].v.i);}
#line 3241 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 213: /* indexElement: '/' IDENT '=' expr  */
#line 510 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                       {yyval.v.p = AbstractSyntaxTree::createNode(OP_KEYWORD,yyvsp[-2].v.p,yyvsp[0].v.p,yyvsp[-3].v.i);}
#line 3247 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 214: /* indexElement: '/' IDENT '=' error  */
#line 511 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                        {yyxpt(_("expecting expression after '=' in keyword assignment"),yyvsp[-1]);}
#line 3253 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 215: /* indexElement: '/' IDENT  */
#line 512 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_KEYWORD,yyvsp[0].v.p,yyvsp[-1].v.i);}
#line 3259 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 216: /* indexElement: '/' error  */
#line 513 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyxpt(_("expecting keyword identifier after '/' in keyword assignment"),yyvsp[-1]);}
#line 3265 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 218: /* indexList: indexList ',' indexElement  */
#line 518 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addPeer(yyvsp[0].v.p);}
#line 3271 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 219: /* cellDef: rowDef  */
#line 522 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3277 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 220: /* cellDef: cellDef rowSeperator rowDef  */
#line 523 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                   {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3283 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 221: /* matrixDef: rowDef  */
#line 527 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
              {yyval.v.p = AbstractSyntaxTree::createNode(OP_BRACKETS,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3289 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 222: /* matrixDef: matrixDef rowSeperator rowDef  */
#line 528 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                                     {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3295 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 226: /* rowDef: expr  */
#line 540 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
       {yyval.v.p = AbstractSyntaxTree::createNode(OP_SEMICOLON,yyvsp[0].v.p,yyvsp[0].v.p->getContext());}
#line 3301 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 227: /* rowDef: rowDef columnSep expr  */
#line 541 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                          {yyval.v.p = yyvsp[-2].v.p; yyval.v.p->addChild(yyvsp[0].v.p);}
#line 3307 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 228: /* parenExpr: '(' expr ')'  */
#line 544 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyval.v.p = yyvsp[-1].v.p;}
#line 3313 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 229: /* parenExpr: '(' expr error  */
#line 545 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
                    {yyxpt(_("a right parenthesis after expression to match this one"),yyvsp[-2]);}
#line 3319 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;

  case 230: /* parenExpr: '(' error  */
#line 546 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"
               {yyxpt(_("an expression after left parenthesis"),yyvsp[-1]);}
#line 3325 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"
    break;


#line 3329 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp"

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

#line 548 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx"

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
