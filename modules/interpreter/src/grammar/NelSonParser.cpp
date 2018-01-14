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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:339  */

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

//bison -L C -k -o NelSonParser.cpp NelSonParser.yxx

#include <stdio.h>
#include <stdlib.h>
#include "AST.hpp"
#include "AstManager.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "ParserInterface.hpp"
#include "Exception.hpp"
#include "FileParser.hpp"
#include "i18n.hpp"

#define YYSTYPE ParseRHS

#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];

#include "LexerInterface.hpp"

extern int yylex(void);

extern int yydebug;

bool interactiveMode;

#ifdef WIN32
#define snprintf _snprintf
#endif

namespace Nelson {
    static ASTPtr mainAST;
    static MacroFunctionDef *mainMDef;


    void chainFunction(MacroFunctionDef *r)
    {
        r->nextFunction = nullptr;
        r->prevFunction = nullptr;
        if (mainMDef == nullptr)
        {
            mainMDef = r;
        }
        else
        {
            r->localFunction = true;
            r->nextFunction = mainMDef->nextFunction;
            if (r->nextFunction)
            {
                r->nextFunction->prevFunction = r;
            }
            mainMDef->nextFunction = r;
            r->prevFunction = mainMDef;
        }
    }

    void yyerror(const char *s)
    {
        return;
    }

    std::string decodeline(ParseRHS val)
    {
        int tokenID;
        int linenumber;
        if (val.isToken)
        {
            tokenID = val.v.i;
        }
        else
        {
            tokenID = val.v.p->context();
        }
        linenumber = tokenID & 0xFFFF;
        char buffer[IDENTIFIER_LENGTH_MAX + 1];
        sprintf(buffer,"%d",linenumber);
        return(std::string(buffer));
    }

    int yyxpt(std::string xStr, ParseRHS val)
    {
        int tokenID;
        int linenumber, colnumber;
        if (val.isToken)
        {
            tokenID = val.v.i;
        }
        else
        {
            tokenID = val.v.p->context();
        }
        linenumber = tokenID & 0xFFFF;
        colnumber = tokenID >> 16;
        if (!interactiveMode)
            snprintf(msgBuffer,MSGBUFLEN,
                     _("Expecting %s\n\tat line %d, column %d of file %s").c_str(),
                     xStr.c_str(),linenumber,colnumber,getParserFilenameU().c_str());
        else
        {
            snprintf(msgBuffer,MSGBUFLEN,_("Expecting %s").c_str(),xStr.c_str());
        }
        throw Exception(msgBuffer);
        return 0;
    }
}

using namespace Nelson;


#line 182 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:339  */

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
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
    POS = 300,
    NEG = 301,
    NOT = 302
};
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);



/* Copy the second part of user declarations.  */

#line 278 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

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

#ifndef __attribute__
/* This feature is available in gcc versions 2.5 and later.  */
# if (! defined __GNUC__ || __GNUC__ < 2 \
      || (__GNUC__ == 2 && __GNUC_MINOR__ < 5))
#  define __attribute__(Spec) /* empty */
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
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


#if ! defined yyoverflow || YYERROR_VERBOSE

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
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
    yytype_int16 yyss_alloc;
    YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
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
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  90
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2624

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  71
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  47
/* YYNRULES -- Number of rules.  */
#define YYNRULES  222
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  337

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   302

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
    0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    2,     2,     2,     2,     2,     2,     2,     2,    45,    59,
    60,    61,    52,    50,    65,    51,    70,    53,     2,     2,
    2,     2,     2,     2,     2,     2,     2,     2,    46,    66,
    47,    62,    48,     2,     2,     2,     2,     2,     2,     2,
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
    35,    36,    37,    38,    39,    40,    41,    42,    43,    49,
    55,    56,    57
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
    0,   148,   148,   149,   149,   150,   154,   166,   174,   185,
    192,   203,   210,   211,   213,   214,   215,   216,   218,   219,
    223,   224,   228,   229,   230,   231,   232,   233,   234,   235,
    239,   240,   244,   244,   256,   257,   261,   265,   269,   276,
    277,   278,   279,   280,   281,   282,   283,   284,   285,   286,
    287,   288,   289,   290,   291,   292,   296,   299,   300,   301,
    302,   303,   304,   305,   308,   312,   316,   319,   323,   329,
    334,   335,   339,   345,   351,   351,   351,   351,   355,   355,
    360,   360,   364,   367,   373,   379,   382,   388,   391,   396,
    401,   402,   403,   405,   406,   407,   408,   409,   410,   414,
    417,   422,   423,   428,   434,   435,   439,   442,   446,   447,
    451,   454,   460,   463,   466,   469,   470,   474,   475,   479,
    482,   486,   490,   494,   495,   496,   497,   498,   499,   503,
    504,   505,   506,   507,   508,   509,   510,   511,   512,   513,
    514,   515,   516,   517,   518,   519,   520,   521,   522,   523,
    524,   525,   526,   527,   528,   529,   530,   531,   532,   533,
    534,   535,   536,   537,   538,   539,   540,   541,   542,   543,
    544,   545,   546,   547,   548,   549,   550,   551,   552,   553,
    554,   558,   559,   560,   561,   562,   563,   564,   565,   566,
    567,   568,   569,   570,   571,   572,   573,   576,   577,   581,
    582,   583,   584,   585,   586,   587,   591,   592,   593,   594,
    595,   596,   600,   601,   605,   606,   610,   611,   615,   615,
    619,   623,   624
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
    "$end", "error", "$undefined", "IDENT", "NUMERIC", "ENDQSTMNT",
    "ENDSTMNT", "LE", "GE", "EQ", "DOTTIMES", "DOTRDIV", "DOTLDIV",
    "DOTPOWER", "DOTTRANSPOSE", "STRING", "SPECIALCALL", "END", "IF",
    "FUNCTION", "FOR", "BREAK", "MAGICEND", "WHILE", "ELSE", "ELSEIF",
    "SWITCH", "CASE", "OTHERWISE", "CONTINUE", "TRY", "CATCH", "FIELD",
    "REFLPAREN", "REFRPAREN", "KEYBOARD", "RETURN", "VARARGIN", "VARARGOUT",
    "QUIT", "ABORT", "ENDFUNCTION", "SOR", "SAND", "'|'", "'&'", "':'",
    "'<'", "'>'", "NE", "'+'", "'-'", "'*'", "'/'", "'\\\\'", "POS", "NEG",
    "NOT", "'^'", "'\\''", "'('", "')'", "'='", "'['", "']'", "','", "';'",
    "'{'", "'}'", "'~'", "'.'", "$accept", "program", "functionDef",
    "functionDefList", "returnDeclaration", "argumentList", "argument",
    "statementList", "statement", "statementType", "endfunctionStatement",
    "specialSyntaxStatement", "returnStatement", "pauseStatement",
    "continueStatement", "breakStatement", "tryStatement", "optionalCatch",
    "switchStatement", "optionalEndStatement", "newLine", "caseBlock",
    "caseList", "caseStatement", "otherwiseClause", "forStatement",
    "forIndexExpression", "whileStatement", "ifStatement",
    "conditionedStatement", "elseIfBlock", "elseIfStatementList",
    "elseIfStatement", "elseStatement", "assignmentStatement",
    "multiFunctionCall", "expr", "terminal", "symbRefList", "symbRef",
    "indexElement", "indexList", "cellDef", "matrixDef", "rowSeperator",
    "columnSep", "rowDef", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
    0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
    265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
    275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
    285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
    295,   296,   297,   298,   124,    38,    58,    60,    62,   299,
    43,    45,    42,    47,    92,   300,   301,   302,    94,    39,
    40,    41,    61,    91,    93,    44,    59,   123,   125,   126,
    46
};
# endif

#define YYPACT_NINF -142

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-142)))

#define YYTABLE_NINF -123

#define yytable_value_is_error(Yytable_value) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    346,  -142,   210,  -142,    22,   268,   143,   208,  -142,  -142,
    1153,  2171,  -142,  2073,  -142,  -142,  -142,  -142,  -142,  2171,
    2171,  1179,  1077,  1102,  1204,    26,  -142,    39,   664,  -142,
    158,  -142,   260,  -142,  -142,  -142,  -142,  -142,  -142,  -142,
    -142,  -142,  -142,  -142,  2416,  -142,   214,  -142,  -142,  -142,
    -142,  -142,  -142,  -142,  1077,    48,  2236,   162,  -142,   177,
    62,   151,   292,  -142,    93,   299,   133,  -142,  2261,  2261,
    876,   240,   240,  -142,  2321,  -142,  -142,  -142,  -142,  2416,
    179,  2171,    17,  -142,  -142,   129,  2171,    17,  -142,   240,
    -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,  1230,
    1255,  1281,  1306,  1332,  1357,  1383,  -142,  1408,  1434,  1459,
    1485,  1510,  1536,  1561,  1587,  1612,  1638,  1663,  1689,  1714,
    1740,  -142,  1909,  1765,  2065,    29,  -142,   230,  1791,   146,
    48,  -142,  -142,  -142,  -142,  -142,  -142,  2073,   352,  -142,
    -142,   130,  -142,  2073,  -142,  -142,  -142,   170,   118,    11,
    -142,  -142,     9,  1816,  -142,    42,  1970,  2023,   178,  2073,
    34,  -142,  -142,   154,  2116,   246,  -142,  2171,  -142,   126,
    144,  -142,  2286,  -142,  2286,  -142,  2286,  -142,   240,  -142,
    240,  -142,   240,  -142,   240,  -142,  2469,  -142,  2485,  -142,
    2538,  -142,  2552,  -142,  2565,  -142,  2286,  -142,  2286,  -142,
    2286,  -142,   431,  -142,   431,  -142,   240,  -142,   240,  -142,
    240,  -142,   240,  -142,   356,  -142,  2416,  -142,    58,  -142,
    2416,     6,  -142,  2171,  -142,  -142,  -142,   823,    37,  -142,
    1917,  -142,    55,   196,    27,  -142,  -142,  -142,    91,    20,
    -142,   201,  2073,  -142,  2416,  -142,  1842,  -142,   717,  -142,
    770,  2171,   200,   178,  -142,   929,  -142,  -142,   357,  -142,
    17,  -142,  2140,  2416,  -142,    17,  -142,  2148,  -142,   171,
    -142,  -142,  2065,  -142,  -142,  2396,  -142,   982,  -142,  -142,
    2073,    35,  -142,  -142,  -142,  -142,    55,   217,   399,  -142,
    2341,  -142,  -142,  -142,  -142,  2261,  2073,    38,  -142,  -142,
    28,  -142,  -142,  1867,  -142,  -142,   452,  -142,  2073,  2073,
    337,  -142,  -142,  2073,  1035,  -142,  -142,  -142,   157,  1128,
    -142,  2416,   505,   558,  -142,  2073,  1917,  -142,  -142,    71,
    -142,    15,   611,  -142,  -142,  -142,  -142
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
    0,     5,   197,   181,   182,     0,     0,     0,    67,   183,
    0,     0,    66,    41,    65,    64,    53,    54,    56,     0,
    0,     0,     0,     0,     0,     0,    20,     3,    41,    34,
    0,    55,    52,    51,    50,    45,    44,    49,    48,    43,
    46,    47,    39,    42,    40,   131,   184,    60,    57,    59,
    58,   104,   197,   182,     0,   108,     0,   184,    12,     0,
    0,     0,     0,    98,    92,     0,     0,   101,     0,     0,
    41,   169,   168,   180,     0,   186,   219,   218,   190,   221,
    0,     0,   216,   196,   195,     0,     0,   214,   171,   170,
    1,    21,    35,    36,    37,    38,    62,    63,    61,     0,
    0,     0,     0,     0,     0,     0,   177,     0,     0,     0,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,   176,     0,     0,     0,     0,   198,     0,     0,   115,
    109,   110,   107,    76,    75,    74,    77,    41,    13,    79,
    78,     0,    23,    41,    22,    27,    32,     0,     0,     0,
    30,    16,     0,     0,    96,     0,    41,    41,    80,    41,
    0,   179,   178,   185,     0,     0,   220,     0,   191,     0,
    0,   153,   152,   157,   156,   159,   158,   163,   162,   165,
    164,   167,   166,   175,   174,   147,   146,   149,   148,   143,
    142,   145,   144,   130,   129,   151,   150,   155,   154,   161,
    160,   133,   132,   135,   134,   137,   136,   139,   138,   141,
    140,   173,   172,   207,     0,   200,   206,   212,     0,   118,
    117,     0,   204,     0,   185,   113,   112,     0,     0,   111,
    106,    14,     0,     0,    41,    33,    25,    28,     0,     0,
    17,     0,    41,    97,    91,    95,     0,    87,     0,    99,
    0,     0,    86,    81,    82,    41,    69,    68,     0,   188,
    217,   187,     0,   222,   193,   215,   192,     0,   211,   210,
    201,   199,     0,   203,   202,     0,   116,    41,   105,   103,
    41,     0,    29,    24,    31,    18,     0,     0,    41,    94,
    0,    89,    88,   102,   100,     0,    41,     0,    83,   128,
    0,   189,   194,     0,   213,   205,    41,    15,    41,    41,
    0,    93,    90,    41,    41,    73,    72,   127,     0,     0,
    209,   208,    41,    41,    19,    41,    84,   125,   119,     0,
    126,     0,    41,   124,   120,   123,   121
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -142,  -142,   211,  -142,  -142,  -116,     2,    31,    57,  -142,
    -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,  -142,   -64,
    -141,  -142,  -142,    -6,  -142,  -142,  -142,  -142,  -142,   128,
    -142,  -142,   136,  -142,  -142,  -142,    -2,  -142,     0,  -142,
    -13,  -118,   181,   138,     1,  -142,   -22
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
    -1,    25,    26,    27,    62,   149,   150,    28,    29,    30,
    31,    32,    33,    34,    35,    36,    37,   160,    38,   137,
    143,   252,   253,   254,   297,    39,    66,    40,    41,    55,
    129,   130,   131,   228,    42,    43,    44,    45,    57,   126,
    217,   218,    85,    80,    81,   167,    82
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
    46,    87,   156,    56,   157,   158,   221,   273,    68,    69,
    240,   242,   237,    46,   139,   140,   335,    71,    72,    74,
    79,    79,    89,   146,    86,   233,    90,    -9,    46,   317,
    2,     3,   222,  -122,  -122,   256,   307,    50,   278,   315,
    139,   140,     4,   245,    70,     5,    -9,     7,     8,     9,
    10,   257,    79,    11,   279,   316,    12,    13,     6,   270,
    139,   140,    14,    15,    87,   147,    16,    17,    18,   241,
    46,   272,   333,   128,   274,   238,   239,    19,    20,    79,
    272,   164,   166,   336,    79,    92,   169,    21,   318,   223,
    22,   280,   282,  -122,    23,   319,    24,   172,   174,   176,
    178,   180,   182,   184,   246,   186,   188,   190,   192,   194,
    196,   198,   200,   202,   204,   206,   208,   210,   212,   271,
    216,   220,   216,   272,   144,   287,    56,    92,   164,    52,
    3,   231,   334,   146,    76,    77,   272,    46,   133,   134,
    308,    53,   260,    46,    58,   309,    59,   265,     9,    76,
    77,   244,   145,   283,   146,   153,    46,    46,   327,    46,
    52,     3,    79,    93,    94,   263,   262,    79,   230,   325,
    227,   267,    53,   235,   234,   147,    19,    20,   138,     9,
    236,    60,   139,   140,    76,    77,    21,   248,   250,    54,
    255,   232,   127,    23,   264,    24,   147,   168,   135,   136,
    329,   331,   285,   213,   146,   251,    61,    19,    20,    63,
    214,    64,   266,    47,    48,   148,   258,    21,   328,   165,
    54,   275,   122,    95,    23,    49,    24,    46,   296,   124,
    46,   313,   125,   303,    46,    76,    77,   141,    91,   142,
    260,   284,    46,   163,   290,   265,   147,   298,    46,   295,
    46,    76,    77,   105,   106,    46,   226,   281,   277,   304,
    79,   239,   286,    96,    97,    79,   229,   170,    65,    51,
    216,    52,     3,   288,   122,    98,   123,    46,   310,     0,
    46,   124,   239,    53,   125,     0,     0,    92,    46,     0,
    9,    92,     0,   151,   224,   152,    46,     0,   120,   121,
    154,   321,   155,     0,     0,    92,    46,    92,    46,    46,
    261,   306,    92,    46,    46,     0,   216,   216,    19,    20,
    0,     0,    46,    46,     0,    46,    46,   314,    21,     0,
    0,    54,    46,     0,    92,    23,     0,    24,   324,   322,
    323,     0,   139,   140,   326,    92,    -4,     1,     0,     2,
    3,   -41,   -41,   -26,     0,   -26,   332,   268,   299,   269,
    300,     4,     0,    92,     5,     6,     7,     8,     9,    10,
    0,    92,    11,     0,     0,    12,    13,     0,     0,    92,
    92,    14,    15,    92,     0,    16,    17,    18,     0,    92,
    0,     0,     0,     0,     0,     0,    19,    20,     0,    -8,
    0,     0,     2,     3,     0,     0,    21,     0,     0,    22,
    0,   -41,     0,    23,     4,    24,     0,     5,    -8,     7,
    8,     9,    10,     0,     0,    11,     0,     0,    12,    13,
    0,     0,     0,     0,    14,    15,     0,     0,    16,    17,
    18,   102,   103,   104,   105,   106,     0,     0,     0,    19,
    20,     0,   -11,     0,     0,     2,     3,     0,     0,    21,
    0,     0,    22,     0,     0,     0,    23,     4,    24,     0,
    5,   -11,     7,     8,     9,    10,     0,     0,    11,     0,
    0,    12,    13,   117,   118,   119,     0,    14,    15,   120,
    121,    16,    17,    18,     0,     0,     0,     0,     0,     0,
    0,     0,    19,    20,     0,    -7,     0,     0,     2,     3,
    0,     0,    21,     0,     0,    22,     0,     0,     0,    23,
    4,    24,     0,     5,    -7,     7,     8,     9,    10,     0,
    0,    11,     0,     0,    12,    13,     0,     0,     0,     0,
    14,    15,     0,     0,    16,    17,    18,     0,     0,     0,
    0,     0,     0,     0,     0,    19,    20,     0,   -10,     0,
    0,     2,     3,     0,     0,    21,     0,     0,    22,     0,
    0,     0,    23,     4,    24,     0,     5,   -10,     7,     8,
    9,    10,     0,     0,    11,     0,     0,    12,    13,     0,
    0,     0,     0,    14,    15,     0,     0,    16,    17,    18,
    0,     0,     0,     0,     0,     0,     0,     0,    19,    20,
    0,    -6,     0,     0,     2,     3,     0,     0,    21,     0,
    0,    22,     0,     0,     0,    23,     4,    24,     0,     5,
    -6,     7,     8,     9,    10,     0,     0,    11,     0,     0,
    12,    13,     0,     0,     0,     0,    14,    15,     0,     0,
    16,    17,    18,     0,     0,     0,     0,     0,     0,     0,
    0,    19,    20,     0,    -2,     0,     0,     2,     3,     0,
    0,    21,     0,     0,    22,     0,     0,     0,    23,     4,
    24,     0,     5,     0,     7,     8,     9,    10,     0,     0,
    11,     0,     0,    12,    13,     0,     0,     0,     0,    14,
    15,     0,     0,    16,    17,    18,     0,     0,     0,     0,
    0,     0,     0,     0,    19,    20,     0,     0,   291,     0,
    2,     3,   -41,   -41,    21,     0,     0,    22,     0,     0,
    0,    23,     4,    24,   292,     5,     0,     7,     8,     9,
    10,     0,     0,    11,     0,     0,    12,    13,     0,     0,
    0,     0,    14,    15,     0,     0,    16,    17,    18,     0,
    0,     0,     0,     0,     0,     0,     0,    19,    20,     0,
    0,   293,     0,     2,     3,   -41,   -41,    21,     0,     0,
    22,     0,   -41,     0,    23,     4,    24,   294,     5,     0,
    7,     8,     9,    10,     0,     0,    11,     0,     0,    12,
    13,     0,     0,     0,     0,    14,    15,     0,     0,    16,
    17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
    19,    20,     0,     0,   276,     0,     2,     3,   -41,   -41,
    21,     0,     0,    22,     0,   -41,     0,    23,     4,    24,
    0,     5,     0,     7,     8,     9,    10,     0,     0,    11,
    0,     0,    12,    13,     0,     0,     0,     0,    14,    15,
    0,     0,    16,    17,    18,     0,     0,     0,     0,     0,
    0,     0,     0,    19,    20,     0,     0,   -71,     0,     2,
    3,     0,     0,    21,     0,     0,    22,     0,   -41,     0,
    23,     4,    24,   -71,     5,     0,     7,     8,     9,    10,
    0,     0,    11,     0,     0,    12,    13,   159,     0,     0,
    0,    14,    15,     0,     0,    16,    17,    18,     0,     0,
    0,     0,     0,     0,     0,     0,    19,    20,     0,     0,
    -70,     0,     2,     3,     0,     0,    21,     0,     0,    22,
    0,     0,     0,    23,     4,    24,   -70,     5,     0,     7,
    8,     9,    10,     0,     0,    11,     0,     0,    12,    13,
    0,     0,     0,     0,    14,    15,     0,     0,    16,    17,
    18,     0,     0,     0,     0,     0,     0,     0,     0,    19,
    20,     0,     0,  -114,     0,     2,     3,     0,     0,    21,
    0,     0,    22,     0,     0,     0,    23,     4,    24,  -114,
    5,     0,     7,     8,     9,    10,     0,     0,    11,     0,
    0,    12,    13,     0,     0,     0,     0,    14,    15,     0,
    0,    16,    17,    18,     0,     0,     0,     0,     0,     0,
    0,     0,    19,    20,     0,     0,   -85,     0,     2,     3,
    0,     0,    21,     0,     0,    22,     0,     0,     0,    23,
    4,    24,   -85,     5,     0,     7,     8,     9,    10,     0,
    0,    11,     0,     0,    12,    13,     0,     0,     0,     0,
    14,    15,     0,     0,    16,    17,    18,     0,    75,     0,
    52,     3,    76,    77,     0,    19,    20,     0,     0,     0,
    0,     0,    53,     0,     0,    21,     0,     0,    22,     9,
    0,     0,    23,    83,    24,    52,     3,    76,    77,     0,
    0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
    0,     0,     0,     0,     9,     0,     0,    19,    20,   330,
    0,    52,     3,     0,     0,     0,     0,    21,     0,     0,
    54,    78,     0,    53,    23,     0,    24,     0,     0,     0,
    9,     0,    19,    20,    67,     0,    52,     3,     0,     0,
    0,     0,    21,     0,     0,    54,     0,     0,    53,    23,
    84,    24,     0,     0,   213,     9,     0,     0,    19,    20,
    73,   214,    52,     3,     0,     0,     0,     0,    21,     0,
    0,    54,     0,     0,    53,    23,     0,    24,     0,     0,
    0,     9,     0,    19,    20,    88,     0,    52,     3,     0,
    0,     0,     0,    21,     0,     0,    54,     0,     0,    53,
    23,     0,    24,     0,     0,     0,     9,     0,     0,    19,
    20,   171,     0,    52,     3,     0,     0,     0,     0,    21,
    0,     0,    54,     0,     0,    53,    23,     0,    24,     0,
    0,     0,     9,     0,    19,    20,   173,     0,    52,     3,
    0,     0,     0,     0,    21,     0,     0,    54,     0,     0,
    53,    23,     0,    24,     0,     0,     0,     9,     0,     0,
    19,    20,   175,     0,    52,     3,     0,     0,     0,     0,
    21,     0,     0,    54,     0,     0,    53,    23,     0,    24,
    0,     0,     0,     9,     0,    19,    20,   177,     0,    52,
    3,     0,     0,     0,     0,    21,     0,     0,    54,     0,
    0,    53,    23,     0,    24,     0,     0,     0,     9,     0,
    0,    19,    20,   179,     0,    52,     3,     0,     0,     0,
    0,    21,     0,     0,    54,     0,     0,    53,    23,     0,
    24,     0,     0,     0,     9,     0,    19,    20,   181,     0,
    52,     3,     0,     0,     0,     0,    21,     0,     0,    54,
    0,     0,    53,    23,     0,    24,     0,     0,     0,     9,
    0,     0,    19,    20,   183,     0,    52,     3,     0,     0,
    0,     0,    21,     0,     0,    54,     0,     0,    53,    23,
    0,    24,     0,     0,     0,     9,     0,    19,    20,   185,
    0,    52,     3,     0,     0,     0,     0,    21,     0,     0,
    54,     0,     0,    53,    23,     0,    24,     0,     0,     0,
    9,     0,     0,    19,    20,   187,     0,    52,     3,     0,
    0,     0,     0,    21,     0,     0,    54,     0,     0,    53,
    23,     0,    24,     0,     0,     0,     9,     0,    19,    20,
    189,     0,    52,     3,     0,     0,     0,     0,    21,     0,
    0,    54,     0,     0,    53,    23,     0,    24,     0,     0,
    0,     9,     0,     0,    19,    20,   191,     0,    52,     3,
    0,     0,     0,     0,    21,     0,     0,    54,     0,     0,
    53,    23,     0,    24,     0,     0,     0,     9,     0,    19,
    20,   193,     0,    52,     3,     0,     0,     0,     0,    21,
    0,     0,    54,     0,     0,    53,    23,     0,    24,     0,
    0,     0,     9,     0,     0,    19,    20,   195,     0,    52,
    3,     0,     0,     0,     0,    21,     0,     0,    54,     0,
    0,    53,    23,     0,    24,     0,     0,     0,     9,     0,
    19,    20,   197,     0,    52,     3,     0,     0,     0,     0,
    21,     0,     0,    54,     0,     0,    53,    23,     0,    24,
    0,     0,     0,     9,     0,     0,    19,    20,   199,     0,
    52,     3,     0,     0,     0,     0,    21,     0,     0,    54,
    0,     0,    53,    23,     0,    24,     0,     0,     0,     9,
    0,    19,    20,   201,     0,    52,     3,     0,     0,     0,
    0,    21,     0,     0,    54,     0,     0,    53,    23,     0,
    24,     0,     0,     0,     9,     0,     0,    19,    20,   203,
    0,    52,     3,     0,     0,     0,     0,    21,     0,     0,
    54,     0,     0,    53,    23,     0,    24,     0,     0,     0,
    9,     0,    19,    20,   205,     0,    52,     3,     0,     0,
    0,     0,    21,     0,     0,    54,     0,     0,    53,    23,
    0,    24,     0,     0,     0,     9,     0,     0,    19,    20,
    207,     0,    52,     3,     0,     0,     0,     0,    21,     0,
    0,    54,     0,     0,    53,    23,     0,    24,     0,     0,
    0,     9,     0,    19,    20,   209,     0,    52,     3,     0,
    0,     0,     0,    21,     0,     0,    54,     0,     0,    53,
    23,     0,    24,     0,     0,     0,     9,     0,     0,    19,
    20,   211,     0,    52,     3,     0,     0,     0,     0,    21,
    0,     0,    54,     0,     0,    53,    23,     0,    24,     0,
    0,     0,     9,     0,    19,    20,   219,     0,    52,     3,
    0,     0,     0,     0,    21,     0,     0,    54,     0,     0,
    53,    23,     0,    24,     0,     0,     0,     9,     0,     0,
    19,    20,   225,     0,    52,     3,     0,     0,     0,     0,
    21,     0,     0,    54,     0,     0,    53,    23,     0,    24,
    0,     0,     0,     9,     0,    19,    20,   243,     0,    52,
    3,     0,     0,     0,     0,    21,     0,     0,    54,     0,
    0,    53,    23,     0,    24,     0,     0,     0,     9,     0,
    0,    19,    20,   289,     0,    52,     3,     0,     0,     0,
    0,    21,     0,     0,    54,     0,     0,    53,    23,     0,
    24,     0,     0,     0,     9,     0,    19,    20,   320,     0,
    52,     3,     0,     0,     0,     0,    21,     0,     0,    54,
    0,     0,    53,    23,     0,    24,     0,     0,     0,     9,
    0,     0,    19,    20,     0,     0,     0,     0,     0,     0,
    0,     0,    21,     0,     0,    54,     0,     0,     0,    23,
    0,    24,    52,     3,     0,     0,     0,    19,    20,     0,
    2,     3,   -41,   -41,    53,     0,     0,    21,     0,     0,
    54,     9,     4,     0,    23,     5,    24,     7,     8,     9,
    10,     0,     0,    11,     0,     0,    12,    13,     0,     0,
    0,     0,    14,    15,     0,   213,    16,    17,    18,    19,
    20,     0,   214,     0,     0,     0,     0,    19,    20,    21,
    215,     0,    54,     2,     3,     0,    23,    21,    24,     0,
    22,     0,   -41,     0,    23,     4,    24,   247,     5,     0,
    7,     8,     9,    10,     0,     0,    11,     0,     0,    12,
    13,     0,     0,     0,     0,    14,    15,     0,     0,    16,
    17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
    19,    20,     0,     0,     0,     0,     2,     3,     0,     0,
    21,     0,     0,    22,     0,     0,     0,    23,     4,    24,
    249,     5,     0,     7,     8,     9,    10,     0,     0,    11,
    0,     0,    12,    13,     0,     0,     0,     0,    14,    15,
    0,     0,    16,    17,    18,     0,     0,     0,    52,     3,
    0,     0,     0,    19,    20,     0,     2,     3,     0,     0,
    53,     0,     0,    21,     0,     0,    22,     9,     4,     0,
    23,     5,    24,     7,     8,     9,    10,     0,     0,    11,
    0,     0,    12,    13,     0,     0,     0,     0,    14,    15,
    0,   213,    16,    17,    18,    19,    20,     0,   214,    52,
    3,     0,     0,    19,    20,    21,     0,     0,    54,     0,
    0,    53,    23,    21,    24,     0,    22,     0,     9,     0,
    23,     0,    24,    52,     3,     0,     0,     0,     0,     0,
    0,    52,     3,     0,     0,    53,     0,     0,     0,     0,
    0,     0,     9,    53,     0,     0,    19,    20,     0,     0,
    9,     0,     0,     0,    52,     3,    21,     0,     0,    54,
    259,     0,     0,    23,     0,    24,    53,     0,     0,     0,
    19,    20,     0,     9,     0,     0,     0,     0,    19,    20,
    21,     0,     0,    54,   301,     0,     0,    23,    21,    24,
    0,    54,     0,     0,     0,    23,   302,    24,     0,     0,
    0,    19,    20,     0,     0,     0,     0,     0,     0,     0,
    0,    21,     0,     0,    54,     0,     0,   132,    23,     0,
    24,   133,   134,    99,   100,   101,   102,   103,   104,   105,
    106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,     0,     0,     0,   133,   134,    99,   100,
    101,   102,   103,   104,   105,   106,     0,     0,   107,   108,
    109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    119,     0,     0,     0,   120,   121,   102,   103,   104,   105,
    106,   135,   136,   107,   108,   109,   110,   111,   112,   113,
    114,   115,   116,   117,   118,   119,     0,     0,     0,   120,
    121,     0,   161,     0,     0,     0,   135,   136,    99,   100,
    101,   102,   103,   104,   105,   106,   115,   116,   117,   118,
    119,     0,   311,     0,   120,   121,     0,     0,    99,   100,
    101,   102,   103,   104,   105,   106,     0,     0,     0,     0,
    0,     0,     0,   107,   108,   109,   110,   111,   112,   113,
    114,   115,   116,   117,   118,   119,     0,     0,     0,   120,
    121,     0,   162,   107,   108,   109,   110,   111,   112,   113,
    114,   115,   116,   117,   118,   119,     0,     0,     0,   120,
    121,     0,   312,    99,   100,   101,   102,   103,   104,   105,
    106,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,     0,    99,   100,   101,   102,   103,   104,   105,
    106,     0,     0,     0,     0,     0,     0,     0,   107,   108,
    109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    119,     0,     0,     0,   120,   121,     0,   305,   107,   108,
    109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
    119,     0,     0,     0,   120,   121,    99,   100,   101,   102,
    103,   104,   105,   106,     0,     0,     0,     0,     0,     0,
    0,     0,    99,   100,   101,   102,   103,   104,   105,   106,
    0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    0,     0,   108,   109,   110,   111,   112,   113,   114,   115,
    116,   117,   118,   119,     0,     0,     0,   120,   121,   109,
    110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
    0,     0,     0,   120,   121,    99,   100,   101,   102,   103,
    104,   105,   106,     0,     0,     0,     0,     0,     0,    99,
    100,   101,   102,   103,   104,   105,   106,     0,     0,     0,
    0,     0,    99,   100,   101,   102,   103,   104,   105,   106,
    0,     0,     0,   110,   111,   112,   113,   114,   115,   116,
    117,   118,   119,     0,     0,     0,   120,   121,   111,   112,
    113,   114,   115,   116,   117,   118,   119,     0,     0,     0,
    120,   121,   112,   113,   114,   115,   116,   117,   118,   119,
    0,     0,     0,   120,   121
};

static const yytype_int16 yycheck[] =
{
    0,    23,    66,     5,    68,    69,   124,     1,    10,    11,
    1,   152,     1,    13,     5,     6,     1,    19,    20,    21,
    22,    23,    24,     3,    23,   141,     0,     0,    28,     1,
    3,     4,     3,     5,     6,     1,     1,    15,     1,     1,
    5,     6,    15,     1,    13,    18,    19,    20,    21,    22,
    23,    17,    54,    26,    17,    17,    29,    30,    19,     1,
    5,     6,    35,    36,    86,    45,    39,    40,    41,    60,
    70,    65,     1,    25,    68,    64,    65,    50,    51,    81,
    65,    80,    65,    68,    86,    28,    85,    60,    60,    60,
    63,   232,     1,    65,    67,    67,    69,    99,   100,   101,
    102,   103,   104,   105,    62,   107,   108,   109,   110,   111,
    112,   113,   114,   115,   116,   117,   118,   119,   120,    61,
    122,   123,   124,    65,    62,   241,   128,    70,   127,     3,
    4,     1,    61,     3,     5,     6,    65,   137,     5,     6,
    281,    15,   164,   143,     1,   286,     3,   169,    22,     5,
    6,   153,     1,    62,     3,    62,   156,   157,     1,   159,
    3,     4,   164,     5,     6,   167,   165,   169,   137,   310,
    24,   170,    15,     3,   143,    45,    50,    51,     1,    22,
    62,    38,     5,     6,     5,     6,    60,   156,   157,    63,
    159,    61,    54,    67,    68,    69,    45,    68,    65,    66,
    318,   319,     1,    46,     3,    27,    63,    50,    51,     1,
    53,     3,    68,     3,     4,    64,    62,    60,    61,    81,
    63,   223,    60,    65,    67,    15,    69,   227,    28,    67,
    230,   295,    70,    62,   234,     5,     6,    60,    27,    62,
    262,   239,   242,    64,   246,   267,    45,   253,   248,   251,
    250,     5,     6,    13,    14,   255,   128,    61,   227,   272,
    262,    65,    61,     3,     4,   267,   130,    86,    60,     1,
    272,     3,     4,   242,    60,    15,    62,   277,    61,    -1,
    280,    67,    65,    15,    70,    -1,    -1,   230,   288,    -1,
    22,   234,    -1,     1,    64,     3,   296,    -1,    58,    59,
    1,   303,     3,    -1,    -1,   248,   306,   250,   308,   309,
    64,   280,   255,   313,   314,    -1,   318,   319,    50,    51,
    -1,    -1,   322,   323,    -1,   325,   326,   296,    60,    -1,
    -1,    63,   332,    -1,   277,    67,    -1,    69,     1,   308,
    309,    -1,     5,     6,   313,   288,     0,     1,    -1,     3,
    4,     5,     6,     1,    -1,     3,   325,     1,     1,     3,
    3,    15,    -1,   306,    18,    19,    20,    21,    22,    23,
    -1,   314,    26,    -1,    -1,    29,    30,    -1,    -1,   322,
    323,    35,    36,   326,    -1,    39,    40,    41,    -1,   332,
    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,     0,
    -1,    -1,     3,     4,    -1,    -1,    60,    -1,    -1,    63,
    -1,    65,    -1,    67,    15,    69,    -1,    18,    19,    20,
    21,    22,    23,    -1,    -1,    26,    -1,    -1,    29,    30,
    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,    39,    40,
    41,    10,    11,    12,    13,    14,    -1,    -1,    -1,    50,
    51,    -1,     0,    -1,    -1,     3,     4,    -1,    -1,    60,
    -1,    -1,    63,    -1,    -1,    -1,    67,    15,    69,    -1,
    18,    19,    20,    21,    22,    23,    -1,    -1,    26,    -1,
    -1,    29,    30,    52,    53,    54,    -1,    35,    36,    58,
    59,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    50,    51,    -1,     0,    -1,    -1,     3,     4,
    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,
    15,    69,    -1,    18,    19,    20,    21,    22,    23,    -1,
    -1,    26,    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,
    35,    36,    -1,    -1,    39,    40,    41,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,     0,    -1,
    -1,     3,     4,    -1,    -1,    60,    -1,    -1,    63,    -1,
    -1,    -1,    67,    15,    69,    -1,    18,    19,    20,    21,
    22,    23,    -1,    -1,    26,    -1,    -1,    29,    30,    -1,
    -1,    -1,    -1,    35,    36,    -1,    -1,    39,    40,    41,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
    -1,     0,    -1,    -1,     3,     4,    -1,    -1,    60,    -1,
    -1,    63,    -1,    -1,    -1,    67,    15,    69,    -1,    18,
    19,    20,    21,    22,    23,    -1,    -1,    26,    -1,    -1,
    29,    30,    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,
    39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    50,    51,    -1,     0,    -1,    -1,     3,     4,    -1,
    -1,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,    15,
    69,    -1,    18,    -1,    20,    21,    22,    23,    -1,    -1,
    26,    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,    35,
    36,    -1,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,     1,    -1,
    3,     4,     5,     6,    60,    -1,    -1,    63,    -1,    -1,
    -1,    67,    15,    69,    17,    18,    -1,    20,    21,    22,
    23,    -1,    -1,    26,    -1,    -1,    29,    30,    -1,    -1,
    -1,    -1,    35,    36,    -1,    -1,    39,    40,    41,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
    -1,     1,    -1,     3,     4,     5,     6,    60,    -1,    -1,
    63,    -1,    65,    -1,    67,    15,    69,    17,    18,    -1,
    20,    21,    22,    23,    -1,    -1,    26,    -1,    -1,    29,
    30,    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,    39,
    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    50,    51,    -1,    -1,     1,    -1,     3,     4,     5,     6,
    60,    -1,    -1,    63,    -1,    65,    -1,    67,    15,    69,
    -1,    18,    -1,    20,    21,    22,    23,    -1,    -1,    26,
    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,    35,    36,
    -1,    -1,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    50,    51,    -1,    -1,     1,    -1,     3,
    4,    -1,    -1,    60,    -1,    -1,    63,    -1,    65,    -1,
    67,    15,    69,    17,    18,    -1,    20,    21,    22,    23,
    -1,    -1,    26,    -1,    -1,    29,    30,    31,    -1,    -1,
    -1,    35,    36,    -1,    -1,    39,    40,    41,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
    1,    -1,     3,     4,    -1,    -1,    60,    -1,    -1,    63,
    -1,    -1,    -1,    67,    15,    69,    17,    18,    -1,    20,
    21,    22,    23,    -1,    -1,    26,    -1,    -1,    29,    30,
    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,    39,    40,
    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
    51,    -1,    -1,     1,    -1,     3,     4,    -1,    -1,    60,
    -1,    -1,    63,    -1,    -1,    -1,    67,    15,    69,    17,
    18,    -1,    20,    21,    22,    23,    -1,    -1,    26,    -1,
    -1,    29,    30,    -1,    -1,    -1,    -1,    35,    36,    -1,
    -1,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    50,    51,    -1,    -1,     1,    -1,     3,     4,
    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,
    15,    69,    17,    18,    -1,    20,    21,    22,    23,    -1,
    -1,    26,    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,
    35,    36,    -1,    -1,    39,    40,    41,    -1,     1,    -1,
    3,     4,     5,     6,    -1,    50,    51,    -1,    -1,    -1,
    -1,    -1,    15,    -1,    -1,    60,    -1,    -1,    63,    22,
    -1,    -1,    67,     1,    69,     3,     4,     5,     6,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    15,    -1,    -1,
    -1,    -1,    -1,    -1,    22,    -1,    -1,    50,    51,     1,
    -1,     3,     4,    -1,    -1,    -1,    -1,    60,    -1,    -1,
    63,    64,    -1,    15,    67,    -1,    69,    -1,    -1,    -1,
    22,    -1,    50,    51,     1,    -1,     3,     4,    -1,    -1,
    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    15,    67,
    68,    69,    -1,    -1,    46,    22,    -1,    -1,    50,    51,
    1,    53,     3,     4,    -1,    -1,    -1,    -1,    60,    -1,
    -1,    63,    -1,    -1,    15,    67,    -1,    69,    -1,    -1,
    -1,    22,    -1,    50,    51,     1,    -1,     3,     4,    -1,
    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    15,
    67,    -1,    69,    -1,    -1,    -1,    22,    -1,    -1,    50,
    51,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    60,
    -1,    -1,    63,    -1,    -1,    15,    67,    -1,    69,    -1,
    -1,    -1,    22,    -1,    50,    51,     1,    -1,     3,     4,
    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,
    15,    67,    -1,    69,    -1,    -1,    -1,    22,    -1,    -1,
    50,    51,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
    60,    -1,    -1,    63,    -1,    -1,    15,    67,    -1,    69,
    -1,    -1,    -1,    22,    -1,    50,    51,     1,    -1,     3,
    4,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
    -1,    15,    67,    -1,    69,    -1,    -1,    -1,    22,    -1,
    -1,    50,    51,     1,    -1,     3,     4,    -1,    -1,    -1,
    -1,    60,    -1,    -1,    63,    -1,    -1,    15,    67,    -1,
    69,    -1,    -1,    -1,    22,    -1,    50,    51,     1,    -1,
    3,     4,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
    -1,    -1,    15,    67,    -1,    69,    -1,    -1,    -1,    22,
    -1,    -1,    50,    51,     1,    -1,     3,     4,    -1,    -1,
    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    15,    67,
    -1,    69,    -1,    -1,    -1,    22,    -1,    50,    51,     1,
    -1,     3,     4,    -1,    -1,    -1,    -1,    60,    -1,    -1,
    63,    -1,    -1,    15,    67,    -1,    69,    -1,    -1,    -1,
    22,    -1,    -1,    50,    51,     1,    -1,     3,     4,    -1,
    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    15,
    67,    -1,    69,    -1,    -1,    -1,    22,    -1,    50,    51,
    1,    -1,     3,     4,    -1,    -1,    -1,    -1,    60,    -1,
    -1,    63,    -1,    -1,    15,    67,    -1,    69,    -1,    -1,
    -1,    22,    -1,    -1,    50,    51,     1,    -1,     3,     4,
    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,
    15,    67,    -1,    69,    -1,    -1,    -1,    22,    -1,    50,
    51,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    60,
    -1,    -1,    63,    -1,    -1,    15,    67,    -1,    69,    -1,
    -1,    -1,    22,    -1,    -1,    50,    51,     1,    -1,     3,
    4,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
    -1,    15,    67,    -1,    69,    -1,    -1,    -1,    22,    -1,
    50,    51,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
    60,    -1,    -1,    63,    -1,    -1,    15,    67,    -1,    69,
    -1,    -1,    -1,    22,    -1,    -1,    50,    51,     1,    -1,
    3,     4,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
    -1,    -1,    15,    67,    -1,    69,    -1,    -1,    -1,    22,
    -1,    50,    51,     1,    -1,     3,     4,    -1,    -1,    -1,
    -1,    60,    -1,    -1,    63,    -1,    -1,    15,    67,    -1,
    69,    -1,    -1,    -1,    22,    -1,    -1,    50,    51,     1,
    -1,     3,     4,    -1,    -1,    -1,    -1,    60,    -1,    -1,
    63,    -1,    -1,    15,    67,    -1,    69,    -1,    -1,    -1,
    22,    -1,    50,    51,     1,    -1,     3,     4,    -1,    -1,
    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    15,    67,
    -1,    69,    -1,    -1,    -1,    22,    -1,    -1,    50,    51,
    1,    -1,     3,     4,    -1,    -1,    -1,    -1,    60,    -1,
    -1,    63,    -1,    -1,    15,    67,    -1,    69,    -1,    -1,
    -1,    22,    -1,    50,    51,     1,    -1,     3,     4,    -1,
    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    15,
    67,    -1,    69,    -1,    -1,    -1,    22,    -1,    -1,    50,
    51,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    60,
    -1,    -1,    63,    -1,    -1,    15,    67,    -1,    69,    -1,
    -1,    -1,    22,    -1,    50,    51,     1,    -1,     3,     4,
    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,
    15,    67,    -1,    69,    -1,    -1,    -1,    22,    -1,    -1,
    50,    51,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
    60,    -1,    -1,    63,    -1,    -1,    15,    67,    -1,    69,
    -1,    -1,    -1,    22,    -1,    50,    51,     1,    -1,     3,
    4,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
    -1,    15,    67,    -1,    69,    -1,    -1,    -1,    22,    -1,
    -1,    50,    51,     1,    -1,     3,     4,    -1,    -1,    -1,
    -1,    60,    -1,    -1,    63,    -1,    -1,    15,    67,    -1,
    69,    -1,    -1,    -1,    22,    -1,    50,    51,     1,    -1,
    3,     4,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
    -1,    -1,    15,    67,    -1,    69,    -1,    -1,    -1,    22,
    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    -1,    67,
    -1,    69,     3,     4,    -1,    -1,    -1,    50,    51,    -1,
    3,     4,     5,     6,    15,    -1,    -1,    60,    -1,    -1,
    63,    22,    15,    -1,    67,    18,    69,    20,    21,    22,
    23,    -1,    -1,    26,    -1,    -1,    29,    30,    -1,    -1,
    -1,    -1,    35,    36,    -1,    46,    39,    40,    41,    50,
    51,    -1,    53,    -1,    -1,    -1,    -1,    50,    51,    60,
    61,    -1,    63,     3,     4,    -1,    67,    60,    69,    -1,
    63,    -1,    65,    -1,    67,    15,    69,    17,    18,    -1,
    20,    21,    22,    23,    -1,    -1,    26,    -1,    -1,    29,
    30,    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,    39,
    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    50,    51,    -1,    -1,    -1,    -1,     3,     4,    -1,    -1,
    60,    -1,    -1,    63,    -1,    -1,    -1,    67,    15,    69,
    17,    18,    -1,    20,    21,    22,    23,    -1,    -1,    26,
    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,    35,    36,
    -1,    -1,    39,    40,    41,    -1,    -1,    -1,     3,     4,
    -1,    -1,    -1,    50,    51,    -1,     3,     4,    -1,    -1,
    15,    -1,    -1,    60,    -1,    -1,    63,    22,    15,    -1,
    67,    18,    69,    20,    21,    22,    23,    -1,    -1,    26,
    -1,    -1,    29,    30,    -1,    -1,    -1,    -1,    35,    36,
    -1,    46,    39,    40,    41,    50,    51,    -1,    53,     3,
    4,    -1,    -1,    50,    51,    60,    -1,    -1,    63,    -1,
    -1,    15,    67,    60,    69,    -1,    63,    -1,    22,    -1,
    67,    -1,    69,     3,     4,    -1,    -1,    -1,    -1,    -1,
    -1,     3,     4,    -1,    -1,    15,    -1,    -1,    -1,    -1,
    -1,    -1,    22,    15,    -1,    -1,    50,    51,    -1,    -1,
    22,    -1,    -1,    -1,     3,     4,    60,    -1,    -1,    63,
    64,    -1,    -1,    67,    -1,    69,    15,    -1,    -1,    -1,
    50,    51,    -1,    22,    -1,    -1,    -1,    -1,    50,    51,
    60,    -1,    -1,    63,    64,    -1,    -1,    67,    60,    69,
    -1,    63,    -1,    -1,    -1,    67,    68,    69,    -1,    -1,
    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    60,    -1,    -1,    63,    -1,    -1,     1,    67,    -1,
    69,     5,     6,     7,     8,     9,    10,    11,    12,    13,
    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,     5,     6,     7,     8,
    9,    10,    11,    12,    13,    14,    -1,    -1,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
    54,    -1,    -1,    -1,    58,    59,    10,    11,    12,    13,
    14,    65,    66,    42,    43,    44,    45,    46,    47,    48,
    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    58,
    59,    -1,     1,    -1,    -1,    -1,    65,    66,     7,     8,
    9,    10,    11,    12,    13,    14,    50,    51,    52,    53,
    54,    -1,     1,    -1,    58,    59,    -1,    -1,     7,     8,
    9,    10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    58,
    59,    -1,    61,    42,    43,    44,    45,    46,    47,    48,
    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    58,
    59,    -1,    61,     7,     8,     9,    10,    11,    12,    13,
    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,     7,     8,     9,    10,    11,    12,    13,
    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
    54,    -1,    -1,    -1,    58,    59,    -1,    61,    42,    43,
    44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
    54,    -1,    -1,    -1,    58,    59,     7,     8,     9,    10,
    11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,     7,     8,     9,    10,    11,    12,    13,    14,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    43,    44,    45,    46,    47,    48,    49,    50,
    51,    52,    53,    54,    -1,    -1,    -1,    58,    59,    44,
    45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
    -1,    -1,    -1,    58,    59,     7,     8,     9,    10,    11,
    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,     7,
    8,     9,    10,    11,    12,    13,    14,    -1,    -1,    -1,
    -1,    -1,     7,     8,     9,    10,    11,    12,    13,    14,
    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
    52,    53,    54,    -1,    -1,    -1,    58,    59,    46,    47,
    48,    49,    50,    51,    52,    53,    54,    -1,    -1,    -1,
    58,    59,    47,    48,    49,    50,    51,    52,    53,    54,
    -1,    -1,    -1,    58,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
    0,     1,     3,     4,    15,    18,    19,    20,    21,    22,
    23,    26,    29,    30,    35,    36,    39,    40,    41,    50,
    51,    60,    63,    67,    69,    72,    73,    74,    78,    79,
    80,    81,    82,    83,    84,    85,    86,    87,    89,    96,
    98,    99,   105,   106,   107,   108,   109,     3,     4,    15,
    15,     1,     3,    15,    63,   100,   107,   109,     1,     3,
    38,    63,    75,     1,     3,    60,    97,     1,   107,   107,
    78,   107,   107,     1,   107,     1,     5,     6,    64,   107,
    114,   115,   117,     1,    68,   113,   115,   117,     1,   107,
    0,    73,    79,     5,     6,    65,     3,     4,    15,     7,
    8,     9,    10,    11,    12,    13,    14,    42,    43,    44,
    45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
    58,    59,    60,    62,    67,    70,   110,   114,    25,   101,
    102,   103,     1,     5,     6,    65,    66,    90,     1,     5,
    6,    60,    62,    91,    62,     1,     3,    45,    64,    76,
    77,     1,     3,    62,     1,     3,    90,    90,    90,    31,
    88,     1,    61,    64,   115,   114,    65,   116,    68,   115,
    113,     1,   107,     1,   107,     1,   107,     1,   107,     1,
    107,     1,   107,     1,   107,     1,   107,     1,   107,     1,
    107,     1,   107,     1,   107,     1,   107,     1,   107,     1,
    107,     1,   107,     1,   107,     1,   107,     1,   107,     1,
    107,     1,   107,    46,    53,    61,   107,   111,   112,     1,
    107,   112,     3,    60,    64,     1,   100,    24,   104,   103,
    78,     1,    61,    76,    78,     3,    62,     1,    64,    65,
    1,    60,    91,     1,   107,     1,    62,    17,    78,    17,
    78,    27,    92,    93,    94,    78,     1,    17,    62,    64,
    117,    64,   115,   107,    68,   117,    68,   115,     1,     3,
    1,    61,    65,     1,    68,   107,     1,    78,     1,    17,
    91,    61,     1,    62,    77,     1,    61,    76,    78,     1,
    107,     1,    17,     1,    17,   107,    28,    95,    94,     1,
    3,    64,    68,    62,   111,    61,    78,     1,    91,    91,
    61,     1,    61,    90,    78,     1,    17,     1,    60,    67,
    1,   107,    78,    78,     1,    91,    78,     1,    61,   112,
    1,   112,    78,     1,    61,     1,    68
};

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
    0,    71,    72,    72,    72,    72,    73,    73,    73,    73,
    73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
    74,    74,    75,    75,    75,    75,    75,    75,    75,    75,
    76,    76,    77,    77,    78,    78,    79,    79,    79,    80,
    80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
    80,    80,    80,    80,    80,    80,    81,    82,    82,    82,
    82,    82,    82,    82,    83,    84,    85,    86,    87,    87,
    88,    88,    89,    89,    90,    90,    90,    90,    91,    91,
    92,    92,    93,    93,    94,    95,    95,    96,    96,    96,
    97,    97,    97,    97,    97,    97,    97,    97,    97,    98,
    98,    98,    98,    99,    99,    99,   100,   100,   101,   101,
    102,   102,   103,   103,   104,   104,   104,   105,   105,   106,
    106,   106,   106,   106,   106,   106,   106,   106,   106,   107,
    107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
    107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
    107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
    107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
    107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
    107,   108,   108,   108,   108,   108,   108,   108,   108,   108,
    108,   108,   108,   108,   108,   108,   108,   109,   109,   110,
    110,   110,   110,   110,   110,   110,   111,   111,   111,   111,
    111,   111,   112,   112,   113,   113,   114,   114,   115,   115,
    116,   117,   117
};

/* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
    0,     2,     1,     1,     0,     1,     8,     7,     5,     4,
    7,     6,     2,     3,     4,     6,     3,     4,     5,     7,
    1,     2,     2,     2,     4,     3,     2,     2,     3,     4,
    1,     3,     1,     2,     1,     2,     2,     2,     2,     1,
    1,     0,     1,     1,     1,     1,     1,     1,     1,     1,
    1,     1,     1,     1,     1,     1,     1,     2,     2,     2,
    2,     2,     2,     2,     1,     1,     1,     1,     4,     4,
    2,     0,     6,     6,     1,     1,     1,     1,     1,     1,
    0,     1,     1,     2,     4,     2,     0,     4,     5,     5,
    5,     3,     1,     5,     4,     3,     2,     3,     1,     4,
    5,     2,     5,     5,     2,     5,     3,     2,     0,     1,
    1,     2,     2,     2,     2,     0,     2,     3,     3,     7,
    8,     8,     5,     8,     8,     7,     7,     6,     5,     3,
    3,     1,     3,     3,     3,     3,     3,     3,     3,     3,
    3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
    3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
    3,     3,     3,     3,     3,     3,     3,     3,     2,     2,
    2,     2,     3,     3,     3,     3,     2,     2,     3,     3,
    2,     1,     1,     1,     1,     3,     2,     4,     4,     5,
    2,     3,     4,     4,     5,     2,     2,     1,     2,     3,
    2,     3,     3,     3,     2,     4,     1,     1,     4,     4,
    2,     2,     1,     3,     1,     3,     1,     3,     1,     1,
    1,     1,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
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

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
    FILE *yyo = yyoutput;
    YYUSE (yyo);
    if (!yyvaluep)
    {
        return;
    }
# ifdef YYPRINT
    if (yytype < YYNTOKENS)
    {
        YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
    }
# endif
    YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
    YYFPRINTF (yyoutput, "%s %s (",
               yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);
    yy_symbol_value_print (yyoutput, yytype, yyvaluep);
    YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
    unsigned long int yylno = yyrline[yyrule];
    int yynrhs = yyr2[yyrule];
    int yyi;
    YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
               yyrule - 1, yylno);
    /* The symbols being reduced.  */
    for (yyi = 0; yyi < yynrhs; yyi++)
    {
        YYFPRINTF (stderr, "   $%d = ", yyi + 1);
        yy_symbol_print (stderr,
                         yystos[yyssp[yyi + 1 - yynrhs]],
                         &(yyvsp[(yyi + 1) - (yynrhs)])
                        );
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
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
    YYSIZE_T yylen;
    for (yylen = 0; yystr[yylen]; yylen++)
    {
        continue;
    }
    return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
    char *yyd = yydest;
    const char *yys = yysrc;
    while ((*yyd++ = *yys++) != '\0')
    {
        continue;
    }
    return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
    if (*yystr == '"')
    {
        YYSIZE_T yyn = 0;
        char const *yyp = yystr;
        for (;;)
            switch (*++yyp)
            {
                case '\'':
                case ',':
                    goto do_not_strip_quotes;
                case '\\':
                    if (*++yyp != '\\')
                    {
                        goto do_not_strip_quotes;
                    }
                /* Fall through.  */
                default:
                    if (yyres)
                    {
                        yyres[yyn] = *yyp;
                    }
                    yyn++;
                    break;
                case '"':
                    if (yyres)
                    {
                        yyres[yyn] = '\0';
                    }
                    return yyn;
            }
do_not_strip_quotes:
        ;
    }
    if (! yyres)
    {
        return yystrlen (yystr);
    }
    return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
    YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
    YYSIZE_T yysize = yysize0;
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    /* Internationalized format string. */
    const char *yyformat = YY_NULL;
    /* Arguments of yyformat. */
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
    /* Number of reported tokens (one for the "unexpected", one per
       "expected"). */
    int yycount = 0;
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yychar) is if
         this state is a consistent state with a default action.  Thus,
         detecting the absence of a lookahead is sufficient to determine
         that there is no unexpected or expected token to report.  In that
         case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is a
         consistent state with a default action.  There might have been a
         previous inconsistent state, consistent state with a non-default
         action, or user semantic action that manipulated yychar.
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */
    if (yytoken != YYEMPTY)
    {
        int yyn = yypact[*yyssp];
        yyarg[yycount++] = yytname[yytoken];
        if (!yypact_value_is_default (yyn))
        {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            /* Stay within bounds of both yycheck and yytname.  */
            int yychecklim = YYLAST - yyn + 1;
            int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
            int yyx;
            for (yyx = yyxbegin; yyx < yyxend; ++yyx)
                if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                        && !yytable_value_is_error (yytable[yyx + yyn]))
                {
                    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                        yycount = 1;
                        yysize = yysize0;
                        break;
                    }
                    yyarg[yycount++] = yytname[yyx];
                    {
                        YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                        if (! (yysize <= yysize1
                                && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                        {
                            return 2;
                        }
                        yysize = yysize1;
                    }
                }
        }
    }
    switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
            YYCASE_(0, YY_("syntax error"));
            YYCASE_(1, YY_("syntax error, unexpected %s"));
            YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
            YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
            YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
            YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }
    {
        YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
        if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
        {
            return 2;
        }
        yysize = yysize1;
    }
    if (*yymsg_alloc < yysize)
    {
        *yymsg_alloc = 2 * yysize;
        if (! (yysize <= *yymsg_alloc
                && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        {
            *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
        }
        return 1;
    }
    /* Avoid sprintf, as that infringes on the user's name space.
       Don't have undefined behavior even if the translation
       produced a string with the wrong number of "%s"s.  */
    {
        char *yyp = *yymsg;
        int yyi = 0;
        while ((*yyp = *yyformat) != '\0')
            if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
            {
                yyp += yytnamerr (yyp, yyarg[yyi++]);
                yyformat += 2;
            }
            else
            {
                yyp++;
                yyformat++;
            }
    }
    return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
    YYUSE (yyvaluep);
    if (!yymsg)
    {
        yymsg = "Deleting";
    }
    YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);
    YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
    YYUSE (yytype);
    YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
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
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;
    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */
    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;
    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;
    YYSIZE_T yystacksize;
    int yyn;
    int yyresult;
    /* Lookahead token as an internal (translated) token number.  */
    int yytoken = 0;
    /* The variables used to return semantic value and location from the
       action routines.  */
    YYSTYPE yyval;
#if YYERROR_VERBOSE
    /* Buffer for error messages, and its allocated size.  */
    char yymsgbuf[128];
    char *yymsg = yymsgbuf;
    YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif
#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))
    /* The number of symbols on the RHS of the reduced rule.
       Keep to zero when no symbol should be popped.  */
    int yylen = 0;
    yyssp = yyss = yyssa;
    yyvsp = yyvs = yyvsa;
    yystacksize = YYINITDEPTH;
    YYDPRINTF ((stderr, "Starting parse\n"));
    yystate = 0;
    yyerrstatus = 0;
    yynerrs = 0;
    yychar = YYEMPTY; /* Cause a token to be read.  */
    goto yysetstate;
    /*------------------------------------------------------------.
    | yynewstate -- Push a new state, which is found in yystate.  |
    `------------------------------------------------------------*/
yynewstate:
    /* In all cases, when you get here, the value and location stacks
       have just been pushed.  So pushing a state here evens the stacks.  */
    yyssp++;
yysetstate:
    *yyssp = yystate;
    if (yyss + yystacksize - 1 <= yyssp)
    {
        /* Get the current used size of the three stacks, in elements.  */
        YYSIZE_T yysize = yyssp - yyss + 1;
#ifdef yyoverflow
        {
            /* Give user a chance to reallocate the stack.  Use copies of
               these so that the &'s don't force the real ones into
               memory.  */
            YYSTYPE *yyvs1 = yyvs;
            yytype_int16 *yyss1 = yyss;
            /* Each stack pointer address is followed by the size of the
               data in use in that stack, in bytes.  This used to be a
               conditional around just the two extra args, but that might
               be undefined if yyoverflow is a macro.  */
            yyoverflow (YY_("memory exhausted"),
                        &yyss1, yysize * sizeof (*yyssp),
                        &yyvs1, yysize * sizeof (*yyvsp),
                        &yystacksize);
            yyss = yyss1;
            yyvs = yyvs1;
        }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
        goto yyexhaustedlab;
# else
        /* Extend the stack our own way.  */
        if (YYMAXDEPTH <= yystacksize)
        {
            goto yyexhaustedlab;
        }
        yystacksize *= 2;
        if (YYMAXDEPTH < yystacksize)
        {
            yystacksize = YYMAXDEPTH;
        }
        {
            yytype_int16 *yyss1 = yyss;
            union yyalloc *yyptr =
                        (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
            if (! yyptr)
        {
                goto yyexhaustedlab;
            }
            YYSTACK_RELOCATE (yyss_alloc, yyss);
            YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
            if (yyss1 != yyssa)
            {
                YYSTACK_FREE (yyss1);
            }
        }
# endif
#endif /* no yyoverflow */
        yyssp = yyss + yysize - 1;
        yyvsp = yyvs + yysize - 1;
        YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                    (unsigned long int) yystacksize));
        if (yyss + yystacksize - 1 <= yyssp)
        {
            YYABORT;
        }
    }
    YYDPRINTF ((stderr, "Entering state %d\n", yystate));
    if (yystate == YYFINAL)
    {
        YYACCEPT;
    }
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
    {
        goto yydefault;
    }
    /* Not known => get a lookahead token if don't already have one.  */
    /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
    if (yychar == YYEMPTY)
    {
        YYDPRINTF ((stderr, "Reading a token: "));
        yychar = yylex ();
    }
    if (yychar <= YYEOF)
    {
        yychar = yytoken = YYEOF;
        YYDPRINTF ((stderr, "Now at end of input.\n"));
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
    {
        goto yydefault;
    }
    yyn = yytable[yyn];
    if (yyn <= 0)
    {
        if (yytable_value_is_error (yyn))
        {
            goto yyerrlab;
        }
        yyn = -yyn;
        goto yyreduce;
    }
    /* Count tokens shifted since error; after three, turn off error
       status.  */
    if (yyerrstatus)
    {
        yyerrstatus--;
    }
    /* Shift the lookahead token.  */
    YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
    /* Discard the shifted token.  */
    yychar = YYEMPTY;
    yystate = yyn;
    YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
    *++yyvsp = yylval;
    YY_IGNORE_MAYBE_UNINITIALIZED_END
    goto yynewstate;
    /*-----------------------------------------------------------.
    | yydefault -- do the default action for the current state.  |
    `-----------------------------------------------------------*/
yydefault:
    yyn = yydefact[yystate];
    if (yyn == 0)
    {
        goto yyerrlab;
    }
    goto yyreduce;
    /*-----------------------------.
    | yyreduce -- Do a reduction.  |
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
        case 2:
#line 148 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                mainAST = (yyvsp[0]).v.p;
            }
#line 2053 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 5:
#line 150 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt("statement list or function definition",(yyvsp[0]));
            }
#line 2059 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 6:
#line 154 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                MacroFunctionDef *r = new MacroFunctionDef();
                if ((yyvsp[-6]).v.p)
                {
                    r->returnVals = (yyvsp[-6]).v.p->toStringList();
                }
                r->name = (yyvsp[-5]).v.p->text;
                r->arguments = (yyvsp[-3]).v.p->toStringList();
                r->code = (yyvsp[0]).v.p;
                r->fileName = getParserFilenameW();
                chainFunction(r);
            }
#line 2076 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 7:
#line 166 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                MacroFunctionDef *r = new MacroFunctionDef();
                r->name = (yyvsp[-5]).v.p->text;
                r->arguments = (yyvsp[-3]).v.p->toStringList();
                r->code = (yyvsp[0]).v.p;
                r->fileName = getParserFilenameW();
                chainFunction(r);
            }
#line 2089 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 8:
#line 174 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                MacroFunctionDef *r = new MacroFunctionDef();
                if ((yyvsp[-3]).v.p)
                {
                    r->returnVals = (yyvsp[-3]).v.p->toStringList();
                }
                r->name = (yyvsp[-2]).v.p->text;
                r->code = (yyvsp[0]).v.p;
                r->fileName = getParserFilenameW();
                chainFunction(r);
            }
#line 2105 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 9:
#line 185 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                MacroFunctionDef *r = new MacroFunctionDef();
                r->name = (yyvsp[-2]).v.p->text;
                r->code = (yyvsp[0]).v.p;
                r->fileName = getParserFilenameW();
                chainFunction(r);
            }
#line 2117 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 10:
#line 192 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                MacroFunctionDef *r = new MacroFunctionDef();
                if ((yyvsp[-5]).v.p)
                {
                    r->returnVals = (yyvsp[-5]).v.p->toStringList();
                }
                r->name = (yyvsp[-4]).v.p->text;
                r->code = (yyvsp[0]).v.p;
                r->fileName = getParserFilenameW();
                chainFunction(r);
            }
#line 2133 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 11:
#line 203 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                MacroFunctionDef *r = new MacroFunctionDef();
                r->name = (yyvsp[-4]).v.p->text;
                r->code = (yyvsp[0]).v.p;
                r->fileName = getParserFilenameW();
                chainFunction(r);
            }
#line 2145 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 12:
#line 210 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("legal function name or return declaration after 'function'"), (yyvsp[-1]));
            }
#line 2151 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 13:
#line 211 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("argument list or statement list after identifier '") +
                      (yyvsp[-1]).v.p->text.c_str() + "'",(yyvsp[-1]));
            }
#line 2158 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 14:
#line 213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("(possibly empty) argument list after '('"),(yyvsp[-1]));
            }
#line 2164 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 15:
#line 214 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("statement list after ')'"),(yyvsp[-1]));
            }
#line 2170 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 16:
#line 215 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("function name for function declared"),(yyvsp[-2]));
            }
#line 2176 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 17:
#line 216 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("argument list or statement list following function name :") +
                      (yyvsp[-1]).v.p->text.c_str(), (yyvsp[-1]));
            }
#line 2183 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 18:
#line 218 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("(possibly empty) argument list after '('"),(yyvsp[-1]));
            }
#line 2189 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 19:
#line 219 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("statement list after ')'"),(yyvsp[-1]));
            }
#line 2195 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 22:
#line 228 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 2201 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 23:
#line 229 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 2207 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 24:
#line 230 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
            }
#line 2213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 25:
#line 231 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2219 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 26:
#line 232 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an '=' symbol after identifier in return declaration"),(yyvsp[-1]));
            }
#line 2225 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 27:
#line 233 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("a valid list of return arguments in return declaration"),(yyvsp[-1]));
            }
#line 2231 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 28:
#line 234 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching ']' in return declaration for '['"),(yyvsp[-2]));
            }
#line 2237 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 29:
#line 235 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an '=' symbol after return declaration"),(yyvsp[-1]));
            }
#line 2243 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 30:
#line 239 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
            }
#line 2249 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 31:
#line 240 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2255 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 33:
#line 244 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
                char *b = (char*) malloc((yyvsp[0]).v.p->text.size() + 2);
                b[0] = '&';
                strcpy(b+1, (yyvsp[0]).v.p->text.c_str());
                (yyval).v.p->text = b;
                free(b);
            }
#line 2268 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 34:
#line 256 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_BLOCK,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 2274 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 35:
#line 257 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2280 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 36:
#line 261 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_QSTATEMENT,NULL,(yyvsp[0]).v.i);
                (yyval).v.p->down = (yyvsp[-1]).v.p;
            }
#line 2289 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 37:
#line 265 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_RSTATEMENT,NULL,(yyvsp[0]).v.i);
                (yyval).v.p->down = (yyvsp[-1]).v.p;
            }
#line 2298 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 38:
#line 269 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_RSTATEMENT,NULL,(yyvsp[0]).v.i);
                (yyval).v.p->down = (yyvsp[-1]).v.p;
            }
#line 2307 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 41:
#line 278 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(null_node,"",-1);
            }
#line 2313 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 57:
#line 299 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SCALL,(yyvsp[-1]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.p->context());
            }
#line 2319 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 58:
#line 300 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SCALL,(yyvsp[-1]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.p->context());
            }
#line 2325 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 59:
#line 301 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SCALL,(yyvsp[-1]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.p->context());
            }
#line 2331 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 60:
#line 302 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SCALL,(yyvsp[-1]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.p->context());
            }
#line 2337 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 61:
#line 303 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyvsp[-1]).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2343 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 62:
#line 304 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyvsp[-1]).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2349 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 63:
#line 305 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyvsp[-1]).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2355 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 68:
#line 324 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-3]).v.p;
                (yyval).v.p->addChild((yyvsp[-2]).v.p);
                if ((yyvsp[-1]).v.p != nullptr)
                {
                    (yyval).v.p->addChild((yyvsp[-1]).v.p);
                }
            }
#line 2365 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 69:
#line 330 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching 'end' to 'try' clause from line ") + decodeline((yyvsp[-3])),(yyvsp[0]));
            }
#line 2371 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 70:
#line 334 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
            }
#line 2377 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 71:
#line 335 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2383 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 72:
#line 339 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-5]).v.p;
                (yyval).v.p->addChild((yyvsp[-4]).v.p);
                if ((yyvsp[-2]).v.p != nullptr)
                {
                    (yyval).v.p->addChild((yyvsp[-2]).v.p);
                }
                if ((yyvsp[-1]).v.p != nullptr)
                {
                    (yyval).v.p->addChild((yyvsp[-1]).v.p);
                }
            }
#line 2394 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 73:
#line 345 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching 'end' to 'switch' clause from line ") + decodeline((yyvsp[-5])),(yyvsp[0]));
            }
#line 2402 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 80:
#line 360 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2408 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 82:
#line 364 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_CASEBLOCK,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 2416 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 83:
#line 367 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2424 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 84:
#line 373 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-3]).v.p;
                (yyval).v.p->addChild((yyvsp[-2]).v.p);
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2432 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 85:
#line 379 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
            }
#line 2440 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 86:
#line 382 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2448 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 87:
#line 388 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2456 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 88:
#line 391 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-4]).v.p;
                (yyval).v.p->addChild((yyvsp[-3]).v.p);
                (yyval).v.p->addChild((yyvsp[-1]).v.p);
            }
#line 2466 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 89:
#line 397 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("'end' to match 'for' statement from line ") + decodeline((yyvsp[-4])),(yyvsp[0]));
            }
#line 2472 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 90:
#line 401 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-3]).v.p;
                (yyval).v.p->addChild((yyvsp[-1]).v.p);
            }
#line 2478 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 91:
#line 402 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2484 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 92:
#line 403 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
                (yyval).v.p->addChild(allocateAbstractSyntaxTree(OP_RHS, allocateAbstractSyntaxTree(id_node,(yyvsp[0]).v.p->text.c_str(), (yyvsp[0]).v.p->context()),(yyvsp[0]).v.p->context()));
            }
#line 2491 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 93:
#line 405 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching right parenthesis"),(yyvsp[-4]));
            }
#line 2497 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 94:
#line 406 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("indexing expression"),(yyvsp[-1]));
            }
#line 2503 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 95:
#line 407 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("equals operator after loop index"),(yyvsp[-1]));
            }
#line 2509 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 96:
#line 408 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("identifier that is the loop variable"),(yyvsp[-1]));
            }
#line 2515 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 97:
#line 409 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("indexing expression"),(yyvsp[-1]));
            }
#line 2521 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 98:
#line 410 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("identifier or assignment (id = expr) after 'for' "),(yyvsp[0]));
            }
#line 2527 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 99:
#line 414 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2535 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 100:
#line 417 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-4]).v.p;
                (yyval).v.p->addChild((yyvsp[-3]).v.p);
                (yyval).v.p->addChild((yyvsp[-1]).v.p);
            }
#line 2545 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 101:
#line 422 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("test expression after 'while'"),(yyvsp[-1]));
            }
#line 2551 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 102:
#line 424 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("'end' to match 'while' statement from line ") + decodeline((yyvsp[-4])),(yyvsp[0]));
            }
#line 2557 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 103:
#line 428 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-4]).v.p;
                (yyval).v.p->addChild((yyvsp[-3]).v.p);
                if ((yyvsp[-2]).v.p != nullptr)
                {
                    (yyval).v.p->addChild((yyvsp[-2]).v.p);
                }
                if ((yyvsp[-1]).v.p != nullptr)
                {
                    (yyval).v.p->addChild((yyvsp[-1]).v.p);
                }
            }
#line 2568 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 104:
#line 434 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("condition expression for 'if'"),(yyvsp[-1]));
            }
#line 2574 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 105:
#line 435 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("'end' to match 'if' statement from line ") + decodeline((yyvsp[-4])),(yyvsp[0]));
            }
#line 2580 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 106:
#line 439 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_CSTAT,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-2]).v.p->context());
            }
#line 2588 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 107:
#line 442 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt("valid list of statements after condition",(yyvsp[0]));
            }
#line 2594 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 108:
#line 446 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2600 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 110:
#line 451 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_ELSEIFBLOCK,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 2608 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 111:
#line 454 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 2616 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 112:
#line 460 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
            }
#line 2624 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 113:
#line 463 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("test condition for 'elseif' clause"),(yyvsp[-1]));
            }
#line 2630 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 114:
#line 466 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[0]).v.p;
            }
#line 2638 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 115:
#line 469 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = nullptr;
            }
#line 2644 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 116:
#line 470 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("statement list for 'else' clause"),(yyvsp[-1]));
            }
#line 2650 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 117:
#line 474 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_ASSIGN,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2656 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 118:
#line 475 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("expression in assignment"),(yyvsp[-1]));
            }
#line 2662 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 119:
#line 479 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_MULTICALL,(yyvsp[-5]).v.p,(yyvsp[-2]).v.p,(yyvsp[-6]).v.i);
            }
#line 2670 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 120:
#line 482 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyvsp[-3]).v.p->addChild(allocateAbstractSyntaxTree(OP_PARENS,(yyvsp[-1]).v.p,(yyvsp[-2]).v.i));
                (yyval).v.p = allocateAbstractSyntaxTree(OP_MULTICALL,(yyvsp[-6]).v.p,(yyvsp[-3]).v.p,(yyvsp[-7]).v.i);
            }
#line 2679 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 121:
#line 486 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyvsp[-3]).v.p->addChild(allocateAbstractSyntaxTree(OP_PARENS,(yyvsp[-1]).v.p,(yyvsp[-2]).v.i));
                (yyval).v.p = allocateAbstractSyntaxTree(OP_MULTICALL,(yyvsp[-6]).v.p,(yyvsp[-3]).v.p,(yyvsp[-7]).v.i);
            }
#line 2688 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 122:
#line 490 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyvsp[0]).v.p->addChild(allocateAbstractSyntaxTree(OP_PARENS,NULL,-1));
                (yyval).v.p = allocateAbstractSyntaxTree(OP_MULTICALL,(yyvsp[-3]).v.p,(yyvsp[0]).v.p,(yyvsp[-4]).v.i);
            }
#line 2697 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 123:
#line 494 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching right bracket"), (yyvsp[-2]));
            }
#line 2703 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 124:
#line 495 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching right parenthesis"), (yyvsp[-2]));
            }
#line 2709 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 125:
#line 496 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("indexing list"), (yyvsp[-1]));
            }
#line 2715 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 126:
#line 497 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("indexing list"), (yyvsp[-1]));
            }
#line 2721 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 127:
#line 498 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("left parenthesis"),(yyvsp[-1]));
            }
#line 2727 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 128:
#line 499 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt("identifier",(yyvsp[-1]));
            }
#line 2733 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 129:
#line 503 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_COLON,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2739 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 130:
#line 504 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after ':'"), (yyvsp[-1]));
            }
#line 2745 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 132:
#line 506 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_PLUS,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2751 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 133:
#line 507 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '+'"), (yyvsp[-1]));
            }
#line 2757 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 134:
#line 508 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SUBTRACT,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2763 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 135:
#line 509 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '-'"), (yyvsp[-1]));
            }
#line 2769 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 136:
#line 510 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_TIMES,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2775 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 137:
#line 511 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '*'"),(yyvsp[-1]));
            }
#line 2781 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 138:
#line 512 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_RDIV,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2787 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 139:
#line 513 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '/'"),(yyvsp[-1]));
            }
#line 2793 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 140:
#line 514 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_LDIV,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2799 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 141:
#line 515 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '\\'"),(yyvsp[-1]));
            }
#line 2805 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 142:
#line 516 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_OR,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2811 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 143:
#line 517 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '|'"),(yyvsp[-1]));
            }
#line 2817 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 144:
#line 518 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_AND,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2823 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 145:
#line 519 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '&'"),(yyvsp[-1]));
            }
#line 2829 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 146:
#line 520 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SOR,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2835 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 147:
#line 521 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '||'"),(yyvsp[-1]));
            }
#line 2841 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 148:
#line 522 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SAND,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2847 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 149:
#line 523 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '&&'"),(yyvsp[-1]));
            }
#line 2853 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 150:
#line 524 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_LT,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2859 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 151:
#line 525 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '<'"),(yyvsp[-1]));
            }
#line 2865 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 152:
#line 526 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_LEQ,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2871 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 153:
#line 527 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '<='"),(yyvsp[-1]));
            }
#line 2877 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 154:
#line 528 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_GT,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2883 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 155:
#line 529 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '>'"),(yyvsp[-1]));
            }
#line 2889 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 156:
#line 530 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_GEQ,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2895 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 157:
#line 531 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '>='"),(yyvsp[-1]));
            }
#line 2901 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 158:
#line 532 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_EQ,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2907 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 159:
#line 533 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '=='"),(yyvsp[-1]));
            }
#line 2913 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 160:
#line 534 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_NEQ,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2919 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 161:
#line 535 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '~='"),(yyvsp[-1]));
            }
#line 2925 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 162:
#line 536 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOT_TIMES,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2931 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 163:
#line 537 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '.*'"), (yyvsp[-1]));
            }
#line 2937 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 164:
#line 538 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOT_RDIV,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2943 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 165:
#line 539 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after './'"),(yyvsp[-1]));
            }
#line 2949 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 166:
#line 540 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOT_LDIV,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2955 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 167:
#line 541 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '.\\'"),(yyvsp[-1]));
            }
#line 2961 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 168:
#line 542 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_NEG,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2967 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 169:
#line 543 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_POS, (yyvsp[0]).v.p, (yyvsp[-1]).v.i);
            }
#line 2973 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 170:
#line 544 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_NOT,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2979 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 171:
#line 545 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after logical not"),(yyvsp[0]));
            }
#line 2985 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 172:
#line 546 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_POWER,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 2991 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 173:
#line 547 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '^'"),(yyvsp[-1]));
            }
#line 2997 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 174:
#line 548 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOT_POWER,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 3003 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 175:
#line 549 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after '.^'"),(yyvsp[-1]));
            }
#line 3009 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 176:
#line 550 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_TRANSPOSE,(yyvsp[-1]).v.p,(yyvsp[0]).v.i);
            }
#line 3015 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 177:
#line 551 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOT_TRANSPOSE,(yyvsp[-1]).v.p,(yyvsp[0]).v.i);
            }
#line 3021 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 178:
#line 552 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 3027 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 179:
#line 553 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("a right parenthesis after expression to match this one"),(yyvsp[-2]));
            }
#line 3033 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 180:
#line 554 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("an expression after left parenthesis"),(yyvsp[-1]));
            }
#line 3039 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 184:
#line 561 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_RHS,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 3045 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 185:
#line 562 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 3051 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 186:
#line 563 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("a matrix definition followed by a right bracket"),(yyvsp[-1]));
            }
#line 3057 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 187:
#line 564 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 3063 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 188:
#line 565 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
            }
#line 3069 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 189:
#line 566 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
            }
#line 3075 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 190:
#line 567 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_EMPTY,NULL,(yyvsp[-1]).v.i);
            }
#line 3081 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 191:
#line 568 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 3087 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 192:
#line 569 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
            }
#line 3093 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 193:
#line 570 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
            }
#line 3099 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 194:
#line 571 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
            }
#line 3105 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 195:
#line 572 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_EMPTY_CELL,NULL,(yyvsp[-1]).v.i);
            }
#line 3111 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 196:
#line 573 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("a cell-array definition followed by a right brace"),(yyvsp[-1]));
            }
#line 3117 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 198:
#line 577 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-1]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 3123 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 199:
#line 581 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_PARENS,(yyvsp[-1]).v.p,(yyvsp[-2]).v.i);
            }
#line 3129 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 200:
#line 582 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_PARENS,NULL,(yyvsp[-1]).v.i);
            }
#line 3135 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 201:
#line 583 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching right parenthesis"),(yyvsp[-2]));
            }
#line 3141 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 202:
#line 584 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_BRACES,(yyvsp[-1]).v.p,(yyvsp[-2]).v.i);
            }
#line 3147 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 203:
#line 585 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("matching right brace"),(yyvsp[-2]));
            }
#line 3153 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 204:
#line 586 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOT,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 3159 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 205:
#line 587 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_DOTDYN,(yyvsp[-1]).v.p,(yyvsp[-3]).v.i);
            }
#line 3165 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 207:
#line 592 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_ALL,NULL,(yyvsp[0]).v.i);
            }
#line 3171 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 208:
#line 593 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_KEYWORD,(yyvsp[-2]).v.p,(yyvsp[0]).v.p,(yyvsp[-3]).v.i);
            }
#line 3177 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 209:
#line 594 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("expecting expression after '=' in keyword assignment"),(yyvsp[-1]));
            }
#line 3183 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 210:
#line 595 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_KEYWORD,(yyvsp[0]).v.p,(yyvsp[-1]).v.i);
            }
#line 3189 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 211:
#line 596 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                yyxpt(_("expecting keyword identifier after '/' in keyword assignment"),(yyvsp[-1]));
            }
#line 3195 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 213:
#line 601 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
                (yyval).v.p->addPeer((yyvsp[0]).v.p);
            }
#line 3201 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 214:
#line 605 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_BRACES,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 3207 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 215:
#line 606 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 3213 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 216:
#line 610 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_BRACKETS,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 3219 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 217:
#line 611 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 3225 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 221:
#line 623 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = allocateAbstractSyntaxTree(OP_SEMICOLON,(yyvsp[0]).v.p,(yyvsp[0]).v.p->context());
            }
#line 3231 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
        case 222:
#line 624 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1646  */
            {
                (yyval).v.p = (yyvsp[-2]).v.p;
                (yyval).v.p->addChild((yyvsp[0]).v.p);
            }
#line 3237 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
            break;
#line 3241 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.cpp" /* yacc.c:1646  */
        default:
            break;
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
    YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);
    YYPOPSTACK (yylen);
    yylen = 0;
    YY_STACK_PRINT (yyss, yyssp);
    *++yyvsp = yyval;
    /* Now 'shift' the result of the reduction.  Determine what state
       that goes to, based on the state we popped back to and the rule
       number reduced by.  */
    yyn = yyr1[yyn];
    yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
    if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    {
        yystate = yytable[yystate];
    }
    else
    {
        yystate = yydefgoto[yyn - YYNTOKENS];
    }
    goto yynewstate;
    /*--------------------------------------.
    | yyerrlab -- here on detecting error.  |
    `--------------------------------------*/
yyerrlab:
    /* Make sure we have latest lookahead translation.  See comments at
       user semantic actions for why this is necessary.  */
    yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);
    /* If not already recovering from an error, report this error.  */
    if (!yyerrstatus)
    {
        ++yynerrs;
#if ! YYERROR_VERBOSE
        yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
        {
            char const *yymsgp = YY_("syntax error");
            int yysyntax_error_status;
            yysyntax_error_status = YYSYNTAX_ERROR;
            if (yysyntax_error_status == 0)
            {
                yymsgp = yymsg;
            }
            else if (yysyntax_error_status == 1)
            {
                if (yymsg != yymsgbuf)
                {
                    YYSTACK_FREE (yymsg);
                }
                yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
                if (!yymsg)
                {
                    yymsg = yymsgbuf;
                    yymsg_alloc = sizeof yymsgbuf;
                    yysyntax_error_status = 2;
                }
                else
                {
                    yysyntax_error_status = YYSYNTAX_ERROR;
                    yymsgp = yymsg;
                }
            }
            yyerror (yymsgp);
            if (yysyntax_error_status == 2)
            {
                goto yyexhaustedlab;
            }
        }
# undef YYSYNTAX_ERROR
#endif
    }
    if (yyerrstatus == 3)
    {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */
        if (yychar <= YYEOF)
        {
            /* Return failure if at end of input.  */
            if (yychar == YYEOF)
            {
                YYABORT;
            }
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
    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (/*CONSTCOND*/ 0)
    {
        goto yyerrorlab;
    }
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
    for (;;)
    {
        yyn = yypact[yystate];
        if (!yypact_value_is_default (yyn))
        {
            yyn += YYTERROR;
            if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
                yyn = yytable[yyn];
                if (0 < yyn)
                {
                    break;
                }
            }
        }
        /* Pop the current state because it cannot handle the error token.  */
        if (yyssp == yyss)
        {
            YYABORT;
        }
        yydestruct ("Error: popping",
                    yystos[yystate], yyvsp);
        YYPOPSTACK (1);
        yystate = *yyssp;
        YY_STACK_PRINT (yyss, yyssp);
    }
    YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
    *++yyvsp = yylval;
    YY_IGNORE_MAYBE_UNINITIALIZED_END
    /* Shift the error token.  */
    YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);
    yystate = yyn;
    goto yynewstate;
    /*-------------------------------------.
    | yyacceptlab -- YYACCEPT comes here.  |
    `-------------------------------------*/
yyacceptlab:
    yyresult = 0;
    goto yyreturn;
    /*-----------------------------------.
    | yyabortlab -- YYABORT comes here.  |
    `-----------------------------------*/
yyabortlab:
    yyresult = 1;
    goto yyreturn;
#if !defined yyoverflow || YYERROR_VERBOSE
    /*-------------------------------------------------.
    | yyexhaustedlab -- memory exhaustion comes here.  |
    `-------------------------------------------------*/
yyexhaustedlab:
    yyerror (YY_("memory exhausted"));
    yyresult = 2;
    /* Fall through.  */
#endif
yyreturn:
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
                    yystos[*yyssp], yyvsp);
        YYPOPSTACK (1);
    }
#ifndef yyoverflow
    if (yyss != yyssa)
    {
        YYSTACK_FREE (yyss);
    }
#endif
#if YYERROR_VERBOSE
    if (yymsg != yymsgbuf)
    {
        YYSTACK_FREE (yymsg);
    }
#endif
    return yyresult;
}
#line 627 "..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParser.yxx" /* yacc.c:1906  */


namespace Nelson {

    void resetParser()
    {
        if (mainAST)
        {
            mainAST = nullptr;
        }
        if (mainMDef)
        {
            mainMDef = nullptr;
        }
    }

    ASTPtr getParsedScriptBlock()
    {
        return mainAST;
    }

    MacroFunctionDef* getParsedFunctionDef()
    {
        return mainMDef;
    }

    ParserState parseState()
    {
        if (mainAST != nullptr)
        {
            return ScriptBlock;
        }
        else
        {
            return FuncDef;
        }
    }

    ParserState parseString(std::string txt)
    {
        resetParser();
        interactiveMode = true;
        setLexBuffer(txt.c_str());
        yyparse();
        return parseState();
    }

    ParserState parseFile(FILE *fp, const char* fname)
    {
        resetParser();
        interactiveMode = false;
        setParserFilename(fname);
        setLexFile(fp);
        yyparse();
        ParserState pstate = parseState();
        setParserFilename("");
        return pstate;
    }
}
