// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton interface for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2021 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


/**
 ** \file D:\Developpements\Github\nelson-lang\nelson-thirdparty-x64\flex-bison\..\..\NelSon\modules\interpreter\src\grammar\NelSonParserCpp.hpp
 ** Define the Nelson::parser class.
 */

// C++ LALR(1) parser skeleton written by Akim Demaille.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.

#ifndef YY_YY_D_DEVELOPPEMENTS_GITHUB_NELSON_LANG_NELSON_THIRDPARTY_X64_FLEX_BISON_NELSON_MODULES_INTERPRETER_SRC_GRAMMAR_NELSONPARSERCPP_HPP_INCLUDED
# define YY_YY_D_DEVELOPPEMENTS_GITHUB_NELSON_LANG_NELSON_THIRDPARTY_X64_FLEX_BISON_NELSON_MODULES_INTERPRETER_SRC_GRAMMAR_NELSONPARSERCPP_HPP_INCLUDED
// "%code requires" blocks.
#line 11 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"

#include "NelsonParserHelpers.hpp"

#line 53 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.hpp"


# include <cstdlib> // std::abort
# include <iostream>
# include <stdexcept>
# include <string>
# include <vector>

#if defined __cplusplus
# define YY_CPLUSPLUS __cplusplus
#else
# define YY_CPLUSPLUS 199711L
#endif

// Support move semantics when possible.
#if 201103L <= YY_CPLUSPLUS
# define YY_MOVE           std::move
# define YY_MOVE_OR_COPY   move
# define YY_MOVE_REF(Type) Type&&
# define YY_RVREF(Type)    Type&&
# define YY_COPY(Type)     Type
#else
# define YY_MOVE
# define YY_MOVE_OR_COPY   copy
# define YY_MOVE_REF(Type) Type&
# define YY_RVREF(Type)    const Type&
# define YY_COPY(Type)     const Type&
#endif

// Support noexcept when possible.
#if 201103L <= YY_CPLUSPLUS
# define YY_NOEXCEPT noexcept
# define YY_NOTHROW
#else
# define YY_NOEXCEPT
# define YY_NOTHROW throw ()
#endif

// Support constexpr when possible.
#if 201703 <= YY_CPLUSPLUS
# define YY_CONSTEXPR constexpr
#else
# define YY_CONSTEXPR
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

#line 5 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
namespace Nelson {
#line 189 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.hpp"




  /// A Bison parser.
  class NelSonParser
  {
  public:
#ifdef YYSTYPE
# ifdef __GNUC__
#  pragma GCC message "bison: do not #define YYSTYPE in C++, use %define api.value.type"
# endif
    typedef YYSTYPE value_type;
#else
    /// Symbol semantic values.
    typedef ParseRHS value_type;
#endif
    /// Backward compatibility (Bison 3.8).
    typedef value_type semantic_type;


    /// Syntax errors thrown from user actions.
    struct syntax_error : std::runtime_error
    {
      syntax_error (const std::string& m)
        : std::runtime_error (m)
      {}

      syntax_error (const syntax_error& s)
        : std::runtime_error (s.what ())
      {}

      ~syntax_error () YY_NOEXCEPT YY_NOTHROW;
    };

    /// Token kinds.
    struct token
    {
      enum token_kind_type
      {
        YYEMPTY = -2,
    YYEOF = 0,                     // "end of file"
    YYerror = 256,                 // error
    YYUNDEF = 257,                 // "invalid token"
    IDENT = 258,                   // IDENT
    NUMERIC = 259,                 // NUMERIC
    ENDQSTMNT = 260,               // ENDQSTMNT
    ENDSTMNT = 261,                // ENDSTMNT
    LE = 262,                      // LE
    GE = 263,                      // GE
    EQ = 264,                      // EQ
    DOTTIMES = 265,                // DOTTIMES
    DOTRDIV = 266,                 // DOTRDIV
    DOTLDIV = 267,                 // DOTLDIV
    DOTPOWER = 268,                // DOTPOWER
    DOTTRANSPOSE = 269,            // DOTTRANSPOSE
    CHARACTER = 270,               // CHARACTER
    STRING = 271,                  // STRING
    SPECIALCALL = 272,             // SPECIALCALL
    END = 273,                     // END
    IF = 274,                      // IF
    FUNCTION = 275,                // FUNCTION
    FOR = 276,                     // FOR
    BREAK = 277,                   // BREAK
    MAGICEND = 278,                // MAGICEND
    WHILE = 279,                   // WHILE
    ELSE = 280,                    // ELSE
    ELSEIF = 281,                  // ELSEIF
    SWITCH = 282,                  // SWITCH
    CASE = 283,                    // CASE
    OTHERWISE = 284,               // OTHERWISE
    CONTINUE = 285,                // CONTINUE
    TRY = 286,                     // TRY
    CATCH = 287,                   // CATCH
    FIELD = 288,                   // FIELD
    REFLPAREN = 289,               // REFLPAREN
    REFRPAREN = 290,               // REFRPAREN
    KEYBOARD = 291,                // KEYBOARD
    RETURN = 292,                  // RETURN
    VARARGIN = 293,                // VARARGIN
    VARARGOUT = 294,               // VARARGOUT
    ABORT = 295,                   // ABORT
    ENDFUNCTION = 296,             // ENDFUNCTION
    SOR = 297,                     // SOR
    SAND = 298,                    // SAND
    NE = 299,                      // NE
    POS = 300,                     // POS
    NEG = 301,                     // NEG
    NOT = 302                      // NOT
      };
      /// Backward compatibility alias (Bison 3.6).
      typedef token_kind_type yytokentype;
    };

    /// Token kind, as returned by yylex.
    typedef token::token_kind_type token_kind_type;

    /// Backward compatibility alias (Bison 3.6).
    typedef token_kind_type token_type;

    /// Symbol kinds.
    struct symbol_kind
    {
      enum symbol_kind_type
      {
        YYNTOKENS = 72, ///< Number of tokens.
        S_YYEMPTY = -2,
        S_YYEOF = 0,                             // "end of file"
        S_YYerror = 1,                           // error
        S_YYUNDEF = 2,                           // "invalid token"
        S_IDENT = 3,                             // IDENT
        S_NUMERIC = 4,                           // NUMERIC
        S_ENDQSTMNT = 5,                         // ENDQSTMNT
        S_ENDSTMNT = 6,                          // ENDSTMNT
        S_LE = 7,                                // LE
        S_GE = 8,                                // GE
        S_EQ = 9,                                // EQ
        S_DOTTIMES = 10,                         // DOTTIMES
        S_DOTRDIV = 11,                          // DOTRDIV
        S_DOTLDIV = 12,                          // DOTLDIV
        S_DOTPOWER = 13,                         // DOTPOWER
        S_DOTTRANSPOSE = 14,                     // DOTTRANSPOSE
        S_CHARACTER = 15,                        // CHARACTER
        S_STRING = 16,                           // STRING
        S_SPECIALCALL = 17,                      // SPECIALCALL
        S_END = 18,                              // END
        S_IF = 19,                               // IF
        S_FUNCTION = 20,                         // FUNCTION
        S_FOR = 21,                              // FOR
        S_BREAK = 22,                            // BREAK
        S_MAGICEND = 23,                         // MAGICEND
        S_WHILE = 24,                            // WHILE
        S_ELSE = 25,                             // ELSE
        S_ELSEIF = 26,                           // ELSEIF
        S_SWITCH = 27,                           // SWITCH
        S_CASE = 28,                             // CASE
        S_OTHERWISE = 29,                        // OTHERWISE
        S_CONTINUE = 30,                         // CONTINUE
        S_TRY = 31,                              // TRY
        S_CATCH = 32,                            // CATCH
        S_FIELD = 33,                            // FIELD
        S_REFLPAREN = 34,                        // REFLPAREN
        S_REFRPAREN = 35,                        // REFRPAREN
        S_KEYBOARD = 36,                         // KEYBOARD
        S_RETURN = 37,                           // RETURN
        S_VARARGIN = 38,                         // VARARGIN
        S_VARARGOUT = 39,                        // VARARGOUT
        S_ABORT = 40,                            // ABORT
        S_ENDFUNCTION = 41,                      // ENDFUNCTION
        S_SOR = 42,                              // SOR
        S_SAND = 43,                             // SAND
        S_44_ = 44,                              // '|'
        S_45_ = 45,                              // '&'
        S_46_ = 46,                              // '<'
        S_47_ = 47,                              // '>'
        S_NE = 48,                               // NE
        S_49_ = 49,                              // ':'
        S_50_ = 50,                              // '+'
        S_51_ = 51,                              // '-'
        S_52_ = 52,                              // '*'
        S_53_ = 53,                              // '/'
        S_54_ = 54,                              // '\\'
        S_POS = 55,                              // POS
        S_NEG = 56,                              // NEG
        S_NOT = 57,                              // NOT
        S_58_ = 58,                              // '^'
        S_59_ = 59,                              // '\''
        S_60_ = 60,                              // '('
        S_61_ = 61,                              // ')'
        S_62_ = 62,                              // '='
        S_63_ = 63,                              // '['
        S_64_ = 64,                              // ']'
        S_65_ = 65,                              // ';'
        S_66_ = 66,                              // '{'
        S_67_ = 67,                              // '}'
        S_68_ = 68,                              // '~'
        S_69_ = 69,                              // '@'
        S_70_ = 70,                              // '.'
        S_71_ = 71,                              // ','
        S_YYACCEPT = 72,                         // $accept
        S_program = 73,                          // program
        S_functionDef = 74,                      // functionDef
        S_functionDefList = 75,                  // functionDefList
        S_returnDeclaration = 76,                // returnDeclaration
        S_argumentList = 77,                     // argumentList
        S_argument = 78,                         // argument
        S_statementList = 79,                    // statementList
        S_statement = 80,                        // statement
        S_statementType = 81,                    // statementType
        S_functionTerminatorStatement = 82,      // functionTerminatorStatement
        S_specialSyntaxStatement = 83,           // specialSyntaxStatement
        S_returnStatement = 84,                  // returnStatement
        S_pauseStatement = 85,                   // pauseStatement
        S_continueStatement = 86,                // continueStatement
        S_breakStatement = 87,                   // breakStatement
        S_tryStatement = 88,                     // tryStatement
        S_optionalCatch = 89,                    // optionalCatch
        S_switchStatement = 90,                  // switchStatement
        S_optionalEndStatement = 91,             // optionalEndStatement
        S_newLine = 92,                          // newLine
        S_caseBlock = 93,                        // caseBlock
        S_caseList = 94,                         // caseList
        S_caseStatement = 95,                    // caseStatement
        S_otherwiseClause = 96,                  // otherwiseClause
        S_forStatement = 97,                     // forStatement
        S_forIndexExpression = 98,               // forIndexExpression
        S_whileStatement = 99,                   // whileStatement
        S_ifStatement = 100,                     // ifStatement
        S_conditionedStatement = 101,            // conditionedStatement
        S_elseIfBlock = 102,                     // elseIfBlock
        S_elseIfStatementList = 103,             // elseIfStatementList
        S_elseIfStatement = 104,                 // elseIfStatement
        S_elseStatement = 105,                   // elseStatement
        S_assignmentStatement = 106,             // assignmentStatement
        S_multiFunctionCall = 107,               // multiFunctionCall
        S_expr = 108,                            // expr
        S_anonymousFunction = 109,               // anonymousFunction
        S_terminal = 110,                        // terminal
        S_postfixableLiteral = 111,              // postfixableLiteral
        S_symbRefList = 112,                     // symbRefList
        S_postfixRefList = 113,                  // postfixRefList
        S_symbRef = 114,                         // symbRef
        S_indexElement = 115,                    // indexElement
        S_indexList = 116,                       // indexList
        S_cellDef = 117,                         // cellDef
        S_matrixDef = 118,                       // matrixDef
        S_rowSeperator = 119,                    // rowSeperator
        S_columnSep = 120,                       // columnSep
        S_rowDef = 121,                          // rowDef
        S_parenExpr = 122                        // parenExpr
      };
    };

    /// (Internal) symbol kind.
    typedef symbol_kind::symbol_kind_type symbol_kind_type;

    /// The number of tokens.
    static const symbol_kind_type YYNTOKENS = symbol_kind::YYNTOKENS;

    /// A complete symbol.
    ///
    /// Expects its Base type to provide access to the symbol kind
    /// via kind ().
    ///
    /// Provide access to semantic value.
    template <typename Base>
    struct basic_symbol : Base
    {
      /// Alias to Base.
      typedef Base super_type;

      /// Default constructor.
      basic_symbol () YY_NOEXCEPT
        : value ()
      {}

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      basic_symbol (basic_symbol&& that)
        : Base (std::move (that))
        , value (std::move (that.value))
      {}
#endif

      /// Copy constructor.
      basic_symbol (const basic_symbol& that);
      /// Constructor for valueless symbols.
      basic_symbol (typename Base::kind_type t);

      /// Constructor for symbols with semantic value.
      basic_symbol (typename Base::kind_type t,
                    YY_RVREF (value_type) v);

      /// Destroy the symbol.
      ~basic_symbol ()
      {
        clear ();
      }



      /// Destroy contents, and record that is empty.
      void clear () YY_NOEXCEPT
      {
        Base::clear ();
      }

#if YYDEBUG || 0
      /// The user-facing name of this symbol.
      const char *name () const YY_NOEXCEPT
      {
        return NelSonParser::symbol_name (this->kind ());
      }
#endif // #if YYDEBUG || 0


      /// Backward compatibility (Bison 3.6).
      symbol_kind_type type_get () const YY_NOEXCEPT;

      /// Whether empty.
      bool empty () const YY_NOEXCEPT;

      /// Destructive move, \a s is emptied into this.
      void move (basic_symbol& s);

      /// The semantic value.
      value_type value;

    private:
#if YY_CPLUSPLUS < 201103L
      /// Assignment operator.
      basic_symbol& operator= (const basic_symbol& that);
#endif
    };

    /// Type access provider for token (enum) based symbols.
    struct by_kind
    {
      /// The symbol kind as needed by the constructor.
      typedef token_kind_type kind_type;

      /// Default constructor.
      by_kind () YY_NOEXCEPT;

#if 201103L <= YY_CPLUSPLUS
      /// Move constructor.
      by_kind (by_kind&& that) YY_NOEXCEPT;
#endif

      /// Copy constructor.
      by_kind (const by_kind& that) YY_NOEXCEPT;

      /// Constructor from (external) token numbers.
      by_kind (kind_type t) YY_NOEXCEPT;



      /// Record that this symbol is empty.
      void clear () YY_NOEXCEPT;

      /// Steal the symbol kind from \a that.
      void move (by_kind& that);

      /// The (internal) type number (corresponding to \a type).
      /// \a empty when empty.
      symbol_kind_type kind () const YY_NOEXCEPT;

      /// Backward compatibility (Bison 3.6).
      symbol_kind_type type_get () const YY_NOEXCEPT;

      /// The symbol kind.
      /// \a S_YYEMPTY when empty.
      symbol_kind_type kind_;
    };

    /// Backward compatibility for a private implementation detail (Bison 3.6).
    typedef by_kind by_type;

    /// "External" symbols: returned by the scanner.
    struct symbol_type : basic_symbol<by_kind>
    {};

    /// Build a parser object.
    NelSonParser (LexerContext &lexerContext_yyarg, ParserContext &parserContext_yyarg);
    virtual ~NelSonParser ();

#if 201103L <= YY_CPLUSPLUS
    /// Non copyable.
    NelSonParser (const NelSonParser&) = delete;
    /// Non copyable.
    NelSonParser& operator= (const NelSonParser&) = delete;
#endif

    /// Parse.  An alias for parse ().
    /// \returns  0 iff parsing succeeded.
    int operator() ();

    /// Parse.
    /// \returns  0 iff parsing succeeded.
    virtual int parse ();

#if YYDEBUG
    /// The current debugging stream.
    std::ostream& debug_stream () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging stream.
    void set_debug_stream (std::ostream &);

    /// Type for debugging levels.
    typedef int debug_level_type;
    /// The current debugging level.
    debug_level_type debug_level () const YY_ATTRIBUTE_PURE;
    /// Set the current debugging level.
    void set_debug_level (debug_level_type l);
#endif

    /// Report a syntax error.
    /// \param msg    a description of the syntax error.
    virtual void error (const std::string& msg);

    /// Report a syntax error.
    void error (const syntax_error& err);

#if YYDEBUG || 0
    /// The user-facing name of the symbol whose (internal) number is
    /// YYSYMBOL.  No bounds checking.
    static const char *symbol_name (symbol_kind_type yysymbol);
#endif // #if YYDEBUG || 0




  private:
#if YY_CPLUSPLUS < 201103L
    /// Non copyable.
    NelSonParser (const NelSonParser&);
    /// Non copyable.
    NelSonParser& operator= (const NelSonParser&);
#endif


    /// Stored state numbers (used for stacks).
    typedef short state_type;

    /// Compute post-reduction state.
    /// \param yystate   the current state
    /// \param yysym     the nonterminal to push on the stack
    static state_type yy_lr_goto_state_ (state_type yystate, int yysym);

    /// Whether the given \c yypact_ value indicates a defaulted state.
    /// \param yyvalue   the value to check
    static bool yy_pact_value_is_default_ (int yyvalue) YY_NOEXCEPT;

    /// Whether the given \c yytable_ value indicates a syntax error.
    /// \param yyvalue   the value to check
    static bool yy_table_value_is_error_ (int yyvalue) YY_NOEXCEPT;

    static const short yypact_ninf_;
    static const short yytable_ninf_;

    /// Convert a scanner token kind \a t to a symbol kind.
    /// In theory \a t should be a token_kind_type, but character literals
    /// are valid, yet not members of the token_kind_type enum.
    static symbol_kind_type yytranslate_ (int t) YY_NOEXCEPT;

#if YYDEBUG || 0
    /// For a symbol, its name in clear.
    static const char* const yytname_[];
#endif // #if YYDEBUG || 0


    // Tables.
    // YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
    // STATE-NUM.
    static const short yypact_[];

    // YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
    // Performed when YYTABLE does not specify something else to do.  Zero
    // means the default is an error.
    static const unsigned char yydefact_[];

    // YYPGOTO[NTERM-NUM].
    static const short yypgoto_[];

    // YYDEFGOTO[NTERM-NUM].
    static const short yydefgoto_[];

    // YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
    // positive, shift that token.  If negative, reduce the rule whose
    // number is the opposite.  If YYTABLE_NINF, syntax error.
    static const short yytable_[];

    static const short yycheck_[];

    // YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
    // state STATE-NUM.
    static const signed char yystos_[];

    // YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.
    static const signed char yyr1_[];

    // YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.
    static const signed char yyr2_[];


#if YYDEBUG
    // YYRLINE[YYN] -- Source line where rule number YYN was defined.
    static const short yyrline_[];
    /// Report on the debug stream that the rule \a r is going to be reduced.
    virtual void yy_reduce_print_ (int r) const;
    /// Print the state stack on the debug stream.
    virtual void yy_stack_print_ () const;

    /// Debugging level.
    int yydebug_;
    /// Debug stream.
    std::ostream* yycdebug_;

    /// \brief Display a symbol kind, value and location.
    /// \param yyo    The output stream.
    /// \param yysym  The symbol.
    template <typename Base>
    void yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const;
#endif

    /// \brief Reclaim the memory associated to a symbol.
    /// \param yymsg     Why this token is reclaimed.
    ///                  If null, print nothing.
    /// \param yysym     The symbol.
    template <typename Base>
    void yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const;

  private:
    /// Type access provider for state based symbols.
    struct by_state
    {
      /// Default constructor.
      by_state () YY_NOEXCEPT;

      /// The symbol kind as needed by the constructor.
      typedef state_type kind_type;

      /// Constructor.
      by_state (kind_type s) YY_NOEXCEPT;

      /// Copy constructor.
      by_state (const by_state& that) YY_NOEXCEPT;

      /// Record that this symbol is empty.
      void clear () YY_NOEXCEPT;

      /// Steal the symbol kind from \a that.
      void move (by_state& that);

      /// The symbol kind (corresponding to \a state).
      /// \a symbol_kind::S_YYEMPTY when empty.
      symbol_kind_type kind () const YY_NOEXCEPT;

      /// The state number used to denote an empty symbol.
      /// We use the initial state, as it does not have a value.
      enum { empty_state = 0 };

      /// The state.
      /// \a empty when empty.
      state_type state;
    };

    /// "Internal" symbol: element of the stack.
    struct stack_symbol_type : basic_symbol<by_state>
    {
      /// Superclass.
      typedef basic_symbol<by_state> super_type;
      /// Construct an empty symbol.
      stack_symbol_type ();
      /// Move or copy construction.
      stack_symbol_type (YY_RVREF (stack_symbol_type) that);
      /// Steal the contents from \a sym to build this.
      stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) sym);
#if YY_CPLUSPLUS < 201103L
      /// Assignment, needed by push_back by some old implementations.
      /// Moves the contents of that.
      stack_symbol_type& operator= (stack_symbol_type& that);

      /// Assignment, needed by push_back by other implementations.
      /// Needed by some other old implementations.
      stack_symbol_type& operator= (const stack_symbol_type& that);
#endif
    };

    /// A stack with random access from its top.
    template <typename T, typename S = std::vector<T> >
    class stack
    {
    public:
      // Hide our reversed order.
      typedef typename S::iterator iterator;
      typedef typename S::const_iterator const_iterator;
      typedef typename S::size_type size_type;
      typedef typename std::ptrdiff_t index_type;

      stack (size_type n = 200) YY_NOEXCEPT
        : seq_ (n)
      {}

#if 201103L <= YY_CPLUSPLUS
      /// Non copyable.
      stack (const stack&) = delete;
      /// Non copyable.
      stack& operator= (const stack&) = delete;
#endif

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      const T&
      operator[] (index_type i) const
      {
        return seq_[size_type (size () - 1 - i)];
      }

      /// Random access.
      ///
      /// Index 0 returns the topmost element.
      T&
      operator[] (index_type i)
      {
        return seq_[size_type (size () - 1 - i)];
      }

      /// Steal the contents of \a t.
      ///
      /// Close to move-semantics.
      void
      push (YY_MOVE_REF (T) t)
      {
        seq_.push_back (T ());
        operator[] (0).move (t);
      }

      /// Pop elements from the stack.
      void
      pop (std::ptrdiff_t n = 1) YY_NOEXCEPT
      {
        for (; 0 < n; --n)
          seq_.pop_back ();
      }

      /// Pop all elements from the stack.
      void
      clear () YY_NOEXCEPT
      {
        seq_.clear ();
      }

      /// Number of elements on the stack.
      index_type
      size () const YY_NOEXCEPT
      {
        return index_type (seq_.size ());
      }

      /// Iterator on top of the stack (going downwards).
      const_iterator
      begin () const YY_NOEXCEPT
      {
        return seq_.begin ();
      }

      /// Bottom of the stack.
      const_iterator
      end () const YY_NOEXCEPT
      {
        return seq_.end ();
      }

      /// Present a slice of the top of a stack.
      class slice
      {
      public:
        slice (const stack& stack, index_type range) YY_NOEXCEPT
          : stack_ (stack)
          , range_ (range)
        {}

        const T&
        operator[] (index_type i) const
        {
          return stack_[range_ - i];
        }

      private:
        const stack& stack_;
        index_type range_;
      };

    private:
#if YY_CPLUSPLUS < 201103L
      /// Non copyable.
      stack (const stack&);
      /// Non copyable.
      stack& operator= (const stack&);
#endif
      /// The wrapped container.
      S seq_;
    };


    /// Stack type.
    typedef stack<stack_symbol_type> stack_type;

    /// The stack.
    stack_type yystack_;

    /// Push a new state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param sym  the symbol
    /// \warning the contents of \a s.value is stolen.
    void yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym);

    /// Push a new look ahead token on the state on the stack.
    /// \param m    a debug message to display
    ///             if null, no trace is output.
    /// \param s    the state
    /// \param sym  the symbol (for its value and location).
    /// \warning the contents of \a sym.value is stolen.
    void yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym);

    /// Pop \a n symbols from the stack.
    void yypop_ (int n = 1) YY_NOEXCEPT;

    /// Constants.
    enum
    {
      yylast_ = 3094,     ///< Last index in yytable_.
      yynnts_ = 51,  ///< Number of nonterminal symbols.
      yyfinal_ = 101 ///< Termination state number.
    };


    // User arguments.
    LexerContext &lexerContext;
    ParserContext &parserContext;

  };


#line 5 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
} // Nelson
#line 918 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.hpp"




#endif // !YY_YY_D_DEVELOPPEMENTS_GITHUB_NELSON_LANG_NELSON_THIRDPARTY_X64_FLEX_BISON_NELSON_MODULES_INTERPRETER_SRC_GRAMMAR_NELSONPARSERCPP_HPP_INCLUDED
