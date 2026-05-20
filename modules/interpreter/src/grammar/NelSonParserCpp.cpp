// A Bison parser, made by GNU Bison 3.8.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

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

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.



// First part of user prologue.
#line 14 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"

// clang-format off
//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// clang-format off
// Generated C++ parser output is committed as NelSonParserCpp.cpp/.hpp.
//=============================================================================
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "NelsonParserHelpers.hpp"
#include "AbstractSyntaxTree.hpp"
#include "i18n.hpp"
#include "LexerContext.hpp"
#include "ParserInterface.hpp"
//=============================================================================
#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
#endif
//=============================================================================
//=============================================================================
extern int yylex(Nelson::LexerContext& lexerContext);
extern thread_local Nelson::ParseRHS yylval;
namespace Nelson {
  static int yylex(ParseRHS* semanticValue, LexerContext& lexerContext);
}
extern int yydebug;
//=============================================================================
namespace Nelson {
  void yyerror(Nelson::LexerContext& lexerContext, Nelson::ParserContext& parserContext, const char* s) {
    return;
  }
}
//=============================================================================
using namespace Nelson;
//=============================================================================

#line 86 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"


#include "NelSonParserCpp.hpp"




#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif


// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif



// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yy_stack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YY_USE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

#line 5 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
namespace Nelson {
#line 165 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"

  /// Build a parser object.
  NelSonParser::NelSonParser (LexerContext &lexerContext_yyarg, ParserContext &parserContext_yyarg)
#if YYDEBUG
    : yydebug_ (false),
      yycdebug_ (&std::cerr),
#else
    :
#endif
      lexerContext (lexerContext_yyarg),
      parserContext (parserContext_yyarg)
  {}

  NelSonParser::~NelSonParser ()
  {}

  NelSonParser::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------.
  | symbol.  |
  `---------*/

  // basic_symbol.
  template <typename Base>
  NelSonParser::basic_symbol<Base>::basic_symbol (const basic_symbol& that)
    : Base (that)
    , value (that.value)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  NelSonParser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t)
    : Base (t)
    , value ()
  {}

  template <typename Base>
  NelSonParser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, YY_RVREF (value_type) v)
    : Base (t)
    , value (YY_MOVE (v))
  {}


  template <typename Base>
  NelSonParser::symbol_kind_type
  NelSonParser::basic_symbol<Base>::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }


  template <typename Base>
  bool
  NelSonParser::basic_symbol<Base>::empty () const YY_NOEXCEPT
  {
    return this->kind () == symbol_kind::S_YYEMPTY;
  }

  template <typename Base>
  void
  NelSonParser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move (s);
    value = YY_MOVE (s.value);
  }

  // by_kind.
  NelSonParser::by_kind::by_kind () YY_NOEXCEPT
    : kind_ (symbol_kind::S_YYEMPTY)
  {}

#if 201103L <= YY_CPLUSPLUS
  NelSonParser::by_kind::by_kind (by_kind&& that) YY_NOEXCEPT
    : kind_ (that.kind_)
  {
    that.clear ();
  }
#endif

  NelSonParser::by_kind::by_kind (const by_kind& that) YY_NOEXCEPT
    : kind_ (that.kind_)
  {}

  NelSonParser::by_kind::by_kind (token_kind_type t) YY_NOEXCEPT
    : kind_ (yytranslate_ (t))
  {}



  void
  NelSonParser::by_kind::clear () YY_NOEXCEPT
  {
    kind_ = symbol_kind::S_YYEMPTY;
  }

  void
  NelSonParser::by_kind::move (by_kind& that)
  {
    kind_ = that.kind_;
    that.clear ();
  }

  NelSonParser::symbol_kind_type
  NelSonParser::by_kind::kind () const YY_NOEXCEPT
  {
    return kind_;
  }


  NelSonParser::symbol_kind_type
  NelSonParser::by_kind::type_get () const YY_NOEXCEPT
  {
    return this->kind ();
  }



  // by_state.
  NelSonParser::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

  NelSonParser::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
  NelSonParser::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
  NelSonParser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  NelSonParser::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

  NelSonParser::symbol_kind_type
  NelSonParser::by_state::kind () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return symbol_kind::S_YYEMPTY;
    else
      return YY_CAST (symbol_kind_type, yystos_[+state]);
  }

  NelSonParser::stack_symbol_type::stack_symbol_type ()
  {}

  NelSonParser::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.value))
  {
#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

  NelSonParser::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.value))
  {
    // that is emptied.
    that.kind_ = symbol_kind::S_YYEMPTY;
  }

#if YY_CPLUSPLUS < 201103L
  NelSonParser::stack_symbol_type&
  NelSonParser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    return *this;
  }

  NelSonParser::stack_symbol_type&
  NelSonParser::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
  NelSonParser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    YY_USE (yysym.kind ());
  }

#if YYDEBUG
  template <typename Base>
  void
  NelSonParser::yy_print_ (std::ostream& yyo, const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YY_USE (yyoutput);
    if (yysym.empty ())
      yyo << "empty symbol";
    else
      {
        symbol_kind_type yykind = yysym.kind ();
        yyo << (yykind < YYNTOKENS ? "token" : "nterm")
            << ' ' << yysym.name () << " (";
        YY_USE (yykind);
        yyo << ')';
      }
  }
#endif

  void
  NelSonParser::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
  NelSonParser::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
  NelSonParser::yypop_ (int n) YY_NOEXCEPT
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  NelSonParser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  NelSonParser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  NelSonParser::debug_level_type
  NelSonParser::debug_level () const
  {
    return yydebug_;
  }

  void
  NelSonParser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  NelSonParser::state_type
  NelSonParser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - YYNTOKENS] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - YYNTOKENS];
  }

  bool
  NelSonParser::yy_pact_value_is_default_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yypact_ninf_;
  }

  bool
  NelSonParser::yy_table_value_is_error_ (int yyvalue) YY_NOEXCEPT
  {
    return yyvalue == yytable_ninf_;
  }

  int
  NelSonParser::operator() ()
  {
    return parse ();
  }

  int
  NelSonParser::parse ()
  {
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << int (yystack_[0].state) << '\n';
    YY_STACK_PRINT ();

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[+yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token\n";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            yyla.kind_ = yytranslate_ (yylex (&yyla.value, lexerContext));
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    if (yyla.kind () == symbol_kind::S_YYerror)
    {
      // The scanner already issued an error message, process directly
      // to error recovery.  But do not keep the error token as
      // lookahead, it is too special and may lead us to an endless
      // loop in error recovery. */
      yyla.kind_ = symbol_kind::S_YYUNDEF;
      goto yyerrlab1;
    }

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.kind ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.kind ())
      {
        goto yydefault;
      }

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", state_type (yyn), YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[+yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;


      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {
  case 2: // program: statementList
#line 90 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                { setParsedScriptBlock(yystack_[0].value.v.p);}
#line 623 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 5: // program: error
#line 92 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
          {yyxpt(_("statement list or function definition"));}
#line 629 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 6: // functionDef: FUNCTION returnDeclaration IDENT '(' argumentList ')' newLine statementList
#line 96 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                                                {
   ParseRHS lhsRhs = yystack_[6].value;
   ParseRHS nameRhs = yystack_[5].value;
   ParseRHS rhsRhs = yystack_[3].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
  }
#line 641 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 7: // functionDef: FUNCTION IDENT '(' argumentList ')' newLine statementList
#line 103 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                               {
   ParseRHS lhsRhs;
   lhsRhs.v.p = nullptr;
   ParseRHS nameRhs = yystack_[5].value;
   ParseRHS rhsRhs = yystack_[3].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 654 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 8: // functionDef: FUNCTION returnDeclaration IDENT newLine statementList
#line 111 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                             {
    ParseRHS lhsRhs = yystack_[3].value;
    ParseRHS nameRhs = yystack_[2].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 667 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 9: // functionDef: FUNCTION IDENT newLine statementList
#line 119 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                           {
    ParseRHS lhsRhs;
    lhsRhs.v.p = nullptr;
    ParseRHS nameRhs = yystack_[2].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 681 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 10: // functionDef: FUNCTION returnDeclaration IDENT '(' ')' newLine statementList
#line 128 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                                     {
    ParseRHS lhsRhs = yystack_[5].value;
    ParseRHS nameRhs = yystack_[4].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 694 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 11: // functionDef: FUNCTION IDENT '(' ')' newLine statementList
#line 136 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                   {
    ParseRHS lhsRhs;
    lhsRhs.v.p = nullptr;
    ParseRHS nameRhs = yystack_[4].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 708 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 12: // functionDef: FUNCTION error
#line 145 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                     {yyxpt(_("legal function name or return declaration after 'function'"), yystack_[1].value);}
#line 714 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 13: // functionDef: FUNCTION IDENT error
#line 146 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                           {yyxpt(_("argument list or statement list after identifier '") + yystack_[1].value.v.p->text.c_str() + "'",yystack_[1].value);}
#line 720 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 14: // functionDef: FUNCTION IDENT '(' error
#line 147 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                {yyxpt(_("(possibly empty) argument list after '('"),yystack_[1].value);}
#line 726 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 15: // functionDef: FUNCTION IDENT '(' argumentList ')' error
#line 148 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                {yyxpt(_("statement list after ')'"),yystack_[1].value);}
#line 732 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 16: // functionDef: FUNCTION returnDeclaration error
#line 149 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                        {yyxpt(_("function name for function declared"),yystack_[2].value);}
#line 738 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 17: // functionDef: FUNCTION returnDeclaration IDENT error
#line 150 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                             {yyxpt(_("argument list or statement list following function name :") + yystack_[1].value.v.p->text.c_str(), yystack_[1].value);}
#line 744 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 18: // functionDef: FUNCTION returnDeclaration IDENT '(' error
#line 151 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                  {yyxpt(_("(possibly empty) argument list after '('"),yystack_[1].value);}
#line 750 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 19: // functionDef: FUNCTION returnDeclaration IDENT '(' argumentList ')' error
#line 152 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                                  {yyxpt(_("statement list after ')'"),yystack_[1].value);}
#line 756 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 22: // returnDeclaration: VARARGOUT '='
#line 161 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 762 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 23: // returnDeclaration: IDENT '='
#line 162 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 768 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 24: // returnDeclaration: '[' argumentList ']' '='
#line 163 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                             {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 774 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 25: // returnDeclaration: '[' ']' '='
#line 164 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yylhs.value.v.p = nullptr;}
#line 780 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 26: // returnDeclaration: IDENT error
#line 165 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yyxpt(_("an '=' symbol after identifier in return declaration"),yystack_[1].value);}
#line 786 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 27: // returnDeclaration: '[' error
#line 166 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yyxpt(_("a valid list of return arguments in return declaration"),yystack_[1].value);}
#line 792 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 28: // returnDeclaration: '[' argumentList error
#line 167 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                           {yyxpt(_("matching ']' in return declaration for '['"),yystack_[2].value);}
#line 798 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 29: // returnDeclaration: '[' argumentList ']' error
#line 168 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                               {yyxpt(_("an '=' symbol after return declaration"),yystack_[1].value);}
#line 804 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 30: // argumentList: argument
#line 172 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
           {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 810 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 31: // argumentList: argumentList columnSep argument
#line 173 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                    {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 816 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 33: // statementList: statement
#line 181 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
            {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BLOCK,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 822 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 34: // statementList: statementList statement
#line 182 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                          {yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 828 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 35: // statement: statementType ENDQSTMNT
#line 186 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                          {
      yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_QSTATEMENT,NULL,yystack_[0].value.v.i);
      yylhs.value.v.p->down = yystack_[1].value.v.p;
  }
#line 837 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 36: // statement: statementType ENDSTMNT
#line 190 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                           {
     yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yystack_[0].value.v.i);
     yylhs.value.v.p->down = yystack_[1].value.v.p;
  }
#line 846 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 37: // statement: statementType columnSep
#line 194 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                            {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yystack_[0].value.v.i);
    yylhs.value.v.p->down = yystack_[1].value.v.p;
  }
#line 855 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 40: // statementType: %empty
#line 203 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
    {yylhs.value.v.p = AbstractSyntaxTree::createNode(null_node,"",-1);}
#line 861 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 54: // functionTerminatorStatement: ENDFUNCTION
#line 220 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yylhs.value.v.p = AbstractSyntaxTree::createNode(reserved_node, NLS_KEYWORD_RETURN, yystack_[0].value.v.i);}
#line 867 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 55: // specialSyntaxStatement: SPECIALCALL
#line 223 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 873 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 56: // specialSyntaxStatement: IDENT NUMERIC
#line 224 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 879 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 57: // specialSyntaxStatement: STRING STRING
#line 225 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 885 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 58: // specialSyntaxStatement: CHARACTER CHARACTER
#line 226 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 891 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 59: // specialSyntaxStatement: IDENT STRING
#line 227 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                 {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 897 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 60: // specialSyntaxStatement: IDENT CHARACTER
#line 228 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 903 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 61: // specialSyntaxStatement: IDENT IDENT
#line 229 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext()); }
#line 909 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 62: // specialSyntaxStatement: specialSyntaxStatement STRING
#line 230 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                  {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 915 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 63: // specialSyntaxStatement: specialSyntaxStatement CHARACTER
#line 231 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                     {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 921 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 64: // specialSyntaxStatement: specialSyntaxStatement IDENT
#line 232 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                 {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 927 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 65: // specialSyntaxStatement: specialSyntaxStatement NUMERIC
#line 233 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                   {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 933 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 70: // tryStatement: TRY statementList optionalCatch END
#line 252 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
  {
    yylhs.value.v.p = yystack_[3].value.v.p;
    yylhs.value.v.p->addChild(yystack_[2].value.v.p);
    if (yystack_[1].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 943 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 71: // tryStatement: TRY statementList optionalCatch error
#line 258 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
  {yyxpt(_("matching 'end' to 'try' clause from line ") + decodeline(yystack_[3].value),yystack_[0].value);}
#line 949 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 72: // optionalCatch: CATCH IDENT newLine statementList
#line 262 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                    {
    yylhs.value.v.p = yystack_[2].value.v.p;
    yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 958 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 73: // optionalCatch: CATCH statementList
#line 266 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 964 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 74: // optionalCatch: %empty
#line 267 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
    {yylhs.value.v.p = nullptr;}
#line 970 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 75: // switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause END
#line 271 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                                  {
    yylhs.value.v.p = yystack_[5].value.v.p;
    yylhs.value.v.p->addChild(yystack_[4].value.v.p);
    if (yystack_[2].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[2].value.v.p);
    if (yystack_[1].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 981 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 76: // switchStatement: SWITCH expr optionalEndStatement caseBlock otherwiseClause error
#line 277 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                                      {
    yyxpt(_("matching 'end' to 'switch' clause from line ") + decodeline(yystack_[5].value),yystack_[0].value);
  }
#line 989 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 83: // caseBlock: %empty
#line 296 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
  {yylhs.value.v.p = nullptr;}
#line 995 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 85: // caseList: caseStatement
#line 301 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_CASEBLOCK,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());
  }
#line 1003 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 86: // caseList: caseList caseStatement
#line 304 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                           {
    yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1011 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 87: // caseStatement: CASE expr optionalEndStatement statementList
#line 310 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                               {
    yylhs.value.v.p = yystack_[3].value.v.p; yylhs.value.v.p->addChild(yystack_[2].value.v.p); yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1019 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 88: // otherwiseClause: OTHERWISE statementList
#line 316 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                          {
    yylhs.value.v.p = yystack_[0].value.v.p;
  }
#line 1027 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 89: // otherwiseClause: %empty
#line 319 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
    {
    yylhs.value.v.p = nullptr;
  }
#line 1035 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 90: // forStatement: FOR forIndexExpression optionalEndStatement END
#line 325 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                  {
   yylhs.value.v.p = nullptr;
  }
#line 1043 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 91: // forStatement: FOR forIndexExpression optionalEndStatement statementList END
#line 328 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                                  {
    yylhs.value.v.p = yystack_[4].value.v.p;
    yylhs.value.v.p->addChild(yystack_[3].value.v.p);
    yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1053 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 92: // forStatement: FOR forIndexExpression optionalEndStatement statementList error
#line 334 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
           {yyxpt(_("'end' to match 'for' statement from line ") + decodeline(yystack_[4].value),yystack_[0].value);}
#line 1059 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 93: // forIndexExpression: '(' IDENT '=' expr ')'
#line 338 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                         {yylhs.value.v.p = yystack_[3].value.v.p; yylhs.value.v.p->addChild(yystack_[1].value.v.p);}
#line 1065 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 94: // forIndexExpression: IDENT '=' expr
#line 339 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1071 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 95: // forIndexExpression: IDENT
#line 340 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
          {yylhs.value.v.p = yystack_[0].value.v.p;
        yylhs.value.v.p->addChild(AbstractSyntaxTree::createNode(OP_RHS, AbstractSyntaxTree::createNode(id_node,yystack_[0].value.v.p->text.c_str(), yystack_[0].value.v.p->getContext()),yystack_[0].value.v.p->getContext())); }
#line 1078 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 96: // forIndexExpression: '(' IDENT '=' expr error
#line 342 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                             {yyxpt(_("matching right parenthesis"),yystack_[4].value);}
#line 1084 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 97: // forIndexExpression: '(' IDENT '=' error
#line 343 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yyxpt(_("indexing expression"),yystack_[1].value);}
#line 1090 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 98: // forIndexExpression: '(' IDENT error
#line 344 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("equals operator after loop index"),yystack_[1].value);}
#line 1096 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 99: // forIndexExpression: '(' error
#line 345 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yyxpt(_("identifier that is the loop variable"),yystack_[1].value);}
#line 1102 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 100: // forIndexExpression: IDENT '=' error
#line 346 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("indexing expression"),yystack_[1].value);}
#line 1108 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 101: // forIndexExpression: error
#line 347 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
          {yyxpt(_("identifier or assignment (id = expr) after 'for' "));}
#line 1114 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 102: // whileStatement: WHILE expr optionalEndStatement END
#line 351 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                      {
    yylhs.value.v.p = nullptr;
  }
#line 1122 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 103: // whileStatement: WHILE expr optionalEndStatement statementList END
#line 354 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                      {
    yylhs.value.v.p = yystack_[4].value.v.p;
    yylhs.value.v.p->addChild(yystack_[3].value.v.p);
    yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1132 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 104: // whileStatement: WHILE error
#line 359 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yyxpt(_("test expression after 'while'"),yystack_[1].value);}
#line 1138 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 105: // whileStatement: WHILE expr optionalEndStatement statementList error
#line 360 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                        {yyxpt(_("'end' to match 'while' statement from line ") + decodeline(yystack_[4].value),yystack_[0].value);}
#line 1144 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 106: // ifStatement: IF conditionedStatement elseIfBlock elseStatement END
#line 364 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                        {
    yylhs.value.v.p = yystack_[4].value.v.p;
    yylhs.value.v.p->addChild(yystack_[3].value.v.p);
    if (yystack_[2].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[2].value.v.p);
    if (yystack_[1].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1155 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 107: // ifStatement: IF error
#line 370 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
             {yyxpt(_("condition expression for 'if'"),yystack_[1].value);}
#line 1161 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 108: // ifStatement: IF conditionedStatement elseIfBlock elseStatement error
#line 371 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                            {yyxpt(_("'end' to match 'if' statement from line ") + decodeline(yystack_[4].value),yystack_[0].value);}
#line 1167 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 109: // conditionedStatement: expr optionalEndStatement statementList
#line 375 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                          {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[2].value.v.p->getContext());
  }
#line 1175 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 110: // conditionedStatement: expr optionalEndStatement
#line 378 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                              {
	  yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yystack_[1].value.v.p,yystack_[1].value.v.p->getContext());
	}
#line 1183 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 111: // conditionedStatement: expr error
#line 381 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
               {yyxpt("valid list of statements after condition",yystack_[0].value);}
#line 1189 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 112: // elseIfBlock: %empty
#line 385 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yylhs.value.v.p = nullptr;}
#line 1195 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 114: // elseIfStatementList: elseIfStatement
#line 390 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ELSEIFBLOCK,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());
  }
#line 1203 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 115: // elseIfStatementList: elseIfStatementList elseIfStatement
#line 393 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                        {
    yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1211 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 116: // elseIfStatement: ELSEIF conditionedStatement
#line 399 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                              {
    yylhs.value.v.p = yystack_[0].value.v.p;
  }
#line 1219 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 117: // elseIfStatement: ELSEIF error
#line 402 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                 {yyxpt(_("test condition for 'elseif' clause"),yystack_[1].value);}
#line 1225 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 118: // elseStatement: ELSE statementList
#line 406 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                     {
    yylhs.value.v.p = yystack_[0].value.v.p;
  }
#line 1233 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 119: // elseStatement: %empty
#line 409 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
               {yylhs.value.v.p = nullptr;}
#line 1239 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 120: // elseStatement: ELSE error
#line 410 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
               {yyxpt(_("statement list for 'else' clause"),yystack_[1].value);}
#line 1245 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 121: // assignmentStatement: symbRefList '=' expr
#line 414 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ASSIGN,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1251 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 122: // assignmentStatement: symbRefList '=' error
#line 415 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                          {yyxpt(_("expression in assignment"),yystack_[1].value);}
#line 1257 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 123: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' ')'
#line 419 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                        {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yystack_[5].value.v.p,yystack_[2].value.v.p,yystack_[6].value.v.i);
  }
#line 1265 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 124: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList ')'
#line 422 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                    {
    yystack_[3].value.v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yystack_[1].value.v.p,yystack_[2].value.v.i));
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yystack_[6].value.v.p,yystack_[3].value.v.p,yystack_[7].value.v.i);
  }
#line 1274 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 125: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList '}'
#line 426 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                    {
    yystack_[3].value.v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,yystack_[1].value.v.p,yystack_[2].value.v.i));
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yystack_[6].value.v.p,yystack_[3].value.v.p,yystack_[7].value.v.i);
  }
#line 1283 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 126: // multiFunctionCall: '[' matrixDef ']' '=' IDENT
#line 430 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                 {
    yystack_[0].value.v.p->addChild(AbstractSyntaxTree::createNode(OP_PARENS,NULL,-1));
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_MULTICALL,yystack_[3].value.v.p,yystack_[0].value.v.p,yystack_[4].value.v.i);
  }
#line 1292 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 127: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' indexList error
#line 434 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                      {yyxpt(_("matching right bracket"), yystack_[2].value);}
#line 1298 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 128: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' indexList error
#line 435 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                      {yyxpt(_("matching right parenthesis"), yystack_[2].value);}
#line 1304 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 129: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '(' error
#line 436 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                            {yyxpt(_("indexing list"), yystack_[1].value);}
#line 1310 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 130: // multiFunctionCall: '[' matrixDef ']' '=' IDENT '{' error
#line 437 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                            {yyxpt(_("indexing list"), yystack_[1].value);}
#line 1316 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 131: // multiFunctionCall: '[' matrixDef ']' '=' IDENT error
#line 438 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                       {yyxpt(_("left parenthesis"),yystack_[1].value);}
#line 1322 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 132: // multiFunctionCall: '[' matrixDef ']' '=' error
#line 439 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                {yyxpt("identifier",yystack_[1].value);}
#line 1328 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 133: // expr: expr ':' expr
#line 443 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_COLON,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1334 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 134: // expr: expr ':' error
#line 444 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after ':'"), yystack_[1].value);}
#line 1340 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 136: // expr: expr '+' expr
#line 446 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_PLUS,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1346 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 137: // expr: expr '+' error
#line 447 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '+'"), yystack_[1].value);}
#line 1352 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 138: // expr: expr '-' expr
#line 448 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SUBTRACT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1358 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 139: // expr: expr '-' error
#line 449 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '-'"), yystack_[1].value);}
#line 1364 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 140: // expr: expr '*' expr
#line 450 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_TIMES,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1370 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 141: // expr: expr '*' error
#line 451 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '*'"),yystack_[1].value);}
#line 1376 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 142: // expr: expr '/' expr
#line 452 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1382 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 143: // expr: expr '/' error
#line 453 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '/'"),yystack_[1].value);}
#line 1388 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 144: // expr: expr '\\' expr
#line 454 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_LDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1394 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 145: // expr: expr '\\' error
#line 455 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("an expression after '\\'"),yystack_[1].value);}
#line 1400 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 146: // expr: expr '|' expr
#line 456 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_OR,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1406 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 147: // expr: expr '|' error
#line 457 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '|'"),yystack_[1].value);}
#line 1412 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 148: // expr: expr '&' expr
#line 458 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_AND,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1418 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 149: // expr: expr '&' error
#line 459 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '&'"),yystack_[1].value);}
#line 1424 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 150: // expr: expr SOR expr
#line 460 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SOR,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1430 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 151: // expr: expr SOR error
#line 461 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '||'"),yystack_[1].value);}
#line 1436 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 152: // expr: expr SAND expr
#line 462 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SAND,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1442 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 153: // expr: expr SAND error
#line 463 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("an expression after '&&'"),yystack_[1].value);}
#line 1448 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 154: // expr: expr '<' expr
#line 464 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_LT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1454 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 155: // expr: expr '<' error
#line 465 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '<'"),yystack_[1].value);}
#line 1460 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 156: // expr: expr LE expr
#line 466 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_LEQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1466 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 157: // expr: expr LE error
#line 467 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '<='"),yystack_[1].value);}
#line 1472 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 158: // expr: expr '>' expr
#line 468 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_GT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1478 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 159: // expr: expr '>' error
#line 469 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '>'"),yystack_[1].value);}
#line 1484 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 160: // expr: expr GE expr
#line 470 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_GEQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1490 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 161: // expr: expr GE error
#line 471 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '>='"),yystack_[1].value);}
#line 1496 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 162: // expr: expr EQ expr
#line 472 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1502 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 163: // expr: expr EQ error
#line 473 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '=='"),yystack_[1].value);}
#line 1508 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 164: // expr: expr NE expr
#line 474 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_NEQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1514 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 165: // expr: expr NE error
#line 475 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yyxpt(_("an expression after '~='"),yystack_[1].value);}
#line 1520 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 166: // expr: expr DOTTIMES expr
#line 476 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_TIMES,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1526 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 167: // expr: expr DOTTIMES error
#line 477 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yyxpt(_("an expression after '.*'"), yystack_[1].value);}
#line 1532 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 168: // expr: expr DOTRDIV expr
#line 478 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_RDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1538 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 169: // expr: expr DOTRDIV error
#line 479 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yyxpt(_("an expression after './'"),yystack_[1].value);}
#line 1544 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 170: // expr: expr DOTLDIV expr
#line 480 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                      {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_LDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1550 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 171: // expr: expr DOTLDIV error
#line 481 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yyxpt(_("an expression after '.\\'"),yystack_[1].value);}
#line 1556 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 172: // expr: '-' expr
#line 482 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_UMINUS,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1562 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 173: // expr: '+' expr
#line 483 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_UPLUS, yystack_[0].value.v.p, yystack_[1].value.v.i);}
#line 1568 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 174: // expr: '~' expr
#line 484 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_NOT,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1574 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 175: // expr: '~' error
#line 485 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yyxpt(_("an expression after logical not"),yystack_[1].value);}
#line 1580 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 176: // expr: expr '^' expr
#line 486 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                   {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_MPOWER,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1586 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 177: // expr: expr '^' error
#line 487 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("an expression after '^'"),yystack_[1].value);}
#line 1592 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 178: // expr: expr DOTPOWER expr
#line 488 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_POWER,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1598 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 179: // expr: expr DOTPOWER error
#line 489 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yyxpt(_("an expression after '.^'"),yystack_[1].value);}
#line 1604 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 180: // expr: expr '\''
#line 490 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_TRANSPOSE,yystack_[1].value.v.p,yystack_[0].value.v.i);}
#line 1610 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 181: // expr: expr DOTTRANSPOSE
#line 491 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_TRANSPOSE,yystack_[1].value.v.p,yystack_[0].value.v.i);}
#line 1616 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 182: // expr: parenExpr postfixRefList
#line 492 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                             {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_POSTFIX,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 1622 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 183: // expr: postfixableLiteral postfixRefList
#line 493 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                      {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_POSTFIX,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 1628 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 185: // anonymousFunction: '@' IDENT
#line 498 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
            {yylhs.value.v.p =  AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_NAMED, yystack_[0].value.v.p, yystack_[0].value.v.i);}
#line 1634 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 186: // anonymousFunction: '@' '(' argumentList ')' expr
#line 499 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yystack_[2].value.v.p,yystack_[0].value.v.p, yystack_[2].value.v.p->getContext());}
#line 1640 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 187: // anonymousFunction: '@' '(' ')' expr
#line 500 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                     {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yystack_[0].value.v.p, yystack_[0].value.v.p->getContext());}
#line 1646 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 188: // anonymousFunction: '@' error
#line 501 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yyxpt(_("function name or parameter list after '@'"), yystack_[1].value);}
#line 1652 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 189: // anonymousFunction: '@' '(' error
#line 502 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yyxpt(_("argument list or closing parenthesis after '('"), yystack_[1].value);}
#line 1658 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 190: // anonymousFunction: '@' '(' argumentList ')' error
#line 503 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                   {yyxpt(_("expression for anonymous function body after ')'"), yystack_[1].value);}
#line 1664 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 191: // anonymousFunction: '@' '(' ')' error
#line 504 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                      {yyxpt(_("expression for anonymous function body after ')'"), yystack_[1].value);}
#line 1670 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 195: // terminal: MAGICEND
#line 510 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
             {yylhs.value.v.p = AbstractSyntaxTree::createNode(reserved_node, NLS_KEYWORD_END, yystack_[0].value.v.i);}
#line 1676 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 197: // terminal: symbRefList
#line 512 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RHS,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 1682 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 198: // terminal: '[' matrixDef ']'
#line 513 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                      {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1688 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 199: // terminal: '[' error
#line 514 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yyxpt(_("a matrix definition followed by a right bracket"),yystack_[1].value);}
#line 1694 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 200: // terminal: '[' rowSeperator matrixDef ']'
#line 515 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1700 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 201: // terminal: '[' matrixDef rowSeperator ']'
#line 516 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1706 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 202: // terminal: '[' rowSeperator matrixDef rowSeperator ']'
#line 517 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1712 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 203: // terminal: '[' ']'
#line 518 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yystack_[1].value.v.i);}
#line 1718 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 204: // terminal: '{' cellDef '}'
#line 519 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1724 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 205: // terminal: '{' rowSeperator cellDef '}'
#line 520 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1730 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 206: // terminal: '{' cellDef rowSeperator '}'
#line 521 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1736 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 207: // terminal: '{' rowSeperator cellDef rowSeperator '}'
#line 522 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                              {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1742 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 208: // terminal: '{' '}'
#line 523 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yystack_[1].value.v.i);}
#line 1748 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 209: // terminal: '{' error
#line 524 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("a cell-array definition followed by a right brace"),yystack_[1].value);}
#line 1754 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 212: // postfixableLiteral: '[' matrixDef ']'
#line 530 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                      {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1760 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 213: // postfixableLiteral: '[' rowSeperator matrixDef ']'
#line 531 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1766 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 214: // postfixableLiteral: '[' matrixDef rowSeperator ']'
#line 532 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1772 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 215: // postfixableLiteral: '[' rowSeperator matrixDef rowSeperator ']'
#line 533 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                                {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1778 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 216: // postfixableLiteral: '[' ']'
#line 534 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yystack_[1].value.v.i);}
#line 1784 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 217: // postfixableLiteral: '{' cellDef '}'
#line 535 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1790 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 218: // postfixableLiteral: '{' rowSeperator cellDef '}'
#line 536 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1796 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 219: // postfixableLiteral: '{' cellDef rowSeperator '}'
#line 537 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1802 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 220: // postfixableLiteral: '{' rowSeperator cellDef rowSeperator '}'
#line 538 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                              {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1808 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 221: // postfixableLiteral: '{' '}'
#line 539 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yystack_[1].value.v.i);}
#line 1814 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 223: // symbRefList: symbRefList symbRef
#line 544 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1820 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 225: // postfixRefList: postfixRefList symbRef
#line 549 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                           {yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addPeer(yystack_[0].value.v.p);}
#line 1826 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 226: // symbRef: '(' indexList ')'
#line 553 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_PARENS,yystack_[1].value.v.p,yystack_[2].value.v.i); }
#line 1832 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 227: // symbRef: '(' ')'
#line 554 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
            {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_PARENS,NULL,yystack_[1].value.v.i); }
#line 1838 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 228: // symbRef: '(' indexList error
#line 555 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                              {yyxpt(_("matching right parenthesis"),yystack_[2].value);}
#line 1844 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 229: // symbRef: '{' indexList '}'
#line 556 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                      {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yystack_[1].value.v.p,yystack_[2].value.v.i); }
#line 1850 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 230: // symbRef: '{' indexList error
#line 557 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                              {yyxpt(_("matching right brace"),yystack_[2].value);}
#line 1856 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 231: // symbRef: '.' IDENT
#line 558 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT,yystack_[0].value.v.p,yystack_[1].value.v.i); }
#line 1862 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 232: // symbRef: '.' '(' expr ')'
#line 559 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                           {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOTDYN,yystack_[1].value.v.p,yystack_[3].value.v.i);}
#line 1868 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 234: // indexElement: ':'
#line 564 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
        {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ALL,NULL,yystack_[0].value.v.i);}
#line 1874 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 235: // indexElement: '/' IDENT '=' expr
#line 565 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                       {yyxpt(_("slash keyword arguments are not supported"),yystack_[3].value);}
#line 1880 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 236: // indexElement: '/' IDENT '=' error
#line 566 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                        {yyxpt(_("slash keyword arguments are not supported"),yystack_[3].value);}
#line 1886 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 237: // indexElement: '/' IDENT
#line 567 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yyxpt(_("slash keyword arguments are not supported"),yystack_[1].value);}
#line 1892 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 238: // indexElement: '/' error
#line 568 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
              {yyxpt(_("slash keyword arguments are not supported"),yystack_[1].value);}
#line 1898 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 240: // indexList: indexList columnSep indexElement
#line 573 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                     {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addPeer(yystack_[0].value.v.p);}
#line 1904 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 241: // cellDef: rowDef
#line 577 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
         {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 1910 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 242: // cellDef: cellDef rowSeperator rowDef
#line 578 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1916 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 243: // matrixDef: rowDef
#line 582 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
         {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BRACKETS,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 1922 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 244: // matrixDef: matrixDef rowSeperator rowDef
#line 583 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                                  {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1928 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 248: // rowDef: expr
#line 596 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SEMICOLON,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 1934 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 249: // rowDef: rowDef columnSep expr
#line 597 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                          {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1940 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 250: // parenExpr: '(' expr ')'
#line 601 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
               {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1946 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 251: // parenExpr: '(' expr error
#line 602 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
                    {yyxpt(_("a right parenthesis after expression to match this one"),yystack_[2].value);}
#line 1952 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;

  case 252: // parenExpr: '(' error
#line 603 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
               {yyxpt(_("an expression after left parenthesis"),yystack_[1].value);}
#line 1958 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"
    break;


#line 1962 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        std::string msg = YY_("syntax error");
        error (YY_MOVE (msg));
      }


    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.kind () == symbol_kind::S_YYEOF)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    // Pop stack until we find a state that shifts the error token.
    for (;;)
      {
        yyn = yypact_[+yystack_[0].state];
        if (!yy_pact_value_is_default_ (yyn))
          {
            yyn += symbol_kind::S_YYerror;
            if (0 <= yyn && yyn <= yylast_
                && yycheck_[yyn] == symbol_kind::S_YYerror)
              {
                yyn = yytable_[yyn];
                if (0 < yyn)
                  break;
              }
          }

        // Pop the current state because it cannot handle the error token.
        if (yystack_.size () == 1)
          YYABORT;

        yy_destroy_ ("Error: popping", yystack_[0]);
        yypop_ ();
        YY_STACK_PRINT ();
      }
    {
      stack_symbol_type error_token;


      // Shift the error token.
      error_token.state = state_type (yyn);
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
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


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    YY_STACK_PRINT ();
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
  NelSonParser::error (const syntax_error& yyexc)
  {
    error (yyexc.what ());
  }

#if YYDEBUG || 0
  const char *
  NelSonParser::symbol_name (symbol_kind_type yysymbol)
  {
    return yytname_[yysymbol];
  }
#endif // #if YYDEBUG || 0









  const short NelSonParser::yypact_ninf_ = -135;

  const short NelSonParser::yytable_ninf_ = -222;

  const short
  NelSonParser::yypact_[] =
  {
     360,  -135,   196,  -135,   148,   181,  -135,  1437,    47,   190,
    -135,  -135,  1517,  2715,  -135,  2460,  -135,  -135,  -135,  -135,
    2715,  2715,  1538,   539,  1404,  1542,   195,    54,  -135,   -14,
    1018,  -135,    23,  -135,   217,  -135,  -135,  -135,  -135,  -135,
    -135,  -135,  -135,  -135,  -135,  -135,  2876,  -135,  -135,     2,
     157,     2,  -135,  -135,  -135,  -135,  -135,  -135,  -135,  -135,
     227,   230,   539,    39,   567,     2,  -135,   106,   -28,    42,
      81,  -135,    -2,   153,    34,  -135,  2293,  2293,  1073,   311,
     311,  -135,  2746,  -135,  -135,  -135,   242,  2876,   170,  2715,
     -29,  -135,   245,    53,  2715,   -29,  -135,   311,  -135,  -135,
      48,  -135,  -135,  -135,  -135,  -135,  -135,  -135,  -135,  -135,
    -135,  -135,  1611,  1615,  1636,  1640,  1709,  1713,  1734,  -135,
    1738,  1807,  1811,  1832,  1836,  1905,  1909,  1930,  1934,  2003,
    2007,  2028,  2032,  2101,  -135,  2544,  2573,     6,     2,  -135,
    2105,  -135,     2,   173,  2126,    66,    39,  -135,  -135,  -135,
    -135,  -135,  1140,  -135,   204,  -135,  -135,   145,  -135,  2460,
    -135,  -135,  -135,    14,     9,  -135,  -135,   180,  2130,  -135,
      40,  2350,  2405,    72,  2515,    35,  -135,  -135,   176,  2605,
     197,  2715,   289,  2628,   134,  -135,  2199,   -15,  -135,  3025,
    -135,  3025,  -135,  3025,  -135,   311,  -135,   311,  -135,   311,
    -135,   311,  -135,  2893,  -135,  2946,  -135,  2961,  -135,  2975,
    -135,  3025,  -135,  3025,  -135,  3025,  -135,  3035,  -135,   723,
    -135,   723,  -135,   311,  -135,   311,  -135,   311,  -135,   311,
    -135,   253,  -135,  2876,  -135,    10,    18,  -135,  2715,  -135,
    -135,  2876,   308,  -135,  -135,   630,    43,  -135,  1140,  -135,
     220,    -4,   688,  -135,  -135,    73,    76,  -135,   152,  2460,
    -135,  2876,  -135,  2203,  -135,   429,  -135,   498,  2715,   114,
      72,  -135,   389,  1198,  -135,  -135,   259,   342,   -29,   343,
    2660,  2876,   355,   -29,   397,  2683,  -135,  2876,  2224,  -135,
      88,  -135,  -135,  2573,  -135,  -135,  2821,  -135,  1253,  -135,
    -135,  2460,   223,  -135,  -135,  -135,  -135,   220,    86,   743,
    -135,  2801,  -135,  -135,  -135,  -135,  2293,  2460,    51,  -135,
    2460,  -135,    32,   398,   411,  -135,  2876,  2228,  -135,  -135,
     798,  -135,  2460,  2460,   270,  -135,  -135,  2460,  1308,  -135,
    -135,  1363,  -135,  1433,  1462,  -135,  2876,   853,   908,  -135,
    2460,  1140,  -135,  -135,    16,  -135,    26,   963,  -135,  -135,
    -135,  -135
  };

  const unsigned char
  NelSonParser::yydefact_[] =
  {
       0,     5,   222,   192,   193,   194,    55,     0,     0,     0,
      69,   195,     0,     0,    68,    40,    67,    66,    52,    54,
       0,     0,     0,     0,     0,     0,     0,     0,    20,     3,
      40,    33,     0,    53,    51,    50,    49,    44,    43,    48,
      47,    42,    45,    46,    38,    41,    39,   196,   135,     0,
     197,   184,    61,    56,    60,    59,    58,    57,   107,   222,
     193,   194,     0,   112,     0,   197,    12,     0,     0,     0,
       0,   101,    95,     0,     0,   104,     0,     0,    40,   173,
     172,   252,     0,   199,   246,   245,   203,   248,     0,     0,
     243,   209,   208,     0,     0,   241,   175,   174,   188,   185,
       0,     1,    21,    34,    35,    36,   247,    37,    64,    65,
      63,    62,     0,     0,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,     0,     0,     0,   183,   224,
       0,   223,   182,     0,     0,   119,   113,   114,   111,    79,
      78,    80,   110,    77,    13,    82,    81,     0,    23,    40,
      22,    27,    32,     0,     0,    30,    16,     0,     0,    99,
       0,    40,    40,    83,    40,     0,   251,   250,   198,     0,
       0,     0,   204,     0,     0,   189,     0,     0,   157,   156,
     161,   160,   163,   162,   167,   166,   169,   168,   171,   170,
     179,   178,   151,   150,   153,   152,   147,   146,   149,   148,
     155,   154,   159,   158,   165,   164,   134,   133,   137,   136,
     139,   138,   141,   140,   143,   142,   145,   144,   177,   176,
     234,     0,   227,   233,   239,     0,     0,   231,     0,   225,
     122,   121,   198,   117,   116,     0,     0,   115,   109,    14,
       0,     0,    40,    25,    28,     0,     0,    17,     0,    40,
     100,    94,    98,     0,    90,     0,   102,     0,     0,    89,
      84,    85,   222,    40,    71,    70,     0,   201,   244,   200,
       0,   249,   206,   242,   205,     0,   191,   187,     0,   238,
     237,   228,   226,     0,   230,   229,     0,   120,    40,   108,
     106,    40,     0,    29,    24,    31,    18,     0,     0,    40,
      97,     0,    92,    91,   105,   103,     0,    40,     0,    86,
      40,   132,     0,   202,   207,   190,   186,     0,   240,   232,
      40,    15,    40,    40,     0,    96,    93,    40,    40,    76,
      75,    40,   131,     0,     0,   236,   235,    40,    40,    19,
      40,    87,   129,   123,     0,   130,     0,    40,   128,   124,
     127,   125
  };

  const short
  NelSonParser::yypgoto_[] =
  {
    -135,  -135,   129,  -135,  -135,   -93,   -86,   -10,     5,  -135,
    -135,  -135,  -135,  -135,  -135,  -135,  -135,  -135,  -135,   -73,
    -112,  -135,  -135,   -87,  -135,  -135,  -135,  -135,  -135,    45,
    -135,  -135,    58,  -135,  -135,  -135,     1,  -135,  -135,  -135,
       0,   143,   -34,   -63,  -134,   150,   -42,     8,   -20,    -6,
    -135
  };

  const short
  NelSonParser::yydefgoto_[] =
  {
       0,    27,    28,    29,    70,   164,   165,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,   175,    40,   152,
     159,   269,   270,   271,   318,    41,    74,    42,    43,    63,
     145,   146,   147,   246,    44,    45,    46,    47,    48,    49,
      65,   138,   139,   234,   235,    93,    88,    89,   153,    90,
      51
  };

  const short
  NelSonParser::yytable_[] =
  {
      50,   171,   236,   172,   173,    78,     8,   187,    64,   237,
     254,   291,   107,    76,    77,    50,   141,   358,    95,   294,
     143,    79,    80,    82,    87,    87,    97,   360,   104,   105,
      50,   141,    94,   342,   160,   103,   274,  -126,  -126,   149,
     150,   262,   106,   161,   299,   162,   288,   180,    66,   185,
      67,   162,   339,   275,   101,   259,   106,   302,    84,    85,
     168,   300,   135,    87,   251,   144,   238,   106,   136,   340,
     181,   292,   137,   255,   303,   181,   253,   359,    50,   162,
     106,   106,   166,   103,   167,   295,    68,   106,    95,   106,
      87,   245,   343,   361,   106,    87,   179,   106,   344,   151,
     268,   183,   263,  -126,   239,   106,   163,   154,   239,   186,
      69,   155,   156,   189,   191,   193,   195,   197,   199,   201,
     182,   203,   205,   207,   209,   211,   213,   215,   217,   219,
     221,   223,   225,   227,   229,   304,   233,   233,   301,    84,
      85,   241,   248,   317,   256,    64,   249,   334,   162,   252,
     327,   179,    50,   306,   169,   162,   170,   106,   102,    50,
     320,   265,   267,    56,   273,   308,   157,   256,   158,   261,
     305,    50,    50,   278,    50,    84,    85,   283,    84,    85,
      87,   257,   281,   319,    87,   155,   156,   287,   280,   244,
     332,    71,   285,    72,   142,   333,    98,    57,    99,    52,
      53,   284,    84,    85,   247,   -26,   250,   -26,  -210,   354,
     356,    54,    55,   307,  -210,   293,   293,   135,  -210,   140,
     108,   109,   350,   136,   331,   155,   156,   137,   155,   156,
     328,   256,   110,   111,   178,   298,  -212,   242,   276,   296,
     258,  -211,  -212,   337,   184,    50,  -212,  -211,    50,   309,
      73,  -211,    50,   103,   289,   100,   290,   103,   181,    50,
     321,   279,   322,   181,   311,    50,     0,    50,     0,   316,
     103,   349,   103,    50,   278,   155,   156,     0,   103,   283,
       0,    87,     0,     0,     0,     0,    87,  -210,   256,   326,
    -211,   330,     0,  -210,   233,     0,  -211,  -210,    50,     0,
    -211,    50,  -216,   103,     0,  -221,     0,   338,  -216,    50,
     341,  -221,  -216,     0,   103,  -221,     0,    50,     0,     0,
      50,     0,   347,   348,   118,   119,     0,   351,   346,     0,
      50,     0,    50,    50,   293,   103,   293,    50,    50,     0,
     357,    50,     0,   103,   233,   233,   103,    50,    50,  -217,
      50,    50,   103,   103,     0,  -217,   103,    50,     0,  -217,
      -4,     1,   103,     2,     3,   -40,   -40,     0,  -212,   133,
     134,     0,     0,     0,  -212,     4,     5,     6,  -212,     7,
       8,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,    52,    53,   155,   156,    16,    17,     0,     0,
      18,    19,  -214,  -213,    54,    55,     0,     0,  -214,  -213,
      20,    21,  -214,  -213,     0,  -219,     0,     0,     0,     0,
      22,  -219,     0,    23,     0,  -219,    24,     0,    25,    26,
     312,   -40,     2,     3,   -40,   -40,     0,     0,     0,     0,
       0,     0,     0,     0,     4,     5,     6,   313,     7,     0,
       9,    10,    11,    12,     0,     0,    13,  -218,  -215,    14,
      15,     0,     0,  -218,  -215,    16,    17,  -218,  -215,    18,
      19,  -220,     0,     0,     0,     0,     0,  -220,     0,    20,
      21,  -220,     0,     0,     0,     0,     0,     0,     0,    22,
       0,     0,    23,     0,     0,    24,     0,    25,    26,   314,
     -40,     2,     3,   -40,   -40,     0,     0,     0,     0,     0,
       0,     0,     0,     4,     5,     6,   315,     7,     0,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
      83,     0,    59,     3,    84,    85,     0,     0,    20,    21,
       0,     0,     0,     0,    60,    61,     0,     0,    22,     0,
       0,    23,    11,     0,    24,     0,    25,    26,   148,   -40,
       0,     0,   149,   150,   112,   113,   114,   115,   116,   117,
     118,   119,     0,     0,     0,     0,     0,     0,     0,    20,
      21,     0,     0,     0,     0,     0,     0,     0,     0,    22,
       0,     0,    62,    86,     0,    24,     0,    25,    26,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,     0,     0,     0,   133,   134,     0,     0,     0,
       0,   297,   151,     2,     3,   -40,   -40,     0,   106,     0,
       0,     0,     0,     0,     0,     4,     5,     6,     0,     7,
       0,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,     0,     0,     0,     0,    16,    17,     0,     0,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,     0,    -9,     0,
      22,     2,     3,    23,     0,     0,    24,     0,    25,    26,
       0,   -40,     0,     4,     5,     6,     0,     7,    -9,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
       0,     0,     0,   115,   116,   117,   118,   119,    20,    21,
       0,     0,     0,    -8,     0,     0,     2,     3,    22,     0,
       0,    23,     0,     0,    24,     0,    25,    26,     4,     5,
       6,     0,     7,    -8,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,   130,   131,   132,     0,    16,
      17,   133,   134,    18,    19,     0,     0,     0,     0,     0,
       0,     0,     0,    20,    21,     0,     0,     0,   -11,     0,
       0,     2,     3,    22,     0,     0,    23,     0,     0,    24,
       0,    25,    26,     4,     5,     6,     0,     7,   -11,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,    -7,     0,     0,     2,     3,    22,     0,
       0,    23,     0,     0,    24,     0,    25,    26,     4,     5,
       6,     0,     7,    -7,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,     0,     0,     0,    16,
      17,     0,     0,    18,    19,     0,     0,     0,     0,     0,
       0,     0,     0,    20,    21,     0,     0,     0,   -10,     0,
       0,     2,     3,    22,     0,     0,    23,     0,     0,    24,
       0,    25,    26,     4,     5,     6,     0,     7,   -10,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,    -6,     0,     0,     2,     3,    22,     0,
       0,    23,     0,     0,    24,     0,    25,    26,     4,     5,
       6,     0,     7,    -6,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,     0,     0,     0,    16,
      17,     0,     0,    18,    19,     0,     0,     0,     0,     0,
       0,     0,     0,    20,    21,     0,     0,     0,    -2,     0,
       0,     2,     3,    22,     0,     0,    23,     0,     0,    24,
       0,    25,    26,     4,     5,     6,     0,     7,     0,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,     0,   -74,     0,     2,     3,    22,     0,
       0,    23,     0,     0,    24,     0,    25,    26,     4,     5,
       6,   -74,     7,     0,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,   174,     0,     0,     0,    16,
      17,     0,     0,    18,    19,     0,     0,     0,     0,     0,
       0,     0,     0,    20,    21,     0,     0,     0,     0,     0,
       0,     0,     0,    22,     0,     0,    23,     0,     0,    24,
       0,    25,    26,     2,     3,   -40,   -40,     0,     0,     0,
       0,     0,     0,     0,     0,     4,     5,     6,     0,     7,
       0,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,     0,     0,     0,     0,    16,    17,     0,     0,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,     0,     0,   -73,
      22,     2,     3,    23,     0,     0,    24,     0,    25,    26,
       0,   -40,     0,     4,     5,     6,   -73,     7,     0,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,     0,  -118,     0,     2,     3,    22,     0,
       0,    23,     0,     0,    24,     0,    25,    26,     4,     5,
       6,  -118,     7,     0,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,     0,     0,     0,    16,
      17,     0,     0,    18,    19,     0,     0,     0,     0,     0,
       0,     0,     0,    20,    21,     0,     0,     0,     0,   -88,
       0,     2,     3,    22,     0,     0,    23,     0,     0,    24,
       0,    25,    26,     4,     5,     6,   -88,     7,     0,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,     0,     0,   -72,     0,     2,     3,    22,     0,
       0,    23,     0,     0,    24,     0,    25,    26,     4,     5,
       6,   -72,     7,     0,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,     0,     0,     0,    16,
      17,     0,     0,    18,    19,    91,     0,    59,     3,    84,
      85,     0,     0,    20,    21,     0,     0,     0,     0,    60,
      61,     0,     0,    22,     0,     0,    23,    11,     0,    24,
       0,    25,    26,     0,   352,     0,    59,     3,    58,     0,
      59,     3,     0,     0,     0,     0,     0,     0,    60,    61,
       0,     0,    60,    61,    20,    21,    11,     0,     0,     0,
      11,     0,     0,   355,    22,    59,     3,    62,     0,     0,
      24,    92,    25,    26,     0,     0,     0,    60,    61,     0,
       0,     0,   230,    20,    21,    11,   231,    20,    21,     0,
       0,     0,     0,    22,   353,     0,    62,    22,     0,    24,
      62,    25,    26,    24,     0,    25,    26,     0,     0,     0,
       0,   230,    20,    21,     0,   231,     0,     0,    75,     0,
      59,     3,    22,     0,     0,    62,     0,     0,    24,     0,
      25,    26,    60,    61,     0,     0,     0,     0,     0,    81,
      11,    59,     3,    96,     0,    59,     3,     0,     0,     0,
       0,     0,     0,    60,    61,     0,     0,    60,    61,     0,
       0,    11,     0,     0,     0,    11,     0,    20,    21,     0,
       0,     0,     0,     0,     0,     0,     0,    22,     0,     0,
      62,     0,     0,    24,     0,    25,    26,     0,    20,    21,
       0,     0,    20,    21,     0,     0,     0,     0,    22,     0,
       0,    62,    22,     0,    24,    62,    25,    26,    24,     0,
      25,    26,   188,     0,    59,     3,   190,     0,    59,     3,
       0,     0,     0,     0,     0,     0,    60,    61,     0,     0,
      60,    61,     0,     0,    11,     0,     0,   192,    11,    59,
       3,   194,     0,    59,     3,     0,     0,     0,     0,     0,
       0,    60,    61,     0,     0,    60,    61,     0,     0,    11,
       0,    20,    21,    11,     0,    20,    21,     0,     0,     0,
       0,    22,     0,     0,    62,    22,     0,    24,    62,    25,
      26,    24,     0,    25,    26,     0,    20,    21,     0,     0,
      20,    21,     0,     0,     0,     0,    22,     0,     0,    62,
      22,     0,    24,    62,    25,    26,    24,     0,    25,    26,
     196,     0,    59,     3,   198,     0,    59,     3,     0,     0,
       0,     0,     0,     0,    60,    61,     0,     0,    60,    61,
       0,     0,    11,     0,     0,   200,    11,    59,     3,   202,
       0,    59,     3,     0,     0,     0,     0,     0,     0,    60,
      61,     0,     0,    60,    61,     0,     0,    11,     0,    20,
      21,    11,     0,    20,    21,     0,     0,     0,     0,    22,
       0,     0,    62,    22,     0,    24,    62,    25,    26,    24,
       0,    25,    26,     0,    20,    21,     0,     0,    20,    21,
       0,     0,     0,     0,    22,     0,     0,    62,    22,     0,
      24,    62,    25,    26,    24,     0,    25,    26,   204,     0,
      59,     3,   206,     0,    59,     3,     0,     0,     0,     0,
       0,     0,    60,    61,     0,     0,    60,    61,     0,     0,
      11,     0,     0,   208,    11,    59,     3,   210,     0,    59,
       3,     0,     0,     0,     0,     0,     0,    60,    61,     0,
       0,    60,    61,     0,     0,    11,     0,    20,    21,    11,
       0,    20,    21,     0,     0,     0,     0,    22,     0,     0,
      62,    22,     0,    24,    62,    25,    26,    24,     0,    25,
      26,     0,    20,    21,     0,     0,    20,    21,     0,     0,
       0,     0,    22,     0,     0,    62,    22,     0,    24,    62,
      25,    26,    24,     0,    25,    26,   212,     0,    59,     3,
     214,     0,    59,     3,     0,     0,     0,     0,     0,     0,
      60,    61,     0,     0,    60,    61,     0,     0,    11,     0,
       0,   216,    11,    59,     3,   218,     0,    59,     3,     0,
       0,     0,     0,     0,     0,    60,    61,     0,     0,    60,
      61,     0,     0,    11,     0,    20,    21,    11,     0,    20,
      21,     0,     0,     0,     0,    22,     0,     0,    62,    22,
       0,    24,    62,    25,    26,    24,     0,    25,    26,     0,
      20,    21,     0,     0,    20,    21,     0,     0,     0,     0,
      22,     0,     0,    62,    22,     0,    24,    62,    25,    26,
      24,     0,    25,    26,   220,     0,    59,     3,   222,     0,
      59,     3,     0,     0,     0,     0,     0,     0,    60,    61,
       0,     0,    60,    61,     0,     0,    11,     0,     0,   224,
      11,    59,     3,   226,     0,    59,     3,     0,     0,     0,
       0,     0,     0,    60,    61,     0,     0,    60,    61,     0,
       0,    11,     0,    20,    21,    11,     0,    20,    21,     0,
       0,     0,     0,    22,     0,     0,    62,    22,     0,    24,
      62,    25,    26,    24,     0,    25,    26,     0,    20,    21,
       0,     0,    20,    21,     0,     0,     0,     0,    22,     0,
       0,    62,    22,     0,    24,    62,    25,    26,    24,     0,
      25,    26,   228,     0,    59,     3,   240,     0,    59,     3,
       0,     0,     0,     0,     0,     0,    60,    61,     0,     0,
      60,    61,     0,     0,    11,     0,     0,   243,    11,    59,
       3,   260,     0,    59,     3,     0,     0,     0,     0,     0,
       0,    60,    61,     0,     0,    60,    61,     0,     0,    11,
       0,    20,    21,    11,     0,    20,    21,     0,     0,     0,
       0,    22,     0,     0,    62,    22,     0,    24,    62,    25,
      26,    24,     0,    25,    26,     0,    20,    21,     0,     0,
      20,    21,     0,     0,     0,     0,    22,     0,     0,    62,
      22,     0,    24,    62,    25,    26,    24,     0,    25,    26,
     286,     0,    59,     3,   310,     0,    59,     3,     0,     0,
       0,     0,     0,     0,    60,    61,     0,     0,    60,    61,
       0,     0,    11,     0,     0,   325,    11,    59,     3,   345,
       0,    59,     3,     0,     0,     0,     0,     0,     0,    60,
      61,     0,     0,    60,    61,     0,     0,    11,     0,    20,
      21,    11,     0,    20,    21,     0,     0,     0,     0,    22,
       0,     0,    62,    22,     0,    24,    62,    25,    26,    24,
       0,    25,    26,     0,    20,    21,     0,     0,    20,    21,
       0,     0,     0,     0,    22,     0,     0,    62,    22,     0,
      24,    62,    25,    26,    24,     0,    25,    26,   149,   150,
     112,   113,   114,   115,   116,   117,   118,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,     0,     0,
       0,   133,   134,     2,     3,     0,     0,     0,   151,     0,
       0,     0,     0,     0,   106,     4,     5,     6,   264,     7,
       0,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,     0,     0,     0,     0,    16,    17,     0,     0,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,     0,     2,     3,
      22,     0,     0,    23,     0,     0,    24,     0,    25,    26,
       4,     5,     6,   266,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,     0,     0,     0,
       0,     0,     0,     0,     0,    20,    21,     0,     0,     0,
       0,     0,     0,     2,     3,    22,     0,     0,    23,     0,
       0,    24,     0,    25,    26,     4,     5,     6,     0,     7,
       0,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,     0,     0,     0,     0,    16,    17,     0,     0,
      18,    19,     0,     0,     0,     0,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,     0,   272,     3,
      22,     0,     0,    23,     0,     0,    24,     0,    25,    26,
       4,     5,     6,     0,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,    59,     3,     0,
       0,    16,    17,     0,     0,    18,    19,     0,     0,    60,
      61,     0,     0,     0,     0,    20,    21,    11,     0,     0,
       0,     0,     0,     0,     0,    22,    59,     3,    23,     0,
       0,    24,     0,    25,    26,     0,     0,     0,    60,    61,
       0,     0,     0,   230,    20,    21,    11,   231,     0,     0,
       0,     0,     0,     0,    22,   232,     0,    62,    59,     3,
      24,     0,    25,    26,     0,     0,     0,     0,     0,     0,
      60,    61,   230,    20,    21,     0,   231,     0,    11,     0,
       0,    59,     3,    22,     0,     0,    62,     0,     0,    24,
       0,    25,    26,    60,    61,     0,     0,     0,     0,     0,
       0,    11,     0,     0,     0,    20,    21,     0,     0,     0,
       0,     0,     0,    59,     3,    22,     0,     0,    62,   277,
       0,    24,     0,    25,    26,    60,    61,     0,    20,    21,
       0,     0,     0,    11,     0,     0,    59,     3,    22,     0,
       0,    62,     0,     0,    24,   282,    25,    26,    60,    61,
       0,     0,     0,     0,     0,     0,    11,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,     0,    59,     3,
      22,     0,     0,    62,   323,     0,    24,     0,    25,    26,
      60,    61,     0,    20,    21,     0,     0,     0,    11,     0,
       0,     0,     0,    22,     0,     0,    62,   176,     0,    24,
     324,    25,    26,   112,   113,   114,   115,   116,   117,   118,
     119,     0,     0,     0,     0,    20,    21,     0,     0,     0,
       0,     0,     0,     0,     0,    22,     0,     0,    62,     0,
       0,    24,     0,    25,    26,     0,     0,     0,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,     0,   335,     0,   133,   134,     0,   177,   112,   113,
     114,   115,   116,   117,   118,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   112,   113,
     114,   115,   116,   117,   118,   119,     0,     0,     0,     0,
       0,     0,     0,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,     0,     0,     0,   133,
     134,     0,   336,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,     0,     0,     0,   133,
     134,     0,   329,   112,   113,   114,   115,   116,   117,   118,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     112,   113,   114,   115,   116,   117,   118,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,     0,     0,     0,   133,   134,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,     0,     0,
       0,   133,   134,   112,   113,   114,   115,   116,   117,   118,
     119,     0,     0,     0,     0,     0,     0,     0,   112,   113,
     114,   115,   116,   117,   118,   119,     0,     0,     0,     0,
       0,     0,   112,   113,   114,   115,   116,   117,   118,   119,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,     0,     0,     0,   133,   134,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,     0,     0,     0,   133,
     134,   124,   125,   126,   127,   128,   129,   130,   131,   132,
       0,     0,     0,   133,   134,   115,   116,   117,   118,   119,
       0,     0,     0,     0,     0,   115,   116,   117,   118,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   127,   128,   129,   130,   131,   132,
       0,     0,     0,   133,   134,   128,   129,   130,   131,   132,
       0,     0,     0,   133,   134
  };

  const short
  NelSonParser::yycheck_[] =
  {
       0,    74,   136,    76,    77,    15,    20,   100,     7,     3,
       1,     1,    32,    12,    13,    15,    50,     1,    24,     1,
      62,    20,    21,    22,    23,    24,    25,     1,     5,     6,
      30,    65,    24,     1,    62,    30,     1,     5,     6,     5,
       6,     1,    71,     1,     1,     3,    61,    89,     1,     1,
       3,     3,     1,    18,     0,   167,    71,    61,     5,     6,
      62,    18,    60,    62,   157,    26,    60,    71,    66,    18,
      90,    61,    70,    64,     1,    95,    62,    61,    78,     3,
      71,    71,     1,    78,     3,    67,    39,    71,    94,    71,
      89,    25,    60,    67,    71,    94,    88,    71,    66,    65,
      28,    93,    62,    71,   138,    71,    64,     1,   142,    61,
      63,     5,     6,   112,   113,   114,   115,   116,   117,   118,
      67,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    62,   135,   136,   250,     5,
       6,   140,   152,    29,   164,   144,     1,    61,     3,   159,
      62,   143,   152,     1,     1,     3,     3,    71,    29,   159,
     272,   171,   172,    15,   174,   258,    60,   187,    62,   168,
     256,   171,   172,   179,   174,     5,     6,   183,     5,     6,
     179,     1,   181,   270,   183,     5,     6,   186,   180,   144,
     302,     1,   184,     3,    51,   307,     1,    16,     3,     3,
       4,    67,     5,     6,   146,     1,    61,     3,    60,   343,
     344,    15,    16,    61,    66,   235,   236,    60,    70,    62,
       3,     4,   334,    66,     1,     5,     6,    70,     5,     6,
     293,   251,    15,    16,    64,   245,    60,    64,    62,   238,
      60,    60,    66,   316,    94,   245,    70,    66,   248,   259,
      60,    70,   252,   248,     1,    60,     3,   252,   278,   259,
       1,    64,     3,   283,   263,   265,    -1,   267,    -1,   268,
     265,     1,   267,   273,   280,     5,     6,    -1,   273,   285,
      -1,   280,    -1,    -1,    -1,    -1,   285,    60,   308,   288,
      60,   301,    -1,    66,   293,    -1,    66,    70,   298,    -1,
      70,   301,    60,   298,    -1,    60,    -1,   317,    66,   309,
     320,    66,    70,    -1,   309,    70,    -1,   317,    -1,    -1,
     320,    -1,   332,   333,    13,    14,    -1,   337,   327,    -1,
     330,    -1,   332,   333,   354,   330,   356,   337,   338,    -1,
     350,   341,    -1,   338,   343,   344,   341,   347,   348,    60,
     350,   351,   347,   348,    -1,    66,   351,   357,    -1,    70,
       0,     1,   357,     3,     4,     5,     6,    -1,    60,    58,
      59,    -1,    -1,    -1,    66,    15,    16,    17,    70,    19,
      20,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,     3,     4,     5,     6,    36,    37,    -1,    -1,
      40,    41,    60,    60,    15,    16,    -1,    -1,    66,    66,
      50,    51,    70,    70,    -1,    60,    -1,    -1,    -1,    -1,
      60,    66,    -1,    63,    -1,    70,    66,    -1,    68,    69,
       1,    71,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    17,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    60,    60,    30,
      31,    -1,    -1,    66,    66,    36,    37,    70,    70,    40,
      41,    60,    -1,    -1,    -1,    -1,    -1,    66,    -1,    50,
      51,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    63,    -1,    -1,    66,    -1,    68,    69,     1,
      71,     3,     4,     5,     6,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    16,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
       1,    -1,     3,     4,     5,     6,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    60,    -1,
      -1,    63,    23,    -1,    66,    -1,    68,    69,     1,    71,
      -1,    -1,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    63,    64,    -1,    66,    -1,    68,    69,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    -1,    -1,    -1,    58,    59,    -1,    -1,    -1,
      -1,     1,    65,     3,     4,     5,     6,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,
      60,     3,     4,    63,    -1,    -1,    66,    -1,    68,    69,
      -1,    71,    -1,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    10,    11,    12,    13,    14,    50,    51,
      -1,    -1,    -1,     0,    -1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    -1,    68,    69,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    52,    53,    54,    -1,    36,
      37,    58,    59,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,     0,    -1,
      -1,     3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,
      -1,    68,    69,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,     0,    -1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    -1,    68,    69,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,     0,    -1,
      -1,     3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,
      -1,    68,    69,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,     0,    -1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    -1,    68,    69,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,     0,    -1,
      -1,     3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,
      -1,    68,    69,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    -1,    68,    69,    15,    16,
      17,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    32,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    66,
      -1,    68,    69,     3,     4,     5,     6,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      60,     3,     4,    63,    -1,    -1,    66,    -1,    68,    69,
      -1,    71,    -1,    15,    16,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    -1,    68,    69,    15,    16,
      17,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,     1,
      -1,     3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,
      -1,    68,    69,    15,    16,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    -1,    68,    69,    15,    16,
      17,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,     1,    -1,     3,     4,     5,
       6,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    15,
      16,    -1,    -1,    60,    -1,    -1,    63,    23,    -1,    66,
      -1,    68,    69,    -1,     1,    -1,     3,     4,     1,    -1,
       3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,
      -1,    -1,    15,    16,    50,    51,    23,    -1,    -1,    -1,
      23,    -1,    -1,     1,    60,     3,     4,    63,    -1,    -1,
      66,    67,    68,    69,    -1,    -1,    -1,    15,    16,    -1,
      -1,    -1,    49,    50,    51,    23,    53,    50,    51,    -1,
      -1,    -1,    -1,    60,    61,    -1,    63,    60,    -1,    66,
      63,    68,    69,    66,    -1,    68,    69,    -1,    -1,    -1,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,     1,    -1,
       3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,    -1,
      68,    69,    15,    16,    -1,    -1,    -1,    -1,    -1,     1,
      23,     3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,
      -1,    23,    -1,    -1,    -1,    23,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,
      63,    -1,    -1,    66,    -1,    68,    69,    -1,    50,    51,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,
      -1,    63,    60,    -1,    66,    63,    68,    69,    66,    -1,
      68,    69,     1,    -1,     3,     4,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,
      15,    16,    -1,    -1,    23,    -1,    -1,     1,    23,     3,
       4,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    16,    -1,    -1,    15,    16,    -1,    -1,    23,
      -1,    50,    51,    23,    -1,    50,    51,    -1,    -1,    -1,
      -1,    60,    -1,    -1,    63,    60,    -1,    66,    63,    68,
      69,    66,    -1,    68,    69,    -1,    50,    51,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
      60,    -1,    66,    63,    68,    69,    66,    -1,    68,    69,
       1,    -1,     3,     4,     1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,
      -1,    -1,    23,    -1,    -1,     1,    23,     3,     4,     1,
      -1,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      16,    -1,    -1,    15,    16,    -1,    -1,    23,    -1,    50,
      51,    23,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    63,    60,    -1,    66,    63,    68,    69,    66,
      -1,    68,    69,    -1,    50,    51,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    60,    -1,
      66,    63,    68,    69,    66,    -1,    68,    69,     1,    -1,
       3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,    -1,
      23,    -1,    -1,     1,    23,     3,     4,     1,    -1,     3,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,
      -1,    15,    16,    -1,    -1,    23,    -1,    50,    51,    23,
      -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,    -1,
      63,    60,    -1,    66,    63,    68,    69,    66,    -1,    68,
      69,    -1,    50,    51,    -1,    -1,    50,    51,    -1,    -1,
      -1,    -1,    60,    -1,    -1,    63,    60,    -1,    66,    63,
      68,    69,    66,    -1,    68,    69,     1,    -1,     3,     4,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    -1,    -1,    15,    16,    -1,    -1,    23,    -1,
      -1,     1,    23,     3,     4,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    15,
      16,    -1,    -1,    23,    -1,    50,    51,    23,    -1,    50,
      51,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    60,
      -1,    66,    63,    68,    69,    66,    -1,    68,    69,    -1,
      50,    51,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,
      60,    -1,    -1,    63,    60,    -1,    66,    63,    68,    69,
      66,    -1,    68,    69,     1,    -1,     3,     4,     1,    -1,
       3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    15,    16,
      -1,    -1,    15,    16,    -1,    -1,    23,    -1,    -1,     1,
      23,     3,     4,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,    -1,
      -1,    23,    -1,    50,    51,    23,    -1,    50,    51,    -1,
      -1,    -1,    -1,    60,    -1,    -1,    63,    60,    -1,    66,
      63,    68,    69,    66,    -1,    68,    69,    -1,    50,    51,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,    -1,
      -1,    63,    60,    -1,    66,    63,    68,    69,    66,    -1,
      68,    69,     1,    -1,     3,     4,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    -1,    -1,
      15,    16,    -1,    -1,    23,    -1,    -1,     1,    23,     3,
       4,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    16,    -1,    -1,    15,    16,    -1,    -1,    23,
      -1,    50,    51,    23,    -1,    50,    51,    -1,    -1,    -1,
      -1,    60,    -1,    -1,    63,    60,    -1,    66,    63,    68,
      69,    66,    -1,    68,    69,    -1,    50,    51,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,
      60,    -1,    66,    63,    68,    69,    66,    -1,    68,    69,
       1,    -1,     3,     4,     1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    15,    16,
      -1,    -1,    23,    -1,    -1,     1,    23,     3,     4,     1,
      -1,     3,     4,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      16,    -1,    -1,    15,    16,    -1,    -1,    23,    -1,    50,
      51,    23,    -1,    50,    51,    -1,    -1,    -1,    -1,    60,
      -1,    -1,    63,    60,    -1,    66,    63,    68,    69,    66,
      -1,    68,    69,    -1,    50,    51,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    60,    -1,
      66,    63,    68,    69,    66,    -1,    68,    69,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    58,    59,     3,     4,    -1,    -1,    -1,    65,    -1,
      -1,    -1,    -1,    -1,    71,    15,    16,    17,    18,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
      60,    -1,    -1,    63,    -1,    -1,    66,    -1,    68,    69,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,    60,    -1,    -1,    63,    -1,
      -1,    66,    -1,    68,    69,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
      60,    -1,    -1,    63,    -1,    -1,    66,    -1,    68,    69,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,     3,     4,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    -1,    -1,    15,
      16,    -1,    -1,    -1,    -1,    50,    51,    23,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    60,     3,     4,    63,    -1,
      -1,    66,    -1,    68,    69,    -1,    -1,    -1,    15,    16,
      -1,    -1,    -1,    49,    50,    51,    23,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    60,    61,    -1,    63,     3,     4,
      66,    -1,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    49,    50,    51,    -1,    53,    -1,    23,    -1,
      -1,     3,     4,    60,    -1,    -1,    63,    -1,    -1,    66,
      -1,    68,    69,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    23,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,    60,    -1,    -1,    63,    64,
      -1,    66,    -1,    68,    69,    15,    16,    -1,    50,    51,
      -1,    -1,    -1,    23,    -1,    -1,     3,     4,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    68,    69,    15,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
      60,    -1,    -1,    63,    64,    -1,    66,    -1,    68,    69,
      15,    16,    -1,    50,    51,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    60,    -1,    -1,    63,     1,    -1,    66,
      67,    68,    69,     7,     8,     9,    10,    11,    12,    13,
      14,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    60,    -1,    -1,    63,    -1,
      -1,    66,    -1,    68,    69,    -1,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,     1,    -1,    58,    59,    -1,    61,     7,     8,
       9,    10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     7,     8,
       9,    10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    58,
      59,    -1,    61,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    58,
      59,    -1,    61,     7,     8,     9,    10,    11,    12,    13,
      14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       7,     8,     9,    10,    11,    12,    13,    14,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    -1,    -1,    58,    59,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    -1,    -1,
      -1,    58,    59,     7,     8,     9,    10,    11,    12,    13,
      14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     7,     8,
       9,    10,    11,    12,    13,    14,    -1,    -1,    -1,    -1,
      -1,    -1,     7,     8,     9,    10,    11,    12,    13,    14,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    -1,    -1,    -1,    58,    59,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    -1,    -1,    -1,    58,
      59,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    58,    59,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    58,    59,    50,    51,    52,    53,    54,
      -1,    -1,    -1,    58,    59
  };

  const signed char
  NelSonParser::yystos_[] =
  {
       0,     1,     3,     4,    15,    16,    17,    19,    20,    21,
      22,    23,    24,    27,    30,    31,    36,    37,    40,    41,
      50,    51,    60,    63,    66,    68,    69,    73,    74,    75,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      90,    97,    99,   100,   106,   107,   108,   109,   110,   111,
     112,   122,     3,     4,    15,    16,    15,    16,     1,     3,
      15,    16,    63,   101,   108,   112,     1,     3,    39,    63,
      76,     1,     3,    60,    98,     1,   108,   108,    79,   108,
     108,     1,   108,     1,     5,     6,    64,   108,   118,   119,
     121,     1,    67,   117,   119,   121,     1,   108,     1,     3,
      60,     0,    74,    80,     5,     6,    71,   120,     3,     4,
      15,    16,     7,     8,     9,    10,    11,    12,    13,    14,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    58,    59,    60,    66,    70,   113,   114,
      62,   114,   113,   118,    26,   102,   103,   104,     1,     5,
       6,    65,    91,   120,     1,     5,     6,    60,    62,    92,
      62,     1,     3,    64,    77,    78,     1,     3,    62,     1,
       3,    91,    91,    91,    32,    89,     1,    61,    64,   119,
     118,   120,    67,   119,   117,     1,    61,    77,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
       1,   108,     1,   108,     1,   108,     1,   108,     1,   108,
      49,    53,    61,   108,   115,   116,   116,     3,    60,   114,
       1,   108,    64,     1,   101,    25,   105,   104,    79,     1,
      61,    77,    79,    62,     1,    64,   120,     1,    60,    92,
       1,   108,     1,    62,    18,    79,    18,    79,    28,    93,
      94,    95,     3,    79,     1,    18,    62,    64,   121,    64,
     119,   108,    67,   121,    67,   119,     1,   108,    61,     1,
       3,     1,    61,   120,     1,    67,   108,     1,    79,     1,
      18,    92,    61,     1,    62,    78,     1,    61,    77,    79,
       1,   108,     1,    18,     1,    18,   108,    29,    96,    95,
      92,     1,     3,    64,    67,     1,   108,    62,   115,    61,
      79,     1,    92,    92,    61,     1,    61,    91,    79,     1,
      18,    79,     1,    60,    66,     1,   108,    79,    79,     1,
      92,    79,     1,    61,   116,     1,   116,    79,     1,    61,
       1,    67
  };

  const signed char
  NelSonParser::yyr1_[] =
  {
       0,    72,    73,    73,    73,    73,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      75,    75,    76,    76,    76,    76,    76,    76,    76,    76,
      77,    77,    78,    79,    79,    80,    80,    80,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    82,    83,    83,    83,    83,    83,
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
     109,   109,   110,   110,   110,   110,   110,   110,   110,   110,
     110,   110,   110,   110,   110,   110,   110,   110,   110,   110,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   111,   112,   112,   113,   113,   114,   114,   114,   114,
     114,   114,   114,   115,   115,   115,   115,   115,   115,   116,
     116,   117,   117,   118,   118,   119,   119,   120,   121,   121,
     122,   122,   122
  };

  const signed char
  NelSonParser::yyr2_[] =
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
       2,     2,     2,     2,     1,     2,     5,     4,     2,     3,
       5,     4,     1,     1,     1,     1,     1,     1,     3,     2,
       4,     4,     5,     2,     3,     4,     4,     5,     2,     2,
       1,     1,     3,     4,     4,     5,     2,     3,     4,     4,
       5,     2,     1,     2,     1,     2,     3,     2,     3,     3,
       3,     2,     4,     1,     1,     4,     4,     2,     2,     1,
       3,     1,     3,     1,     3,     1,     1,     1,     1,     3,
       3,     3,     2
  };


#if YYDEBUG
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a YYNTOKENS, nonterminals.
  const char*
  const NelSonParser::yytname_[] =
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
  "functionTerminatorStatement", "specialSyntaxStatement",
  "returnStatement", "pauseStatement", "continueStatement",
  "breakStatement", "tryStatement", "optionalCatch", "switchStatement",
  "optionalEndStatement", "newLine", "caseBlock", "caseList",
  "caseStatement", "otherwiseClause", "forStatement", "forIndexExpression",
  "whileStatement", "ifStatement", "conditionedStatement", "elseIfBlock",
  "elseIfStatementList", "elseIfStatement", "elseStatement",
  "assignmentStatement", "multiFunctionCall", "expr", "anonymousFunction",
  "terminal", "postfixableLiteral", "symbRefList", "postfixRefList",
  "symbRef", "indexElement", "indexList", "cellDef", "matrixDef",
  "rowSeperator", "columnSep", "rowDef", "parenExpr", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  NelSonParser::yyrline_[] =
  {
       0,    90,    90,    91,    91,    92,    96,   103,   111,   119,
     128,   136,   145,   146,   147,   148,   149,   150,   151,   152,
     156,   157,   161,   162,   163,   164,   165,   166,   167,   168,
     172,   173,   177,   181,   182,   186,   190,   194,   201,   202,
     203,   204,   205,   206,   207,   208,   209,   210,   211,   212,
     213,   214,   215,   216,   220,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   236,   240,   244,   247,
     251,   257,   262,   266,   267,   271,   277,   283,   284,   285,
     286,   290,   291,   296,   297,   301,   304,   310,   316,   319,
     325,   328,   333,   338,   339,   340,   342,   343,   344,   345,
     346,   347,   351,   354,   359,   360,   364,   370,   371,   375,
     378,   381,   385,   386,   390,   393,   399,   402,   406,   409,
     410,   414,   415,   419,   422,   426,   430,   434,   435,   436,
     437,   438,   439,   443,   444,   445,   446,   447,   448,   449,
     450,   451,   452,   453,   454,   455,   456,   457,   458,   459,
     460,   461,   462,   463,   464,   465,   466,   467,   468,   469,
     470,   471,   472,   473,   474,   475,   476,   477,   478,   479,
     480,   481,   482,   483,   484,   485,   486,   487,   488,   489,
     490,   491,   492,   493,   494,   498,   499,   500,   501,   502,
     503,   504,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     528,   529,   530,   531,   532,   533,   534,   535,   536,   537,
     538,   539,   543,   544,   548,   549,   553,   554,   555,   556,
     557,   558,   559,   563,   564,   565,   566,   567,   568,   572,
     573,   577,   578,   582,   583,   587,   588,   592,   596,   597,
     601,   602,   603
  };

  void
  NelSonParser::yy_stack_print_ () const
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << int (i->state);
    *yycdebug_ << '\n';
  }

  void
  NelSonParser::yy_reduce_print_ (int yyrule) const
  {
    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  NelSonParser::symbol_kind_type
  NelSonParser::yytranslate_ (int t) YY_NOEXCEPT
  {
    // YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to
    // TOKEN-NUM as returned by yylex.
    static
    const signed char
    translate_table[] =
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
    // Last valid token kind.
    const int code_max = 302;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= code_max)
      return static_cast <symbol_kind_type> (translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

#line 5 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"
} // Nelson
#line 3142 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.cpp"

#line 605 "D:\\Developpements\\Github\\nelson-lang\\nelson-thirdparty-x64\\flex-bison\\..\\..\\NelSon\\modules\\interpreter\\src\\grammar\\NelSonParserCpp.yxx"

//=============================================================================
namespace Nelson {
//=============================================================================
  int callyyparse(LexerContext& lexerContext, ParserContext& parserContext) {
    NelSonParser parser(lexerContext, parserContext);
    return parser.parse();
  }

  static int yylex(ParseRHS* semanticValue, Nelson::LexerContext& lexerContext) {
    int token = ::yylex(lexerContext);
    if (token > 0) {
      *semanticValue = ::yylval;
    }
    return token;
  }

  void NelSonParser::error(const std::string& message) {
    const int column = lexerContext.linestart == nullptr
        ? 0
        : static_cast<int>(lexerContext.datap - lexerContext.linestart + 1);
    setParserDiagnostic(message, lexerContext.lineNumber + 1, column);
  }
//=============================================================================
}
//=============================================================================
// clang-format on
