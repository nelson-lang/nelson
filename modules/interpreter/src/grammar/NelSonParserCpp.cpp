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

// As a special NelsonException, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special NelsonException, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special NelsonException.

// This special NelsonException was added by the Free Software Foundation in
// version 2.2 of Bison.

// DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
// especially those whose name start with YY_ or yy_.  They are
// private implementation details that can be changed or removed.



// First part of user prologue.
#line 14 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"

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
#include "ErrorID.hpp"
#include "LexerContext.hpp"
#include "ParserInterface.hpp"
#include "AnonymousFunctionText.hpp"
//=============================================================================
#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
#endif

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

#line 88 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"


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


// Whether we are compiled with NelsonException support.
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

#line 5 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
namespace Nelson {
#line 167 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"

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
            YYCDEBUG << "Caught NelsonException: " << yyexc.what() << '\n';
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
#line 93 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                { setParsedScriptBlock(yystack_[0].value.v.p);}
#line 625 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 5: // program: error
#line 95 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
          {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:statementListFunctionDefinition", "statement list or function definition"));}
#line 631 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 6: // functionDef: FUNCTION returnDeclaration IDENT '(' inputArgumentList ')' functionSignatureTerminator statementList
#line 99 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                                         {
   ParseRHS lhsRhs = yystack_[6].value;
   ParseRHS nameRhs = yystack_[5].value;
   ParseRHS rhsRhs = yystack_[3].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
  }
#line 643 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 7: // functionDef: FUNCTION IDENT '(' inputArgumentList ')' functionSignatureTerminator statementList
#line 106 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                        {
   ParseRHS lhsRhs;
   lhsRhs.v.p = nullptr;
   ParseRHS nameRhs = yystack_[5].value;
   ParseRHS rhsRhs = yystack_[3].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 656 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 8: // functionDef: FUNCTION returnDeclaration IDENT functionSignatureTerminator statementList
#line 114 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                 {
    ParseRHS lhsRhs = yystack_[3].value;
    ParseRHS nameRhs = yystack_[2].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 669 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 9: // functionDef: FUNCTION IDENT functionSignatureTerminator statementList
#line 122 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                               {
    ParseRHS lhsRhs;
    lhsRhs.v.p = nullptr;
    ParseRHS nameRhs = yystack_[2].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 683 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 10: // functionDef: FUNCTION returnDeclaration IDENT '(' ')' functionSignatureTerminator statementList
#line 131 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                         {
    ParseRHS lhsRhs = yystack_[5].value;
    ParseRHS nameRhs = yystack_[4].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
    ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 696 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 11: // functionDef: FUNCTION IDENT '(' ')' functionSignatureTerminator statementList
#line 139 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                       {
    ParseRHS lhsRhs;
    lhsRhs.v.p = nullptr;
    ParseRHS nameRhs = yystack_[4].value;
    ParseRHS rhsRhs;
    rhsRhs.v.p = nullptr;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   }
#line 710 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 12: // functionDef: FUNCTION returnDeclaration IDENT '(' inputArgumentList ')' functionSignatureTerminator emptyFunctionBody
#line 148 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                                               {
   ParseRHS lhsRhs = yystack_[6].value;
   ParseRHS nameRhs = yystack_[5].value;
   ParseRHS rhsRhs = yystack_[3].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   YYACCEPT;
  }
#line 723 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 13: // functionDef: FUNCTION IDENT '(' inputArgumentList ')' functionSignatureTerminator emptyFunctionBody
#line 156 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                            {
   ParseRHS lhsRhs;
   lhsRhs.v.p = nullptr;
   ParseRHS nameRhs = yystack_[5].value;
   ParseRHS rhsRhs = yystack_[3].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   YYACCEPT;
   }
#line 737 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 14: // functionDef: FUNCTION returnDeclaration IDENT '(' inputArgumentList ')' emptyFunctionBody
#line 165 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                                   {
   ParseRHS lhsRhs = yystack_[5].value;
   ParseRHS nameRhs = yystack_[4].value;
   ParseRHS rhsRhs = yystack_[2].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   YYACCEPT;
  }
#line 750 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 15: // functionDef: FUNCTION IDENT '(' inputArgumentList ')' emptyFunctionBody
#line 173 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                {
   ParseRHS lhsRhs;
   lhsRhs.v.p = nullptr;
   ParseRHS nameRhs = yystack_[4].value;
   ParseRHS rhsRhs = yystack_[2].value;
   ParseRHS codeRhs = yystack_[0].value;
   functionBody(lhsRhs, nameRhs, rhsRhs, codeRhs);
   YYACCEPT;
   }
#line 764 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 16: // functionDef: FUNCTION error
#line 182 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                     {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:legalFunctionNameReturn", "legal function name or return declaration after 'function'"), yystack_[1].value);}
#line 770 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 17: // functionDef: FUNCTION IDENT error
#line 183 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                           {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:listStatementListAfter", "argument list or statement list after identifier '") + yystack_[1].value.v.p->text.c_str() + "'",yystack_[1].value);}
#line 776 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 18: // functionDef: FUNCTION IDENT '(' error
#line 184 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:possiblyEmptyListAfter", "(possibly empty) argument list after '('"),yystack_[1].value);}
#line 782 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 19: // functionDef: FUNCTION IDENT '(' inputArgumentList ')' error
#line 185 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                     {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:statementListAfter", "statement list after ')'"),yystack_[1].value);}
#line 788 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 20: // functionDef: FUNCTION returnDeclaration error
#line 186 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                        {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:functionNameFunctionDeclared", "function name for function declared"),yystack_[2].value);}
#line 794 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 21: // functionDef: FUNCTION returnDeclaration IDENT error
#line 187 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                             {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:listStatementListFollowing", "argument list or statement list following function name :") + yystack_[1].value.v.p->text.c_str(), yystack_[1].value);}
#line 800 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 22: // functionDef: FUNCTION returnDeclaration IDENT '(' error
#line 188 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                  {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:possiblyEmptyListAfter", "(possibly empty) argument list after '('"),yystack_[1].value);}
#line 806 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 23: // functionDef: FUNCTION returnDeclaration IDENT '(' inputArgumentList ')' error
#line 189 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                       {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:statementListAfter", "statement list after ')'"),yystack_[1].value);}
#line 812 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 28: // emptyFunctionBody: $end
#line 206 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
        {
    AbstractSyntaxTreePtr emptyStatement = AbstractSyntaxTree::createNode(null_node, "", -1);
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BLOCK, emptyStatement, -1);
  }
#line 821 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 29: // returnDeclaration: VARARGOUT '='
#line 213 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 827 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 30: // returnDeclaration: IDENT '='
#line 214 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 833 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 31: // returnDeclaration: '[' argumentList ']' '='
#line 215 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                             {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 839 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 32: // returnDeclaration: '[' ']' '='
#line 216 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yylhs.value.v.p = nullptr;}
#line 845 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 33: // returnDeclaration: IDENT error
#line 217 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:symbolAfterIdentifierReturn", "an '=' symbol after identifier in return declaration"),yystack_[1].value);}
#line 851 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 34: // returnDeclaration: '[' error
#line 218 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:validListReturnReturn", "a valid list of return arguments in return declaration"),yystack_[1].value);}
#line 857 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 35: // returnDeclaration: '[' argumentList error
#line 219 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                           {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:matchingReturnDeclaration", "matching ']' in return declaration for '['"),yystack_[2].value);}
#line 863 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 36: // returnDeclaration: '[' argumentList ']' error
#line 220 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                               {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:symbolAfterReturnDeclaration", "an '=' symbol after return declaration"),yystack_[1].value);}
#line 869 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 37: // argumentList: argument
#line 224 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
           {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 875 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 38: // argumentList: argumentList columnSep argument
#line 225 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                    {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 881 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 40: // inputArgumentList: inputArgument
#line 233 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 887 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 41: // inputArgumentList: inputArgumentList columnSep inputArgument
#line 234 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                              {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 893 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 43: // inputArgument: '~'
#line 239 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
        {yylhs.value.v.p = AbstractSyntaxTree::createNode(id_node, "~", yystack_[0].value.v.i);}
#line 899 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 44: // statementList: statement
#line 243 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
            {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BLOCK,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 905 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 45: // statementList: statementList statement
#line 244 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 911 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 46: // statement: statementType ENDQSTMNT
#line 248 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {
      yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_QSTATEMENT,NULL,yystack_[0].value.v.i);
      yylhs.value.v.p->down = yystack_[1].value.v.p;
  }
#line 920 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 47: // statement: statementType ENDSTMNT
#line 252 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                           {
     yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yystack_[0].value.v.i);
     yylhs.value.v.p->down = yystack_[1].value.v.p;
  }
#line 929 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 48: // statement: statementType columnSep
#line 256 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                            {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RSTATEMENT,NULL,yystack_[0].value.v.i);
    yylhs.value.v.p->down = yystack_[1].value.v.p;
  }
#line 938 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 51: // statementType: %empty
#line 265 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
    {yylhs.value.v.p = AbstractSyntaxTree::createNode(null_node,"",-1);}
#line 944 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 66: // argumentsStatement: ARGUMENTS
#line 283 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
            {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ARGUMENTS, yystack_[0].value.v.p, yystack_[0].value.v.p->getContext());}
#line 950 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 67: // functionTerminatorStatement: ENDFUNCTION
#line 287 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yylhs.value.v.p = AbstractSyntaxTree::createNode(reserved_node, NLS_KEYWORD_RETURN, yystack_[0].value.v.p->getContext());}
#line 956 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 68: // specialSyntaxStatement: SPECIALCALL
#line 290 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 962 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 69: // specialSyntaxStatement: IDENT NUMERIC
#line 291 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 968 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 70: // specialSyntaxStatement: STRING STRING
#line 292 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 974 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 71: // specialSyntaxStatement: CHARACTER CHARACTER
#line 293 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 980 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 72: // specialSyntaxStatement: IDENT STRING
#line 294 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                 {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 986 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 73: // specialSyntaxStatement: IDENT CHARACTER
#line 295 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 992 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 74: // specialSyntaxStatement: IDENT IDENT
#line 296 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SCALL,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext()); }
#line 998 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 75: // specialSyntaxStatement: specialSyntaxStatement STRING
#line 297 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                  {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 1004 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 76: // specialSyntaxStatement: specialSyntaxStatement CHARACTER
#line 298 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                     {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 1010 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 77: // specialSyntaxStatement: specialSyntaxStatement IDENT
#line 299 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                 {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 1016 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 78: // specialSyntaxStatement: specialSyntaxStatement NUMERIC
#line 300 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                   {yystack_[1].value.v.p->addChild(yystack_[0].value.v.p);}
#line 1022 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 83: // tryStatement: TRY statementList optionalCatch END
#line 319 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
  {
    yylhs.value.v.p = yystack_[3].value.v.p;
    yylhs.value.v.p->addChild(yystack_[2].value.v.p);
    if (yystack_[1].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1032 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 84: // tryStatement: TRY statementList optionalCatch error
#line 325 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
  {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:matchingEndClauseFrom", "matching 'end' to '%s' clause from line %s"), "try", decodeline(yystack_[3].value)),yystack_[0].value);}
#line 1038 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 85: // optionalCatch: CATCH IDENT newLine statementList
#line 329 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                    {
    yylhs.value.v.p = yystack_[2].value.v.p;
    yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1047 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 86: // optionalCatch: CATCH IDENT newLine
#line 333 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {
    // empty catch body: give the identifier node the same block-shaped child as usual
    yylhs.value.v.p = yystack_[1].value.v.p;
    AbstractSyntaxTreePtr emptyStatement = AbstractSyntaxTree::createNode(OP_QSTATEMENT, NULL, yystack_[0].value.v.i);
    emptyStatement->down = AbstractSyntaxTree::createNode(null_node, "", -1);
    yylhs.value.v.p->addChild(AbstractSyntaxTree::createNode(OP_BLOCK, emptyStatement, emptyStatement->getContext()));
  }
#line 1059 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 87: // optionalCatch: CATCH statementList
#line 340 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 1065 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 88: // optionalCatch: %empty
#line 341 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
    {yylhs.value.v.p = nullptr;}
#line 1071 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 89: // switchStatement: SWITCH expr switchExprTerminator caseBlock otherwiseClause END
#line 345 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                  {
    yylhs.value.v.p = yystack_[5].value.v.p;
    yylhs.value.v.p->addChild(yystack_[4].value.v.p);
    if (yystack_[2].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[2].value.v.p);
    if (yystack_[1].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1082 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 90: // switchStatement: SWITCH expr switchExprTerminator caseBlock otherwiseClause error
#line 351 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                      {
    yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:matchingEndClauseFrom", "matching 'end' to '%s' clause from line %s"), "switch", decodeline(yystack_[5].value)),yystack_[0].value);
  }
#line 1090 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 99: // caseBlock: %empty
#line 380 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
  {yylhs.value.v.p = nullptr;}
#line 1096 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 101: // caseList: caseStatement
#line 385 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_CASEBLOCK,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());
  }
#line 1104 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 102: // caseList: caseList caseStatement
#line 388 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                           {
    yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1112 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 103: // caseStatement: CASE expr optionalEndStatement statementList
#line 394 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                               {
    yylhs.value.v.p = yystack_[3].value.v.p; yylhs.value.v.p->addChild(yystack_[2].value.v.p); yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1120 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 104: // caseStatement: CASE expr optionalEndStatement
#line 397 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                   {
    yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1128 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 105: // otherwiseClause: OTHERWISE statementList
#line 403 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {
    yylhs.value.v.p = yystack_[0].value.v.p;
  }
#line 1136 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 106: // otherwiseClause: OTHERWISE
#line 406 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {
    yylhs.value.v.p = nullptr;
  }
#line 1144 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 107: // otherwiseClause: %empty
#line 409 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
    {
    yylhs.value.v.p = nullptr;
  }
#line 1152 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 108: // forStatement: FOR forIndexExpression optionalEndStatement END
#line 415 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                  {
   yylhs.value.v.p = yystack_[3].value.v.p;
   yylhs.value.v.p->addChild(yystack_[2].value.v.p);
  }
#line 1161 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 109: // forStatement: FOR forIndexExpression optionalEndStatement statementList END
#line 419 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                                  {
    yylhs.value.v.p = yystack_[4].value.v.p;
    yylhs.value.v.p->addChild(yystack_[3].value.v.p);
    yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1171 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 110: // forStatement: FOR forIndexExpression optionalEndStatement statementList error
#line 425 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
           {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:endMatchStatementFrom", "'end' to match '%s' statement from line %s"), "for", decodeline(yystack_[4].value)),yystack_[0].value);}
#line 1177 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 111: // forIndexExpression: '(' IDENT '=' expr ')'
#line 429 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                         {yylhs.value.v.p = yystack_[3].value.v.p; yylhs.value.v.p->addChild(yystack_[1].value.v.p);}
#line 1183 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 112: // forIndexExpression: IDENT '=' expr
#line 430 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1189 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 113: // forIndexExpression: IDENT
#line 431 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
          {yylhs.value.v.p = yystack_[0].value.v.p;
        yylhs.value.v.p->addChild(AbstractSyntaxTree::createNode(OP_RHS, AbstractSyntaxTree::createNode(id_node,yystack_[0].value.v.p->text.c_str(), yystack_[0].value.v.p->getContext()),yystack_[0].value.v.p->getContext())); }
#line 1196 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 114: // forIndexExpression: '(' IDENT '=' expr error
#line 433 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                             {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:matchingRightParenthesis", "matching right parenthesis"),yystack_[4].value);}
#line 1202 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 115: // forIndexExpression: '(' IDENT '=' error
#line 434 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:indexingExpression", "indexing expression"),yystack_[1].value);}
#line 1208 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 116: // forIndexExpression: '(' IDENT error
#line 435 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:equalsOperatorAfterLoop", "equals operator after loop index"),yystack_[1].value);}
#line 1214 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 117: // forIndexExpression: '(' error
#line 436 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:identifierThatLoopVariable", "identifier that is the loop variable"),yystack_[1].value);}
#line 1220 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 118: // forIndexExpression: IDENT '=' error
#line 437 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:indexingExpression", "indexing expression"),yystack_[1].value);}
#line 1226 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 119: // forIndexExpression: error
#line 438 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
          {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:identifierAssignmentIdExpr", "identifier or assignment (id = expr) after 'for' "));}
#line 1232 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 120: // whileStatement: WHILE expr optionalEndStatement END
#line 442 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                      {
    yylhs.value.v.p = yystack_[3].value.v.p;
    yylhs.value.v.p->addChild(yystack_[2].value.v.p);
  }
#line 1241 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 121: // whileStatement: WHILE expr optionalEndStatement statementList END
#line 446 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                      {
    yylhs.value.v.p = yystack_[4].value.v.p;
    yylhs.value.v.p->addChild(yystack_[3].value.v.p);
    yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1251 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 122: // whileStatement: WHILE error
#line 451 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:testExpressionAfterWhile", "test expression after 'while'"),yystack_[1].value);}
#line 1257 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 123: // whileStatement: WHILE expr optionalEndStatement statementList error
#line 452 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                        {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:endMatchStatementFrom", "'end' to match '%s' statement from line %s"), "while", decodeline(yystack_[4].value)),yystack_[0].value);}
#line 1263 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 124: // ifStatement: IF conditionedStatement elseIfBlock elseStatement END
#line 456 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                        {
    yylhs.value.v.p = yystack_[4].value.v.p;
    yylhs.value.v.p->addChild(yystack_[3].value.v.p);
    if (yystack_[2].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[2].value.v.p);
    if (yystack_[1].value.v.p != nullptr) yylhs.value.v.p->addChild(yystack_[1].value.v.p);
  }
#line 1274 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 125: // ifStatement: IF error
#line 462 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
             {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:conditionExpressionIf", "condition expression for 'if'"),yystack_[1].value);}
#line 1280 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 126: // ifStatement: IF conditionedStatement elseIfBlock elseStatement error
#line 463 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                            {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:endMatchStatementFrom", "'end' to match '%s' statement from line %s"), "if", decodeline(yystack_[4].value)),yystack_[0].value);}
#line 1286 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 127: // conditionedStatement: expr optionalEndStatement statementList
#line 467 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                          {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[2].value.v.p->getContext());
  }
#line 1294 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 128: // conditionedStatement: expr optionalEndStatement
#line 470 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                              {
	  yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_CSTAT,yystack_[1].value.v.p,yystack_[1].value.v.p->getContext());
	}
#line 1302 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 129: // conditionedStatement: expr error
#line 473 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
               {yyxpt("valid list of statements after condition",yystack_[0].value);}
#line 1308 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 130: // elseIfBlock: %empty
#line 477 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yylhs.value.v.p = nullptr;}
#line 1314 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 132: // elseIfStatementList: elseIfStatement
#line 482 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {
    yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ELSEIFBLOCK,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());
  }
#line 1322 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 133: // elseIfStatementList: elseIfStatementList elseIfStatement
#line 485 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                        {
    yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);
  }
#line 1330 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 134: // elseIfStatement: ELSEIF conditionedStatement
#line 491 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                              {
    yylhs.value.v.p = yystack_[0].value.v.p;
  }
#line 1338 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 135: // elseIfStatement: ELSEIF error
#line 494 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                 {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:testConditionElseifClause", "test condition for 'elseif' clause"),yystack_[1].value);}
#line 1344 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 136: // elseStatement: ELSE statementList
#line 498 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                     {
    yylhs.value.v.p = yystack_[0].value.v.p;
  }
#line 1352 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 137: // elseStatement: %empty
#line 501 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
               {yylhs.value.v.p = nullptr;}
#line 1358 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 138: // elseStatement: ELSE error
#line 502 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
               {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:statementListElseClause", "statement list for 'else' clause"),yystack_[1].value);}
#line 1364 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 139: // assignmentStatement: symbRefList '=' expr
#line 506 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ASSIGN,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1370 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 140: // assignmentStatement: symbRefList '=' error
#line 507 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAssignment", "expression in assignment"),yystack_[1].value);}
#line 1376 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 141: // multiFunctionCall: '[' matrixDef ']' '=' expr
#line 511 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                             {
    yylhs.value.v.p = assignOrMultiFunctionCall(yystack_[3].value.v.p,yystack_[0].value.v.p,yystack_[4].value.v.i,yystack_[1].value.v.i);
  }
#line 1384 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 142: // multiFunctionCall: '[' matrixDef ']' '=' error
#line 514 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionRightHandSide", "expression on the right hand side of assignment"),yystack_[1].value);}
#line 1390 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 143: // expr: expr ':' expr
#line 518 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_COLON,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1396 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 144: // expr: expr ':' error
#line 519 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), ":"), yystack_[1].value);}
#line 1402 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 146: // expr: expr '+' expr
#line 521 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_PLUS,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1408 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 147: // expr: expr '+' error
#line 522 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "+"), yystack_[1].value);}
#line 1414 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 148: // expr: expr '-' expr
#line 523 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SUBTRACT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1420 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 149: // expr: expr '-' error
#line 524 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "-"), yystack_[1].value);}
#line 1426 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 150: // expr: expr '*' expr
#line 525 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_TIMES,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1432 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 151: // expr: expr '*' error
#line 526 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "*"),yystack_[1].value);}
#line 1438 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 152: // expr: expr '/' expr
#line 527 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1444 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 153: // expr: expr '/' error
#line 528 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "/"),yystack_[1].value);}
#line 1450 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 154: // expr: expr '\\' expr
#line 529 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_LDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1456 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 155: // expr: expr '\\' error
#line 530 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "\\"),yystack_[1].value);}
#line 1462 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 156: // expr: expr '|' expr
#line 531 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_OR,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1468 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 157: // expr: expr '|' error
#line 532 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "|"),yystack_[1].value);}
#line 1474 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 158: // expr: expr '&' expr
#line 533 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_AND,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1480 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 159: // expr: expr '&' error
#line 534 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "&"),yystack_[1].value);}
#line 1486 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 160: // expr: expr SOR expr
#line 535 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SOR,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1492 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 161: // expr: expr SOR error
#line 536 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "||"),yystack_[1].value);}
#line 1498 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 162: // expr: expr SAND expr
#line 537 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SAND,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1504 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 163: // expr: expr SAND error
#line 538 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "&&"),yystack_[1].value);}
#line 1510 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 164: // expr: expr '<' expr
#line 539 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_LT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1516 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 165: // expr: expr '<' error
#line 540 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "<"),yystack_[1].value);}
#line 1522 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 166: // expr: expr LE expr
#line 541 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_LEQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1528 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 167: // expr: expr LE error
#line 542 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "<="),yystack_[1].value);}
#line 1534 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 168: // expr: expr '>' expr
#line 543 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_GT,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1540 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 169: // expr: expr '>' error
#line 544 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), ">"),yystack_[1].value);}
#line 1546 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 170: // expr: expr GE expr
#line 545 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_GEQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1552 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 171: // expr: expr GE error
#line 546 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), ">="),yystack_[1].value);}
#line 1558 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 172: // expr: expr EQ expr
#line 547 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1564 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 173: // expr: expr EQ error
#line 548 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "=="),yystack_[1].value);}
#line 1570 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 174: // expr: expr NE expr
#line 549 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_NEQ,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1576 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 175: // expr: expr NE error
#line 550 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "~="),yystack_[1].value);}
#line 1582 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 176: // expr: expr DOTTIMES expr
#line 551 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_TIMES,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1588 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 177: // expr: expr DOTTIMES error
#line 552 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), ".*"), yystack_[1].value);}
#line 1594 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 178: // expr: expr DOTRDIV expr
#line 553 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_RDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1600 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 179: // expr: expr DOTRDIV error
#line 554 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "./"),yystack_[1].value);}
#line 1606 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 180: // expr: expr DOTLDIV expr
#line 555 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                      {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_LDIV,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1612 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 181: // expr: expr DOTLDIV error
#line 556 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), ".\\"),yystack_[1].value);}
#line 1618 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 182: // expr: '-' expr
#line 557 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_UMINUS,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1624 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 183: // expr: '+' expr
#line 558 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_UPLUS, yystack_[0].value.v.p, yystack_[1].value.v.i);}
#line 1630 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 184: // expr: '~' expr
#line 559 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_NOT,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1636 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 185: // expr: '~' error
#line 560 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfterLogical", "an expression after logical not"),yystack_[1].value);}
#line 1642 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 186: // expr: expr '^' expr
#line 561 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                   {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_MPOWER,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1648 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 187: // expr: expr '^' error
#line 562 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), "^"),yystack_[1].value);}
#line 1654 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 188: // expr: expr DOTPOWER expr
#line 563 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_POWER,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1660 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 189: // expr: expr DOTPOWER error
#line 564 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yyxpt(formatI18nMessage(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfter", "an expression after '%s'"), ".^"),yystack_[1].value);}
#line 1666 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 190: // expr: expr '\''
#line 565 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_TRANSPOSE,yystack_[1].value.v.p,yystack_[0].value.v.i);}
#line 1672 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 191: // expr: expr DOTTRANSPOSE
#line 566 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT_TRANSPOSE,yystack_[1].value.v.p,yystack_[0].value.v.i);}
#line 1678 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 192: // expr: parenExpr postfixRefList
#line 567 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                             {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_POSTFIX,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 1684 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 193: // expr: postfixableLiteral postfixRefList
#line 568 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                      {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_POSTFIX,yystack_[1].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.p->getContext());}
#line 1690 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 195: // namedHandleTarget: IDENT
#line 573 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
        {yylhs.value = yystack_[0].value;}
#line 1696 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 196: // namedHandleTarget: namedHandleTarget '.' IDENT
#line 574 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                {yylhs.value.isToken = false; yylhs.value.v.p = AbstractSyntaxTree::createNode(id_node, yystack_[2].value.v.p->text + "." + yystack_[0].value.v.p->text, yystack_[2].value.v.p->getContext());}
#line 1702 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 197: // anonymousFunction: '@' namedHandleTarget
#line 578 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yylhs.value.v.p =  AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_NAMED, yystack_[0].value.v.p, yystack_[0].value.v.p->getContext());}
#line 1708 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 198: // anonymousFunction: '@' '(' inputArgumentList ')' expr
#line 579 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yystack_[2].value.v.p,yystack_[0].value.v.p, yystack_[2].value.v.p->getContext()); yylhs.value.v.p->text = takeAnonymousFunctionText(lexerContext);}
#line 1714 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 199: // anonymousFunction: '@' '(' ')' expr
#line 580 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                     {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_FUNCTION_HANDLE_ANONYMOUS,yystack_[0].value.v.p, yystack_[0].value.v.p->getContext()); yylhs.value.v.p->text = takeAnonymousFunctionText(lexerContext);}
#line 1720 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 200: // anonymousFunction: '@' error
#line 581 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:functionNameParameterList", "function name or parameter list after '@'"), yystack_[1].value);}
#line 1726 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 201: // anonymousFunction: '@' '(' error
#line 582 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:listClosingParenthesisAfter", "argument list or closing parenthesis after '('"), yystack_[1].value);}
#line 1732 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 202: // anonymousFunction: '@' '(' inputArgumentList ')' error
#line 583 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                        {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAnonymousFunctionBody", "expression for anonymous function body after ')'"), yystack_[1].value);}
#line 1738 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 203: // anonymousFunction: '@' '(' ')' error
#line 584 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                      {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAnonymousFunctionBody", "expression for anonymous function body after ')'"), yystack_[1].value);}
#line 1744 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 207: // terminal: MAGICEND
#line 590 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
             {yylhs.value.v.p = AbstractSyntaxTree::createNode(reserved_node, NLS_KEYWORD_END, yystack_[0].value.v.p->getContext());}
#line 1750 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 209: // terminal: symbRefList
#line 592 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_RHS,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 1756 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 210: // terminal: IDENT '@' symbRefList
#line 593 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SUPERCLASS_CALL,yystack_[2].value.v.p,yystack_[0].value.v.p,yystack_[1].value.v.i);}
#line 1762 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 211: // terminal: '[' matrixDef ']'
#line 594 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                      {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1768 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 212: // terminal: '[' error
#line 595 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:matrixDefinitionFollowedBy", "a matrix definition followed by a right bracket"),yystack_[1].value);}
#line 1774 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 213: // terminal: '[' rowSeparator matrixDef ']'
#line 596 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1780 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 214: // terminal: '[' matrixDef rowSeparator ']'
#line 597 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1786 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 215: // terminal: '[' rowSeparator matrixDef rowSeparator ']'
#line 598 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1792 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 216: // terminal: '[' ']'
#line 599 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yystack_[1].value.v.i);}
#line 1798 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 217: // terminal: '{' cellDef '}'
#line 600 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1804 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 218: // terminal: '{' rowSeparator cellDef '}'
#line 601 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1810 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 219: // terminal: '{' cellDef rowSeparator '}'
#line 602 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1816 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 220: // terminal: '{' rowSeparator cellDef rowSeparator '}'
#line 603 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                              {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1822 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 221: // terminal: '{' '}'
#line 604 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yystack_[1].value.v.i);}
#line 1828 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 222: // terminal: '{' error
#line 605 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:cellArrayDefinitionFollowed", "a cell-array definition followed by a right brace"),yystack_[1].value);}
#line 1834 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 225: // postfixableLiteral: '[' matrixDef ']'
#line 611 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                      {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1840 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 226: // postfixableLiteral: '[' rowSeparator matrixDef ']'
#line 612 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1846 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 227: // postfixableLiteral: '[' matrixDef rowSeparator ']'
#line 613 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                   {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1852 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 228: // postfixableLiteral: '[' rowSeparator matrixDef rowSeparator ']'
#line 614 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                                {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1858 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 229: // postfixableLiteral: '[' ']'
#line 615 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY,NULL,yystack_[1].value.v.i);}
#line 1864 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 230: // postfixableLiteral: '{' cellDef '}'
#line 616 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1870 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 231: // postfixableLiteral: '{' rowSeparator cellDef '}'
#line 617 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 1876 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 232: // postfixableLiteral: '{' cellDef rowSeparator '}'
#line 618 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                 {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1882 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 233: // postfixableLiteral: '{' rowSeparator cellDef rowSeparator '}'
#line 619 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                              {yylhs.value.v.p = yystack_[2].value.v.p;}
#line 1888 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 234: // postfixableLiteral: '{' '}'
#line 620 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                  {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_EMPTY_CELL,NULL,yystack_[1].value.v.i);}
#line 1894 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 236: // symbRefList: symbRefList symbRef
#line 625 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1900 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 238: // postfixRefList: postfixRefList symbRef
#line 630 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                           {yylhs.value.v.p = yystack_[1].value.v.p; yylhs.value.v.p->addPeer(yystack_[0].value.v.p);}
#line 1906 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 239: // symbRef: '(' indexList ')'
#line 634 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_PARENS,yystack_[1].value.v.p,yystack_[2].value.v.i); }
#line 1912 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 240: // symbRef: '(' ')'
#line 635 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
            {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_PARENS,NULL,yystack_[1].value.v.i); }
#line 1918 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 241: // symbRef: '(' indexList error
#line 636 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:matchingRightParenthesis", "matching right parenthesis"),yystack_[2].value);}
#line 1924 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 242: // symbRef: '{' indexList '}'
#line 637 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                      {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yystack_[1].value.v.p,yystack_[2].value.v.i); }
#line 1930 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 243: // symbRef: '{' indexList error
#line 638 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:matchingRightBrace", "matching right brace"),yystack_[2].value);}
#line 1936 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 244: // symbRef: '.' IDENT
#line 639 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOT,yystack_[0].value.v.p,yystack_[1].value.v.i); }
#line 1942 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 245: // symbRef: '.' '(' expr ')'
#line 640 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                           {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_DOTDYN,yystack_[1].value.v.p,yystack_[3].value.v.i);}
#line 1948 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 247: // indexElement: ':'
#line 645 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
        {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_ALL,NULL,yystack_[0].value.v.i);}
#line 1954 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 248: // indexElement: '/' IDENT '=' expr
#line 646 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                       {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:slashKeywordSupported", "slash keyword arguments are not supported"),yystack_[3].value);}
#line 1960 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 249: // indexElement: '/' IDENT '=' error
#line 647 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                        {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:slashKeywordSupported", "slash keyword arguments are not supported"),yystack_[3].value);}
#line 1966 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 250: // indexElement: '/' IDENT
#line 648 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:slashKeywordSupported", "slash keyword arguments are not supported"),yystack_[1].value);}
#line 1972 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 251: // indexElement: '/' error
#line 649 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:slashKeywordSupported", "slash keyword arguments are not supported"),yystack_[1].value);}
#line 1978 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 253: // indexList: indexList columnSep indexElement
#line 654 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                     {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addPeer(yystack_[0].value.v.p);}
#line 1984 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 254: // cellDef: rowDef
#line 658 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
         {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BRACES,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 1990 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 255: // cellDef: cellDef rowSeparator rowDef
#line 659 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 1996 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 256: // matrixDef: rowDef
#line 663 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
         {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_BRACKETS,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 2002 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 257: // matrixDef: matrixDef rowSeparator rowDef
#line 664 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                  {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 2008 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 261: // rowDef: rowElements
#line 677 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
              {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 2014 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 262: // rowDef: columnSep rowElements
#line 678 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {yylhs.value.v.p = yystack_[0].value.v.p;}
#line 2020 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 263: // rowDef: rowElements columnSep
#line 679 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                          {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 2026 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 264: // rowDef: columnSep rowElements columnSep
#line 680 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                                    {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 2032 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 265: // rowElements: expr
#line 684 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
       {yylhs.value.v.p = AbstractSyntaxTree::createNode(OP_SEMICOLON,yystack_[0].value.v.p,yystack_[0].value.v.p->getContext());}
#line 2038 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 266: // rowElements: rowElements columnSep expr
#line 685 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                               {yylhs.value.v.p = yystack_[2].value.v.p; yylhs.value.v.p->addChild(yystack_[0].value.v.p);}
#line 2044 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 267: // parenExpr: '(' expr ')'
#line 690 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
               {yylhs.value.v.p = yystack_[1].value.v.p;}
#line 2050 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 268: // parenExpr: '(' expr error
#line 691 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
                    {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:rightParenthesisAfterExpression", "a right parenthesis after expression to match this one"),yystack_[2].value);}
#line 2056 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;

  case 269: // parenExpr: '(' error
#line 692 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
               {yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:expressionAfterLeftParenthesis", "an expression after left parenthesis"),yystack_[1].value);}
#line 2062 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"
    break;


#line 2066 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught NelsonException: " << yyexc.what() << '\n';
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
        YYCDEBUG << "NelsonException caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an NelsonException.
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









  const short NelSonParser::yypact_ninf_ = -196;

  const short NelSonParser::yytable_ninf_ = -235;

  const short
  NelSonParser::yypact_[] =
  {
     445,  -196,    64,  -196,   146,   134,  -196,  1805,   124,    56,
    -196,  -196,  1839,  3015,  -196,  2907,  -196,  -196,  -196,  -196,
    -196,  3015,  3015,  1864,   739,  1891,   139,   809,    19,  -196,
       4,  1204,  -196,    44,  -196,  -196,   175,  -196,  -196,  -196,
    -196,  -196,  -196,  -196,  -196,  -196,  -196,  -196,  3175,  -196,
    -196,   145,   186,   145,  -196,  -196,  -196,  -196,    24,  -196,
    -196,  -196,   -39,   185,   197,   739,    72,   725,   145,  -196,
      27,   -18,    43,    61,  -196,    39,   195,    37,  -196,  2737,
    2737,  1316,   152,   152,  -196,  3079,  -196,  -196,  -196,   208,
    -196,  3175,    88,  1916,  3015,  -196,    41,  -196,   152,  -196,
    -196,    68,    60,  -196,   218,    29,  1916,  -196,  -196,  -196,
    -196,  -196,  -196,  -196,  -196,  -196,  -196,  -196,  1943,  1968,
    1995,  2020,  2047,  2072,  2099,  -196,  2124,  2151,  2176,  2203,
    2228,  2255,  2280,  2307,  2332,  2359,  2384,  2411,  2436,  2463,
    -196,  2703,   149,  2993,   145,  -196,  2488,  -196,   145,  -196,
     145,   138,  2515,   110,    72,  -196,  -196,  -196,  -196,  -196,
    1384,  -196,   238,  -196,  -196,    75,  -196,  2907,  -196,  -196,
    -196,  -196,  -196,    76,    10,  -196,  -196,    35,  2540,  -196,
      47,  2795,  2851,    55,  -196,  2963,   128,  -196,  -196,   189,
     695,   143,    41,  3015,  -196,  -196,  2567,  -196,   -33,  -196,
     155,   226,   778,    32,  -196,  3324,  -196,  3324,  -196,  3324,
    -196,   152,  -196,   152,  -196,   152,  -196,   152,  -196,  3229,
    -196,  3245,  -196,  3299,  -196,  3313,  -196,  3324,  -196,  3324,
    -196,  3324,  -196,  3375,  -196,   903,  -196,   903,  -196,   152,
    -196,   152,  -196,   152,  -196,   152,  -196,   270,  -196,  3175,
    -196,    12,  -196,  3015,    15,  -196,  -196,  3175,   230,  -196,
    -196,   655,   156,  -196,  1384,  -196,    46,     1,   868,  -196,
    -196,    69,   181,  -196,   135,  2907,  -196,  3175,  -196,  2592,
    -196,   515,  -196,   585,  3015,  -196,   126,   161,  -196,    50,
    1443,  -196,  -196,  2619,   241,  -196,   242,  1753,  3015,  3175,
    -196,  3175,  2644,    11,  -196,   252,  -196,   258,  1775,  -196,
     136,  -196,  -196,  2993,  3155,  -196,  -196,  -196,  1499,  -196,
    -196,  2907,    17,  -196,  -196,  -196,  -196,    46,   108,   924,
    -196,  3099,  -196,  -196,  -196,  -196,  2737,  1555,   176,  -196,
    1611,  -196,  3175,   262,  -196,  3175,  -196,   274,  2671,  -196,
    -196,   980,  -196,  -196,  1260,  -196,  2907,    20,  -196,  -196,
    1384,  1667,  -196,  -196,  1723,  -196,  3175,  -196,  1036,  1092,
    -196,  1260,  -196,  1384,  -196,  1148
  };

  const short
  NelSonParser::yydefact_[] =
  {
       0,     5,   235,   204,   205,   206,    68,     0,     0,     0,
      82,   207,     0,     0,    81,    51,    80,    79,    63,    67,
      66,     0,     0,     0,     0,     0,     0,     0,     0,    26,
       3,    51,    44,     0,    65,    64,    62,    61,    60,    55,
      54,    59,    58,    53,    56,    57,    49,    52,    50,   208,
     145,     0,   209,   194,    74,    69,    73,    72,     0,    71,
      70,   125,   235,   205,   206,     0,   130,     0,   209,    16,
       0,     0,     0,     0,   119,   113,     0,     0,   122,     0,
       0,    51,   183,   182,   269,     0,   212,   259,   258,   216,
     260,   265,     0,     0,     0,   256,   261,   185,   184,   200,
     195,     0,   197,   222,   221,     0,     0,   254,     1,    27,
      45,    46,    47,    48,    77,    78,    76,    75,     0,     0,
       0,     0,     0,     0,     0,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,     0,     0,     0,   193,   237,     0,   236,   192,   235,
     210,     0,     0,   137,   131,   132,   129,    95,    94,    96,
     128,    93,    17,    98,    97,     0,    30,    51,    24,    25,
      29,    34,    39,     0,     0,    37,    20,     0,     0,   117,
       0,    51,    51,    99,    91,    51,     0,   268,   267,   211,
       0,     0,   262,   263,   201,    42,     0,    43,     0,    40,
       0,   217,     0,     0,   167,   166,   171,   170,   173,   172,
     177,   176,   179,   178,   181,   180,   189,   188,   161,   160,
     163,   162,   157,   156,   159,   158,   165,   164,   169,   168,
     175,   174,   144,   143,   147,   146,   149,   148,   151,   150,
     153,   152,   155,   154,   187,   186,   247,     0,   240,   246,
     252,     0,   244,     0,     0,   238,   140,   139,   211,   135,
     134,     0,     0,   133,   127,    18,     0,     0,    51,    32,
      35,     0,     0,    21,     0,    51,   118,   112,   116,     0,
     108,     0,   120,     0,     0,    92,   107,   100,   101,   235,
      51,    84,    83,     0,   214,   257,   213,     0,   264,   266,
     203,   199,     0,     0,   196,   219,   255,   218,     0,   251,
     250,   241,   239,     0,     0,   243,   242,   138,    51,   126,
     124,    51,     0,    36,    31,    38,    22,     0,     0,    51,
     115,     0,   110,   109,   123,   121,     0,    51,     0,   102,
      51,   142,   141,   215,   202,   198,    41,   220,     0,   253,
     245,    51,    28,    19,    51,    15,    51,     0,   114,   111,
     104,    51,    90,    89,    51,   249,   248,    13,    51,    51,
      23,    51,    14,   103,    12,    51
  };

  const short
  NelSonParser::yypgoto_[] =
  {
    -196,  -196,   179,  -171,  -196,  -195,  -196,  -196,   -48,  -160,
     -60,   -13,    16,  -196,  -196,  -196,  -196,  -196,  -196,  -196,
    -196,  -196,  -196,  -196,  -196,   -76,  -174,  -196,  -196,   -24,
    -196,  -196,  -196,  -196,  -196,    92,  -196,  -196,   112,  -196,
    -196,  -196,    99,  -196,  -196,  -196,  -196,     0,   219,   -43,
     -28,   131,   199,    80,   -20,   237,   -15,   221,  -196
  };

  const short
  NelSonParser::yydefgoto_[] =
  {
       0,    28,    29,   167,    30,   355,    73,   174,   175,   198,
     199,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,   186,    42,   183,   160,   168,   286,   287,   288,
     338,    43,    77,    44,    45,    66,   153,   154,   155,   262,
      46,    47,    48,   102,    49,    50,    51,    68,   144,   147,
     250,   251,   105,    92,    93,    94,    95,    96,    53
  };

  const short
  NelSonParser::yytable_[] =
  {
      52,   181,    81,   182,   184,   267,   275,   106,   145,   285,
     145,   270,   107,   311,   195,    52,   315,   352,   353,   108,
     352,   370,   163,   164,     8,   163,   164,   149,   162,   302,
      58,    52,   163,   164,    87,    88,   273,    87,    88,    90,
     163,   164,   157,   158,   171,   170,   172,   110,   278,   111,
     112,   163,   164,    54,    55,   163,   164,    74,   150,    75,
     163,   164,   176,   322,   177,    56,    57,    54,    55,   194,
     323,   195,   190,    90,   312,   271,   265,   197,   195,    56,
      57,    52,    90,   284,    90,   202,   316,    90,   165,    90,
     166,   107,    90,    87,    88,   321,   274,   110,   152,    90,
     201,   255,   178,   307,   159,   255,    67,    90,   173,    90,
     279,    79,    80,    90,   328,   340,    90,    76,    90,    58,
      82,    83,    85,    91,    98,    69,    91,    70,   200,   291,
     196,   190,   324,    58,   197,   261,   326,   266,   195,   269,
      99,   197,   100,    87,    88,   151,   292,   264,    87,    88,
      60,   354,   252,   189,   268,   337,   356,   319,   304,   367,
      52,    59,   372,    71,    91,   124,   125,    52,   281,   283,
     357,   297,   290,   191,   320,   295,   374,   362,   114,   115,
      90,    52,    52,   308,   172,    52,   371,   306,    72,   284,
     116,   117,    91,    91,   363,  -224,   179,   327,   180,   348,
     101,   197,  -224,   258,  -224,    91,   141,  -223,   296,   109,
     253,   139,   140,   142,  -223,   143,  -223,   205,   207,   209,
     211,   213,   215,   217,   325,   219,   221,   223,   225,   227,
     229,   231,   233,   235,   237,   239,   241,   243,   245,   -33,
     249,   -33,   249,   346,   260,   257,  -223,   141,   318,   146,
    -225,    67,   293,  -223,   142,  -223,   143,  -225,  -224,  -225,
     360,    52,   329,   339,    52,  -224,   263,  -224,    52,  -229,
     113,   309,   148,   310,   254,    52,  -229,   277,  -229,  -234,
     110,    52,   295,    52,   110,   349,  -234,  -230,  -234,    91,
      52,  -225,   299,   306,  -230,   301,  -230,   110,  -225,   110,
    -225,    91,  -227,  -226,   161,   203,   110,   169,   351,  -227,
    -226,  -227,  -226,  -232,   161,   192,   161,   161,    52,  -231,
    -232,    52,  -232,  -228,   361,     0,  -231,   364,  -231,    52,
    -228,     0,  -228,   193,   110,  -233,     0,    52,     0,     0,
      52,   368,  -233,   369,  -233,   110,     0,   373,     0,     0,
       0,    52,   314,     0,    52,     0,    52,     0,   375,     0,
      52,    52,     0,     0,    52,     0,     0,   110,    52,    52,
       0,    52,     0,    52,     0,    52,     0,   110,   331,     0,
     110,     0,     0,   336,   110,   110,     0,     0,     0,   110,
       0,   110,   342,     0,     0,     0,    91,   299,     0,     0,
       0,   345,     0,     0,     0,     0,     0,    91,     0,     0,
       0,   272,   249,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   298,
       0,     0,     0,     0,     0,   303,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -4,     1,   366,     2,     3,
     -51,   -51,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     0,     7,     8,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,    20,   313,     0,
       0,   313,     0,     0,     0,     0,    21,    22,     0,     0,
       0,     0,     0,   169,   303,     0,    23,     0,     0,    24,
       0,    25,     0,     0,    26,    27,   332,   -51,     2,     3,
     -51,   -51,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     6,   333,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,    20,     0,   169,
       0,     0,     0,     0,   169,   303,    21,    22,     0,     0,
       0,     0,     0,   161,     0,     0,    23,     0,     0,    24,
       0,    25,     0,     0,    26,    27,   334,   -51,     2,     3,
     -51,   -51,     0,     0,   169,     0,     0,     0,     0,     0,
       4,     5,     6,   335,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,    20,     0,     0,
       0,     0,     0,     0,     0,     0,    21,    22,     0,     0,
       0,     0,     0,     0,     0,     0,    23,     0,     0,    24,
       0,    25,     0,     0,    26,    27,   317,   -51,     2,     3,
     -51,   -51,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     0,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,    20,    62,     3,
       0,     0,     0,     0,     0,     0,    21,    22,     0,     0,
      63,    64,     0,     0,     0,     0,    23,     0,    11,    24,
       0,    25,     0,     0,    26,    27,   156,   -51,     0,     0,
     157,   158,   118,   119,   120,   121,   122,   123,   124,   125,
      86,     0,    62,     3,    87,    88,    21,    22,     0,     0,
       0,     0,     0,     0,    63,    64,    23,     0,     0,    65,
     294,    25,    11,     0,    26,    27,     0,    90,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,    62,     3,     0,   139,   140,     0,     0,     0,     0,
      21,    22,   159,    63,    64,     0,     0,    90,     0,     0,
      23,    11,     0,    65,    89,    25,     0,     0,    26,    27,
     103,    90,    62,     3,    87,    88,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,    21,
      22,     0,    11,     0,     0,     0,     0,     0,     0,    23,
       0,     0,    65,     0,    25,     0,     0,    26,    27,   305,
      90,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      21,    22,     0,     0,     0,     0,     0,     0,    -9,     0,
      23,     2,     3,    65,     0,    25,     0,     0,    26,    27,
     104,    90,     0,     4,     5,     6,     0,     7,    -9,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
      20,     0,     0,   121,   122,   123,   124,   125,     0,    21,
      22,     0,     0,     0,    -8,     0,     0,     2,     3,    23,
       0,     0,    24,     0,    25,     0,     0,    26,    27,     4,
       5,     6,     0,     7,    -8,     9,    10,    11,    12,     0,
       0,    13,     0,     0,    14,    15,   136,   137,   138,     0,
      16,    17,   139,   140,    18,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    21,    22,     0,     0,     0,
     -11,     0,     0,     2,     3,    23,     0,     0,    24,     0,
      25,     0,     0,    26,    27,     4,     5,     6,     0,     7,
     -11,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,     0,     0,     0,     0,    16,    17,     0,     0,
      18,    19,    20,     0,     0,     0,     0,     0,     0,     0,
       0,    21,    22,     0,     0,     0,    -7,     0,     0,     2,
       3,    23,     0,     0,    24,     0,    25,     0,     0,    26,
      27,     4,     5,     6,     0,     7,    -7,     9,    10,    11,
      12,     0,     0,    13,     0,     0,    14,    15,     0,     0,
       0,     0,    16,    17,     0,     0,    18,    19,    20,     0,
       0,     0,     0,     0,     0,     0,     0,    21,    22,     0,
       0,     0,   -10,     0,     0,     2,     3,    23,     0,     0,
      24,     0,    25,     0,     0,    26,    27,     4,     5,     6,
       0,     7,   -10,     9,    10,    11,    12,     0,     0,    13,
       0,     0,    14,    15,     0,     0,     0,     0,    16,    17,
       0,     0,    18,    19,    20,     0,     0,     0,     0,     0,
       0,     0,     0,    21,    22,     0,     0,     0,    -6,     0,
       0,     2,     3,    23,     0,     0,    24,     0,    25,     0,
       0,    26,    27,     4,     5,     6,     0,     7,    -6,     9,
      10,    11,    12,     0,     0,    13,     0,     0,    14,    15,
       0,     0,     0,     0,    16,    17,     0,     0,    18,    19,
      20,     0,     0,     0,     0,     0,     0,     0,     0,    21,
      22,     0,     0,     0,    -2,     0,     0,     2,     3,    23,
       0,     0,    24,     0,    25,     0,     0,    26,    27,     4,
       5,     6,     0,     7,     0,     9,    10,    11,    12,     0,
       0,    13,     0,     0,    14,    15,     0,     0,     0,     0,
      16,    17,     0,     0,    18,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    21,    22,     0,     0,     0,
     352,     0,     0,     2,     3,    23,     0,     0,    24,     0,
      25,     0,     0,    26,    27,     4,     5,     6,     0,     7,
       0,     9,    10,    11,    12,     0,     0,    13,     0,     0,
      14,    15,     0,     0,     0,     0,    16,    17,     0,     0,
      18,    19,    20,     0,     0,     0,     0,     0,     0,     0,
       0,    21,    22,     0,     0,     0,     0,   -88,     0,     2,
       3,    23,     0,     0,    24,     0,    25,     0,     0,    26,
      27,     4,     5,     6,   -88,     7,     0,     9,    10,    11,
      12,     0,     0,    13,     0,     0,    14,    15,   185,     0,
       0,     0,    16,    17,     0,     0,    18,    19,    20,     0,
       0,     0,     0,     0,     0,     0,     0,    21,    22,     0,
       0,     0,     0,     0,     0,     0,     0,    23,     0,     0,
      24,     0,    25,     0,     0,    26,    27,     2,     3,   -51,
     -51,     0,     0,     0,     0,     0,     0,     0,     0,     4,
       5,     6,     0,     7,     0,     9,    10,    11,    12,     0,
       0,    13,     0,     0,    14,    15,     0,     0,     0,     0,
      16,    17,     0,     0,    18,    19,    20,     0,     0,     0,
       0,     0,     0,     0,     0,    21,    22,     0,     0,     0,
       0,     0,     0,     0,   -87,    23,     2,     3,    24,     0,
      25,     0,     0,    26,    27,     0,   -51,     0,     4,     5,
       6,   -87,     7,     0,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,     0,     0,     0,    16,
      17,     0,     0,    18,    19,    20,     0,     0,     0,     0,
       0,     0,     0,     0,    21,    22,     0,     0,     0,     0,
    -136,     0,     2,     3,    23,     0,     0,    24,     0,    25,
       0,     0,    26,    27,     4,     5,     6,  -136,     7,     0,
       9,    10,    11,    12,     0,     0,    13,     0,     0,    14,
      15,     0,     0,     0,     0,    16,    17,     0,     0,    18,
      19,    20,     0,     0,     0,     0,     0,     0,     0,     0,
      21,    22,     0,     0,     0,     0,  -106,     0,     2,     3,
      23,     0,     0,    24,     0,    25,     0,     0,    26,    27,
       4,     5,     6,  -106,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,    20,     0,     0,
       0,     0,     0,     0,     0,     0,    21,    22,     0,     0,
       0,     0,   -86,     0,     2,     3,    23,     0,     0,    24,
       0,    25,     0,     0,    26,    27,     4,     5,     6,   -86,
       7,     0,     9,    10,    11,    12,     0,     0,    13,     0,
       0,    14,    15,     0,     0,     0,     0,    16,    17,     0,
       0,    18,    19,    20,     0,     0,     0,     0,     0,     0,
       0,     0,    21,    22,     0,     0,     0,     0,  -105,     0,
       2,     3,    23,     0,     0,    24,     0,    25,     0,     0,
      26,    27,     4,     5,     6,  -105,     7,     0,     9,    10,
      11,    12,     0,     0,    13,     0,     0,    14,    15,     0,
       0,     0,     0,    16,    17,     0,     0,    18,    19,    20,
       0,     0,     0,     0,     0,     0,     0,     0,    21,    22,
       0,     0,     0,     0,   -85,     0,     2,     3,    23,     0,
       0,    24,     0,    25,     0,     0,    26,    27,     4,     5,
       6,   -85,     7,     0,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,    62,     3,     0,    16,
      17,     0,     0,    18,    19,    20,     0,     0,    63,    64,
       0,     0,     0,     0,    21,    22,    11,     0,    62,     3,
       0,     0,     0,     0,    23,     0,     0,    24,     0,    25,
      63,    64,    26,    27,     0,     0,     0,     0,    11,     0,
       0,     0,     0,     0,    21,    22,    61,     0,    62,     3,
       0,     0,     0,     0,    23,     0,     0,    65,   343,    25,
      63,    64,    26,    27,     0,    90,    21,    22,    11,     0,
       0,     0,     0,     0,     0,     0,    23,     0,     0,    65,
      78,    25,    62,     3,    26,    27,   347,    90,     0,     0,
       0,     0,     0,     0,    63,    64,    21,    22,     0,     0,
       0,     0,    11,     0,     0,    84,    23,    62,     3,    65,
       0,    25,     0,     0,    26,    27,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,    11,     0,     0,
      21,    22,    97,     0,    62,     3,     0,     0,     0,     0,
      23,     0,     0,    65,     0,    25,    63,    64,    26,    27,
       0,     0,     0,     0,    11,    21,    22,     0,     0,    62,
       3,     0,     0,     0,     0,    23,     0,     0,    65,     0,
      25,    63,    64,    26,    27,     0,     0,     0,     0,    11,
       0,     0,    21,    22,   204,     0,    62,     3,     0,     0,
       0,     0,    23,     0,     0,    65,     0,    25,    63,    64,
      26,    27,     0,     0,     0,     0,    11,    21,    22,   206,
       0,    62,     3,     0,     0,     0,     0,    23,     0,     0,
      65,     0,    25,    63,    64,    26,    27,     0,    90,     0,
       0,    11,     0,     0,    21,    22,   208,     0,    62,     3,
       0,     0,     0,     0,    23,     0,     0,    65,     0,    25,
      63,    64,    26,    27,     0,     0,     0,     0,    11,    21,
      22,   210,     0,    62,     3,     0,     0,     0,     0,    23,
       0,     0,    65,     0,    25,    63,    64,    26,    27,     0,
       0,     0,     0,    11,     0,     0,    21,    22,   212,     0,
      62,     3,     0,     0,     0,     0,    23,     0,     0,    65,
       0,    25,    63,    64,    26,    27,     0,     0,     0,     0,
      11,    21,    22,   214,     0,    62,     3,     0,     0,     0,
       0,    23,     0,     0,    65,     0,    25,    63,    64,    26,
      27,     0,     0,     0,     0,    11,     0,     0,    21,    22,
     216,     0,    62,     3,     0,     0,     0,     0,    23,     0,
       0,    65,     0,    25,    63,    64,    26,    27,     0,     0,
       0,     0,    11,    21,    22,   218,     0,    62,     3,     0,
       0,     0,     0,    23,     0,     0,    65,     0,    25,    63,
      64,    26,    27,     0,     0,     0,     0,    11,     0,     0,
      21,    22,   220,     0,    62,     3,     0,     0,     0,     0,
      23,     0,     0,    65,     0,    25,    63,    64,    26,    27,
       0,     0,     0,     0,    11,    21,    22,   222,     0,    62,
       3,     0,     0,     0,     0,    23,     0,     0,    65,     0,
      25,    63,    64,    26,    27,     0,     0,     0,     0,    11,
       0,     0,    21,    22,   224,     0,    62,     3,     0,     0,
       0,     0,    23,     0,     0,    65,     0,    25,    63,    64,
      26,    27,     0,     0,     0,     0,    11,    21,    22,   226,
       0,    62,     3,     0,     0,     0,     0,    23,     0,     0,
      65,     0,    25,    63,    64,    26,    27,     0,     0,     0,
       0,    11,     0,     0,    21,    22,   228,     0,    62,     3,
       0,     0,     0,     0,    23,     0,     0,    65,     0,    25,
      63,    64,    26,    27,     0,     0,     0,     0,    11,    21,
      22,   230,     0,    62,     3,     0,     0,     0,     0,    23,
       0,     0,    65,     0,    25,    63,    64,    26,    27,     0,
       0,     0,     0,    11,     0,     0,    21,    22,   232,     0,
      62,     3,     0,     0,     0,     0,    23,     0,     0,    65,
       0,    25,    63,    64,    26,    27,     0,     0,     0,     0,
      11,    21,    22,   234,     0,    62,     3,     0,     0,     0,
       0,    23,     0,     0,    65,     0,    25,    63,    64,    26,
      27,     0,     0,     0,     0,    11,     0,     0,    21,    22,
     236,     0,    62,     3,     0,     0,     0,     0,    23,     0,
       0,    65,     0,    25,    63,    64,    26,    27,     0,     0,
       0,     0,    11,    21,    22,   238,     0,    62,     3,     0,
       0,     0,     0,    23,     0,     0,    65,     0,    25,    63,
      64,    26,    27,     0,     0,     0,     0,    11,     0,     0,
      21,    22,   240,     0,    62,     3,     0,     0,     0,     0,
      23,     0,     0,    65,     0,    25,    63,    64,    26,    27,
       0,     0,     0,     0,    11,    21,    22,   242,     0,    62,
       3,     0,     0,     0,     0,    23,     0,     0,    65,     0,
      25,    63,    64,    26,    27,     0,     0,     0,     0,    11,
       0,     0,    21,    22,   244,     0,    62,     3,     0,     0,
       0,     0,    23,     0,     0,    65,     0,    25,    63,    64,
      26,    27,     0,     0,     0,     0,    11,    21,    22,   256,
       0,    62,     3,     0,     0,     0,     0,    23,     0,     0,
      65,     0,    25,    63,    64,    26,    27,     0,     0,     0,
       0,    11,     0,     0,    21,    22,   259,     0,    62,     3,
       0,     0,     0,     0,    23,     0,     0,    65,     0,    25,
      63,    64,    26,    27,     0,     0,     0,     0,    11,    21,
      22,   276,     0,    62,     3,     0,     0,     0,     0,    23,
       0,     0,    65,     0,    25,    63,    64,    26,    27,     0,
       0,     0,     0,    11,     0,     0,    21,    22,   300,     0,
      62,     3,     0,     0,     0,     0,    23,     0,     0,    65,
       0,    25,    63,    64,    26,    27,     0,     0,     0,     0,
      11,    21,    22,   330,     0,    62,     3,     0,     0,     0,
       0,    23,     0,     0,    65,     0,    25,    63,    64,    26,
      27,     0,     0,     0,     0,    11,     0,     0,    21,    22,
     341,     0,    62,     3,     0,     0,     0,     0,    23,     0,
       0,    65,     0,    25,    63,    64,    26,    27,     0,     0,
       0,     0,    11,    21,    22,   344,     0,    62,     3,     0,
       0,     0,     0,    23,     0,     0,    65,     0,    25,    63,
      64,    26,    27,     0,     0,     0,     0,    11,     0,     0,
      21,    22,   365,     0,    62,     3,     0,     0,     0,     0,
      23,     0,     0,    65,     0,    25,    63,    64,    26,    27,
       0,     0,     0,     0,    11,    21,    22,     0,     0,     0,
       0,     0,     0,     0,     0,    23,    62,     3,    65,     0,
      25,     0,     0,    26,    27,     0,     0,     0,    63,    64,
       0,     0,    21,    22,     0,     0,    11,     0,     0,     0,
       0,     0,    23,     0,     0,    65,     0,    25,     0,     0,
      26,    27,   157,   158,   118,   119,   120,   121,   122,   123,
     124,   125,     0,   246,    21,    22,     0,   247,     0,     0,
       0,     0,     0,     0,    23,   248,     0,    65,     0,    25,
       0,     0,    26,    27,     0,     0,     0,     0,     0,     0,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   137,   138,     0,     0,     0,   139,   140,     2,     3,
       0,     0,     0,     0,   159,     0,     0,     0,     0,    90,
       4,     5,     6,   280,     7,     0,     9,    10,    11,    12,
       0,     0,    13,     0,     0,    14,    15,     0,     0,     0,
       0,    16,    17,     0,     0,    18,    19,    20,     0,     0,
       0,     0,     0,     0,     0,     0,    21,    22,     0,     0,
       0,     0,     0,     0,     2,     3,    23,     0,     0,    24,
       0,    25,     0,     0,    26,    27,     4,     5,     6,   282,
       7,     0,     9,    10,    11,    12,     0,     0,    13,     0,
       0,    14,    15,     0,     0,     0,     0,    16,    17,     0,
       0,    18,    19,    20,     0,     0,     0,     0,     0,     0,
       0,     0,    21,    22,     0,     0,     0,     0,     0,     0,
       2,     3,    23,     0,     0,    24,     0,    25,     0,     0,
      26,    27,     4,     5,     6,     0,     7,     0,     9,    10,
      11,    12,     0,     0,    13,     0,     0,    14,    15,     0,
       0,     0,     0,    16,    17,     0,     0,    18,    19,    20,
       0,     0,     0,     0,     0,     0,     0,     0,    21,    22,
       0,     0,     0,     0,     0,     0,   289,     3,    23,     0,
       0,    24,     0,    25,     0,     0,    26,    27,     4,     5,
       6,     0,     7,     0,     9,    10,    11,    12,     0,     0,
      13,     0,     0,    14,    15,     0,    62,     3,     0,    16,
      17,     0,     0,    18,    19,    20,     0,     0,    63,    64,
       0,     0,     0,     0,    21,    22,    11,     0,    62,     3,
       0,     0,     0,     0,    23,     0,     0,    24,     0,    25,
      63,    64,    26,    27,     0,     0,     0,     0,    11,     0,
       0,     0,     0,   246,    21,    22,     0,   247,     0,     0,
       0,     0,     0,     0,    23,     0,     0,    65,     0,    25,
       0,     0,    26,    27,     0,     0,    21,    22,     0,     0,
       0,     0,     0,     0,     0,     0,    23,     0,     0,    65,
     187,    25,     0,     0,    26,    27,   118,   119,   120,   121,
     122,   123,   124,   125,     0,     0,     0,     0,     0,     0,
     358,     0,     0,     0,     0,     0,   118,   119,   120,   121,
     122,   123,   124,   125,     0,     0,     0,     0,     0,     0,
       0,     0,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,     0,     0,     0,   139,   140,
       0,   188,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,     0,     0,     0,   139,   140,
       0,   359,   118,   119,   120,   121,   122,   123,   124,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,   119,   120,   121,   122,   123,   124,   125,
       0,     0,     0,     0,     0,     0,     0,     0,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,     0,     0,     0,   139,   140,     0,   350,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,     0,     0,     0,   139,   140,   118,   119,   120,   121,
     122,   123,   124,   125,     0,     0,     0,     0,     0,     0,
       0,     0,   118,   119,   120,   121,   122,   123,   124,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,     0,     0,     0,   139,   140,
     128,   129,   130,   131,   132,   133,   134,   135,   136,   137,
     138,     0,     0,     0,   139,   140,   118,   119,   120,   121,
     122,   123,   124,   125,     0,     0,     0,     0,     0,     0,
     118,   119,   120,   121,   122,   123,   124,   125,     0,     0,
       0,     0,     0,     0,   121,   122,   123,   124,   125,     0,
       0,     0,     0,     0,     0,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,     0,     0,     0,   139,   140,
     130,   131,   132,   133,   134,   135,   136,   137,   138,     0,
       0,     0,   139,   140,   133,   134,   135,   136,   137,   138,
       0,     0,     0,   139,   140,   121,   122,   123,   124,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   134,   135,   136,   137,
     138,     0,     0,     0,   139,   140
  };

  const short
  NelSonParser::yycheck_[] =
  {
       0,    77,    15,    79,    80,   165,   177,    27,    51,   183,
      53,     1,    27,     1,     3,    15,     1,     0,     1,     0,
       0,     1,     5,     6,    20,     5,     6,     3,     1,    62,
      69,    31,     5,     6,     5,     6,     1,     5,     6,    72,
       5,     6,     5,     6,     1,    63,     3,    31,     1,     5,
       6,     5,     6,     3,     4,     5,     6,     1,    58,     3,
       5,     6,     1,    62,     3,    15,    16,     3,     4,     1,
       1,     3,    92,    72,    62,    65,     1,    66,     3,    15,
      16,    81,    72,    28,    72,   105,    71,    72,    61,    72,
      63,   106,    72,     5,     6,   266,    61,    81,    26,    72,
      71,   144,    63,    71,    67,   148,     7,    72,    65,    72,
      63,    12,    13,    72,   274,   289,    72,    61,    72,    69,
      21,    22,    23,    24,    25,     1,    27,     3,    68,     1,
      62,   151,    63,    69,    66,    25,     1,    62,     3,    63,
       1,    66,     3,     5,     6,    65,    18,   160,     5,     6,
      16,   322,     3,    65,   167,    29,   327,     1,     3,   354,
     160,    15,   357,    39,    65,    13,    14,   167,   181,   182,
      62,   191,   185,    93,    18,   190,   371,     1,     3,     4,
      72,   181,   182,   203,     3,   185,   357,   202,    64,    28,
      15,    16,    93,    94,    18,    61,     1,    62,     3,    63,
      61,    66,    68,    65,    70,   106,    61,    61,    65,    30,
      61,    59,    60,    68,    68,    70,    70,   118,   119,   120,
     121,   122,   123,   124,   272,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,     1,
     141,     3,   143,   303,   152,   146,    61,    61,   261,    63,
      61,   152,    63,    68,    68,    70,    70,    68,    61,    70,
     336,   261,   275,   287,   264,    68,   154,    70,   268,    61,
      33,     1,    53,     3,   143,   275,    68,   178,    70,    61,
     264,   281,   297,   283,   268,   313,    68,    61,    70,   190,
     290,    61,   193,   308,    68,   196,    70,   281,    68,   283,
      70,   202,    61,    61,    67,   106,   290,    70,   321,    68,
      68,    70,    70,    61,    77,    94,    79,    80,   318,    61,
      68,   321,    70,    61,   337,    -1,    68,   340,    70,   329,
      68,    -1,    70,    96,   318,    61,    -1,   337,    -1,    -1,
     340,   354,    68,   356,    70,   329,    -1,   360,    -1,    -1,
      -1,   351,   253,    -1,   354,    -1,   356,    -1,   371,    -1,
     360,   361,    -1,    -1,   364,    -1,    -1,   351,   368,   369,
      -1,   371,    -1,   373,    -1,   375,    -1,   361,   279,    -1,
     364,    -1,    -1,   284,   368,   369,    -1,    -1,    -1,   373,
      -1,   375,   293,    -1,    -1,    -1,   297,   298,    -1,    -1,
      -1,   302,    -1,    -1,    -1,    -1,    -1,   308,    -1,    -1,
      -1,   174,   313,    -1,   177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,   198,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     0,     1,   348,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,   251,    -1,
      -1,   254,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,    -1,   266,   267,    -1,    61,    -1,    -1,    64,
      -1,    66,    -1,    -1,    69,    70,     1,    72,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,   322,
      -1,    -1,    -1,    -1,   327,   328,    51,    52,    -1,    -1,
      -1,    -1,    -1,   336,    -1,    -1,    61,    -1,    -1,    64,
      -1,    66,    -1,    -1,    69,    70,     1,    72,     3,     4,
       5,     6,    -1,    -1,   357,    -1,    -1,    -1,    -1,    -1,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
      -1,    66,    -1,    -1,    69,    70,     1,    72,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,     3,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      15,    16,    -1,    -1,    -1,    -1,    61,    -1,    23,    64,
      -1,    66,    -1,    -1,    69,    70,     1,    72,    -1,    -1,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       1,    -1,     3,     4,     5,     6,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    61,    -1,    -1,    64,
      65,    66,    23,    -1,    69,    70,    -1,    72,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     3,     4,    -1,    59,    60,    -1,    -1,    -1,    -1,
      51,    52,    67,    15,    16,    -1,    -1,    72,    -1,    -1,
      61,    23,    -1,    64,    65,    66,    -1,    -1,    69,    70,
       1,    72,     3,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    -1,    -1,    -1,    51,
      52,    -1,    23,    -1,    -1,    -1,    -1,    -1,    -1,    61,
      -1,    -1,    64,    -1,    66,    -1,    -1,    69,    70,    71,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,
      61,     3,     4,    64,    -1,    66,    -1,    -1,    69,    70,
      71,    72,    -1,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      42,    -1,    -1,    10,    11,    12,    13,    14,    -1,    51,
      52,    -1,    -1,    -1,     0,    -1,    -1,     3,     4,    61,
      -1,    -1,    64,    -1,    66,    -1,    -1,    69,    70,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,    24,    -1,
      -1,    27,    -1,    -1,    30,    31,    53,    54,    55,    -1,
      36,    37,    59,    60,    40,    41,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
       0,    -1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,
      66,    -1,    -1,    69,    70,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,     0,    -1,    -1,     3,
       4,    61,    -1,    -1,    64,    -1,    66,    -1,    -1,    69,
      70,    15,    16,    17,    -1,    19,    20,    21,    22,    23,
      24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,
      -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,     0,    -1,    -1,     3,     4,    61,    -1,    -1,
      64,    -1,    66,    -1,    -1,    69,    70,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    24,    -1,    -1,    27,
      -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,
      -1,    -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,     0,    -1,
      -1,     3,     4,    61,    -1,    -1,    64,    -1,    66,    -1,
      -1,    69,    70,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,
      -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,
      42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,
      52,    -1,    -1,    -1,     0,    -1,    -1,     3,     4,    61,
      -1,    -1,    64,    -1,    66,    -1,    -1,    69,    70,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,
      36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
       0,    -1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,
      66,    -1,    -1,    69,    70,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,
      30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,
      40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,     3,
       4,    61,    -1,    -1,    64,    -1,    66,    -1,    -1,    69,
      70,    15,    16,    17,    18,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    27,    -1,    -1,    30,    31,    32,    -1,
      -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,
      64,    -1,    66,    -1,    -1,    69,    70,     3,     4,     5,
       6,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,
      36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    61,     3,     4,    64,    -1,
      66,    -1,    -1,    69,    70,    -1,    72,    -1,    15,    16,
      17,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,    -1,    -1,    -1,    36,
      37,    -1,    -1,    40,    41,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,    61,    -1,    -1,    64,    -1,    66,
      -1,    -1,    69,    70,    15,    16,    17,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    27,    -1,    -1,    30,
      31,    -1,    -1,    -1,    -1,    36,    37,    -1,    -1,    40,
      41,    42,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,
      61,    -1,    -1,    64,    -1,    66,    -1,    -1,    69,    70,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,     1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    66,    -1,    -1,    69,    70,    15,    16,    17,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    66,    -1,    -1,
      69,    70,    15,    16,    17,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    -1,     1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    66,    -1,    -1,    69,    70,    15,    16,
      17,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,     3,     4,    -1,    36,
      37,    -1,    -1,    40,    41,    42,    -1,    -1,    15,    16,
      -1,    -1,    -1,    -1,    51,    52,    23,    -1,     3,     4,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,
      15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    -1,    51,    52,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    65,    66,
      15,    16,    69,    70,    -1,    72,    51,    52,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
       1,    66,     3,     4,    69,    70,    71,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    51,    52,    -1,    -1,
      -1,    -1,    23,    -1,    -1,     1,    61,     3,     4,    64,
      -1,    66,    -1,    -1,    69,    70,    -1,    -1,    -1,    15,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    23,    -1,    -1,
      51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      61,    -1,    -1,    64,    -1,    66,    15,    16,    69,    70,
      -1,    -1,    -1,    -1,    23,    51,    52,    -1,    -1,     3,
       4,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      66,    15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,
      -1,    -1,    51,    52,     1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    66,    15,    16,
      69,    70,    -1,    -1,    -1,    -1,    23,    51,    52,     1,
      -1,     3,     4,    -1,    -1,    -1,    -1,    61,    -1,    -1,
      64,    -1,    66,    15,    16,    69,    70,    -1,    72,    -1,
      -1,    23,    -1,    -1,    51,    52,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,
      15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,    51,
      52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    61,
      -1,    -1,    64,    -1,    66,    15,    16,    69,    70,    -1,
      -1,    -1,    -1,    23,    -1,    -1,    51,    52,     1,    -1,
       3,     4,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
      -1,    66,    15,    16,    69,    70,    -1,    -1,    -1,    -1,
      23,    51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    61,    -1,    -1,    64,    -1,    66,    15,    16,    69,
      70,    -1,    -1,    -1,    -1,    23,    -1,    -1,    51,    52,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    64,    -1,    66,    15,    16,    69,    70,    -1,    -1,
      -1,    -1,    23,    51,    52,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,    15,
      16,    69,    70,    -1,    -1,    -1,    -1,    23,    -1,    -1,
      51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      61,    -1,    -1,    64,    -1,    66,    15,    16,    69,    70,
      -1,    -1,    -1,    -1,    23,    51,    52,     1,    -1,     3,
       4,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      66,    15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,
      -1,    -1,    51,    52,     1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    66,    15,    16,
      69,    70,    -1,    -1,    -1,    -1,    23,    51,    52,     1,
      -1,     3,     4,    -1,    -1,    -1,    -1,    61,    -1,    -1,
      64,    -1,    66,    15,    16,    69,    70,    -1,    -1,    -1,
      -1,    23,    -1,    -1,    51,    52,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,
      15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,    51,
      52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    61,
      -1,    -1,    64,    -1,    66,    15,    16,    69,    70,    -1,
      -1,    -1,    -1,    23,    -1,    -1,    51,    52,     1,    -1,
       3,     4,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
      -1,    66,    15,    16,    69,    70,    -1,    -1,    -1,    -1,
      23,    51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    61,    -1,    -1,    64,    -1,    66,    15,    16,    69,
      70,    -1,    -1,    -1,    -1,    23,    -1,    -1,    51,    52,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    64,    -1,    66,    15,    16,    69,    70,    -1,    -1,
      -1,    -1,    23,    51,    52,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,    15,
      16,    69,    70,    -1,    -1,    -1,    -1,    23,    -1,    -1,
      51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      61,    -1,    -1,    64,    -1,    66,    15,    16,    69,    70,
      -1,    -1,    -1,    -1,    23,    51,    52,     1,    -1,     3,
       4,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,
      66,    15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,
      -1,    -1,    51,    52,     1,    -1,     3,     4,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    66,    15,    16,
      69,    70,    -1,    -1,    -1,    -1,    23,    51,    52,     1,
      -1,     3,     4,    -1,    -1,    -1,    -1,    61,    -1,    -1,
      64,    -1,    66,    15,    16,    69,    70,    -1,    -1,    -1,
      -1,    23,    -1,    -1,    51,    52,     1,    -1,     3,     4,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,
      15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,    51,
      52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,    61,
      -1,    -1,    64,    -1,    66,    15,    16,    69,    70,    -1,
      -1,    -1,    -1,    23,    -1,    -1,    51,    52,     1,    -1,
       3,     4,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
      -1,    66,    15,    16,    69,    70,    -1,    -1,    -1,    -1,
      23,    51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,
      -1,    61,    -1,    -1,    64,    -1,    66,    15,    16,    69,
      70,    -1,    -1,    -1,    -1,    23,    -1,    -1,    51,    52,
       1,    -1,     3,     4,    -1,    -1,    -1,    -1,    61,    -1,
      -1,    64,    -1,    66,    15,    16,    69,    70,    -1,    -1,
      -1,    -1,    23,    51,    52,     1,    -1,     3,     4,    -1,
      -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,    15,
      16,    69,    70,    -1,    -1,    -1,    -1,    23,    -1,    -1,
      51,    52,     1,    -1,     3,     4,    -1,    -1,    -1,    -1,
      61,    -1,    -1,    64,    -1,    66,    15,    16,    69,    70,
      -1,    -1,    -1,    -1,    23,    51,    52,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    61,     3,     4,    64,    -1,
      66,    -1,    -1,    69,    70,    -1,    -1,    -1,    15,    16,
      -1,    -1,    51,    52,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    -1,    61,    -1,    -1,    64,    -1,    66,    -1,    -1,
      69,    70,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    -1,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    61,    62,    -1,    64,    -1,    66,
      -1,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    -1,    -1,    -1,    59,    60,     3,     4,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    72,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    -1,    -1,    30,    31,    -1,    -1,    -1,
      -1,    36,    37,    -1,    -1,    40,    41,    42,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,    61,    -1,    -1,    64,
      -1,    66,    -1,    -1,    69,    70,    15,    16,    17,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    27,    -1,
      -1,    30,    31,    -1,    -1,    -1,    -1,    36,    37,    -1,
      -1,    40,    41,    42,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,    61,    -1,    -1,    64,    -1,    66,    -1,    -1,
      69,    70,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    27,    -1,    -1,    30,    31,    -1,
      -1,    -1,    -1,    36,    37,    -1,    -1,    40,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,    61,    -1,
      -1,    64,    -1,    66,    -1,    -1,    69,    70,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      27,    -1,    -1,    30,    31,    -1,     3,     4,    -1,    36,
      37,    -1,    -1,    40,    41,    42,    -1,    -1,    15,    16,
      -1,    -1,    -1,    -1,    51,    52,    23,    -1,     3,     4,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,
      15,    16,    69,    70,    -1,    -1,    -1,    -1,    23,    -1,
      -1,    -1,    -1,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    61,    -1,    -1,    64,    -1,    66,
      -1,    -1,    69,    70,    -1,    -1,    51,    52,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    61,    -1,    -1,    64,
       1,    66,    -1,    -1,    69,    70,     7,     8,     9,    10,
      11,    12,    13,    14,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,     7,     8,     9,    10,
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
      -1,    -1,    -1,    -1,    10,    11,    12,    13,    14,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    -1,    -1,    59,    60,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    -1,
      -1,    -1,    59,    60,    50,    51,    52,    53,    54,    55,
      -1,    -1,    -1,    59,    60,    10,    11,    12,    13,    14,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
      55,    -1,    -1,    -1,    59,    60
  };

  const unsigned char
  NelSonParser::yystos_[] =
  {
       0,     1,     3,     4,    15,    16,    17,    19,    20,    21,
      22,    23,    24,    27,    30,    31,    36,    37,    40,    41,
      42,    51,    52,    61,    64,    66,    69,    70,    74,    75,
      77,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    96,   104,   106,   107,   113,   114,   115,   117,
     118,   119,   120,   131,     3,     4,    15,    16,    69,    15,
      16,     1,     3,    15,    16,    64,   108,   115,   120,     1,
       3,    39,    64,    79,     1,     3,    61,   105,     1,   115,
     115,    84,   115,   115,     1,   115,     1,     5,     6,    65,
      72,   115,   126,   127,   128,   129,   130,     1,   115,     1,
       3,    61,   116,     1,    71,   125,   127,   129,     0,    75,
      85,     5,     6,   128,     3,     4,    15,    16,     7,     8,
       9,    10,    11,    12,    13,    14,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    59,
      60,    61,    68,    70,   121,   122,    63,   122,   121,     3,
     120,   126,    26,   109,   110,   111,     1,     5,     6,    67,
      98,   128,     1,     5,     6,    61,    63,    76,    99,   128,
      63,     1,     3,    65,    80,    81,     1,     3,    63,     1,
       3,    98,    98,    97,    98,    32,    95,     1,    62,    65,
     127,   126,   130,   128,     1,     3,    62,    66,    82,    83,
      68,    71,   127,   125,     1,   115,     1,   115,     1,   115,
       1,   115,     1,   115,     1,   115,     1,   115,     1,   115,
       1,   115,     1,   115,     1,   115,     1,   115,     1,   115,
       1,   115,     1,   115,     1,   115,     1,   115,     1,   115,
       1,   115,     1,   115,     1,   115,    50,    54,    62,   115,
     123,   124,     3,    61,   124,   122,     1,   115,    65,     1,
     108,    25,   112,   111,    84,     1,    62,    82,    84,    63,
       1,    65,   128,     1,    61,    76,     1,   115,     1,    63,
      18,    84,    18,    84,    28,    99,   100,   101,   102,     3,
      84,     1,    18,    63,    65,   129,    65,   127,   128,   115,
       1,   115,    62,   128,     3,    71,   129,    71,   127,     1,
       3,     1,    62,   128,   115,     1,    71,     1,    84,     1,
      18,    76,    62,     1,    63,    81,     1,    62,    82,    84,
       1,   115,     1,    18,     1,    18,   115,    29,   103,   102,
      99,     1,   115,    65,     1,   115,    83,    71,    63,   123,
      62,    84,     0,     1,    76,    78,    76,    62,     1,    62,
      98,    84,     1,    18,    84,     1,   115,    78,    84,    84,
       1,    76,    78,    84,    78,    84
  };

  const unsigned char
  NelSonParser::yyr1_[] =
  {
       0,    73,    74,    74,    74,    74,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    76,    76,    77,    77,    78,    79,
      79,    79,    79,    79,    79,    79,    79,    80,    80,    81,
      82,    82,    83,    83,    84,    84,    85,    85,    85,    86,
      86,    86,    86,    86,    86,    86,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    87,    88,    89,    89,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    90,
      91,    92,    93,    94,    94,    95,    95,    95,    95,    96,
      96,    97,    97,    98,    98,    98,    98,    99,    99,   100,
     100,   101,   101,   102,   102,   103,   103,   103,   104,   104,
     104,   105,   105,   105,   105,   105,   105,   105,   105,   105,
     106,   106,   106,   106,   107,   107,   107,   108,   108,   108,
     109,   109,   110,   110,   111,   111,   112,   112,   112,   113,
     113,   114,   114,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   115,   115,   116,   116,   117,   117,   117,
     117,   117,   117,   117,   118,   118,   118,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
     118,   118,   118,   119,   119,   119,   119,   119,   119,   119,
     119,   119,   119,   119,   119,   120,   120,   121,   121,   122,
     122,   122,   122,   122,   122,   122,   123,   123,   123,   123,
     123,   123,   124,   124,   125,   125,   126,   126,   127,   127,
     128,   129,   129,   129,   129,   130,   130,   131,   131,   131
  };

  const signed char
  NelSonParser::yyr2_[] =
  {
       0,     2,     1,     1,     0,     1,     8,     7,     5,     4,
       7,     6,     8,     7,     7,     6,     2,     3,     4,     6,
       3,     4,     5,     7,     1,     1,     1,     2,     1,     2,
       2,     4,     3,     2,     2,     3,     4,     1,     3,     1,
       1,     3,     1,     1,     1,     2,     2,     2,     2,     1,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       1,     1,     1,     4,     4,     4,     3,     2,     0,     6,
       6,     1,     2,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     4,     3,     2,     1,     0,     4,     5,
       5,     5,     3,     1,     5,     4,     3,     2,     3,     1,
       4,     5,     2,     5,     5,     2,     5,     3,     2,     2,
       0,     1,     1,     2,     2,     2,     2,     0,     2,     3,
       3,     5,     5,     3,     3,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     2,     2,     3,     3,     3,     3,
       2,     2,     2,     2,     1,     1,     3,     2,     5,     4,
       2,     3,     5,     4,     1,     1,     1,     1,     1,     1,
       3,     3,     2,     4,     4,     5,     2,     3,     4,     4,
       5,     2,     2,     1,     1,     3,     4,     4,     5,     2,
       3,     4,     4,     5,     2,     1,     2,     1,     2,     3,
       2,     3,     3,     3,     2,     4,     1,     1,     4,     4,
       2,     2,     1,     3,     1,     3,     1,     3,     1,     1,
       1,     1,     2,     2,     3,     1,     3,     3,     3,     2
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
  "VARARGIN", "VARARGOUT", "ABORT", "ENDFUNCTION", "ARGUMENTS", "SOR",
  "SAND", "'|'", "'&'", "'<'", "'>'", "NE", "':'", "'+'", "'-'", "'*'",
  "'/'", "'\\\\'", "POS", "NEG", "NOT", "'^'", "'\\''", "'('", "')'",
  "'='", "'['", "']'", "'~'", "';'", "'.'", "'@'", "'{'", "'}'", "','",
  "$accept", "program", "functionDef", "functionSignatureTerminator",
  "functionDefList", "emptyFunctionBody", "returnDeclaration",
  "argumentList", "argument", "inputArgumentList", "inputArgument",
  "statementList", "statement", "statementType", "argumentsStatement",
  "functionTerminatorStatement", "specialSyntaxStatement",
  "returnStatement", "pauseStatement", "continueStatement",
  "breakStatement", "tryStatement", "optionalCatch", "switchStatement",
  "switchExprTerminator", "optionalEndStatement", "newLine", "caseBlock",
  "caseList", "caseStatement", "otherwiseClause", "forStatement",
  "forIndexExpression", "whileStatement", "ifStatement",
  "conditionedStatement", "elseIfBlock", "elseIfStatementList",
  "elseIfStatement", "elseStatement", "assignmentStatement",
  "multiFunctionCall", "expr", "namedHandleTarget", "anonymousFunction",
  "terminal", "postfixableLiteral", "symbRefList", "postfixRefList",
  "symbRef", "indexElement", "indexList", "cellDef", "matrixDef",
  "rowSeparator", "columnSep", "rowDef", "rowElements", "parenExpr", YY_NULLPTR
  };
#endif


#if YYDEBUG
  const short
  NelSonParser::yyrline_[] =
  {
       0,    93,    93,    94,    94,    95,    99,   106,   114,   122,
     131,   139,   148,   156,   165,   173,   182,   183,   184,   185,
     186,   187,   188,   189,   196,   197,   201,   202,   206,   213,
     214,   215,   216,   217,   218,   219,   220,   224,   225,   229,
     233,   234,   238,   239,   243,   244,   248,   252,   256,   263,
     264,   265,   266,   267,   268,   269,   270,   271,   272,   273,
     274,   275,   276,   277,   278,   279,   283,   287,   290,   291,
     292,   293,   294,   295,   296,   297,   298,   299,   300,   303,
     307,   311,   314,   318,   324,   329,   333,   340,   341,   345,
     351,   362,   363,   367,   368,   369,   370,   374,   375,   380,
     381,   385,   388,   394,   397,   403,   406,   409,   415,   419,
     424,   429,   430,   431,   433,   434,   435,   436,   437,   438,
     442,   446,   451,   452,   456,   462,   463,   467,   470,   473,
     477,   478,   482,   485,   491,   494,   498,   501,   502,   506,
     507,   511,   514,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   573,   574,   578,   579,   580,
     581,   582,   583,   584,   587,   588,   589,   590,   591,   592,
     593,   594,   595,   596,   597,   598,   599,   600,   601,   602,
     603,   604,   605,   609,   610,   611,   612,   613,   614,   615,
     616,   617,   618,   619,   620,   624,   625,   629,   630,   634,
     635,   636,   637,   638,   639,   640,   644,   645,   646,   647,
     648,   649,   653,   654,   658,   659,   663,   664,   668,   669,
     673,   677,   678,   679,   680,   684,   685,   690,   691,   692
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
       2,     2,     2,     2,     2,     2,     2,     2,    46,    60,
      61,    62,    53,    51,    72,    52,    68,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    50,    67,
      47,    63,    48,     2,    69,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    64,    55,    65,    59,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    70,    45,    71,    66,     2,     2,     2,
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
    // Last valid token kind.
    const int code_max = 303;

    if (t <= 0)
      return symbol_kind::S_YYEOF;
    else if (t <= code_max)
      return static_cast <symbol_kind_type> (translate_table[t]);
    else
      return symbol_kind::S_YYUNDEF;
  }

#line 5 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"
} // Nelson
#line 3323 "modules/interpreter/src/grammar/NelSonParserCpp.cpp"

#line 694 "modules/interpreter/src/grammar/NelSonParserCpp.yxx"

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
