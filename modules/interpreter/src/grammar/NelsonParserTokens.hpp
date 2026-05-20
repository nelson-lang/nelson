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
#include "NelSonParserCpp.hpp"
//=============================================================================
extern thread_local Nelson::ParseRHS yylval;
//=============================================================================
namespace Nelson {
//=============================================================================
using ParserToken = NelSonParser::token;

static constexpr int IDENT = ParserToken::IDENT;
static constexpr int NUMERIC = ParserToken::NUMERIC;
static constexpr int ENDQSTMNT = ParserToken::ENDQSTMNT;
static constexpr int ENDSTMNT = ParserToken::ENDSTMNT;
static constexpr int LE = ParserToken::LE;
static constexpr int GE = ParserToken::GE;
static constexpr int EQ = ParserToken::EQ;
static constexpr int DOTTIMES = ParserToken::DOTTIMES;
static constexpr int DOTRDIV = ParserToken::DOTRDIV;
static constexpr int DOTLDIV = ParserToken::DOTLDIV;
static constexpr int DOTPOWER = ParserToken::DOTPOWER;
static constexpr int DOTTRANSPOSE = ParserToken::DOTTRANSPOSE;
static constexpr int CHARACTER = ParserToken::CHARACTER;
static constexpr int STRING = ParserToken::STRING;
static constexpr int SPECIALCALL = ParserToken::SPECIALCALL;
static constexpr int END = ParserToken::END;
static constexpr int IF = ParserToken::IF;
static constexpr int FUNCTION = ParserToken::FUNCTION;
static constexpr int FOR = ParserToken::FOR;
static constexpr int BREAK = ParserToken::BREAK;
static constexpr int MAGICEND = ParserToken::MAGICEND;
static constexpr int WHILE = ParserToken::WHILE;
static constexpr int ELSE = ParserToken::ELSE;
static constexpr int ELSEIF = ParserToken::ELSEIF;
static constexpr int SWITCH = ParserToken::SWITCH;
static constexpr int CASE = ParserToken::CASE;
static constexpr int OTHERWISE = ParserToken::OTHERWISE;
static constexpr int CONTINUE = ParserToken::CONTINUE;
static constexpr int TRY = ParserToken::TRY;
static constexpr int CATCH = ParserToken::CATCH;
static constexpr int FIELD = ParserToken::FIELD;
static constexpr int REFLPAREN = ParserToken::REFLPAREN;
static constexpr int REFRPAREN = ParserToken::REFRPAREN;
static constexpr int KEYBOARD = ParserToken::KEYBOARD;
static constexpr int RETURN = ParserToken::RETURN;
static constexpr int VARARGIN = ParserToken::VARARGIN;
static constexpr int VARARGOUT = ParserToken::VARARGOUT;
static constexpr int ABORT = ParserToken::ABORT;
static constexpr int ENDFUNCTION = ParserToken::ENDFUNCTION;
static constexpr int SOR = ParserToken::SOR;
static constexpr int SAND = ParserToken::SAND;
static constexpr int NE = ParserToken::NE;
static constexpr int POS = ParserToken::POS;
static constexpr int NEG = ParserToken::NEG;
static constexpr int NOT = ParserToken::NOT;
//=============================================================================
} // namespace Nelson
//=============================================================================
