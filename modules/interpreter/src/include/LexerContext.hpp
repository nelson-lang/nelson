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
#include <cstdio>
#include <string>
#include "Keywords.hpp"
#include "AbstractSyntaxTree.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
union contextOrPointer
{
    int i;
    AbstractSyntaxTreePtr p;
};
//=============================================================================
struct ParseRHS
{
    bool isToken;
    contextOrPointer v;
};
//=============================================================================

enum LexingStates
{
    Initial,
    Scanning,
    SpecScan
};
//=============================================================================
#define DEFAULT_BUFFER_SIZE_LEXER 256
//=============================================================================
struct LexerContext
{
    char* textbuffer = nullptr;
    char* datap = nullptr;
    char* linestart = nullptr;
    int lineNumber = 0;
    int continuationCount = 0;
    int inBlock = 0;
    int inStatement = 0;
    bool inFunction = false;
    int countEndFunction = 0;
    LexingStates lexState;
    int bracketStack[DEFAULT_BUFFER_SIZE_LEXER];
    int bracketStackSize;
    int vcStack[DEFAULT_BUFFER_SIZE_LEXER];
    int vcStackSize;
    int vcFlag;
    //=============================================================================
    /*
     * These variables capture the token information
     */
    int tokenActive;
    int tokenType;
    ParseRHS tokenValue;
    int previousToken = 0;
    //=============================================================================
    keywordStruct tSearch, *pSearch;
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
