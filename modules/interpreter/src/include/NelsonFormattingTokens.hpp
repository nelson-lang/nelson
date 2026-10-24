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
#include <cstddef>
#include <string>
#include <vector>
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct FormattingToken
{
    int type = 0;
    std::string text;
    std::size_t offset = 0;
    int line = 0;
    int column = 0;
    bool isComment = false;
    bool isBlockComment = false;
};
//=============================================================================
NLSINTERPRETER_IMPEXP std::vector<FormattingToken>
tokenizeForFormatting(const std::string& source);
//=============================================================================
NLSINTERPRETER_IMPEXP std::string
maskFormattingBlockComments(const std::string& source);
//=============================================================================
NLSINTERPRETER_IMPEXP void
setFormattingTokenSink(std::vector<FormattingToken>* sink);
//=============================================================================
NLSINTERPRETER_IMPEXP std::vector<FormattingToken>*
getFormattingTokenSink();
//=============================================================================
NLSINTERPRETER_IMPEXP const std::vector<const char*>&
getFormattingContextualBlockKeywords();
//=============================================================================
NLSINTERPRETER_IMPEXP const std::vector<const char*>&
getFormattingOperators();
//=============================================================================
NLSINTERPRETER_IMPEXP bool
isFormattingUnaryOperatorCandidate(const std::string& op);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
isFormattingPostfixOperator(const std::string& op);
//=============================================================================
} // namespace Nelson
//=============================================================================
