//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DataInterProcessToExchange.hpp"
//=============================================================================
void
dataInterProcessToExchange::clear()
{
    valueAnswer = false;
    pid = 0;
    serializedCompressedVariable.clear();
    content.clear();
    variableName.clear();
    scope.clear();
}
//=============================================================================
bool
dataInterProcessToExchange::isFullySerialized()
{
    switch (commandType) {
    case OPEN_FILES: {
        return true;
    } break;
    case LOAD_FILES: {
        return true;
    } break;
    case RUN_FILES: {
        return true;
    } break;
    case EVAL: {
        return true;
    } break;
    case GET: {
        return true;
    } break;
    case PUT:
    case GET_ANSWER:
    case IS_VAR_ANSWER: {
        return fullySerialized;
    } break;
    case IS_VAR: {
        return true;
    } break;
    case IS_MINIMIZED: {
        return true;
    } break;
    case IS_MINIMIZED_ANSWER: {
        return true;
    } break;
    case SET_MINIMIZE: {
        return true;
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
