//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    lineToEvaluate.clear();
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
    case PUT: {
        return fullySerialized;
    } break;
    case GET: {
        return true;
    } break;
    case GET_ANSWER: {
        return fullySerialized;
    } break;
    case IS_VAR: {
        return true;
    } break;
    case IS_VAR_ANSWER: {
        return fullySerialized;
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
