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
#pragma once
//=============================================================================
#include "i18n.hpp"
#include "nlsText_completion_exports.h"
#include <string>
//=============================================================================
#define POSTFIX_BUILTIN _W("Builtin")
#define POSTFIX_MACRO _W("Macro")
#define POSTFIX_VARIABLE _W("Variable")
#define POSTFIX_FILES _W("File or directory")
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
completerLine(std::wstring currentLine, std::wstring stringToAdd, std::wstring filePattern,
    std::wstring defaultPattern, bool stringToAddIsPath);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
getPartialLineAsPath(std::wstring line);
//=============================================================================
NLSTEXT_COMPLETION_IMPEXP std::wstring
getPartialLine(std::wstring line);
//=============================================================================
} // namespace Nelson
//=============================================================================
