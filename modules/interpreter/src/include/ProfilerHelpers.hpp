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
#pragma once
//=============================================================================
#include <tuple>
#include <vector>
#include <string>
#include <Types.hpp>
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using profileParent = std::tuple<std::string, uint64>;
//=============================================================================
using profileParentStack = std::vector<profileParent>;
//=============================================================================
using internalProfileFunction = std::tuple<profileParentStack, std::string, std::wstring, bool>;
//=============================================================================
using profileFunction = std::tuple<internalProfileFunction, uint64, uint64, uint64>;
//=============================================================================
internalProfileFunction
computeProfileStack(Evaluator* eval, const std::string& currentFunctionName,
    const std::wstring& currentFilename, bool isBuiltin = true);
//=============================================================================
} // namespace Nelson
//=============================================================================
