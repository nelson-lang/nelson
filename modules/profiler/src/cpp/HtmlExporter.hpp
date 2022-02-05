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
#include <vector>
#include <string>
#include <tuple>
#include <set>
#include "nlsProfiler_exports.h"
#include "Types.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
copyHtmlDependencies(
    const std::wstring& moduleProfilerPath, const std::wstring& directoryDestination);
//=============================================================================
void
generateProfileFileHtml(const std::wstring& srcFilename, const stringVector& functionContent,
    const std::vector<std::tuple<int, std::string, int, double>>& fiveSlowerLines,
    const std::tuple<int, int, int, int, int, double>& coverage,
    const std::vector<std::tuple<int, double>>& lineInfo, int nbCalls, double totalTime,
    const std::wstring& htmlFilename);
//=============================================================================
void
generateProfileIndexHtml(const std::wstring& htmlFilename,
    const std::vector<std::tuple<std::wstring, std::wstring, int, double, double>>& indexData);
//=============================================================================
} // namespace Nelson
//=============================================================================
