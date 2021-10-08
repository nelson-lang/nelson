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
#include <string>
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "NelsonConfiguration.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatShort(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatShort(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexShort(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexShort(single realPart, single imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatShortEng(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatShortEng(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexShortEng(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexShortEng(single realPart, single imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatLongEng(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatLongEng(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexLongEng(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexLongEng(single realPart, single imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatHex(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatHex(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexHex(double realPart, double imgPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexHex(single realPart, single imgPart, bool trim = false);
//=============================================================================

















NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatBank(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatBank(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexBank(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexBank(single realPart, single imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatRational(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatRational(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexRational(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexRational(single realPart, single imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatPlus(double number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatPlus(single number, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexPlus(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP
std::wstring
formatComplexPlus(single realPart, single imagPart, bool trim = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
