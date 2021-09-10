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
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    NLS_NUMERIC_FORMAT_SHORT,
    NLS_NUMERIC_FORMAT_LONG,
    NLS_NUMERIC_FORMAT_SHORTE,
    NLS_NUMERIC_FORMAT_LONGE,
    NLS_NUMERIC_FORMAT_SHORTG,
    NLS_NUMERIC_FORMAT_LONGG,
    NLS_NUMERIC_FORMAT_SHORTENG,
    NLS_NUMERIC_FORMAT_LONGENG,
    NLS_NUMERIC_FORMAT_PLUS,
    NLS_NUMERIC_FORMAT_BANK,
    NLS_NUMERIC_FORMAT_HEX,
    NLS_NUMERIC_FORMAT_RATIONAL
} NumericFormatDisplay;
//=============================================================================
typedef enum
{
    NLS_LINE_SPACING_COMPACT,
    NLS_LINE_SPACING_LOOSE
} LineSpacingDisplay;
//=============================================================================
class NLSNELSON_MANAGER_IMPEXP NelsonConfiguration
{
    //=============================================================================
public:
    //=============================================================================
    static NelsonConfiguration*
    getInstance();
    //=============================================================================
    bool
    getInterruptPending();
    bool
    setInterruptPending(bool bInterruptPending);
    //=============================================================================
    NumericFormatDisplay
    setNumericFormatDisplay(NumericFormatDisplay desiredNumericFormatDisplay);
    NumericFormatDisplay
    getNumericFormatDisplay();
    //=============================================================================
    LineSpacingDisplay
    setLineSpacingDisplay(LineSpacingDisplay desiredLineSpacingDisplay);
    LineSpacingDisplay
    getLineSpacingDisplay();
    //=============================================================================
    void
    destroy();
    //=============================================================================
    void
    enableModulesProtection();
    //=============================================================================
    void
    disableModulesProtection();
    //=============================================================================
    bool
    isModulesProtected();
    //=============================================================================
    void
    setNelsonRootDirectory(const std::wstring& nelsonroot);
    //=============================================================================
    std::wstring
    getNelsonRootDirectory();
    //=============================================================================
private:
    NelsonConfiguration();
    //=============================================================================
    static NelsonConfiguration* m_pInstance;
    //=============================================================================
    /**
     * Pending control-C
     */
    bool InterruptPending;
    //=============================================================================
    /**
     * Current numeric format
     */
    NumericFormatDisplay currentNumericFormatDisplay;
    //=============================================================================
    /**
     * Current line spacing
     */
    LineSpacingDisplay currentLineSpacingDisplay;
    //=============================================================================
    bool modulesProtected;
    //=============================================================================
    std::wstring nelsonRootPath;
    //=============================================================================
    bool ipcEnabled;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
