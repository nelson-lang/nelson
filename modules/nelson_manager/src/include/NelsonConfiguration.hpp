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
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    NLS_FORMAT_SHORT,
    NLS_FORMAT_LONG,
    NLS_FORMAT_SHORTE,
    NLS_FORMAT_LONGE,
    NLS_FORMAT_HEX
} OutputFormatDisplay;
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
    OutputFormatDisplay
    setOutputFormatDisplay(OutputFormatDisplay desiredOutputFormatDisplay);
    OutputFormatDisplay
    getOutputFormatDisplay();
    //=============================================================================
    void
    destroy();
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
     * Current output format
     */
    OutputFormatDisplay currentOutputFormatDisplay;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
