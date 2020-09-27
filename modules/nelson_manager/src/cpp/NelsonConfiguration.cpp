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
#include <new>
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NelsonConfiguration* NelsonConfiguration::m_pInstance = nullptr;
//=============================================================================
NelsonConfiguration::NelsonConfiguration()
{
    InterruptPending = false;
    currentOutputFormatDisplay = NLS_FORMAT_SHORT;
    modulesProtected = false;
    nelsonRootPath = L"";
    ipcEnabled = false;
}
//=============================================================================
NelsonConfiguration*
NelsonConfiguration::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new NelsonConfiguration();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
NelsonConfiguration::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
NelsonConfiguration::getInterruptPending()
{
    return InterruptPending;
}
//=============================================================================
bool
NelsonConfiguration::setInterruptPending(bool bInterruptPending)
{
    bool bPrevious = InterruptPending;
    InterruptPending = bInterruptPending;
    return bPrevious;
}
//=============================================================================
OutputFormatDisplay
NelsonConfiguration::setOutputFormatDisplay(OutputFormatDisplay desiredOutputFormatDisplay)
{
    OutputFormatDisplay previousOutputFormatDisplay = currentOutputFormatDisplay;
    currentOutputFormatDisplay = desiredOutputFormatDisplay;
    return previousOutputFormatDisplay;
}
//=============================================================================
OutputFormatDisplay
NelsonConfiguration::getOutputFormatDisplay()
{
    return currentOutputFormatDisplay;
}
//=============================================================================
void
NelsonConfiguration::enableModulesProtection()
{
    modulesProtected = true;
}
//=============================================================================
void
NelsonConfiguration::disableModulesProtection()
{
    modulesProtected = false;
}
//=============================================================================
bool
NelsonConfiguration::isModulesProtected()
{
    return modulesProtected;
}
//=============================================================================
void
NelsonConfiguration::setNelsonRootDirectory(const std::wstring& nelsonroot)
{
    nelsonRootPath = nelsonroot;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonRootDirectory()
{
    return nelsonRootPath;
}
//=============================================================================
void
NelsonConfiguration::setIpcEnabled(bool value)
{
    ipcEnabled = value;
}
//=============================================================================
bool
NelsonConfiguration::isIpcEnabled()
{
    return ipcEnabled;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
