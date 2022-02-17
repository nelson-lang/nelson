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
#include "helpbrowserBuiltin.hpp"
#include "Error.hpp"
#include "HelpBrowser.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpBrowserGateway::helpbrowserBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() == 0) {
        nargoutcheck(nLhs, 0, 0);
        std::wstring msg;
        if (!HelpBrowser::getInstance()->startBrowser(msg)) {
            Error(msg);
        }
    } else if (argIn.size() == 1) {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"-cache" || param1 == L"-attributes") {
            nargoutcheck(nLhs, 0, 1);
            ArrayOfVector retval;
            if (param1 == L"-cache") {
                retval << ArrayOf::characterArrayConstructor(
                    HelpBrowser::getInstance()->getCachePath());
            } else {
                retval << ToCellStringAsColumn(HelpBrowser::getInstance()->getAttributes());
            }
            return retval;
        }
        nargoutcheck(nLhs, 0, 0);
        if (param1 == L"-close") {
            HelpBrowser::getInstance()->closeBrowser();
        } else if (param1 == L"-clearcache") {
            HelpBrowser::getInstance()->clearCache();
        } else if (param1 == L"-sync") {
            HelpBrowser::getInstance()->syncBrowser();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
        }

    } else if (argIn.size() == 2) {
        nargoutcheck(nLhs, 0, 0);
        std::wstring param1 = argIn[0].getContentAsWideString();
        std::wstring param2 = argIn[1].getContentAsWideString();
        if (param1 == L"-register") {
            HelpBrowser::getInstance()->registerHelpFile(param2);
        } else if (param1 == L"-unregister") {
            HelpBrowser::getInstance()->unregisterHelpFile(param2);
        } else if (param1 == L"-name") {
            HelpBrowser::getInstance()->showDocByName(param2);
        } else if (param1 == L"-identifier") {
            HelpBrowser::getInstance()->showDocByIdentifier(param2);
        } else if (param1 == L"-show") {
            if ((param2 == L"contents") || (param2 == L"index") || (param2 == L"bookmarks")
                || (param2 == L"search")) {
                std::wstring command = L"show " + param2;
                HelpBrowser::getInstance()->sendCommand(command);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE);
            }
        } else if (param1 == L"-hide") {
            if ((param2 == L"contents") || (param2 == L"index") || (param2 == L"bookmarks")
                || (param2 == L"search")) {
                std::wstring command = L"hide " + param2;
                HelpBrowser::getInstance()->sendCommand(command);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_VALUE);
            }
        } else if (param1 == L"-setsource") {
            std::wstring command = L"setSource " + param2;
            HelpBrowser::getInstance()->sendCommand(command);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
        }
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
