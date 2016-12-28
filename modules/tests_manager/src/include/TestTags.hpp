//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <string>
#include "nlsTests_manager_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
#define NOT_FIXED_TAG        "<--NOT FIXED-->"
#define INTERACTIVE_TEST_TAG "<--INTERACTIVE TEST-->"
#define CLI_MODE_TAG 	     "<--CLI MODE-->"
#define ADV_CLI_MODE_TAG 	 "<--ADV-CLI MODE-->"
#define GUI_MODE_TAG 	     "<--GUI MODE-->"
#define CHECK_REF_TAG 	     "<--CHECK REF-->"
#define ENGLISH_IMPOSED_TAG  "<--ENGLISH IMPOSED-->"
#define WINDOWS_ONLY_TAG 	 "<--WINDOWS ONLY-->"
#define MACOS_ONLY_TAG 	     "<--MACOS ONLY-->"
#define UNIX_ONLY_TAG 	     "<--UNIX ONLY-->"
#define WITH_DISPLAY_TAG 	 "<--WITH DISPLAY-->"

    class NLSTESTS_MANAGER_IMPEXP TestTags {
    private:
        bool _notFixed;
        bool _interactiveTest;
        bool _cliMode;
        bool _guiMode;
        bool _advCliMode;
        bool _checkRef;
        bool _englishImposed;
        bool _windowsOnly;
        bool _macOnly;
        bool _unixOnly;
        bool _withDisplay;
    public:
        TestTags() {
            _notFixed = false;
            _interactiveTest = false;
            _cliMode = true;
            _guiMode = false;
            _advCliMode = false;
            _checkRef = false;
            _englishImposed = false;
            _windowsOnly = false;
            _macOnly = false;
            _unixOnly = false;
            _withDisplay = false;
        }

        bool isWithDisplay() {
            return _withDisplay;
        }

        bool isNotFixed() {
            return _notFixed;
        }
        bool isInteractiveTest() {
            return _interactiveTest;
        }
        bool isCliMode() {
            return _cliMode;
        }
        bool isGuiMode() {
            return _guiMode;
        }
        bool isAdvCliMode() {
            return _advCliMode;
        }
        bool isCheckRef() {
            return _checkRef;
        }
        bool isEnglishImposed() {
            return _englishImposed;
        }
        bool isWindowsOnly() {
            return _windowsOnly;
        }
        bool isMacOnly() {
            return _macOnly;
        }
        bool isUnixOnly() {
            return _unixOnly;
        }

        void setWithDisplay(bool val) {
            _withDisplay = val;
        }

        void setNotFixed(bool val) {
            _notFixed = val;
        }
        void setInteractiveTest(bool val) {
            _interactiveTest = val;
        }
        void setCliMode(bool val) {
            _cliMode = val;
        }
        void setGuiMode(bool val) {
            _guiMode = val;
        }
        void setAdvCliMode(bool val) {
            _advCliMode = val;
        }
        void setCheckRef(bool val) {
            _checkRef = val;
        }
        void setEnglishImposed(bool val) {
            _englishImposed = val;
        }
        void setWindowsOnly(bool val) {
            _windowsOnly = val;
        }
        void setMacOnly(bool val) {
            _macOnly = val;
        }
        void setUnixOnly(bool val) {
            _unixOnly = val;
        }
    };
}
//=============================================================================
