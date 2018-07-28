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
#include "Types.hpp"
#include "nlsTests_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
#define PREFIX_TAG "<--"
#define POSTFIX_TAG "-->"
#define NOT_FIXED_TAG "<--NOT FIXED-->"
#define INTERACTIVE_TEST_TAG "<--INTERACTIVE TEST-->"
#define CLI_MODE_TAG "<--CLI MODE-->"
#define ADV_CLI_MODE_TAG "<--ADV-CLI MODE-->"
#define GUI_MODE_TAG "<--GUI MODE-->"
#define CHECK_REF_TAG "<--CHECK REF-->"
#define ENGLISH_IMPOSED_TAG "<--ENGLISH IMPOSED-->"
#define WINDOWS_ONLY_TAG "<--WINDOWS ONLY-->"
#define MACOS_ONLY_TAG "<--MACOS ONLY-->"
#define UNIX_ONLY_TAG "<--UNIX ONLY-->"
#define WITH_DISPLAY_TAG "<--WITH DISPLAY-->"
#define RELEASE_ONLY_TAG "<--RELEASE ONLY-->"
#define EXCEL_REQUIRED_TAG "<--EXCEL REQUIRED-->"
#define MPI_MODE_TAG "<--MPI MODE-->"
#define AUDIO_INPUT_REQUIRED_TAG "<--AUDIO INPUT REQUIRED-->"
#define AUDIO_OUTPUT_REQUIRED_TAG "<--AUDIO OUTPUT REQUIRED-->"
#define C_COMPILER_REQUIRED_TAG "<--C/C++ COMPILER REQUIRED-->"
#define INDEX_64_BIT_REQUIRED_TAG "<--INDEX 64 BIT REQUIRED-->"

class NLSTESTS_MANAGER_IMPEXP TestTags
{
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
    bool _releaseOnly;
    bool _excelRequired;
    bool _mpiMode;
    bool _audioInputRequired;
    bool _audioOutputRequired;
    bool _cCompilerRequired;
    bool _index64BitRequired;

public:
    TestTags()
    {
        _notFixed = false;
        _interactiveTest = false;
        _cliMode = false;
        _guiMode = false;
        _advCliMode = false;
        _checkRef = false;
        _englishImposed = false;
        _windowsOnly = false;
        _macOnly = false;
        _unixOnly = false;
        _withDisplay = false;
        _releaseOnly = false;
        _excelRequired = false;
        _mpiMode = false;
        _audioInputRequired = false;
        _audioOutputRequired = false;
        _cCompilerRequired = false;
        _index64BitRequired = false;
    }

    bool
    isWithDisplay()
    {
        return _withDisplay;
    }
    bool
    isNotFixed()
    {
        return _notFixed;
    }
    bool
    isInteractiveTest()
    {
        return _interactiveTest;
    }
    bool
    isCliMode()
    {
        return _cliMode;
    }
    bool
    isGuiMode()
    {
        return _guiMode;
    }
    bool
    isAdvCliMode()
    {
        return _advCliMode;
    }
    bool
    isCheckRef()
    {
        return _checkRef;
    }
    bool
    isEnglishImposed()
    {
        return _englishImposed;
    }
    bool
    isWindowsOnly()
    {
        return _windowsOnly;
    }
    bool
    isMacOnly()
    {
        return _macOnly;
    }
    bool
    isUnixOnly()
    {
        return _unixOnly;
    }
    bool
    isReleaseOnly()
    {
        return _releaseOnly;
    }
    bool
    isExcelRequired()
    {
        return _excelRequired;
    }
    bool
    isMpiMode()
    {
        return _mpiMode;
    }
    bool
    isAudioInputRequired()
    {
        return _audioInputRequired;
    }
    bool
    isAudioOutputRequired()
    {
        return _audioOutputRequired;
    }
    bool
    isCCompilerRequired()
    {
        return _cCompilerRequired;
    }
    bool
    isIndex64BitRequired()
    {
        return _index64BitRequired;
    }

    void
    setWithDisplay(bool val)
    {
        _withDisplay = val;
    }

    void
    setNotFixed(bool val)
    {
        _notFixed = val;
    }
    void
    setInteractiveTest(bool val)
    {
        _interactiveTest = val;
    }
    void
    setCliMode(bool val)
    {
        _cliMode = val;
    }
    void
    setGuiMode(bool val)
    {
        _guiMode = val;
    }
    void
    setAdvCliMode(bool val)
    {
        _advCliMode = val;
    }
    void
    setCheckRef(bool val)
    {
        _checkRef = val;
    }
    void
    setEnglishImposed(bool val)
    {
        _englishImposed = val;
    }
    void
    setWindowsOnly(bool val)
    {
        _windowsOnly = val;
    }
    void
    setMacOnly(bool val)
    {
        _macOnly = val;
    }
    void
    setUnixOnly(bool val)
    {
        _unixOnly = val;
    }
    void
    setReleaseOnly(bool val)
    {
        _releaseOnly = val;
    }
    void
    setExcelRequired(bool val)
    {
        _excelRequired = val;
    }
    void
    setMpiMode(bool val)
    {
        _mpiMode = val;
    }
    void
    setAudioInputRequired(bool val)
    {
        _audioInputRequired = val;
    }
    void
    setAudioOutputRequired(bool val)
    {
        _audioOutputRequired = val;
    }
    void
    setCCompilerRequired(bool val)
    {
        _cCompilerRequired = val;
    }
    void
    setIndex64BitRequired(bool val)
    {
        _index64BitRequired = val;
    }
};
} // namespace Nelson
//=============================================================================
