//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "dynamic_library.hpp"
#include "nlsLocalization_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
class NLSLOCALIZATION_IMPEXP Localization
{
public:
    static Localization*
    Instance();
    std::wstring
    getCurrentLanguage();
    std::wstring
    getDefaultLanguage();
    bool
    setLanguage(const std::wstring &lang, bool save = true);
    bool
    getManagedLanguages(wstringVector& langs);
    std::wstring
    initializeLocalization(const std::wstring &lang);
    bool
    isSupportedLanguage(const std::wstring &lang);
    void
    destroy();

private:
    Localization();
    Localization(Localization const& /*unused*/){};
    static Localization* m_pInstance;

    std::wstring currentLanguage;
    wstringVector LanguageSupported;
    library_handle nlsCoreDynamicLibrary;
    bool bFirstDynamicLibraryCall;

    void
    initCoreDynamicLibrary();
    std::wstring
    getPreferencesPathDynamic();
    std::wstring
    getNelsonPathDynamic();
    void
    setLanguageEnvironment(const std::wstring &lang);
    void
    initLanguageSupported();
};
} // namespace Nelson
//=============================================================================
