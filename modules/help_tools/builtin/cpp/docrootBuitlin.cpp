//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "docrootBuiltin.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
#include <nlohmann/json.hpp>
#include <fstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::docrootBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        std::wstring newUrl = argIn[0].getContentAsWideString();
        if (newUrl.empty()) {
            std::wstring prefdir
                = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
            std::wstring docroot = prefdir + L"/docroot.json";
            FileSystemWrapper::Path::remove(docroot);
            NelsonConfiguration::getInstance()->setDocBookUrl(newUrl);
        } else {
            nlohmann::json jsonObject;
            jsonObject["docbook_url"] = wstring_to_utf8(newUrl);
            std::wstring prefdir
                = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
            std::wstring docroot = prefdir + L"/docroot.json";
#ifdef _MSC_VER
            std::wofstream file(docroot);
#else
            std::ofstream file(wstring_to_utf8(docroot));
#endif
            // Serialize the JSON object to the file
            if (file.is_open()) {
#ifdef _MSC_VER
                file << utf8_to_wstring(jsonObject.dump(2));
                file << L"\n";
#else
                file << jsonObject.dump(2);
                file << "\n";
#endif
                file.close();
                NelsonConfiguration::getInstance()->setDocBookUrl(newUrl);
            }
        }
    }
    retval << ArrayOf::characterArrayConstructor(
        NelsonConfiguration::getInstance()->getDocBookUrl());
    return retval;
}
//=============================================================================
