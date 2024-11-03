//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include <regex>
#include "ModulesManager.hpp"
#include "XmlDocCheckIfLinkExists.hpp"
#include "FileSystemWrapper.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
XmlDocCheckIfLinkExists(
    const std::wstring& directorysource, const std::wstring& linkname, const std::wstring& language)
{
    if (StringHelpers::starts_with(linkname, L"http://")
        || StringHelpers::starts_with(linkname, L"https://")) {
        return true;
    }
    std::wstring filepath;
    std::wregex exp(L"\\$\\{\\w+\\}");
    std::wsregex_iterator it(linkname.begin(), linkname.end(), exp);
    std::wsregex_iterator end;
    if (it != end) {
        std::wstring modulename = it->str();
        StringHelpers::replace_all(modulename, L"${", L"");
        StringHelpers::replace_all(modulename, L"}", L"");
        std::vector<module> modules = GetModules(true);
        for (auto& module : modules) {
            if (module.modulename == modulename) {
                std::wstring name = linkname;
                StringHelpers::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
                filepath = module.modulepath + L"/" + L"help" + L"/" + language + L"/" + L"xml"
                    + L"/" + name + utf8_to_wstring(XML_FILE_EXTENSION);
                if (FileSystemWrapper::Path::is_regular_file(filepath)) {
                    return true;
                }
                if (language != NelsonConfiguration::getInstance()->getDefaultLocale()) {
                    filepath = module.modulepath + L"/" + L"help" + L"/"
                        + NelsonConfiguration::getInstance()->getDefaultLocale() + L"/" + L"xml"
                        + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
                    if (FileSystemWrapper::Path::is_regular_file(filepath)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    filepath = directorysource + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
    if (FileSystemWrapper::Path::is_regular_file(filepath)) {
        return true;
    }
    if (FileSystemWrapper::Path::is_regular_file(linkname)) {
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
