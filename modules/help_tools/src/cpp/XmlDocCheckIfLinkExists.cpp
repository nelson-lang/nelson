//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/container/vector.hpp>
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include "ModulesManager.hpp"
#include "XmlDocCheckIfLinkExists.hpp"
#include "IsFile.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
XmlDocCheckIfLinkExists(
    const std::wstring& directorysource, const std::wstring& linkname, const std::wstring& language)
{
    if (boost::algorithm::starts_with(linkname, "http://")
        || boost::algorithm::starts_with(linkname, "https://")) {
        return true;
    }
    std::wstring filepath;
    boost::wregex exp(L"\\$\\{\\w+\\}");
    boost::wsregex_iterator it(linkname.begin(), linkname.end(), exp);
    boost::wsregex_iterator end;
    if (it != end) {
        std::wstring modulename = it->str();
        boost::replace_all(modulename, L"${", L"");
        boost::replace_all(modulename, L"}", L"");
        boost::container::vector<module> modules = GetModules(true);
        for (auto& module : modules) {
            if (module.modulename == modulename) {
                std::wstring name = linkname;
                boost::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
                filepath = module.modulepath + L"/" + L"help" + L"/" + language + L"/" + L"xml"
                    + L"/" + name + utf8_to_wstring(XML_FILE_EXTENSION);
                if (IsFile(filepath)) {
                    return true;
                }
                if (language != L"en_US") {
                    filepath = module.modulepath + L"/" + L"help" + L"/" + L"en_US" + L"/" + L"xml"
                        + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
                    if (IsFile(filepath)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
    filepath = directorysource + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
    if (IsFile(filepath)) {
        return true;
    }
    if (IsFile(linkname)) {
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
