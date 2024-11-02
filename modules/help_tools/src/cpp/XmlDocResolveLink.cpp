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
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "XmlDocResolveLink.hpp"
#include "RelativePath.hpp"
#include "ModulesManager.hpp"
#include "XmlDocumentTags.hpp"
#include "XmlTarget.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
XmlDocResolveLink(const std::wstring& directorysource, const std::wstring& linkname,
    const std::wstring& currentModuleName, DOCUMENT_OUTPUT outputTarget,
    const std::wstring& destinationDir, const std::wstring& language, std::wstring& resolvedlink)
{
    bool bRes = false;
    if (StringHelpers::starts_with(linkname, L"http://")
        || StringHelpers::starts_with(linkname, L"https://")) {
        resolvedlink = linkname;
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
        bool bFound = false;
        for (auto& module : modules) {
            if (module.modulename == modulename) {
                std::wstring name = linkname;
                StringHelpers::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
                if (outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
                    resolvedlink = L"qthelp://org.nelson.modules." + modulename
                        + std::wstring(L".help/help/") + name + L".html";
                    bFound = true;
                    return bFound;
                }
                filepath = module.modulepath + L"/" + L"help" + L"/" + language + L"/" + L"xml"
                    + L"/" + name + utf8_to_wstring(XML_FILE_EXTENSION);
                if (FileSystemWrapper::Path::is_regular_file(filepath)) {
                    if (outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
                        if (currentModuleName != module.modulename) {
                            resolvedlink = L"../" + module.modulename + L"/" + name + L".md";
                        } else {
                            resolvedlink = name + L".md";
                        }
                    } else {
                        resolvedlink = name + L".html";
                    }
                    bFound = true;
                    return bFound;
                }
                if (language != NelsonConfiguration::getInstance()->getDefaultLocale()) {
                    filepath = module.modulepath + L"/" + L"help" + L"/"
                        + NelsonConfiguration::getInstance()->getDefaultLocale() + L"/" + L"xml"
                        + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
                    if (FileSystemWrapper::Path::is_regular_file(filepath)) {
                        if (outputTarget == DOCUMENT_OUTPUT::MARKDOWN) {
                            if (currentModuleName != module.modulename) {
                                resolvedlink = L"../" + module.modulename + L"/" + name + L".md";
                            } else {
                                resolvedlink = name + L".md";
                            }
                        } else {
                            resolvedlink = name + L".html";
                        }
                        bFound = true;
                        return bFound;
                    }
                }
            }
        }
        if (outputTarget != DOCUMENT_OUTPUT::QT_HELP) {
            std::wstring name = linkname;
            StringHelpers::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
            resolvedlink = name + L".html";
            return true;
        }
        return false;
    }
    filepath = directorysource + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
    if (FileSystemWrapper::Path::is_regular_file(filepath)) {
        bool _bRes = false;
        resolvedlink = RelativePath(directorysource, filepath, _bRes);
        return true;
    }
    if (FileSystemWrapper::Path::is_regular_file(linkname)) {
        bool _bRes = false;
        resolvedlink = RelativePath(directorysource, linkname, _bRes);
        return true;
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
