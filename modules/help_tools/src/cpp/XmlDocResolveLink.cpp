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
#include <boost/regex.hpp>
#include "XmlDocResolveLink.hpp"
#include "RelativePath.hpp"
#include "IsFile.hpp"
#include "ModulesManager.hpp"
#include "XmlDocumentTags.hpp"
#include "XmlTarget.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
XmlDocResolveLink(const std::wstring& directorysource, const std::wstring& linkname,
    const std::wstring& currentModuleName, DOCUMENT_OUTPUT outputTarget,
    const std::wstring& destinationDir, const std::wstring& language, std::wstring& resolvedlink)
{
    bool bRes = false;
    if (boost::algorithm::starts_with(linkname, "http://")
        || boost::algorithm::starts_with(linkname, "https://")) {
        resolvedlink = linkname;
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
        bool bFound = false;
        for (auto& module : modules) {
            if (module.modulename == modulename) {
                std::wstring name = linkname;
                boost::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
                if (outputTarget == DOCUMENT_OUTPUT::QT_HELP) {
                    resolvedlink = L"qthelp://org.nelson.modules." + modulename
                        + std::wstring(L".help/help/") + name + L".html";
                    bFound = true;
                    return bFound;
                }
                filepath = module.modulepath + L"/" + L"help" + L"/" + language + L"/" + L"xml"
                    + L"/" + name + utf8_to_wstring(XML_FILE_EXTENSION);
                if (IsFile(filepath)) {
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
                if (language != L"en_US") {
                    filepath = module.modulepath + L"/" + L"help" + L"/" + L"en_US" + L"/" + L"xml"
                        + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
                    if (IsFile(filepath)) {
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
            boost::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
            resolvedlink = name + L".html";
            return true;
        }
        return false;
    }
    filepath = directorysource + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
    if (IsFile(filepath)) {
        bool bRes = false;
        resolvedlink = RelativePath(directorysource, filepath, bRes);
        return true;
    }
    if (IsFile(linkname)) {
        bool bRes = false;
        resolvedlink = RelativePath(directorysource, linkname, bRes);
        return true;
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
