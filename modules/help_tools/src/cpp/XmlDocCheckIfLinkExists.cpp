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
#include "XmlDocCheckIfLinkExists.hpp"
#include "IsFile.hpp"
#include "ModulesManager.hpp"
#include "XmlDocumentTags.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/container/vector.hpp>
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
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
        for (size_t k = 0; k < modules.size(); k++) {
            if (modules[k].modulename == modulename) {
                std::wstring name = linkname;
                boost::replace_all(name, std::wstring(L"${") + modulename + L"}", L"");
                filepath = modules[k].modulepath + L"/" + L"help" + L"/" + language + L"/" + L"xml"
                    + L"/" + name + utf8_to_wstring(XML_FILE_EXTENSION);
                if (IsFile(filepath)) {
                    return true;
                }
                if (language != L"en_US") {
                    filepath = modules[k].modulepath + L"/" + L"help" + L"/" + L"en_US" + L"/"
                        + L"xml" + L"/" + linkname + utf8_to_wstring(XML_FILE_EXTENSION);
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
