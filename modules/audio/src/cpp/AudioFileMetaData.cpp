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
#include "AudioFileMetaData.hpp"
#include "MakeValidFieldname.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <fileref.h>
#include <tag.h>
#include <tpropertymap.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
revertFieldname(std::wstring fieldname, std::wstring defaultPrefix = L"x")
{
    std::wstring modifiedFieldname = fieldname;
    if (boost::algorithm::starts_with(fieldname, defaultPrefix)) {
        boost::replace_first(modifiedFieldname, defaultPrefix, L"");
    }
    if (boost::algorithm::starts_with(modifiedFieldname, L"_")) {
        modifiedFieldname = modifiedFieldname.substr(1);
    }
    boost::replace_all(modifiedFieldname, L"_", L" ");
    return modifiedFieldname;
}
//=============================================================================
bool
AudioFileMetaData(std::wstring filename, wstringVector& fieldnames, wstringVector& fiedvalues,
    std::wstring& errorMessage)
{
    errorMessage = L"";
#ifdef _MSC_VER
    TagLib::FileRef f(filename.c_str());
#else
    TagLib::FileRef f(wstring_to_utf8(filename).c_str());
#endif
    if (!f.isNull() && f.tag()) {
        TagLib::PropertyMap tags = f.file()->properties();
        // map used to remove duplicated fields
        std::map<std::wstring, std::wstring> map;
        for (TagLib::PropertyMap::ConstIterator i = tags.begin(); i != tags.end(); ++i) {
            std::wstring name = MakeValidFieldname(i->first.toWString());
            std::wstring content;
            for (TagLib::StringList::ConstIterator j = i->second.begin(); j != i->second.end();
                 ++j) {
                if (content.empty()) {
                    content = j->toWString();
                } else {
                    content = content + L"\n" + j->toWString();
                }
            }
            map[name] = content;
        }
        for (std::map<std::wstring, std::wstring>::iterator it = map.begin(); it != map.end();
             ++it) {
            fieldnames.push_back(it->first);
            fiedvalues.push_back(it->second);
        }
        return true;
    } else {
        if (f.isNull()) {
            errorMessage = _W("Invalid filename.");
            return false;
        }
        if (!f.tag()) {
            errorMessage = _W("No tags available.");
            return false;
        }
    }
    return false;
}
//=============================================================================
bool
setAudioFileMetaData(std::wstring filename, wstringVector fieldnames, wstringVector fieldvalues,
    std::wstring& errorMessage)
{
    errorMessage = L"";
#ifdef _MSC_VER
    TagLib::FileRef f(filename.c_str());
#else
    TagLib::FileRef f(wstring_to_utf8(filename).c_str());
#endif
    if (!f.isNull() && f.tag()) {
        TagLib::PropertyMap tags = f.file()->properties();
        for (size_t k = 0; k < fieldnames.size(); k++) {
            TagLib::PropertyMap::ConstIterator found1 = tags.find(fieldnames[k]);
            TagLib::PropertyMap::ConstIterator found2 = tags.find(revertFieldname(fieldnames[k]));
            if (found1 != tags.end() || found2 != tags.end()) {
                if (found1 != tags.end()) {
                    tags.replace(fieldnames[k], TagLib::String(fieldvalues[k]));
                } else {
                    tags.replace(revertFieldname(fieldnames[k]), TagLib::String(fieldvalues[k]));
                }
            } else {
                tags.insert(fieldnames[k], TagLib::String(fieldvalues[k]));
            }
        }
        f.file()->setProperties(tags);
        if (f.save()) {
            return true;
        }
        errorMessage = _W("Cannot save file.");
        return false;
    } else {
        if (f.isNull()) {
            errorMessage = _W("Invalid filename.");
            return false;
        }
        if (!f.tag()) {
            errorMessage = _W("No tags available.");
            return false;
        }
    }
    return false;
}
//=============================================================================
bool
deleteAudioFileMetaData(std::wstring filename, std::wstring fieldname, std::wstring& errorMessage)
{
    errorMessage = L"";
#ifdef _MSC_VER
    TagLib::FileRef f(filename.c_str());
#else
    TagLib::FileRef f(wstring_to_utf8(filename).c_str());
#endif
    if (!f.isNull() && f.tag()) {
        TagLib::PropertyMap tags = f.file()->properties();
        TagLib::PropertyMap::ConstIterator found1 = tags.find(fieldname);
        TagLib::PropertyMap::ConstIterator found2 = tags.find(revertFieldname(fieldname));
        if (found1 != tags.end() || found2 != tags.end()) {
            if (found1 != tags.end()) {
                tags.erase(fieldname);
            } else {
                tags.erase(revertFieldname(fieldname));
            }
            f.file()->setProperties(tags);
            if (f.save()) {
                return true;
            }
            errorMessage = _W("Cannot save file.");
            return false;
        }
    } else {
        if (f.isNull()) {
            errorMessage = _W("Invalid filename.");
            return false;
        }
        if (!f.tag()) {
            errorMessage = _W("No tags available.");
            return false;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
