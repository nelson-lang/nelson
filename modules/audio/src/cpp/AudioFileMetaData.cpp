//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#if WITH_TAGLIB
#include <fileref.h>
#include <tag.h>
#include <tpropertymap.h>
#endif
#include "StringHelpers.hpp"
#include "AudioFileMetaData.hpp"
#include "MakeValidFieldname.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
revertFieldname(const std::wstring& fieldname, const std::wstring& defaultPrefix = L"x")
{
    std::wstring modifiedFieldname = fieldname;
    if (StringHelpers::starts_with(fieldname, defaultPrefix)) {
        StringHelpers::replace_first(modifiedFieldname, defaultPrefix, L"");
    }
    if (StringHelpers::starts_with(modifiedFieldname, L"_")) {
        modifiedFieldname = modifiedFieldname.substr(1);
    }
    StringHelpers::replace_all(modifiedFieldname, L"_", L" ");
    return modifiedFieldname;
}
//=============================================================================
bool
AudioFileMetaData(const std::wstring& filename, wstringVector& fieldnames,
    wstringVector& fiedvalues, std::wstring& errorMessage)
{
    errorMessage.clear();
#if WITH_TAGLIB
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
            for (const auto& j : i->second) {
                if (content.empty()) {
                    content = j.toWString();
                } else {
                    content = content + L"\n" + j.toWString();
                }
            }
            map[name] = content;
        }
        for (auto& it : map) {
            fieldnames.push_back(it.first);
            fiedvalues.push_back(it.second);
        }
        return true;
    }
    if (f.isNull()) {
        errorMessage = _W("Invalid filename.");
        return false;
    }
    if (!f.tag()) {
        errorMessage = _W("No tags available.");
        return false;
    }
#else
    errorMessage = _W("Taglib not available.");
#endif
    return false;
}
//=============================================================================
bool
setAudioFileMetaData(const std::wstring& filename, wstringVector fieldnames,
    wstringVector fieldvalues, std::wstring& errorMessage)
{
    errorMessage.clear();
#if WITH_TAGLIB
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
    }
    if (f.isNull()) {
        errorMessage = _W("Invalid filename.");
        return false;
    }
    if (!f.tag()) {
        errorMessage = _W("No tags available.");
        return false;
    }
#else
    errorMessage = _W("Taglib not available.");
#endif
    return false;
}
//=============================================================================
bool
deleteAudioFileMetaData(
    const std::wstring& filename, const std::wstring& fieldname, std::wstring& errorMessage)
{
    errorMessage.clear();
#if WITH_TAGLIB
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
#else
    errorMessage = _W("Taglib not available.");
#endif
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
