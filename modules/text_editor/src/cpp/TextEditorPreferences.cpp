//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <nlohmann/json.hpp>
#include "FileSystemWrapper.hpp"
#include "TextEditorPreferences.hpp"
#include "GetVariableEnvironment.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "DefaultFont.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
bool
TextEditorSavePreferences(
    QFont currentFont, QPoint pos, QSize sz, Nelson::wstringVector recentFiles)
{
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring editorConfFile
        = prefDir + L"/" + utf8_to_wstring(TEXT_EDITOR_PREFERENCES_FILENAME);
    QString fontQString = currentFont.toString();
    std::wstring fontName = QStringTowstring(fontQString);

    nlohmann::json data;

    data["FONT_SIZE"] = currentFont.pointSize();
    data["FONT_FIXED_PITCH"] = currentFont.fixedPitch();
    data["FONT_NAME"] = wstring_to_utf8(fontName);
    data["POSITION_X"] = pos.x();
    data["POSITION_Y"] = pos.y();
    data["SIZE_X"] = sz.width();
    data["SIZE_Y"] = sz.height();

    stringVector utfFilenames;
    for (auto& name : recentFiles) {
        utfFilenames.push_back(wstring_to_utf8(name));
    }
    nlohmann::json j_vector(utfFilenames);
    data["RECENT_FILES"] = j_vector;
#ifdef _MSC_VER
    std::ofstream out(editorConfFile);
#else
    std::ofstream out(wstring_to_utf8(editorConfFile));
#endif
    if (out.is_open()) {
        out << data.dump(4);
        out.close();
        return true;
    }
    return false;
}
//=============================================================================
bool
TextEditorLoadPreferences(
    QFont& currentFont, QPoint& pos, QSize& sz, Nelson::wstringVector& recentFiles)
{
    int pref_pos_x = TEXT_EDITOR_DEFAULT_POS_X;
    int pref_pos_y = TEXT_EDITOR_DEFAULT_POS_Y;
    int pref_sz_x = TEXT_EDITOR_DEFAULT_SIZE_X;
    int pref_sz_y = TEXT_EDITOR_DEFAULT_SIZE_Y;
    int pref_font_size = -1;
    bool pref_font_fixed_pitch = true;
    std::string pref_font_name = wstring_to_utf8(getDefaultFontName());
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring editorConfFile
        = prefDir + L"/" + utf8_to_wstring(TEXT_EDITOR_PREFERENCES_FILENAME);
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(editorConfFile);
    if (bIsFile) {
#ifdef _MSC_VER
        std::ifstream jsonFile(editorConfFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(editorConfFile));
#endif
        if (jsonFile.is_open()) {
            nlohmann::json data;
            try {
                data = nlohmann::json::parse(jsonFile);
                pref_font_size = data["FONT_SIZE"];
                pref_font_fixed_pitch = data["FONT_FIXED_PITCH"];
                pref_font_name = data["FONT_NAME"];
                pref_pos_x = data["POSITION_X"];
                pref_pos_y = data["POSITION_Y"];
                pref_sz_x = data["SIZE_X"];
                pref_sz_y = data["SIZE_Y"];
                stringVector _recentFiles = data["RECENT_FILES"];
                for (auto name : _recentFiles) {
                    recentFiles.push_back(utf8_to_wstring(name));
                }
            } catch (const nlohmann::json::exception&) {
            }
            jsonFile.close();
        }
    }
    QString font = QString::fromUtf8(pref_font_name.c_str());
    if (!font.isNull()) {
        QFont new_font;
        if (new_font.fromString(font)) {
            new_font.setFixedPitch(pref_font_fixed_pitch);
            if (pref_font_size != -1) {
                new_font.setPointSize(pref_font_size);
            }
            currentFont = new_font;
        }
    }
    pos.setX(pref_pos_x);
    pos.setY(pref_pos_y);
    sz.setWidth(pref_sz_x);
    sz.setHeight(pref_sz_y);
    return true;
}
//=============================================================================
