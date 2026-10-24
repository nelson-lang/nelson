//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <nlohmann/json.hpp>
#include <QtCore/QSettings>
#include "FileSystemWrapper.hpp"
#include "TextEditorPreferences.hpp"
#include "TextEditorIndentPreferences.h"
#include "GetVariableEnvironment.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "DefaultFont.hpp"
#include "NelsonConfiguration.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "NelsonEditorSettings.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// cached indentation width (-1: not yet read from the settings)
static int textEditorIndentSize = -1;
//=============================================================================
// The editor user preferences (indent, code analyzer, font) live in QSettings,
// shared with the preferences dialog. The one-time migration below imports a
// value still found only in the legacy JSON file.
static int
readLegacyJsonInt(const char* key, int defaultValue)
{
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring editorConfFile
        = prefDir + L"/" + utf8_to_wstring(TEXT_EDITOR_PREFERENCES_FILENAME);
    if (!FileSystemWrapper::Path::is_regular_file(editorConfFile)) {
        return defaultValue;
    }
#ifdef _MSC_VER
    std::ifstream jsonFile(editorConfFile);
#else
    std::ifstream jsonFile(wstring_to_utf8(editorConfFile));
#endif
    if (!jsonFile.is_open()) {
        return defaultValue;
    }
    int result = defaultValue;
    try {
        nlohmann::json data = nlohmann::json::parse(jsonFile);
        if (data.contains(key)) {
            result = data[key];
        }
    } catch (const nlohmann::json::exception&) {
    }
    jsonFile.close();
    return result;
}
//=============================================================================
int
TextEditorGetIndentSize()
{
    if (textEditorIndentSize <= 0) {
        QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
        int value = TEXT_EDITOR_DEFAULT_INDENT_SIZE;
        if (settings.contains(NELSON_EDITOR_SETTING_INDENT_SIZE)) {
            value = settings.value(NELSON_EDITOR_SETTING_INDENT_SIZE).toInt();
        } else {
            // migrate a value that only exists in the legacy JSON file
            value = readLegacyJsonInt("INDENT_SIZE", TEXT_EDITOR_DEFAULT_INDENT_SIZE);
        }
        if ((value <= 0) || (value > TEXT_EDITOR_MAX_INDENT_SIZE)) {
            value = TEXT_EDITOR_DEFAULT_INDENT_SIZE;
        }
        textEditorIndentSize = value;
    }
    return textEditorIndentSize;
}
//=============================================================================
void
TextEditorSetIndentSize(int indentSize)
{
    if ((indentSize > 0) && (indentSize <= TEXT_EDITOR_MAX_INDENT_SIZE)) {
        textEditorIndentSize = indentSize;
        QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
        settings.setValue(NELSON_EDITOR_SETTING_INDENT_SIZE, indentSize);
    }
}
//=============================================================================
bool
TextEditorSavePreferences(QFont currentFont, QPoint pos, QSize sz,
    Nelson::wstringVector recentFiles, QColor debugLineColor, bool codeAnalyzerEnabled)
{
    std::wstring prefDir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring editorConfFile
        = prefDir + L"/" + utf8_to_wstring(TEXT_EDITOR_PREFERENCES_FILENAME);
    // User preferences (font, code analyzer, indent) go to QSettings, shared
    // with the preferences dialog; window state stays in the JSON file below.
    {
        QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
        settings.setValue(NELSON_EDITOR_SETTING_FONT_NAME, currentFont.family());
        settings.setValue(NELSON_EDITOR_SETTING_FONT_SIZE, currentFont.pointSize());
        settings.setValue(NELSON_EDITOR_SETTING_FONT_FIXED_PITCH, currentFont.fixedPitch());
        settings.setValue(NELSON_EDITOR_SETTING_CODE_ANALYZER_ENABLED, codeAnalyzerEnabled);
        settings.setValue(NELSON_EDITOR_SETTING_INDENT_SIZE, TextEditorGetIndentSize());
    }

    nlohmann::json data;

    data["POSITION_X"] = pos.x();
    data["POSITION_Y"] = pos.y();
    data["SIZE_X"] = sz.width();
    data["SIZE_Y"] = sz.height();
    data["DEBUG_LINE_COLOR_R"] = debugLineColor.red();
    data["DEBUG_LINE_COLOR_G"] = debugLineColor.green();
    data["DEBUG_LINE_COLOR_B"] = debugLineColor.blue();
    data["DEBUG_LINE_COLOR_A"] = debugLineColor.alpha();

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
TextEditorLoadPreferences(QFont& currentFont, QPoint& pos, QSize& sz,
    Nelson::wstringVector& recentFiles, QColor& debugLineColor, bool& codeAnalyzerEnabled)
{
    int pref_pos_x = TEXT_EDITOR_DEFAULT_POS_X;
    int pref_pos_y = TEXT_EDITOR_DEFAULT_POS_Y;
    int pref_sz_x = TEXT_EDITOR_DEFAULT_SIZE_X;
    int pref_sz_y = TEXT_EDITOR_DEFAULT_SIZE_Y;
    int debug_r = 255, debug_g = 255, debug_b = 0, debug_a = 128;
    // legacy JSON font, used only to migrate an old preferences file
    int legacy_font_size = -1;
    bool legacy_font_fixed_pitch = true;
    std::string legacy_font_name = wstring_to_utf8(getDefaultFontName());
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
                if (data.contains("POSITION_X")) {
                    pref_pos_x = data["POSITION_X"];
                    pref_pos_y = data["POSITION_Y"];
                    pref_sz_x = data["SIZE_X"];
                    pref_sz_y = data["SIZE_Y"];
                }
                if (data.contains("DEBUG_LINE_COLOR_R")) {
                    debug_r = data["DEBUG_LINE_COLOR_R"];
                    debug_g = data["DEBUG_LINE_COLOR_G"];
                    debug_b = data["DEBUG_LINE_COLOR_B"];
                    debug_a = data["DEBUG_LINE_COLOR_A"];
                }
                if (data.contains("FONT_SIZE")) {
                    legacy_font_size = data["FONT_SIZE"];
                }
                if (data.contains("FONT_FIXED_PITCH")) {
                    legacy_font_fixed_pitch = data["FONT_FIXED_PITCH"];
                }
                if (data.contains("FONT_NAME")) {
                    legacy_font_name = data["FONT_NAME"];
                }
                if (data.contains("RECENT_FILES")) {
                    stringVector _recentFiles = data["RECENT_FILES"];
                    for (auto name : _recentFiles) {
                        recentFiles.push_back(utf8_to_wstring(name));
                    }
                }
            } catch (const nlohmann::json::exception&) {
            }
            jsonFile.close();
        }
    }
    // User preferences: QSettings is the source of truth; fall back on the
    // legacy JSON values (migration) when a key is not set yet.
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    codeAnalyzerEnabled
        = settings.value(NELSON_EDITOR_SETTING_CODE_ANALYZER_ENABLED, true).toBool();
    if (settings.contains(NELSON_EDITOR_SETTING_FONT_NAME)) {
        QFont new_font(settings.value(NELSON_EDITOR_SETTING_FONT_NAME).toString());
        new_font.setPointSize(
            settings.value(NELSON_EDITOR_SETTING_FONT_SIZE, getDefaultFontSize()).toInt());
        new_font.setFixedPitch(
            settings.value(NELSON_EDITOR_SETTING_FONT_FIXED_PITCH, true).toBool());
        currentFont = new_font;
    } else {
        // migrate the font stored (as a full descriptor) in the legacy JSON file
        QString font = QString::fromUtf8(legacy_font_name.c_str());
        if (!font.isNull()) {
            QFont new_font;
            if (new_font.fromString(font)) {
                new_font.setFixedPitch(legacy_font_fixed_pitch);
                if (legacy_font_size != -1) {
                    new_font.setPointSize(legacy_font_size);
                }
                currentFont = new_font;
            }
        }
    }
    pos.setX(pref_pos_x);
    pos.setY(pref_pos_y);
    sz.setWidth(pref_sz_x);
    sz.setHeight(pref_sz_y);
    debugLineColor = QColor(debug_r, debug_g, debug_b, debug_a);
    return true;
}
//=============================================================================
