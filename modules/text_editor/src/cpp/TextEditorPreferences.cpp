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
#include "TextEditorPreferences.hpp"
#include "GetVariableEnvironment.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
getPreferencesPath()
{
#define NELSON_PREFERENCES_PATH_ENV L"NELSON_PREFERENCES_PATH"
    std::wstring prefPath = GetVariableEnvironment(NELSON_PREFERENCES_PATH_ENV, L"");
    return prefPath;
}
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
bool
TextEditorSavePreferences(
    QFont currentFont, QPoint pos, QSize sz, Nelson::wstringVector recentFiles)
{
    bool bRes = false;
    std::wstring prefDir = getPreferencesPath();
    std::wstring editorConfFile
        = prefDir + L"/" + utf8_to_wstring(TEXT_EDITOR_PREFERENCES_FILENAME);
    boost::property_tree::ptree pt;
    QString fontQString = currentFont.toString();
    ;
    std::wstring fontName = QStringTowstring(fontQString);
    pt.put("FONT_SIZE", currentFont.pointSize());
    pt.put("FONT_FIXED_PITCH", currentFont.fixedPitch());
    pt.put("FONT_NAME", wstring_to_utf8(fontName));
    pt.put("POSITION_X", pos.x());
    pt.put("POSITION_Y", pos.y());
    pt.put("SIZE_X", sz.width());
    pt.put("SIZE_Y", sz.height());
    boost::property_tree::ptree recent_files_node;
    for (auto& name : recentFiles) {
        boost::property_tree::ptree name_node;
        name_node.put("", wstring_to_utf8(name));
        recent_files_node.push_back(std::make_pair("", name_node));
    }
    pt.add_child("RECENT_FILES", recent_files_node);
    std::ostringstream buf;
    boost::property_tree::write_json(buf, pt, false);
    std::string json = buf.str();
#ifdef _MSC_VER
    std::ofstream out(editorConfFile);
#else
    std::ofstream out(wstring_to_utf8(editorConfFile));
#endif
    out << json;
    out.close();
    return true;
}
//=============================================================================
bool
TextEditorLoadPreferences(
    QFont& currentFont, QPoint& pos, QSize& sz, Nelson::wstringVector& recentFiles)
{
    bool bRes = false;
    int pref_pos_x = TEXT_EDITOR_DEFAULT_POS_X;
    int pref_pos_y = TEXT_EDITOR_DEFAULT_POS_Y;
    int pref_sz_x = TEXT_EDITOR_DEFAULT_SIZE_X;
    int pref_sz_y = TEXT_EDITOR_DEFAULT_SIZE_Y;
    int pref_font_size = -1;
    bool pref_font_fixed_pitch = true;
    std::string pref_font_name = TEXT_EDITOR_DEFAULT_FONT;
    std::wstring prefDir = getPreferencesPath();
    std::wstring editorConfFile
        = prefDir + L"/" + utf8_to_wstring(TEXT_EDITOR_PREFERENCES_FILENAME);
    bool bIsFile = boost::filesystem::exists(editorConfFile)
        && !boost::filesystem::is_directory(editorConfFile);
    if (bIsFile) {
        std::string jsonString = "";
        std::string tmpline;
#ifdef _MSC_VER
        std::ifstream jsonFile(editorConfFile);
#else
        std::ifstream jsonFile(wstring_to_utf8(editorConfFile));
#endif
        if (jsonFile.is_open()) {
            while (safegetline(jsonFile, tmpline)) {
                jsonString += tmpline + '\n';
            }
            jsonFile.close();
            boost::property_tree::ptree pt;
            std::istringstream is(jsonString);
            try {
                boost::property_tree::read_json(is, pt);
                if (pt.count("FONT_NAME") != 0) {
                    pref_font_name = pt.get<std::string>("FONT_NAME");
                }
                if (pt.count("FONT_SIZE") != 0) {
                    pref_font_size = pt.get<int>("FONT_SIZE");
                }
                if (pt.count("FONT_FIXED_PITCH") != 0) {
                    pref_font_fixed_pitch = pt.get<bool>("FONT_FIXED_PITCH");
                }
                recentFiles.clear();
                if (pt.count("RECENT_FILES") != 0) {
                    std::vector<std::string> fruits;
                    for (boost::property_tree::ptree::value_type& names :
                        pt.get_child("RECENT_FILES")) {
                        recentFiles.push_back(utf8_to_wstring(names.second.data()));
                    }
                }
                pref_pos_x = pt.get<int>("POSITION_X");
                pref_pos_y = pt.get<int>("POSITION_Y");
                pref_sz_x = pt.get<int>("SIZE_X");
                pref_sz_y = pt.get<int>("SIZE_Y");
            } catch (const boost::property_tree::json_parser::json_parser_error& je) {
                je.message();
            }
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
