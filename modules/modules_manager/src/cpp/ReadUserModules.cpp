//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include "ReadUserModules.hpp"
#include "GetExternalModulesPath.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (!myline.empty() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
bool
ReadUserModules(
    std::vector<std::tuple<std::wstring, std::wstring, bool>>& userModules, bool reverse)
{
    std::wstring modulesJsonPath = GetExternalModulesPath() + std::wstring(L"modules.json");
    std::string jsonString;
#ifdef _MSC_VER
    std::ifstream jsonFile(modulesJsonPath);
#else
    std::ifstream jsonFile(wstring_to_utf8(modulesJsonPath));
#endif
    bool succeeded = false;
    if (jsonFile.is_open()) {
        std::string tmpline;
        while (safegetline(jsonFile, tmpline)) {
            jsonString += tmpline + '\n';
        }
        jsonFile.close();
        boost::property_tree::ptree root;
        boost::property_tree::ptree root2;
        std::istringstream is(jsonString);
        bool parse;
        try {
            boost::property_tree::read_json(is, root);
            root2 = root;
            parse = true;
        } catch (const boost::property_tree::json_parser::json_parser_error&) {
            parse = false;
        }
        if (parse) {
            succeeded = true;
            for (auto& e : root.get_child("")) {
                try {
                    std::string name = e.first.c_str();
                    std::string path = e.second.get<std::string>("path");
                    bool load = e.second.get<bool>("load");
                    std::tuple<std::wstring, std::wstring, bool> module
                        = std::make_tuple(utf8_to_wstring(name), utf8_to_wstring(path), load);
                    userModules.push_back(module);
                } catch (const boost::property_tree::json_parser::json_parser_error&) {
                    succeeded = false;
                }
            }
            if (reverse) {
                if (!userModules.empty()) {
                    std::reverse(userModules.begin(), userModules.end());
                }
            }
        }
    }
    return succeeded;
}
//=============================================================================
}
//=============================================================================
