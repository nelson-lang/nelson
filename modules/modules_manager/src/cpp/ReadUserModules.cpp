//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <fstream>
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
        std::istringstream is(jsonString);
        bool parse;
        try {
            boost::property_tree::read_json(is, root);
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
