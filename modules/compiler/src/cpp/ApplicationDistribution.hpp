//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include "nlohmann/json.hpp"
#include <string>
//=============================================================================
namespace Nelson::Application::Distribution {
using Json = nlohmann::json;
Json
parse(const std::wstring& encoded);
Json
normalizeReport(Json report);
Json
records(const Json& value);
std::string
text(const Json& value);
std::string
key(const std::string& value, bool windows);
void
validateDestinations(const Json& files, const Json& directories, bool windows);
//=============================================================================
} // namespace Nelson::Application::Distribution
//=============================================================================
