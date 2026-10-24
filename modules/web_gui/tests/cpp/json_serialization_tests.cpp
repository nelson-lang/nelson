//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebGuiJson.hpp"
#include <cassert>
#include <string>
//=============================================================================
int
main()
{
    std::string invalidUtf8 = "invalid-";
    invalidUtf8.push_back(static_cast<char>(0xFF));
    invalidUtf8 += "-name";

    const nlohmann::json response = { { "id", 1 }, { "result", { { "path", invalidUtf8 } } } };
    const std::string serialized = Nelson::serializeWebGuiJson(response);
    const nlohmann::json parsed = nlohmann::json::parse(serialized);

    assert(parsed["id"] == 1);
    assert(parsed["result"]["path"] == "invalid-\xEF\xBF\xBD-name");
    return 0;
}
//=============================================================================
