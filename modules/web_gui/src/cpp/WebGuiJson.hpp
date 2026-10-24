//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <nlohmann/json.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
// POSIX paths may contain arbitrary bytes. Keep every WebUI payload valid JSON
// instead of allowing a UTF-8 serialization error to escape a C callback.
inline std::string
serializeWebGuiJson(const nlohmann::json& value)
{
    return value.dump(-1, ' ', false, nlohmann::json::error_handler_t::replace);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
