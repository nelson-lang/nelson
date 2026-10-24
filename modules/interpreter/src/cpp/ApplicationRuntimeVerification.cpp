//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BytecodeArtifact.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "BytecodeIO.hpp"
#include "characters_encoding.hpp"
#include "nlohmann/json.hpp"
#include "picoSHA2/picosha2.h"
#include <map>
#include <set>
//=============================================================================
namespace Nelson {
std::string
bytecodeEngineFingerprint()
{
    return BytecodeIO::engineFingerprint();
}
//=============================================================================
void
verifyApplicationRuntime(const std::wstring& manifest)
{
    using Json = nlohmann::json;
    BytecodeIO::require(manifest.size() <= 16 * 1024 * 1024, "Application manifest is too large.");
    const auto expected = Json::parse(wstring_to_utf8(manifest));
    BytecodeIO::require(expected.at("engineFingerprint") == BytecodeIO::engineFingerprint(),
        "Incompatible application engine.");
    const auto& catalog = expected.at("builtinCatalog");
    BytecodeIO::require(
        catalog.is_array() && catalog.size() <= 10000, "Invalid application builtin catalog.");
    std::map<std::string, BuiltInFunctionDef*> available;
    for (auto* definition : BuiltInFunctionDefManager::getInstance()->getTable()) {
        auto* builtin = static_cast<BuiltInFunctionDef*>(definition);
        available.emplace(builtin->getName(), builtin);
    }
    Json actual = Json::array();
    std::set<std::string> names;
    for (const auto& item : catalog) {
        const auto name = item.at("name").get<std::string>();
        const auto found = available.find(name);
        BytecodeIO::require(found != available.end() && names.insert(name).second,
            "Missing or duplicate application builtin.");
        const auto* builtin = found->second;
        Json signature { { "name", name }, { "module", wstring_to_utf8(builtin->moduleName) },
            { "inputs", builtin->argCount }, { "outputs", builtin->retCount },
            { "prototype", builtin->builtinPrototype } };
        BytecodeIO::require(signature == item, "Incompatible application builtin signature.");
        actual.push_back(std::move(signature));
    }
    BytecodeIO::require(
        expected.at("builtinCatalogDigest") == picosha2::hash256_hex_string(actual.dump()),
        "Incompatible application builtin catalog.");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
