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
#include "BytecodeIO.hpp"
#include "MacroFunctionDef.hpp"
#include "ClassdefParser.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <sstream>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct StoredFunction
    {
        uint32_t parent = UINT32_MAX;
        bool local = false;
        bool nested = false;
        std::unique_ptr<BytecodeChunk> chunk;
    };
    //=============================================================================
    std::vector<StoredFunction>
    readFunctions(const std::string& payload)
    {
        BytecodeIO::Reader reader(payload);
        const auto count = BytecodeIO::read<uint32_t>(reader);
        BytecodeIO::require(count > 0 && count <= 4096, "Invalid bytecode function count.");
        std::vector<StoredFunction> functions;
        functions.reserve(count);
        for (uint32_t i = 0; i < count; ++i) {
            StoredFunction function;
            function.parent = BytecodeIO::read<uint32_t>(reader);
            function.local = BytecodeIO::readBool(reader);
            function.nested = BytecodeIO::readBool(reader);
            BytecodeIO::require(function.parent == UINT32_MAX || function.parent < i,
                "Invalid bytecode lexical parent.");
            BytecodeIO::require(
                i != 0 || (!function.local && !function.nested && function.parent == UINT32_MAX),
                "Invalid bytecode entry function.");
            function.chunk = BytecodeIO::readChunk(reader);
            BytecodeIO::require(
                i == 0 || !function.chunk->isScript, "Only the bytecode entry may be a script.");
            functions.push_back(std::move(function));
        }
        BytecodeIO::require(reader.atEnd(), "Unexpected trailing bytecode payload.");
        return functions;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
isBytecodeArtifact(const std::wstring& filename)
{
    return std::filesystem::u8path(wstring_to_utf8(filename)).extension() == ".nbc";
}
//=============================================================================
void
compileBytecodeArtifact(const std::wstring& source, const std::wstring& destination)
{
    BytecodeIO::require(
        isBytecodeArtifact(destination), "Bytecode destination must have the .nbc extension.");
    BytecodeIO::require(
        !isBytecodeArtifact(source), "Bytecode compilation requires a source file.");
    BytecodeIO::require(!classdefIsClassdefFile(source),
        "Class definitions require source representation in the application archive.");
    MacroFunctionDef entry(source, false, false);
    std::vector<MacroFunctionDef*> functions;
    std::unordered_map<MacroFunctionDef*, uint32_t> indices;
    for (auto* function = &entry; function != nullptr; function = function->nextFunction) {
        BytecodeIO::require(functions.size() < 4096 && indices.count(function) == 0,
            "Bytecode function count limit exceeded.");
        indices.emplace(function, static_cast<uint32_t>(functions.size()));
        functions.push_back(function);
        function->ensureBytecodeCompiled();
        BytecodeIO::require(
            function->bytecodeChunk != nullptr, "Cannot compile function bytecode.");
    }
    std::string payload;
    BinaryWriter writer(payload);
    writer.writePod<uint32_t>(static_cast<uint32_t>(functions.size()));
    for (auto* function : functions) {
        uint32_t parent = UINT32_MAX;
        if (function->prevFunction != nullptr) {
            const auto found = indices.find(function->prevFunction);
            BytecodeIO::require(
                found != indices.end(), "Bytecode lexical parent is outside the source unit.");
            parent = found->second;
        }
        writer.writePod(parent);
        writer.writeU8(function->localFunction);
        writer.writeU8(function->nestedFunction);
        BytecodeIO::writeChunk(writer, *function->bytecodeChunk);
        BytecodeIO::require(payload.size() <= BytecodeIO::maxArtifactBytes - 1024,
            "Bytecode artifact size limit exceeded.");
    }
    // Validate the same representation that a future process will load before
    // creating any output file.
    readFunctions(payload);
    BytecodeIO::writeArtifactPayload(destination, payload);
}
//=============================================================================
void
loadBytecodeArtifact(MacroFunctionDef& function, const std::wstring& filename)
{
    BytecodeIO::require(function.code == nullptr && function.bytecodeChunk == nullptr
            && function.nextFunction == nullptr,
        "Bytecode loading requires an empty function definition.");
    const std::wstring artifactPath = filename;
    auto stored = readFunctions(BytecodeIO::readArtifactPayload(artifactPath));
    std::vector<std::unique_ptr<MacroFunctionDef>> owned;
    std::vector<MacroFunctionDef*> definitions { &function };
    owned.reserve(stored.size() - 1);
    definitions.reserve(stored.size());
    for (size_t i = 1; i < stored.size(); ++i) {
        owned.push_back(std::make_unique<MacroFunctionDef>());
        definitions.push_back(owned.back().get());
    }
    for (size_t i = 0; i < stored.size(); ++i) {
        auto* definition = definitions[i];
        auto& item = stored[i];
        definition->setFilename(artifactPath);
        definition->setIsScript(item.chunk->isScript);
        definition->setName(i == 0
                ? std::filesystem::u8path(wstring_to_utf8(artifactPath)).stem().u8string()
                : item.chunk->functionName);
        definition->localFunction = item.local;
        definition->nestedFunction = item.nested;
        definition->arguments = item.chunk->argNames;
        definition->returnVals = item.chunk->retNames;
        definition->classdefOwnerClassName = item.chunk->classdefAccessClassName;
        item.chunk->selfFunction = definition;
        item.chunk->selfIsNestedMacro = item.nested;
        item.chunk->sourcePath = artifactPath;
        if (!item.chunk->isScript) {
            item.chunk->functionName = definition->getName();
        }
        definition->bytecodeChunk = std::move(item.chunk);
    }
    // Publish the raw ownership chain only after every allocation has
    // succeeded. Lexical parents may skip siblings but never point forward.
    for (size_t i = 0; i < stored.size(); ++i) {
        definitions[i]->prevFunction
            = stored[i].parent == UINT32_MAX ? nullptr : definitions[stored[i].parent];
        if (i > 0) {
            definitions[i - 1]->nextFunction = owned[i - 1].release();
        }
    }
}
//=============================================================================
std::string
inspectBytecodeArtifact(const std::wstring& filename)
{
    const auto functions = readFunctions(BytecodeIO::readArtifactPayload(filename));
    std::ostringstream result;
    result << "NBC format=" << BytecodeIO::formatVersion << " functions=" << functions.size()
           << "\n";
    for (size_t i = 0; i < functions.size(); ++i) {
        const auto& function = functions[i];
        result << "unit=" << i << " parent=" << function.parent << " local=" << function.local
               << " nested=" << function.nested << "\n";
        result << function.chunk->disassemble();
    }
    return result.str();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
