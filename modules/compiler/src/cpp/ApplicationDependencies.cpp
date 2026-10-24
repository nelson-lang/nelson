//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ApplicationDependencies.hpp"
#include "BytecodeChunk.hpp"
#include "ClassdefParser.hpp"
#include "Keywords.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "picoSHA2/picosha2.h"
#include <algorithm>
#include <array>
#include <cctype>
#include <fstream>
#include <map>
#include <stdexcept>
#include <tuple>
//=============================================================================
namespace Nelson::Application {
std::string
digestFile(const std::filesystem::path& path)
{
    std::ifstream input(path, std::ios::binary);
    if (!input.good()) {
        throw std::runtime_error("Cannot read application dependency: " + path.generic_u8string());
    }
    picosha2::hash256_one_by_one hash;
    std::array<char, 32768> buffer;
    while (input) {
        input.read(buffer.data(), buffer.size());
        hash.process(buffer.begin(), buffer.begin() + input.gcount());
    }
    if (!input.eof()) {
        throw std::runtime_error("Cannot read application dependency: " + path.generic_u8string());
    }
    hash.finish();
    return picosha2::get_hash_hex_string(hash);
}
//=============================================================================
namespace {
    bool
    isOperation(AbstractSyntaxTreePtr node, OP_TYPE operation)
    {
        return node != nullptr && node->type == non_terminal && node->opNum == operation;
    }
    //=============================================================================
    AbstractSyntaxTreePtr
    unwrap(AbstractSyntaxTreePtr node)
    {
        return isOperation(node, OP_RHS) ? node->down : node;
    }
    //=============================================================================
    std::string
    literal(AbstractSyntaxTreePtr node)
    {
        node = unwrap(node);
        return node != nullptr
                && (node->type == const_character_array_node || node->type == const_string_node)
            ? node->text
            : std::string();
    }
    //=============================================================================
    void
    append(SourceDependencies& result, AbstractSyntaxTreePtr node, const std::string& name,
        const std::string& kind, bool loadFilename = false)
    {
        if (!name.empty()) {
            result.references.push_back({ name, kind,
                node == nullptr ? 0 : node->getContext() & 0xFFFF, {}, loadFilename });
        }
    }
    //=============================================================================
    bool
    callbackProperty(std::string name)
    {
        std::transform(name.begin(), name.end(), name.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return (name.size() >= 3 && name.compare(name.size() - 3, 3, "fcn") == 0)
            || (name.size() >= 8 && name.compare(name.size() - 8, 8, "callback") == 0);
    }
    //=============================================================================
    bool
    possibleCallbackProperty(AbstractSyntaxTreePtr node, const std::set<std::string>& known = {})
    {
        node = unwrap(node);
        if (node == nullptr) {
            return true;
        }
        if (node->type == id_node && node->down == nullptr && known.count(node->text) != 0) {
            return false;
        }
        if (node->type == const_character_array_node || node->type == const_string_node) {
            return callbackProperty(node->text);
        }
        if (isOperation(node, OP_BRACKETS) && isOperation(node->down, OP_SEMICOLON)
            && node->down->right == nullptr) {
            auto* last = node->down->down;
            while (last != nullptr && last->right != nullptr) {
                last = last->right;
            }
            auto suffix = literal(last);
            if (!suffix.empty()) {
                std::transform(suffix.begin(), suffix.end(), suffix.begin(),
                    [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
                for (const std::string ending : { "fcn", "callback" }) {
                    const auto length = std::min(suffix.size(), ending.size());
                    if (suffix.compare(
                            suffix.size() - length, length, ending, ending.size() - length, length)
                        == 0) {
                        return true;
                    }
                }
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    staticCallback(AbstractSyntaxTreePtr node)
    {
        node = unwrap(node);
        if (node == nullptr || isOperation(node, OP_EMPTY) || isOperation(node, OP_EMPTY_CELL)
            || isOperation(node, OP_FUNCTION_HANDLE_NAMED)
            || isOperation(node, OP_FUNCTION_HANDLE_ANONYMOUS)) {
            return true;
        }
        if (node->type == const_character_array_node || node->type == const_string_node) {
            return node->text.empty();
        }
        if (isOperation(node, OP_BRACES) && isOperation(node->down, OP_SEMICOLON)) {
            auto* first = unwrap(node->down->down);
            return isOperation(first, OP_FUNCTION_HANDLE_NAMED)
                || isOperation(first, OP_FUNCTION_HANDLE_ANONYMOUS);
        }
        return false;
    }
    //=============================================================================
    bool
    dynamicCallbackArguments(const std::string& name, AbstractSyntaxTreePtr arguments,
        const std::set<std::string>& known)
    {
        if (name == "set" && arguments != nullptr) {
            for (auto* property = arguments->right; property != nullptr;) {
                auto* value = property->right;
                if (possibleCallbackProperty(property, known)
                    && (value != nullptr ? !staticCallback(value) : literal(property).empty())) {
                    return true;
                }
                property = value == nullptr ? nullptr : value->right;
            }
            return false;
        }
        for (auto* argument = arguments; argument != nullptr; argument = argument->right) {
            if (callbackProperty(literal(argument)) && argument->right != nullptr
                && !staticCallback(argument->right)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    void
    collectCall(SourceDependencies& result, AbstractSyntaxTreePtr node, const std::string& name,
        AbstractSyntaxTreePtr arguments, bool commandForm = false,
        const std::set<std::string>& known = {})
    {
        append(result, node, name, "call");
        if (dynamicCallbackArguments(name, arguments, known)) {
            append(result, node, name, "callback");
        }
        static const std::set<std::string> dynamic { "eval", "evalc", "evalin", "execstr", "feval",
            "str2func", "run", "assignin", "addpath", "rmpath", "path", "cd" };
        if (dynamic.count(name) != 0) {
            append(result, node, name, "dynamic");
        }
        const auto target
            = commandForm && arguments != nullptr ? arguments->text : literal(arguments);
        if (name == "feval" || name == "str2func" || name == "builtin") {
            append(result, node, target, name == "builtin" ? "builtin" : "handle");
            if (name == "builtin" && dynamic.count(target) != 0) {
                append(result, node, target, "dynamic");
            }
        } else if (name == "run") {
            append(result, node, target, "script");
        } else if (name == "dlopen") {
            append(result, node, target, "native-library");
        } else if (name == "fopen" || name == "fileread" || name == "load" || name == "loadmat"
            || name == "loadnh5" || name == "imread" || name == "audioread" || name == "readmatrix"
            || name == "readtable") {
            const auto mode = name == "fopen" && arguments != nullptr && arguments->right != nullptr
                ? (commandForm ? arguments->right->text : literal(arguments->right))
                : std::string();
            if (!mode.empty() && (mode[0] == 'w' || mode[0] == 'a')) {
                return;
            }
            append(result, node, target, "resource", name == "load");
        }
    }
    //=============================================================================
    void
    collectIdentifier(SourceDependencies& result, AbstractSyntaxTreePtr node,
        const std::set<std::string>& locals, const std::set<std::string>& definitions,
        const std::set<std::string>& known)
    {
        if (node == nullptr || node->type != id_node) {
            return;
        }
        std::string name = node->text;
        auto* postfix = node->down;
        while (isOperation(postfix, OP_DOT) && postfix->down != nullptr
            && postfix->down->type == id_node) {
            name += "." + postfix->down->text;
            postfix = postfix->right;
        }
        const bool local = locals.count(node->text) != 0;
        if (!local && definitions.count(name) == 0) {
            if (isOperation(postfix, OP_PARENS)) {
                collectCall(result, node, name, postfix->down, false, known);
            } else if (postfix == nullptr) {
                append(result, node, name, "identifier");
            }
        }
        // A local indexed expression may be an array or a function handle.
        // Retain its possible global target without claiming type inference.
        if (local && isOperation(postfix, OP_PARENS)) {
            append(result, node, name, "possible-call");
        }
    }
    //=============================================================================
    void
    collectCallbackAssignment(
        SourceDependencies& result, AbstractSyntaxTreePtr node, const std::set<std::string>& known)
    {
        auto* lhs = node->down;
        if (lhs == nullptr || lhs->type != id_node || staticCallback(lhs->right)) {
            return;
        }
        for (auto* index = lhs->down; index != nullptr; index = index->right) {
            if (isOperation(index, OP_DOT) && index->down != nullptr
                && callbackProperty(index->down->text)) {
                append(result, node, "callback-property:" + index->down->text, "dynamic");
                return;
            }
            if (isOperation(index, OP_DOTDYN)) {
                if (possibleCallbackProperty(index->down, known)) {
                    append(result, node, "computed-callback-property", "dynamic");
                    return;
                }
            }
        }
    }
    //=============================================================================
    std::set<std::string>
    nonCallbackLocals(AbstractSyntaxTreePtr root, const std::vector<std::string>& arguments)
    {
        std::map<std::string, std::pair<size_t, bool>> writes;
        std::vector<AbstractSyntaxTreePtr> pending;
        if (root != nullptr) {
            pending.push_back(root);
        }
        size_t visited = 0;
        while (!pending.empty()) {
            auto* node = pending.back();
            pending.pop_back();
            if (++visited > 1000000) {
                throw std::runtime_error("Application source tree limit exceeded.");
            }
            if ((node->type == reserved_node && node->tokenNumber == NLS_KEYWORD_TRY)
                || (node->type == id_node
                    && (node->text == "load" || node->text == "global"
                        || node->text == "persistent"))) {
                return {};
            }
            if (isOperation(node, OP_ASSIGN) && node->down != nullptr
                && node->down->type == id_node) {
                auto* lhs = node->down;
                auto& value = writes[lhs->text];
                ++value.first;
                value.second = lhs->down == nullptr && !possibleCallbackProperty(lhs->right);
            }
            if (isOperation(node, OP_MULTICALL) && isOperation(node->down, OP_BRACKETS)
                && isOperation(node->down->down, OP_SEMICOLON)) {
                for (auto* lhs = node->down->down->down; lhs != nullptr; lhs = lhs->right) {
                    auto* target = unwrap(lhs);
                    if (target != nullptr && target->type == id_node) {
                        writes[target->text].first += 2;
                    }
                }
            }
            if (node->type == reserved_node && node->tokenNumber == NLS_KEYWORD_FOR
                && node->down != nullptr) {
                writes[node->down->text].first += 2;
            }
            if (node->right != nullptr) {
                pending.push_back(node->right);
            }
            if (node->down != nullptr) {
                pending.push_back(node->down);
            }
        }
        std::set<std::string> result;
        for (const auto& [name, value] : writes) {
            if (value.first == 1 && value.second
                && std::find(arguments.begin(), arguments.end(), name) == arguments.end()) {
                result.insert(name);
            }
        }
        return result;
    }
    //=============================================================================
    void
    scan(SourceDependencies& result, AbstractSyntaxTreePtr root,
        const std::set<std::string>& locals, const std::set<std::string>& definitions,
        const std::set<std::string>& known)
    {
        std::vector<AbstractSyntaxTreePtr> pending;
        if (root != nullptr) {
            pending.push_back(root);
        }
        size_t visited = 0;
        while (!pending.empty()) {
            auto* node = pending.back();
            pending.pop_back();
            if (++visited > 1000000) {
                throw std::runtime_error("Application source tree limit exceeded.");
            }
            if (node->type == non_terminal) {
                switch (node->opNum) {
                case OP_ASSIGN:
                    collectCallbackAssignment(result, node, known);
                    break;
                case OP_RHS:
                    collectIdentifier(result, node->down, locals, definitions, known);
                    break;
                case OP_SCALL:
                    if (node->down != nullptr && node->down->text != "import"
                        && node->down->text != "global" && node->down->text != "persistent") {
                        collectCall(
                            result, node->down, node->down->text, node->down->right, true, known);
                    }
                    break;
                case OP_FUNCTION_HANDLE_NAMED:
                    if (node->down != nullptr && definitions.count(node->down->text) == 0) {
                        append(result, node, node->down->text, "handle");
                    }
                    break;
                default:
                    break;
                }
            }
            if (node->right != nullptr) {
                pending.push_back(node->right);
            }
            if (node->down != nullptr) {
                pending.push_back(node->down);
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
analyzeFunctions(SourceDependencies& result, MacroFunctionDef& entry)
{
    std::set<std::string> definitions;
    bool nested = false;
    for (auto* function = &entry; function != nullptr; function = function->nextFunction) {
        definitions.insert(function->getName());
        result.definitions.insert(function->getName());
        nested = nested || function->nestedFunction;
    }
    std::map<MacroFunctionDef*, std::vector<std::string>> lexicalImports;
    for (auto* function = &entry; function != nullptr; function = function->nextFunction) {
        function->ensureBytecodeCompiled();
        if (function->bytecodeChunk == nullptr) {
            throw std::runtime_error("Cannot analyze application function bytecode: "
                + wstring_to_utf8(entry.getFilename()) + " (" + function->getName() + ").");
        }
        const auto& chunk = *function->bytecodeChunk;
        const auto firstReference = result.references.size();
        auto& imports = lexicalImports[function];
        if (function->nestedFunction && function->prevFunction != nullptr) {
            imports = lexicalImports[function->prevFunction];
        }
        std::set<std::string> locals(function->arguments.begin(), function->arguments.end());
        locals.insert(chunk.capturedNames.begin(), chunk.capturedNames.end());
        for (size_t i = 0; i < chunk.localAssigned.size(); ++i) {
            if (chunk.localAssigned[i]) {
                locals.insert(chunk.localNames[i]);
            }
        }
        // A single local assignment with a fixed non-callback suffix cannot
        // become a callback property. Shared or externally populated scopes
        // stay conservative; this is not general constant propagation.
        const auto known = nested || entry.getIsScript()
            ? std::set<std::string>()
            : nonCallbackLocals(function->code, function->arguments);
        scan(result, function->code, locals, definitions, known);
        // Argument validation is lowered from opaque parser text. Its names
        // must be retained too, including custom validation functions.
        for (const auto& instruction : chunk.code) {
            if (instruction.op == OpCode::DECLARE_IMPORT) {
                const auto& name = chunk.names[instruction.A];
                if (std::find(imports.begin(), imports.end(), name) == imports.end()) {
                    imports.push_back(name);
                }
            }
            if (instruction.op == OpCode::CALL_NAMED || instruction.op == OpCode::MAKE_FH_NAMED) {
                const auto& name = chunk.names[instruction.A];
                if (locals.count(name) == 0 && definitions.count(name) == 0) {
                    collectCall(result, nullptr, name, nullptr);
                }
            }
        }
        for (size_t i = firstReference; i < result.references.size(); ++i) {
            result.references[i].imports = imports;
        }
    }
}
//=============================================================================
SourceDependencies
analyzeSource(const std::filesystem::path& source)
{
    const auto filename = utf8_to_wstring(source.u8string());
    SourceDependencies result;
    if (source.extension() == ".m" && classdefIsClassdefFile(filename)) {
        result = analyzeClassSource(source);
    } else {
        if (source.extension() == ".nbc") {
            result.references.push_back({ "precompiled-unit", "dynamic", 0 });
        }
        MacroFunctionDef entry(filename, false, false);
        result.script = entry.getIsScript();
        analyzeFunctions(result, entry);
        retainConstructorMethods(result, source);
    }
    std::sort(result.references.begin(), result.references.end(),
        [](const Reference& a, const Reference& b) {
            return std::tie(a.name, a.kind, a.line, a.imports, a.loadFilename)
                < std::tie(b.name, b.kind, b.line, b.imports, b.loadFilename);
        });
    result.references.erase(std::unique(result.references.begin(), result.references.end(),
                                [](const Reference& a, const Reference& b) {
                                    return a.name == b.name && a.kind == b.kind && a.line == b.line
                                        && a.imports == b.imports
                                        && a.loadFilename == b.loadFilename;
                                }),
        result.references.end());
    return result;
}
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
