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
#include "ClassdefParser.hpp"
#include "ClassdefParserGenerator.hpp"
#include "ClassdefParserNative.hpp"
#include "ClassdefParserRegistration.hpp"
#include "ClassdefParserScanner.hpp"
#include "characters_encoding.hpp"
#include <fstream>
#include <iterator>
#include <memory>
//=============================================================================
namespace Nelson::Application {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    void
    analyzeText(SourceDependencies& result, const ClassdefDefinition& definition,
        const fs::path& source, const std::string& text, size_t line = 0)
    {
        auto combined = classdef_detail::appendClassdefLocalFunctions(definition, text);
        std::unique_ptr<MacroFunctionDef> function(classdef_detail::parseGeneratedFunction(
            combined, utf8_to_wstring(source.u8string()), line));
        Application::require(function != nullptr, "Cannot analyze application class method.");
        analyzeFunctions(result, *function);
    }
    //=============================================================================
    void
    analyzeExpression(SourceDependencies& result, const ClassdefDefinition& definition,
        const fs::path& source, const std::string& expression, const std::string& argument = {})
    {
        if (!expression.empty()) {
            analyzeText(result, definition, source,
                "function value = __nelsonc_expression__(" + argument + ")\nvalue = " + expression
                    + ";\nend\n");
        }
    }
    //=============================================================================
    void
    analyzeProperties(
        SourceDependencies& result, const ClassdefDefinition& definition, const fs::path& source)
    {
        for (const auto& property : definition.properties) {
            analyzeExpression(result, definition, source, property.defaultExpression);
            if (property.validationExpression.empty()) {
                continue;
            }
            stringVector dimensions, validators;
            std::string typeName;
            Application::require(
                classdef_detail::parseValidationExpression(
                    property.validationExpression, dimensions, typeName, validators),
                "Cannot analyze application property validation.");
            if (!typeName.empty()) {
                result.references.push_back({ typeName, "class", 0 });
            }
            for (const auto& validator : validators) {
                std::string name;
                stringVector arguments;
                Application::require(
                    classdef_detail::parseValidatorCall(validator, name, arguments),
                    "Cannot analyze application property validator.");
                const auto expression
                    = validator == name ? name + "(" + property.name + ")" : validator;
                analyzeExpression(result, definition, source, expression, property.name);
            }
        }
    }
    //=============================================================================
    void
    retainExternalMethods(
        SourceDependencies& result, const ClassdefDefinition& definition, const fs::path& source)
    {
        auto directory = source.parent_path();
        if (directory.filename().u8string() != "@" + source.stem().u8string()) {
            directory /= fs::u8path("@" + source.stem().u8string());
        }
        for (const auto& method : definition.methods) {
            if (!method.source.empty() || classdef_detail::isAbstractMethod(method)) {
                continue;
            }
            auto path = directory / fs::u8path(method.name + ".m");
            if (!fs::is_regular_file(path)) {
                path.replace_extension(".nbc");
            }
            result.references.push_back(
                { path.lexically_relative(source.parent_path()).generic_u8string(), "method-file",
                    static_cast<int>(method.sourceStartLine) });
        }
        if (!fs::is_directory(directory)) {
            return;
        }
        // Dispatch can reach any method of a retained class, including operators
        // and methods not declared by a prototype in the class definition.
        for (const auto& item : fs::directory_iterator(directory)) {
            const auto path = item.path();
            if (!item.is_regular_file() || path == source
                || (path.extension() != ".m" && path.extension() != ".nbc")) {
                continue;
            }
            auto preferred = path;
            preferred.replace_extension(".m");
            if (path.extension() == ".nbc" && fs::is_regular_file(preferred)) {
                continue;
            }
            result.methods.insert(path.stem().u8string());
            result.references.push_back(
                { path.lexically_relative(source.parent_path()).generic_u8string(), "method-file",
                    0 });
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
retainConstructorMethods(SourceDependencies& result, const std::filesystem::path& source)
{
    if (source.parent_path().filename().u8string() == "@" + source.stem().u8string()) {
        retainExternalMethods(result, ClassdefDefinition {}, source);
    }
}
//=============================================================================
SourceDependencies
analyzeClassSource(const std::filesystem::path& source)
{
    Application::require(
        fs::file_size(source) <= 64 * 1024 * 1024, "Application class source is too large.");
    std::ifstream input(source, std::ios::binary);
    Application::require(input.good(), "Cannot read application class source.");
    const std::string text((std::istreambuf_iterator<char>(input)), {});
    Application::require(!input.bad(), "Cannot read application class source.");
    ClassdefDefinition definition;
    std::string error;
    if (!classdef_detail::parseClassdefSourceNativeFull(text, definition, error)) {
        throw std::runtime_error(
            "Cannot analyze application class: " + source.generic_u8string() + ": " + error);
    }
    SourceDependencies result;
    result.sourceRequired = true;
    result.references.push_back({ "metaclass", "builtin", 0 });
    for (const auto& superclass : definition.superclasses) {
        result.references.push_back({ superclass, "class", 1 });
    }
    for (const auto& method : definition.methods) {
        result.methods.insert(method.name);
        if (!method.source.empty()) {
            analyzeText(result, definition, source,
                classdef_detail::generatedMethodSource(definition, method),
                method.sourceStartLine == 0 ? 0 : method.sourceStartLine - 1);
        }
    }
    analyzeProperties(result, definition, source);
    for (const auto& item : definition.enumerationArguments) {
        analyzeExpression(result, definition, source, "{" + item.second + "}");
    }
    retainExternalMethods(result, definition, source);
    return result;
}
//=============================================================================
} // namespace Nelson::Application
//=============================================================================
