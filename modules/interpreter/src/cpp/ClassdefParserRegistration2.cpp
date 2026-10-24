//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserRegistration.hpp"
#include "ClassdefMixinNames.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "ClassdefParserValidation.hpp"
#include "ClassdefParserGenerator.hpp"
#include "ClassdefParserNative.hpp"
#include "AbstractSyntaxTree.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Evaluator.hpp"
#include "FunctionsInMemory.hpp"
#include "OverloadName.hpp"
#include "ParserInterface.hpp"
#include "PathFunctionIndexerManager.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
void
registerMixinSupport(const ClassdefDefinition& definition, const std::wstring& filename)
{
    if (definitionUsesCompactDisplayProvider(definition)) {
        // nelson.mixin.CustomCompactDisplayProvider: data-representation helpers
        // for nelson.display.CompactDisplayRepresentation.
        if (!hasInstanceMethod(definition, "widthConstrainedDataRepresentation")) {
            std::string source
                = "function rep = widthConstrainedDataRepresentation(obj, config, width, "
                  "varargin)\n";
            source += "  rep = nelson.display.internal.buildCompactRepresentation(config, "
                      "width, varargin{:});\n";
            source += "end\n";
            registerOwnedGeneratedFunction(definition, filename,
                getOverloadFunctionName(definition.name, "widthConstrainedDataRepresentation"),
                source);
        }
        if (!hasInstanceMethod(definition, "fullDataRepresentation")) {
            std::string source = "function rep = fullDataRepresentation(obj, config, varargin)\n";
            source += "  rep = nelson.display.internal.buildCompactRepresentation(config, Inf, "
                      "varargin{:});\n";
            source += "end\n";
            registerOwnedGeneratedFunction(definition, filename,
                getOverloadFunctionName(definition.name, "fullDataRepresentation"), source);
        }
        // Bridge called by the container-display hook. It returns the
        // PaddedDisplayOutput text of compactRepresentationForSingleLine.
        std::string bridge = "function __nls_s = __nls_compact_render__(obj, __nls_w)\n";
        bridge += "  __nls_cfg = nelson.display.DisplayConfiguration();\n";
        bridge += "  __nls_rep = obj.compactRepresentationForSingleLine(__nls_cfg, __nls_w);\n";
        bridge += "  if isa(__nls_rep, 'nelson.display.CompactDisplayRepresentation')\n";
        bridge += "    __nls_s = char(__nls_rep.PaddedDisplayOutput);\n";
        bridge += "  else\n";
        bridge += "    __nls_s = char(__nls_rep);\n";
        bridge += "  end\n";
        bridge += "end\n";
        registerOwnedGeneratedFunction(definition, filename,
            getOverloadFunctionName(definition.name, "__nls_compact_render__"), bridge);
    }
    if (definitionUsesCopyable(definition)
        && !hasInstanceMethod(definition, CLASSDEF_FUNCTION_COPY)) {
        MacroFunctionDef* macro = buildForwardingFunction(
            "value", CLASSDEF_FUNCTION_COPY, { "obj" }, CLASSDEF_COPY_BUILTIN, filename);
        if (macro != nullptr) {
            macro->classdefOwnerClassName = definition.name;
        }
        registerGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_COPY), macro);
    }

    if (definitionUsesSetGet(definition)) {
        // Property reads and writes go through the classdef handle builtins:
        // dot access on the object would dispatch back to get/set.
        // nelson.mixin.SetGetExactNames derives from SetGet but matches property
        // names case-sensitively (strcmp) instead of case-insensitively (strcmpi).
        const std::string nameCompare
            = definitionUsesSetGetExactNames(definition) ? "strcmp" : "strcmpi";
        if (!hasInstanceMethod(definition, "get")) {
            std::string source = "function varargout = get(obj, varargin)\n";
            source += "if nargin == 1\n";
            source += "  s = struct();\n";
            source += "  names = properties(obj);\n";
            source += "  for k = 1:numel(names)\n";
            source += "    s.(names{k}) = " + std::string(CLASSDEF_HANDLE_GET_BUILTIN)
                + "(obj, names{k});\n";
            source += "  end\n";
            source += "  varargout{1} = s;\n";
            source += "  return\n";
            source += "end\n";
            source += "name = varargin{1};\n";
            source += "if iscell(name)\n";
            source += "  values = cell(size(name));\n";
            source += "  for k = 1:numel(name)\n";
            source += "    values{k} = get(obj, name{k});\n";
            source += "  end\n";
            source += "  varargout{1} = values;\n";
            source += "  return\n";
            source += "end\n";
            source += "if isstring(name)\n";
            source += "  name = char(name);\n";
            source += "end\n";
            source += "names = properties(obj);\n";
            source += "idx = find(" + nameCompare + "(names, name));\n";
            source += "if isempty(idx)\n";
            source += "  error(sprintf(_('Unrecognized property ''%s''.'), name));\n";
            source += "end\n";
            source += "varargout{1} = " + std::string(CLASSDEF_HANDLE_GET_BUILTIN)
                + "(obj, names{idx(1)});\n";
            source += "end\n";
            registerOwnedGeneratedFunction(
                definition, filename, getOverloadFunctionName(definition.name, "get"), source);
        }
        if (!hasInstanceMethod(definition, "set")) {
            std::string source = "function varargout = set(obj, varargin)\n";
            source += "if nargin == 1\n";
            source += "  varargout{1} = properties(obj);\n";
            source += "  return\n";
            source += "end\n";
            source += "if mod(numel(varargin), 2) ~= 0\n";
            source += "  error(_('Name-value pairs expected.'));\n";
            source += "end\n";
            source += "names = properties(obj);\n";
            source += "for k = 1:2:numel(varargin)\n";
            source += "  name = varargin{k};\n";
            source += "  if isstring(name)\n";
            source += "    name = char(name);\n";
            source += "  end\n";
            source += "  idx = find(" + nameCompare + "(names, name));\n";
            source += "  if isempty(idx)\n";
            source += "    error(sprintf(_('Unrecognized property ''%s''.'), name));\n";
            source += "  end\n";
            source += "  " + std::string(CLASSDEF_HANDLE_SET_BUILTIN)
                + "(obj, names{idx(1)}, varargin{k + 1});\n";
            source += "end\n";
            source += "end\n";
            registerOwnedGeneratedFunction(
                definition, filename, getOverloadFunctionName(definition.name, "set"), source);
        }
    }

    registerRedefinesIndexingDerivedMethods(definition, filename);
    if (!definitionUsesCustomDisplay(definition)) {
        return;
    }
    const bool hasDisplayScalar
        = hasInstanceMethod(definition, CLASSDEF_FUNCTION_DISPLAY_SCALAR_OBJECT);
    const bool hasGetHeader = hasInstanceMethod(definition, "getHeader");
    const bool hasGetFooter = hasInstanceMethod(definition, "getFooter");
    const bool hasGetPropertyGroups = hasInstanceMethod(definition, "getPropertyGroups");
    if (!hasDisplayScalar) {
        std::string source = "function ";
        source += CLASSDEF_FUNCTION_DISPLAY_SCALAR_OBJECT;
        source += "(obj)\n";
        source += hasGetHeader ? "  __nls_h = getHeader(obj);\n" : "  __nls_h = [];\n";
        source
            += hasGetPropertyGroups ? "  __nls_g = getPropertyGroups(obj);\n" : "  __nls_g = [];\n";
        source += hasGetFooter ? "  __nls_f = getFooter(obj);\n" : "  __nls_f = [];\n";
        source += "  nelson.mixin.util.renderCustomDisplay(obj, __nls_h, __nls_g, __nls_f);\n";
        source += "end\n";
        registerOwnedGeneratedFunction(definition, filename,
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISPLAY_SCALAR_OBJECT),
            source);
    }
    if (!hasInstanceMethod(definition, "displayNonScalarObject")) {
        std::string source = "function displayNonScalarObject(obj)\n";
        source += "  nelson.mixin.util.renderCustomDisplayArray(obj);\n";
        source += "end\n";
        registerOwnedGeneratedFunction(definition, filename,
            getOverloadFunctionName(definition.name, "displayNonScalarObject"), source);
    }
    if (!hasInstanceMethod(definition, "displayEmptyObject")) {
        std::string source = "function displayEmptyObject(obj)\n";
        source += "  nelson.mixin.util.renderCustomDisplayEmpty(obj);\n";
        source += "end\n";
        registerOwnedGeneratedFunction(definition, filename,
            getOverloadFunctionName(definition.name, "displayEmptyObject"), source);
    }
    // Shape-aware dispatch: scalar -> displayScalarObject, non-scalar ->
    // displayNonScalarObject, empty -> displayEmptyObject.
    std::string dispatch = "  if isempty(obj)\n";
    dispatch += "    displayEmptyObject(obj);\n";
    dispatch += "  elseif isscalar(obj)\n";
    dispatch += "    ";
    dispatch += CLASSDEF_FUNCTION_DISPLAY_SCALAR_OBJECT;
    dispatch += "(obj);\n";
    dispatch += "  else\n";
    dispatch += "    displayNonScalarObject(obj);\n";
    dispatch += "  end\n";
    if (!hasInstanceMethod(definition, CLASSDEF_FUNCTION_DISPLAY)) {
        std::string source = "function display(obj, varargin)\n";
        source += "  if nargin > 1\n";
        source += "    __nls_name = varargin{1};\n";
        source += "  else\n";
        source += "    __nls_name = inputname(1);\n";
        source += "  end\n";
        source += "  nelson.mixin.util.displayCustomName(__nls_name);\n";
        source += dispatch;
        source += "end\n";
        registerOwnedGeneratedFunction(definition, filename,
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISPLAY), source);
    }
    if (!hasInstanceMethod(definition, CLASSDEF_FUNCTION_DISP)) {
        std::string source = "function disp(obj)\n";
        source += dispatch;
        source += "end\n";
        registerOwnedGeneratedFunction(definition, filename,
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISP), source);
    }
}
//=============================================================================
void
registerRedefinesIndexingDerivedMethods(
    const ClassdefDefinition& definition, const std::wstring& filename)
{
    // Redefines{Paren,Brace,Dot}: inject the size/cat-derived methods (Paren)
    // and the public dispatchers that wrap raw indices into a
    // nelson.indexing.IndexingOperation. Each is injected only when the subclass
    // does not define it.
    auto inject = [&](const char* name, const std::string& body) {
        if (!hasInstanceMethod(definition, name)) {
            registerOwnedGeneratedFunction(
                definition, filename, getOverloadFunctionName(definition.name, name), body);
        }
    };
    if (definitionUsesRedefinesBrace(definition)) {
        inject("__nls_ridx_brace_ref__",
            "function varargout = __nls_ridx_brace_ref__(obj, varargin)\n"
            "  if nargin == 2 && isa(varargin{1}, '"
                + std::string(NLS_INDEXING_OPERATION)
                + "')\n"
                  "    [varargout{1:nargout}] = obj.braceReference(varargin{1});\n"
                  "    return\n"
                  "  end\n"
                  "  if nargin == 2 && iscell(varargin{1})\n"
                  "    __nls_idx = varargin{1};\n"
                  "  else\n"
                  "    __nls_idx = varargin;\n"
                  "  end\n"
                  "  __nls_op = "
                + std::string(NLS_INDEXING_OPERATION)
                + "('Brace', __nls_idx);\n"
                  "  [varargout{1:nargout}] = obj.braceReference(__nls_op);\nend\n");
        inject("__nls_ridx_brace_asgn__",
            "function obj = __nls_ridx_brace_asgn__(obj, __nls_indices, __nls_val)\n"
            "  if iscell(__nls_indices)\n"
            "    __nls_idx = __nls_indices;\n"
            "  else\n"
            "    __nls_idx = {__nls_indices};\n"
            "  end\n"
            "  __nls_op = "
                + std::string(NLS_INDEXING_OPERATION)
                + "('Brace', __nls_idx);\n"
                  "  obj = obj.braceAssign(__nls_op, __nls_val);\nend\n");
    }
    if (definitionUsesRedefinesDot(definition)) {
        inject("__nls_ridx_dot_ref__",
            "function varargout = __nls_ridx_dot_ref__(obj, __nls_name)\n"
            "  if isa(__nls_name, '"
                + std::string(NLS_INDEXING_OPERATION)
                + "')\n"
                  "    [varargout{1:nargout}] = obj.dotReference(__nls_name);\n"
                  "    return\n"
                  "  end\n"
                  "  __nls_op = "
                + std::string(NLS_INDEXING_OPERATION)
                + "('Dot', {}, __nls_name);\n"
                  "  [varargout{1:nargout}] = obj.dotReference(__nls_op);\nend\n");
        inject("__nls_ridx_dot_asgn__",
            "function obj = __nls_ridx_dot_asgn__(obj, __nls_name, __nls_val)\n"
            "  __nls_op = "
                + std::string(NLS_INDEXING_OPERATION)
                + "('Dot', {}, __nls_name);\n"
                  "  obj = obj.dotAssign(__nls_op, __nls_val);\nend\n");
    }
    if (!definitionUsesRedefinesParen(definition)) {
        return;
    }
    inject("numel", "function n = numel(obj)\n  n = prod(size(obj));\nend\n");
    inject("ndims", "function n = ndims(obj)\n  n = numel(size(obj));\nend\n");
    inject("isempty", "function tf = isempty(obj)\n  tf = any(size(obj) == 0);\nend\n");
    inject("length",
        "function n = length(obj)\n  __nls_s = size(obj);\n  if any(__nls_s == 0)\n    n = "
        "0;\n  else\n    n = max(__nls_s);\n  end\nend\n");
    inject("end",
        "function ind = end(obj, k, n)\n  __nls_s = size(obj);\n  if k < n\n    ind = "
        "__nls_s(k);\n  else\n    ind = prod(__nls_s(k:end));\n  end\nend\n");
    inject("horzcat", "function r = horzcat(varargin)\n  r = cat(2, varargin{:});\nend\n");
    inject("vertcat", "function r = vertcat(varargin)\n  r = cat(1, varargin{:});\nend\n");
    // Public dispatchers forward to the protected parenReference / parenAssign of
    // the subclass. The interpreter routes paren indexing of a RedefinesParen
    // object here so the hook is reachable from any context and receives indexOp.
    inject("__nls_ridx_paren_ref__",
        "function varargout = __nls_ridx_paren_ref__(obj, varargin)\n"
        "  if nargin == 2 && isa(varargin{1}, '"
            + std::string(NLS_INDEXING_OPERATION)
            + "')\n"
              "    [varargout{1:nargout}] = obj.parenReference(varargin{1});\n"
              "    return\n"
              "  end\n"
              "  if nargin == 2 && iscell(varargin{1})\n"
              "    __nls_idx = varargin{1};\n"
              "  else\n"
              "    __nls_idx = varargin;\n"
              "  end\n"
              "  __nls_op = "
            + std::string(NLS_INDEXING_OPERATION)
            + "('Paren', __nls_idx);\n"
              "  [varargout{1:nargout}] = obj.parenReference(__nls_op);\nend\n");
    inject("__nls_ridx_paren_asgn__",
        "function obj = __nls_ridx_paren_asgn__(obj, __nls_indices, __nls_val)\n"
        "  if iscell(__nls_indices)\n"
        "    __nls_idx = __nls_indices;\n"
        "  else\n"
        "    __nls_idx = {__nls_indices};\n"
        "  end\n"
        "  __nls_op = "
            + std::string(NLS_INDEXING_OPERATION)
            + "('Paren', __nls_idx);\n"
              "  obj = obj.parenAssign(__nls_op, __nls_val);\nend\n");
}
//=============================================================================
const ClassdefMethodDefinition*
findConstructorMethod(const ClassdefDefinition& definition)
{
    for (const auto& method : definition.methods) {
        if (equalsClassNameOrLeaf(method.name, definition.name)) {
            return &method;
        }
    }
    return nullptr;
}
//=============================================================================
bool
isAbstractMethod(const ClassdefMethodDefinition& method)
{
    return attributeListContains(method.attributes, CLASSDEF_ATTR_ABSTRACT);
}
//=============================================================================
bool
isAbstractProperty(const ClassdefPropertyDefinition& property)
{
    return attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT);
}
//=============================================================================
bool
hasUnimplementedAbstractMethods(const ClassdefDefinition& definition)
{
    for (const auto& method : classdefMethodDefinitions(definition.name)) {
        if (!isAbstractMethod(method)) {
            continue;
        }
        // The merged method list keeps an inherited abstract declaration when
        // the implementing method is an external file (@Class/method.m) that
        // is not declared in the classdef, so it never overrides the abstract
        // entry by name. Such a file still satisfies the abstract requirement,
        // so probe for a concrete implementation before treating the class as
        // abstract.
        std::string resolved;
        const bool implemented = method.isStatic
            ? classdefResolveStaticMethodFunction(
                  definition.name, method.name, resolved, definition.name)
            : classdefResolveMethodFunction(
                  definition.name, method.name, resolved, definition.name);
        if (!implemented) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
hasUnimplementedAbstractProperties(const ClassdefDefinition& definition)
{
    for (const auto& property : classdefPropertyDefinitions(definition.name)) {
        if (isAbstractProperty(property)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
hasExplicitDefaultValue(const ClassdefPropertyDefinition& property)
{
    const std::string value = trim(property.defaultExpression);
    return !value.empty() && value != CLASSDEF_DEFAULT_PROPERTY_VALUE;
}
//=============================================================================
std::string
abstractPropertyDefaultValueName(const ClassdefDefinition& definition)
{
    for (const auto& property : definition.properties) {
        if (isAbstractProperty(property) && hasExplicitDefaultValue(property)) {
            return property.name;
        }
    }
    for (const auto& superclass : definition.superclasses) {
        const std::string superclassName = trim(superclass);
        if (superclassName.empty() || !classdefLoadClass(superclassName)) {
            continue;
        }
        const ClassdefDefinition* superclassDefinition = findClassdefDefinition(superclassName);
        if (superclassDefinition == nullptr) {
            continue;
        }
        const std::string propertyName = abstractPropertyDefaultValueName(*superclassDefinition);
        if (!propertyName.empty()) {
            return propertyName;
        }
    }
    return {};
}
//=============================================================================
std::string
privateAbstractMethodName(const ClassdefDefinition& definition)
{
    for (const auto& method : classdefMethodDefinitions(definition.name)) {
        if (isAbstractMethod(method)
            && equalsIgnoreCase(accessAttributeValue(method.attributes), CLASSDEF_ACCESS_PRIVATE)) {
            return method.name;
        }
    }
    return {};
}
//=============================================================================
std::string
abstractClassConstructorSource(const ClassdefDefinition& definition)
{
    std::string source = "function obj = " + classNameLeaf(definition.name) + "()\n";
    const std::string abstractPropertyDefault = abstractPropertyDefaultValueName(definition);
    if (!abstractPropertyDefault.empty()) {
        source += "error('Nelson:class:abstract', 'Abstract property cannot have an initial "
                  "value: "
            + abstractPropertyDefault + "');\n";
    } else if (const std::string privateMethod = privateAbstractMethodName(definition);
        !privateMethod.empty()) {
        source += "error('Nelson:class:abstract', 'Abstract method cannot have private Access: "
            + privateMethod + "');\n";
    } else {
        source += "error('Nelson:class:abstract', 'Cannot instantiate abstract class: "
            + definition.name + "');\n";
    }
    source += "end\n";
    return source;
}
//=============================================================================
void
registerSuperclassArray(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const ClassdefMethodDefinition* constructor = findConstructorMethod(definition);
    if (constructor == nullptr) {
        return;
    }
    GeneratedClassdefSource generated = injectConstructorInitialization(definition, *constructor);
    if (generated.source.empty()) {
        return;
    }
    renameGeneratedFunctionLine(
        generated, classNameLeaf(definition.name), generatedConstructorFunctionName(definition));
    generated.source = appendClassdefLocalFunctions(definition, generated.source);
    MacroFunctionDef* macro = parseGeneratedFunction(generated.source, filename,
        constructor->sourceStartLine == 0 ? 0 : constructor->sourceStartLine - 1,
        generated.sourceLineMap);
    if (macro != nullptr) {
        macro->classdefOwnerClassName = definition.name;
        macro->classdefConstructor = true;
        macro->classdefConstructorRestricted = !equalsIgnoreCase(
            accessAttributeValue(constructor->attributes), CLASSDEF_ACCESS_PUBLIC);
    }
    registerGeneratedFunction(generatedConstructorFunctionName(definition), macro);
}
//=============================================================================
void
registerMethods(const ClassdefDefinition& definition, const std::wstring& filename)
{
    for (const auto& method : definition.methods) {
        if (equalsClassNameOrLeaf(method.name, definition.name) || method.source.empty()) {
            continue;
        }
        std::string source
            = appendClassdefLocalFunctions(definition, generatedMethodSource(definition, method));
        MacroFunctionDef* macro = parseGeneratedFunction(
            source, filename, method.sourceStartLine == 0 ? 0 : method.sourceStartLine - 1);
        if (macro != nullptr) {
            macro->classdefOwnerClassName = definition.name;
        }
        if (method.isStatic) {
            registerGeneratedFunction(definition.name + "." + method.name, macro);
        } else if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)
            && definitionIsHandleClass(definition)) {
            const std::string userDeleteName = CLASSDEF_GENERATED_FUNCTION_PREFIX + definition.name
                + "_" + CLASSDEF_FUNCTION_DELETE + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
            if (macro != nullptr) {
                // only ever called by its mangled name from the destructor
                // protocol, with the exact object: never re-dispatched
                macro->overloadAutoMode = NLS_OVERLOAD_AUTO_OFF;
            }
            registerGeneratedFunction(userDeleteName, macro);
            MacroFunctionDef* deleteMacro = buildHandleDeleteFunction(filename);
            if (deleteMacro != nullptr) {
                deleteMacro->classdefOwnerClassName = definition.name;
            }
            registerGeneratedFunction(
                getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE), deleteMacro);
        } else {
            registerGeneratedFunction(getOverloadFunctionName(definition.name, method.name), macro);
        }
    }
}
//=============================================================================
FunctionDef*
buildConstructor(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const ClassdefMethodDefinition* constructor = findConstructorMethod(definition);

    GeneratedClassdefSource generated;
    if (!abstractPropertyDefaultValueName(definition).empty()) {
        generated.source = abstractClassConstructorSource(definition);
    } else if (attributeListContains(definition.attributes, CLASSDEF_ATTR_ABSTRACT)
        || hasUnimplementedAbstractMethods(definition)
        || hasUnimplementedAbstractProperties(definition)) {
        generated.source = abstractClassConstructorSource(definition);
    } else {
        if (constructor == nullptr) {
            generated.source = generatedTypedEnumerationConstructorSource(definition);
            if (generated.source.empty()) {
                // Native default constructor (all-invariant-default classes);
                // registers itself and returns early on success.
                MacroFunctionDef* nativeMacro = buildDefaultConstructor(definition, filename);
                if (nativeMacro != nullptr) {
                    nativeMacro->classdefOwnerClassName = definition.name;
                    nativeMacro->classdefConstructor = true;
                    registerGeneratedFunction(definition.name, nativeMacro);
                    return nativeMacro;
                }
                generated.source = generatedDefaultConstructorSource(definition);
            }
        } else {
            generated = injectConstructorInitialization(definition, *constructor);
        }
    }
    if (generated.source.empty()) {
        return nullptr;
    }
    generated.source = appendClassdefLocalFunctions(definition, generated.source);
    MacroFunctionDef* macro = parseGeneratedFunction(generated.source, filename,
        constructor == nullptr || constructor->sourceStartLine == 0
            ? 0
            : constructor->sourceStartLine - 1,
        generated.sourceLineMap);
    if (macro != nullptr) {
        macro->classdefOwnerClassName = definition.name;
        macro->classdefConstructor = true;
        macro->classdefConstructorRestricted = constructor != nullptr
            && !equalsIgnoreCase(
                accessAttributeValue(constructor->attributes), CLASSDEF_ACCESS_PUBLIC);
    }
    registerGeneratedFunction(definition.name, macro);
    return macro;
}
//=============================================================================
bool
loadDefinitionFromFile(
    const std::wstring& filename, const std::string& expectedName, ClassdefDefinition& definition)
{
    std::string source;
    if (!readTextFile(filename, source)) {
        return false;
    }
    std::string errorMessage;
    // Native token-driven parse.
    if (!parseClassdefSourceNativeFull(source, definition, errorMessage)) {
        return false;
    }
    if (!expectedName.empty() && !equalsIgnoreCase(expectedName, definition.name)) {
        if (!equalsIgnoreCase(classNameLeaf(expectedName), definition.name)) {
            return false;
        }
        definition.name = expectedName;
        for (auto& property : definition.properties) {
            property.definingClassName = definition.name;
        }
    }
    return true;
}
//=============================================================================
std::string
joinAttributeFlags(const stringVector& attributes)
{
    std::string flags;
    for (const auto& attribute : attributes) {
        if (!flags.empty()) {
            flags += ";";
        }
        flags += attribute;
    }
    return flags;
}
//=============================================================================
void
collectInheritanceOrder(
    const std::string& className, stringVector& inheritanceOrder, stringVector& visited)
{
    if (className.empty()
        || std::find(visited.begin(), visited.end(), className) != visited.end()) {
        return;
    }
    visited.push_back(className);
    inheritanceOrder.push_back(className);
    for (const auto& superclass : classdefSuperclasses(className)) {
        collectInheritanceOrder(trim(superclass), inheritanceOrder, visited);
    }
}
//=============================================================================
ClassdefLayoutPtr
buildClassdefLayout(const std::string& className)
{
    auto layout = std::make_shared<ClassdefLayout>();
    layout->className = className;
    const ClassdefDefinition* definition = findClassdefDefinition(className);
    for (const auto& property : classdefPropertyDefinitions(className)) {
        layout->propertyNames.push_back(property.name);
        layout->propertyFlags.push_back(joinAttributeFlags(property.attributes));
        if (!attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
            && !attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
            && !attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
            layout->storagePropertyNames.push_back(property.name);
        }
        if (definition != nullptr && canUseStoredDefaultAssignment(*definition, property)) {
            layout->directStoredPropertyNames.push_back(property.name);
        }
    }
    stringVector visited;
    collectInheritanceOrder(className, layout->inheritanceOrder, visited);
    layout->events = classdefEvents(className);
    layout->enumerations = classdefEnumerations(className);
    layout->methodNames = classdefMethods(className);
    return layout;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
