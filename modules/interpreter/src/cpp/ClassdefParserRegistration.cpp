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
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "ClassdefParserValidation.hpp"
#include "ClassdefParserGenerator.hpp"
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
static bool
definesStaticEmpty(const ClassdefDefinition& definition);
//=============================================================================
void
registerGeneratedFunction(const std::string& key, MacroFunctionDef* macro)
{
    if (macro == nullptr) {
        return;
    }
    macro->setOverload(!key.empty() && key.front() == OVERLOAD_SYMBOL_CHAR);
    FunctionsInMemory::getInstance()->deleteMFunction(key);
    FunctionsInMemory::getInstance()->add(key, macro);
}
//=============================================================================
void
unregisterGeneratedFunction(const std::string& key)
{
    FunctionsInMemory::getInstance()->deleteMFunction(key);
}
//=============================================================================
void
unregisterClassdefGeneratedFunctions(const ClassdefDefinition& definition)
{
    unregisterGeneratedFunction(definition.name);
    unregisterGeneratedFunction(generatedConstructorFunctionName(definition));
    for (const auto& property : definition.properties) {
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)) {
            unregisterGeneratedFunction(definition.name + "." + property.name);
        }
    }
    for (const auto& member : definition.enumerations) {
        unregisterGeneratedFunction(definition.name + "." + member);
    }
    unregisterGeneratedFunction(definition.name + "." + CLASSDEF_FUNCTION_EMPTY);
    if (!definition.enumerations.empty()) {
        static const char* numericTypes[]
            = { NLS_UINT8_STR, NLS_INT8_STR, NLS_UINT16_STR, NLS_INT16_STR, NLS_UINT32_STR,
                  NLS_INT32_STR, NLS_UINT64_STR, NLS_INT64_STR, NLS_SINGLE_STR, NLS_DOUBLE_STR };
        for (const char* numericType : numericTypes) {
            unregisterGeneratedFunction(getOverloadFunctionName(definition.name, numericType));
        }
        unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_EQ));
        unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_NE));
        unregisterGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_CHAR));
        static const char* baseSupportFunctions[] = { "string", "islogical", "isnumeric", "isfloat",
            "isinteger", "strcmp", "strcmpi", "isequal", "isequaln" };
        for (const char* functionName : baseSupportFunctions) {
            unregisterGeneratedFunction(getOverloadFunctionName(definition.name, functionName));
        }
    }
    for (const auto& method : definition.methods) {
        if (equalsClassNameOrLeaf(method.name, definition.name) || method.source.empty()) {
            continue;
        }
        if (method.isStatic) {
            unregisterGeneratedFunction(definition.name + "." + method.name);
        } else if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)) {
            unregisterGeneratedFunction(CLASSDEF_GENERATED_FUNCTION_PREFIX + definition.name + "_"
                + CLASSDEF_FUNCTION_DELETE + CLASSDEF_GENERATED_FUNCTION_SUFFIX);
            unregisterGeneratedFunction(
                getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE));
        } else {
            unregisterGeneratedFunction(getOverloadFunctionName(definition.name, method.name));
        }
    }
    unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_GET));
    unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_SET));
    unregisterGeneratedFunction(
        getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_ISVALID));
    unregisterGeneratedFunction(
        getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_FIELDNAMES));
    unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_STRUCT));
    unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_COPY));
    unregisterGeneratedFunction(
        getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISPLAY));
    unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISP));
    unregisterGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE));
}
//=============================================================================
void
registerConstantProperties(const ClassdefDefinition& definition, const std::wstring& filename)
{
    for (const auto& property : definition.properties) {
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
            || !attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)) {
            continue;
        }
        const std::string defaultExpression = property.defaultExpression.empty()
            ? std::string(CLASSDEF_DEFAULT_PROPERTY_VALUE)
            : property.defaultExpression;
        const bool invariant = isEvaluationInvariantDefaultExpression(defaultExpression);
        if (invariant) {
            // value = <expr>; is emitted natively. On a parse miss the source
            // is generated below.
            MacroFunctionDef* nativeMacro
                = buildInvariantConstantPropertyGetter(definition, property, filename);
            if (nativeMacro != nullptr) {
                registerGeneratedFunction(definition.name + "." + property.name, nativeMacro);
                continue;
            }
        }
        std::string source
            = "function value = " + classNameLeaf(definition.name) + "_" + property.name + "()\n";
        if (invariant) {
            source += "value = " + defaultExpression + ";\n";
        } else {
            // The expression runs once per defining class; subclass,
            // instance and cross-class accesses all resolve to this same
            // function, so every later access returns the stored value.
            const std::string keyArguments = "('" + definition.name + "', '" + property.name + "'";
            source += "[" CLASSDEF_PROPERTY_DEFAULT_FOUND_VARIABLE
                      ", value] = " CLASSDEF_PROPERTY_DEFAULT_BUILTIN
                + keyArguments + ");\n";
            source += "if ~" CLASSDEF_PROPERTY_DEFAULT_FOUND_VARIABLE "\n";
            source += "value = " + defaultExpression + ";\n";
            source += std::string(CLASSDEF_PROPERTY_DEFAULT_BUILTIN) + keyArguments + ", value);\n";
            source += "end\n";
        }
        source += "end\n";
        // The default expression may call a local function of the class file
        // (defined after the closing end). Appending that source links the
        // local functions into the getter's nextFunction chain, as for methods
        // and the constructor.
        source = appendClassdefLocalFunctions(definition, source);
        MacroFunctionDef* macro = parseGeneratedFunction(source, filename);
        if (macro != nullptr && !invariant) {
            // The stored-default builtin is restricted to class context.
            macro->classdefOwnerClassName = definition.name;
        }
        registerGeneratedFunction(definition.name + "." + property.name, macro);
    }
}
//=============================================================================
void
registerEmptyStaticMethod(const ClassdefDefinition& definition, const std::wstring& filename)
{
    if (definesStaticEmpty(definition)) {
        return;
    }
    if (definitionUsesScalar(definition)) {
        // nelson.mixin.Scalar: an empty array of a scalar class is not allowed.
        std::string src = "function obj = " + classNameLeaf(definition.name) + "_"
            + CLASSDEF_FUNCTION_EMPTY + "(varargin)\n";
        src += "error('Nelson:class:emptyScalar', 'Creating an empty array of a scalar "
               "class is not supported.');\n";
        src += "end\n";
        registerGeneratedFunction(
            definition.name + "." + CLASSDEF_FUNCTION_EMPTY, parseGeneratedFunction(src, filename));
        return;
    }
    const std::string builtin
        = definitionIsHandleClass(definition) ? CLASSDEF_HANDLE_BUILTIN : CLASSDEF_VALUE_BUILTIN;
    std::string source = "function obj = " + classNameLeaf(definition.name) + "_"
        + CLASSDEF_FUNCTION_EMPTY + "(varargin)\n";
    source += "if nargin == 0\n";
    source += "  obj = " + builtin + "('" + definition.name + "', [0 0]);\n";
    source += "elseif nargin == 1\n";
    source += "  obj = " + builtin + "('" + definition.name + "', varargin{1});\n";
    source += "else\n";
    source += "  obj = " + builtin + "('" + definition.name + "', [varargin{:}]);\n";
    source += "end\n";
    source += "end\n";
    MacroFunctionDef* macro = parseGeneratedFunction(source, filename);
    registerGeneratedFunction(definition.name + "." + CLASSDEF_FUNCTION_EMPTY, macro);
}
//=============================================================================
void
registerEnumerationMembers(const ClassdefDefinition& definition, const std::wstring& filename)
{
    size_t ordinal = 1;
    for (const auto& member : definition.enumerations) {
        // The member constructor is built natively for typed or argument-free
        // members, and for untyped members whose argument expressions parse.
        // Otherwise the forwarder source is generated and parsed.
        MacroFunctionDef* macro
            = buildEnumerationMemberFunction(definition, member, ordinal, filename);
        if (macro == nullptr) {
            auto argumentsIt = definition.enumerationArguments.find(member);
            const std::string arguments = argumentsIt == definition.enumerationArguments.end()
                ? std::string()
                : trim(argumentsIt->second);
            std::string source
                = "function obj = " + classNameLeaf(definition.name) + "_" + member + "()\n";
            source += "tmp = " + classNameLeaf(definition.name) + "(" + arguments + ");\n";
            source += "obj = " + std::string(CLASSDEF_VALUE_BUILTIN) + "('" + definition.name
                + "');\n";
            std::vector<ClassdefPropertyDefinition> properties
                = classdefPropertyDefinitions(definition.name);
            for (const auto& property : properties) {
                if (attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
                    || attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
                    || attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
                    continue;
                }
                source += "obj." + property.name + " = tmp." + property.name + ";\n";
            }
            source += "obj." + std::string(CLASSDEF_ENUM_PROPERTY_NAME) + " = '" + member + "';\n";
            source += "obj." + std::string(CLASSDEF_ENUM_PROPERTY_ORDINAL) + " = "
                + std::to_string(ordinal) + ";\n";
            source += "end\n";
            macro = parseGeneratedFunction(source, filename);
        }
        if (macro != nullptr) {
            // Member creation runs in class context so that enumeration
            // classes with a restricted constructor keep working.
            macro->classdefOwnerClassName = definition.name;
        }
        registerGeneratedFunction(definition.name + "." + member, macro);
        ++ordinal;
    }
}
//=============================================================================
void
registerEnumerationBaseConversion(
    const ClassdefDefinition& definition, const std::wstring& filename)
{
    const std::string baseType = enumerationBaseType(definition);
    if (baseType.empty()) {
        return;
    }

    std::string source = "function r = " + baseType + "(obj)\n";
    source += "r = builtin('" + baseType + "', zeros(size(obj)));\n";
    source += "for k = 1:numel(obj)\n";
    size_t ordinal = 1;
    for (const auto& member : definition.enumerations) {
        auto argumentsIt = definition.enumerationArguments.find(member);
        std::string value = argumentsIt == definition.enumerationArguments.end()
            ? std::string()
            : trim(argumentsIt->second);
        stringVector values = splitCommaList(value);
        value = values.empty() ? std::to_string(ordinal) : values.front();
        source += "if strcmp(obj(k)." + std::string(CLASSDEF_ENUM_PROPERTY_NAME) + ", '" + member
            + "')\n";
        source += "r(k) = builtin('" + baseType + "', " + value + ");\n";
        source += "end\n";
        ++ordinal;
    }
    source += "end\n";
    source += "end\n";
    registerGeneratedFunction(getOverloadFunctionName(definition.name, baseType),
        parseGeneratedFunction(source, filename));
}
//=============================================================================
void
registerEnumerationBaseSupport(const ClassdefDefinition& definition, const std::wstring& filename)
{
    // An enumeration deriving from a builtin type behaves like the value it
    // wraps. char/logical/eq are registered elsewhere; this adds double(),
    // string(), the type predicates, strcmp and isequal.
    const std::string baseType = enumerationBaseType(definition);
    if (baseType.empty()) {
        return;
    }
    const std::string& className = definition.name;
    auto reg = [&](const std::string& functionName, const std::string& source) {
        // Never override a method the user implemented with a body.
        for (const auto& method : definition.methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, functionName)
                && !method.source.empty()) {
                return;
            }
        }
        registerGeneratedFunction(getOverloadFunctionName(className, functionName),
            parseGeneratedFunction(source, filename));
    };

    // double(state) yields the underlying value (0/1 for a logical base). The
    // base conversion (logical/int.../double) is already registered.
    if (baseType != NLS_DOUBLE_STR) {
        reg("double", "function r = double(obj)\nr = double(" + baseType + "(obj));\nend\n");
    }
    // string(state) mirrors char(state): the displayed 'on'/'off' text.
    reg("string", "function r = string(obj)\nr = string(char(obj));\nend\n");

    // Type predicates follow the base type. Only register the ones that differ
    // from a plain class (which already answers false everywhere).
    const bool isNumericBase = baseType != NLS_LOGICAL_STR;
    const bool isFloatBase = baseType == NLS_DOUBLE_STR || baseType == NLS_SINGLE_STR;
    auto predicateTrue = [&](const std::string& functionName) {
        reg(functionName, "function r = " + functionName + "(obj)\nr = true;\nend\n");
    };
    if (baseType == NLS_LOGICAL_STR) {
        predicateTrue("islogical");
    }
    if (isNumericBase) {
        predicateTrue("isnumeric");
        predicateTrue(isFloatBase ? "isfloat" : "isinteger");
    }

    // strcmp/strcmpi compare the displayed text: unwrap only the class operands
    // to char, then defer to the builtin (which returns false against non-text).
    for (const std::string& functionName : { std::string("strcmp"), std::string("strcmpi") }) {
        std::string source = "function r = " + functionName + "(varargin)\n";
        source += "for k = 1:numel(varargin)\n";
        source
            += "if isa(varargin{k}, '" + className + "'); varargin{k} = char(varargin{k}); end\n";
        source += "end\n";
        source += "r = " + functionName + "(varargin{:});\n";
        source += "end\n";
        reg(functionName, source);
    }
    // Like eq, isequal/isequaln match the 'on'/'off' text when compared with
    // text, and the base value otherwise. A class operand is unwrapped to
    // char when any other operand is text, and to the base value otherwise.
    for (const std::string& functionName : { std::string("isequal"), std::string("isequaln") }) {
        std::string source = "function r = " + functionName + "(varargin)\n";
        source += "__useText = false;\n";
        source += "for k = 1:numel(varargin)\n";
        source += "if ~isa(varargin{k}, '" + className
            + "') && (ischar(varargin{k}) || isstring(varargin{k})); __useText = true; end\n";
        source += "end\n";
        source += "for k = 1:numel(varargin)\n";
        source += "if isa(varargin{k}, '" + className + "')\n";
        source += "if __useText; varargin{k} = char(varargin{k}); else; varargin{k} = " + baseType
            + "(varargin{k}); end\n";
        source += "end\n";
        source += "end\n";
        source += "r = " + functionName + "(varargin{:});\n";
        source += "end\n";
        reg(functionName, source);
    }
}
//=============================================================================
void
registerEnumerationOperators(const ClassdefDefinition& definition, const std::wstring& filename)
{
    if (definition.enumerations.empty()) {
        return;
    }

    // Comparing a member with a non-member is legal: text matches the member
    // name, and a base-typed enumeration compares the underlying values.
    // Reading the member property off a non-member would raise "Invalid
    // indexing".
    registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_EQ),
        buildEnumerationEqOperator(definition, filename));

    registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_NE),
        buildEnumerationNeOperator(definition, filename));

    registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_CHAR),
        buildEnumerationCharOperator(definition, filename));

    bool hasUserDisp = false;
    bool hasUserDisplay = false;
    for (const auto& method : definition.methods) {
        if (method.isStatic) {
            continue;
        }
        if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DISP)) {
            hasUserDisp = true;
        } else if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DISPLAY)) {
            hasUserDisplay = true;
        }
    }

    if (!hasUserDisp) {
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISP),
            buildEnumerationDispOperator(definition, filename));
    }

    if (!hasUserDisplay) {
        registerGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DISPLAY),
            buildEnumerationDisplayOperator(definition, filename));
    }
}
//=============================================================================
void
registerBuiltinBaseSupport(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const std::string baseType = builtinBaseTypeName(definition);
    if (baseType.empty()) {
        return;
    }
    const std::string baseProperty = CLASSDEF_BUILTIN_BASE_PROPERTY;
    const std::string className = definition.name;
    auto registerForwarder = [&](const std::string& functionName, const std::string& source) {
        for (const auto& method : definition.methods) {
            if (!method.isStatic && equalsIgnoreCase(method.name, functionName)
                && !method.source.empty()) {
                return;
            }
        }
        registerOwnedGeneratedFunction(
            definition, filename, getOverloadFunctionName(className, functionName), source);
    };
    auto unwrapLine = [&](const std::string& variable) {
        return "if isa(" + variable + ", '" + className + "'); " + variable + " = " + variable + "."
            + baseProperty + "; end\n";
    };
    const std::string rewrapPrefix
        = "__r = " + std::string(CLASSDEF_VALUE_BUILTIN) + "('" + className + "');\n";

    // binary operations return the subclass
    const stringVector wrappedBinary
        = { "plus", "minus", "times", "mtimes", "rdivide", "ldivide", "mrdivide", "mldivide",
              "power", "mpower", "mod", "rem", "horzcat", "vertcat", "min", "max" };
    for (const auto& op : wrappedBinary) {
        std::string source = "function r = " + op + "(varargin)\n";
        source += "for k = 1:nargin\n";
        source += "  if isa(varargin{k}, '" + className + "')\n";
        source += "    varargin{k} = varargin{k}." + baseProperty + ";\n";
        source += "  end\n";
        source += "end\n";
        source += rewrapPrefix;
        source += "__r." + baseProperty + " = " + op + "(varargin{:});\n";
        source += "r = __r;\n";
        source += "end\n";
        registerForwarder(op, source);
    }
    // comparisons and predicates return plain results
    const stringVector plainBinary
        = { "eq", "ne", "lt", "le", "gt", "ge", "and", "or", "xor", "strcmp", "strcmpi" };
    for (const auto& op : plainBinary) {
        std::string source = "function r = " + op + "(a, b)\n";
        source += unwrapLine("a");
        source += unwrapLine("b");
        source += "r = " + op + "(a, b);\n";
        source += "end\n";
        registerForwarder(op, source);
    }
    // unary operations return the subclass
    const stringVector wrappedUnary = { "uminus", "uplus", "transpose", "ctranspose", "abs", "sqrt",
        "round", "floor", "ceil", "fix", "real", "imag", "sum", "prod", "reshape", "permute" };
    for (const auto& op : wrappedUnary) {
        std::string source = "function r = " + op + "(obj, varargin)\n";
        source += rewrapPrefix;
        source += "__r." + baseProperty + " = " + op + "(obj." + baseProperty + ", varargin{:});\n";
        source += "r = __r;\n";
        source += "end\n";
        registerForwarder(op, source);
    }
    // shape and emptiness forward to the base data
    const stringVector plainUnary
        = { "length", "ndims", "numel", "isempty", "isscalar", "isvector", "ismatrix", "isrow",
              "iscolumn", "isreal", "isfinite", "isinf", "isnan", "full", "mean", "norm" };
    for (const auto& op : plainUnary) {
        std::string source = "function r = " + op + "(obj, varargin)\n";
        source += "r = " + op + "(obj." + baseProperty + ", varargin{:});\n";
        source += "end\n";
        registerForwarder(op, source);
    }
    {
        std::string source = "function varargout = size(obj, varargin)\n";
        source += "[varargout{1:max(1, nargout)}] = size(obj." + baseProperty + ", varargin{:});\n";
        source += "end\n";
        registerForwarder("size", source);
    }
    // conversions to the base type return plain data
    {
        std::string source = "function r = " + baseType + "(obj)\n";
        source += "r = obj." + baseProperty + ";\n";
        source += "end\n";
        registerForwarder(baseType, source);
    }
    if (baseType != NLS_DOUBLE_STR) {
        std::string source = "function r = double(obj)\n";
        source += "r = double(obj." + baseProperty + ");\n";
        source += "end\n";
        registerForwarder("double", source);
    }
    // type predicates follow the base type
    const bool isNumericBase = baseType != NLS_LOGICAL_STR && baseType != NLS_CHAR_STR;
    const bool isFloatBase = baseType == NLS_DOUBLE_STR || baseType == NLS_SINGLE_STR;
    const bool isIntegerBase = isNumericBase && !isFloatBase;
    auto registerPredicate = [&](const std::string& functionName, bool value) {
        std::string source = "function r = " + functionName + "(obj)\n";
        source += std::string("r = ") + (value ? "true" : "false") + ";\n";
        source += "end\n";
        registerForwarder(functionName, source);
    };
    registerPredicate("isnumeric", isNumericBase);
    registerPredicate("isfloat", isFloatBase);
    registerPredicate("isinteger", isIntegerBase);
    registerPredicate("islogical", baseType == NLS_LOGICAL_STR);
    registerPredicate("ischar", baseType == NLS_CHAR_STR);
    // simple value indexing keeps the subclass
    {
        std::string source = "function value = parenReference(obj, indices)\n";
        source += rewrapPrefix;
        source += "__r." + baseProperty + " = obj." + baseProperty + "(indices{:});\n";
        source += "value = __r;\n";
        source += "end\n";
        registerForwarder("parenReference", source);
    }
    {
        std::string source = "function obj = parenAssign(obj, indices, value)\n";
        source += unwrapLine("value");
        source += "obj." + baseProperty + "(indices{:}) = value;\n";
        source += "end\n";
        registerForwarder("parenAssign", source);
    }
    // display shows the underlying data
    {
        std::string source = "function disp(obj)\n";
        source += "disp(obj." + baseProperty + ");\n";
        source += "end\n";
        registerForwarder("disp", source);
    }
    {
        std::string source = "function display(obj, varargin)\n";
        source += "if nargin > 1 && ischar(varargin{1})\n";
        source += "  disp([varargin{1}, ' =']);\n";
        source += "end\n";
        source += "disp(obj." + baseProperty + ");\n";
        source += "end\n";
        registerForwarder("display", source);
    }
}
//=============================================================================
void
registerHandleSupport(const ClassdefDefinition& definition, const std::wstring& filename)
{
    if (!definitionIsHandleClass(definition)) {
        return;
    }

    // A user-defined or mixin-generated get/set must not be overwritten by
    // the simple handle accessors.
    if (!hasInstanceMethod(definition, CLASSDEF_FUNCTION_GET)
        && !definitionUsesSetGet(definition)) {
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_GET),
            buildForwardingFunction("value", CLASSDEF_FUNCTION_GET, { "obj", "name" },
                CLASSDEF_HANDLE_GET_BUILTIN, filename));
    }

    if (!hasInstanceMethod(definition, CLASSDEF_FUNCTION_SET)
        && !definitionUsesSetGet(definition)) {
        registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_SET),
            buildForwardingFunction("", CLASSDEF_FUNCTION_SET, { "obj", "name", "value" },
                CLASSDEF_HANDLE_SET_BUILTIN, filename));
    }

    registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_ISVALID),
        buildForwardingFunction(
            "r", CLASSDEF_FUNCTION_ISVALID, { "obj" }, CLASSDEF_HANDLE_ISVALID_BUILTIN, filename));

    registerGeneratedFunction(
        getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_FIELDNAMES),
        buildForwardingFunction(
            "names", CLASSDEF_FUNCTION_FIELDNAMES, { "obj" }, "properties", filename));

    registerGeneratedFunction(getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_STRUCT),
        buildForwardingFunction("value", CLASSDEF_FUNCTION_STRUCT, { "obj" },
            CLASSDEF_HANDLE_STRUCT_BUILTIN, filename));

    bool hasUserDelete = false;
    for (const auto& method : definition.methods) {
        if (equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)) {
            hasUserDelete = true;
            break;
        }
    }
    if (!hasUserDelete) {
        registerGeneratedFunction(
            getOverloadFunctionName(definition.name, CLASSDEF_FUNCTION_DELETE),
            buildHandleDeleteFunction(filename));
    }
}
//=============================================================================
bool
hasInstanceMethod(const ClassdefDefinition& definition, const std::string& methodName)
{
    for (const auto& method : definition.methods) {
        if (!method.isStatic && equalsIgnoreCase(method.name, methodName)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
void
registerOwnedGeneratedFunction(const ClassdefDefinition& definition, const std::wstring& filename,
    const std::string& key, const std::string& source)
{
    MacroFunctionDef* macro = parseGeneratedFunction(source, filename);
    if (macro != nullptr) {
        macro->classdefOwnerClassName = definition.name;
    }
    registerGeneratedFunction(key, macro);
}
//=============================================================================
static bool
definesStaticEmpty(const ClassdefDefinition& definition)
{
    for (const auto& method : definition.methods) {
        if (method.isStatic && method.name == CLASSDEF_FUNCTION_EMPTY) {
            return true;
        }
    }
    for (const auto& superclass : definition.superclasses) {
        const ClassdefDefinition* parent = findClassdefDefinition(superclass);
        if (parent != nullptr && definesStaticEmpty(*parent)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
