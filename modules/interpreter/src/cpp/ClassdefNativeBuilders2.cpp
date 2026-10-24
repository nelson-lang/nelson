//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserGenerator.hpp"
#include "ClassdefNativeBuilders.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserConstants.hpp"
#include "ClassdefParserAttributes.hpp"
#include "ClassdefParserScanner.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "AbstractSyntaxTree.hpp"
#include "Keywords.hpp"
#include "ClassName.hpp"
#include <algorithm>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson::classdef_detail {
//=============================================================================
MacroFunctionDef*
buildDefaultConstructor(const ClassdefDefinition& definition, const std::wstring& filename)
{
    // Only the simple case is native: no class-local functions (the generated
    // path would append them to the body) and every stored property has an
    // evaluation-invariant default (no stored-default registry needed), and
    // no ancestor constructor has to be called implicitly.
    if (!definition.localFunctionsSource.empty()
        || !implicitSuperclassConstructors(definition).empty()) {
        return nullptr;
    }
    const int context = 1;
    std::vector<ClassdefPropertyDefinition> properties
        = classdefPropertyDefinitions(definition.name);
    struct PropertyInitialization
    {
        std::string name;
        std::string expression;
        bool setStored;
    };
    std::vector<PropertyInitialization> initializations;
    for (const auto& property : properties) {
        if (attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
            || attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
            || attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
            continue;
        }
        const std::string expression = propertyDefaultExpression(property);
        if (!isEvaluationInvariantDefaultExpression(expression)) {
            return nullptr; // stored-default registry needed: fall back
        }
        initializations.push_back(
            { property.name, expression, canUseStoredDefaultAssignment(definition, property) });
    }
    AbstractSyntaxTreePtrVector owned;
    std::vector<AbstractSyntaxTreePtr> valueExpressions;
    for (const auto& initialization : initializations) {
        AbstractSyntaxTreePtr value = parseExpressionInto(initialization.expression, owned);
        if (value == nullptr) {
            AbstractSyntaxTree::deleteReferences(owned);
            return nullptr;
        }
        valueExpressions.push_back(value);
    }
    AbstractSyntaxTree::clearReferences();
    std::vector<AbstractSyntaxTreePtr> statements;
    // obj = __classdef_value__('Class'); (or __classdef_handle__ for a handle)
    const char* baseBuiltin
        = definitionIsHandleClass(definition) ? CLASSDEF_HANDLE_BUILTIN : CLASSDEF_VALUE_BUILTIN;
    statements.push_back(astQuietStatement(
        astAssignment(astIdentifier("obj", context),
            astReferenceExpression(
                astCall(baseBuiltin, { astCharacterArray(definition.name, context) }, context),
                context),
            context),
        context));
    const bool isHandleClass = definitionIsHandleClass(definition);
    for (size_t k = 0; k < initializations.size(); ++k) {
        if (initializations[k].setStored && isHandleClass) {
            // __classdef_handle_set_stored__(obj, 'P', <value>);
            statements.push_back(astQuietStatement(
                astReferenceExpression(
                    astCall(CLASSDEF_HANDLE_SET_STORED_BUILTIN,
                        { astReferenceExpression(astIdentifier("obj", context), context),
                            astCharacterArray(initializations[k].name, context),
                            valueExpressions[k] },
                        context),
                    context),
                context));
        } else if (initializations[k].setStored) {
            // obj = __classdef_value_set_stored__(obj, 'P', <value>);
            statements.push_back(astQuietStatement(
                astAssignment(astIdentifier("obj", context),
                    astReferenceExpression(
                        astCall(CLASSDEF_VALUE_SET_STORED_BUILTIN,
                            { astReferenceExpression(astIdentifier("obj", context), context),
                                astCharacterArray(initializations[k].name, context),
                                valueExpressions[k] },
                            context),
                        context),
                    context),
                context));
        } else {
            statements.push_back(astQuietStatement(
                astAssignment(astDotReference("obj", initializations[k].name, context),
                    valueExpressions[k], context),
                context));
        }
    }
    auto* macro = new MacroFunctionDef();
    macro->returnVals = { "obj" };
    macro->setName(classNameLeaf(definition.name));
    macro->code = astStatementBlock(statements, context);
    macro->overloadAutoMode = NLS_OVERLOAD_AUTO_OFF;
    AbstractSyntaxTreePtrVector handBuilt = AbstractSyntaxTree::getReferences();
    owned.insert(owned.end(), handBuilt.begin(), handBuilt.end());
    macro->ptrAstCodeAsVector = owned;
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildEnumerationDispOperator(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const int context = 1;
    AbstractSyntaxTree::clearReferences();
    auto reference = [&](const std::string& name) {
        return astReferenceExpression(astIdentifier(name, context), context);
    };
    auto call
        = [&](const std::string& function, const std::vector<AbstractSyntaxTreePtr>& arguments) {
              return astReferenceExpression(astCall(function, arguments, context), context);
          };
    // if numel(obj) == 1; fprintf('\n  %s enumeration\n\n    %s\n\n', class(obj), char(obj));
    AbstractSyntaxTreePtr scalarCondition = astBinaryOperation(
        OP_EQ, call("numel", { reference("obj") }), astDoubleLiteral("1", context), context);
    AbstractSyntaxTreePtr scalarBody = astStatementBlock(
        { astQuietStatement(
            call("fprintf",
                { astCharacterArray("\\n  %s enumeration\\n\\n    %s\\n\\n", context),
                    call("class", { reference("obj") }), call("char", { reference("obj") }) }),
            context) },
        context);
    // else for k = 1:numel(obj); fprintf('    %s', char(obj(k))); end; fprintf('\n\n');
    AbstractSyntaxTreePtr loopBody = astStatementBlock(
        { astQuietStatement(call("fprintf",
                                { astCharacterArray("    %s", context),
                                    call("char", { call("obj", { reference("k") }) }) }),
            context) },
        context);
    AbstractSyntaxTreePtr range = astBinaryOperation(
        OP_COLON, astDoubleLiteral("1", context), call("numel", { reference("obj") }), context);
    AbstractSyntaxTreePtr arrayBody = astStatementBlock(
        { astQuietStatement(astForLoop("k", range, loopBody, context), context),
            astQuietStatement(call("fprintf", { astCharacterArray("\\n\\n", context) }), context) },
        context);
    auto* macro = new MacroFunctionDef();
    macro->arguments = { "obj" };
    macro->setName(CLASSDEF_FUNCTION_DISP);
    macro->code = astStatementBlock(
        { astQuietStatement(astIfElse(scalarCondition, scalarBody, arrayBody, context), context) },
        context);
    macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildEnumerationDisplayOperator(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const int context = 1;
    AbstractSyntaxTree::clearReferences();
    auto reference = [&](const std::string& name) {
        return astReferenceExpression(astIdentifier(name, context), context);
    };
    auto call
        = [&](const std::string& function, const std::vector<AbstractSyntaxTreePtr>& arguments) {
              return astReferenceExpression(astCall(function, arguments, context), context);
          };
    auto firstVararg = [&]() {
        return astReferenceExpression(
            astBraceIndex("varargin", astDoubleLiteral("1", context), context), context);
    };
    // if nargin > 1 && ischar(varargin{1}); fprintf('\n%s =\n', varargin{1}); end
    AbstractSyntaxTreePtr condition = astBinaryOperation(OP_SAND,
        astBinaryOperation(OP_GT, reference("nargin"), astDoubleLiteral("1", context), context),
        call("ischar", { firstVararg() }), context);
    AbstractSyntaxTreePtr headerBody = astStatementBlock(
        { astQuietStatement(
            call("fprintf", { astCharacterArray("\\n%s =\\n", context), firstVararg() }),
            context) },
        context);
    auto* macro = new MacroFunctionDef();
    macro->arguments = { "obj", "varargin" };
    macro->setName(CLASSDEF_FUNCTION_DISPLAY);
    macro->code = astStatementBlock(
        { astQuietStatement(astIfElse(condition, headerBody, nullptr, context), context),
            astQuietStatement(call("disp", { reference("obj") }), context) },
        context);
    macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildHandleDeleteFunction(const std::wstring& filename)
{
    // function delete(obj); __classdef_handle_delete__(obj); end
    // The per-element protocol (ObjectBeingDestroyed, the user delete chain
    // from the most derived class upward, invalidation) lives in
    // RunClassdefDelete so that an explicit delete(obj) and the implicit
    // last-reference finalization share one implementation.
    return buildForwardingFunction(std::string(), CLASSDEF_FUNCTION_DELETE, { "obj" },
        CLASSDEF_HANDLE_DELETE_BUILTIN, filename);
}
//=============================================================================
MacroFunctionDef*
buildForwardingFunction(const std::string& returnValue, const std::string& functionName,
    const std::vector<std::string>& argumentNames, const std::string& callee,
    const std::wstring& filename)
{
    const int context = 1;
    AbstractSyntaxTree::clearReferences();
    std::vector<AbstractSyntaxTreePtr> callArguments;
    callArguments.reserve(argumentNames.size());
    for (const auto& argument : argumentNames) {
        callArguments.push_back(astReferenceExpression(astIdentifier(argument, context), context));
    }
    AbstractSyntaxTreePtr callExpression
        = astReferenceExpression(astCall(callee, callArguments, context), context);
    AbstractSyntaxTreePtr statement = returnValue.empty()
        ? astQuietStatement(callExpression, context)
        : astQuietStatement(
              astAssignment(astIdentifier(returnValue, context), callExpression, context), context);
    auto* macro = new MacroFunctionDef();
    if (!returnValue.empty()) {
        macro->returnVals = { returnValue };
    }
    macro->arguments = argumentNames;
    macro->setName(functionName);
    macro->code = astStatementBlock({ statement }, context);
    macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildEnumerationEqOperator(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const int context = 1;
    const std::string& className = definition.name;
    const std::string baseType = enumerationBaseType(definition);
    AbstractSyntaxTree::clearReferences();
    auto reference = [&](const std::string& name) {
        return astReferenceExpression(astIdentifier(name, context), context);
    };
    auto call
        = [&](const std::string& function, const std::vector<AbstractSyntaxTreePtr>& arguments) {
              return astReferenceExpression(astCall(function, arguments, context), context);
          };
    auto assign = [&](const std::string& name, AbstractSyntaxTreePtr value) {
        return astStatementBlock(
            { astQuietStatement(
                astAssignment(astIdentifier(name, context), value, context), context) },
            context);
    };
    // if isa(a, 'Class') && isa(b, 'Class'); r = strcmp(a.Name, b.Name);
    AbstractSyntaxTreePtr sameClass = astBinaryOperation(OP_SAND,
        call("isa", { reference("a"), astCharacterArray(className, context) }),
        call("isa", { reference("b"), astCharacterArray(className, context) }), context);
    AbstractSyntaxTreePtr byName = assign("r",
        call("strcmp",
            { astReferenceExpression(
                  astDotReference("a", CLASSDEF_ENUM_PROPERTY_NAME, context), context),
                astReferenceExpression(
                    astDotReference("b", CLASSDEF_ENUM_PROPERTY_NAME, context), context) }));
    // elseif ischar(a) || isstring(a) || ischar(b) || isstring(b);
    //   r = strcmp(char(a), char(b));
    AbstractSyntaxTreePtr anyText = astBinaryOperation(OP_SOR,
        astBinaryOperation(OP_SOR,
            astBinaryOperation(OP_SOR, call("ischar", { reference("a") }),
                call("isstring", { reference("a") }), context),
            call("ischar", { reference("b") }), context),
        call("isstring", { reference("b") }), context);
    AbstractSyntaxTreePtr byText = assign("r",
        call("strcmp", { call("char", { reference("a") }), call("char", { reference("b") }) }));
    // else: r = false; (untyped) or r = base(a) == base(b); (base-typed)
    AbstractSyntaxTreePtr fallbackValue = baseType.empty()
        ? astIdentifier("false", context)
        : astBinaryOperation(OP_EQ, call(baseType, { reference("a") }),
              call(baseType, { reference("b") }), context);
    AbstractSyntaxTreePtr byValue = assign("r", fallbackValue);
    AbstractSyntaxTreePtr innerIf = astStatementBlock(
        { astQuietStatement(astIfElse(anyText, byText, byValue, context), context) }, context);
    AbstractSyntaxTreePtr outerIf
        = astQuietStatement(astIfElse(sameClass, byName, innerIf, context), context);
    auto* macro = new MacroFunctionDef();
    macro->returnVals = { "r" };
    macro->arguments = { "a", "b" };
    macro->setName(CLASSDEF_FUNCTION_EQ);
    macro->code = astStatementBlock({ outerIf }, context);
    macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildEnumerationCharOperator(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const int context = 1;
    AbstractSyntaxTree::clearReferences();
    // r = obj.Name;
    AbstractSyntaxTreePtr statement = astQuietStatement(
        astAssignment(astIdentifier("r", context),
            astReferenceExpression(
                astDotReference("obj", CLASSDEF_ENUM_PROPERTY_NAME, context), context),
            context),
        context);
    auto* macro = new MacroFunctionDef();
    macro->returnVals = { "r" };
    macro->arguments = { "obj" };
    macro->setName(CLASSDEF_FUNCTION_CHAR);
    macro->code = astStatementBlock({ statement }, context);
    macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildEnumerationNeOperator(const ClassdefDefinition& definition, const std::wstring& filename)
{
    const int context = 1;
    AbstractSyntaxTree::clearReferences();
    // r = ~eq(a, b);
    AbstractSyntaxTreePtr eqCall = astReferenceExpression(
        astCall(CLASSDEF_FUNCTION_EQ,
            { astReferenceExpression(astIdentifier("a", context), context),
                astReferenceExpression(astIdentifier("b", context), context) },
            context),
        context);
    AbstractSyntaxTreePtr statement = astQuietStatement(
        astAssignment(astIdentifier("r", context), astUnaryNot(eqCall, context), context), context);
    auto* macro = new MacroFunctionDef();
    macro->returnVals = { "r" };
    macro->arguments = { "a", "b" };
    macro->setName(CLASSDEF_FUNCTION_NE);
    macro->code = astStatementBlock({ statement }, context);
    macro->ptrAstCodeAsVector = AbstractSyntaxTree::getReferences();
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
static bool
classHasOwnConstructor(const ClassdefDefinition& definition)
{
    for (const auto& method : definition.methods) {
        if (equalsClassNameOrLeaf(method.name, definition.name)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::string
enumerationConstructorClass(const ClassdefDefinition& definition)
{
    if (classHasOwnConstructor(definition)) {
        return definition.name;
    }
    // No own constructor: forward to the nearest superclass that defines one.
    // Constructors are excluded from classdefMethodDefinitions, so query each
    // ancestor's parsed definition directly (this also loads the ancestor).
    std::vector<std::string> pending = classdefSuperclasses(definition.name);
    std::vector<std::string> visited;
    while (!pending.empty()) {
        const std::string ancestor = trim(pending.front());
        pending.erase(pending.begin());
        if (ancestor.empty()
            || std::find(visited.begin(), visited.end(), ancestor) != visited.end()) {
            continue;
        }
        visited.push_back(ancestor);
        if (!classdefLoadClass(ancestor)) {
            continue;
        }
        const ClassdefDefinition* ancestorDefinition = findClassdefDefinition(ancestor);
        if (ancestorDefinition == nullptr) {
            continue;
        }
        if (classHasOwnConstructor(*ancestorDefinition)) {
            return ancestor;
        }
        for (const auto& super : ancestorDefinition->superclasses) {
            pending.push_back(super);
        }
    }
    return definition.name;
}
//=============================================================================
MacroFunctionDef*
buildEnumerationMemberFunction(const ClassdefDefinition& definition, const std::string& member,
    size_t ordinal, const std::wstring& filename)
{
    // Synthetic single-line context (line 1, column 0): these statements have
    // no user source, and the member constructor has no error path of its own.
    const int context = 1;
    // An untyped enumeration whose member declares constructor arguments has to
    // forward them to the class constructor and copy the storage properties;
    // the argument expressions are user-written, so they are parsed (not
    // generated) and their nodes are owned alongside the hand-built ones.
    const bool typedEnumeration = !enumerationBaseType(definition).empty();
    std::string arguments;
    auto argumentsIt = definition.enumerationArguments.find(member);
    if (argumentsIt != definition.enumerationArguments.end()) {
        arguments = trim(argumentsIt->second);
    }
    const bool forwardConstructor = !typedEnumeration && !arguments.empty();

    AbstractSyntaxTreePtrVector owned;
    std::vector<AbstractSyntaxTreePtr> constructorArguments;
    std::vector<std::string> copiedProperties;
    if (forwardConstructor) {
        for (const auto& argument : splitCommaList(arguments)) {
            AbstractSyntaxTreePtr argumentNode = parseExpressionInto(trim(argument), owned);
            if (argumentNode == nullptr) {
                AbstractSyntaxTree::deleteReferences(owned);
                return nullptr; // fall back to the generated path
            }
            constructorArguments.push_back(argumentNode);
        }
        for (const auto& property : classdefPropertyDefinitions(definition.name)) {
            if (attributeListContains(property.attributes, CLASSDEF_ATTR_ABSTRACT)
                || attributeListContains(property.attributes, CLASSDEF_ATTR_CONSTANT)
                || attributeListContains(property.attributes, CLASSDEF_ATTR_DEPENDENT)) {
                continue;
            }
            // Name and Ordinal are enumeration-synthesized and assigned
            // explicitly below; they are not carried by the constructor's
            // class (which may be a superclass), so never copy them from tmp.
            if (property.name == CLASSDEF_ENUM_PROPERTY_NAME
                || property.name == CLASSDEF_ENUM_PROPERTY_ORDINAL) {
                continue;
            }
            copiedProperties.push_back(property.name);
        }
    }

    AbstractSyntaxTree::clearReferences();
    std::vector<AbstractSyntaxTreePtr> statements;
    if (forwardConstructor) {
        // tmp = ConstructorClass(arguments);
        statements.push_back(astQuietStatement(
            astAssignment(astIdentifier("tmp", context),
                astReferenceExpression(
                    astCall(classNameLeaf(enumerationConstructorClass(definition)),
                        constructorArguments, context),
                    context),
                context),
            context));
    }
    // obj = __classdef_value__('Class');
    statements.push_back(astQuietStatement(
        astAssignment(astIdentifier("obj", context),
            astReferenceExpression(astCall(CLASSDEF_VALUE_BUILTIN,
                                       { astCharacterArray(definition.name, context) }, context),
                context),
            context),
        context));
    for (const auto& property : copiedProperties) {
        // obj.property = tmp.property;
        statements.push_back(astQuietStatement(
            astAssignment(astDotReference("obj", property, context),
                astReferenceExpression(astDotReference("tmp", property, context), context),
                context),
            context));
    }
    // obj.Name = 'member';
    statements.push_back(astQuietStatement(
        astAssignment(astDotReference("obj", CLASSDEF_ENUM_PROPERTY_NAME, context),
            astCharacterArray(member, context), context),
        context));
    statements.push_back(astQuietStatement(
        astAssignment(astDotReference("obj", CLASSDEF_ENUM_PROPERTY_ORDINAL, context),
            astDoubleLiteral(std::to_string(ordinal), context), context),
        context));
    AbstractSyntaxTreePtr body = astStatementBlock(statements, context);
    auto* macro = new MacroFunctionDef();
    macro->returnVals = { "obj" };
    macro->setName(classNameLeaf(definition.name) + "_" + member);
    macro->code = body;
    AbstractSyntaxTreePtrVector handBuilt = AbstractSyntaxTree::getReferences();
    owned.insert(owned.end(), handBuilt.begin(), handBuilt.end());
    macro->ptrAstCodeAsVector = owned;
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
MacroFunctionDef*
buildInvariantConstantPropertyGetter(const ClassdefDefinition& definition,
    const ClassdefPropertyDefinition& property, const std::wstring& filename)
{
    const int context = 1;
    const std::string defaultExpression = property.defaultExpression.empty()
        ? std::string(CLASSDEF_DEFAULT_PROPERTY_VALUE)
        : property.defaultExpression;
    AbstractSyntaxTreePtrVector owned;
    AbstractSyntaxTreePtr valueExpression = parseExpressionInto(defaultExpression, owned);
    if (valueExpression == nullptr) {
        AbstractSyntaxTree::deleteReferences(owned);
        return nullptr; // fall back to the generated path
    }
    AbstractSyntaxTree::clearReferences();
    // value = <expr>;
    AbstractSyntaxTreePtr statement = astQuietStatement(
        astAssignment(astIdentifier("value", context), valueExpression, context), context);
    AbstractSyntaxTreePtr body = astStatementBlock({ statement }, context);
    auto* macro = new MacroFunctionDef();
    macro->returnVals = { "value" };
    macro->setName(classNameLeaf(definition.name) + "_" + property.name);
    macro->code = body;
    AbstractSyntaxTreePtrVector handBuilt = AbstractSyntaxTree::getReferences();
    owned.insert(owned.end(), handBuilt.begin(), handBuilt.end());
    macro->ptrAstCodeAsVector = owned;
    macro->setFilename(filename);
    AbstractSyntaxTree::clearReferences();
    return macro;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
