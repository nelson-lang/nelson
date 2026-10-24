//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FindObj.hpp"
#include <cmath>
#include <limits>
#include <memory>
#include <regex>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOColorHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "GraphicsObject.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class NodeKind
{
    All,
    PropertyValue,
    HasProperty,
    Regexp,
    Not,
    And,
    Or,
    Xor
};
//=============================================================================
struct FindObjNode
{
    NodeKind kind = NodeKind::All;
    std::wstring propertyName;
    ArrayOf value;
    std::shared_ptr<FindObjNode> left;
    std::shared_ptr<FindObjNode> right;
};
//=============================================================================
static bool
isStringScalar(const ArrayOf& arg);
//=============================================================================
static std::wstring
getStringScalar(const ArrayOf& arg);
//=============================================================================
static bool
isRootHandle(const ArrayOf& arg);
//=============================================================================
static bool
isOperator(const std::wstring& value);
//=============================================================================
static bool
isPredicateStart(const ArrayOf& arg);
//=============================================================================
static std::wstring
resolvePropertyName(GraphicsObject* fp, const std::wstring& propertyName, bool raiseError);
//=============================================================================
static std::vector<ArrayOf>
cellElements(const ArrayOf& arg);
//=============================================================================
static std::shared_ptr<FindObjNode>
parsePrimary(const std::vector<ArrayOf>& args, size_t& index);
//=============================================================================
static std::shared_ptr<FindObjNode>
parseAnd(const std::vector<ArrayOf>& args, size_t& index);
//=============================================================================
static std::shared_ptr<FindObjNode>
parseExpression(const std::vector<ArrayOf>& args, size_t& index);
//=============================================================================
static bool
sameDimensions(const ArrayOf& lhs, const ArrayOf& rhs);
//=============================================================================
static bool
numericEquals(ArrayOf lhs, ArrayOf rhs);
//=============================================================================
static bool
logicalEquals(const ArrayOf& lhs, const ArrayOf& rhs);
//=============================================================================
static bool
graphicsHandleEquals(const ArrayOf& lhs, const ArrayOf& rhs);
//=============================================================================
static bool
stringEquals(const ArrayOf& lhs, const ArrayOf& rhs);
//=============================================================================
static bool
isColorPropertyName(const std::wstring& propertyName);
//=============================================================================
static bool
colorEquals(const std::wstring& propertyName, const ArrayOf& lhs, const ArrayOf& rhs);
//=============================================================================
static bool
propertyValueEquals(const std::wstring& propertyName, const ArrayOf& lhs, const ArrayOf& rhs);
//=============================================================================
static bool
regexpMatches(const ArrayOf& propertyValue, const ArrayOf& expression);
//=============================================================================
static bool
evaluateNode(GraphicsObject* fp, const std::shared_ptr<FindObjNode>& node);
//=============================================================================
static bool
rootShowsHiddenHandles();
//=============================================================================
static void
collectFindObj(int64 handle, int depth, const std::shared_ptr<FindObjNode>& expression,
    bool includeHiddenHandles, bool isExplicitRoot, std::vector<int64>& matches);
//=============================================================================
static ArrayOf
handlesToGraphicsArray(const std::vector<int64>& handles);
//=============================================================================
ArrayOf
FindObj(const ArrayOfVector& argIn, bool includeHiddenHandles)
{
    size_t index = 0;
    std::vector<int64> roots;
    if (!argIn.empty() && argIn[0].isGraphicsObject()) {
        const nelson_handle* handles = static_cast<const nelson_handle*>(argIn[0].dataPointer());
        for (indexType k = 0; k < argIn[0].elementCount(); ++k) {
            graphicsObjectFromHandle(handles[k], true);
            roots.push_back(handles[k]);
        }
        index = 1;
    } else if (!argIn.empty() && isRootHandle(argIn[0])) {
        roots.push_back(HANDLE_ROOT_OBJECT);
        index = 1;
    } else {
        roots.push_back(HANDLE_ROOT_OBJECT);
    }

    int depth = -1;
    if (index < argIn.size() && isStringScalar(argIn[index])) {
        std::wstring option = getStringScalar(argIn[index]);
        if (StringHelpers::iequals(option, L"flat")) {
            depth = 0;
            ++index;
        } else if (StringHelpers::iequals(option, L"-depth")) {
            if (index + 1 >= argIn.size()) {
                Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
            }
            double depthValue = argIn[index + 1].asDoubleScalar();
            if (std::isinf(depthValue)) {
                depth = -1;
            } else if (depthValue < 0 || std::floor(depthValue) != depthValue) {
                Error(
                    L"Nelson:findobj:invalidDepth", L"Depth must be a nonnegative integer or Inf.");
            } else {
                depth = static_cast<int>(depthValue);
            }
            index += 2;
        }
    }

    std::shared_ptr<FindObjNode> expression = std::make_shared<FindObjNode>();
    if (index < argIn.size()) {
        std::vector<ArrayOf> expressionArgs;
        expressionArgs.reserve(argIn.size() - index);
        for (size_t k = index; k < argIn.size(); ++k) {
            expressionArgs.push_back(argIn[k]);
        }
        size_t expressionIndex = 0;
        expression = parseExpression(expressionArgs, expressionIndex);
        if (expressionIndex != expressionArgs.size()) {
            Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
        }
    }

    includeHiddenHandles = includeHiddenHandles || rootShowsHiddenHandles();

    std::vector<int64> matches;
    for (int64 root : roots) {
        collectFindObj(root, depth, expression, includeHiddenHandles, true, matches);
    }

    return handlesToGraphicsArray(matches);
}
//=============================================================================
bool
isStringScalar(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || arg.isScalarStringArray();
}
//=============================================================================
std::wstring
getStringScalar(const ArrayOf& arg)
{
    if (!isStringScalar(arg)) {
        Error(L"Nelson:findobj:invalidInput", L"Expected a character vector or scalar string.");
    }
    return arg.asWideString();
}
//=============================================================================
bool
isRootHandle(const ArrayOf& arg)
{
    if (!arg.isNumeric() || !arg.isScalar()) {
        return false;
    }
    try {
        return arg.asDoubleScalar() == 0;
    } catch (const NelsonException&) {
        return false;
    }
}
//=============================================================================
bool
isOperator(const std::wstring& value)
{
    return StringHelpers::iequals(value, L"-and") || StringHelpers::iequals(value, L"-or")
        || StringHelpers::iequals(value, L"-xor") || StringHelpers::iequals(value, L"-not");
}
//=============================================================================
bool
isPredicateStart(const ArrayOf& arg)
{
    if (arg.isCell()) {
        return true;
    }
    if (!isStringScalar(arg)) {
        return false;
    }
    std::wstring value = getStringScalar(arg);
    return !StringHelpers::iequals(value, L"-and") && !StringHelpers::iequals(value, L"-or")
        && !StringHelpers::iequals(value, L"-xor");
}
//=============================================================================
std::wstring
resolvePropertyName(GraphicsObject* fp, const std::wstring& propertyName, bool raiseError)
{
    GOGenericProperty* hp = fp->findProperty(propertyName, false);
    if (hp) {
        return propertyName;
    }
    wstringVector fieldnames = fp->fieldnames();
    for (const auto& name : fieldnames) {
        if (StringHelpers::iequals(name, propertyName)) {
            return name;
        }
    }
    if (raiseError) {
        fp->findProperty(propertyName, true);
    }
    return propertyName;
}
//=============================================================================
std::vector<ArrayOf>
cellElements(const ArrayOf& arg)
{
    if (!arg.isCell()) {
        return {};
    }
    std::vector<ArrayOf> result;
    result.reserve(arg.elementCount());
    ArrayOf* elements = const_cast<ArrayOf*>(static_cast<const ArrayOf*>(arg.dataPointer()));
    for (indexType k = 0; k < arg.elementCount(); ++k) {
        result.push_back(elements[k]);
    }
    return result;
}
//=============================================================================
std::shared_ptr<FindObjNode>
parsePrimary(const std::vector<ArrayOf>& args, size_t& index)
{
    if (index >= args.size()) {
        Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
    }
    const ArrayOf& current = args[index];
    if (current.isCell()) {
        std::vector<ArrayOf> nestedArgs = cellElements(current);
        if (nestedArgs.empty()) {
            Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
        }
        size_t nestedIndex = 0;
        std::shared_ptr<FindObjNode> nested = parseExpression(nestedArgs, nestedIndex);
        if (nestedIndex != nestedArgs.size()) {
            Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
        }
        ++index;
        return nested;
    }
    std::wstring token = getStringScalar(current);
    if (StringHelpers::iequals(token, L"-not")) {
        ++index;
        auto node = std::make_shared<FindObjNode>();
        node->kind = NodeKind::Not;
        node->left = parsePrimary(args, index);
        return node;
    }
    if (StringHelpers::iequals(token, L"-property")) {
        if (index + 1 >= args.size()) {
            Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
        }
        auto node = std::make_shared<FindObjNode>();
        node->kind = NodeKind::HasProperty;
        node->propertyName = getStringScalar(args[index + 1]);
        index += 2;
        return node;
    }
    if (StringHelpers::iequals(token, L"-regexp")) {
        if (index + 2 >= args.size()) {
            Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
        }
        auto node = std::make_shared<FindObjNode>();
        node->kind = NodeKind::Regexp;
        node->propertyName = getStringScalar(args[index + 1]);
        node->value = args[index + 2];
        index += 3;
        return node;
    }
    if (isOperator(token)) {
        Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
    }
    if (index + 1 >= args.size()) {
        Error(L"Nelson:findobj:invalidExpression", L"Invalid findobj expression.");
    }
    auto node = std::make_shared<FindObjNode>();
    node->kind = NodeKind::PropertyValue;
    node->propertyName = token;
    node->value = args[index + 1];
    index += 2;
    return node;
}
//=============================================================================
std::shared_ptr<FindObjNode>
parseAnd(const std::vector<ArrayOf>& args, size_t& index)
{
    std::shared_ptr<FindObjNode> node = parsePrimary(args, index);
    while (index < args.size()) {
        if (isStringScalar(args[index])) {
            std::wstring token = getStringScalar(args[index]);
            if (StringHelpers::iequals(token, L"-or") || StringHelpers::iequals(token, L"-xor")) {
                break;
            }
            if (StringHelpers::iequals(token, L"-and")) {
                ++index;
            }
        }
        if (index >= args.size() || !isPredicateStart(args[index])) {
            break;
        }
        auto parent = std::make_shared<FindObjNode>();
        parent->kind = NodeKind::And;
        parent->left = node;
        parent->right = parsePrimary(args, index);
        node = parent;
    }
    return node;
}
//=============================================================================
std::shared_ptr<FindObjNode>
parseExpression(const std::vector<ArrayOf>& args, size_t& index)
{
    std::shared_ptr<FindObjNode> node = parseAnd(args, index);
    while (index < args.size() && isStringScalar(args[index])) {
        std::wstring token = getStringScalar(args[index]);
        NodeKind kind;
        if (StringHelpers::iequals(token, L"-or")) {
            kind = NodeKind::Or;
        } else if (StringHelpers::iequals(token, L"-xor")) {
            kind = NodeKind::Xor;
        } else {
            break;
        }
        ++index;
        auto parent = std::make_shared<FindObjNode>();
        parent->kind = kind;
        parent->left = node;
        parent->right = parseAnd(args, index);
        node = parent;
    }
    return node;
}
//=============================================================================
bool
sameDimensions(const ArrayOf& lhs, const ArrayOf& rhs)
{
    return lhs.dimensions().equals(rhs.dimensions());
}
//=============================================================================
bool
numericEquals(ArrayOf lhs, ArrayOf rhs)
{
    if (!lhs.isNumeric() || !rhs.isNumeric() || !sameDimensions(lhs, rhs)) {
        return false;
    }
    try {
        lhs.promoteType(NLS_DOUBLE);
        rhs.promoteType(NLS_DOUBLE);
    } catch (const NelsonException&) {
        return false;
    }
    const double* lp = static_cast<const double*>(lhs.dataPointer());
    const double* rp = static_cast<const double*>(rhs.dataPointer());
    for (indexType k = 0; k < lhs.elementCount(); ++k) {
        if (lp[k] != rp[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
logicalEquals(const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (lhs.dataClass() != NLS_LOGICAL || rhs.dataClass() != NLS_LOGICAL
        || !sameDimensions(lhs, rhs)) {
        return false;
    }
    const logical* lp = static_cast<const logical*>(lhs.dataPointer());
    const logical* rp = static_cast<const logical*>(rhs.dataPointer());
    for (indexType k = 0; k < lhs.elementCount(); ++k) {
        if (lp[k] != rp[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
graphicsHandleEquals(const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (!lhs.isGraphicsObject() || !rhs.isGraphicsObject() || !sameDimensions(lhs, rhs)) {
        return false;
    }
    const nelson_handle* lp = static_cast<const nelson_handle*>(lhs.dataPointer());
    const nelson_handle* rp = static_cast<const nelson_handle*>(rhs.dataPointer());
    for (indexType k = 0; k < lhs.elementCount(); ++k) {
        if (lp[k] != rp[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
stringEquals(const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (!isStringScalar(lhs) || !isStringScalar(rhs)) {
        return false;
    }
    return lhs.asWideString() == rhs.asWideString();
}
//=============================================================================
bool
isColorPropertyName(const std::wstring& propertyName)
{
    return StringHelpers::iequals(propertyName, L"Color")
        || StringHelpers::iends_with(propertyName, L"Color");
}
//=============================================================================
bool
colorEquals(const std::wstring& propertyName, const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (!isColorPropertyName(propertyName)) {
        return false;
    }
    std::vector<double> lhsColor;
    std::vector<double> rhsColor;
    if (!ParseColorToRGB(lhs, lhsColor) || !ParseColorToRGB(rhs, rhsColor)
        || lhsColor.size() != rhsColor.size()) {
        return false;
    }
    for (size_t k = 0; k < lhsColor.size(); ++k) {
        if (std::abs(lhsColor[k] - rhsColor[k]) > std::numeric_limits<double>::epsilon() * 16) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
propertyValueEquals(const std::wstring& propertyName, const ArrayOf& lhs, const ArrayOf& rhs)
{
    return colorEquals(propertyName, lhs, rhs) || stringEquals(lhs, rhs) || numericEquals(lhs, rhs)
        || logicalEquals(lhs, rhs) || graphicsHandleEquals(lhs, rhs);
}
//=============================================================================
bool
regexpMatches(const ArrayOf& propertyValue, const ArrayOf& expression)
{
    if (!isStringScalar(propertyValue)) {
        return false;
    }
    std::vector<std::wstring> expressions;
    if (isStringScalar(expression)) {
        expressions.push_back(expression.asWideString());
    } else if (expression.isStringArray() || expression.isCellArrayOfCharacterVectors()) {
        expressions = expression.asWideStringVector();
    } else {
        Error(L"Nelson:findobj:invalidRegularExpression",
            L"Regular expression must be a character vector, string array, or cell array.");
    }
    std::wstring value = propertyValue.asWideString();
    for (const auto& expr : expressions) {
        try {
            std::wregex pattern(expr);
            if (std::regex_search(value, pattern)) {
                return true;
            }
        } catch (const std::regex_error&) {
            Error(L"Nelson:findobj:regexpCompileFailed", L"Invalid regular expression.");
        }
    }
    return false;
}
//=============================================================================
bool
evaluateNode(GraphicsObject* fp, const std::shared_ptr<FindObjNode>& node)
{
    if (!node) {
        return true;
    }
    switch (node->kind) {
    case NodeKind::All:
        return true;
    case NodeKind::PropertyValue: {
        std::wstring propertyName = resolvePropertyName(fp, node->propertyName, false);
        GOGenericProperty* hp = fp->findProperty(propertyName, false);
        if (!hp) {
            return false;
        }
        return propertyValueEquals(propertyName, hp->get(), node->value);
    }
    case NodeKind::HasProperty:
        return fp->findProperty(resolvePropertyName(fp, node->propertyName, false), false)
            != nullptr;
    case NodeKind::Regexp: {
        std::wstring propertyName = resolvePropertyName(fp, node->propertyName, false);
        GOGenericProperty* hp = fp->findProperty(propertyName, false);
        if (!hp) {
            return false;
        }
        return regexpMatches(hp->get(), node->value);
    }
    case NodeKind::Not:
        return !evaluateNode(fp, node->left);
    case NodeKind::And:
        return evaluateNode(fp, node->left) && evaluateNode(fp, node->right);
    case NodeKind::Or:
        return evaluateNode(fp, node->left) || evaluateNode(fp, node->right);
    case NodeKind::Xor:
        return evaluateNode(fp, node->left) != evaluateNode(fp, node->right);
    }
    return false;
}
//=============================================================================
bool
isHandleVisible(GraphicsObject* fp)
{
    GOGenericProperty* hp = fp->findProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, false);
    if (!hp) {
        return true;
    }
    return StringHelpers::iequals(hp->get().asWideString(), GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
bool
rootShowsHiddenHandles()
{
    GraphicsObject* root = graphicsObjectFromHandle(HANDLE_ROOT_OBJECT, false);
    if (!root) {
        return false;
    }
    GOGenericProperty* hp = root->findProperty(L"ShowHiddenHandles", false);
    if (!hp) {
        return false;
    }
    return StringHelpers::iequals(hp->get().asWideString(), GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
void
collectFindObj(int64 handle, int depth, const std::shared_ptr<FindObjNode>& expression,
    bool includeHiddenHandles, bool isExplicitRoot, std::vector<int64>& matches)
{
    GraphicsObject* fp = graphicsObjectFromHandle(handle, false);
    if (!fp) {
        return;
    }
    // A handle passed explicitly to findobj is always searched, regardless of
    // its own HandleVisibility. A hidden descendant (and its subtree) is skipped
    // unless hidden handles are requested (findall / ShowHiddenHandles).
    bool visible = includeHiddenHandles || isExplicitRoot || isHandleVisible(fp);
    if (!visible) {
        return;
    }
    if (evaluateNode(fp, expression)) {
        matches.push_back(handle);
    }
    if (depth == 0) {
        return;
    }
    // Descends in the order the children are reported, so a search lists them the
    // same way the Children property does.
    std::vector<int64> childHandles = reportedChildren(fp);
    if (childHandles.empty()) {
        return;
    }
    int nextDepth = depth < 0 ? depth : depth - 1;
    for (int64 child : childHandles) {
        collectFindObj(child, nextDepth, expression, includeHiddenHandles, false, matches);
    }
}
//=============================================================================
ArrayOf
handlesToGraphicsArray(const std::vector<int64>& handles)
{
    return graphicsObjectVector(handles);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
