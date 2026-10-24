//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timerBuiltin.hpp"
#include <cmath>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleManager.hpp"
#include "TimerHelpers.hpp"
#include "TimerObject.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
struct TimerCriterion
{
    std::wstring propertyName;
    ArrayOf value;
};
//=============================================================================
static bool
textEqual(const ArrayOf& left, const ArrayOf& right)
{
    bool leftText = left.isRowVectorCharacterArray() || left.isScalarStringArray();
    bool rightText = right.isRowVectorCharacterArray() || right.isScalarStringArray();
    return leftText && rightText && left.asWideString() == right.asWideString();
}
//=============================================================================
static bool
numericEqual(const ArrayOf& left, const ArrayOf& right)
{
    if (!left.isNumeric() || !right.isNumeric()) {
        return false;
    }
    if (!left.dimensions().equals(right.dimensions())) {
        return false;
    }
    ArrayOf leftDouble(left);
    ArrayOf rightDouble(right);
    leftDouble.promoteType(NLS_DOUBLE);
    rightDouble.promoteType(NLS_DOUBLE);
    const auto* leftData = static_cast<const double*>(leftDouble.dataPointer());
    const auto* rightData = static_cast<const double*>(rightDouble.dataPointer());
    for (indexType k = 0; k < leftDouble.elementCount(); ++k) {
        bool sameValue = leftData[k] == rightData[k];
        bool bothNaN = std::isnan(leftData[k]) && std::isnan(rightData[k]);
        if (!sameValue && !bothNaN) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
handleEqual(const ArrayOf& left, const ArrayOf& right)
{
    if (!left.isHandle() || !right.isHandle()) {
        return false;
    }
    if (!left.dimensions().equals(right.dimensions())) {
        return false;
    }
    const auto* leftData = static_cast<const HandleRef*>(left.dataPointer());
    const auto* rightData = static_cast<const HandleRef*>(right.dataPointer());
    for (indexType k = 0; k < left.elementCount(); ++k) {
        if (!leftData[k] || !rightData[k] || leftData[k]->id() != rightData[k]->id()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
valuesEqual(const ArrayOf& left, const ArrayOf& right)
{
    if (left.isEmpty() || right.isEmpty()) {
        return left.isEmpty() && right.isEmpty();
    }
    if (textEqual(left, right)) {
        return true;
    }
    if (numericEqual(left, right)) {
        return true;
    }
    if (handleEqual(left, right)) {
        return true;
    }
    return left.dataClass() == right.dataClass() && left.dimensions().equals(right.dimensions())
        && left.dataPointer() == right.dataPointer();
}
//=============================================================================
static std::vector<TimerCriterion>
criteriaFromStruct(const ArrayOf& value)
{
    if (!value.isStruct() || !value.isScalar()) {
        Error(L"Nelson:time:scalarStructureExpected", L"%s expected.", L"Scalar structure");
    }
    std::vector<TimerCriterion> criteria;
    for (const std::string& fieldName : value.fieldNames()) {
        criteria.push_back({ utf8_to_wstring(fieldName), value.field(fieldName) });
    }
    return criteria;
}
//=============================================================================
static void
appendPairCriteria(
    std::vector<TimerCriterion>& criteria, const ArrayOfVector& argIn, size_t firstIndex)
{
    if (((argIn.size() - firstIndex) % 2) != 0) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    for (size_t k = firstIndex; k < argIn.size(); k += 2) {
        criteria.push_back({ argIn[k].asWideString(), argIn[k + 1] });
    }
}
//=============================================================================
static bool
matchesCriteria(TimerObject* object, const std::vector<TimerCriterion>& criteria)
{
    for (const TimerCriterion& criterion : criteria) {
        ArrayOf actual;
        if (!object->get(criterion.propertyName, actual)) {
            Error(L"Nelson:timer:unknownProperty", L"Unknown %s property: %s",
                std::wstring(L"timer"), criterion.propertyName);
        }
        if (!valuesEqual(actual, criterion.value)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static std::vector<HandleRef>
allTimerRefs()
{
    return HandleManager::getInstance()->getAllHandleRefsOfCategory(NLS_HANDLE_TIMER_CATEGORY_STR);
}
//=============================================================================
static bool
isVisibleTimer(TimerObject* object)
{
    return object != nullptr && object->getObjectVisibility() == L"on";
}
//=============================================================================
static std::vector<HandleRef>
filterTimerRefs(const std::vector<HandleRef>& refs, const std::vector<TimerCriterion>& criteria,
    bool includeHidden)
{
    std::vector<HandleRef> result;
    for (const HandleRef& ref : refs) {
        if (!ref || !ref->isValid()) {
            continue;
        }
        auto* object = dynamic_cast<TimerObject*>(ref->object());
        if (object == nullptr || (!includeHidden && !isVisibleTimer(object))) {
            continue;
        }
        if (matchesCriteria(object, criteria)) {
            result.push_back(ref);
        }
    }
    return result;
}
//=============================================================================
static ArrayOfVector
timerFindCommon(const ArrayOfVector& argIn, bool includeHidden)
{
    std::vector<HandleRef> sourceRefs = allTimerRefs();
    std::vector<TimerCriterion> criteria;
    size_t firstPair = 0;
    if (!argIn.empty() && argIn[0].isHandle()) {
        sourceRefs = getTimerHandleRefs(argIn[0]);
        firstPair = 1;
    } else if (!argIn.empty() && argIn[0].isStruct()) {
        criteria = criteriaFromStruct(argIn[0]);
        firstPair = 1;
    }
    appendPairCriteria(criteria, argIn, firstPair);
    std::vector<HandleRef> result = filterTimerRefs(sourceRefs, criteria, includeHidden);
    return ArrayOfVector { timerHandleRefsToArray(result) };
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timerfindBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    return timerFindCommon(argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timerfindallBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    return timerFindCommon(argIn, true);
}
//=============================================================================
