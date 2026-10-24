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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "TimerHelpers.hpp"
#include "TimerObject.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
propertyNameFromArray(const ArrayOf& value)
{
    return value.asWideString();
}
//=============================================================================
static void
applyNameValuePairs(TimerObject* object, const ArrayOfVector& argIn, size_t firstIndex)
{
    if (((argIn.size() - firstIndex) % 2) != 0) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    for (size_t k = firstIndex; k < argIn.size(); k += 2) {
        std::wstring propertyName = propertyNameFromArray(argIn[k]);
        std::wstring errorMessage;
        if (!object->set(propertyName, argIn[k + 1], errorMessage)) {
            Error(L"Nelson:timer:setPropertyFailed", errorMessage);
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    auto* object = new TimerObject();
    try {
        applyNameValuePairs(object, argIn, 0);
    } catch (const NelsonException&) {
        delete object;
        throw;
    }
    ArrayOf timerHandle = ArrayOf::handle(object);
    object->retainUntilDeleted();
    retval << timerHandle;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<TimerObject*> objects = getTimerObjects(argIn[0]);
    if (argIn.size() == 1) {
        if (objects.empty()) {
            Dimensions dims(0, 0);
            wstringVector fields;
            return ArrayOfVector { ArrayOf::emptyStruct(fields, dims) };
        }
        if (objects.size() != 1) {
            Error(
                L"Nelson:time:scalarTimerHandleExpected", L"%s expected.", L"Scalar timer handle");
        }
        return ArrayOfVector { objects[0]->toStruct() };
    }
    if (objects.size() != 1) {
        Error(L"Nelson:time:scalarTimerHandleExpected", L"%s expected.", L"Scalar timer handle");
    }
    std::wstring propertyName = propertyNameFromArray(argIn[1]);
    ArrayOf res;
    if (!objects[0]->get(propertyName, res)) {
        Error(L"Nelson:timer:invalidPropertyValue", L"Invalid property: %s.", propertyName);
    }
    return ArrayOfVector { res };
}
//=============================================================================
static void
setFromStruct(TimerObject* object, const ArrayOf& settings)
{
    if (!settings.isStruct() || !settings.isScalar()) {
        Error(L"Nelson:time:scalarStructureExpected", L"%s expected.", L"Scalar structure");
    }
    stringVector fieldNames = settings.fieldNames();
    for (const std::string& fieldName : fieldNames) {
        std::wstring propertyName = utf8_to_wstring(fieldName);
        if (!object->isWritableProperty(propertyName)) {
            continue;
        }
        ArrayOf value = settings.field(fieldName);
        std::wstring errorMessage;
        if (!object->set(propertyName, value, errorMessage)) {
            Error(L"Nelson:timer:setPropertyFailed", errorMessage);
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 0);
    std::vector<TimerObject*> objects = getTimerObjects(argIn[0]);
    if (argIn.size() == 2) {
        for (TimerObject* object : objects) {
            setFromStruct(object, argIn[1]);
        }
        return {};
    }
    if (((argIn.size() - 1) % 2) != 0) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    for (TimerObject* object : objects) {
        applyNameValuePairs(object, argIn, 1);
    }
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isHandle() || argIn[0].handleCategory() != NLS_HANDLE_TIMER_CATEGORY_STR) {
        Error(L"Nelson:time:timerHandleExpected", L"%s expected.", L"timer handle");
    }
    std::vector<HandleRef> refs = getTimerHandleRefs(argIn[0]);
    for (const HandleRef& ref : refs) {
        if (!ref || !ref->isValid()) {
            continue;
        }
        auto* object = dynamic_cast<TimerObject*>(ref->object());
        if (object != nullptr) {
            object->stop(true);
            object->releaseRetainedReference();
            ref->invalidateAs<TimerObject>(NLS_HANDLE_TIMER_CATEGORY_STR,
                [](TimerObject* timerObject) { delete timerObject; });
        }
    }
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_isvalidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isHandle()) {
        Error(L"Nelson:time:timerHandleExpected", L"%s expected.", L"timer handle");
    }
    Dimensions dims = argIn[0].dimensions();
    auto* data = static_cast<const HandleRef*>(argIn[0].dataPointer());
    auto* result = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, dims.elementCount(), stringVector(), false));
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        result[k]
            = data[k] && data[k]->isValid() && data[k]->category() == NLS_HANDLE_TIMER_CATEGORY_STR;
    }
    return ArrayOfVector { ArrayOf(NLS_LOGICAL, dims, result) };
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::vector<TimerObject*> objects = getTimerObjects(argIn[0]);
    if (objects.empty()) {
        return ArrayOfVector { ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
            wstringVector {}) };
    }
    return ArrayOfVector { ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
        objects[0]->properties()) };
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    return timer_fieldnamesBuiltin(nLhs, argIn);
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_methodsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::vector<TimerObject*> objects = getTimerObjects(argIn[0]);
    if (objects.empty()) {
        return ArrayOfVector { ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
            wstringVector {}) };
    }
    return ArrayOfVector { ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
        objects[0]->methods()) };
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_ispropBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    getTimerObjects(argIn[0]);
    return ArrayOfVector { ArrayOf::logicalScalar(
        argIn[0].isHandleProperty(propertyNameFromArray(argIn[1]))) };
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_ismethodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    getTimerObjects(argIn[0]);
    return ArrayOfVector { ArrayOf::logicalScalar(
        argIn[0].isHandleMethod(propertyNameFromArray(argIn[1]))) };
}
//=============================================================================
