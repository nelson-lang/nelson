//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "class_subsagnBuiltin.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
ArrayOf
callPropertySetter(
    Evaluator* eval, const ArrayOf& object, const std::string& propertyName, const ArrayOf& value)
{
    struct PropertySetContextGuard
    {
        Evaluator* eval = nullptr;
        bool active = false;

        PropertySetContextGuard(
            Evaluator* evaluator, const std::string& className, const std::string& propertyName)
            : eval(evaluator)
        {
            if (eval == nullptr) {
                return;
            }
            ClassdefPropertyDefinition property;
            if (!classdefPropertyDefinition(className, propertyName, property)) {
                return;
            }
            const std::string setterClassName
                = property.definingClassName.empty() ? className : property.definingClassName;
            eval->pushClassdefPropertySetContext(setterClassName, property.name);
            active = true;
        }

        ~PropertySetContextGuard()
        {
            if (active) {
                eval->popClassdefPropertySetContext();
            }
        }

        PropertySetContextGuard(const PropertySetContextGuard&) = delete;
        PropertySetContextGuard&
        operator=(const PropertySetContextGuard&)
            = delete;
    };

    if (eval == nullptr || eval->getContext() == nullptr) {
        Error("Nelson:subsasgn:cannotSetProperty", "Cannot set property: %s", propertyName);
    }
    std::string functionName;

    if (!classdefResolvePropertyAccessorFunction(
            object.classType(), propertyName, "set", functionName)) {
        Error("Nelson:subsasgn:cannotSetProperty", "Cannot set property: %s", propertyName);
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        Error("Nelson:subsasgn:cannotSetProperty", "Cannot set property: %s", propertyName);
    }
    ArrayOfVector inputs;
    inputs << object;
    inputs << value;
    int nLhs = functionDef->outputArgCount() == 0 ? 0 : 1;
    PropertySetContextGuard guard(eval, object.classType(), propertyName);
    ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, nLhs);
    return result.empty() ? object : result[0];
}
//=============================================================================
bool
storedPropertyHasValue(const ArrayOf& object, const std::string& propertyName)
{
    ArrayOfVector values = object.fieldAsList(propertyName);
    for (const auto& value : values) {
        if (!value.isEmpty()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::class_subsagnBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 3, 3);
    if (!argIn[0].isClassType()) {
        Error(L"Nelson:data_structures:wrongTypeArgument1Class",
            L"Wrong type for argument #%d: %s expected.", 1, L"class");
    }
    if (!argIn[1].isStruct()) {
        Error(L"Nelson:data_structures:wrongTypeArgument2Struct",
            L"Wrong type for argument #%d: %s expected.", 2, L"struct");
    }
    stringVector fieldnames = argIn[1].fieldNames();
    bool isSupportedStruct = (fieldnames.size() == 2)
        && ((fieldnames[0] == "type" && fieldnames[1] == "subs")
            || (fieldnames[0] == "subs" && fieldnames[1] == "type"));
    if (!isSupportedStruct) {
        Error("Nelson:subsasgn:invalidSubsStruct",
            "Second argument must be a structure with two fields whose names are 'type' and "
            "'subs'.");
    }

    ArrayOfVector typeFields = argIn[1].fieldAsList("type");
    ArrayOfVector subsFields = argIn[1].fieldAsList("subs");

    if (typeFields.empty()) {
        Error("Nelson:data_structures:illegalIndexingStructureArgumentType",
            "Illegal indexing structure argument: type '.' expected.");
    }
    std::string typeFieldStr = typeFields[0].asUtf8String();
    if (typeFieldStr != ".") {
        Error("Nelson:data_structures:illegalIndexingStructureArgumentType",
            "Illegal indexing structure argument: type '.' expected.");
    }
    std::string fieldname = subsFields[0].asUtf8String();

    const std::string className = argIn[0].classType();
    const bool isClassdef = classdefLoadClass(className);
    stringVector existingFields = argIn[0].fieldNames();
    const bool propertyAlreadyExists
        = std::find(existingFields.begin(), existingFields.end(), fieldname)
        != existingFields.end();
    const bool propertyAlreadyInitialized
        = propertyAlreadyExists && storedPropertyHasValue(argIn[0], fieldname);
    if (isClassdef && !classdefHasProperty(className, fieldname)) {
        Error("Nelson:subsasgn:noSuchProperty", "No such property: %s", fieldname);
    }
    if (isClassdef && classdefHasConstantProperty(className, fieldname)) {
        RaiseError("Cannot modify constant property: " + fieldname,
            "Nelson:interpreter:cannotModifyConstantProperty");
    }
    if (isClassdef
        && !classdefCanSetProperty(className, fieldname,
            eval == nullptr ? std::string() : eval->getClassdefAccessContext(),
            classdefHasDependentProperty(className, fieldname) ? true : propertyAlreadyInitialized,
            eval)) {
        Error("Nelson:subsasgn:cannotSetProperty", "Cannot set property: %s", fieldname);
    }
    ArrayOf assignedValue = argIn[2];
    ClassdefPropertyDefinition property;
    bool hasPropertyDefinition = false;
    if (isClassdef) {
        hasPropertyDefinition = classdefPropertyDefinition(className, fieldname, property);
        if (hasPropertyDefinition) {
            assignedValue = classdefValidatePropertyValue(eval, property, assignedValue);
        }
    }
    if (isClassdef) {
        std::string setterFunctionName;
        bool hasSetter = classdefResolvePropertyAccessorFunction(
            className, fieldname, "set", setterFunctionName);
        bool isSameStoredPropertySetter = false;
        if (hasSetter && hasPropertyDefinition
            && !classdefHasDependentProperty(className, fieldname) && eval != nullptr) {
            const std::string setterClassName
                = property.definingClassName.empty() ? className : property.definingClassName;
            isSameStoredPropertySetter
                = eval->isCurrentClassdefPropertySetContext(setterClassName, property.name);
        }
        if (hasSetter && !isSameStoredPropertySetter) {
            retval << callPropertySetter(eval, argIn[0], fieldname, assignedValue);
            return retval;
        }
    }
    if (isClassdef && classdefHasDependentProperty(className, fieldname)) {
        retval << callPropertySetter(eval, argIn[0], fieldname, assignedValue);
        return retval;
    }
    ArrayOf res = argIn[0];
    ArrayOfVector value;
    value << assignedValue;
    res.setFieldAsList(fieldname, value);
    retval << res;
    return retval;
}
//=============================================================================
