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
callDependentSetter(
    Evaluator* eval, const ArrayOf& object, const std::string& propertyName, const ArrayOf& value)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        Error(_("Cannot set property: ") + propertyName);
    }
    std::string functionName;
    auto* manager = ClassdefDefinitionManager::getInstance();
    if (!manager->resolvePropertyAccessorFunction(
            object.getClassType(), propertyName, "set", functionName)) {
        Error(_("Cannot set property: ") + propertyName);
    }
    FunctionDef* functionDef = nullptr;
    if (!eval->getContext()->lookupFunction(functionName, functionDef) || functionDef == nullptr) {
        Error(_("Cannot set property: ") + propertyName);
    }
    ArrayOfVector inputs;
    inputs << object;
    inputs << value;
    int nLhs = functionDef->outputArgCount() == 0 ? 0 : 1;
    ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, nLhs);
    return result.empty() ? object : result[0];
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
        Error(_W("Wrong type for argument #1. class expected."));
    }
    if (!argIn[1].isStruct()) {
        Error(_W("Wrong type for argument #2. struct expected."));
    }
    stringVector fieldnames = argIn[1].getFieldNames();
    bool isSupportedStruct = (fieldnames.size() == 2)
        && ((fieldnames[0] == "type" && fieldnames[1] == "subs")
            || (fieldnames[0] == "subs" && fieldnames[1] == "type"));
    if (!isSupportedStruct) {
        Error(_("Second argument must be a structure with two fields whose names are 'type' and "
                "'subs'."));
    }

    ArrayOfVector typeFields = argIn[1].getFieldAsList("type");
    ArrayOfVector subsFields = argIn[1].getFieldAsList("subs");

    if (typeFields.empty()) {
        Error(_("Illegal indexing structure argument: type '.' expected."));
    }
    std::string typeFieldStr = typeFields[0].getContentAsCString();
    if (typeFieldStr != ".") {
        Error(_("Illegal indexing structure argument: type '.' expected."));
    }
    std::string fieldname = subsFields[0].getContentAsCString();
    auto* manager = ClassdefDefinitionManager::getInstance();
    const std::string className = argIn[0].getClassType();
    const bool isClassdef = manager->loadClass(className);
    stringVector existingFields = argIn[0].getFieldNames();
    const bool propertyAlreadyExists
        = std::find(existingFields.begin(), existingFields.end(), fieldname)
        != existingFields.end();
    if (isClassdef && !manager->hasProperty(className, fieldname)) {
        Error(_("No such property: ") + fieldname);
    }
    if (isClassdef && manager->hasConstantProperty(className, fieldname)) {
        if (propertyAlreadyExists) {
            Error("Cannot modify constant property: " + fieldname);
        }
    }
    if (isClassdef
        && !manager->canSetProperty(className, fieldname,
            eval == nullptr ? std::string() : eval->getClassdefAccessContext(),
            manager->hasDependentProperty(className, fieldname) ? true : propertyAlreadyExists)) {
        Error(_("Cannot set property: ") + fieldname);
    }
    if (isClassdef && manager->hasDependentProperty(className, fieldname)) {
        retval << callDependentSetter(eval, argIn[0], fieldname, argIn[2]);
        return retval;
    }
    ArrayOf res = argIn[0];
    ArrayOfVector value;
    value << argIn[2];
    res.setFieldAsList(fieldname, value);
    retval << res;
    return retval;
}
//=============================================================================
