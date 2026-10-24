//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "metaPackageBuiltin.hpp"
#include <algorithm>
#include <set>
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define META_PACKAGE_CLASS_NAME "meta.package"
#define META_PACKAGE_FIELD_NAME "Name"
#define META_PACKAGE_FIELD_FUNCTION_LIST "FunctionList"
#define META_PACKAGE_FIELD_CLASS_LIST "ClassList"
#define META_PACKAGE_FIELD_PACKAGE_LIST "PackageList"
#define META_PACKAGE_FIELD_CONTAINING_PACKAGE "ContainingPackage"
//=============================================================================
static stringVector
metaPackageFieldNames();
static ArrayOf
buildEmptyMetaPackage();
static ArrayOf
buildMetaPackageObject(const std::string& packageName, bool& packageExists);
static stringVector
listTopLevelPackages();
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::metaPackageFromNameBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!(argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    const std::string packageName = argIn[0].asUtf8String();
    bool packageExists = false;
    ArrayOf result = buildMetaPackageObject(packageName, packageExists);
    if (!packageExists) {
        retval << buildEmptyMetaPackage();
    } else {
        retval << result;
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::metaPackageGetAllPackagesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    const stringVector packages = listTopLevelPackages();
    if (packages.empty()) {
        retval << buildEmptyMetaPackage();
        return retval;
    }
    std::vector<ArrayOf> elements;
    elements.reserve(packages.size());
    for (const auto& name : packages) {
        bool packageExists = false;
        ArrayOf element = buildMetaPackageObject(name, packageExists);
        if (packageExists) {
            elements.push_back(element);
        }
    }
    if (elements.empty()) {
        retval << buildEmptyMetaPackage();
        return retval;
    }
    stringVector fieldnames = metaPackageFieldNames();
    Dimensions dims(static_cast<indexType>(elements.size()), 1);
    void* data
        = ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, dims.elementCount(), fieldnames, true);
    ArrayOf packageArray(NLS_CLASSDEF_ARRAY, dims, data, false, fieldnames);
    packageArray.setClassType(META_PACKAGE_CLASS_NAME);
    for (size_t k = 0; k < elements.size(); ++k) {
        elements[k].copyElements(0, data, static_cast<indexType>(k), 1);
    }
    retval << packageArray;
    return retval;
}
//=============================================================================
static stringVector
metaPackageFieldNames()
{
    return { META_PACKAGE_FIELD_NAME, META_PACKAGE_FIELD_FUNCTION_LIST,
        META_PACKAGE_FIELD_CLASS_LIST, META_PACKAGE_FIELD_PACKAGE_LIST,
        META_PACKAGE_FIELD_CONTAINING_PACKAGE };
}
//=============================================================================
static ArrayOf
buildEmptyMetaPackage()
{
    stringVector fieldnames = metaPackageFieldNames();
    Dimensions dims(0, 0);
    return ArrayOf::emptyClassdef(META_PACKAGE_CLASS_NAME, fieldnames, dims);
}
//=============================================================================
static ArrayOf
buildMetaPackageObject(const std::string& packageName, bool& packageExists)
{
    packageExists = false;
    if (packageName.empty()) {
        return buildEmptyMetaPackage();
    }
    const std::string prefix = packageName + ".";
    PathFunctionIndexerManager* manager = PathFunctionIndexerManager::getInstance();
    const wstringVector macros = manager->getMacrosList(false);

    stringVector functionList;
    stringVector classList;
    std::set<std::string> packageSet;

    for (const auto& macroW : macros) {
        const std::string macro = wstring_to_utf8(macroW);
        if (macro.size() <= prefix.size() || macro.compare(0, prefix.size(), prefix) != 0) {
            continue;
        }
        const std::string rest = macro.substr(prefix.size());
        if (rest.empty()) {
            continue;
        }
        const size_t dotPosition = rest.find('.');
        if (dotPosition != std::string::npos) {
            // A deeper member (subpackage or class inside a subpackage): only the
            // immediate subpackage name is recorded in PackageList.
            packageSet.insert(packageName + "." + rest.substr(0, dotPosition));
            continue;
        }
        // Direct member: the resolved file tells a class from a function.
        // The class is not loaded.
        std::wstring filename;
        bool isClassdef = false;
        if (manager->find(macro, filename)) {
            isClassdef = classdefIsClassdefFile(filename);
        }
        if (isClassdef) {
            classList.push_back(macro);
        } else {
            functionList.push_back(macro);
        }
    }

    if (functionList.empty() && classList.empty() && packageSet.empty()) {
        return buildEmptyMetaPackage();
    }

    std::sort(functionList.begin(), functionList.end());
    std::sort(classList.begin(), classList.end());
    stringVector packageList(packageSet.begin(), packageSet.end());

    std::string containingPackage;
    const size_t lastDot = packageName.find_last_of('.');
    if (lastDot != std::string::npos) {
        containingPackage = packageName.substr(0, lastDot);
    }

    stringVector fieldnames = metaPackageFieldNames();
    ArrayOfVector values;
    values << ArrayOf::characterArray(packageName);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(functionList);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(classList);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(packageList);
    values << ArrayOf::characterArray(containingPackage);
    packageExists = true;
    return ArrayOf::classdefArray(META_PACKAGE_CLASS_NAME, fieldnames, values);
}
//=============================================================================
static stringVector
listTopLevelPackages()
{
    PathFunctionIndexerManager* manager = PathFunctionIndexerManager::getInstance();
    const wstringVector macros = manager->getMacrosList(false);
    std::set<std::string> packageSet;
    for (const auto& macroW : macros) {
        const std::string macro = wstring_to_utf8(macroW);
        if (macro.empty() || macro.front() == '@') {
            continue;
        }
        const size_t dotPosition = macro.find('.');
        if (dotPosition == std::string::npos || dotPosition == 0) {
            continue;
        }
        packageSet.insert(macro.substr(0, dotPosition));
    }
    return stringVector(packageSet.begin(), packageSet.end());
}
//=============================================================================
