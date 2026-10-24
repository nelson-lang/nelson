//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "gpuDeviceBuiltin.hpp"
#include "GpuContext.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "OverloadName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::string
overloadNameToAscii(const std::wstring& name, const std::wstring& prefix);
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayFunctionsBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    // The list is derived from the actually registered @gpuArray/<op> overloads
    // (both the module builtins and the .m method overloads), so it stays
    // correct as functions are added.
    const std::string prefix = getOverloadFunctionName("gpuArray", "");
    stringVector names;
    stringVector builtinNames = BuiltInFunctionDefManager::getInstance()->getNameList();
    for (const std::string& name : builtinNames) {
        if (name.size() > prefix.size() && name.compare(0, prefix.size(), prefix) == 0) {
            names.push_back(name.substr(prefix.size()));
        }
    }
    // .m method overloads live in functions/@gpuArray; their names are ASCII
    const std::wstring wprefix(prefix.begin(), prefix.end());
    wstringVector macroNames = PathFunctionIndexerManager::getInstance()->getMacrosList(false);
    for (const std::wstring& name : macroNames) {
        std::string plain = overloadNameToAscii(name, wprefix);
        if (!plain.empty()) {
            names.push_back(plain);
        }
    }
    std::sort(names.begin(), names.end());
    names.erase(std::unique(names.begin(), names.end()), names.end());
    ArrayOfVector retval;
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(names);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuDeviceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (argIn.size() == 1) {
        indexType index = argIn[0].asScalarIndex(false);
        if (index != 1) {
            Error(L"Nelson:gpuDevice:invalidDeviceIndex", L"Invalid GPU device index.");
        }
    }
    stringVector fieldnames
        = { "Index", "Name", "Vendor", "Architecture", "Backend", "MaxBufferSize" };
    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::doubleScalar(1);
    fieldvalues << ArrayOf::characterArray(context->deviceName());
    fieldvalues << ArrayOf::characterArray(context->vendorName());
    fieldvalues << ArrayOf::characterArray(context->architectureName());
    fieldvalues << ArrayOf::characterArray(context->backendName());
    fieldvalues << ArrayOf::doubleScalar((double)context->maxBufferSize());
    ArrayOfVector retval;
    retval << ArrayOf::structScalar(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuDeviceCountBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar((double)GpuContext::getInstance()->deviceCount());
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::canUseGPUBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(GpuContext::getInstance()->isAvailable());
    return retval;
}
//=============================================================================
// static helper functions
//=============================================================================
static std::string
overloadNameToAscii(const std::wstring& name, const std::wstring& prefix)
{
    // an @gpuArray method name: strip the prefix; overload names are ASCII
    if (name.size() <= prefix.size() || name.compare(0, prefix.size(), prefix) != 0) {
        return {};
    }
    std::string plain;
    plain.reserve(name.size() - prefix.size());
    for (size_t k = prefix.size(); k < name.size(); ++k) {
        plain.push_back(static_cast<char>(name[k]));
    }
    return plain;
}
//=============================================================================
