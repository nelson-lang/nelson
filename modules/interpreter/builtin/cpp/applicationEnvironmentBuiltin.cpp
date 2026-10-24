//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "applicationEnvironmentBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "Error.hpp"
#include <mutex>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
ArrayOfVector
deploymentRoot(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].asWideString() != L"deployment") {
        RaiseError("Unknown application environment operation.", "Nelson:nelsonc:environment");
    }
    static std::wstring root;
    static std::mutex mutex;
    const std::lock_guard<std::mutex> lock(mutex);
    const auto previous = root;
    if (argIn.size() == 2) {
        root = argIn[1].asWideString();
    }
    ArrayOfVector result;
    if (nLhs > 0) {
        result << ArrayOf::characterArray(previous);
    }
    return result;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::applicationEnvironmentBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    if (argIn[0].isCharacterArray() || argIn[0].isStringArray()) {
        return deploymentRoot(nLhs, argIn);
    }
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    const bool currentDirectory = argIn[0].asLogicalScalar();
    const auto userPath = argIn[1].asWideString();
    auto* manager = PathFunctionIndexerManager::getInstance();
    const auto previousUserPath = manager->getUserPath();
    const bool previous = manager->setCurrentDirectorySearchEnabled(currentDirectory);
    if (userPath.empty()) {
        manager->clearUserPath(false);
    } else {
        manager->setUserPath(userPath, false);
    }
    manager->clearCache(eval->getCallers(true));
    ArrayOfVector result;
    if (nLhs > 0) {
        result << ArrayOf::logicalScalar(previous);
    }
    if (nLhs > 1) {
        result << ArrayOf::characterArray(previousUserPath);
    }
    return result;
}
//=============================================================================
