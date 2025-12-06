//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "OnCleanupObjectHandle.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
OnCleanupObjectHandle::OnCleanupObjectHandle(ArrayOf functionAsArrayOf)
    : HandleGenericObject(NLS_HANDLE_ONCLEANUP_CATEGORY_STR, this, true)
{
    this->methodNames = { L"cancel" };
    this->propertyNames = { L"task" };
    this->task = functionAsArrayOf;
}
//=============================================================================
OnCleanupObjectHandle::~OnCleanupObjectHandle() { }
//=============================================================================
void
OnCleanupObjectHandle::cleanup(Evaluator* eval)
{
    if (isCanceled) {
        return;
    }
    function_handle fh = task.getContentAsFunctionHandle();
    FunctionDef* funcDef = nullptr;
    if (fh.anonymousHandle != nullptr) {
        funcDef = (FunctionDef*)fh.anonymousHandle;
    }
    if (funcDef) {
        funcDef->evaluateFunction(eval, ArrayOfVector(), 0);
    }
    cancel();
}
//=============================================================================
void
OnCleanupObjectHandle::cancel()
{
    isCanceled = true;
}
//=============================================================================
wstringVector
OnCleanupObjectHandle::getMethods()
{
    return methodNames;
}
//=============================================================================
bool
OnCleanupObjectHandle::isMethod(const std::wstring& methodName)
{
    return std::find(methodNames.begin(), methodNames.end(), methodName) != methodNames.end();
}
//=============================================================================
bool
OnCleanupObjectHandle::isProperty(const std::wstring& propertyName)
{
    return std::find(propertyNames.begin(), propertyNames.end(), propertyName)
        != propertyNames.end();
}
//=============================================================================
wstringVector
OnCleanupObjectHandle::getProperties()
{
    return propertyNames;
}
//=============================================================================
ArrayOf
OnCleanupObjectHandle::getTask()
{
    return task;
}
//=============================================================================
}
//=============================================================================
