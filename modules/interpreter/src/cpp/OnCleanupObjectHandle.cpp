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
#include <exception>
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
    cancel();
    function_handle fh = task.asFunctionHandle();
    FunctionDef* funcDef = nullptr;
    if (fh.anonymousHandle != nullptr) {
        funcDef = (FunctionDef*)fh.anonymousHandle;
    }
    if (funcDef) {
        funcDef->evaluateFunction(eval, ArrayOfVector(), 0);
    }
}
//=============================================================================
void
OnCleanupObjectHandle::cancel()
{
    isCanceled = true;
}
//=============================================================================
void
OnCleanupObjectHandle::onClear(Evaluator* eval)
{
    cleanup(eval);
}
//=============================================================================
wstringVector
OnCleanupObjectHandle::methods()
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
OnCleanupObjectHandle::properties()
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
void
runOnCleanupTasks(Evaluator* eval, const ArrayOfVector& tasks)
{
    std::exception_ptr firstError;
    for (const ArrayOf& task : tasks) {
        try {
            if (task.isHandle() && task.handleClassName() == NLS_HANDLE_ONCLEANUP_CATEGORY_STR) {
                auto* obj = static_cast<OnCleanupObjectHandle*>(task.asHandleScalar());
                if (obj != nullptr && obj->isScoped()) {
                    obj->cleanup(eval);
                }
            }
        } catch (...) {
            if (!firstError) {
                firstError = std::current_exception();
            }
        }
    }
    if (firstError) {
        std::rethrow_exception(firstError);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
