//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <atomic>
#include "FunctionHandleOwner.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<FunctionHandleDefinitionDeleter> definitionDeleter { nullptr };
//=============================================================================
void
setFunctionHandleDefinitionDeleter(FunctionHandleDefinitionDeleter deleter)
{
    definitionDeleter.store(deleter, std::memory_order_release);
}
//=============================================================================
FunctionHandleOwner::FunctionHandleOwner(nelson_handle* anonymousHandle)
    : HandleGenericObject(NLS_FUNCTION_HANDLE_OWNER_CATEGORY_STR, anonymousHandle, false)
{
}
//=============================================================================
FunctionHandleOwner::~FunctionHandleOwner()
{
    FunctionHandleDefinitionDeleter deleter = definitionDeleter.load(std::memory_order_acquire);
    if (deleter != nullptr) {
        deleter(static_cast<nelson_handle*>(pointer()));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
