//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define NLS_FUNCTION_HANDLE_OWNER_CATEGORY_STR "function_handle_owner"
//=============================================================================
/**
 * Destroys the definition object behind a function handle.
 *
 * The module defining that object (the interpreter) registers the deleter
 * when it loads and unregisters it before it unloads. While no deleter is
 * registered, definitions are left alive.
 */
using FunctionHandleDefinitionDeleter = void (*)(nelson_handle* anonymousHandle);
//=============================================================================
NLSTYPES_IMPEXP void
setFunctionHandleDefinitionDeleter(FunctionHandleDefinitionDeleter deleter);
//=============================================================================
/**
 * Sole owner of the definition behind one function handle value.
 *
 * A function handle is stored as a struct-like block whose "owner" field is a
 * handle to this object. The definition is therefore destroyed exactly when
 * the last storage block referencing it is released: value copies share the
 * field (hence the owner), and deep copies of the block copy the field.
 */
class NLSTYPES_IMPEXP FunctionHandleOwner : public HandleGenericObject
{
public:
    explicit FunctionHandleOwner(nelson_handle* anonymousHandle);
    ~FunctionHandleOwner() override;
    FunctionHandleOwner(const FunctionHandleOwner&) = delete;
    FunctionHandleOwner&
    operator=(const FunctionHandleOwner&)
        = delete;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
