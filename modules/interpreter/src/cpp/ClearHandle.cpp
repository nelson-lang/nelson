//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClearHandle.hpp"
#include "HandleState.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ClearHandle(Evaluator* eval, Scope* scope, const std::string& variable)
{
    ArrayOf val;
    if (!scope->lookupVariable(variable, val) || !val.isHandle()) {
        return false;
    }
    const auto* handles = static_cast<const HandleRef*>(val.dataPointer());
    bool res = false;
    for (indexType k = 0; handles != nullptr && k < val.elementCount(); ++k) {
        HandleRef ref = handles[k];
        HandleGenericObject* object = ref ? ref->object() : nullptr;
        if (object != nullptr && object->isScoped()) {
            object->onClear(eval);
            res = true;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
