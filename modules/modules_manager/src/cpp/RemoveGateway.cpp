//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RemoveGateway.hpp"
#include "Error.hpp"
#include "GatewaysManager.hpp"
//=============================================================================
namespace Nelson {
void
RemoveGateway(Evaluator* eval, const std::wstring& dynlibname)
{
    std::wstring errorMessage;
    GatewaysManager::getInstance()->removeGateway(eval, dynlibname, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
