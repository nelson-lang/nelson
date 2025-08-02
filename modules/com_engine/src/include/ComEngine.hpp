//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "ComHandleObject.hpp"
#include "nlsCom_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSCOM_ENGINE_IMPEXP ComEngine
{
public:
    static ComEngine*
    getInstance();
    void
    create();
    void
    finish();

private:
    bool isInitialized;
    ComEngine();
    static ComEngine* m_pInstance;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
