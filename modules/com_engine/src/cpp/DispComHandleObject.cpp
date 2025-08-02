//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <Ole2.h>
#include <Windows.h>
#include <ocidl.h>
#include "DispComHandleObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "classnameComHandleObject.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
DispComHandleObject(Interface* io, ComHandleObject* comHandle)
{
    if (comHandle != nullptr) {
        io->outputMessage("\n");
        std::string fullClassName;
        classnameComHandle(comHandle, fullClassName);
        io->outputMessage("\t" + fullClassName);
        io->outputMessage("\n");
    }
}
//=============================================================================
void
DispComHandleObject(Interface* io, const ArrayOf& A, const std::string& name)
{
    if (A.isHandle()) {
        DisplayVariableHeader(io, A, utf8_to_wstring(name), false);
        if (A.isScalar()) {
            auto* qp = (nelson_handle*)A.getDataPointer();
            nelson_handle hl = qp[0];
            HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
            if (hlObj->getCategory() != NLS_HANDLE_COM_CATEGORY_STR) {
                Error(_W("COM handle expected."));
            }
            auto* comhandleobj = (ComHandleObject*)hlObj;
            DispComHandleObject(io, comhandleobj);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("COM handle expected."));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
