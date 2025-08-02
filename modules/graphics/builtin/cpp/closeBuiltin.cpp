//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "closeBuiltin.hpp"
#include "CloseFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOPropertyNames.hpp"
#include "GOStringProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
closeHelper(const ArrayOf& arg, bool asAll);
//=============================================================================
ArrayOfVector
closeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);
    bool result = false;
    if (argIn.empty()) {
        result = closeCurrentFigure();
    }
    if (argIn.size() == 1) {
        result = closeHelper(argIn[0], true);
        if (!result) {
            Error(_W("Specified window does not exist."), L"Nelson:close:WindowNotFound");
        }
    } else {
        for (size_t k = 0; k < argIn.size(); k++) {
            if (k == 0) {
                result = closeHelper(argIn[k], false);
            } else {
                result = closeHelper(argIn[k], false) || result;
            }
        }
    }
    if (nLhs == 1) {
        retval << ArrayOf::logicalConstructor(result);
    }
    return retval;
}
//=============================================================================
bool
closeHelper(const ArrayOf& arg, bool asAll)
{
    bool result = false;
    if (arg.isRowVectorCharacterArray()) {
        std::wstring str = arg.getContentAsWideString();
        if (str == L"all" && asAll) {
            closeAllFigures();
            return true;
        } else {
            return closeFigureByName(str);
        }
    } else if (arg.isNumeric()) {
        ArrayOf _arg(arg);
        _arg.promoteType(NLS_INT64);
        int64* ptr = static_cast<int64*>(
            const_cast<void*>(static_cast<const void*>(_arg.getDataPointer())));
        std::vector<go_handle> ids;
        for (size_t k = 0; k < arg.getElementCount(); k++) {
            GOWindow* win = getHandleWindow(ptr[k] - 1);
            if (win) {
                ids.push_back(ptr[k] - 1);
            }
        }
        bool ok = true;
        for (size_t k = 0; k < ids.size(); k++) {
            if (k == 0) {
                ok = closeFigure(ids[k]);
            } else {
                ok = closeFigure(ids[k]) || ok;
            }
        }
        result = ok;
    } else if (arg.isGraphicsObject()) {
        nelson_handle* ptr = static_cast<nelson_handle*>(
            const_cast<void*>(static_cast<const void*>(arg.getDataPointer())));
        std::vector<go_handle> ids;
        for (indexType k = 0; k < arg.getElementCount(); ++k) {
            GOWindow* win = getHandleWindow(ptr[k]);
            if (win) {
                ids.push_back(ptr[k]);
            }
        }
        bool ok = true;
        for (size_t k = 0; k < ids.size(); k++) {
            if (k == 0) {
                ok = closeFigure(ids[k]);
            } else {
                ok = closeFigure(ids[k]) || ok;
            }
        }
        result = ok;
    } else if (arg.isEmpty()) {
        result = true;
    } else {
        result = false;
    }
    return result;
}
//=============================================================================
}
//=============================================================================
