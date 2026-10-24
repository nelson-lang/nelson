//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwctype>
#include "closeBuiltin.hpp"
#include "CloseFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOStringProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
closeHelper(const ArrayOf& arg, bool asAll, bool& found, bool forceClose);
//=============================================================================
static bool
isForceFlag(const ArrayOf& arg)
{
    if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
        return false;
    }
    std::wstring str = arg.asWideString();
    std::wstring lower;
    lower.reserve(str.size());
    for (wchar_t c : str) {
        lower.push_back(static_cast<wchar_t>(std::towlower(c)));
    }
    return lower == L"force";
}
//=============================================================================
static bool
closeFigureForced(go_handle fignum, bool forceClose)
{
    if (forceClose) {
        return deleteFigure(fignum, true);
    }
    return closeFigure(fignum);
}
//=============================================================================
ArrayOfVector
closeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    // A trailing 'force' flag closes figures even when a CloseRequestFcn
    // callback would otherwise veto the operation.
    ArrayOfVector args(argIn);
    bool forceClose = false;
    if (!args.empty() && isForceFlag(args.back())) {
        forceClose = true;
        args.pop_back();
    }

    bool result = false;
    if (args.empty()) {
        result = closeCurrentFigure(forceClose);
    } else if (args.size() == 1) {
        bool found = false;
        result = closeHelper(args[0], true, found, forceClose);
        if (!found) {
            Error(L"Nelson:close:WindowNotFound", L"Specified window does not exist.");
        }
    } else {
        for (size_t k = 0; k < args.size(); k++) {
            bool found = false;
            if (k == 0) {
                result = closeHelper(args[k], false, found, forceClose);
            } else {
                result = closeHelper(args[k], false, found, forceClose) || result;
            }
        }
    }
    if (nLhs == 1) {
        retval << ArrayOf::logicalScalar(result);
    }
    return retval;
}
//=============================================================================
bool
closeHelper(const ArrayOf& arg, bool asAll, bool& found, bool forceClose)
{
    bool result = false;
    if (arg.isRowVectorCharacterArray()) {
        std::wstring str = arg.asWideString();
        if (str == L"all" && asAll) {
            found = true;
            closeAllFigures(forceClose);
            return true;
        } else {
            std::vector<int64> nums = getFigureGraphicsObjects();
            for (long long num : nums) {
                GOFigureHost* window = getFigure(num);
                if (!window) {
                    continue;
                }
                GraphicsObject* goFigure = window->hostFigure();
                if (!goFigure) {
                    continue;
                }
                GOStringProperty* propertyName = static_cast<GOStringProperty*>(
                    goFigure->findProperty(GO_NAME_PROPERTY_NAME_STR));
                if (propertyName->data() == str) {
                    found = true;
                    return closeFigureForced(num, forceClose);
                }
            }
            return false;
        }
    } else if (arg.isNumeric()) {
        ArrayOf _arg(arg);
        _arg.promoteType(NLS_INT64);
        int64* ptr
            = static_cast<int64*>(const_cast<void*>(static_cast<const void*>(_arg.dataPointer())));
        std::vector<go_handle> ids;
        for (size_t k = 0; k < arg.elementCount(); k++) {
            GOFigureHost* win = getHandleWindow(ptr[k] - 1);
            if (win) {
                ids.push_back(ptr[k] - 1);
            }
        }
        found = true;
        bool ok = true;
        for (size_t k = 0; k < ids.size(); k++) {
            if (k == 0) {
                ok = closeFigureForced(ids[k], forceClose);
            } else {
                ok = closeFigureForced(ids[k], forceClose) || ok;
            }
        }
        result = ok;
    } else if (arg.isGraphicsObject()) {
        nelson_handle* ptr = static_cast<nelson_handle*>(
            const_cast<void*>(static_cast<const void*>(arg.dataPointer())));
        std::vector<go_handle> ids;
        for (indexType k = 0; k < arg.elementCount(); ++k) {
            GOFigureHost* win = getHandleWindow(ptr[k]);
            if (win) {
                ids.push_back(ptr[k]);
            }
        }
        found = true;
        bool ok = true;
        for (size_t k = 0; k < ids.size(); k++) {
            if (k == 0) {
                ok = closeFigureForced(ids[k], forceClose);
            } else {
                ok = closeFigureForced(ids[k], forceClose) || ok;
            }
        }
        result = ok;
    } else if (arg.isEmpty()) {
        found = true;
        result = true;
    } else {
        found = false;
        result = false;
    }
    return result;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
