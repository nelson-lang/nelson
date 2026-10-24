//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QObject_propertiesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "fieldnamesQObjectHandleObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::QmlEngineGateway::QObject_propertiesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool fullList = false;
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(
            L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", ((1)), L"handle");
    }
    wstringVector fieldnames;
    fieldnamesQObjectHandleObject(param1, fullList, fieldnames);
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        if (io) {
            std::wstring msg;
            if (fieldnames.size() == 0) {
                msg = L"\n" + _W("No property for class QObject.") + L"\n";
            } else {
                msg = L"\n" + _W("Properties for class QObject:") + L"\n\n";
                for (auto& fieldname : fieldnames) {
                    msg = msg + std::wstring(L"\t") + fieldname + std::wstring(L"\n");
                }
                msg = msg + std::wstring(L"\n");
            }
            io->outputMessage(msg);
        }
    } else {
        retval.push_back(ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fieldnames));
    }
    return retval;
}
//=============================================================================
