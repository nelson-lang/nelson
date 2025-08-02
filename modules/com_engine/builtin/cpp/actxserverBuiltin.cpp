//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "actxserverBuiltin.hpp"
#include "ActiveXServer.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::actxserverBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    nargoutcheck(nLhs, 0, 1);
    std::wstring progid;
    std::wstring machine;
    switch (argIn.size()) {
    case 1: {
        progid = argIn[0].getContentAsWideString();
    } break;
    case 3: {
        std::wstring type = argIn[2].getContentAsWideString();
        if (!(type == L"machine")) {
            Error(_W("'machine' value expected."));
        }
        progid = argIn[0].getContentAsWideString();
    } break;
    case 2:
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    ComHandleObject* comhandle = ActiveXServer(progid, machine);
    retval << ArrayOf::handleConstructor(comhandle);
#else
    Error(_W("Not implemented on this platform."));
#endif
    return retval;
}
//=============================================================================
