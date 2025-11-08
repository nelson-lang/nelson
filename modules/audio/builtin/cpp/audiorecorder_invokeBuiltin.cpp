//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_invokeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "AudiorecorderObject.hpp"
#include "HandleGenericObject.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================

ArrayOfVector
Nelson::AudioGateway::audiorecorder_invokeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0);
    HandleGenericObject* hgo = argIn[0].getContentAsHandleScalar();
    if (!hgo || hgo->getCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("Audio recorder object expected."));
    }

    std::string methodname = argIn[1].getContentAsCString();
    ArrayOfVector params;
    for (size_t k = 2; k < argIn.size(); k++) {
        params.push_back(argIn[k]);
    }

    AudiorecorderObject* poh = (AudiorecorderObject*)hgo;
    Interface* io = nullptr;
    if (eval) {
        io = eval->getInterface();
    }
    if (!poh->invokeMethod(io, params, nLhs, methodname, retval)) {
        Error(ERROR_WRONG_ARGUMENT_2_VALUE + L" " + utf8_to_wstring(methodname));
    }
    return retval;
}
//=============================================================================
