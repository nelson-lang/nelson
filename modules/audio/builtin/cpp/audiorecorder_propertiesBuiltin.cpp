//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "audiorecorder_propertiesBuiltin.hpp"
#include "AudiorecorderObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::audiorecorder_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
        Error(_W("audiorecorder handle expected."));
    }
    auto* objRec = (AudiorecorderObject*)param1.getContentAsHandleScalar();
    wstringVector fieldnames = objRec->fieldnames();
    if (nLhs == 0) {
        std::wstring msg;
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
            msg = L"\n";
        }
        if (fieldnames.empty()) {
            msg = msg + _W("No property for class: audiorecorder.") + L"\n";
        } else {
            msg = msg + _W("Properties for class: audiorecorder:") + L"\n\n";
            for (auto& fieldname : fieldnames) {
                msg = msg + std::wstring(L"\t") + fieldname + std::wstring(L"\n");
            }
        }
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
            msg = msg + std::wstring(L"\n");
        }
        NelsonPrint(msg);
    } else {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(fieldnames);
    }
    return retval;
}
//=============================================================================
