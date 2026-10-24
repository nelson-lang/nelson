//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmltransformBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "XmlTransform.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::xmltransformBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);

    const std::wstring xmlfile = argIn[0].asWideString();
    const std::wstring xslfile = argIn[1].asWideString();
    const std::wstring outputfile = argIn[2].asWideString();
    bool overwrite = true;
    if (argIn.size() == 4) {
        overwrite = argIn[3].asLogicalScalar();
    }

    std::wstring errorMessage;
    bool res = XmlTransform(
        xmlfile, xslfile, outputfile, overwrite, DOCUMENT_OUTPUT::UNDEFINED, errorMessage);
    if (!res) {
        Error(L"Nelson:xmltransform:transformFailed", errorMessage);
    }
    retval << ArrayOf::logicalScalar(res);
    return retval;
}
//=============================================================================
