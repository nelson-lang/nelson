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
//=============================================================================
using namespace Nelson;
//=============================================================================
// xmldocbuild(source_dirs, destination_dir, main_title, export_format, overwrite)

ArrayOfVector
Nelson::HelpToolsGateway::xmltransformBuiltin(int nLhs, const ArrayOfVector& argIn)
{

    ArrayOfVector retval;
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);

    const std::wstring xmlfile = argIn[0].getContentAsWideString();
    const std::wstring xslfile = argIn[1].getContentAsWideString();
    const std::wstring outputfile = argIn[2].getContentAsWideString();
    bool overwrite = true;
    if (argIn.size() == 4) {
        overwrite = argIn[3].getContentAsLogicalScalar();
    }

    std::wstring errorMessage;
    bool res = XmlTransform(
        xmlfile, xslfile, outputfile, overwrite, DOCUMENT_OUTPUT::HTML_WEB, errorMessage);
    if (!res) {
        Error(errorMessage);
    }
    retval << ArrayOf::logicalConstructor(res);
    return retval;
}
//=============================================================================
