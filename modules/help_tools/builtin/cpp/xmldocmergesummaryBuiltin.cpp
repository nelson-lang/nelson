//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmldocmergesummaryBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "XmlDocMergeSummary.hpp"
//=============================================================================
namespace Nelson::HelpToolsGateway {
//=============================================================================
ArrayOfVector
xmldocmergesummaryBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring destinationDirectory = argIn[0].getContentAsWideString();
    wstringVector filenames = argIn[1].getContentAsWideStringVector();
    std::wstring errorMessage;
    if (!XmlDocMergeSummary(destinationDirectory, filenames, errorMessage)) {
        retval.push_back(ArrayOf::logicalConstructor(false));
        Error(errorMessage);
    } else {
        retval.push_back(ArrayOf::logicalConstructor(true));
    }

    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
