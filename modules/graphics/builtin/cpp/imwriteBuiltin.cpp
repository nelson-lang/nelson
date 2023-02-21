//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include "imwriteBuiltin.hpp"
#include "ImageWriter.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::imwriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // imwrite(A, filename)
    // imwrite(A, map, filename)
    // imwrite(..., filename, fmt)
    // imwrite(..., filename, fmt, "Comments", comment, "Quality", value)

    ArrayOfVector retval = {};
    nargincheck(argIn, 2, 10);
    nargoutcheck(nLhs, 0, 0);

    ArrayOf image = argIn[0];
    ArrayOf map = ArrayOf::emptyConstructor(0, 0);
    ArrayOf alphaMap = ArrayOf::emptyConstructor(0, 0);
    std::wstring filename;
    std::wstring fmt;
    int quality = DEFAULT_QUALITY;
    std::map<std::wstring, wstringVector> nameValue;

    indexType pos;

    if (argIn[1].isNumeric()) {
        map = argIn[1];
        nargincheck(argIn, 3, 10);
        filename = argIn[2].getContentAsWideString();
        pos = 3;
    } else {
        filename = argIn[1].getContentAsWideString();
        pos = 2;
    }

    if ((argIn.size() - pos) > 0) {
        if ((argIn.size() - pos) > 1) {
            if ((argIn.size() - pos) % 2 != 0) {
                fmt = argIn[pos].getContentAsWideString();
                pos = pos + 1;
            }
            for (indexType k = pos; k < argIn.size(); k = k + 2) {
                std::wstring name = argIn[k].getContentAsWideString();
                if (StringHelpers::iequals(name, L"quality")) {
                    quality = argIn[k + 1].getContentAsInteger8Scalar();
                    if (quality < 0 || quality > 100) {
                        Error(_W("Wrong value for quality: [0, 100] expected."));
                    }
                } else if (StringHelpers::iequals(name, L"alpha")) {
                    alphaMap = argIn[k + 1];
                } else {
                    wstringVector value = argIn[k + 1].getContentAsWideStringVector();
                    nameValue.insert(std::make_pair(name, value));
                }
            }
        } else {
            fmt = argIn[pos].getContentAsWideString();
        }
    }

    imageWriter(filename, image, map, fmt, alphaMap, quality, nameValue);

    return retval;
}
//=============================================================================
