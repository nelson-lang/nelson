//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__activecontour_kernel__Builtin.hpp"
#include "ActiveContour.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
ArrayOfVector
__activecontour_kernel__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 6, 6);
    nargoutcheck(nLhs, 0, 1);
    double iterations = argIn[2].asDoubleScalar();
    std::wstring method = argIn[3].asWideString();
    double smoothFactor = argIn[4].asDoubleScalar();
    double contractionBias = argIn[5].asDoubleScalar();
    ArrayOfVector retval;
    retval << ActiveContour(argIn[0], argIn[1], iterations, method, smoothFactor, contractionBias);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
