//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nh5fileBuiltin.hpp"
#include "Nh5FileIO.hpp"
#include "PartialFileAccess.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Hdf5Gateway::nh5fileReadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring filename = argIn[0].asWideString();
    const std::string name = argIn[1].asUtf8String();
    ArrayOfVector retval;
    if (argIn.size() == 3) {
        ArrayOfVector subscripts = PartialFileSubscripts(argIn[2]);
        retval << Nh5FileRead(eval, filename, name, &subscripts);
    } else {
        retval << Nh5FileRead(eval, filename, name, nullptr);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Hdf5Gateway::nh5fileWriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 0);
    const std::wstring filename = argIn[0].asWideString();
    const std::string name = argIn[1].asUtf8String();
    if (argIn.size() == 4) {
        ArrayOfVector subscripts = PartialFileSubscripts(argIn[3]);
        Nh5FileWrite(eval, filename, name, &subscripts, argIn[2]);
    } else {
        Nh5FileWrite(eval, filename, name, nullptr, argIn[2]);
    }
    return {};
}
//=============================================================================
ArrayOfVector
Hdf5Gateway::nh5fileSizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << Nh5FileVariableSize(argIn[0].asWideString(), argIn[1].asUtf8String());
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
