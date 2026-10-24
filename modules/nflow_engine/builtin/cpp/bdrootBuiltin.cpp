//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bdrootBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// bdroot(obj)
//   Returns the name of the top-level model containing a block path: the part
//   before the first '/'. bdroot('m') is 'm'; bdroot('m/Sub/Blk') is 'm'. The
//   root model must be loaded.
//=============================================================================
ArrayOfVector
bdrootBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string path = argIn[0].asUtf8String();
    const size_t slash = path.find('/');
    std::string root = (slash == std::string::npos) ? path : path.substr(0, slash);

    std::string modelName;
    ArrayOf rootArg = ArrayOf::characterArray(root);
    (void)requireModel(rootArg, modelName);

    ArrayOfVector retval;
    retval << ArrayOf::characterArray(root);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
