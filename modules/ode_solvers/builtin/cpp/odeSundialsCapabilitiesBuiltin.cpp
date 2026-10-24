//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "odeSundialsCapabilitiesBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
logicalCapability(bool value)
{
    return ArrayOf::logicalScalar(value);
}
//=============================================================================
ArrayOfVector
Nelson::OdeSolversGateway::odeSundialsCapabilitiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    stringVector names { "available", "sparseMatrix", "spgmr", "spfgmr", "spbcgs", "sptfqmr", "pcg",
        "klu", "kinsol" };
    ArrayOfVector values;
#ifdef NELSON_WITH_SUNDIALS
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_SPARSE_MATRIX
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_SPGMR
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_SPFGMR
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_SPBCGS
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_SPTFQMR
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_PCG
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_KLU
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
#ifdef NELSON_WITH_SUNDIALS_KINSOL
    values << logicalCapability(true);
#else
    values << logicalCapability(false);
#endif
    ArrayOfVector retval;
    retval << ArrayOf::structArray(names, values);
    return retval;
}
//=============================================================================
