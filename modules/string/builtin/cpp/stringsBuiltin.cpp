//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "stringsBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::stringsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        retval << ArrayOf::stringArray(std::string());
    }
    if (argIn.size() == 1) {
        if (argIn[0].dataClass() == NLS_DOUBLE) {
            if (argIn[0].isVector() || argIn[0].isScalar()) {
                if (argIn[0].isScalar()) {
                    ArrayOf arg = argIn[0];
                    double dindex = arg.asDoubleScalar();
                    if (!std::isfinite(dindex)) {
                        Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #1");
                    }
                    if (dindex < 0) {
                        dindex = 0;
                    }
                    auto index = static_cast<indexType>(dindex);
                    if (static_cast<double>(index) != dindex) {
                        Error(L"Nelson:expectedInteger", L"%s must be an integer.", L"Argument #1");
                    }
                    Dimensions dims(index, index);
                    auto* elements = new ArrayOf[index * index];
                    for (indexType k = 0; k < index * index; k++) {
                        elements[k] = ArrayOf::characterArray("");
                    }
                    retval << ArrayOf(NLS_STRING_ARRAY, dims, elements);
                } else {
                    ArrayOf arg = argIn[0];
                    Dimensions dims(arg.elementCount());
                    auto* dindex = (double*)arg.dataPointer();
                    for (indexType k = 0; k < arg.elementCount(); k++) {
                        double _dIndex = dindex[k];
                        if (!std::isfinite(_dIndex)) {
                            Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #1");
                        }
                        if (_dIndex < 0) {
                            _dIndex = 0;
                        }
                        auto index = static_cast<indexType>(_dIndex);
                        if (static_cast<double>(index) != _dIndex) {
                            Error(L"Nelson:expectedInteger", L"%s must be an integer.",
                                L"Argument #1");
                        }
                        dims.setDimensionLength(k, index);
                    }
                    dims.simplify();
                    ompIndexType elementCount = dims.elementCount();
                    auto* elements = new ArrayOf[elementCount];
                    OMP_PARALLEL_FOR_LOOP(elementCount)
                    for (ompIndexType k = 0; k < elementCount; k++) {
                        elements[k] = ArrayOf::characterArray("");
                    }
                    retval << ArrayOf(NLS_STRING_ARRAY, dims, elements);
                }
            } else {
                Error(L"Nelson:expectedVector", L"%s must be a vector.", L"Argument #1");
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"double");
        }
    } else {
        Dimensions dims(argIn.size());
        for (indexType k = 0; k < static_cast<indexType>(argIn.size()); k++) {
            if (argIn[k].dataClass() == NLS_DOUBLE) {
                if (argIn[k].isScalar()) {
                    ArrayOf arg = argIn[k];
                    double dindex = arg.asDoubleScalar();
                    if (!std::isfinite(dindex)) {
                        Error(L"Nelson:expectedFinite", L"%s must be finite.",
                            L"Argument #" + std::to_wstring(k + 1));
                    }
                    if (dindex < 0) {
                        dindex = 0;
                    }
                    auto index = static_cast<indexType>(dindex);
                    if (static_cast<double>(index) != dindex) {
                        Error(L"Nelson:expectedInteger", L"%s must be an integer.",
                            L"Argument #" + std::to_wstring(k + 1));
                    }
                    dims.setDimensionLength(k, index);
                } else {
                    Error(L"Nelson:scalarExpected",
                        L"Wrong size for argument #%d: scalar expected.", (k + 1));
                }
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (k + 1),
                    L"double");
            }
        }
        dims.simplify();
        ompIndexType elementCount = dims.elementCount();
        auto* elements = new ArrayOf[elementCount];
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            elements[k] = ArrayOf::characterArray("");
        }
        retval << ArrayOf(NLS_STRING_ARRAY, dims, elements);
    }
    return retval;
}
//=============================================================================
