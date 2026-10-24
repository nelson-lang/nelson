//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "gpuOperatorsBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
toHostArray(const ArrayOf& value);
static void
collectParts(const ArrayOfVector& args, size_t start, std::vector<GpuArrayObject*>& parts,
    std::vector<GpuArrayObject*>& temporaries);
static void
freeTemporaries(std::vector<GpuArrayObject*>& temporaries);
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayTransposeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    ArrayOfVector retval;
    retval << GpuTranspose(a, false); // .' : no conjugation
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCtransposeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    ArrayOfVector retval;
    retval << GpuTranspose(a, true); // ' : conjugate transpose
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayReshapeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 32);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    indexType total = a->dimensions().elementCount();

    // collect requested dimension lengths; a single empty argument means the
    // corresponding dimension is inferred from the total element count.
    std::vector<double> requested;
    int inferredPosition = -1;
    if (argIn.size() == 2 && !argIn[1].isEmpty() && !argIn[1].isScalar()) {
        // reshape(A, sizeVector)
        requested = argIn[1].asDoubleVector();
    } else {
        for (size_t k = 1; k < argIn.size(); ++k) {
            if (argIn[k].isEmpty()) {
                if (inferredPosition >= 0) {
                    Error(L"Nelson:reshape:multipleUnknownDimensions",
                        L"reshape: only one dimension can be inferred with [] .");
                }
                inferredPosition = (int)(k - 1);
                requested.push_back(0);
            } else {
                requested.push_back(argIn[k].asDoubleScalar());
            }
        }
    }
    std::vector<indexType> newLengths(requested.size());
    indexType knownProduct = 1;
    for (size_t k = 0; k < requested.size(); ++k) {
        if ((int)k == inferredPosition) {
            continue;
        }
        newLengths[k] = (indexType)requested[k];
        knownProduct *= newLengths[k];
    }
    if (inferredPosition >= 0) {
        if (knownProduct == 0 || (total % knownProduct) != 0) {
            Error(L"Nelson:reshape:cannotInferDimension",
                L"reshape: cannot infer the missing dimension.");
        }
        newLengths[inferredPosition] = total / knownProduct;
    }
    Dimensions newDims(newLengths);
    ArrayOfVector retval;
    retval << GpuReshape(a, newDims);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIndexBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    ArrayOfVector retval;
    if (argIn.size() == 2) {
        ArrayOf idxHost = toHostArray(argIn[1]);
        retval << GpuIndexLinear(a, idxHost.asDoubleVector(), idxHost.dimensions());
    } else if (argIn.size() == 3 && a->dimensions().is2D()) {
        ArrayOf rowsHost = toHostArray(argIn[1]);
        ArrayOf colsHost = toHostArray(argIn[2]);
        retval << GpuIndex2D(a, rowsHost.asDoubleVector(), colsHost.asDoubleVector());
    } else {
        // three or more subscripts, or two subscripts on an N-D array (the last
        // subscript folds the trailing dimensions)
        std::vector<std::vector<double>> subs;
        subs.reserve(argIn.size() - 1);
        for (size_t k = 1; k < argIn.size(); ++k) {
            subs.push_back(toHostArray(argIn[k]).asDoubleVector());
        }
        retval << GpuIndexND(a, subs);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayAsgnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);
    nargoutcheck(nLhs, 0, 1);
    GpuArrayObject* a = AsGpuArrayObject(argIn[0]);
    if (a == nullptr) {
        Error(L"Nelson:gpuArray:notGpuArray", L"gpuArray expected.");
    }
    // right-hand side: reuse a device operand, or upload a host one as temp
    GpuArrayObject* rhs = AsGpuArrayObject(argIn[1]);
    GpuArrayObject* rhsTemporary = nullptr;
    if (rhs == nullptr) {
        rhsTemporary = UploadGpuArray(argIn[1]);
        rhs = rhsTemporary;
    }
    ArrayOfVector retval;
    try {
        if (argIn.size() == 3) {
            ArrayOf idxHost = toHostArray(argIn[2]);
            retval << GpuSubsasgnLinear(a, idxHost.asDoubleVector(), rhs);
        } else if (argIn.size() == 4 && a->dimensions().is2D()) {
            ArrayOf rowsHost = toHostArray(argIn[2]);
            ArrayOf colsHost = toHostArray(argIn[3]);
            retval << GpuSubsasgn2D(a, rowsHost.asDoubleVector(), colsHost.asDoubleVector(), rhs);
        } else {
            // three or more subscripts, or two subscripts on an N-D array
            std::vector<std::vector<double>> subs;
            subs.reserve(argIn.size() - 2);
            for (size_t k = 2; k < argIn.size(); ++k) {
                subs.push_back(toHostArray(argIn[k]).asDoubleVector());
            }
            retval << GpuSubsasgnND(a, subs, rhs);
        }
    } catch (...) {
        delete rhsTemporary;
        throw;
    }
    delete rhsTemporary;
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayHorzcatBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1024);
    nargoutcheck(nLhs, 0, 1);
    std::vector<GpuArrayObject*> parts;
    std::vector<GpuArrayObject*> temporaries;
    collectParts(argIn, 0, parts, temporaries);
    ArrayOfVector retval;
    try {
        retval << GpuHorzcat(parts);
    } catch (...) {
        freeTemporaries(temporaries);
        throw;
    }
    freeTemporaries(temporaries);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayVertcatBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1024);
    nargoutcheck(nLhs, 0, 1);
    std::vector<GpuArrayObject*> parts;
    std::vector<GpuArrayObject*> temporaries;
    collectParts(argIn, 0, parts, temporaries);
    ArrayOfVector retval;
    try {
        retval << GpuVertcat(parts);
    } catch (...) {
        freeTemporaries(temporaries);
        throw;
    }
    freeTemporaries(temporaries);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 1024);
    nargoutcheck(nLhs, 0, 1);
    indexType dim = argIn[0].asScalarIndex(false);
    std::vector<GpuArrayObject*> parts;
    std::vector<GpuArrayObject*> temporaries;
    collectParts(argIn, 1, parts, temporaries);
    ArrayOfVector retval;
    try {
        if (dim == 1) {
            retval << GpuVertcat(parts);
        } else if (dim == 2) {
            retval << GpuHorzcat(parts);
        } else {
            // reached only once the host cat() dispatches N-D concatenation to
            // this overload (today its .m macro handles dim >= 3 itself)
            retval << GpuCatND(parts, (int)dim);
        }
    } catch (...) {
        freeTemporaries(temporaries);
        throw;
    }
    freeTemporaries(temporaries);
    return retval;
}
//=============================================================================
// static helper functions
//=============================================================================
static ArrayOf
toHostArray(const ArrayOf& value)
{
    GpuArrayObject* gpuObject = AsGpuArrayObject(value);
    if (gpuObject != nullptr) {
        return GatherGpuArray(gpuObject);
    }
    return value;
}
//=============================================================================
static void
collectParts(const ArrayOfVector& args, size_t start, std::vector<GpuArrayObject*>& parts,
    std::vector<GpuArrayObject*>& temporaries)
{
    for (size_t k = start; k < args.size(); ++k) {
        if (args[k].isEmpty()) {
            continue; // empty inputs are ignored by concatenation
        }
        GpuArrayObject* part = AsGpuArrayObject(args[k]);
        if (part == nullptr) {
            part = UploadGpuArray(args[k]);
            temporaries.push_back(part);
        }
        parts.push_back(part);
    }
    if (parts.empty()) {
        Error(L"Nelson:gpuArray:emptyConcatInput",
            L"gpuArray concatenation requires at least one non-empty input.");
    }
}
//=============================================================================
static void
freeTemporaries(std::vector<GpuArrayObject*>& temporaries)
{
    for (auto* temporary : temporaries) {
        delete temporary;
    }
}
//=============================================================================
