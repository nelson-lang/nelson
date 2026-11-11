//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "gpuMatrixStructureBuiltin.hpp"
#include "gpuFallbackBuiltin.hpp"
#include "GpuArrayObject.hpp"
#include "GpuKernels.hpp"
#include "GpuMatrixStructure.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
ArrayOfVector
host(Evaluator* eval, const wchar_t* name, int nLhs, const ArrayOfVector& argIn)
{
    return GpuEngineGateway::gpuArrayHostFallback(eval, name, nLhs, argIn);
}
//=============================================================================
ArrayOfVector
yesNo(bool value)
{
    ArrayOfVector r;
    r << ArrayOf::logicalScalar(value);
    return r;
}
//=============================================================================
// structure of the only (gpuArray) argument; false sends the call to the host
bool
structureOf(const ArrayOfVector& argIn, size_t maxArgs, GpuStructureInfo& info)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    return a != nullptr && argIn.size() <= maxArgs && GpuMatrixStructure(a, info);
}
//=============================================================================
// real host or device vector (or scalar) on the device
GpuArrayObject*
realVector(const ArrayOf& value, ArrayOf& owner)
{
    GpuArrayObject* g = AsGpuArrayObject(value);
    if (g != nullptr) {
        bool ok = !g->isComplex() && !g->isRawIntStorage() && GpuIntegerResultType(g, g).empty()
            && (g->dimensions().isVector() || g->dimensions().isScalar() || g->elementCount() == 0);
        return ok ? g : nullptr;
    }
    if (!(value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_SINGLE) || value.isComplex()
        || value.isSparse() || !(value.isVector() || value.isScalar() || value.isEmpty())) {
        return nullptr;
    }
    owner = CreateGpuArray(value);
    return AsGpuArrayObject(owner);
}
//=============================================================================
ArrayOfVector
one(const ArrayOf& value)
{
    ArrayOfVector r;
    r << value;
    return r;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIstrilBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuStructureInfo info;
    if (!structureOf(argIn, 1, info)) {
        return host(eval, L"istril", nLhs, argIn);
    }
    return yesNo(info.upperBandwidth == 0);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIstriuBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuStructureInfo info;
    if (!structureOf(argIn, 1, info)) {
        return host(eval, L"istriu", nLhs, argIn);
    }
    return yesNo(info.lowerBandwidth == 0);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIsdiagBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuStructureInfo info;
    if (!structureOf(argIn, 1, info)) {
        return host(eval, L"isdiag", nLhs, argIn);
    }
    return yesNo(info.lowerBandwidth == 0 && info.upperBandwidth == 0);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIsbandedBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuStructureInfo info;
    if (argIn.size() != 3 || AsGpuArrayObject(argIn[1]) || AsGpuArrayObject(argIn[2])
        || !argIn[1].isScalar() || !argIn[2].isScalar() || !structureOf(argIn, 3, info)) {
        return host(eval, L"isbanded", nLhs, argIn);
    }
    double lower = argIn[1].asDoubleScalar();
    double upper = argIn[2].asDoubleScalar();
    if (lower < 0 || upper < 0) {
        return host(eval, L"isbanded", nLhs, argIn);
    }
    return yesNo((double)info.lowerBandwidth <= lower && (double)info.upperBandwidth <= upper);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBandwidthBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuStructureInfo info;
    std::string type;
    if (argIn.size() == 2) {
        if (!(argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
            return host(eval, L"bandwidth", nLhs, argIn);
        }
        type = argIn[1].asUtf8String();
        if ((type != "lower" && type != "upper") || nLhs > 1) {
            return host(eval, L"bandwidth", nLhs, argIn);
        }
    }
    if (!structureOf(argIn, 2, info)) {
        return host(eval, L"bandwidth", nLhs, argIn);
    }
    ArrayOfVector retval;
    if (type == "upper") {
        retval << ArrayOf::doubleScalar((double)info.upperBandwidth);
        return retval;
    }
    retval << ArrayOf::doubleScalar((double)info.lowerBandwidth);
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar((double)info.upperBandwidth);
    }
    return retval;
}
//=============================================================================
// issymmetric(A [, 'skew' | 'nonskew']) and ishermitian; tolerances stay on the host
static ArrayOfVector
symmetryTest(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool hermitian)
{
    const wchar_t* name = hermitian ? L"ishermitian" : L"issymmetric";
    bool skew = false;
    if (argIn.size() == 2) {
        if (!(argIn[1].isRowVectorCharacterArray() || argIn[1].isScalarStringArray())) {
            return host(eval, name, nLhs, argIn);
        }
        std::string option = argIn[1].asUtf8String();
        if (option != "skew" && option != "nonskew") {
            return host(eval, name, nLhs, argIn);
        }
        skew = (option == "skew");
    }
    GpuStructureInfo info;
    if (!structureOf(argIn, 2, info)) {
        return host(eval, name, nLhs, argIn);
    }
    if (hermitian) {
        return yesNo(skew ? info.skewHermitian : info.hermitian);
    }
    return yesNo(skew ? info.skewSymmetric : info.symmetric);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIssymmetricBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return symmetryTest(eval, nLhs, argIn, false);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayIshermitianBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return symmetryTest(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayToeplitzBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf ownerC, ownerR;
    GpuArrayObject* c
        = (argIn.size() == 1 || argIn.size() == 2) ? realVector(argIn[0], ownerC) : nullptr;
    GpuArrayObject* r = (argIn.size() == 2) ? realVector(argIn[1], ownerR) : c;
    if (c == nullptr || r == nullptr || nLhs > 1) {
        return host(eval, L"toeplitz", nLhs, argIn);
    }
    return one(GpuStructuredMatrix(
        GpuStructuredKind::Toeplitz, c, r, c->elementCount(), r->elementCount()));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayHankelBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf ownerC, ownerR;
    GpuArrayObject* c
        = (argIn.size() == 1 || argIn.size() == 2) ? realVector(argIn[0], ownerC) : nullptr;
    GpuArrayObject* r = (argIn.size() == 2) ? realVector(argIn[1], ownerR) : nullptr;
    if (c == nullptr || (argIn.size() == 2 && r == nullptr) || nLhs > 1) {
        return host(eval, L"hankel", nLhs, argIn);
    }
    indexType m = c->elementCount();
    indexType n = r ? r->elementCount() : m;
    return one(GpuStructuredMatrix(GpuStructuredKind::Hankel, c, r, m, n));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayVanderBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf owner;
    GpuArrayObject* v = (argIn.size() == 1) ? realVector(argIn[0], owner) : nullptr;
    if (v == nullptr || nLhs > 1) {
        return host(eval, L"vander", nLhs, argIn);
    }
    indexType n = v->elementCount();
    return one(GpuStructuredMatrix(GpuStructuredKind::Vander, v, nullptr, n, n));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayDiagBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    GpuArrayObject* a = AsGpuArrayObject(argIn.empty() ? ArrayOf() : argIn[0]);
    int k = 0;
    if (argIn.size() == 2) {
        if (AsGpuArrayObject(argIn[1]) || !argIn[1].isScalar() || !argIn[1].isNumeric()) {
            return host(eval, L"diag", nLhs, argIn);
        }
        double kd = argIn[1].asDoubleScalar();
        if (kd != (double)(int)kd) {
            return host(eval, L"diag", nLhs, argIn);
        }
        k = (int)kd;
    }
    if (a == nullptr || argIn.size() > 2 || nLhs > 1 || a->isComplex() || a->isRawIntStorage()
        || !GpuIntegerResultType(a, a).empty() || a->dimensions().length() > 2) {
        return host(eval, L"diag", nLhs, argIn);
    }
    Dimensions dims = a->dimensions();
    bool vector = dims.rows() == 1 || dims.columns() == 1;
    if (vector) {
        indexType n = a->elementCount() + (indexType)std::abs(k);
        if (a->elementCount() == 0) {
            n = 0;
        }
        return one(GpuStructuredMatrix(GpuStructuredKind::DiagonalMatrix, a, nullptr, n, n, k));
    }
    indexType rows = dims.rows();
    indexType cols = dims.columns();
    int64_t count = (k >= 0) ? std::min<int64_t>(rows, (int64_t)cols - k)
                             : std::min<int64_t>((int64_t)rows + k, cols);
    count = std::max<int64_t>(count, 0);
    return one(
        GpuStructuredMatrix(GpuStructuredKind::Diagonal, a, nullptr, (indexType)count, 1, k, rows));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayCompanBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf owner;
    GpuArrayObject* u = (argIn.size() == 1) ? realVector(argIn[0], owner) : nullptr;
    if (u == nullptr || nLhs > 1 || u->elementCount() < 2) {
        return host(eval, L"compan", nLhs, argIn);
    }
    // the leading coefficient must be nonzero (the host reports the error)
    ArrayOf first = GatherGpuArray(u);
    if (((const single*)first.dataPointer())[0] == 0.0f) {
        return host(eval, L"compan", nLhs, argIn);
    }
    indexType n = u->elementCount() - 1;
    return one(GpuStructuredMatrix(GpuStructuredKind::Companion, u, nullptr, n, n));
}
//=============================================================================
ArrayOfVector
Nelson::GpuEngineGateway::gpuArrayBlkdiagBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // real single blocks placed along the diagonal, zeros elsewhere
    std::vector<ArrayOf> owners(argIn.size());
    std::vector<GpuArrayObject*> blocks;
    indexType totalColumns = 0;
    for (size_t k = 0; k < argIn.size(); ++k) {
        GpuArrayObject* g = AsGpuArrayObject(argIn[k]);
        if (g == nullptr) {
            const ArrayOf& v = argIn[k];
            if (!(v.dataClass() == NLS_DOUBLE || v.dataClass() == NLS_SINGLE) || v.isComplex()
                || v.isSparse() || !v.is2D()) {
                return host(eval, L"blkdiag", nLhs, argIn);
            }
            owners[k] = CreateGpuArray(v);
            g = AsGpuArrayObject(owners[k]);
        }
        if (g->isComplex() || g->underlyingType() != "single" || !g->dimensions().is2D()) {
            return host(eval, L"blkdiag", nLhs, argIn);
        }
        blocks.push_back(g);
        totalColumns += g->dimensions().columns();
    }
    if (blocks.empty() || nLhs > 1) {
        return host(eval, L"blkdiag", nLhs, argIn);
    }
    std::vector<ArrayOf> rowOwners;
    std::vector<GpuArrayObject*> rows;
    indexType before = 0;
    for (GpuArrayObject* g : blocks) {
        indexType r = g->dimensions().rows();
        indexType c = g->dimensions().columns();
        std::vector<ArrayOf> parts;
        std::vector<GpuArrayObject*> row;
        if (before > 0) {
            parts.push_back(GpuFilled(Dimensions(r, before), 0.0f, "single"));
            row.push_back(AsGpuArrayObject(parts.back()));
        }
        row.push_back(g);
        indexType after = totalColumns - before - c;
        if (after > 0) {
            parts.push_back(GpuFilled(Dimensions(r, after), 0.0f, "single"));
            row.push_back(AsGpuArrayObject(parts.back()));
        }
        rowOwners.push_back(row.size() == 1 ? ArrayOf::handle(CloneGpuArray(g)) : GpuHorzcat(row));
        rows.push_back(AsGpuArrayObject(rowOwners.back()));
        before += c;
    }
    return one(rows.size() == 1 ? rowOwners[0] : GpuVertcat(rows));
}
//=============================================================================
