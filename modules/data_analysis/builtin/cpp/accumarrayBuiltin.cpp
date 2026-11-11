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
#include <cmath>
#include <complex>
#include <cstring>
#include <numeric>
#include <vector>
#include "accumarrayBuiltin.hpp"
#include "objectArgumentOverload.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FunctionDef.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Decomplexify.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
isComplexClass(NelsonType c)
{
    return c == NLS_DCOMPLEX || c == NLS_SCOMPLEX;
}
//=============================================================================
NelsonType
realClassOf(NelsonType c)
{
    if (c == NLS_DCOMPLEX) {
        return NLS_DOUBLE;
    }
    if (c == NLS_SCOMPLEX) {
        return NLS_SINGLE;
    }
    return c;
}
//=============================================================================
NelsonType
complexClassOf(NelsonType c)
{
    return (realClassOf(c) == NLS_SINGLE) ? NLS_SCOMPLEX : NLS_DCOMPLEX;
}
//=============================================================================
// numeric (integer, float, complex), logical or char
bool
isAccumulableClass(NelsonType c)
{
    switch (c) {
    case NLS_LOGICAL:
    case NLS_CHAR:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        return true;
    default:
        return false;
    }
}
//=============================================================================
void
errorSubscripts()
{
    Error(L"Nelson:data_analysis:subscriptsPositiveIntegers",
        L"Subscripts must be positive integers.");
}
//=============================================================================
// one subscript to 1-based index, rejecting zero, fractions, NaN and Inf
inline indexType
subscriptValue(double s)
{
    if (!(s >= 1.0 && s <= 9007199254740992.0)) {
        errorSubscripts();
    }
    indexType si = (indexType)s;
    if ((double)si != s) {
        errorSubscripts();
    }
    return si;
}
//=============================================================================
// subscripts as an N x K double matrix (a cell holds one column per dimension)
ArrayOf
subscriptsMatrix(const ArrayOf& arg, indexType& N, indexType& K)
{
    auto checkClass = [](const ArrayOf& A) {
        NelsonType c = A.dataClass();
        if (!isAccumulableClass(c) || c == NLS_LOGICAL || c == NLS_CHAR || isComplexClass(c)
            || A.isSparse()) {
            Error(L"Nelson:data_analysis:accumarraySubscriptsType",
                L"Subscripts must be a real numeric matrix or a cell array of vectors.");
        }
    };
    if (arg.isCell()) {
        K = arg.elementCount();
        if (K == 0) {
            Error(L"Nelson:data_analysis:accumarraySubscriptsType",
                L"Subscripts must be a real numeric matrix or a cell array of vectors.");
        }
        const ArrayOf* cols = (const ArrayOf*)arg.dataPointer();
        N = cols[0].elementCount();
        double* out = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, N * K);
        ArrayOf res(NLS_DOUBLE, Dimensions(N, K), out);
        for (indexType j = 0; j < K; ++j) {
            ArrayOf c = cols[j];
            checkClass(c);
            if (c.elementCount() != N) {
                Error(L"Nelson:data_analysis:accumarraySubscriptCellsLength",
                    L"All subscript vectors in the cell array must have the same length.");
            }
            c.promoteType(NLS_DOUBLE);
            const double* pc = (const double*)c.dataPointer();
            std::copy(pc, pc + N, out + j * N);
        }
        return res;
    }
    checkClass(arg);
    Dimensions d = arg.dimensions();
    if (d.length() > 2 || d.columns() == 0) {
        Error(L"Nelson:data_analysis:accumarraySubscriptsShape",
            L"Subscripts must be a non-empty column vector or a two-dimensional matrix.");
    }
    N = d.rows();
    K = d.columns();
    ArrayOf res(arg);
    if (res.dataClass() != NLS_DOUBLE) {
        res.promoteType(NLS_DOUBLE);
    }
    return res;
}
//=============================================================================
// promote to a class, going through the real class when a real array becomes complex
void
promoteTo(ArrayOf& A, NelsonType c)
{
    if (A.dataClass() == c) {
        return;
    }
    if (isComplexClass(c) && !isComplexClass(A.dataClass())) {
        A.promoteType(realClassOf(c));
    }
    A.promoteType(c);
}
//=============================================================================
// default accumulation: sum into T, fill applied to untouched cells
template <class T>
void
sumAccumulate(T* out, indexType outTotal, const T* pv, bool valScalar, const T& scalarValue,
    const std::vector<indexType>& lin, const T& fill)
{
    const indexType N = (indexType)lin.size();
    if (fill == T(0)) {
        if (valScalar) {
            for (indexType i = 0; i < N; ++i) {
                out[lin[i]] += scalarValue;
            }
        } else {
            for (indexType i = 0; i < N; ++i) {
                out[lin[i]] += pv[i];
            }
        }
        return;
    }
    std::vector<char> touched(outTotal, 0);
    for (indexType i = 0; i < N; ++i) {
        indexType L = lin[i];
        out[L] += valScalar ? scalarValue : pv[i];
        touched[L] = 1;
    }
    for (indexType k = 0; k < outTotal; ++k) {
        if (!touched[k]) {
            out[k] = fill;
        }
    }
}
//=============================================================================
template <class T>
ArrayOf
sumPath(NelsonType outClass, const Dimensions& outDims, indexType outTotal, const ArrayOf& vals,
    bool valScalar, const std::vector<indexType>& lin, const ArrayOf& fillValue)
{
    T* out = (T*)ArrayOf::allocateArrayOf(outClass, outTotal, stringVector(), true);
    ArrayOf result(outClass, outDims, out);
    ArrayOf v(vals);
    promoteTo(v, outClass);
    const T* pv = (const T*)v.dataPointer();
    T scalarValue = valScalar ? pv[0] : T(0);
    T fill = T(0);
    if (!fillValue.isEmpty()) {
        ArrayOf f(fillValue);
        promoteTo(f, outClass);
        fill = ((const T*)f.dataPointer())[0];
    }
    sumAccumulate<T>(out, outTotal, valScalar ? nullptr : pv, valScalar, scalarValue, lin, fill);
    return result;
}
//=============================================================================
void
errorFillClass()
{
    Error(L"Nelson:data_analysis:accumarrayFillClass",
        L"The fill value must have the same class as the accumulated result.");
}
//=============================================================================
void
checkFillClass(const ArrayOf& fillValue, NelsonType outClass)
{
    if (!fillValue.isEmpty() && realClassOf(fillValue.dataClass()) != realClassOf(outClass)) {
        errorFillClass();
    }
}
//=============================================================================
// output array of outClass whose every element holds the fill value (0 by default)
ArrayOf
filledOutput(
    NelsonType outClass, const Dimensions& outDims, indexType outTotal, const ArrayOf& fillValue)
{
    if (!fillValue.isEmpty() && fillValue.isComplex()) {
        outClass = complexClassOf(outClass);
    }
    void* p = ArrayOf::allocateArrayOf(outClass, outTotal, stringVector(), true);
    ArrayOf out(outClass, outDims, p);
    if (!fillValue.isEmpty()) {
        ArrayOf f(fillValue);
        promoteTo(f, outClass);
        const indexType esz = out.elementSize();
        const char* src = (const char*)f.dataPointer();
        char* dst = (char*)p;
        bool allZero = true;
        for (indexType b = 0; b < esz; ++b) {
            allZero = allZero && (src[b] == 0);
        }
        if (!allZero) {
            for (indexType k = 0; k < outTotal; ++k) {
                std::memcpy(dst + k * esz, src, esz);
            }
        }
    }
    return out;
}
//=============================================================================
void
makeSparseOutput(ArrayOf& result, const Dimensions& outDims)
{
    NelsonType c = realClassOf(result.dataClass());
    if (c != NLS_DOUBLE && c != NLS_SINGLE) {
        Error(L"Nelson:data_analysis:accumarraySparseResultClass",
            L"Sparse output requires double or single results.");
    }
    result.makeSparse();
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::accumarrayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector overloaded;
    if (callObjectArgumentOverload(eval, nLhs, argIn, "accumarray", overloaded)) {
        return overloaded;
    }
    ArrayOfVector retval;
    nargincheck(argIn, 2, 6);
    nargoutcheck(nLhs, 0, 1);

    bool sparseOutput = false;
    bool sparseFlagGiven = false;
    if (argIn.size() >= 6 && !argIn[5].isEmpty()) {
        sparseFlagGiven = true;
        const ArrayOf& sparseFlag = argIn[5];
        NelsonType c = sparseFlag.dataClass();
        bool valid = sparseFlag.isScalar() && !sparseFlag.isSparse()
            && (c == NLS_LOGICAL || (isAccumulableClass(c) && c != NLS_CHAR && !isComplexClass(c)));
        double flag = valid ? sparseFlag.asDoubleScalar() : -1;
        if (!valid || (flag != 0 && flag != 1)) {
            Error(L"Nelson:data_analysis:sparseFlagLogicalScalar",
                L"The sparse flag must be a logical scalar.");
        }
        sparseOutput = flag == 1;
    }

    indexType N = 0;
    indexType K = 1;
    ArrayOf subsD = subscriptsMatrix(argIn[0], N, K);
    const double* ps = (const double*)subsD.dataPointer();

    ArrayOf vals = argIn[1];
    if (!isAccumulableClass(vals.dataClass())) {
        Error(L"Nelson:data_analysis:accumarrayValuesType",
            L"Values must be numeric, logical or char.");
    }
    // sparse values give a sparse result
    if (vals.isSparse()) {
        if (sparseFlagGiven && !sparseOutput) {
            Error(L"Nelson:data_analysis:accumarraySparseValuesFullOutput",
                L"Sparse values require a sparse output.");
        }
        sparseOutput = true;
        vals.makeDense();
    }
    bool valScalar = vals.isScalar();
    if (!valScalar) {
        Dimensions vd = vals.dimensions();
        bool isVec = vd.length() == 2 && (vd.rows() == 1 || vd.columns() == 1);
        if (!isVec || vals.elementCount() != N) {
            Error(L"Nelson:data_analysis:numberValuesMatchNumberSubscript",
                L"The number of values must match the number of subscript rows.");
        }
    }
    const NelsonType valClass = vals.dataClass();

    bool useSum = true;
    ArrayOf funArg;
    if (argIn.size() >= 4 && !argIn[3].isEmpty()) {
        useSum = false;
        funArg = argIn[3];
        if (!funArg.isFunctionHandle()) {
            Error(L"Nelson:data_analysis:accumulationFunctionFunctionHandle",
                L"%s must be a function handle.", L"The accumulation function");
        }
    }

    // an empty fill ([] or {}) keeps the default fill
    ArrayOf fillValue = ArrayOf::empty();
    if (argIn.size() >= 5 && !argIn[4].isEmpty()) {
        fillValue = argIn[4];
        if (!fillValue.isScalar() || fillValue.isSparse()) {
            Error(L"Nelson:data_analysis:accumarrayFillScalar",
                L"The fill value must be a full scalar.");
        }
        if (!fillValue.isCell() && !isAccumulableClass(fillValue.dataClass())) {
            errorFillClass();
        }
    }
    const bool fillIsCell = !fillValue.isEmpty() && fillValue.isCell();

    if (sparseOutput) {
        NelsonType rc = realClassOf(valClass);
        if (rc != NLS_DOUBLE && rc != NLS_SINGLE) {
            Error(L"Nelson:data_analysis:accumarraySparseValuesClass",
                L"Sparse output requires double or single values.");
        }
        if (!fillValue.isEmpty()) {
            NelsonType fc = realClassOf(fillValue.dataClass());
            bool zeroFill = (fc == NLS_DOUBLE || fc == NLS_SINGLE)
                && fillValue.asDoubleComplexScalar() == std::complex<double>(0, 0);
            if (!zeroFill) {
                Error(L"Nelson:data_analysis:accumarraySparseFill",
                    L"The fill value must be zero for sparse output.");
            }
        }
    }

    bool hasSize = (argIn.size() >= 3 && !argIn[2].isEmpty());
    std::vector<indexType> outsz;
    std::vector<indexType> lin(N);
    if (K == 1 && !hasSize) {
        // column subscripts: one pass gives the extent and the 0-based indices
        indexType mx = 0;
        for (indexType i = 0; i < N; ++i) {
            indexType si = subscriptValue(ps[i]);
            lin[i] = si - 1;
            if (si > mx) {
                mx = si;
            }
        }
        outsz.push_back(mx);
        outsz.push_back(1);
    } else {
        if (hasSize) {
            ArrayOf szD(argIn[2]);
            NelsonType zc = szD.dataClass();
            bool validSz = szD.isVector() && !szD.isSparse() && isAccumulableClass(zc)
                && zc != NLS_LOGICAL && zc != NLS_CHAR && !isComplexClass(zc)
                && ((K == 1) ? szD.elementCount() == 2 : szD.elementCount() == K);
            if (validSz) {
                szD.promoteType(NLS_DOUBLE);
                const double* pz = (const double*)szD.dataPointer();
                for (indexType i = 0; i < szD.elementCount(); ++i) {
                    double z = pz[i];
                    if (!(z >= 0 && z <= 9007199254740992.0) || z != std::floor(z)) {
                        validSz = false;
                        break;
                    }
                    outsz.push_back((indexType)z);
                }
                if (K == 1 && validSz && outsz[1] != 1) {
                    validSz = false;
                }
            }
            if (!validSz) {
                Error(L"Nelson:data_analysis:accumarraySizeVector",
                    L"The size must be a vector of nonnegative integers with one entry per "
                    L"subscript column ([n 1] for column subscripts).");
            }
        } else {
            outsz.assign(K, 0);
            for (indexType j = 0; j < K; ++j) {
                indexType mx = 0;
                for (indexType i = 0; i < N; ++i) {
                    indexType si = subscriptValue(ps[j * N + i]);
                    if (si > mx) {
                        mx = si;
                    }
                }
                outsz[j] = mx;
            }
            if (K == 1) {
                outsz.push_back(1);
            }
        }
        for (indexType i = 0; i < N; ++i) {
            indexType idx = 0;
            indexType mult = 1;
            for (indexType j = 0; j < K; ++j) {
                indexType si = subscriptValue(ps[j * N + i]);
                if (si > outsz[j]) {
                    Error(L"Nelson:data_analysis:accumarraySubscriptExceedsSize",
                        L"Subscripts exceed the requested output size.");
                }
                idx += (si - 1) * mult;
                mult *= outsz[j];
            }
            lin[i] = idx;
        }
    }

    indexType outTotal = 1;
    for (indexType d : outsz) {
        outTotal *= d;
    }
    Dimensions outDims(outsz);
    outDims.simplify();
    if (sparseOutput && outDims.length() > 2) {
        Error(L"Nelson:data_analysis:sparseOutputRequiresTwoDimensional",
            L"Sparse output requires a two-dimensional result.");
    }

    if (useSum) {
        // sums run in double, or in single for single values
        NelsonType outClass = (realClassOf(valClass) == NLS_SINGLE) ? NLS_SINGLE : NLS_DOUBLE;
        if (fillIsCell) {
            errorFillClass();
        }
        if (!sparseOutput) {
            checkFillClass(fillValue, outClass);
        }
        bool cplx = isComplexClass(valClass) || (!fillValue.isEmpty() && fillValue.isComplex());
        ArrayOf result;
        if (outClass == NLS_SINGLE) {
            result = cplx
                ? sumPath<std::complex<float>>(
                      NLS_SCOMPLEX, outDims, outTotal, vals, valScalar, lin, fillValue)
                : sumPath<float>(NLS_SINGLE, outDims, outTotal, vals, valScalar, lin, fillValue);
        } else {
            result = cplx
                ? sumPath<std::complex<double>>(
                      NLS_DCOMPLEX, outDims, outTotal, vals, valScalar, lin, fillValue)
                : sumPath<double>(NLS_DOUBLE, outDims, outTotal, vals, valScalar, lin, fillValue);
        }
        if (cplx) {
            result = decomplexify(result);
        }
        if (sparseOutput) {
            makeSparseOutput(result, outDims);
        }
        retval << result;
        return retval;
    }

    // function path: groups gathered in subscript order, values keep their class
    function_handle fh = funArg.asFunctionHandle();
    FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    if (funcDef == nullptr) {
        Error(L"Nelson:data_analysis:accumulationFunctionFunctionHandle",
            L"%s must be a function handle.", L"The accumulation function");
    }
    // @max and @min on char values keep the char class
    bool keepCharClass = false;
    if (valClass == NLS_CHAR) {
        AnonymousMacroFunctionDef* named
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        if (named->isFunctionHandle()) {
            std::string name = named->getContent();
            keepCharClass = (name == "max" || name == "min");
        }
    }
    std::vector<indexType> order(N);
    if (outTotal <= 2 * N + 1024) {
        // counting sort keeps the subscript order stable in linear time
        std::vector<indexType> start(outTotal + 1, 0);
        for (indexType i = 0; i < N; ++i) {
            start[lin[i] + 1]++;
        }
        for (indexType k = 0; k < outTotal; ++k) {
            start[k + 1] += start[k];
        }
        for (indexType i = 0; i < N; ++i) {
            order[start[lin[i]]++] = i;
        }
    } else {
        std::iota(order.begin(), order.end(), (indexType)0);
        std::stable_sort(
            order.begin(), order.end(), [&](indexType a, indexType b) { return lin[a] < lin[b]; });
    }

    const indexType valElementSize = vals.elementSize();
    const char* valBytes = (const char*)vals.dataPointer();
    ArrayOf out;
    char* outBytes = nullptr;
    ArrayOf* cellPtr = nullptr;
    indexType outElementSize = 0;
    NelsonType outRealClass = NLS_DOUBLE;
    bool cellMode = false;
    bool modeDecided = false;
    indexType i = 0;
    while (i < N) {
        indexType L = lin[order[i]];
        indexType j = i;
        while (j < N && lin[order[j]] == L) {
            ++j;
        }
        indexType groupSize = j - i;
        char* gp = (char*)ArrayOf::allocateArrayOf(valClass, groupSize);
        for (indexType g = 0; g < groupSize; ++g) {
            indexType src = valScalar ? 0 : order[i + g];
            std::memcpy(gp + g * valElementSize, valBytes + src * valElementSize, valElementSize);
        }
        ArrayOfVector callInputs;
        callInputs.push_back(ArrayOf(valClass, Dimensions(groupSize, 1), gp));
        ArrayOfVector result = funcDef->evaluateFunction(eval, callInputs, 1);
        if (result.empty() || result[0].elementCount() != 1) {
            Error(L"Nelson:data_analysis:accumulationFunctionReturnedNonScalar",
                L"The accumulation function returned a non-scalar value.");
        }
        ArrayOf r0 = result[0];
        if (r0.isSparse()) {
            r0.makeDense();
        }
        if (keepCharClass && r0.dataClass() == NLS_DOUBLE) {
            r0.promoteType(NLS_CHAR);
        }
        bool isCellResult = r0.isCell();
        if (!isCellResult && !isAccumulableClass(r0.dataClass())) {
            Error(L"Nelson:data_analysis:accumarrayFunctionResultClass",
                L"The accumulation function must return a numeric, logical, char or cell value.");
        }
        if (!modeDecided) {
            cellMode = isCellResult;
            if (cellMode) {
                if (sparseOutput) {
                    Error(L"Nelson:data_analysis:accumarraySparseResultClass",
                        L"Sparse output requires double or single results.");
                }
                if (!fillValue.isEmpty() && !fillIsCell) {
                    errorFillClass();
                }
                ArrayOf* cellOut = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, outTotal);
                ArrayOf content
                    = fillIsCell ? ((const ArrayOf*)fillValue.dataPointer())[0] : ArrayOf::empty();
                for (indexType k = 0; k < outTotal; ++k) {
                    cellOut[k] = content;
                }
                out = ArrayOf(NLS_CELL_ARRAY, outDims, cellOut);
                cellPtr = cellOut;
            } else {
                outRealClass = realClassOf(r0.dataClass());
                if (fillIsCell
                    || (!sparseOutput && !fillValue.isEmpty()
                        && realClassOf(fillValue.dataClass()) != outRealClass)) {
                    errorFillClass();
                }
                out = filledOutput(outRealClass, outDims, outTotal, fillValue);
                outBytes = (char*)out.readWriteDataPointer();
                outElementSize = out.elementSize();
            }
            modeDecided = true;
        } else if (cellMode != isCellResult
            || (!cellMode && realClassOf(r0.dataClass()) != outRealClass)) {
            Error(L"Nelson:data_analysis:accumarrayFunctionResultsClass",
                L"The accumulation function must return values of a single class.");
        }
        if (cellMode) {
            cellPtr[L] = ((const ArrayOf*)r0.dataPointer())[0];
        } else {
            if (r0.isComplex() && !out.isComplex()) {
                out.promoteType(complexClassOf(outRealClass));
                outBytes = (char*)out.readWriteDataPointer();
                outElementSize = out.elementSize();
            }
            promoteTo(r0, out.dataClass());
            std::memcpy(outBytes + L * outElementSize, r0.dataPointer(), outElementSize);
        }
        i = j;
    }

    if (cellMode) {
        retval << out;
        return retval;
    }
    // no group: the function is never called, the result takes the fill class (double by default)
    if (!modeDecided) {
        if (fillIsCell) {
            ArrayOf* cellOut = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, outTotal);
            ArrayOf content = ((const ArrayOf*)fillValue.dataPointer())[0];
            for (indexType k = 0; k < outTotal; ++k) {
                cellOut[k] = content;
            }
            retval << ArrayOf(NLS_CELL_ARRAY, outDims, cellOut);
            return retval;
        }
        NelsonType emptyClass
            = fillValue.isEmpty() || sparseOutput ? NLS_DOUBLE : realClassOf(fillValue.dataClass());
        out = filledOutput(emptyClass, outDims, outTotal, fillValue);
    }
    if (out.isComplex()) {
        out = decomplexify(out);
    }
    if (sparseOutput) {
        makeSparseOutput(out, outDims);
    }
    retval << out;
    return retval;
}
//=============================================================================
