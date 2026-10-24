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
#include <numeric>
#include <vector>
#include "accumarrayBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FunctionDef.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::accumarrayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 6);
    nargoutcheck(nLhs, 0, 1);

    bool sparseOutput = false;
    if (argIn.size() >= 6 && !argIn[5].isEmpty()) {
        ArrayOf sparseFlag(argIn[5]);
        if (!sparseFlag.isScalar()) {
            Error(L"Nelson:data_analysis:sparseFlagLogicalScalar",
                L"The sparse flag must be a logical scalar.");
        }
        sparseOutput = sparseFlag.asDoubleScalar() != 0;
    }

    ArrayOf subsD = argIn[0];
    if (subsD.dataClass() != NLS_DOUBLE) {
        subsD.promoteType(NLS_DOUBLE);
    }
    indexType N = 0;
    indexType K = 1;
    Dimensions sd = subsD.dimensions();
    if (subsD.isVector()) {
        N = subsD.elementCount();
        K = 1;
    } else {
        N = sd[0];
        K = sd[1];
    }
    const double* ps = (const double*)subsD.dataPointer();

    ArrayOf valD = argIn[1];
    if (valD.dataClass() != NLS_DOUBLE) {
        valD.promoteType(NLS_DOUBLE);
    }
    bool valScalar = valD.isScalar();
    double valScalarValue = valScalar ? valD.asDoubleScalar() : 0.0;
    const double* pv = valScalar ? nullptr : (const double*)valD.dataPointer();
    if (!valScalar && valD.elementCount() != N) {
        Error(L"Nelson:data_analysis:numberValuesMatchNumberSubscript",
            L"The number of values must match the number of subscript rows.");
    }

    bool useSum = true;
    ArrayOf funArg;
    if (argIn.size() >= 4 && !argIn[3].isEmpty()) {
        useSum = false;
        funArg = argIn[3];
    }
    // The fill value is numeric for a numeric output, or a scalar cell whose
    // content fills untouched cells when the output is a cell array. An empty
    // cell ({}) behaves like an unspecified fill: untouched cells hold [].
    double fillval = 0.0;
    bool fillIsCell = (argIn.size() >= 5 && argIn[4].isCell());
    ArrayOf cellFillContent = ArrayOf::empty();
    if (fillIsCell) {
        if (!argIn[4].isEmpty()) {
            const ArrayOf* fillElems = (const ArrayOf*)argIn[4].dataPointer();
            cellFillContent = fillElems[0];
        }
    } else if (argIn.size() >= 5 && !argIn[4].isEmpty()) {
        fillval = argIn[4].asDoubleScalar();
    }

    bool hasSize = (argIn.size() >= 3 && !argIn[2].isEmpty());
    std::vector<indexType> outsz;
    std::vector<indexType> lin(N);
    if (K == 1 && !hasSize) {
        // Common vector-subscript case: one pass computes both the extent and
        // the (0-based) linear indices, validating with a cast (no floor call).
        indexType mx = 0;
        for (indexType i = 0; i < N; ++i) {
            double s = ps[i];
            indexType si = (indexType)s;
            if ((double)si != s || si < 1) {
                Error(L"Nelson:data_analysis:subscriptsPositiveIntegers",
                    L"Subscripts must be positive integers.");
            }
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
            szD.promoteType(NLS_DOUBLE);
            const double* pz = (const double*)szD.dataPointer();
            for (indexType i = 0; i < szD.elementCount(); ++i) {
                outsz.push_back((indexType)pz[i]);
            }
        } else {
            outsz.assign(K, 0);
            for (indexType j = 0; j < K; ++j) {
                indexType mx = 0;
                for (indexType i = 0; i < N; ++i) {
                    double s = ps[j * N + i];
                    indexType si = (indexType)s;
                    if ((double)si != s || si < 1) {
                        Error(L"Nelson:data_analysis:subscriptsPositiveIntegers",
                            L"Subscripts must be positive integers.");
                    }
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
                double s = ps[j * N + i];
                indexType si = (indexType)s;
                if ((double)si != s || si < 1) {
                    Error(L"Nelson:data_analysis:subscriptsPositiveIntegers",
                        L"Subscripts must be positive integers.");
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

    if (useSum) {
        double* out = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outTotal);
        for (indexType i = 0; i < outTotal; ++i) {
            out[i] = fillval;
        }
        if (fillval == 0.0) {
            for (indexType i = 0; i < N; ++i) {
                out[lin[i]] += valScalar ? valScalarValue : pv[i];
            }
        } else {
            std::vector<char> touched(outTotal, 0);
            for (indexType i = 0; i < N; ++i) {
                indexType L = lin[i];
                double v = valScalar ? valScalarValue : pv[i];
                if (!touched[L]) {
                    out[L] = v;
                    touched[L] = 1;
                } else {
                    out[L] += v;
                }
            }
        }
        ArrayOf result(NLS_DOUBLE, outDims, out);
        if (sparseOutput) {
            if (outDims.length() > 2) {
                Error(L"Nelson:data_analysis:sparseOutputRequiresTwoDimensional",
                    L"Sparse output requires a two-dimensional result.");
            }
            result.makeSparse();
        }
        retval << result;
        return retval;
    }

    // Function-handle accumulation. Groups are gathered in subscript order and
    // reduced one at a time. The output stays a plain double array while the
    // function returns real scalars; if the function returns a cell (e.g.
    // @(x){x}) the output becomes a cell array holding each group's cell
    // content, and untouched cells hold the fill content.
    if (!funArg.isFunctionHandle()) {
        Error(L"Nelson:data_analysis:accumulationFunctionFunctionHandle",
            L"%s must be a function handle.", L"The accumulation function");
    }
    function_handle fh = funArg.asFunctionHandle();
    FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
    if (funcDef == nullptr) {
        Error(L"Nelson:data_analysis:accumulationFunctionFunctionHandle",
            L"%s must be a function handle.", L"The accumulation function");
    }
    std::vector<indexType> order(N);
    std::iota(order.begin(), order.end(), (indexType)0);
    std::stable_sort(
        order.begin(), order.end(), [&](indexType a, indexType b) { return lin[a] < lin[b]; });

    double* out = nullptr;
    ArrayOf* cellOut = nullptr;
    bool cellMode = false;
    bool modeDecided = false;
    indexType i = 0;
    while (i < N) {
        indexType L = lin[order[i]];
        indexType j = i;
        std::vector<double> group;
        while (j < N && lin[order[j]] == L) {
            group.push_back(valScalar ? valScalarValue : pv[order[j]]);
            ++j;
        }
        double* gp = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, (indexType)group.size());
        for (size_t g = 0; g < group.size(); ++g) {
            gp[g] = group[g];
        }
        ArrayOf groupArray(NLS_DOUBLE, Dimensions((indexType)group.size(), 1), gp);
        ArrayOfVector callInputs;
        callInputs.push_back(groupArray);
        ArrayOfVector result = funcDef->evaluateFunction(eval, callInputs, 1);
        if (result.empty()) {
            i = j;
            continue;
        }
        ArrayOf& r0 = result[0];
        if (!modeDecided) {
            cellMode = r0.isCell();
            if (cellMode) {
                cellOut = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, outTotal);
                for (indexType k = 0; k < outTotal; ++k) {
                    cellOut[k] = cellFillContent;
                }
            } else {
                out = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outTotal);
                for (indexType k = 0; k < outTotal; ++k) {
                    out[k] = fillval;
                }
            }
            modeDecided = true;
        }
        if (cellMode) {
            if (!r0.isCell() || !r0.isScalar()) {
                Error(L"Nelson:data_analysis:accumulationFunctionReturnedNonScalar",
                    L"The accumulation function returned a non-scalar value.");
            }
            const ArrayOf* inner = (const ArrayOf*)r0.dataPointer();
            cellOut[L] = inner[0];
        } else {
            out[L] = r0.asDoubleScalar();
        }
        i = j;
    }

    // With no processed groups the function is never called, so a cell return
    // is never observed: the output stays a double array (an unset cell fill
    // does not by itself force a cell output).
    if (cellMode) {
        retval << ArrayOf(NLS_CELL_ARRAY, outDims, cellOut);
        return retval;
    }

    if (out == nullptr) {
        out = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, outTotal);
        for (indexType k = 0; k < outTotal; ++k) {
            out[k] = fillval;
        }
    }
    ArrayOf result(NLS_DOUBLE, outDims, out);
    if (sparseOutput) {
        if (outDims.length() > 2) {
            Error(L"Nelson:data_analysis:sparseOutputRequiresTwoDimensional",
                L"Sparse output requires a two-dimensional result.");
        }
        result.makeSparse();
    }
    retval << result;
    return retval;
}
//=============================================================================
