//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "symrcmBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include <Eigen/Sparse>
#include <algorithm>
#include <queue>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T>
void
appendSparseEdges(const ArrayOf& value, std::vector<std::vector<signedIndexType>>& adjacency)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
            signedIndexType row = it.row();
            if (row == col || it.value() == T(0)) {
                continue;
            }
            adjacency[static_cast<size_t>(row)].push_back(col);
            adjacency[static_cast<size_t>(col)].push_back(row);
        }
    }
}
//=============================================================================
template <class T>
void
appendDenseEdges(const ArrayOf& value, std::vector<std::vector<signedIndexType>>& adjacency)
{
    const T* data = static_cast<const T*>(value.dataPointer());
    for (indexType col = 0; col < value.columns(); ++col) {
        for (indexType row = 0; row < value.rows(); ++row) {
            if (row == col || data[row + col * value.rows()] == T(0)) {
                continue;
            }
            adjacency[static_cast<size_t>(row)].push_back(static_cast<signedIndexType>(col));
            adjacency[static_cast<size_t>(col)].push_back(static_cast<signedIndexType>(row));
        }
    }
}
//=============================================================================
std::vector<std::vector<signedIndexType>>
buildAdjacency(const ArrayOf& value)
{
    std::vector<std::vector<signedIndexType>> adjacency(static_cast<size_t>(value.rows()));
    if (value.isSparse()) {
        switch (value.dataClass()) {
        case NLS_LOGICAL:
            appendSparseEdges<logical>(value, adjacency);
            break;
        case NLS_DOUBLE:
            appendSparseEdges<double>(value, adjacency);
            break;
        case NLS_SINGLE:
            appendSparseEdges<single>(value, adjacency);
            break;
        case NLS_DCOMPLEX:
            appendSparseEdges<doublecomplex>(value, adjacency);
            break;
        case NLS_SCOMPLEX:
            appendSparseEdges<singlecomplex>(value, adjacency);
            break;
        default:
            OverloadRequired("symrcm");
        }
    } else {
        switch (value.dataClass()) {
        case NLS_LOGICAL:
            appendDenseEdges<logical>(value, adjacency);
            break;
        case NLS_SINGLE:
            appendDenseEdges<single>(value, adjacency);
            break;
        case NLS_DOUBLE:
            appendDenseEdges<double>(value, adjacency);
            break;
        case NLS_SCOMPLEX:
            appendDenseEdges<singlecomplex>(value, adjacency);
            break;
        case NLS_DCOMPLEX:
            appendDenseEdges<doublecomplex>(value, adjacency);
            break;
        default:
            OverloadRequired("symrcm");
        }
    }
    for (auto& neighbors : adjacency) {
        std::sort(neighbors.begin(), neighbors.end());
        neighbors.erase(std::unique(neighbors.begin(), neighbors.end()), neighbors.end());
    }
    return adjacency;
}
//=============================================================================
signedIndexType
nextStartVertex(
    const std::vector<std::vector<signedIndexType>>& adjacency, const std::vector<bool>& visited)
{
    for (signedIndexType k = 0; k < static_cast<signedIndexType>(adjacency.size()); ++k) {
        if (!visited[static_cast<size_t>(k)]) {
            return k;
        }
    }
    return -1;
}
//=============================================================================
signedIndexType
lowestDegreeVertexInComponent(const std::vector<std::vector<signedIndexType>>& adjacency,
    const std::vector<bool>& visited, signedIndexType seed)
{
    signedIndexType best = seed;
    std::vector<bool> seen(adjacency.size(), false);
    std::queue<signedIndexType> pending;
    seen[static_cast<size_t>(seed)] = true;
    pending.push(seed);
    while (!pending.empty()) {
        signedIndexType vertex = pending.front();
        pending.pop();
        if (adjacency[static_cast<size_t>(vertex)].size()
                < adjacency[static_cast<size_t>(best)].size()
            || (adjacency[static_cast<size_t>(vertex)].size()
                    == adjacency[static_cast<size_t>(best)].size()
                && vertex < best)) {
            best = vertex;
        }
        for (signedIndexType neighbor : adjacency[static_cast<size_t>(vertex)]) {
            if (!visited[static_cast<size_t>(neighbor)] && !seen[static_cast<size_t>(neighbor)]) {
                seen[static_cast<size_t>(neighbor)] = true;
                pending.push(neighbor);
            }
        }
    }
    return best;
}
//=============================================================================
std::vector<signedIndexType>
reverseCuthillMckee(const std::vector<std::vector<signedIndexType>>& adjacency)
{
    std::vector<signedIndexType> permutation;
    permutation.reserve(adjacency.size());
    std::vector<bool> visited(adjacency.size(), false);
    while (permutation.size() < adjacency.size()) {
        signedIndexType start = nextStartVertex(adjacency, visited);
        if (start < 0) {
            break;
        }
        start = lowestDegreeVertexInComponent(adjacency, visited, start);
        std::vector<signedIndexType> component;
        std::queue<signedIndexType> pending;
        visited[static_cast<size_t>(start)] = true;
        pending.push(start);
        while (!pending.empty()) {
            signedIndexType vertex = pending.front();
            pending.pop();
            component.push_back(vertex);
            std::vector<signedIndexType> neighbors = adjacency[static_cast<size_t>(vertex)];
            std::sort(neighbors.begin(), neighbors.end(),
                [&adjacency](signedIndexType left, signedIndexType right) {
                    size_t leftDegree = adjacency[static_cast<size_t>(left)].size();
                    size_t rightDegree = adjacency[static_cast<size_t>(right)].size();
                    if (leftDegree != rightDegree) {
                        return leftDegree < rightDegree;
                    }
                    return left < right;
                });
            for (signedIndexType neighbor : neighbors) {
                if (!visited[static_cast<size_t>(neighbor)]) {
                    visited[static_cast<size_t>(neighbor)] = true;
                    pending.push(neighbor);
                }
            }
        }
        permutation.insert(permutation.end(), component.rbegin(), component.rend());
    }
    return permutation;
}
//=============================================================================
ArrayOf
permutationToArray(const std::vector<signedIndexType>& permutation)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, permutation.size()));
    for (size_t k = 0; k < permutation.size(); ++k) {
        data[k] = static_cast<double>(permutation[k] + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, permutation.size()), data, false);
}
//=============================================================================
bool
isFloatingPointOrLogicalClass(const ArrayOf& value)
{
    switch (value.dataClass()) {
    case NLS_LOGICAL:
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
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::symrcmBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].is2D()) {
        Error(L"Nelson:validators:mustBeMatrix", L"%s must be a 2-D matrix.", L"Value");
    }
    if (!argIn[0].isNumeric() && !argIn[0].isLogical()) {
        Error(L"Nelson:types:numericOrLogical2DExpected",
            L"A numeric or logical 2-D matrix expected.");
    }
    if (!isFloatingPointOrLogicalClass(argIn[0])) {
        Error(L"Nelson:types:floatingPointOrLogical2DExpected",
            L"A floating-point or logical 2-D matrix expected.");
    }
    if (argIn[0].rows() != argIn[0].columns()) {
        Error(L"Nelson:sparse:squareMatrixExpected", L"Matrix must be square.");
    }
    return permutationToArray(reverseCuthillMckee(buildAdjacency(argIn[0])));
}
//=============================================================================
