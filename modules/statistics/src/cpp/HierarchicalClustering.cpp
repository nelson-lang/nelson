//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StatisticsClustering.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "StatisticsDistances.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class LinkageMethod
{
    Single,
    Complete,
    Average,
    Weighted,
    Centroid,
    Median,
    Ward
};
//=============================================================================
struct LinkageState
{
    std::vector<double> distances;
    std::vector<indexType> sizes;
    std::vector<indexType> ids;
    std::vector<bool> active;
    indexType activeCount;
};
//=============================================================================
static bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static std::wstring
lowerText(const ArrayOf& value)
{
    std::wstring text = value.asWideString();
    std::transform(text.begin(), text.end(), text.begin(), [](wchar_t c) { return towlower(c); });
    return text;
}
//=============================================================================
static bool
isRealNumericMatrix(const ArrayOf& value)
{
    return value.isNumeric() && !value.isSparse() && !value.isComplex()
        && value.dimensions().length() == 2;
}
//=============================================================================
static ArrayOf
asDoubleMatrix(const ArrayOf& value, const std::wstring& message)
{
    if (!isRealNumericMatrix(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    ArrayOf copy = value;
    copy.promoteType(NLS_DOUBLE);
    return copy;
}
//=============================================================================
static std::wstring
matchText(const ArrayOf& value, const std::vector<std::wstring>& names, const std::wstring& message)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    std::wstring text = lowerText(value);
    std::vector<std::wstring> matches;
    for (const auto& name : names) {
        if (name.rfind(text, 0) == 0) {
            matches.push_back(name);
        }
    }
    if (matches.empty() || matches.size() > 1) {
        Error(L"Nelson:statistics:invalidValue", message);
    }
    return matches[0];
}
//=============================================================================
static LinkageMethod
linkageMethod(const ArrayOf& value)
{
    std::wstring name = matchText(value,
        { L"single", L"complete", L"average", L"weighted", L"centroid", L"median", L"ward" },
        resolveMessageTemplate(
            L"Nelson:statistics:invalidLinkageMethod", L"Invalid linkage method."));
    if (name == L"complete") {
        return LinkageMethod::Complete;
    }
    if (name == L"average") {
        return LinkageMethod::Average;
    }
    if (name == L"weighted") {
        return LinkageMethod::Weighted;
    }
    if (name == L"centroid") {
        return LinkageMethod::Centroid;
    }
    if (name == L"median") {
        return LinkageMethod::Median;
    }
    if (name == L"ward") {
        return LinkageMethod::Ward;
    }
    return LinkageMethod::Single;
}
//=============================================================================
static indexType
squareSizeFromVector(indexType count)
{
    double n = (1.0 + std::sqrt(1.0 + 8.0 * static_cast<double>(count))) / 2.0;
    indexType size = static_cast<indexType>(std::floor(n + 0.5));
    if (size * (size - 1) / 2 != count) {
        Error("Nelson:statistics:invalidDistanceVectorLength",
            "Distance vector length must be a binomial coefficient.");
    }
    return size;
}
//=============================================================================
static double&
distanceAt(std::vector<double>& distances, indexType n, indexType a, indexType b)
{
    return distances[a + b * n];
}
//=============================================================================
static double
distanceAt(const std::vector<double>& distances, indexType n, indexType a, indexType b)
{
    return distances[a + b * n];
}
//=============================================================================
static LinkageState
initialState(const double* condensed, indexType n)
{
    LinkageState state;
    state.distances.assign(n * n, std::numeric_limits<double>::infinity());
    state.sizes.assign(n, 1);
    state.ids.resize(n);
    state.active.assign(n, true);
    state.activeCount = n;
    std::iota(state.ids.begin(), state.ids.end(), 1);
    indexType k = 0;
    for (indexType c = 1; c < n; ++c) {
        for (indexType r = 0; r < c; ++r) {
            distanceAt(state.distances, n, r, c) = condensed[k];
            distanceAt(state.distances, n, c, r) = condensed[k];
            ++k;
        }
    }
    return state;
}
//=============================================================================
static void
nearestPair(const LinkageState& state, indexType n, indexType& a, indexType& b, double& best)
{
    best = std::numeric_limits<double>::infinity();
    a = 0;
    b = 0;
    for (indexType j = 1; j < n; ++j) {
        if (!state.active[j]) {
            continue;
        }
        for (indexType i = 0; i < j; ++i) {
            if (state.active[i] && distanceAt(state.distances, n, i, j) < best) {
                best = distanceAt(state.distances, n, i, j);
                a = i;
                b = j;
            }
        }
    }
}
//=============================================================================
static double
updatedDistance(LinkageMethod method, double dps, double dqs, double dpq, indexType np,
    indexType nq, indexType ns)
{
    if (method == LinkageMethod::Single) {
        return std::min(dps, dqs);
    }
    if (method == LinkageMethod::Complete) {
        return std::max(dps, dqs);
    }
    if (method == LinkageMethod::Average) {
        return (np * dps + nq * dqs) / static_cast<double>(np + nq);
    }
    if (method == LinkageMethod::Weighted) {
        return 0.5 * (dps + dqs);
    }
    double value = 0;
    if (method == LinkageMethod::Ward) {
        value = ((ns + np) * dps * dps + (ns + nq) * dqs * dqs - ns * dpq * dpq)
            / static_cast<double>(ns + np + nq);
    } else if (method == LinkageMethod::Centroid) {
        double total = static_cast<double>(np + nq);
        value = (np * dps * dps + nq * dqs * dqs) / total - (np * nq * dpq * dpq) / (total * total);
    } else {
        value = 0.5 * dps * dps + 0.5 * dqs * dqs - 0.25 * dpq * dpq;
    }
    return std::sqrt(std::max(0.0, value));
}
//=============================================================================
static void
mergePair(LinkageState& state, indexType n, indexType a, indexType b, LinkageMethod method,
    indexType newId)
{
    indexType np = state.sizes[a];
    indexType nq = state.sizes[b];
    double dpq = distanceAt(state.distances, n, a, b);
    for (indexType s = 0; s < n; ++s) {
        if (!state.active[s] || s == a || s == b) {
            continue;
        }
        double d = updatedDistance(method, distanceAt(state.distances, n, a, s),
            distanceAt(state.distances, n, b, s), dpq, np, nq, state.sizes[s]);
        distanceAt(state.distances, n, a, s) = d;
        distanceAt(state.distances, n, s, a) = d;
    }
    state.sizes[a] += state.sizes[b];
    state.ids[a] = newId;
    state.active[b] = false;
    --state.activeCount;
}
//=============================================================================
static ArrayOf
distanceVectorFromInput(const ArrayOfVector& args, LinkageMethod& method, indexType& n)
{
    ArrayOf first = asDoubleMatrix(args[0],
        resolveMessageTemplate(L"Nelson:statistics:inputRealNumericMatrix",
            L"Input must be a real numeric matrix or vector."));
    method = args.size() > 1 ? linkageMethod(args[1]) : LinkageMethod::Single;
    bool vectorInput = first.isVector() && (first.rows() == 1 || first.columns() == 1);
    if (vectorInput) {
        n = squareSizeFromVector(first.elementCount());
        return first;
    }
    if (first.rows() < 2) {
        Error("Nelson:statistics:xNeedsTwoRows", "X must have at least two rows.");
    }
    ArrayOfVector pdistArgs;
    pdistArgs << first;
    if (args.size() > 2) {
        pdistArgs << args[2];
    }
    if (args.size() > 3) {
        pdistArgs << args[3];
    }
    ArrayOf distances = PDistance(pdistArgs);
    n = first.rows();
    return distances;
}
//=============================================================================
ArrayOf
Linkage(const ArrayOfVector& args)
{
    LinkageMethod method;
    indexType n;
    ArrayOf distances = distanceVectorFromInput(args, method, n);
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, (n - 1) * 3));
    const double* condensed = static_cast<const double*>(distances.dataPointer());
    LinkageState state = initialState(condensed, n);
    for (indexType step = 0; step < n - 1; ++step) {
        indexType a;
        indexType b;
        double best;
        nearestPair(state, n, a, b, best);
        indexType left = std::min(state.ids[a], state.ids[b]);
        indexType right = std::max(state.ids[a], state.ids[b]);
        out[step] = static_cast<double>(left);
        out[step + (n - 1)] = static_cast<double>(right);
        out[step + 2 * (n - 1)] = best;
        mergePair(state, n, a, b, method, n + step + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n - 1, 3), out);
}
//=============================================================================
static std::vector<indexType>
leafList(indexType node, indexType n, const std::vector<std::vector<indexType>>& leaves)
{
    if (node <= n) {
        return { node };
    }
    return leaves[node - n - 1];
}
//=============================================================================
static std::vector<std::vector<indexType>>
linkageLeaves(const ArrayOf& z)
{
    indexType rows = z.rows();
    indexType n = rows + 1;
    const double* data = static_cast<const double*>(z.dataPointer());
    std::vector<std::vector<indexType>> leaves(rows);
    for (indexType r = 0; r < rows; ++r) {
        indexType a = static_cast<indexType>(data[r]);
        indexType b = static_cast<indexType>(data[r + rows]);
        std::vector<indexType> merged = leafList(a, n, leaves);
        std::vector<indexType> right = leafList(b, n, leaves);
        merged.insert(merged.end(), right.begin(), right.end());
        leaves[r] = merged;
    }
    return leaves;
}
//=============================================================================
static indexType
findRoot(std::vector<indexType>& parent, indexType x)
{
    while (parent[x] != x) {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    return x;
}
//=============================================================================
static void
unionLeaves(std::vector<indexType>& parent, const std::vector<indexType>& leaves)
{
    if (leaves.empty()) {
        return;
    }
    indexType root = findRoot(parent, leaves[0] - 1);
    for (indexType k = 1; k < leaves.size(); ++k) {
        parent[findRoot(parent, leaves[k] - 1)] = root;
    }
}
//=============================================================================
static std::vector<double>
labelsFromParents(std::vector<indexType>& parent)
{
    indexType n = static_cast<indexType>(parent.size());
    std::vector<indexType> roots(n);
    for (indexType k = 0; k < n; ++k) {
        roots[k] = findRoot(parent, k);
    }
    std::vector<indexType> uniqueRoots = roots;
    std::sort(uniqueRoots.begin(), uniqueRoots.end());
    uniqueRoots.erase(std::unique(uniqueRoots.begin(), uniqueRoots.end()), uniqueRoots.end());
    std::vector<double> labels(n);
    for (indexType k = 0; k < n; ++k) {
        auto it = std::find(uniqueRoots.begin(), uniqueRoots.end(), roots[k]);
        labels[k] = static_cast<double>(std::distance(uniqueRoots.begin(), it) + 1);
    }
    return labels;
}
//=============================================================================
static std::vector<double>
clusterByRows(const ArrayOf& z, indexType rowsToMerge)
{
    indexType n = z.rows() + 1;
    std::vector<indexType> parent(n);
    std::iota(parent.begin(), parent.end(), 0);
    std::vector<std::vector<indexType>> leaves = linkageLeaves(z);
    for (indexType r = 0; r < rowsToMerge; ++r) {
        unionLeaves(parent, leaves[r]);
    }
    return labelsFromParents(parent);
}
//=============================================================================
static std::vector<double>
clusterByCutoff(const ArrayOf& z, double cutoff)
{
    indexType rows = z.rows();
    const double* data = static_cast<const double*>(z.dataPointer());
    indexType mergeRows = 0;
    while (mergeRows < rows && data[mergeRows + 2 * rows] <= cutoff) {
        ++mergeRows;
    }
    return clusterByRows(z, mergeRows);
}
//=============================================================================
static std::vector<double>
clusterByMaxClust(const ArrayOf& z, indexType maxClust)
{
    indexType n = z.rows() + 1;
    indexType target = std::max<indexType>(1, std::min(maxClust, n));
    return clusterByRows(z, n - target);
}
//=============================================================================
static std::wstring
clusterOptionName(const ArrayOf& value)
{
    return matchText(value, { L"cutoff", L"maxclust", L"criterion" },
        resolveMessageTemplate(L"Nelson:statistics:invalidOptionName", L"Invalid option name."));
}
//=============================================================================
static std::vector<double>
numericVector(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf copy = asDoubleMatrix(value, message);
    const double* ptr = static_cast<const double*>(copy.dataPointer());
    return std::vector<double>(ptr, ptr + copy.elementCount());
}
//=============================================================================
ArrayOf
Cluster(const ArrayOfVector& args)
{
    ArrayOf z = asDoubleMatrix(args[0],
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:statistics:realNumericMatrix",
                              L"%s must be a real numeric matrix."),
            L"Z"));
    if (z.columns() != 3) {
        Error("Nelson:statistics:invalidLinkage", "Z must have three columns.");
    }
    std::vector<double> cutoff;
    std::vector<double> maxClust;
    for (sizeType pos = 1; pos < args.size(); pos += 2) {
        if (pos + 1 >= args.size()) {
            Error("Nelson:statistics:nameValueNotPaired", "Name-value arguments must be paired.");
        }
        std::wstring name = clusterOptionName(args[pos]);
        if (name == L"cutoff") {
            cutoff = numericVector(args[pos + 1],
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                    L"Cutoff"));
        } else if (name == L"maxclust") {
            maxClust = numericVector(args[pos + 1],
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                    L"MaxClust"));
        } else {
            matchText(args[pos + 1], { L"distance" }, _W("Criterion must be 'distance'."));
        }
    }
    if (cutoff.empty() == maxClust.empty()) {
        Error("Nelson:statistics:cutoffOrMaxClust", "Specify either Cutoff or MaxClust.");
    }
    indexType n = z.rows() + 1;
    const std::vector<double>& cuts = cutoff.empty() ? maxClust : cutoff;
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n * cuts.size()));
    for (indexType c = 0; c < cuts.size(); ++c) {
        std::vector<double> labels = cutoff.empty()
            ? clusterByMaxClust(z, static_cast<indexType>(cuts[c]))
            : clusterByCutoff(z, cuts[c]);
        std::copy(labels.begin(), labels.end(), out + c * n);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n, static_cast<indexType>(cuts.size())), out);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
