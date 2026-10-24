//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GeometryQhull.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
// The reentrant C library (qhull_r) is used instead of the C++ wrapper
// (qhullcpp): qhullcpp is distributed as a release-only static library and
// linking it into a debug build mixes incompatible C runtimes.
#include "libqhull_r/qhull_ra.h"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <limits>
#include <map>
#include <set>
#include <vector>
//=============================================================================
#ifdef _MSC_VER
// setjmp is only used in frames holding trivially destructible objects.
#pragma warning(disable : 4611)
#endif
//=============================================================================
namespace Nelson::Geometry {
//=============================================================================
namespace {
    //=============================================================================
    class QhullSession
    {
    public:
        QhullSession() { qh_zero(&m_qh, stderr); }

        ~QhullSession()
        {
            int curlong = 0;
            int totlong = 0;
            qh_freeqhull(&m_qh, !qh_ALL);
            qh_memfreeshort(&m_qh, &curlong, &totlong);
        }

        QhullSession(const QhullSession&) = delete;
        QhullSession&
        operator=(const QhullSession&)
            = delete;

        qhT*
        handle()
        {
            return &m_qh;
        }

        // Returns true when qhull completed successfully. The point coordinates are
        // copied because qhull may rescale or project them in place.
        bool
        run(const QhullInput& input, const std::string& options)
        {
            m_points = input.coordinates;
            std::string command = "qhull " + options;
            return qh_new_qhull(&m_qh, static_cast<int>(input.dimension),
                       static_cast<int>(input.pointCount), m_points.data(), False, command.data(),
                       nullptr, nullptr)
                == qh_ERRnone;
        }

    private:
        qhT m_qh {};
        std::vector<double> m_points;
    };
    //=============================================================================
    // qhull reports failures by longjmp'ing on qh.errexit. Each helper below keeps
    // setjmp in a frame that owns trivially destructible objects only.
    bool
    computeArea(qhT* qh)
    {
        int exitcode = setjmp(qh->errexit);
        if (exitcode == 0) {
            qh->NOerrexit = False;
            if (!qh->hasAreaVolume) {
                qh_getarea(qh, qh->facet_list);
            }
        }
        qh->NOerrexit = True;
        return exitcode == 0;
    }
    //=============================================================================
    // Computes the Voronoi center of every facet and the facet neighbors of every
    // vertex.
    bool
    computeVoronoiCenters(qhT* qh)
    {
        int exitcode = setjmp(qh->errexit);
        if (exitcode == 0) {
            qh->NOerrexit = False;
            qh_setvoronoi_all(qh);
        }
        qh->NOerrexit = True;
        return exitcode == 0;
    }
    //=============================================================================
} // namespace
//=============================================================================
static void
validateInput(const QhullInput& input)
{
    if (input.pointCount == 0 || input.dimension < 2) {
        Error(L"Nelson:geometry:invalidPoints:leastOnePointTwoMoreCoordinates",
            L"At least one point with two or more coordinates expected.");
    }
    if (input.coordinates.size() != input.pointCount * input.dimension) {
        Error(L"Nelson:geometry:invalidPoints:invalidPointCoordinateBuffer",
            L"Invalid point coordinate buffer.");
    }
    for (double v : input.coordinates) {
        if (!std::isfinite(v)) {
            Error(L"Nelson:validators:mustBeFinite", L"%s must be finite.", L"Input points");
        }
    }
}
//=============================================================================
bool
qhullInputIsDegenerate(const QhullInput& input)
{
    size_t d = input.dimension;
    size_t n = input.pointCount;
    if (d == 0 || n < d + 1) {
        return true;
    }
    // The affine dimension of the point cloud must be equal to the space
    // dimension, otherwise qhull cannot build an initial simplex
    // (collinear / coplanar points).
    std::vector<double> mean(d, 0.);
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < d; ++j) {
            mean[j] += input.coordinates[i * d + j];
        }
    }
    for (size_t j = 0; j < d; ++j) {
        mean[j] /= static_cast<double>(n);
    }
    std::vector<double> gram(d * d, 0.);
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < d; ++j) {
            double vj = input.coordinates[i * d + j] - mean[j];
            for (size_t k = j; k < d; ++k) {
                double vk = input.coordinates[i * d + k] - mean[k];
                gram[j * d + k] += vj * vk;
            }
        }
    }
    for (size_t j = 0; j < d; ++j) {
        for (size_t k = 0; k < j; ++k) {
            gram[j * d + k] = gram[k * d + j];
        }
    }
    double maxDiagonal = 0.;
    for (size_t j = 0; j < d; ++j) {
        maxDiagonal = std::max(maxDiagonal, gram[j * d + j]);
    }
    if (!(maxDiagonal > 0.)) {
        return true;
    }
    double tolerance = maxDiagonal * std::numeric_limits<double>::epsilon()
        * static_cast<double>(std::max(n, d));
    // pivoted symmetric elimination: count numerically independent directions
    std::vector<bool> used(d, false);
    size_t rank = 0;
    for (size_t step = 0; step < d; ++step) {
        size_t pivot = d;
        double pivotValue = tolerance;
        for (size_t j = 0; j < d; ++j) {
            if (!used[j] && gram[j * d + j] > pivotValue) {
                pivotValue = gram[j * d + j];
                pivot = j;
            }
        }
        if (pivot == d) {
            break;
        }
        used[pivot] = true;
        ++rank;
        for (size_t j = 0; j < d; ++j) {
            if (used[j]) {
                continue;
            }
            double factor = gram[j * d + pivot] / pivotValue;
            for (size_t k = 0; k < d; ++k) {
                if (used[k]) {
                    continue;
                }
                gram[j * d + k] -= factor * gram[pivot * d + k];
            }
        }
    }
    return rank < d;
}
//=============================================================================
static std::vector<int>
facetVertices(qhT* qh, facetT* facet)
{
    std::vector<int> ids;
    vertexT* vertex = nullptr;
    vertexT** vertexp = nullptr;
    FOREACHvertex_(facet->vertices)
    {
        int id = qh_pointid(qh, vertex->point) + 1;
        if (id > 0) {
            ids.push_back(id);
        }
    }
    std::sort(ids.begin(), ids.end());
    return ids;
}
//=============================================================================
static std::wstring
degenerateInputMessage(bool delaunay)
{
    return delaunay
        ? Nelson::resolveMessageTemplate(L"Nelson:geometry:errorComputingDelaunayTriangulation",
              L"Error computing the Delaunay triangulation. The points may be collinear.")
        : Nelson::resolveMessageTemplate(L"Nelson:geometry:errorComputingConvexHull",
              L"Error computing the convex hull. The points may be collinear.");
}
//=============================================================================
static FacetResult
runFacetQhull(const QhullInput& input, const std::string& options, bool delaunay)
{
    validateInput(input);
    if (qhullInputIsDegenerate(input)) {
        // qhull cannot build an initial simplex from degenerate inputs and its
        // error path is not recoverable: raise a clean error instead.
        RaiseError(degenerateInputMessage(delaunay), L"Nelson:geometry:degenerateInput");
    }
    FacetResult result;
    {
        QhullSession session;
        qhT* qh = session.handle();
        if (!session.run(input, options)) {
            RaiseError(degenerateInputMessage(delaunay), L"Nelson:geometry:degenerateInput");
        }
        std::set<std::vector<int>> uniqueFacets;
        facetT* facet = nullptr;
        FORALLfacets
        {
            if (!facet->good) {
                continue;
            }
            if (delaunay && facet->upperdelaunay) {
                continue;
            }
            std::vector<int> ids = facetVertices(qh, facet);
            if (ids.size() >= input.dimension && uniqueFacets.insert(ids).second) {
                result.facets.push_back(ids);
            }
        }
        if (result.facets.empty()) {
            RaiseError(degenerateInputMessage(delaunay), L"Nelson:geometry:degenerateInput");
        }
        if (!delaunay) {
            if (!computeArea(qh)) {
                RaiseError(degenerateInputMessage(delaunay), L"Nelson:geometry:degenerateInput");
            }
            result.area = qh->totarea;
            result.volume = qh->totvol;
        }
    }
    return result;
}
//=============================================================================
FacetResult
qhullConvexHull(const QhullInput& input, const std::string& options)
{
    return runFacetQhull(input, options, false);
}
//=============================================================================
FacetResult
qhullDelaunay(const QhullInput& input, const std::string& options)
{
    return runFacetQhull(input, options, true);
}
//=============================================================================
VoronoiResult
qhullVoronoi(const QhullInput& input, const std::string& options)
{
    validateInput(input);
    if (qhullInputIsDegenerate(input)) {
        RaiseError(degenerateInputMessage(true), L"Nelson:geometry:degenerateInput");
    }
    VoronoiResult result;
    {
        QhullSession session;
        qhT* qh = session.handle();
        if (!session.run(input, options)) {
            RaiseError(degenerateInputMessage(true), L"Nelson:geometry:degenerateInput");
        }
        if (!computeVoronoiCenters(qh)) {
            RaiseError(degenerateInputMessage(true), L"Nelson:geometry:degenerateInput");
        }
        const size_t centerDimension = static_cast<size_t>(qh->hull_dim - 1);

        result.dimension = input.dimension;
        result.vertices.assign(input.dimension, std::numeric_limits<double>::infinity());
        result.vertexCount = 1;

        std::map<int, int> facetToVertex;
        facetT* facet = nullptr;
        FORALLfacets
        {
            if (!facet->good || facet->upperdelaunay) {
                continue;
            }
            if (facet->center == nullptr || centerDimension != input.dimension) {
                continue;
            }
            int index = static_cast<int>(result.vertexCount) + 1;
            facetToVertex[static_cast<int>(facet->id)] = index;
            result.vertices.insert(
                result.vertices.end(), facet->center, facet->center + input.dimension);
            result.vertexCount++;
        }

        result.cells.resize(input.pointCount);
        vertexT* vertex = nullptr;
        FORALLvertices
        {
            int site = qh_pointid(qh, vertex->point);
            if (site < 0 || static_cast<size_t>(site) >= input.pointCount) {
                continue;
            }
            std::vector<int> cell;
            if (vertex->neighbors == nullptr) {
                cell.push_back(1);
            } else {
                facetT* neighbor = nullptr;
                facetT** neighborp = nullptr;
                FOREACHneighbor_(vertex)
                {
                    auto it = facetToVertex.find(static_cast<int>(neighbor->id));
                    if (it == facetToVertex.end()) {
                        cell.push_back(1);
                    } else {
                        cell.push_back(it->second);
                    }
                }
            }
            std::sort(cell.begin(), cell.end());
            cell.erase(std::unique(cell.begin(), cell.end()), cell.end());
            if (cell.empty()) {
                cell.push_back(1);
            }
            result.cells[site] = cell;
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson::Geometry
//=============================================================================
