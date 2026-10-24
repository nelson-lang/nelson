//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOAxis.hpp"
#include "GOAxisHelpers.hpp"
#include "GOLinearLogProperty.hpp"
#include "GONormalReverseProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
// Camera properties are already in scaled axes coordinates, as computed by
// updateCamera and consumed by camera navigation. Only reflect directions here;
// applying a logarithm again collapses the viewing direction.
std::vector<double>
GOAxis::mapCamera(const std::vector<double>& point)
{
    auto mapped = point;
    const auto limits = getAxisLimits();
    const std::wstring directions[]
        = { GO_X_DIR_PROPERTY_NAME_STR, GO_Y_DIR_PROPERTY_NAME_STR, GO_Z_DIR_PROPERTY_NAME_STR };
    for (size_t k = 0; k < 3; ++k) {
        if (stringCheck(directions[k], GO_PROPERTY_VALUE_REVERSE_STR)) {
            mapped[k] = limits[2 * k] + limits[2 * k + 1] - point[k];
        }
    }
    return mapped;
}
//=============================================================================
std::vector<double>
GOAxis::reMap(const std::vector<double>& t, bool forceLinearScale)
{
    std::vector<double> s;
    s.reserve(t.size() * 3);
    for (size_t i = 0; i < t.size(); i += 3) {
        s.push_back(mapX(t[i], forceLinearScale));
        s.push_back(mapY(t[i + 1], forceLinearScale));
        if (t.size() > 2) {
            s.push_back(mapZ(t[i + 2], forceLinearScale));
        } else {
            s.push_back(mapZ(0, forceLinearScale));
        }
    }
    return s;
}
//=============================================================================
void
GOAxis::reMap(const std::vector<double>& xs, const std::vector<double>& ys,
    const std::vector<double>& zs, std::vector<double>& ax, std::vector<double>& ay,
    std::vector<double>& az)
{
    ax.resize(xs.size());
    ay.resize(xs.size());
    az.resize(xs.size());
    auto* xdir = static_cast<GONormalReverseProperty*>(findProperty(GO_X_DIR_PROPERTY_NAME_STR));
    auto* ydir = static_cast<GONormalReverseProperty*>(findProperty(GO_Y_DIR_PROPERTY_NAME_STR));
    auto* zdir = static_cast<GONormalReverseProperty*>(findProperty(GO_Z_DIR_PROPERTY_NAME_STR));
    auto* xlim = static_cast<GOTwoVectorProperty*>(findProperty(GO_X_LIM_PROPERTY_NAME_STR));
    auto* ylim = static_cast<GOTwoVectorProperty*>(findProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    auto* zlim = static_cast<GOTwoVectorProperty*>(findProperty(GO_Z_LIM_PROPERTY_NAME_STR));
    auto* xscale = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    auto* yscale = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    auto* zscale = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    const std::vector<double> xlims = xlim->data();
    const std::vector<double> ylims = ylim->data();
    const std::vector<double> zlims = zlim->data();
    const bool xReverse = xdir->isEqual(GO_PROPERTY_VALUE_REVERSE_STR);
    const bool yReverse = ydir->isEqual(GO_PROPERTY_VALUE_REVERSE_STR);
    const bool zReverse = zdir->isEqual(GO_PROPERTY_VALUE_REVERSE_STR);
    const bool xLog = xscale->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    const bool yLog = yscale->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    const bool zLog = zscale->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    const double xReverseSum
        = xReverse ? (xLog ? tickLog(xlims[0]) + tickLog(xlims[1]) : xlims[0] + xlims[1]) : 0.;
    const double yReverseSum
        = yReverse ? (yLog ? tickLog(ylims[0]) + tickLog(ylims[1]) : ylims[0] + ylims[1]) : 0.;
    const double zReverseSum
        = zReverse ? (zLog ? tickLog(zlims[0]) + tickLog(zlims[1]) : zlims[0] + zlims[1]) : 0.;
    OMP_PARALLEL_FOR_LOOP(xs.size())
    for (ompIndexType i = 0; i < (ompIndexType)xs.size(); i++) {
        double x = xLog ? tickLog(xs[i]) : xs[i];
        double y = yLog ? tickLog(ys[i]) : ys[i];
        double z = zLog ? tickLog(zs[i]) : zs[i];
        ax[i] = xReverse ? xReverseSum - x : x;
        ay[i] = yReverse ? yReverseSum - y : y;
        az[i] = zReverse ? zReverseSum - z : z;
    }
}
//=============================================================================
double
GOAxis::mapX(double x, bool forceLinearScale)
{
    GONormalReverseProperty* hp
        = static_cast<GONormalReverseProperty*>(findProperty(GO_X_DIR_PROPERTY_NAME_STR));
    GOTwoVectorProperty* xlim
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_X_LIM_PROPERTY_NAME_STR));
    std::vector<double> lims(xlim->data());
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR) && !forceLinearScale) {
        x = tickLog(x);
        lims[0] = tickLog(lims[0]);
        lims[1] = tickLog(lims[1]);
    }
    if (hp->isEqual(GO_PROPERTY_VALUE_REVERSE_STR)) {
        double xmin(lims[0]);
        double xmax(lims[1]);
        return (xmin + xmax - x);
    }
    return (x);
}
//=============================================================================
double
GOAxis::mapY(double y, bool forceLinearScale)
{
    GONormalReverseProperty* hp
        = static_cast<GONormalReverseProperty*>(findProperty(GO_Y_DIR_PROPERTY_NAME_STR));
    GOTwoVectorProperty* ylim
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    std::vector<double> lims(ylim->data());
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR) && !forceLinearScale) {
        y = tickLog(y);
        lims[0] = tickLog(lims[0]);
        lims[1] = tickLog(lims[1]);
    }
    if (hp->isEqual(GO_PROPERTY_VALUE_REVERSE_STR)) {
        double ymin(lims[0]);
        double ymax(lims[1]);
        return (ymin + ymax - y);
    }
    return (y);
}
//=============================================================================
double
GOAxis::mapZ(double z, bool forceLinearScale)
{
    GONormalReverseProperty* hp
        = static_cast<GONormalReverseProperty*>(findProperty(GO_Z_DIR_PROPERTY_NAME_STR));
    GOTwoVectorProperty* zlim
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_Z_LIM_PROPERTY_NAME_STR));
    std::vector<double> lims(zlim->data());
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LOG_STR) && !forceLinearScale) {
        z = tickLog(z);
        lims[0] = tickLog(lims[0]);
        lims[1] = tickLog(lims[1]);
    }
    if (hp->isEqual(GO_PROPERTY_VALUE_REVERSE_STR)) {
        double zmin(lims[0]);
        double zmax(lims[1]);
        return (zmin + zmax - z);
    }
    return (z);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
