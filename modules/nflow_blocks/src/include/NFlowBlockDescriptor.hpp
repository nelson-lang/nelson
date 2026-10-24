//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Lightweight BlockDescriptor
//=============================================================================
#pragma once
//=============================================================================
#include "NFlowCodegenHelpers.hpp"
#include "FieldNames.hpp"
#include "SimEngineTypes.hpp"
//=============================================================================
#include <string>
#include <vector>
//=============================================================================
namespace nflow {
class BlockDescriptor
{
public:
    BlockDescriptor(const json& block, const VarMap& vars) : m_block(block), m_vars(vars) { }
    BlockDescriptor(const Nelson::NFlow::Block& block, const VarMap& vars)
        : m_block(json::object()), m_vars(vars)
    {
        m_block[nflow::kId] = block.id;
        m_block[nflow::kType] = block.type;
        m_block[nflow::kParams] = block.params;
    }

    std::string
    id() const
    {
        return jstr(m_block, nflow::kId);
    }
    std::string
    type() const
    {
        return jstr(m_block, nflow::kType);
    }

    double
    paramDouble(std::string_view key, double fallback = 0.0) const
    {
        json params = jval(m_block, nflow::kParams);
        json v = jval(params, key);
        if (v.is_null()) {
            // Absent parameter: use the fallback. resolveNumeric would coerce null
            // to 0.0 and clamp e.g. an integrator without limits to [0, 0].
            return fallback;
        }
        if (v.is_array()) {
            // Scalar param in bracket form ([0.9] or [[0.9]]): take the single
            // element, otherwise resolveNumeric([...]) = 0.0 simulates the wrong
            // dynamics. A multi-element matrix is out of range: use the fallback.
            std::vector<double> flat = parseList(v, m_vars);
            if (flat.size() == 1) {
                return std::isnan(flat[0]) ? fallback : flat[0];
            }
            return fallback;
        }
        double r = resolveNumeric(v, m_vars);
        return std::isnan(r) ? fallback : r;
    }

    std::vector<double>
    paramList(std::string_view key) const
    {
        json params = jval(m_block, nflow::kParams);
        return parseList(jval(params, key), m_vars);
    }

    std::string
    paramStr(std::string_view key, const std::string& fallback = "") const
    {
        json params = jval(m_block, nflow::kParams);
        auto it = params.find(std::string(key));
        if (it != params.end() && it->is_string()) {
            return it->get<std::string>();
        }
        return fallback;
    }

    const json&
    raw() const
    {
        return m_block;
    }

private:
    json m_block;
    VarMap m_vars;
};
//=============================================================================
} // namespace nflow
//=============================================================================
