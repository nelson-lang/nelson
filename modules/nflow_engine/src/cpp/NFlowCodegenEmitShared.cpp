//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowCodegenEmitShared.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "FieldNames.hpp"
#include "BlockRegistry.hpp"
//=============================================================================
namespace nflow {
//=============================================================================
using json = nlohmann::json;
//=============================================================================
CodegenCrossScan
buildCodegenCrossScan(const std::vector<json>& blocks, const char* lang,
    const std::unordered_map<std::string, double>& variables)
{
    CodegenCrossScan scan;
    for (const auto& b : blocks) {
        const std::string ctype = jstr(b, nflow::kType);
        Nelson::NFlow::BlockCodegenTemplate xtpl;
        if (!Nelson::NFlow::BlockRegistry::instance().getCodegen(ctype, lang, xtpl)
            || xtpl.notImplemented || !xtpl.crossingCount || !xtpl.emitRk4) {
            continue;
        }
        const std::string rawId = jstr(b, nflow::kId);
        json bp = jval(b, nflow::kParams);
        scan.paramsStore.push_back(bp.is_object() ? bp : json::object());
        Nelson::NFlow::BlockCodegenArgs xa;
        xa.id = sanitizeId(rawId);
        xa.block = &b;
        xa.params = &scan.paramsStore.back();
        xa.variables = &variables;
        const int nsurf = xtpl.crossingCount(xa);
        if (nsurf > 0) {
            scan.list.push_back({ rawId, ctype, &b, scan.total });
            scan.params.push_back(&scan.paramsStore.back());
            scan.total += nsurf;
        } else {
            scan.paramsStore.pop_back();
        }
    }
    return scan;
}
//=============================================================================
std::vector<std::string>
condGatesOf(const json& block)
{
    std::vector<std::string> gates;
    const json gts = jval(jval(block, nflow::kParams), "__nfgGates");
    if (gts.is_array()) {
        for (const auto& g : gts) {
            if (g.is_string()) {
                gates.push_back(sanitizeId(g.get<std::string>()));
            }
        }
    }
    return gates;
}
//=============================================================================
void
appendCondExecutionEdges(const std::vector<json>& blocks, std::vector<json>& extraEdges)
{
    // A guarded member reads out_<gate> without a data wire, so the gate goes before
    // every tagged block. A trigger's end-of-step latch rewrites the gate's edge
    // memory, so it goes after its gate.
    for (const auto& block : blocks) {
        const json gts = jval(jval(block, nflow::kParams), "__nfgGates");
        if (gts.is_array()) {
            for (const auto& g : gts) {
                if (g.is_string()) {
                    extraEdges.push_back({ { nflow::kFrom, g.get<std::string>() },
                        { nflow::kTo, jstr(block, nflow::kId) } });
                }
            }
        }
        if (jstr(block, nflow::kType) == "condLatch") {
            const std::string g = jval(jval(block, nflow::kParams), "gate").is_string()
                ? jval(jval(block, nflow::kParams), "gate").get<std::string>()
                : std::string();
            if (!g.empty()) {
                extraEdges.push_back(
                    { { nflow::kFrom, g }, { nflow::kTo, jstr(block, nflow::kId) } });
            }
        }
        // The gate's edge re-seeds the states of reset members first.
        {
            const json rg = jval(jval(block, nflow::kParams), "__nfgReset");
            if (rg.is_string()) {
                extraEdges.push_back({ { nflow::kFrom, rg.get<std::string>() },
                    { nflow::kTo, jstr(block, nflow::kId) } });
            }
        }
        // A data store read must see what THIS step wrote, so every write goes before
        // every read. Neither has a wire to the other, so without this ordering the
        // read, having no input, lands in the first wave and returns the previous value.
        if (jstr(block, nflow::kType) == "dataStoreRead") {
            const json rn = jval(jval(block, nflow::kParams), "DataStoreName");
            const std::string name = rn.is_string() ? rn.get<std::string>() : std::string("A");
            for (const auto& other : blocks) {
                if (jstr(other, nflow::kType) != "dataStoreWrite") {
                    continue;
                }
                const json wn = jval(jval(other, nflow::kParams), "DataStoreName");
                const std::string wname = wn.is_string() ? wn.get<std::string>() : std::string("A");
                if (wname == name) {
                    extraEdges.push_back({ { nflow::kFrom, jstr(other, nflow::kId) },
                        { nflow::kTo, jstr(block, nflow::kId) } });
                }
            }
        }
    }
}
//=============================================================================
} // namespace nflow
//=============================================================================
