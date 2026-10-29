//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineInit.hpp"
#include "SimEngineResult.hpp"
#include "NFlowBlockCatalog.hpp"
#include "FieldNames.hpp"
#include "NFlowCodegenHelpers.hpp"
#include <cmath>
#include <functional>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
namespace {
    // A numeric parameter holding text that resolves to nothing (neither a literal
    // nor an expression over the model's variables): the reader would fall back,
    // usually to 0, and the run would simulate something else.
    bool
    unresolvedNumericText(const json& value, const std::unordered_map<std::string, double>& vars,
        std::string& offending)
    {
        if (value.is_string()) {
            if (!nflow::resolvesNumeric(value, vars)) {
                offending = value.get<std::string>();
                return true;
            }
            return false;
        }
        if (value.is_array()) {
            for (const json& el : value) {
                if (unresolvedNumericText(el, vars, offending)) {
                    return true;
                }
            }
        }
        return false;
    }
    //=============================================================================
    bool
    checkOneBlock(const std::string& id, const std::string& type, const json& params,
        const std::unordered_map<std::string, double>& variables, SimDiagnostics& diagnostics)
    {
        const BlockCatalogEntry* entry = NFlowBlockCatalog::instance().find(type);
        if (entry != nullptr && entry->defaultParams.is_object() && params.is_object()) {
            for (auto it = params.begin(); it != params.end(); ++it) {
                auto def = entry->defaultParams.find(it.key());
                if (def == entry->defaultParams.end() || !def->is_number()) {
                    continue; // not declared numeric: an enum / text parameter
                }
                std::string offending;
                if (!unresolvedNumericText(it.value(), variables, offending)) {
                    continue;
                }
                SimDiagnostic d;
                d.code = "unresolved_parameter";
                d.blockId = id;
                d.message = std::string("parameter '") + it.key() + "' of block '" + id + "' ("
                    + type + ") is '" + offending
                    + "', which is neither a number nor a variable of this model. It would read "
                      "back as its default and the run would simulate something else.";
                diagnostics.fail(d);
                return true; // one clear cause is enough
            }
        }
        // Inner diagrams are checked too. A masked subsystem scopes its parameters
        // over the parent's, as the runtime does.
        if (params.is_object()) {
            auto sub = params.find("subsystem");
            if (sub != params.end() && sub->is_object()) {
                auto inner = sub->find(nflow::kBlocks);
                if (inner != sub->end() && inner->is_array()) {
                    std::unordered_map<std::string, double> scope = variables;
                    auto mask = params.find("maskParameters");
                    if (mask != params.end() && mask->is_object()) {
                        for (const auto& kv : mask->items()) {
                            if (kv.value().is_number()) {
                                scope[kv.key()] = kv.value().get<double>();
                            }
                        }
                    }
                    for (const json& ib : *inner) {
                        if (!ib.is_object()) {
                            continue;
                        }
                        if (checkOneBlock(ib.value(nflow::kId, std::string()),
                                ib.value(nflow::kType, std::string()),
                                ib.value(nflow::kParams, json::object()), scope, diagnostics)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }
    //=============================================================================
    void
    checkNumericParams(const std::vector<Block>& blocks,
        const std::unordered_map<std::string, double>& variables, SimDiagnostics& diagnostics)
    {
        NFlowBlockCatalog::instance().ensureLoaded();
        for (const Block& b : blocks) {
            if (checkOneBlock(b.id, b.type, asJson(b.params), variables, diagnostics)) {
                return;
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
std::optional<std::string>
runInitPhase(SimCtx& ctx, const std::vector<Block>& blocks, std::vector<BlockState>& blockState,
    const BlockRegistry& reg, const std::unordered_map<std::string, double>& variables,
    const std::vector<int>& initIdx, SimDiagnostics& diagnostics,
    const std::vector<std::vector<int>>& inputIds)
{
    // Before anything reads a parameter: a numeric one naming nothing would
    // silently become its default.
    checkNumericParams(blocks, variables, diagnostics);

    for (int i : initIdx) {
        auto fn = reg.getHandler(blocks[i].type);
        if (fn) {
            fn(ctx, blocks[i], Phase::INIT);
        }
    }

    // A triggered / enableTrigger subsystem runs only at discrete control edges,
    // so continuous states inside have no defined integration between edges:
    // reject them. INIT has run, so st.subsystem and ctrlKind are set.
    // Enabled/action subsystems hold their continuous states while inactive.
    {
        std::function<int(const std::vector<Block>&, const std::vector<BlockState>&)>
            countContinuous
            = [&](const std::vector<Block>& bs, const std::vector<BlockState>& states) -> int {
            int total = 0;
            for (const Block& ib : bs) {
                if (ib.nid < 0 || ib.nid >= (int)states.size()) {
                    continue;
                }
                const BlockState& ist = states[(size_t)ib.nid];
                const BlockMetadata m = reg.metadataOf(ib.type);
                total += m.continuousStateCount ? m.continuousStateCount(ib, variables)
                                                : m.numContinuousStates;
                if (ist.subsystem) {
                    total += countContinuous(ist.subsystem->blocks, ist.subsystem->blockStateVec);
                }
            }
            return total;
        };
        std::function<void(const std::vector<Block>&, const std::vector<BlockState>&)>
            checkTriggered
            = [&](const std::vector<Block>& bs, const std::vector<BlockState>& states) {
                  for (const Block& ib : bs) {
                      if (ib.nid < 0 || ib.nid >= (int)states.size()) {
                          continue;
                      }
                      const BlockState& ist = states[(size_t)ib.nid];
                      if (!ist.subsystem) {
                          continue;
                      }
                      const int ck = ist.subsystem->ctrlKind;
                      if ((ck == 2 || ck == 3)
                          && countContinuous(ist.subsystem->blocks, ist.subsystem->blockStateVec)
                              > 0) {
                          SimDiagnostic d;
                          d.code = "triggered_continuous_state";
                          d.blockId = ib.id;
                          d.message = "triggered subsystem '" + ib.id
                              + "' contains continuous states; a triggered subsystem "
                                "executes only at discrete trigger edges and cannot "
                                "integrate continuous states";
                          diagnostics.fail(d);
                      }
                      // A For / While iterator (ck 7 / 8) runs its body several
                      // times per step and owns its state advance: reject
                      // continuous states the same way.
                      if ((ck == 7 || ck == 8)
                          && countContinuous(ist.subsystem->blocks, ist.subsystem->blockStateVec)
                              > 0) {
                          SimDiagnostic d;
                          d.code = "iterator_continuous_state";
                          d.blockId = ib.id;
                          d.message = "iterator subsystem '" + ib.id
                              + "' contains continuous states; a For/While iterator "
                                "subsystem runs its body multiple times per step and "
                                "cannot integrate continuous states";
                          diagnostics.fail(d);
                      }
                      checkTriggered(ist.subsystem->blocks, ist.subsystem->blockStateVec);
                  }
              };
        checkTriggered(blocks, blockState);
    }

    // A block INIT may report a fatal diagnostic (e.g. an FMU that cannot be
    // loaded): abort the run.
    if (diagnostics.aborted()) {
        std::string reason;
        std::string errBlocks = "\"errorBlocks\":[";
        bool firstErrBlock = true;
        for (const SimDiagnostic& d : diagnostics.all()) {
            if (!d.fatal) {
                continue;
            }
            if (!reason.empty()) {
                reason += "; ";
            }
            reason += d.message;
            if (!d.blockId.empty()) {
                if (!firstErrBlock) {
                    errBlocks += ",";
                }
                firstErrBlock = false;
                errBlocks += "\"" + escapeJsonString(d.blockId) + "\"";
            }
        }
        errBlocks += "]";
        const std::vector<double> noTime;
        const std::vector<BlockState> noState(blocks.size());
        return buildOutputJson("error", reason, noTime, blocks, noState, inputIds, errBlocks);
    }

    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
