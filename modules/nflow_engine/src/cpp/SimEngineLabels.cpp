//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineLabels.hpp"
#include "FieldNames.hpp"
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
bool
resolveLabelSourcesOnce(const std::vector<Block>& blocks,
    const std::unordered_map<std::string, std::string>& labelSinks,
    const std::unordered_map<std::string, BlockId>& nameToId,
    const std::vector<std::vector<int>>& inputIds, const std::vector<int>& outputBase,
    const SignalLayout& sigLayout, std::vector<double>& outputs)
{
    bool changed = false;
    for (const Block& b : blocks) {
        if (b.type != nflow::kLabelSource) {
            continue;
        }
        if (b.params.contains(nflow::kIsExternalPort)
            && b.params[nflow::kIsExternalPort].get<bool>()) {
            continue;
        }
        std::string name;
        if (b.params.contains(nflow::kGotoTag) && b.params[nflow::kGotoTag].is_string()) {
            name = b.params[nflow::kGotoTag].get<std::string>();
        }
        if (name.empty()) {
            continue;
        }
        auto sit = labelSinks.find(name);
        if (sit == labelSinks.end()) {
            continue;
        }
        auto nit = nameToId.find(sit->second);
        if (nit == nameToId.end()) {
            continue;
        }
        BlockId sinkNid = nit->second;
        if (inputIds[sinkNid].empty() || inputIds[sinkNid][0] < 0) {
            continue; // sink has no connection yet
        }
        // The labelSource port was sized to the sink's input during signal layout.
        const int srcSlot = inputIds[sinkNid][0];
        const int labelSlot = outputBase[b.nid];
        const int w = sigLayout.sigs[sigLayout.portBase[b.nid]].width();
        for (int k = 0; k < w; ++k) {
            if (outputs[labelSlot + k] != outputs[srcSlot + k]) {
                outputs[labelSlot + k] = outputs[srcSlot + k];
                changed = true;
            }
        }
    }
    return changed;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
