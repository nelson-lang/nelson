//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineFunctionCall.hpp"
#include <algorithm>
#include <functional>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
bool
wireFunctionCalls(SimCtx& ctx, std::vector<Block>& blocks, std::vector<BlockState>& blockState,
    const BlockRegistry& reg, const SignalLayout& sigLayout,
    const std::vector<std::vector<int>>& inputIds, int totalSlots)
{
    bool hasFunctionCall = false;
    for (int nid = 0; nid < (int)blocks.size(); ++nid) {
        const auto& sub = blockState[nid].subsystem;
        if (sub && sub->ctrlKind == 6) {
            hasFunctionCall = true;
            break;
        }
    }
    {
        // Event signals are scalar, so one slot per port.
        auto outSlotOf = [&](int b, int p) -> int {
            if (b < 0 || p < 0 || p >= blocks[b].outputs) {
                return -1;
            }
            return sigLayout.sigs[sigLayout.portBase[b] + p].offset;
        };
        // slot -> consumers, in ascending (nid, port) order.
        std::vector<std::vector<int>> slotConsumers(totalSlots);
        for (int nid = 0; nid < (int)blocks.size(); ++nid) {
            for (int port = 0; port < (int)inputIds[nid].size(); ++port) {
                const int s = inputIds[nid][port];
                if (s >= 0 && s < totalSlots) {
                    slotConsumers[s].push_back(nid);
                }
            }
        }
        // Collects callees reachable from an event output slot, expanding
        // functionCallSplit routers in output-port order. `visited` guards against
        // event-wire loops through splits.
        std::vector<char> visited(blocks.size(), 0);
        std::function<void(int, std::vector<int>&)> collect
            = [&](int outSlot, std::vector<int>& callees) {
                  if (outSlot < 0 || outSlot >= totalSlots) {
                      return;
                  }
                  for (int consumer : slotConsumers[outSlot]) {
                      const auto& csub = blockState[consumer].subsystem;
                      if (csub && csub->ctrlKind == 6) {
                          callees.push_back(consumer);
                      } else if (blocks[consumer].type == "functionCallSplit") {
                          if (visited[consumer]) {
                              continue;
                          }
                          visited[consumer] = 1;
                          for (int p = 0; p < blocks[consumer].outputs; ++p) {
                              collect(outSlotOf(consumer, p), callees);
                          }
                      }
                  }
              };
        for (int gen = 0; gen < (int)blocks.size(); ++gen) {
            if (blocks[gen].type != "functionCallGenerator") {
                continue;
            }
            std::fill(visited.begin(), visited.end(), 0);
            collect(outSlotOf(gen, 0), blockState[gen].functionCallCallees);
        }
        auto subFn = reg.getHandler("subsystem");
        ctx.invokeSubsystem = [&ctx, &blocks, subFn](int calleeNid, int iters) {
            if (ctx.sideEffectFree) {
                return; // side-effect-free solver eval (rhs/roots): do not fire callees
            }
            if (!subFn || calleeNid < 0 || calleeNid >= (int)blocks.size()) {
                return;
            }
            auto& css = ctx.blockState[calleeNid].subsystem;
            if (!css) {
                return;
            }
            for (int k = 0; k < iters; ++k) {
                css->functionCallInvoking = true;
                subFn(ctx, blocks[calleeNid], Phase::ALGEBRAIC);
                subFn(ctx, blocks[calleeNid], Phase::UPDATE);
                css->functionCallInvoking = false;
            }
        };
    }

    return hasFunctionCall;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
