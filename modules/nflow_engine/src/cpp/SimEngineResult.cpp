//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SimEngineResult.hpp"
#include <algorithm>
#include <cstdio>
#include <functional>
//=============================================================================
namespace Nelson::NFlow {
//=============================================================================
std::string
escapeJsonString(const std::string& s)
{
    static const char* kHex = "0123456789abcdef";
    std::string r;
    r.reserve(s.size());
    for (char c : s) {
        const unsigned char u = static_cast<unsigned char>(c);
        switch (c) {
        case '"':
            r += "\\\"";
            continue;
        case '\\':
            r += "\\\\";
            continue;
        case '\b':
            r += "\\b";
            continue;
        case '\f':
            r += "\\f";
            continue;
        case '\n':
            r += "\\n";
            continue;
        case '\r':
            r += "\\r";
            continue;
        case '\t':
            r += "\\t";
            continue;
        default:
            break;
        }
        if (u < 0x20) {
            r += "\\u00";
            r += kHex[(u >> 4) & 0x0F];
            r += kHex[u & 0x0F];
            continue;
        }
        r += c;
    }
    return r;
}
//=============================================================================
static std::string
escapeJsonId(const std::string& s)
{
    return escapeJsonString(s);
}
//=============================================================================
std::string
buildSignalPayloadJson(const std::vector<Block>& blocks, const SignalLayout& sigLayout,
    const std::vector<double>& outputs, const std::vector<double>& outputsImag,
    const std::vector<long long>& outputsI64)
{
    std::string signals = "\"signals\":{";
    std::string signalDims = "\"signalDims\":{";
    std::string signalTypes = "\"signalTypes\":{";
    const bool emitComplex = sigLayout.hasComplex;
    std::string signalsImag = "\"signalsImag\":{";
    std::string signalComplex = "\"signalComplex\":{";
    bool firstSignal = true;
    for (size_t i = 0; i < blocks.size(); ++i) {
        const int nOut = blocks[i].outputs;
        if (nOut <= 0) {
            continue;
        }
        if (!firstSignal) {
            signals += ",";
            signalDims += ",";
            signalTypes += ",";
            signalsImag += ",";
            signalComplex += ",";
        }
        firstSignal = false;
        signals += "\"" + escapeJsonId(blocks[i].id) + "\":[";
        signalDims += "\"" + escapeJsonId(blocks[i].id) + "\":[";
        signalTypes += "\"" + escapeJsonId(blocks[i].id) + "\":[";
        signalsImag += "\"" + escapeJsonId(blocks[i].id) + "\":[";
        signalComplex += "\"" + escapeJsonId(blocks[i].id) + "\":[";
        for (int p = 0; p < nOut; ++p) {
            if (p) {
                signals += ",";
                signalDims += ",";
                signalTypes += ",";
                signalsImag += ",";
                signalComplex += ",";
            }
            const PortSig& ps = sigLayout.sigs[sigLayout.portBase[i] + p];
            // Exact-64 ports serialize from the 64-bit lane (the double lane
            // only holds a lossy projection).
            auto emitOne = [&](int k) {
                if (portUsesI64(ps) && !outputsI64.empty()) {
                    const long long v = outputsI64[static_cast<size_t>(ps.offset + k)];
                    if (ps.type == SigType::UInt64) {
                        signals += std::to_string(static_cast<unsigned long long>(v));
                    } else {
                        signals += std::to_string(v);
                    }
                    return;
                }
                signals += doubleToStr(outputs[static_cast<size_t>(ps.offset + k)]);
            };
            if (ps.width() <= 1) {
                emitOne(0);
            } else {
                signals += "[";
                for (int k = 0; k < ps.width(); ++k) {
                    if (k) {
                        signals += ",";
                    }
                    emitOne(k);
                }
                signals += "]";
            }
            // Full N-D shape, padded to at least [rows, cols] so 1-D vectors
            // keep the historical [n, 1] rendering.
            signalDims += "[";
            const int nd = std::max(2, ps.numDims());
            for (int k = 0; k < nd; ++k) {
                if (k) {
                    signalDims += ",";
                }
                signalDims += std::to_string(k < ps.numDims() ? ps.dims[(size_t)k] : 1);
            }
            signalDims += "]";
            signalTypes += std::string("\"") + sigTypeName(ps.type) + "\"";
            signalComplex += ps.isComplex ? "true" : "false";
            // Imaginary slice (0 for real ports of a complex diagram).
            auto emitImagOne = [&](int k) {
                if (ps.isComplex && !outputsImag.empty()) {
                    signalsImag += doubleToStr(outputsImag[static_cast<size_t>(ps.offset + k)]);
                } else {
                    signalsImag += "0";
                }
            };
            if (ps.width() <= 1) {
                emitImagOne(0);
            } else {
                signalsImag += "[";
                for (int k = 0; k < ps.width(); ++k) {
                    if (k) {
                        signalsImag += ",";
                    }
                    emitImagOne(k);
                }
                signalsImag += "]";
            }
        }
        signals += "]";
        signalDims += "]";
        signalTypes += "]";
        signalsImag += "]";
        signalComplex += "]";
    }
    signals += "}";
    signalDims += "}";
    signalTypes += "}";
    signalsImag += "}";
    signalComplex += "}";
    std::string payload = signals + "," + signalDims + "," + signalTypes;
    if (emitComplex) {
        payload += "," + signalsImag + "," + signalComplex;
    }
    return payload;
}
//=============================================================================
std::string
buildSignalBusJson(
    const std::vector<Block>& blocks, const SignalLayout& sigLayout, const BusRegistry& busRegistry)
{
    bool anyBus = false;
    for (const auto& ps : sigLayout.sigs) {
        if (ps.busTypeId >= 0) {
            anyBus = true;
            break;
        }
    }
    if (!anyBus) {
        return "";
    }
    std::function<std::string(int)> busJson = [&](int id) -> std::string {
        const BusType* t = busRegistry.get(id);
        if (!t) {
            return "null";
        }
        std::string s = "{\"name\":\"" + escapeJsonId(t->name)
            + "\",\"nonvirtual\":" + (t->nonvirtual ? "true" : "false") + ",\"members\":[";
        int off = 0;
        bool first = true;
        for (const auto& m : t->members) {
            if (!first) {
                s += ",";
            }
            first = false;
            const int mw
                = (m.sig.busTypeId >= 0) ? busRegistry.packedWidth(m.sig.busTypeId) : m.sig.width();
            s += "{\"name\":\"" + escapeJsonId(m.name) + "\",\"offset\":" + std::to_string(off)
                + ",\"width\":" + std::to_string(mw);
            if (m.sig.busTypeId >= 0) {
                s += ",\"bus\":" + busJson(m.sig.busTypeId);
            } else {
                s += ",\"type\":\"" + std::string(sigTypeName(m.sig.type))
                    + "\",\"complex\":" + (m.sig.isComplex ? "true" : "false") + ",\"dims\":[";
                const int nd = std::max(2, m.sig.numDims());
                for (int k = 0; k < nd; ++k) {
                    if (k) {
                        s += ",";
                    }
                    s += std::to_string(k < m.sig.numDims() ? m.sig.dims[(size_t)k] : 1);
                }
                s += "]";
            }
            s += "}";
            off += mw;
        }
        s += "]}";
        return s;
    };
    std::string signalBus = "\"signalBus\":{";
    bool firstBlk = true;
    for (size_t i = 0; i < blocks.size(); ++i) {
        const int nOut = blocks[i].outputs;
        if (nOut <= 0) {
            continue;
        }
        if (!firstBlk) {
            signalBus += ",";
        }
        firstBlk = false;
        signalBus += "\"" + escapeJsonId(blocks[i].id) + "\":[";
        for (int p = 0; p < nOut; ++p) {
            if (p) {
                signalBus += ",";
            }
            const PortSig& ps = sigLayout.sigs[sigLayout.portBase[i] + p];
            signalBus += (ps.busTypeId >= 0) ? busJson(ps.busTypeId) : "null";
        }
        signalBus += "]";
    }
    signalBus += "}";
    return "," + signalBus;
}
//=============================================================================
std::string
buildFinalResult(bool algLoopFailed, double algLoopTime, const std::vector<int>& algFailBlocks,
    const std::vector<int>& algIdx, const std::vector<Block>& blocks,
    const std::vector<double>& time, const std::vector<BlockState>& blockState,
    const std::vector<std::vector<int>>& inputIds, const SignalLayout& sigLayout,
    const std::vector<double>& outputs, const std::vector<double>& outputsImag,
    const std::vector<long long>& outputsI64, const BusRegistry& busRegistry)
{
    // Same full escaping as everywhere else: a multi-line block name would break
    // the result payload.
    auto escapeId = [](const std::string& s) { return escapeJsonString(s); };

    if (algLoopFailed) {
        char reason[128];
        snprintf(
            reason, sizeof(reason), "algebraic loop did not converge at t=%.6f s", algLoopTime);
        // List only the blocks of the offending loop (the failing SCC), so the
        // UI highlights the actual cycle rather than every algebraic block.
        const std::vector<int>& failList = algFailBlocks.empty() ? algIdx : algFailBlocks;
        std::string errBlocks = "\"errorBlocks\":[";
        for (size_t k = 0; k < failList.size(); ++k) {
            if (k) {
                errBlocks += ",";
            }
            errBlocks += "\"" + escapeId(blocks[static_cast<size_t>(failList[k])].id) + "\"";
        }
        errBlocks += "]";
        return buildOutputJson(
            "error", std::string(reason), time, blocks, blockState, inputIds, errBlocks);
    }

    std::string payload
        = buildSignalPayloadJson(blocks, sigLayout, outputs, outputsImag, outputsI64);
    payload += buildSignalBusJson(blocks, sigLayout, busRegistry);
    return buildOutputJson("done", "", time, blocks, blockState, inputIds, payload);
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================
