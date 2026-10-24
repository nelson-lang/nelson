//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Bus type registry: an ordered, NAMED list of heterogeneous members, each
// member carrying a full signal descriptor (type, complexity, N-D dims, or a
// nested bus via its member PortSig's busTypeId). PortSig references a bus
// by integer id (PortSig stays a light value type); at runtime both virtual
// and nonvirtual buses are materialized as a packed contiguous region of the
// flat signal buffer (member offsets from memberOffset). The virtual /
// nonvirtual distinction only surfaces where it is observable: code
// generation (struct vs flattened wires), validation and labeling.
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include <unordered_map>
#include "SimEngineTypes.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    struct BusMember
    {
        std::string name;
        PortSig sig; // full descriptor; sig.busTypeId >= 0 = nested bus
    };
    //=============================================================================
    struct BusType
    {
        std::string name; // "" = anonymous (inferred by a busCreator)
        bool nonvirtual = false;
        std::vector<BusMember> members;
    };
    //=============================================================================
    class BusRegistry
    {
    public:
        std::vector<BusType> types;
        std::unordered_map<std::string, int> byName;

        int
        add(BusType t)
        {
            const int id = (int)types.size();
            if (!t.name.empty()) {
                byName[t.name] = id;
            }
            types.push_back(std::move(t));
            return id;
        }
        const BusType*
        get(int id) const
        {
            return (id >= 0 && id < (int)types.size()) ? &types[(size_t)id] : nullptr;
        }
        int
        idOf(const std::string& name) const
        {
            auto it = byName.find(name);
            return (it != byName.end()) ? it->second : -1;
        }

        // Total packed element count of bus #id (recursive over nested
        // buses; depth-guarded against definition cycles).
        int
        packedWidth(int id, int depth = 0) const
        {
            const BusType* t = get(id);
            if (!t || depth > 32) {
                return 0;
            }
            int w = 0;
            for (const auto& m : t->members) {
                w += (m.sig.busTypeId >= 0) ? packedWidth(m.sig.busTypeId, depth + 1)
                                            : m.sig.width();
            }
            return w;
        }

        // Element offset of member path "a" or "a.b.c" inside the packed
        // region of bus #id; -1 when the path does not resolve.
        int
        memberOffset(int id, const std::string& path, int depth = 0) const
        {
            const BusType* t = get(id);
            if (!t || depth > 32) {
                return -1;
            }
            const size_t dot = path.find('.');
            const std::string head = (dot == std::string::npos) ? path : path.substr(0, dot);
            int off = 0;
            for (const auto& m : t->members) {
                const int mw = (m.sig.busTypeId >= 0) ? packedWidth(m.sig.busTypeId, depth + 1)
                                                      : m.sig.width();
                if (m.name == head) {
                    if (dot == std::string::npos) {
                        return off;
                    }
                    if (m.sig.busTypeId < 0) {
                        return -1; // path descends into a leaf
                    }
                    const int sub = memberOffset(m.sig.busTypeId, path.substr(dot + 1), depth + 1);
                    return (sub < 0) ? -1 : off + sub;
                }
                off += mw;
            }
            return -1;
        }

        // Descriptor of member path inside bus #id (nullptr when absent).
        const PortSig*
        resolvePath(int id, const std::string& path, int depth = 0) const
        {
            const BusType* t = get(id);
            if (!t || depth > 32) {
                return nullptr;
            }
            const size_t dot = path.find('.');
            const std::string head = (dot == std::string::npos) ? path : path.substr(0, dot);
            for (const auto& m : t->members) {
                if (m.name == head) {
                    if (dot == std::string::npos) {
                        return &m.sig;
                    }
                    if (m.sig.busTypeId < 0) {
                        return nullptr;
                    }
                    return resolvePath(m.sig.busTypeId, path.substr(dot + 1), depth + 1);
                }
            }
            return nullptr;
        }

        // Structural equality of two bus types (member names + descriptors,
        // recursive): two anonymous inferred buses with identical members
        // compare equal even under different ids.
        bool
        structurallyEqual(int a, int b, int depth = 0) const
        {
            if (a == b) {
                return true;
            }
            const BusType* ta = get(a);
            const BusType* tb = get(b);
            if (!ta || !tb || depth > 32 || ta->members.size() != tb->members.size()) {
                return false;
            }
            for (size_t i = 0; i < ta->members.size(); ++i) {
                const BusMember& ma = ta->members[i];
                const BusMember& mb = tb->members[i];
                if (ma.name != mb.name || ma.sig.dims != mb.sig.dims || ma.sig.type != mb.sig.type
                    || ma.sig.isComplex != mb.sig.isComplex) {
                    return false;
                }
                const bool na = ma.sig.busTypeId >= 0;
                const bool nb = mb.sig.busTypeId >= 0;
                if (na != nb) {
                    return false;
                }
                if (na && !structurallyEqual(ma.sig.busTypeId, mb.sig.busTypeId, depth + 1)) {
                    return false;
                }
            }
            return true;
        }
    };
    //=============================================================================
    // Parse the model-level "busTypes" array into a registry. Member format:
    //   { "name": "pos", "type": "double", "dims": [3], "complex": false }
    //   { "name": "sub", "bus": "OtherBusName" }   (nested bus, by name)
    // Two passes so nested references may appear in any declaration order.
    // Returns false + err on an invalid definition.
    NLSNFLOW_BLOCKS_IMPEXP bool
    parseBusRegistry(const json& diagram, BusRegistry& reg, std::string& err);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
