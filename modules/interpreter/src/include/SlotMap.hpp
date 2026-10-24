//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class SlotKind : uint8_t
{
    Local,
    Global,
    Arg
};
//=============================================================================
struct SlotInfo
{
    uint16_t index = 0;
    SlotKind kind = SlotKind::Local;
    bool everAssigned = false;
};
//=============================================================================
class SlotMap
{
public:
    explicit SlotMap(bool isScript);

    void
    addArg(const std::string& name, uint16_t slot);

    std::optional<SlotInfo>
    resolve(const std::string& name);

    // resolve() allocates a slot for unknown names; peek only reports an
    // existing binding so probes do not add the name to the slot tables
    std::optional<SlotInfo>
    peek(const std::string& name) const
    {
        auto found = map_.find(name);
        if (found != map_.end()) {
            return found->second;
        }
        return std::nullopt;
    }

    // Allocates an anonymous local slot for compiler-internal temporaries.
    // The slot carries an empty name so it never leaks into the workspace
    // (the context-sync loops skip empty-named slots).
    uint16_t
    allocTempSlot();

    void
    forceGlobal(const std::string& name);

    void
    markAssigned(const std::string& name);

    uint16_t
    totalSlots() const
    {
        return nextSlot_;
    }

    std::vector<std::string>
    slotNames() const
    {
        return slotNames_;
    }

    std::vector<uint8_t>
    slotAssigned() const
    {
        return slotAssigned_;
    }

    bool
    hasEvalEscape() const
    {
        return hasEvalEscape_;
    }

    void
    setEvalEscape();

private:
    bool isScript_;
    bool hasEvalEscape_ = false;
    uint16_t nextSlot_ = 0;
    std::unordered_map<std::string, SlotInfo> map_;
    std::vector<std::string> slotNames_;
    std::vector<uint8_t> slotAssigned_;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
