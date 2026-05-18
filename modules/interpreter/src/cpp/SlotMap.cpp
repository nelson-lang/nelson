//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SlotMap.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
SlotMap::SlotMap(bool isScript) : isScript_(isScript) { }
//=============================================================================
void
SlotMap::addArg(const std::string& name, uint16_t slot)
{
    map_[name] = SlotInfo { slot, SlotKind::Arg, true };
    if (slot >= slotNames_.size()) {
        slotNames_.resize(static_cast<size_t>(slot) + 1);
    }
    slotNames_[slot] = name;
    if (slot >= slotAssigned_.size()) {
        slotAssigned_.resize(static_cast<size_t>(slot) + 1, 0);
    }
    slotAssigned_[slot] = 1;
    if (slot >= nextSlot_) {
        nextSlot_ = static_cast<uint16_t>(slot + 1);
    }
}
//=============================================================================
std::optional<SlotInfo>
SlotMap::resolve(const std::string& name)
{
    auto found = map_.find(name);
    if (found != map_.end()) {
        return found->second;
    }
    if (hasEvalEscape_) {
        return SlotInfo { 0, SlotKind::Global, false };
    }
    SlotInfo info { nextSlot_++, SlotKind::Local, false };
    map_[name] = info;
    if (info.index >= slotNames_.size()) {
        slotNames_.resize(static_cast<size_t>(info.index) + 1);
    }
    slotNames_[info.index] = name;
    if (info.index >= slotAssigned_.size()) {
        slotAssigned_.resize(static_cast<size_t>(info.index) + 1, 0);
    }
    return info;
}
//=============================================================================
void
SlotMap::forceGlobal(const std::string& name)
{
    map_[name] = SlotInfo { 0, SlotKind::Global, true };
}
//=============================================================================
void
SlotMap::markAssigned(const std::string& name)
{
    auto found = map_.find(name);
    if (found != map_.end()) {
        found->second.everAssigned = true;
        if (found->second.kind != SlotKind::Global) {
            if (found->second.index >= slotAssigned_.size()) {
                slotAssigned_.resize(static_cast<size_t>(found->second.index) + 1, 0);
            }
            slotAssigned_[found->second.index] = 1;
        }
    }
}
//=============================================================================
void
SlotMap::setEvalEscape()
{
    hasEvalEscape_ = true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
