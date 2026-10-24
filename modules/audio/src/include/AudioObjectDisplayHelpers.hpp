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
#include <algorithm>
#include <string>
#include <vector>
#include "Interface.hpp"
#include "Types.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct AudioDisplayItem
{
    std::wstring label;
    std::wstring value;
};
//=============================================================================
inline size_t
audioDisplayLabelWidth(const std::vector<AudioDisplayItem>& items, const wstringVector& labels)
{
    size_t width = 0;
    for (const std::wstring& label : labels) {
        auto item = std::find_if(items.begin(), items.end(),
            [&](const AudioDisplayItem& entry) { return entry.label == label; });
        if (item != items.end()) {
            width = std::max(width, label.length());
        }
    }
    return width;
}
//=============================================================================
inline std::wstring
audioDisplayLines(const std::vector<AudioDisplayItem>& items, const wstringVector& labels)
{
    size_t width = audioDisplayLabelWidth(items, labels);
    std::wstring text;
    for (const std::wstring& label : labels) {
        auto item = std::find_if(items.begin(), items.end(),
            [&](const AudioDisplayItem& entry) { return entry.label == label; });
        if (item != items.end()) {
            std::wstring padding(width > label.length() ? width - label.length() : 0, L' ');
            text += L"      " + padding + item->label + L": " + item->value + L"\n";
        }
    }
    return text;
}
//=============================================================================
inline bool
displayAudioObject(Interface* io, const std::wstring& objectName,
    const std::vector<AudioDisplayItem>& items, const wstringVector& compactLabels)
{
    if (!io) {
        return false;
    }
    wstringVector allLabels;
    allLabels.reserve(items.size());
    for (const AudioDisplayItem& item : items) {
        allLabels.push_back(item.label);
    }
    io->outputMessage(L"\n");
    io->outputMessage(L"  " + objectName + L" " + _W("with properties:") + L"\n\n");
    io->outputMessage(audioDisplayLines(items, compactLabels));
    io->outputMessage(L"\n");
    std::wstring expandedText = L"\n" + audioDisplayLines(items, allLabels);
    io->outputExpandableText(_W("Show all properties"), expandedText, expandedText);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
