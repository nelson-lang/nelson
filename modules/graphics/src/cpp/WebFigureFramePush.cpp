//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureFramePush.hpp"
#include "WebDesktopBridge.hpp"
#include "WebFigureBridgeExports.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct WebFigureAttachmentStore
{
    std::vector<std::string> ids;
    std::vector<std::vector<unsigned char>> bytes;
};
//=============================================================================
static void
appendJsonChunk(void* ctx, const char* data, int len)
{
    static_cast<std::string*>(ctx)->append(data, static_cast<size_t>(len));
}
//=============================================================================
static void
appendAttachment(void* ctx, const char* id, const unsigned char* data, int len)
{
    auto* store = static_cast<WebFigureAttachmentStore*>(ctx);
    if (id == nullptr || data == nullptr || len <= 0) {
        return;
    }
    store->ids.emplace_back(id);
    store->bytes.emplace_back(data, data + len);
}
//=============================================================================
bool
pushWebFigureAnimationFrame(WebDesktopBridge* bridge, int64_t handle)
{
    if (bridge == nullptr || !bridge->hasWebFigureFrameHook()) {
        return false;
    }
    std::string json;
    bool ok = false;
    WebFigureAttachmentStore store;
    if (bridge->hasWebFigureFrameAttachmentHook()) {
        ok = nlsWebGuiRenderFigureAnimationFrameWithAttachments(
            handle, appendJsonChunk, &json, appendAttachment, &store);
    } else {
        ok = nlsWebGuiRenderFigureAnimationFrame(handle, appendJsonChunk, &json);
    }
    if (!ok || json.empty()) {
        return false;
    }
    if (store.ids.empty()) {
        bridge->pushWebFigureFrame(handle, json.data(), static_cast<int>(json.size()));
        return true;
    }
    std::vector<WebFigureFrameAttachment> attachments;
    attachments.reserve(store.ids.size());
    for (size_t i = 0; i < store.ids.size(); ++i) {
        attachments.push_back({ store.ids[i].c_str(), store.bytes[i].data(),
            static_cast<int>(store.bytes[i].size()) });
    }
    bridge->pushWebFigureFrame(handle, json.data(), static_cast<int>(json.size()),
        attachments.data(), static_cast<int>(attachments.size()));
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
