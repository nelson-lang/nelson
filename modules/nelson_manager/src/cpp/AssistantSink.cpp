//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AssistantSink.hpp"
#include <atomic>
#include <map>
#include <mutex>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::mutex&
sinkMutex()
{
    static std::mutex m;
    return m;
}
//=============================================================================
static AssistantSinkFn&
theSink()
{
    static AssistantSinkFn sink;
    return sink;
}
//=============================================================================
void
SetAssistantSink(const AssistantSinkFn& sink)
{
    std::lock_guard<std::mutex> lock(sinkMutex());
    theSink() = sink;
}
//=============================================================================
void
AssistantSinkPush(const std::string& channel, const std::string& payloadJson)
{
    AssistantSinkFn sink;
    {
        std::lock_guard<std::mutex> lock(sinkMutex());
        sink = theSink();
    }
    if (sink) {
        sink(channel, payloadJson);
    }
}
//=============================================================================
static AssistantSinkFn&
theNFlowSink()
{
    static AssistantSinkFn sink;
    return sink;
}
//=============================================================================
void
SetNFlowAssistantSink(const AssistantSinkFn& sink)
{
    std::lock_guard<std::mutex> lock(sinkMutex());
    theNFlowSink() = sink;
}
//=============================================================================
void
NFlowAssistantSinkPush(const std::string& channel, const std::string& payloadJson)
{
    AssistantSinkFn sink;
    {
        std::lock_guard<std::mutex> lock(sinkMutex());
        sink = theNFlowSink();
    }
    if (sink) {
        sink(channel, payloadJson);
    }
}
//=============================================================================
static std::atomic<bool>&
assistantStopFlag()
{
    static std::atomic<bool> flag { false };
    return flag;
}
//=============================================================================
void
RequestAssistantStop()
{
    assistantStopFlag().store(true);
}
//=============================================================================
bool
ConsumeAssistantStop()
{
    return assistantStopFlag().exchange(false);
}
//=============================================================================
// Per-surface routing (the model behind isolated assistant sessions): each
// surface (qt / web / nflow / notebook) registers its own sink under a surfaceId
// so events reach only the surface they belong to, and Stop is per-surface.
static std::map<std::string, AssistantSinkFn>&
sinkRegistry()
{
    static std::map<std::string, AssistantSinkFn> registry;
    return registry;
}
//=============================================================================
void
RegisterAssistantSink(const std::string& surfaceId, const AssistantSinkFn& sink)
{
    std::lock_guard<std::mutex> lock(sinkMutex());
    sinkRegistry()[surfaceId] = sink;
}
//=============================================================================
void
UnregisterAssistantSink(const std::string& surfaceId)
{
    std::lock_guard<std::mutex> lock(sinkMutex());
    sinkRegistry().erase(surfaceId);
}
//=============================================================================
void
AssistantSinkPushTo(
    const std::string& surfaceId, const std::string& channel, const std::string& payloadJson)
{
    AssistantSinkFn sink;
    {
        std::lock_guard<std::mutex> lock(sinkMutex());
        auto it = sinkRegistry().find(surfaceId);
        if (it != sinkRegistry().end()) {
            sink = it->second;
        }
    }
    if (sink) {
        sink(channel, payloadJson);
    }
}
//=============================================================================
void
AssistantSinkBroadcast(const std::string& channel, const std::string& payloadJson)
{
    std::vector<AssistantSinkFn> sinks;
    {
        std::lock_guard<std::mutex> lock(sinkMutex());
        for (const auto& entry : sinkRegistry()) {
            sinks.push_back(entry.second);
        }
    }
    for (const auto& sink : sinks) {
        if (sink) {
            sink(channel, payloadJson);
        }
    }
}
//=============================================================================
static std::map<std::string, bool>&
stopFlags()
{
    static std::map<std::string, bool> flags;
    return flags;
}
//=============================================================================
void
RequestAssistantStop(const std::string& surfaceId)
{
    std::lock_guard<std::mutex> lock(sinkMutex());
    stopFlags()[surfaceId] = true;
}
//=============================================================================
bool
ConsumeAssistantStop(const std::string& surfaceId)
{
    std::lock_guard<std::mutex> lock(sinkMutex());
    auto it = stopFlags().find(surfaceId);
    if (it != stopFlags().end() && it->second) {
        it->second = false;
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
