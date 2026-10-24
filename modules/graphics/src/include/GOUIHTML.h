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
#include "GOUIComponent.h"
#include <mutex>
//=============================================================================
namespace Nelson {
//=============================================================================
// uihtml: an HTML page hosted inside a uifigure, wired to the engine through
// the contract its page implements, i.e. a global setup(htmlComponent) that
// reads and writes htmlComponent.Data, listens for named events and sends its
// own back.
//
// Data crosses as JSON text in both directions, so the value that comes back is
// what jsonencode/jsondecode make of it: integers arrive as double, row vectors
// come back as columns, datetime and categorical arrive as text.
//
// Notification is asymmetric:
//   - a write from the engine notifies the page ("DataChanged") and NOT
//     DataChangedFcn;
//   - a write from the page fires DataChangedFcn and is NOT pushed back down.
// Writing a value equal to the current one notifies nobody.
class NLSGRAPHICS_IMPEXP GOUIHTML : public GOUIComponent
{
public:
    //=============================================================================
    GOUIHTML();
    ~GOUIHTML() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    // The JSON the page holds. Encoded once, when Data changes, on the
    // interpreter thread; this reader only copies a string. The web layer may
    // call it from a server thread, and touching an ArrayOf there would race
    // with the interpreter.
    std::string
    publishedData();
    //=============================================================================
    void
    onPageDataChanged(const std::string& dataJson);
    //=============================================================================
    void
    onPageEvent(const std::string& eventName, const std::string& eventDataJson);
    //=============================================================================
    // Monotonic stamp of the last Data value the engine published, so the page
    // can ignore an echo of a value it already has.
    uint64_t
    dataRevision();
    //=============================================================================
    // The web layer drains the queue on the next describe. Returns false when the name is
    // empty.
    bool
    queueEventToPage(const std::string& eventName, const ArrayOf& eventData);
    //=============================================================================
    std::vector<std::pair<std::string, std::string>>
    drainEventsToPage();
    //=============================================================================
    // Grows on every queued event, so the frontend sees that one is waiting without the
    // payload travelling in the display list.
    uint64_t
    eventsQueuedCount();
    //=============================================================================
private:
    //=============================================================================
    // Encodes the current Data. Interpreter thread only.
    std::string
    encodeCurrentData();
    void
    constructProperties();
    void
    setupDefaults();
    //=============================================================================
    // Kept to detect an equal write and to feed PreviousData.
    std::string publishedDataJson;
    uint64_t revision = 0;
    uint64_t eventsQueued = 0;
    bool published = false;
    std::vector<std::pair<std::string, std::string>> pendingEventsToPage;
    // Guards the members the web layer reads from a server thread.
    std::mutex sharedMutex;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
