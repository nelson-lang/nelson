//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIHTML.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOStringProperty.hpp"
#include "JsonDecode.hpp"
#include "JsonEncode.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
encodeToJson(const ArrayOf& value);
//=============================================================================
GOUIHTML::GOUIHTML()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIHTML::getType()
{
    return GO_PROPERTY_VALUE_UIHTML_STR;
}
//=============================================================================
std::string
GOUIHTML::getClassName()
{
    return GO_CLASS_NAME_UI_HTML_STR;
}
//=============================================================================
void
GOUIHTML::updateState()
{
    if (hasChanged(GO_DATA_PROPERTY_NAME_STR)) {
        // Encoding touches the ArrayOf, so it happens here, on the interpreter
        // thread, and never in the reader the web layer calls.
        const std::string encoded = encodeCurrentData();
        std::lock_guard<std::mutex> lock(sharedMutex);
        // An equal write notifies nobody: the page is only told about values it
        // does not already hold.
        if (!published || encoded != publishedDataJson) {
            publishedDataJson = encoded;
            published = true;
            ++revision;
        }
    }
    if (hasChanged(GO_HTML_SOURCE_PROPERTY_NAME_STR)) {
        // A new source means a new page, which pulls the current Data as part
        // of its handshake, so the value has to be published again.
        std::lock_guard<std::mutex> lock(sharedMutex);
        ++revision;
    }
    GOUIComponent::updateState();
}
//=============================================================================
std::string
GOUIHTML::publishedData()
{
    std::lock_guard<std::mutex> lock(sharedMutex);
    return publishedDataJson;
}
//=============================================================================
uint64_t
GOUIHTML::dataRevision()
{
    std::lock_guard<std::mutex> lock(sharedMutex);
    return revision;
}
//=============================================================================
uint64_t
GOUIHTML::eventsQueuedCount()
{
    std::lock_guard<std::mutex> lock(sharedMutex);
    return eventsQueued;
}
//=============================================================================
std::string
GOUIHTML::encodeCurrentData()
{
    GOArrayOfProperty* property
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DATA_PROPERTY_NAME_STR, false));
    if (property == nullptr) {
        return "[]";
    }
    return encodeToJson(property->data());
}
//=============================================================================
void
GOUIHTML::onPageDataChanged(const std::string& dataJson)
{
    std::string decodeError;
    ArrayOf decoded = jsonDecode(dataJson, decodeError);
    if (!decodeError.empty()) {
        return;
    }
    GOArrayOfProperty* property
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DATA_PROPERTY_NAME_STR, false));
    if (property == nullptr) {
        return;
    }
    ArrayOf previous = property->data();
    // The page already holds this value, so record it as published: pushing it
    // back down is exactly the echo the contract forbids.
    property->data(decoded);
    {
        std::lock_guard<std::mutex> lock(sharedMutex);
        publishedDataJson = dataJson;
        published = true;
    }
    clearChanged(GO_DATA_PROPERTY_NAME_STR);

    ArrayOfVector fieldValues;
    fieldValues.push_back(decoded);
    fieldValues.push_back(previous);
    emitCallbackEvent(GO_DATA_CHANGED_FCN_PROPERTY_NAME_STR, L"DataChangedData", L"DataChanged",
        { L"Data", L"PreviousData" }, fieldValues, true);
}
//=============================================================================
void
GOUIHTML::onPageEvent(const std::string& eventName, const std::string& eventDataJson)
{
    std::string decodeError;
    ArrayOf decoded = jsonDecode(eventDataJson.empty() ? "null" : eventDataJson, decodeError);
    if (!decodeError.empty()) {
        decoded = ArrayOf::empty();
    }
    ArrayOfVector fieldValues;
    fieldValues.push_back(ArrayOf::characterArray(eventName));
    fieldValues.push_back(decoded);
    emitCallbackEvent(GO_HTML_EVENT_RECEIVED_FCN_PROPERTY_NAME_STR, L"HTMLEventReceivedData",
        L"HTMLEventReceived", { L"HTMLEventName", L"HTMLEventData" }, fieldValues, true);
}
//=============================================================================
bool
GOUIHTML::queueEventToPage(const std::string& eventName, const ArrayOf& eventData)
{
    if (eventName.empty()) {
        return false;
    }
    const std::string encoded = encodeToJson(eventData);
    std::lock_guard<std::mutex> lock(sharedMutex);
    pendingEventsToPage.emplace_back(eventName, encoded);
    ++eventsQueued;
    return true;
}
//=============================================================================
std::vector<std::pair<std::string, std::string>>
GOUIHTML::drainEventsToPage()
{
    std::vector<std::pair<std::string, std::string>> drained;
    std::lock_guard<std::mutex> lock(sharedMutex);
    drained.swap(pendingEventsToPage);
    return drained;
}
//=============================================================================
void
GOUIHTML::constructProperties()
{
    // No fonts and no background colour: the page owns its own styling.
    registerUIComponentCommonProperties(false, false);
    registerProperty(new GOStringProperty, GO_HTML_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DATA_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_HTML_EVENT_RECEIVED_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUIHTML::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 100, 100, false, false);
    setStringDefault(GO_HTML_SOURCE_PROPERTY_NAME_STR, L"");
    GOArrayOfProperty* data
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DATA_PROPERTY_NAME_STR, false));
    if (data != nullptr) {
        // Empty double, which the page sees as [].
        data->data(ArrayOf::empty());
    }
    publishedDataJson = "[]";
    published = true;
}
//=============================================================================
static std::string
encodeToJson(const ArrayOf& value)
{
    std::wstring encodeError;
    ArrayOf encoded = jsonEncode(value, true, encodeError);
    if (!encodeError.empty() || !encoded.isRowVectorCharacterArray()) {
        // Values JSON cannot represent (complex numbers, handles) leave the
        // page with an empty value rather than a broken document.
        return "[]";
    }
    return wstring_to_utf8(encoded.asWideString());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
