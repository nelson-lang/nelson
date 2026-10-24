//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "TimeZone.hpp"
#include "IEEEFP.hpp"
#include "StringHelpers.hpp"
#include "date/ptz.h"
#include "date/tz.h"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <ctime>
#include <mutex>
#include <stdexcept>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    constexpr double UNIX_EPOCH_DATE_NUMBER = 719529.0;
    constexpr double SECONDS_PER_DAY = 86400.0;
    //=============================================================================
    struct TzdbState
    {
        std::string dataPath;
        bool loaded = false;
    };
    //=============================================================================
    struct ZoneRule
    {
        const char* name;
        const char* posixRule;
    };
    //=============================================================================
    const std::vector<ZoneRule>&
    namedZoneRules()
    {
        static const std::vector<ZoneRule> rules = {
            { "UTC", "UTC0" },
            { "Etc/UTC", "UTC0" },
            { "UTCLeapSeconds", "UTC0" },
            { "Europe/London", "GMT0BST,M3.5.0/1,M10.5.0/2" },
            { "Europe/Paris", "CET-1CEST,M3.5.0/2,M10.5.0/3" },
            { "Europe/Berlin", "CET-1CEST,M3.5.0/2,M10.5.0/3" },
            { "Europe/Madrid", "CET-1CEST,M3.5.0/2,M10.5.0/3" },
            { "Europe/Rome", "CET-1CEST,M3.5.0/2,M10.5.0/3" },
            { "America/New_York", "EST5EDT,M3.2.0/2,M11.1.0/2" },
            { "America/Chicago", "CST6CDT,M3.2.0/2,M11.1.0/2" },
            { "America/Denver", "MST7MDT,M3.2.0/2,M11.1.0/2" },
            { "America/Los_Angeles", "PST8PDT,M3.2.0/2,M11.1.0/2" },
            { "Asia/Tokyo", "JST-9" },
            { "Asia/Hong_Kong", "HKT-8" },
            { "Asia/Shanghai", "CST-8" },
            { "Asia/Kolkata", "IST-5:30" },
            { "Australia/Sydney", "AEST-10AEDT,M10.1.0/2,M4.1.0/3" },
        };
        return rules;
    }
    //=============================================================================
    TzdbState&
    tzdbState()
    {
        static TzdbState state;
        return state;
    }
    //=============================================================================
    std::mutex&
    tzdbMutex()
    {
        static std::mutex mutex;
        return mutex;
    }
    //=============================================================================
    std::string
    normalizeZoneName(const std::string& zoneName)
    {
        std::string name = StringHelpers::trim_copy(zoneName);
        if (StringHelpers::iequals(name, "z")) {
            return "UTC";
        }
        if (StringHelpers::iequals(name, "gmt")) {
            return "UTC";
        }
        if (StringHelpers::iequals(name, "utc")) {
            return "UTC";
        }
        if (StringHelpers::iequals(name, "local")) {
            return "local";
        }
        return name;
    }
    //=============================================================================
    bool
    hasTimeZoneDataPath()
    {
        std::lock_guard<std::mutex> lock(tzdbMutex());
        return !tzdbState().dataPath.empty();
    }
    //=============================================================================
    bool
    tryGetTzdb(const date::tzdb*& db)
    {
        if (!hasTimeZoneDataPath()) {
            return false;
        }
        try {
            db = &date::get_tzdb();
            std::lock_guard<std::mutex> lock(tzdbMutex());
            tzdbState().loaded = true;
            return true;
        } catch (const std::exception&) {
            db = nullptr;
            return false;
        }
    }
    //=============================================================================
    bool
    parseFixedOffset(const std::string& zoneName, int& seconds)
    {
        if (zoneName.size() != 6 || (zoneName[0] != '+' && zoneName[0] != '-')
            || zoneName[3] != ':') {
            return false;
        }
        if (!std::isdigit(zoneName[1]) || !std::isdigit(zoneName[2]) || !std::isdigit(zoneName[4])
            || !std::isdigit(zoneName[5])) {
            return false;
        }
        int hours = (zoneName[1] - '0') * 10 + (zoneName[2] - '0');
        int minutes = (zoneName[4] - '0') * 10 + (zoneName[5] - '0');
        if (hours > 24 || minutes > 59 || (hours == 24 && minutes != 0)) {
            return false;
        }
        seconds = hours * 3600 + minutes * 60;
        if (zoneName[0] == '-') {
            seconds = -seconds;
        }
        return true;
    }
    //=============================================================================
    time_t
    serialToUnixTime(double serialDateNumber)
    {
        double unixSeconds = (serialDateNumber - UNIX_EPOCH_DATE_NUMBER) * SECONDS_PER_DAY;
        return static_cast<time_t>(std::llround(unixSeconds));
    }
    //=============================================================================
    date::local_seconds
    serialToLocalSeconds(double serialDateNumber)
    {
        return date::local_seconds { std::chrono::seconds { serialToUnixTime(serialDateNumber) } };
    }
    //=============================================================================
    date::sys_seconds
    serialToSysSeconds(double serialDateNumber)
    {
        return date::sys_seconds { std::chrono::seconds { serialToUnixTime(serialDateNumber) } };
    }
    //=============================================================================
    template <class Zone>
    void
    offsetOf(const Zone& zone, double serialDateNumber, bool atUtc, int& offsetSeconds, bool& isDst)
    {
        date::sys_info info;
        if (atUtc) {
            info = zone.get_info(serialToSysSeconds(serialDateNumber));
        } else {
            // a time skipped by a DST gap takes the offset before the gap, an
            // ambiguous time the later occurrence (standard time).
            date::local_info local = zone.get_info(serialToLocalSeconds(serialDateNumber));
            info = local.result == date::local_info::ambiguous ? local.second : local.first;
        }
        offsetSeconds = static_cast<int>(info.offset.count());
        isDst = info.save != std::chrono::minutes { 0 };
    }
    //=============================================================================
    bool
    tzdbInfo(double serialDateNumber, const std::string& zoneName, bool atUtc, int& offsetSeconds,
        bool& isDst)
    {
        const date::tzdb* db = nullptr;
        if (!tryGetTzdb(db)) {
            return false;
        }
        try {
            const date::time_zone* zone = db->locate_zone(zoneName);
            offsetOf(*zone, serialDateNumber, atUtc, offsetSeconds, isDst);
            return true;
        } catch (const std::exception&) {
            return false;
        }
    }
    //=============================================================================
    bool
    localTimeInfo(double serialDateNumber, int& offsetSeconds, bool& isDst)
    {
        // the C library resolves the system time zone from a UTC instant.
        time_t unixTime = serialToUnixTime(serialDateNumber);
        std::tm localTm {};
#ifdef _MSC_VER
        if (localtime_s(&localTm, &unixTime) != 0) {
            return false;
        }
        __time64_t localAsUtc = _mkgmtime64(&localTm);
#else
        if (localtime_r(&unixTime, &localTm) == nullptr) {
            return false;
        }
        time_t localAsUtc = timegm(&localTm);
#endif
        offsetSeconds = static_cast<int>(localAsUtc - unixTime);
        isDst = localTm.tm_isdst > 0;
        return true;
    }
    //=============================================================================
    const char*
    findPosixRule(const std::string& zoneName)
    {
        for (const auto& rule : namedZoneRules()) {
            if (zoneName == rule.name) {
                return rule.posixRule;
            }
        }
        return nullptr;
    }
    //=============================================================================
    bool
    posixInfo(
        double serialDateNumber, const char* rule, bool atUtc, int& offsetSeconds, bool& isDst)
    {
        try {
            Posix::time_zone zone { std::string(rule) };
            offsetOf(zone, serialDateNumber, atUtc, offsetSeconds, isDst);
            return true;
        } catch (const std::exception&) {
            return false;
        }
    }
    //=============================================================================
    bool
    zoneInfo(double serialDateNumber, const std::string& zoneName, bool atUtc, int& offsetSeconds,
        bool& isDst)
    {
        if (!IsFinite(serialDateNumber)) {
            offsetSeconds = 0;
            isDst = false;
            return false;
        }
        std::string normalized = normalizeZoneName(zoneName);
        if (normalized.empty()) {
            offsetSeconds = 0;
            isDst = false;
            return true;
        }
        if (normalized == "local") {
            return localTimeInfo(serialDateNumber, offsetSeconds, isDst);
        }
        if (parseFixedOffset(normalized, offsetSeconds)) {
            isDst = false;
            return true;
        }
        if (tzdbInfo(serialDateNumber, normalized, atUtc, offsetSeconds, isDst)) {
            return true;
        }
        const char* rule = findPosixRule(normalized);
        if (rule != nullptr) {
            return posixInfo(serialDateNumber, rule, atUtc, offsetSeconds, isDst);
        }
        return posixInfo(serialDateNumber, normalized.c_str(), atUtc, offsetSeconds, isDst);
    }
    //=============================================================================
    void
    checkedZoneInfo(double serialDateNumber, const std::string& zoneName, bool atUtc,
        int& offsetSeconds, bool& isDst)
    {
        if (!zoneInfo(serialDateNumber, zoneName, atUtc, offsetSeconds, isDst)) {
            if (!IsFinite(serialDateNumber)) {
                offsetSeconds = 0;
                isDst = false;
                return;
            }
            throw std::invalid_argument("Invalid time zone.");
        }
    }
    //=============================================================================
    bool
    normalizeOffsetName(const std::string& name, std::string& offsetName)
    {
        // '[GMT|UTC]+H', '+HH', '+HHMM' or '+HH:MM' (any case) as '+HH:MM'.
        std::string text = name;
        if (text.size() > 3
            && (StringHelpers::iequals(text.substr(0, 3), "gmt")
                || StringHelpers::iequals(text.substr(0, 3), "utc"))) {
            text = text.substr(3);
        }
        if (text.size() < 2 || (text[0] != '+' && text[0] != '-')) {
            return false;
        }
        std::string digits = text.substr(1);
        size_t colon = digits.find(':');
        std::string hours = colon == std::string::npos ? digits : digits.substr(0, colon);
        std::string minutes = colon == std::string::npos ? "" : digits.substr(colon + 1);
        if (colon == std::string::npos && digits.size() > 2) {
            hours = digits.substr(0, digits.size() - 2);
            minutes = digits.substr(digits.size() - 2);
        }
        if (hours.empty() || hours.size() > 2
            || (colon != std::string::npos && minutes.size() != 2)) {
            return false;
        }
        if (minutes.empty()) {
            minutes = "00";
        }
        if (hours.size() == 1) {
            hours = "0" + hours;
        }
        offsetName = std::string(1, text[0]) + hours + ":" + minutes;
        int offsetSeconds = 0;
        return parseFixedOffset(offsetName, offsetSeconds);
    }
    //=============================================================================
    std::string
    tzdbZoneName(const date::time_zone& zone)
    {
        // 'Etc/...' zones are fixed offsets: 'Etc/GMT+5' is named '-05:00'.
        const std::string& name = zone.name();
        if (name.rfind("Etc/", 0) != 0) {
            return name;
        }
        date::sys_info info = zone.get_info(date::sys_seconds {});
        long long total = static_cast<long long>(info.offset.count());
        char sign = total < 0 ? '-' : '+';
        total = total < 0 ? -total : total;
        char buffer[8];
        std::snprintf(
            buffer, sizeof(buffer), "%c%02lld:%02lld", sign, total / 3600, (total % 3600) / 60);
        return buffer;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
TimeZoneSetDataPath(const std::string& dataPath)
{
    std::string path = StringHelpers::trim_copy(dataPath);
    if (path.empty()) {
        return;
    }
    std::lock_guard<std::mutex> lock(tzdbMutex());
    TzdbState& state = tzdbState();
    if (state.dataPath == path) {
        return;
    }
    date::set_install(path);
    state.dataPath = path;
    if (state.loaded) {
        try {
            date::reload_tzdb();
        } catch (const std::exception&) {
            state.loaded = false;
        }
    }
}
//=============================================================================
int
TimeZoneOffsetSeconds(double serialDateNumber, const std::string& timeZoneName)
{
    int offsetSeconds = 0;
    bool isDst = false;
    checkedZoneInfo(serialDateNumber, timeZoneName, false, offsetSeconds, isDst);
    return offsetSeconds;
}
//=============================================================================
int
TimeZoneOffsetSecondsAtUtc(double utcSerialDateNumber, const std::string& timeZoneName)
{
    int offsetSeconds = 0;
    bool isDst = false;
    checkedZoneInfo(utcSerialDateNumber, timeZoneName, true, offsetSeconds, isDst);
    return offsetSeconds;
}
//=============================================================================
bool
TimeZoneIsDst(double serialDateNumber, const std::string& timeZoneName)
{
    int offsetSeconds = 0;
    bool isDst = false;
    checkedZoneInfo(serialDateNumber, timeZoneName, false, offsetSeconds, isDst);
    return isDst;
}
//=============================================================================
std::string
TimeZoneCanonicalName(const std::string& timeZoneName)
{
    std::string name = StringHelpers::trim_copy(timeZoneName);
    if (name.empty() || name == "UTCLeapSeconds") {
        return name;
    }
    if (StringHelpers::iequals(name, "utc") || StringHelpers::iequals(name, "gmt")) {
        return "UTC";
    }
    if (StringHelpers::iequals(name, "z")) {
        return "+00:00";
    }
    std::string offsetName;
    if (normalizeOffsetName(name, offsetName)) {
        return offsetName;
    }
    bool isLocal = StringHelpers::iequals(name, "local");
    const date::tzdb* db = nullptr;
    if (tryGetTzdb(db)) {
        try {
            const date::time_zone* zone = isLocal ? date::current_zone() : db->locate_zone(name);
            return tzdbZoneName(*zone);
        } catch (const std::exception&) {
            if (isLocal) {
                return "local";
            }
        }
    } else if (isLocal || findPosixRule(name) != nullptr) {
        return isLocal ? "local" : name;
    }
    throw std::invalid_argument("Unknown time zone.");
}
//=============================================================================
std::vector<std::string>
TimeZoneList()
{
    const date::tzdb* db = nullptr;
    if (tryGetTzdb(db)) {
        std::vector<std::string> names;
        names.reserve(db->zones.size() + db->links.size() + 1);
        names.push_back("local");
        for (const auto& zone : db->zones) {
            names.push_back(zone.name());
        }
        for (const auto& link : db->links) {
            names.push_back(link.name());
        }
        std::sort(names.begin(), names.end());
        names.erase(std::unique(names.begin(), names.end()), names.end());
        return names;
    }

    std::vector<std::string> names;
    names.reserve(namedZoneRules().size() + 1);
    names.push_back("local");
    for (const auto& rule : namedZoneRules()) {
        names.push_back(rule.name);
    }
    return names;
}
//=============================================================================
std::string
TimeZoneDataVersion()
{
    const date::tzdb* db = nullptr;
    if (tryGetTzdb(db)) {
        return db->version;
    }
    return "date-3.0.4-posix";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
