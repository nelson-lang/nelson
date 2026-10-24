//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsNativeWidgetCapture.hpp"
#include <QtCore/QByteArray>
#include <QtCore/QString>
#include <QtGui/QGuiApplication>
#if defined(__APPLE__)
#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CGSession.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isTruthyEnvironmentValue(const char* name)
{
    QByteArray value = qgetenv(name);
    if (value.isEmpty()) {
        return false;
    }
    QString normalized = QString::fromUtf8(value).trimmed().toLower();
    return normalized != QStringLiteral("0") && normalized != QStringLiteral("false")
        && normalized != QStringLiteral("no") && normalized != QStringLiteral("off");
}
//=============================================================================
static bool
hasRemoteSessionEnvironment()
{
    static const char* remoteSessionVariables[]
        = { "CHROME_REMOTE_DESKTOP_SESSION", "CHROME_REMOTE_DESKTOP", "CRD_SESSION", "VNC_SESSION",
              "NXSESSIONID", "SSH_CONNECTION", "SSH_TTY", "REMOTEHOST" };

    for (const char* name : remoteSessionVariables) {
        if (isTruthyEnvironmentValue(name)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
hasRemoteOrHeadlessPlatform()
{
    QString platformName = QGuiApplication::platformName().toLower();
    return platformName == QStringLiteral("offscreen") || platformName == QStringLiteral("minimal")
        || platformName.contains(QStringLiteral("vnc"))
        || platformName.contains(QStringLiteral("remote"));
}
//=============================================================================
#if defined(__APPLE__)
static bool
isMacConsoleSession()
{
    CFDictionaryRef session = CGSessionCopyCurrentDictionary();
    if (!session) {
        return true;
    }

    bool onConsole = true;
    const void* value = CFDictionaryGetValue(session, kCGSessionOnConsoleKey);
    if (value && CFGetTypeID(value) == CFBooleanGetTypeID()) {
        onConsole = CFBooleanGetValue(reinterpret_cast<CFBooleanRef>(value));
    }
    CFRelease(session);
    return onConsole;
}
#endif
//=============================================================================
bool
shouldAvoidNativeWidgetCapture()
{
    if (isTruthyEnvironmentValue("NELSON_FORCE_NATIVE_WIDGET_CAPTURE")) {
        return false;
    }
    if (isTruthyEnvironmentValue("NELSON_DISABLE_NATIVE_WIDGET_CAPTURE")) {
        return true;
    }
    if (hasRemoteOrHeadlessPlatform() || hasRemoteSessionEnvironment()) {
        return true;
    }
#if defined(__APPLE__)
    if (!isMacConsoleSession()) {
        return true;
    }
#endif
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
