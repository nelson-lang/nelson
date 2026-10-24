//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtCore/QEventLoop>
#include <QtCore/QThread>
#include <QtCore/QMetaObject>
#include <filesystem>
#include <string>
#include <vector>
#ifdef _WIN32
#include <mutex>
#include <windows.h>
#include <shobjidl.h>
#include <shlobj.h>
#pragma comment(lib, "ole32.lib")
#pragma comment(lib, "shell32.lib")
#else
#include <QtCore/QAbstractEventDispatcher>
#include <QtCore/QFileInfo>
#include <QtWidgets/QFileDialog>
#include <QtCore/QStringList>
#include "QStringConverter.hpp"
#endif
#include "GuiDialogProvider.hpp"
#include "DialogDirectories.hpp"
#include "characters_encoding.hpp"
//=============================================================================
// Bridges the OS file chooser to a Qt-free C entry point so a front end that
// does not link Qt (the NFlow editor, nflow_gui) can show the system dialog by
// resolving this symbol dynamically from libnlsGui.
//
// On Windows the chooser must be owned by the editor window (a native WebView
// window, not a Qt widget). Qt's foreign-window parent does not create a real
// Win32 owner, so the dialog was not modal and not reliably in front. The native
// IFileDialog is driven directly with Show(hwndOwner). macOS and Linux use
// QFileDialog.
//
// Threading: callers are webui RPC worker threads, never a Qt GUI thread. On
// Windows the IFileDialog runs on the caller thread. Elsewhere QFileDialog is
// marshalled to the thread that owns QApplication, and only when that thread
// pumps events; otherwise the dialog is reported unavailable and the front end
// falls back to its HTML picker. QApplication is never constructed here: one
// owned by a worker thread has no event loop, and a later cross-thread
// Qt::BlockingQueuedConnection call would deadlock. nlsGuiEnsureApplication is
// for the WebView host to call on its main thread before the first RPC; it then
// drains events through nlsGuiProcessEvents.
//=============================================================================
namespace Nelson {
//=============================================================================
static std::filesystem::path
dialogPath(const std::wstring& value)
{
#ifdef _WIN32
    return std::filesystem::path(value);
#else
    return std::filesystem::u8path(wstring_to_utf8(value));
#endif
}
//=============================================================================
static std::wstring
dialogPathString(const std::filesystem::path& value)
{
#ifdef _WIN32
    return value.wstring();
#else
    return utf8_to_wstring(value.generic_u8string());
#endif
}
//=============================================================================
static std::wstring
defaultHistoryKey(const GuiDialogRequest& request)
{
    if (!request.historyKey.empty()) {
        return request.historyKey;
    }
    return L"desktop."
        + (request.fileMode == L"dir" ? std::wstring(L"directory") : request.fileMode);
}
//=============================================================================
static std::wstring
resolvedDialogPath(const GuiDialogRequest& request, const std::wstring& historyKey)
{
    std::error_code ec;
    const std::filesystem::path current = std::filesystem::current_path(ec);
    const std::wstring fallback = ec ? std::wstring() : dialogPathString(current);
    std::wstring explicitDirectory = request.defaultPath;
    std::wstring suggestedName = request.suggestedName;
    if (request.fileMode == L"save" && !request.defaultPath.empty()
        && request.defaultPath != L".") {
        const std::filesystem::path requested = dialogPath(request.defaultPath);
        if (!std::filesystem::is_directory(requested, ec)) {
            if (suggestedName.empty()) {
                suggestedName = dialogPathString(requested.filename());
            }
            const std::filesystem::path parent = requested.parent_path();
            explicitDirectory = (parent.empty() || parent == std::filesystem::path("."))
                ? L""
                : dialogPathString(parent);
        }
    }
    const std::wstring directory = resolveDialogDirectory(historyKey, explicitDirectory, fallback);
    if (request.fileMode != L"save" || suggestedName.empty()) {
        return directory;
    }
    return dialogPathString(dialogPath(directory) / dialogPath(suggestedName));
}
//=============================================================================
static void
recordDialogResult(
    const std::wstring& historyKey, const std::wstring& mode, const std::wstring& resultPath)
{
    const std::filesystem::path selected = dialogPath(resultPath);
    const std::filesystem::path directory = mode == L"dir" ? selected : selected.parent_path();
    if (!directory.empty()) {
        setDialogDirectory(historyKey, dialogPathString(directory));
    }
}
//=============================================================================
#ifdef _WIN32
//=============================================================================
struct FilterSpec
{
    std::wstring name;
    std::wstring spec; // ';'-separated patterns, e.g. "*.nflow"
};
//=============================================================================
// Parses a Qt filter string ("Name (*.a *.b);;All Files (*)") into IFileDialog
// filter specs (patterns become ';'-separated).
static std::vector<FilterSpec>
parseQtFilters(const std::wstring& qtFilter)
{
    std::vector<FilterSpec> out;
    size_t start = 0;
    while (start <= qtFilter.size()) {
        const size_t sep = qtFilter.find(L";;", start);
        const std::wstring entry
            = qtFilter.substr(start, sep == std::wstring::npos ? std::wstring::npos : sep - start);
        if (!entry.empty()) {
            const size_t op = entry.find(L'(');
            const size_t cp
                = (op == std::wstring::npos) ? std::wstring::npos : entry.find(L')', op);
            std::wstring name = (op == std::wstring::npos) ? entry : entry.substr(0, op);
            while (!name.empty() && name.back() == L' ') {
                name.pop_back();
            }
            std::wstring patterns
                = (op != std::wstring::npos && cp != std::wstring::npos && cp > op)
                ? entry.substr(op + 1, cp - op - 1)
                : std::wstring(L"*");
            std::wstring spec;
            for (const wchar_t c : patterns) {
                spec += (c == L' ') ? L';' : c;
            }
            if (spec.empty()) {
                spec = L"*";
            }
            out.push_back({ name.empty() ? spec : name, spec });
        }
        if (sep == std::wstring::npos) {
            break;
        }
        start = sep + 2;
    }
    if (out.empty()) {
        out.push_back({ L"All Files", L"*" });
    }
    return out;
}
//=============================================================================
static void
setDialogFolder(IFileDialog* dialog, const std::wstring& folder)
{
    if (folder.empty()) {
        return;
    }
    std::wstring native = folder;
    for (wchar_t& c : native) {
        if (c == L'/') {
            c = L'\\';
        }
    }
    IShellItem* item = nullptr;
    if (SUCCEEDED(SHCreateItemFromParsingName(native.c_str(), nullptr, IID_PPV_ARGS(&item)))
        && item) {
        dialog->SetFolder(item);
        item->Release();
    }
}
//=============================================================================
static bool
runWin32FileDialog(HWND owner, const std::wstring& mode, const std::wstring& title,
    const std::wstring& defaultPath, const std::wstring& filter, std::wstring& outPath)
{
    outPath.clear();

    // The editor's WebView handle may be a child window (e.g. the WebView2
    // child) rather than the top-level frame. IFileDialog::Show only makes the
    // dialog modal to and owned by a TOP-LEVEL window, so climb to the root; if
    // the handle is missing/invalid, fall back to the current foreground window.
    HWND effectiveOwner = (owner && IsWindow(owner)) ? GetAncestor(owner, GA_ROOT) : nullptr;
    if (effectiveOwner == nullptr || !IsWindow(effectiveOwner)) {
        effectiveOwner = GetForegroundWindow();
    }

    const HRESULT hrInit
        = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
    const bool comInited = SUCCEEDED(hrInit);

    bool ok = false;
    const bool saveMode = (mode == L"save");
    const bool dirMode = (mode == L"dir");
    IFileDialog* dialog = nullptr;
    const CLSID clsid = saveMode ? CLSID_FileSaveDialog : CLSID_FileOpenDialog;
    if (SUCCEEDED(CoCreateInstance(clsid, nullptr, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&dialog)))
        && dialog) {
        DWORD options = 0;
        dialog->GetOptions(&options);
        options |= FOS_FORCEFILESYSTEM;
        if (dirMode) {
            options |= FOS_PICKFOLDERS;
        } else if (saveMode) {
            options |= FOS_OVERWRITEPROMPT;
        } else {
            options |= FOS_FILEMUSTEXIST | FOS_PATHMUSTEXIST;
        }
        dialog->SetOptions(options);

        if (!title.empty()) {
            dialog->SetTitle(title.c_str());
        }

        std::vector<FilterSpec> filters;
        std::vector<COMDLG_FILTERSPEC> specs;
        if (!dirMode) {
            filters = parseQtFilters(filter);
            specs.reserve(filters.size());
            for (const FilterSpec& f : filters) {
                specs.push_back({ f.name.c_str(), f.spec.c_str() });
            }
            if (!specs.empty()) {
                dialog->SetFileTypes(static_cast<UINT>(specs.size()), specs.data());
            }
        }

        if (saveMode) {
            std::wstring folder;
            std::wstring name = defaultPath;
            std::error_code ec;
            if (std::filesystem::is_directory(dialogPath(defaultPath), ec)) {
                folder = defaultPath;
                name.clear();
            } else {
                const size_t p = defaultPath.find_last_of(L"/\\");
                if (p != std::wstring::npos) {
                    folder = defaultPath.substr(0, p);
                    name = defaultPath.substr(p + 1);
                }
            }
            setDialogFolder(dialog, folder);
            if (!name.empty()) {
                dialog->SetFileName(name.c_str());
            }
        } else if (!defaultPath.empty() && defaultPath != L".") {
            setDialogFolder(dialog, defaultPath);
        }

        if (SUCCEEDED(dialog->Show(effectiveOwner))) {
            IShellItem* item = nullptr;
            if (SUCCEEDED(dialog->GetResult(&item)) && item) {
                PWSTR psz = nullptr;
                if (SUCCEEDED(item->GetDisplayName(SIGDN_FILESYSPATH, &psz)) && psz) {
                    outPath = psz;
                    CoTaskMemFree(psz);
                    ok = true;
                }
                item->Release();
            }
        }
        dialog->Release();
    }

    if (comInited) {
        CoUninitialize();
    }
    return ok;
}
#endif
//=============================================================================
#ifndef _WIN32
static std::wstring
runFileChooserOnGuiThread(void* parent, const std::wstring& mode, const std::wstring& title,
    const std::wstring& defaultPath, const std::wstring& filter)
{
    (void)parent;
    QFileDialog dialog(nullptr, wstringToQString(title));
    dialog.setWindowModality(Qt::ApplicationModal);
    const QStringList filters
        = wstringToQString(filter).split(QStringLiteral(";;"), Qt::SkipEmptyParts);
    const QString defaultQ = wstringToQString(defaultPath);
    if (mode == L"dir") {
        dialog.setFileMode(QFileDialog::Directory);
        dialog.setOption(QFileDialog::ShowDirsOnly, true);
        if (!defaultQ.isEmpty() && defaultQ != QStringLiteral(".")) {
            dialog.setDirectory(defaultQ);
        }
    } else if (mode == L"save") {
        dialog.setAcceptMode(QFileDialog::AcceptSave);
        dialog.setFileMode(QFileDialog::AnyFile);
        if (!filters.isEmpty()) {
            dialog.setNameFilters(filters);
        }
        if (QFileInfo(defaultQ).isDir()) {
            dialog.setDirectory(defaultQ);
        } else if (!defaultQ.isEmpty()) {
            dialog.selectFile(defaultQ);
        }
    } else {
        dialog.setAcceptMode(QFileDialog::AcceptOpen);
        dialog.setFileMode(QFileDialog::ExistingFile);
        if (!filters.isEmpty()) {
            dialog.setNameFilters(filters);
        }
        if (!defaultQ.isEmpty() && defaultQ != QStringLiteral(".")) {
            dialog.setDirectory(defaultQ);
        }
    }
    if (dialog.exec() == QDialog::Accepted) {
        const QStringList selected = dialog.selectedFiles();
        if (!selected.isEmpty()) {
            return QStringTowstring(selected.first());
        }
    }
    return std::wstring();
}
#endif
//=============================================================================
extern "C" NLSGUI_IMPEXP bool
nlsGuiRunFileDialog(void* parent, const GuiDialogRequest& request, GuiDialogResponse& response)
{
    response.ok = false;
    response.path.clear();

#ifdef _WIN32
    // When the editor has no local top-level owner, show the system chooser
    // unowned instead of rejecting the native dialog request.
    {
        HWND top = parent ? GetAncestor(reinterpret_cast<HWND>(parent), GA_ROOT) : nullptr;
        DWORD pid = 0;
        if (top) {
            GetWindowThreadProcessId(top, &pid);
        }
        if (top == nullptr || pid != GetCurrentProcessId()) {
            parent = nullptr;
        }
    }
#endif

    const std::wstring filter = request.fileFilters.empty() ? std::wstring(L"All Files (*)")
                                                            : request.fileFilters.front();
    const std::wstring historyKey = defaultHistoryKey(request);
    const std::wstring initialPath = resolvedDialogPath(request, historyKey);

    std::wstring resultPath;
#ifdef _WIN32
    // IFileDialog needs COM, not Qt: it runs on the calling RPC worker thread
    // (Show() has its own modal message loop). One dialog at a time, otherwise
    // concurrent scripted requests stack two OS dialogs.
    {
        static std::mutex dialogMutex;
        const std::lock_guard<std::mutex> guard(dialogMutex);
        runWin32FileDialog(reinterpret_cast<HWND>(parent), request.fileMode, request.title,
            initialPath, filter, resultPath);
    }
#else
    // QFileDialog must run on the thread that owns QApplication. No qApp means
    // the host did not initialize Qt on its main thread: report unavailable so
    // the front end falls back to its HTML picker.
    if (qApp == nullptr) {
        return false;
    }
    auto task = [&]() {
        resultPath = runFileChooserOnGuiThread(
            parent, request.fileMode, request.title, initialPath, filter);
    };
    if (QThread::currentThread() == qApp->thread()) {
        task();
    } else if (QAbstractEventDispatcher::instance(qApp->thread()) != nullptr) {
        // The qApp thread has an event dispatcher (the WebView host drains it
        // each main-loop turn), so the blocking queued call completes once the
        // user answers.
        QMetaObject::invokeMethod(qApp, task, Qt::BlockingQueuedConnection);
    } else {
        // qApp exists but its thread never created an event dispatcher: a
        // queued call would never run. Fall back instead of deadlocking.
        return false;
    }
#endif

    response.ok = !resultPath.empty();
    response.path = resultPath;
    if (response.ok) {
        recordDialogResult(historyKey, request.fileMode, resultPath);
    }
    return true;
}
//=============================================================================
extern "C" NLSGUI_IMPEXP bool
nlsGuiEnsureApplication(void)
{
    if (qApp != nullptr) {
        return true;
    }

    // The WebView host is not linked to Qt and normally has no QApplication.
    // Construct the optional application on its main thread so QFileDialog can
    // be invoked there through the regular Qt queued-call path.
    static int argc = 1;
    static char applicationName[] = "nelson";
    static char* argv[] = { applicationName, nullptr };
    try {
        static QApplication application(argc, argv);
        (void)application;
    } catch (...) {
        return false;
    }
    return qApp != nullptr;
}
//=============================================================================
extern "C" NLSGUI_IMPEXP void
nlsGuiProcessEvents(void)
{
    if (qApp != nullptr) {
        qApp->processEvents(QEventLoop::AllEvents, 0);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
