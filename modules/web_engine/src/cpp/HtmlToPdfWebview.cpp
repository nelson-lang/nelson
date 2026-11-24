//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HtmlToPdfWebview.hpp"
#include "HtmlToPdfProvider.hpp"
#include "RenderHtmlHelpers.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
#ifdef __APPLE__
#include "MacHtmlRenderer.h"
#endif
#include <chrono>
#include <filesystem>
#include <string>
#include <thread>
#include <vector>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <csignal>
#include <sys/wait.h>
#include <unistd.h>
#endif
//=============================================================================
extern "C"
{
#include "webui.h"
    // NELSON PATCH entry point: resolve an installed browser's launch path.
    bool
    webui_get_browser_path(size_t browser, char* out, size_t len);
}
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
#ifndef __APPLE__
    //=============================================================================
    // Wait at most this long for the browser to render and print the page.
    constexpr int PRINT_TIMEOUT_MS = 60000;
    //=============================================================================
    std::filesystem::path
    temporaryProfilePath()
    {
        std::error_code ec;
        std::filesystem::path base = std::filesystem::temp_directory_path(ec);
        if (ec || base.empty()) {
            base = std::filesystem::current_path();
        }
        const auto stamp = std::chrono::high_resolution_clock::now().time_since_epoch().count();
        return base / ("nelson_htmltopdf_" + std::to_string(stamp));
    }
    //=============================================================================
    // Ask webui (maintained, registry-aware) for an installed Chromium browser.
    // Returns the executable path/command with any surrounding quotes removed.
    std::wstring
    findChromiumBrowser()
    {
        const size_t candidates[] = { Edge, Chrome, Chromium, Brave, Vivaldi, Epic, Yandex };
        for (size_t browser : candidates) {
            char path[4096] = { 0 };
            if (!webui_get_browser_path(browser, path, sizeof(path))) {
                continue;
            }
            std::string value(path);
            if (value.empty()) {
                continue;
            }
            // On macOS webui returns an `open -a ...` command that cannot be driven
            // with `--print-to-pdf`; skip it (conversion unsupported there for now).
            if (value.rfind("open ", 0) == 0) {
                continue;
            }
            if (value.size() >= 2 && value.front() == '"' && value.back() == '"') {
                value = value.substr(1, value.size() - 2);
            }
            return utf8_to_wstring(value);
        }
        return L"";
    }
    //=============================================================================
#ifdef _WIN32
    std::wstring
    quote(const std::wstring& value)
    {
        return L"\"" + value + L"\"";
    }
    //=============================================================================
    bool
    runBrowser(const std::wstring& browser, const std::vector<std::wstring>& arguments)
    {
        std::wstring commandLine = quote(browser);
        for (const std::wstring& argument : arguments) {
            commandLine += L" " + argument;
        }

        STARTUPINFOW startupInfo;
        ZeroMemory(&startupInfo, sizeof(startupInfo));
        startupInfo.cb = sizeof(startupInfo);
        PROCESS_INFORMATION processInfo;
        ZeroMemory(&processInfo, sizeof(processInfo));

        std::vector<wchar_t> buffer(commandLine.begin(), commandLine.end());
        buffer.push_back(L'\0');
        // CREATE_NO_WINDOW keeps the headless browser from flashing a console window.
        if (!CreateProcessW(nullptr, buffer.data(), nullptr, nullptr, FALSE, CREATE_NO_WINDOW,
                nullptr, nullptr, &startupInfo, &processInfo)) {
            return false;
        }
        DWORD wait = WaitForSingleObject(processInfo.hProcess, PRINT_TIMEOUT_MS);
        if (wait != WAIT_OBJECT_0) {
            TerminateProcess(processInfo.hProcess, 1);
            WaitForSingleObject(processInfo.hProcess, 2000);
        }
        CloseHandle(processInfo.hThread);
        CloseHandle(processInfo.hProcess);
        return wait == WAIT_OBJECT_0;
    }
#else
    //=============================================================================
    bool
    runBrowser(const std::wstring& browser, const std::vector<std::wstring>& arguments)
    {
        std::string browserUtf8 = wstring_to_utf8(browser);
        std::vector<std::string> argv;
        argv.push_back(browserUtf8);
        for (const std::wstring& argument : arguments) {
            argv.push_back(wstring_to_utf8(argument));
        }
        std::vector<char*> execArgs;
        for (std::string& argument : argv) {
            execArgs.push_back(argument.data());
        }
        execArgs.push_back(nullptr);

        pid_t pid = fork();
        if (pid < 0) {
            return false;
        }
        if (pid == 0) {
            execvp(browserUtf8.c_str(), execArgs.data());
            _exit(127);
        }

        const auto start = std::chrono::steady_clock::now();
        while (true) {
            int status = 0;
            pid_t result = waitpid(pid, &status, WNOHANG);
            if (result == pid) {
                return WIFEXITED(status) && WEXITSTATUS(status) == 0;
            }
            if (result < 0) {
                return false;
            }
            const auto elapsed = std::chrono::steady_clock::now() - start;
            if (std::chrono::duration_cast<std::chrono::milliseconds>(elapsed).count()
                > PRINT_TIMEOUT_MS) {
                kill(pid, SIGKILL);
                waitpid(pid, &status, 0);
                return false;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(50));
        }
    }
#endif
#endif // !__APPLE__
    //=============================================================================
} // namespace
//=============================================================================
bool
HtmlFileToPdfWithWebview(
    const std::wstring& htmlFilePath, const std::wstring& pdfFilePath, std::wstring& errorMessage)
{
    errorMessage.clear();

    std::error_code ec;
    if (!std::filesystem::is_regular_file(std::filesystem::path(htmlFilePath), ec)) {
        errorMessage = resolveMessageTemplate(L"Nelson:webview:cannotCreateTemporaryHtml",
            L"Cannot create temporary HTML renderer file.");
        return false;
    }

#ifdef __APPLE__
    // macOS: render and print with the native WebKit (WKWebView createPDF).
    if (NelsonHtmlFileToPdfWithWebKit(
            wstring_to_utf8(htmlFilePath).c_str(), wstring_to_utf8(pdfFilePath).c_str())) {
        return true;
    }
    errorMessage = resolveMessageTemplate(
        L"Nelson:webview:cannotGeneratePdf", L"Cannot generate the PDF file.");
    return false;
#else
    const std::wstring browser = findChromiumBrowser();
    if (browser.empty()) {
        errorMessage = resolveMessageTemplate(L"Nelson:webview:cannotFindChromiumBrowser",
            L"Cannot find a Chromium browser for HTML rendering.");
        return false;
    }

    const std::wstring fileUrl
        = utf8_to_wstring(WebviewRender::fileUrlFromPath(std::filesystem::path(htmlFilePath)));

    // Remove a stale destination so a kept old PDF is never mistaken for success.
    std::filesystem::remove(std::filesystem::path(pdfFilePath), ec);

    std::filesystem::path profile = temporaryProfilePath();
    std::filesystem::create_directories(profile, ec);

    std::vector<std::wstring> arguments = {
        L"--headless",
        L"--disable-gpu",
        L"--no-first-run",
        L"--no-default-browser-check",
        L"--disable-extensions",
        L"--disable-background-networking",
        L"--disable-sync",
        L"--allow-file-access-from-files",
        L"--run-all-compositor-stages-before-draw",
        L"--virtual-time-budget=10000",
        L"--no-pdf-header-footer",
#ifdef _WIN32
        L"--user-data-dir=\"" + profile.wstring() + L"\"",
        L"--print-to-pdf=\"" + pdfFilePath + L"\"",
        L"\"" + fileUrl + L"\"",
#else
        L"--user-data-dir=" + profile.wstring(),
        L"--print-to-pdf=" + pdfFilePath,
        fileUrl,
#endif
    };

    const bool ok = runBrowser(browser, arguments);
    std::filesystem::remove_all(profile, ec);

    if (ok && std::filesystem::exists(std::filesystem::path(pdfFilePath), ec)
        && std::filesystem::file_size(std::filesystem::path(pdfFilePath), ec) > 0) {
        return true;
    }

    errorMessage = resolveMessageTemplate(
        L"Nelson:webview:cannotGeneratePdf", L"Cannot generate the PDF file.");
    return false;
#endif
}
//=============================================================================
void
RegisterHtmlToPdfProvider()
{
    setHtmlToPdfProvider([](const std::wstring& htmlFilePath, const std::wstring& pdfFilePath,
                             std::wstring& errorMessage) {
        return HtmlFileToPdfWithWebview(htmlFilePath, pdfFilePath, errorMessage);
    });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
