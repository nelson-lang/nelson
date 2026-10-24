//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "InstallerOptions.hpp"
#include <algorithm>
#include <fstream>
#include <sstream>
#include <stdexcept>
//=============================================================================
namespace Nelson::Installer {
//=============================================================================
namespace {
    void
    require(bool condition, const char* message)
    {
        if (!condition) {
            throw std::runtime_error(message);
        }
    }
    //=============================================================================
    std::filesystem::path
    path(const std::wstring& value, bool absolute)
    {
        require(
            !value.empty() && value.find(L'"') == std::wstring::npos, "Invalid installation path.");
        const std::filesystem::path result(value);
        require(!absolute || result.is_absolute(), "The installation folder must be absolute.");
        return std::filesystem::absolute(result);
    }
    //=============================================================================
    void
    setOption(Options& options, const std::wstring& key, const std::wstring& value, bool control)
    {
        require(value.size() <= 32768, "Installation option is too long.");
        require(std::none_of(value.begin(), value.end(), [](wchar_t c) { return c < 32; }),
            "Control character in installation option.");
        if (key == L"agreeToLicense") {
            require(value == L"yes", "License agreement is required.");
            options.agreed = true;
            return;
        }
        require(options.seen.insert(key).second, "Duplicate installation option.");
        if (key == L"applicationFolder" || key == L"destinationFolder") {
            require(options.directory.empty(),
                "applicationFolder and destinationFolder cannot be combined.");
            require(key != L"destinationFolder" || options.runtimeDirectory.empty(),
                "runtimeFolder and destinationFolder cannot be combined.");
            options.directory = path(value, true);
        } else if (key == L"outputFile") {
            options.log = path(value, false);
            require(!std::filesystem::exists(options.log), "The output log must be a new file.");
        } else if (key == L"desktopShortcut" || key == L"startMenuShortcut") {
            require(control, "Shortcut settings require an installer control file.");
            require(
                value == L"true" || value == L"false", "Shortcut settings require true or false.");
            (key == L"desktopShortcut" ? options.desktop : options.startMenu) = value == L"true";
        } else if (key == L"runtimeFolder") {
            require(!options.seen.count(L"destinationFolder"),
                "runtimeFolder and destinationFolder cannot be combined.");
            options.runtimeDirectory = path(value, true);
        } else {
            throw std::runtime_error("Unsupported installation option: " + utf8(key));
        }
    }
    //=============================================================================
    void
    controlFile(Options& options, const std::wstring& option, const std::wstring& name)
    {
        require(!options.seen.count(L"inputFile") && !options.seen.count(L"inputfile"),
            "Duplicate installer control file.");
        options.seen.insert(option);
        require(std::filesystem::is_regular_file(std::filesystem::path(name)),
            "Expected a regular installer control file.");
        std::ifstream input(std::filesystem::path(name), std::ios::binary);
        require(input.good(), "Cannot read installer control file.");
        std::string bytes(1048577, '\0');
        input.read(bytes.data(), bytes.size());
        bytes.resize(static_cast<size_t>(input.gcount()));
        require(bytes.size() <= 1048576 && !input.bad(),
            "Expected an installer control file no larger than 1 MiB.");
        auto text = wide(bytes);
        require(text.find(L'\0') == std::wstring::npos,
            "Installer control files cannot contain NUL bytes.");
        if (!text.empty() && text.front() == 0xfeff) {
            text.erase(text.begin());
        }
        std::wistringstream lines(text);
        std::wstring line;
        bool first = true;
        while (std::getline(lines, line)) {
            if (!line.empty() && line.back() == L'\r') {
                line.pop_back();
            }
            if (line.empty() || line.front() == L'#') {
                continue;
            }
            const auto equals = line.find(L'=');
            require(equals != std::wstring::npos && equals > 0,
                "Control file entries must use key=value.");
            const auto key = line.substr(0, equals);
            const auto value = line.substr(equals + 1);
            if (first && !options.agreed) {
                require(key == L"agreeToLicense" && value == L"yes",
                    "agreeToLicense=yes must be the first control file option.");
            }
            first = false;
            setOption(options, key, value, true);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
std::string
utf8(const std::wstring& value)
{
    if (value.empty()) {
        return {};
    }
    const auto size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value.data(),
        static_cast<int>(value.size()), nullptr, 0, nullptr, nullptr);
    require(size > 0, "Invalid Unicode installation option.");
    std::string result(size, '\0');
    require(WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, value.data(),
                static_cast<int>(value.size()), result.data(), size, nullptr, nullptr)
            == size,
        "Cannot encode installation option.");
    return result;
}
//=============================================================================
std::wstring
wide(const std::string& value)
{
    if (value.empty()) {
        return {};
    }
    const auto size = MultiByteToWideChar(
        CP_UTF8, MB_ERR_INVALID_CHARS, value.data(), static_cast<int>(value.size()), nullptr, 0);
    require(size > 0, "Installer control files must use valid UTF-8.");
    std::wstring result(size, L'\0');
    require(MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, value.data(),
                static_cast<int>(value.size()), result.data(), size)
            == size,
        "Cannot decode installer control file.");
    return result;
}
//=============================================================================
void
parseOptions(const std::vector<std::wstring>& arguments, Options& options)
{
    require(arguments.size() >= 2, "Installation options require a value.");
    if (arguments[0] == L"-inputFile" || arguments[0] == L"-inputfile") {
        controlFile(options, arguments[0].substr(1), arguments[1]);
    } else {
        require(arguments[0] == L"-agreeToLicense" && arguments[1] == L"yes",
            "Use -agreeToLicense yes first or an installer control file.");
        options.agreed = true;
    }
    for (size_t index = 2; index < arguments.size();) {
        const auto& key = arguments[index];
        auto native = key;
        std::transform(native.begin(), native.end(), native.begin(), towupper);
        if (native == L"/CURRENTUSER" || native == L"/ALLUSERS" || native == L"/PORTABLE=1") {
            require(options.seen.insert(native).second, "Duplicate native installation option.");
            options.native += L" " + native;
            ++index;
        } else {
            require(index + 1 < arguments.size() && !key.empty() && key.front() == L'-',
                "Installation options require a value.");
            const auto& value = arguments[index + 1];
            auto next = value;
            std::transform(next.begin(), next.end(), next.begin(), towupper);
            require(!value.empty() && value.front() != L'-' && next != L"/CURRENTUSER"
                    && next != L"/ALLUSERS" && next != L"/PORTABLE=1",
                "Installation options require a value.");
            if (key == L"-inputFile" || key == L"-inputfile") {
                controlFile(options, key.substr(1), value);
            } else {
                setOption(options, key.substr(1), value, false);
            }
            index += 2;
        }
    }
    require(options.agreed, "License agreement is required.");
    require(!(options.seen.count(L"/CURRENTUSER") && options.seen.count(L"/ALLUSERS")),
        "Conflicting installation scopes.");
    require(options.runtimeDirectory.empty() || !options.seen.count(L"/PORTABLE=1"),
        "runtimeFolder requires a registered, non-portable installation.");
}
//=============================================================================
void
validateOptions(const Options& options, bool runtime)
{
    for (const auto& key : options.seen) {
        const bool runtimeOnly = key == L"inputfile";
        const bool applicationOnly = key == L"inputFile" || key == L"applicationFolder"
            || key == L"runtimeFolder" || key == L"desktopShortcut" || key == L"startMenuShortcut";
        require(!(runtime ? applicationOnly : runtimeOnly),
            runtime ? "Unsupported runtime installation option."
                    : "Unsupported application installation option.");
    }
}
//=============================================================================
std::wstring
nativeArguments(const Options& options)
{
    std::wstring result = L"/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- "
                          L"/NOCLOSEAPPLICATIONS /NORESTARTAPPLICATIONS /NELSONCLI=1";
    result += options.native;
    // The installer engine treats backslashes literally, including before a closing quote.
    if (!options.directory.empty()) {
        result += L" \"/DIR=" + options.directory.native() + L"\"";
    }
    if (!options.runtimeDirectory.empty()) {
        result += L" \"/NELSONRUNTIME=" + options.runtimeDirectory.native() + L"\"";
    }
    result += L" \"/LOG=" + options.log.native() + L"\"";
    if (options.desktop) {
        result += L" /NELSONDESKTOP=1";
    }
    if (options.startMenu) {
        result += L" /NELSONSTARTMENU=1";
    }
    require(result.size() < 30000, "Installation command line is too long.");
    return result;
}
//=============================================================================
} // namespace Nelson::Installer
//=============================================================================
