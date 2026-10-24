//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ApplicationExecutable.hpp"
#include "ApplicationArchiveFormat.hpp"
#include "ApplicationWindowsResources.hpp"
#include <fstream>
#include <memory>
#include <type_traits>
//=============================================================================
namespace Nelson {
#ifdef _WIN32
namespace {
    using Bytes = std::vector<uint8_t>;
    using ApplicationArchive::require;
    constexpr std::array<uint32_t, 7> iconSizes { 16, 24, 32, 48, 64, 128, 256 };
    Bytes
    readIconImage(const std::wstring& filename, uint32_t side)
    {
        require(!filename.empty() && filename.find(L'\0') == std::wstring::npos,
            "Invalid icon image filename.");
        std::ifstream input(std::filesystem::path(filename), std::ios::binary | std::ios::ate);
        require(input.good(), "Cannot read application icon image.");
        const auto size = input.tellg();
        require(size >= 33 && size <= 1048576, "Invalid application icon image size.");
        Bytes bytes(static_cast<size_t>(size));
        input.seekg(0);
        input.read(reinterpret_cast<char*>(bytes.data()), bytes.size());
        require(input.good(), "Cannot read application icon image.");
        constexpr std::array<uint8_t, 16> header { 137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13,
            'I', 'H', 'D', 'R' };
        require(std::equal(header.begin(), header.end(), bytes.begin()),
            "Application icon frames must be PNG images.");
        const auto integer = [&](size_t offset) {
            return (uint32_t(bytes[offset]) << 24) | (uint32_t(bytes[offset + 1]) << 16)
                | (uint32_t(bytes[offset + 2]) << 8) | uint32_t(bytes[offset + 3]);
        };
        require(integer(16) == side && integer(20) == side && bytes[24] == 8 && bytes[25] == 6,
            "Application icon frames must have the expected RGBA dimensions.");
        HICON decoded = CreateIconFromResourceEx(bytes.data(), static_cast<DWORD>(bytes.size()),
            TRUE, 0x00030000, static_cast<int>(side), static_cast<int>(side), LR_DEFAULTCOLOR);
        require(decoded != nullptr, "Invalid application icon PNG data.");
        DestroyIcon(decoded);
        return bytes;
    }
    //=============================================================================
    struct Resource
    {
        WORD type;
        WORD id;
        WORD language;
        std::wstring name;
    };
    //=============================================================================
    struct Inventory
    {
        std::vector<Resource> resources;
        bool failed = false;
    };
    //=============================================================================
    BOOL CALLBACK
    collectLanguage(HMODULE, LPCWSTR type, LPCWSTR name, WORD language, LONG_PTR parameter)
    {
        auto& inventory = *reinterpret_cast<Inventory*>(parameter);
        try {
            Resource resource { static_cast<WORD>(reinterpret_cast<ULONG_PTR>(type)), 0, language,
                {} };
            if (IS_INTRESOURCE(name)) {
                resource.id = static_cast<WORD>(reinterpret_cast<ULONG_PTR>(name));
            } else {
                resource.name = name;
            }
            inventory.resources.push_back(std::move(resource));
            return TRUE;
        } catch (...) {
            inventory.failed = true;
            return FALSE;
        }
    }
    //=============================================================================
    BOOL CALLBACK
    collectName(HMODULE module, LPCWSTR type, LPWSTR name, LONG_PTR parameter)
    {
        const auto success = EnumResourceLanguagesW(module, type, name, collectLanguage, parameter);
        if (!success) {
            reinterpret_cast<Inventory*>(parameter)->failed = true;
        }
        return success;
    }
    //=============================================================================
    std::vector<Resource>
    existingIcons(const std::wstring& filename)
    {
        const auto module = LoadLibraryExW(
            filename.c_str(), nullptr, LOAD_LIBRARY_AS_DATAFILE | LOAD_LIBRARY_AS_IMAGE_RESOURCE);
        require(module != nullptr, "Cannot inspect application icon resources.");
        std::unique_ptr<std::remove_pointer_t<HMODULE>, decltype(&FreeLibrary)> library(
            module, FreeLibrary);
        Inventory inventory;
        for (WORD type : { WORD(3), WORD(14) }) {
            SetLastError(ERROR_SUCCESS);
            const auto success = EnumResourceNamesW(module, MAKEINTRESOURCEW(type), collectName,
                reinterpret_cast<LONG_PTR>(&inventory));
            const auto error = GetLastError();
            require(!inventory.failed
                    && (success || error == ERROR_RESOURCE_TYPE_NOT_FOUND
                        || error == ERROR_RESOURCE_DATA_NOT_FOUND),
                "Cannot enumerate application icon resources.");
        }
        return inventory.resources;
    }
    //=============================================================================
    Bytes
    groupResource(const std::vector<Bytes>& images)
    {
        Bytes group(6 + 14 * images.size(), 0);
        ApplicationArchive::writeInteger(group.data() + 2, 1, 2);
        ApplicationArchive::writeInteger(group.data() + 4, images.size(), 2);
        for (size_t i = 0; i < images.size(); ++i) {
            auto* entry = group.data() + 6 + i * 14;
            entry[0] = entry[1] = static_cast<uint8_t>(iconSizes[i]);
            ApplicationArchive::writeInteger(entry + 4, 1, 2);
            ApplicationArchive::writeInteger(entry + 6, 32, 2);
            ApplicationArchive::writeInteger(entry + 8, images[i].size(), 4);
            ApplicationArchive::writeInteger(entry + 12, i + 1, 2);
        }
        return group;
    }
    //=============================================================================
} // namespace
#endif
void
setApplicationIcon(const std::wstring& filename, const std::vector<std::wstring>& paths)
{
#ifdef _WIN32
    validateApplicationResourceTarget(filename);
    ApplicationArchive::require(
        paths.size() == iconSizes.size(), "Seven application icon frames are required.");
    std::vector<Bytes> images;
    for (size_t i = 0; i < paths.size(); ++i) {
        images.push_back(readIconImage(paths[i], iconSizes[i]));
    }
    const auto previous = existingIcons(filename);
    ApplicationWindowsResources update(filename);
    for (const auto& resource : previous) {
        update.set(MAKEINTRESOURCEW(resource.type),
            resource.name.empty() ? MAKEINTRESOURCEW(resource.id) : resource.name.c_str(),
            resource.language, {});
    }
    for (size_t i = 0; i < images.size(); ++i) {
        update.set(MAKEINTRESOURCEW(3), MAKEINTRESOURCEW(i + 1),
            MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), images[i]);
    }
    update.set(MAKEINTRESOURCEW(14), MAKEINTRESOURCEW(1), MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL),
        groupResource(images));
    update.commit();
#else
    (void)filename;
    (void)paths;
    throw std::runtime_error("ExecutableIcon resources currently require Windows.");
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
