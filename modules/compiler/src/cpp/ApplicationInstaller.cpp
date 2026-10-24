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
#include "InstallerIO.hpp"
#include "InstallerPayload.hpp"
#include "ApplicationWindowsResources.hpp"
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
#ifdef _WIN32
    using ApplicationArchive::require;
    void
    requireRuntimeContract(const std::wstring& launcher)
    {
        const auto module = LoadLibraryExW(launcher.c_str(), nullptr,
            LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE | LOAD_LIBRARY_AS_IMAGE_RESOURCE);
        require(module != nullptr, "Cannot open installer launcher resources.");
        const auto resource = FindResourceW(module, L"NELSON_INSTALLER_CONTRACT", RT_RCDATA);
        const auto loaded = resource ? LoadResource(module, resource) : nullptr;
        const auto bytes = loaded ? static_cast<const uint8_t*>(LockResource(loaded)) : nullptr;
        const bool supported = bytes && SizeofResource(module, resource) == 4
            && ApplicationArchive::readInteger(bytes, 4) == 2;
        FreeLibrary(module);
        require(supported, "The installer launcher does not support runtime deployment arguments.");
    }
    //=============================================================================
    struct ResourceCopy
    {
        ApplicationWindowsResources& target;
        std::exception_ptr error;
    };
    //=============================================================================
    BOOL CALLBACK
    copyLanguage(HMODULE source, LPCWSTR type, LPCWSTR name, WORD language, LONG_PTR context)
    {
        auto& copy = *reinterpret_cast<ResourceCopy*>(context);
        try {
            const auto resource = FindResourceExW(source, type, name, language);
            require(resource != nullptr, "Cannot find installer resource.");
            const auto size = SizeofResource(source, resource);
            const auto loaded = LoadResource(source, resource);
            const auto bytes = static_cast<const uint8_t*>(LockResource(loaded));
            require(size > 0 && bytes != nullptr, "Cannot read installer resource.");
            copy.target.set(type, name, language, std::vector<uint8_t>(bytes, bytes + size));
            return TRUE;
        } catch (...) {
            copy.error = std::current_exception();
            return FALSE;
        }
    }
    //=============================================================================
    BOOL CALLBACK
    copyName(HMODULE source, LPCWSTR type, LPWSTR name, LONG_PTR context)
    {
        return EnumResourceLanguagesW(source, type, name, copyLanguage, context);
    }
    //=============================================================================
    void
    copyResources(const std::wstring& source, const std::wstring& target)
    {
        const auto module = LoadLibraryExW(source.c_str(), nullptr,
            LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE | LOAD_LIBRARY_AS_IMAGE_RESOURCE);
        require(module != nullptr, "Cannot open installer resources.");
        try {
            ApplicationWindowsResources update(target);
            ResourceCopy copy { update, nullptr };
            for (const auto type : { RT_ICON, RT_GROUP_ICON, RT_VERSION }) {
                const auto ok
                    = EnumResourceNamesW(module, type, copyName, reinterpret_cast<LONG_PTR>(&copy));
                if (copy.error) {
                    std::rethrow_exception(copy.error);
                }
                require(ok || GetLastError() == ERROR_RESOURCE_TYPE_NOT_FOUND,
                    "Cannot enumerate installer resources.");
            }
            update.commit();
        } catch (...) {
            FreeLibrary(module);
            throw;
        }
        FreeLibrary(module);
    }
#endif
    //=============================================================================
} // namespace
//=============================================================================
void
wrapApplicationInstaller(const std::wstring& launcher, const std::wstring& installer,
    const std::wstring& destination, bool runtime)
{
#ifdef _WIN32
    namespace fs = std::filesystem;
    require(fs::is_regular_file(launcher) && fs::is_regular_file(installer),
        "Installer launcher and payload must be regular files.");
    if (runtime) {
        requireRuntimeContract(launcher);
    }
    require(fs::copy_file(launcher, destination), "Cannot create installer launcher.");
    try {
        copyResources(installer, destination);
        const auto offset = fs::file_size(destination);
        const auto length = fs::file_size(installer);
        require(length >= 64, "Installer payload is too small.");
        std::ifstream input(fs::path(installer), std::ios::binary);
        require(input.good(), "Cannot read installer payload.");
        FILE* raw = nullptr;
        _wfopen_s(&raw, destination.c_str(), L"ab");
        require(raw != nullptr, "Cannot append installer payload.");
        std::unique_ptr<FILE, decltype(&fclose)> output(raw, fclose);
        const auto digest = Installer::transfer(input, output.get(), length);
        const auto padding = (8 - (offset + length) % 8) % 8;
        for (uint64_t index = 0; index < padding; ++index) {
            require(fputc(0, output.get()) != EOF, "Cannot align installer payload.");
        }
        std::array<uint8_t, Installer::footerSize> footer {};
        std::memcpy(footer.data(), runtime ? Installer::runtimeSignature : Installer::signature,
            sizeof(Installer::signature));
        ApplicationArchive::writeInteger(footer.data() + 16, offset, 8);
        ApplicationArchive::writeInteger(footer.data() + 24, length, 8);
        std::copy(digest.begin(), digest.end(), footer.begin() + 32);
        require(fwrite(footer.data(), 1, footer.size(), output.get()) == footer.size(),
            "Cannot write installer footer.");
        require(fclose(output.release()) == 0, "Cannot close installer launcher.");
    } catch (...) {
        std::error_code ignored;
        fs::remove(destination, ignored);
        throw;
    }
#else
    (void)launcher;
    (void)installer;
    (void)destination;
    (void)runtime;
    throw std::runtime_error("Native installer launchers require Windows.");
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
