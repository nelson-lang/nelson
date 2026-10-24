//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationArchiveBuilder.hpp"
#include "ApplicationArchiveIO.hpp"
#include "Zip.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    //=============================================================================
    using namespace ApplicationArchive;
    //=============================================================================
    fs::path
    nativePath(const std::wstring& value)
    {
        return fs::u8path(wstring_to_utf8(value));
    }
    //=============================================================================
    std::array<uint8_t, trailerSize>
    encode(const Trailer& value)
    {
        require(value.installedRuntime ? validDigest(value.engineFingerprint)
                                       : validRuntimeDirectory(value.runtimeDirectory),
            "Invalid application runtime identifier.");
        require(validDigest(value.digest), "Invalid application digest.");
        std::array<uint8_t, trailerSize> bytes {};
        const std::string magic = "NLSAPP01";
        std::copy(magic.begin(), magic.end(), bytes.begin());
        writeInteger(
            bytes.data() + 8, value.externalArchive ? 3 : (value.installedRuntime ? 2 : 1), 4);
        writeInteger(bytes.data() + 12,
            (value.gui ? 1 : 0) | (value.installedRuntime ? 2 : 0)
                | (value.externalArchive ? 4 : 0),
            4);
        writeInteger(bytes.data() + 16, value.offset, 8);
        writeInteger(bytes.data() + 24, value.length, 8);
        std::copy(value.digest.begin(), value.digest.end(), bytes.begin() + 32);
        const auto& identifier
            = value.installedRuntime ? value.engineFingerprint : value.runtimeDirectory;
        std::copy(identifier.begin(), identifier.end(), bytes.begin() + 96);
        return bytes;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
createApplicationArchive(const std::wstring& stubName, const std::wstring& archiveName,
    const std::wstring& destinationName, bool gui, const std::string& installedEngineFingerprint,
    bool embedArchive)
{
    const auto stub = nativePath(stubName);
    const auto archive = nativePath(archiveName);
    const auto destination = nativePath(destinationName);
    const auto external = ApplicationArchive::externalArchivePath(destination);
    require(
        embedArchive || external != destination, "Executable and archive filenames must differ.");
    ApplicationArchive::Trailer trailer;
    trailer.offset = fs::file_size(stub);
    trailer.length = fs::file_size(archive);
    trailer.gui = gui;
    trailer.installedRuntime = !installedEngineFingerprint.empty();
    trailer.externalArchive = !embedArchive;
    trailer.engineFingerprint = installedEngineFingerprint;
    require(
        !trailer.installedRuntime || ApplicationArchive::validDigest(installedEngineFingerprint),
        "Invalid application engine fingerprint.");
    trailer.runtimeDirectory = destination.stem().u8string() + ".runtime";
    require(trailer.offset > 0 && trailer.length >= 22
            && trailer.length <= ApplicationArchive::maxArchiveBytes,
        "Invalid application payload size.");
    require(ApplicationArchive::validRuntimeDirectory(trailer.runtimeDirectory),
        "Invalid application runtime directory.");
    FILE* output = createFile(destination);
    FILE* archiveOutput = nullptr;
    bool archiveCreated = false;
    try {
        std::ifstream inputStub(stub, std::ios::binary);
        transfer(inputStub, output, trailer.offset);
        std::ifstream inputArchive(archive, std::ios::binary);
        if (embedArchive) {
            trailer.digest = transfer(inputArchive, output, trailer.length);
        } else {
            archiveOutput = createFile(external);
            archiveCreated = true;
            trailer.digest = transfer(inputArchive, archiveOutput, trailer.length);
            const auto status = fclose(archiveOutput);
            archiveOutput = nullptr;
            require(status == 0, "Cannot close external application archive.");
        }
        const auto bytes = encode(trailer);
        require(fwrite(bytes.data(), 1, bytes.size(), output) == bytes.size(),
            "Cannot write application archive trailer.");
        const auto status = fclose(output);
        output = nullptr;
        require(status == 0, "Cannot close application executable.");
        fs::permissions(destination, fs::status(stub).permissions());
    } catch (...) {
        if (output != nullptr) {
            fclose(output);
        }
        if (archiveOutput != nullptr) {
            fclose(archiveOutput);
        }
        std::error_code ignored;
        fs::remove(destination, ignored);
        if (archiveCreated) {
            fs::remove(external, ignored);
        }
        throw;
    }
}
//=============================================================================
void
packApplicationArchive(
    const std::wstring& archive, const wstringVector& files, const std::wstring& root)
{
    wstringVector entries;
    Zip(archive, files, root, entries, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
