//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ZipArchive.hpp"
#include "GzipCodec.hpp"
#include "TarArchive.hpp"
#include "ArchiveInterruption.hpp"
#include "ArchiveStreams.hpp"
#include "ZstdCodec.hpp"
#include "encrypted_archives.hpp"
#include <algorithm>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
static int failures = 0;
//=============================================================================
constexpr uint32_t localHeader = 0x04034b50;
constexpr uint32_t centralHeader = 0x02014b50;
//=============================================================================
static void
check(bool condition, const char* message);
static void
checkThrows(const std::function<void()>& action, ZipErrorCode expected, const char* message);
static std::string
asText(const std::vector<uint8_t>& bytes);
static size_t
findSignature(const std::vector<uint8_t>& bytes, uint32_t signature, size_t start = 0);
static void
writeUint16(std::vector<uint8_t>& bytes, size_t offset, uint16_t value);
static uint16_t
readUint16(const std::vector<uint8_t>& bytes, size_t offset);
static std::vector<uint8_t>
buildArchive(ZipCompression method);
static void
testMemoryRoundTrip(ZipCompression method, const char* label);
static void
testCrcMismatch();
static void
setAllMethods(std::vector<uint8_t>& bytes, uint16_t method);
static void
testUnsupportedMethod();
static void
testCp437Name();
static void
testFilesAndModes();
static void
testNotAnArchive();
static void
testCopyEntry();
static void
testZstdCodec();
static void
testPullReading();
static void
testZstdStreams();
static void
testSeekTable();
static void
testAddFiles();
static void
testInterruption();
static void
testGzipCodec();
static void
testTarArchive(TarCompression compression);
static void
testEncryptedEntries();
static std::string
toHex(const uint8_t* bytes, size_t size);
static std::vector<uint8_t>
fromHex(const std::string& hex);
static void
checkThrowsZstd(const std::function<void()>& action, Nelson::ZstdCodec::ZstdErrorCode expected,
    const char* message);
//=============================================================================
int
main()
{
    testMemoryRoundTrip(ZipCompression::Deflate, "deflate");
    testMemoryRoundTrip(ZipCompression::Store, "store");
    if (ZipArchive::supportsCompression(static_cast<uint16_t>(ZipCompression::Zstd))) {
        testMemoryRoundTrip(ZipCompression::Zstd, "zstd");
    } else {
        std::cerr << "zstd not built: skipped\n";
    }
    testCrcMismatch();
    testUnsupportedMethod();
    testCp437Name();
    testFilesAndModes();
    testNotAnArchive();
    testCopyEntry();
    testZstdCodec();
    testPullReading();
    testZstdStreams();
    testSeekTable();
    testAddFiles();
    testInterruption();
    testGzipCodec();
    testTarArchive(TarCompression::None);
    testTarArchive(TarCompression::Gzip);
    if (!ZstdCodec::version().empty()) {
        testTarArchive(TarCompression::Zstd);
    }
    testEncryptedEntries();
    if (failures != 0) {
        std::cerr << failures << " check(s) failed\n";
        return 1;
    }
    std::cerr << "all ZipArchive checks passed\n";
    return 0;
}
//=============================================================================
static void
check(bool condition, const char* message)
{
    if (!condition) {
        std::cerr << "FAILED: " << message << '\n';
        ++failures;
    }
}
//=============================================================================
// Runs action and checks that it throws ZipArchiveError with the expected code.
static void
checkThrows(const std::function<void()>& action, ZipErrorCode expected, const char* message)
{
    try {
        action();
    } catch (const ZipArchiveError& error) {
        check(error.code() == expected, message);
        return;
    } catch (...) {
    }
    check(false, message);
}
//=============================================================================
static std::string
asText(const std::vector<uint8_t>& bytes)
{
    return std::string(bytes.begin(), bytes.end());
}
//=============================================================================
// Finds the next ZIP record with this 4-byte signature at or after start.
static size_t
findSignature(const std::vector<uint8_t>& bytes, uint32_t signature, size_t start)
{
    for (size_t i = start; i + 4 <= bytes.size(); ++i) {
        uint32_t value = bytes[i] | (bytes[i + 1] << 8) | (bytes[i + 2] << 16)
            | (static_cast<uint32_t>(bytes[i + 3]) << 24);
        if (value == signature) {
            return i;
        }
    }
    return std::string::npos;
}
//=============================================================================
static void
writeUint16(std::vector<uint8_t>& bytes, size_t offset, uint16_t value)
{
    bytes[offset] = static_cast<uint8_t>(value & 0xFF);
    bytes[offset + 1] = static_cast<uint8_t>(value >> 8);
}
//=============================================================================
static uint16_t
readUint16(const std::vector<uint8_t>& bytes, size_t offset)
{
    return static_cast<uint16_t>(bytes[offset] | (bytes[offset + 1] << 8));
}
//=============================================================================
static std::vector<uint8_t>
buildArchive(ZipCompression method)
{
    ZipArchive writer = ZipArchive::createInMemory();
    ZipWriteOptions options;
    options.method = method;
    const std::string hello(1000, 'h');
    writer.addDirectory("folder", options);
    writer.addBuffer("folder/hello.txt", hello.data(), hello.size(), options);
    options.method = ZipCompression::Store;
    const std::string plain = "plain text";
    writer.addBuffer("plain.txt", plain.data(), plain.size(), options);
    writer.close();
    return writer.memoryBytes();
}
//=============================================================================
static void
testMemoryRoundTrip(ZipCompression method, const char* label)
{
    std::cerr << "round trip " << label << '\n';
    ZipArchive reader = ZipArchive::openMemory(buildArchive(method));
    const auto& entries = reader.entries();
    check(entries.size() == 3, "three entries");
    check(entries[0].name == "folder/" && entries[0].isDirectory, "directory entry");
    check(entries[1].name == "folder/hello.txt" && entries[1].size == 1000, "file entry size");
    check(entries[1].method == static_cast<uint16_t>(method), "compression method kept");
    check(entries[2].method == 0, "stored entry");
    check(asText(reader.readEntry(1)) == std::string(1000, 'h'), "compressed content");
    check(asText(reader.readEntry(2)) == "plain text", "stored content");
    check(reader.find("plain.txt") == 2 && !reader.find("missing"), "find by name");
    checkThrows([&] { reader.readEntry(7); }, ZipErrorCode::EntryNotFound, "index out of range");
    check(asText(reader.readEntry(2, 10)) == "plain text", "size limit equal to the entry size");
    checkThrows(
        [&] { reader.readEntry(1, 999); }, ZipErrorCode::SizeLimitExceeded, "size limit exceeded");
    checkThrows([&] { reader.addDirectory("x", ZipWriteOptions()); }, ZipErrorCode::InvalidState,
        "no writing on a reader");
}
//=============================================================================
static void
testCrcMismatch()
{
    std::vector<uint8_t> bytes = buildArchive(ZipCompression::Deflate);
    const std::string plain = "plain text";
    auto it = std::search(bytes.begin(), bytes.end(), plain.begin(), plain.end());
    check(it != bytes.end(), "stored payload found");
    *it = 'P';
    ZipArchive reader = ZipArchive::openMemory(bytes);
    checkThrows([&] { reader.readEntry(2); }, ZipErrorCode::CrcMismatch, "corrupted entry");
    check(asText(reader.readEntry(1)) == std::string(1000, 'h'), "other entries stay readable");
}
//=============================================================================
// Rewrites the method of every local and central header.
static void
setAllMethods(std::vector<uint8_t>& bytes, uint16_t method)
{
    for (size_t at = findSignature(bytes, localHeader); at != std::string::npos;
        at = findSignature(bytes, localHeader, at + 4)) {
        writeUint16(bytes, at + 8, method);
    }
    for (size_t at = findSignature(bytes, centralHeader); at != std::string::npos;
        at = findSignature(bytes, centralHeader, at + 4)) {
        writeUint16(bytes, at + 10, method);
    }
}
//=============================================================================
static void
testUnsupportedMethod()
{
    std::vector<uint8_t> bytes = buildArchive(ZipCompression::Store);
    setAllMethods(bytes, 9);
    ZipArchive reader = ZipArchive::openMemory(bytes);
    check(reader.entries()[2].method == 9, "deflate64 method reported");
    checkThrows(
        [&] { reader.readEntry(2); }, ZipErrorCode::UnsupportedCompression, "deflate64 rejected");
}
//=============================================================================
static void
testCp437Name()
{
    ZipArchive writer = ZipArchive::createInMemory();
    const std::string data = "x";
    writer.addBuffer("cafX.txt", data.data(), data.size(), ZipWriteOptions());
    writer.close();
    std::vector<uint8_t> bytes = writer.memoryBytes();
    // 0x82 is 'e' with acute accent in CP437 and not valid UTF-8 on its own.
    const std::string ascii = "cafX.txt";
    for (auto it = std::search(bytes.begin(), bytes.end(), ascii.begin(), ascii.end());
        it != bytes.end(); it = std::search(it + 1, bytes.end(), ascii.begin(), ascii.end())) {
        *(it + 3) = 0x82;
    }
    const size_t local = findSignature(bytes, localHeader);
    const size_t central = findSignature(bytes, centralHeader);
    writeUint16(bytes, local + 6, readUint16(bytes, local + 6) & ~0x0800);
    writeUint16(bytes, central + 8, readUint16(bytes, central + 8) & ~0x0800);
    ZipArchive reader = ZipArchive::openMemory(bytes);
    check(reader.entries()[0].name == "caf\xc3\xa9.txt", "CP437 name converted to UTF-8");
    check(asText(reader.readEntry(0)) == "x", "CP437 entry readable");
}
//=============================================================================
static void
testFilesAndModes()
{
    namespace fs = std::filesystem;
    const fs::path dir = fs::temp_directory_path() / "nelson_zip_archive_tests";
    fs::remove_all(dir);
    fs::create_directories(dir);
    const std::wstring archive = (dir / "a.zip").wstring();
    const std::wstring source = (dir / "source.bin").wstring();
    {
        std::vector<char> payload(300000);
        for (size_t i = 0; i < payload.size(); ++i) {
            payload[i] = static_cast<char>((i * 7) % 251);
        }
        std::FILE* f = std::fopen((dir / "source.bin").string().c_str(), "wb");
        std::fwrite(payload.data(), 1, payload.size(), f);
        std::fclose(f);
    }
    {
        ZipArchive writer = ZipArchive::openFile(archive, ZipArchive::Mode::Create);
        ZipWriteOptions options;
        ZipArchive::fileMetadata(source, options.modified, options.externalAttributes);
        check(options.modified != 0, "file modification time read");
        writer.addFile("data/source.bin", source, options);
        writer.close();
    }
    checkThrows([&] { ZipArchive::openFile(archive, ZipArchive::Mode::Create); },
        ZipErrorCode::OpenFailed, "create refuses an existing file");
    {
        ZipArchive writer = ZipArchive::openFile(archive, ZipArchive::Mode::Append);
        const std::string extra = "appended";
        writer.addBuffer("extra.txt", extra.data(), extra.size(), ZipWriteOptions());
        writer.close();
    }
    {
        ZipArchive reader = ZipArchive::openFile(archive, ZipArchive::Mode::Read);
        check(reader.entries().size() == 2, "append keeps existing entries");
        auto content = reader.readEntry(*reader.find("data/source.bin"));
        check(content.size() == 300000 && content[1000] == static_cast<uint8_t>((1000 * 7) % 251),
            "file content");
        check(
            asText(reader.readEntry(*reader.find("extra.txt"))) == "appended", "appended content");
        size_t blocks = 0;
        reader.readEntry(0, [&](const uint8_t*, size_t) { ++blocks; });
        check(blocks > 1, "large entries are streamed in blocks");
    }
    {
        ZipArchive writer = ZipArchive::openFile(archive, ZipArchive::Mode::Overwrite);
        writer.addDirectory("only/", ZipWriteOptions());
        writer.close();
        ZipArchive reader = ZipArchive::openFile(archive, ZipArchive::Mode::Read);
        check(reader.entries().size() == 1, "overwrite replaces the archive");
    }
    checkThrows(
        [&] { ZipArchive::openFile((dir / "missing.zip").wstring(), ZipArchive::Mode::Read); },
        ZipErrorCode::OpenFailed, "missing archive");
    fs::remove_all(dir);
}
//=============================================================================
static void
testNotAnArchive()
{
    std::vector<uint8_t> junk(200, 0x41);
    checkThrows(
        [&] { ZipArchive::openMemory(junk); }, ZipErrorCode::NotAZipArchive, "junk is not a ZIP");
    ZipArchive empty = ZipArchive::createInMemory();
    empty.close();
    ZipArchive reader = ZipArchive::openMemory(empty.memoryBytes());
    check(reader.entries().empty(), "empty archive");
}
//=============================================================================
static void
testCopyEntry()
{
    ZipArchive source = ZipArchive::openMemory(buildArchive(ZipCompression::Deflate));
    ZipArchive writer = ZipArchive::createInMemory();
    writer.copyEntry(source, 1, "renamed/hello.txt");
    writer.copyEntry(source, 2, "plain.txt");
    writer.close();
    ZipArchive reader = ZipArchive::openMemory(writer.memoryBytes());
    check(reader.entries().size() == 2, "copy entry count");
    check(reader.entries()[0].name == "renamed/hello.txt", "copy renames");
    check(reader.entries()[0].method == 8, "copy keeps the method");
    check(asText(reader.readEntry(0)) == std::string(1000, 'h'), "copy content and CRC");
    check(asText(reader.readEntry(1)) == "plain text", "copy of a stored entry");
}
//=============================================================================
static void
testZstdCodec()
{
    using namespace Nelson::ZstdCodec;
    if (version().empty()) {
        std::cerr << "zstd codec not built: skipped\n";
        checkThrowsZstd(
            [] { compress(nullptr, 0, 0); }, ZstdErrorCode::NotAvailable, "codec unavailable");
        return;
    }
    const std::string text(5000, 'z');
    const auto* bytes = reinterpret_cast<const uint8_t*>(text.data());
    std::vector<uint8_t> packed = compress(bytes, text.size(), 0);
    check(packed.size() < text.size(), "zstd compresses");
    check(asText(decompress(packed.data(), packed.size(), noSizeLimit)) == text, "zstd round trip");
    std::vector<uint8_t> twice = packed;
    twice.insert(twice.end(), packed.begin(), packed.end());
    check(decompress(twice.data(), twice.size(), noSizeLimit).size() == 2 * text.size(),
        "zstd concatenated frames");
    checkThrowsZstd([&] { decompress(packed.data(), packed.size(), 100); }, ZstdErrorCode::TooLarge,
        "zstd size limit");
    checkThrowsZstd([&] { decompress(packed.data(), packed.size() - 4, noSizeLimit); },
        ZstdErrorCode::InvalidData, "zstd truncated data");
    checkThrowsZstd([&] { compress(bytes, 1, maximumLevel() + 1); }, ZstdErrorCode::InvalidLevel,
        "zstd invalid level");
    namespace fs = std::filesystem;
    const fs::path dir = fs::temp_directory_path() / "nelson_zstd_codec_tests";
    fs::remove_all(dir);
    fs::create_directories(dir);
    {
        std::FILE* f = std::fopen((dir / "in.txt").string().c_str(), "wb");
        std::fwrite(text.data(), 1, text.size(), f);
        std::fclose(f);
    }
    compressFile((dir / "in.txt").wstring(), (dir / "in.txt.zst").wstring(), 9);
    decompressFile((dir / "in.txt.zst").wstring(), (dir / "out.txt").wstring(), noSizeLimit);
    check(fs::file_size(dir / "out.txt") == text.size(), "zstd file round trip");
    checkThrowsZstd(
        [&] {
            decompressFile((dir / "in.txt").wstring(), (dir / "bad.txt").wstring(), noSizeLimit);
        },
        ZstdErrorCode::InvalidData, "zstd file not compressed");
    check(!fs::exists(dir / "bad.txt"), "zstd failed file not left behind");
    fs::remove_all(dir);
}
//=============================================================================
// Entries read block by block, reopened handles and the entry writer guard.
static void
testPullReading()
{
    ZipArchive reader = ZipArchive::openMemory(buildArchive(ZipCompression::Deflate));
    reader.openEntry(1);
    std::string text;
    uint8_t block[100];
    for (size_t count = reader.readEntryData(block, sizeof(block)); count > 0;
        count = reader.readEntryData(block, sizeof(block))) {
        text.append(reinterpret_cast<const char*>(block), count);
    }
    reader.closeEntry();
    check(text == std::string(1000, 'h'), "pull reading");
    ZipArchive other = reader.reopenForReading();
    other.openEntry(2);
    reader.openEntry(1);
    check(other.readEntryData(block, 5) == 5, "independent handle");
    checkThrows(
        [&] { reader.openEntry(1, 10); }, ZipErrorCode::SizeLimitExceeded, "pull size limit");
    ZipArchive writer = ZipArchive::createInMemory();
    writer.beginEntry("open.txt", ZipWriteOptions());
    writer.writeEntryData("abc", 3);
    checkThrows([&] { writer.requireNoEntryBeingWritten(); }, ZipErrorCode::EntryStillOpen,
        "entry being written");
    checkThrows([&] { writer.addDirectory("x", ZipWriteOptions()); }, ZipErrorCode::EntryStillOpen,
        "no second entry while writing");
    writer.endEntry();
    writer.requireNoEntryBeingWritten();
    writer.close();
    ZipArchive back = ZipArchive::openMemory(writer.memoryBytes());
    check(asText(back.readEntry(0)) == "abc", "entry written block by block");
}
//=============================================================================
// Encoder and Decoder: several writes, small reads, reset.
static void
testSeekTable()
{
    using namespace Nelson::ZstdCodec;
    // Two fake frames of 10 and 20 compressed bytes before the table.
    std::vector<uint8_t> source(30, 0);
    const std::vector<uint8_t> table = seekTableFrame({ { 10, 100 }, { 20, 7 } });
    source.insert(source.end(), table.begin(), table.end());
    auto readAt = [&](uint64_t offset, uint8_t* buffer, size_t count) {
        if (offset + count > source.size()) {
            return false;
        }
        std::copy(source.begin() + offset, source.begin() + offset + count, buffer);
        return true;
    };
    SeekTable parsed;
    check(readSeekTable(readAt, source.size(), parsed), "seek table found");
    check(parsed.compressedOffsets == std::vector<uint64_t>({ 0, 10, 30 })
            && parsed.decompressedOffsets == std::vector<uint64_t>({ 0, 100, 107 }),
        "seek table offsets");
    source.erase(source.begin());
    check(!readSeekTable(readAt, source.size(), parsed), "seek table must match the frames");
    std::vector<uint8_t> plain(64, 1);
    source = plain;
    check(!readSeekTable(readAt, source.size(), parsed), "no seek table in plain data");
}
//=============================================================================
// A pending interruption stops writing and decoding loops; no check, no stop.
static void
testInterruption()
{
    bool stop = false;
    ArchiveInterruption::Scope scope([&stop]() { return stop; });
    const std::string text(200000, 'i');
    ZipArchive writer = ZipArchive::createInMemory();
    writer.addBuffer("ok.txt", text.data(), text.size(), ZipWriteOptions());
    stop = true;
    bool interrupted = false;
    try {
        std::vector<uint8_t> output;
        ArchiveStreams::encodeAll(ArchiveStreams::memoryInput(
                                      reinterpret_cast<const uint8_t*>(text.data()), text.size()),
            *std::unique_ptr<ArchiveStreams::StreamEncoder>(
                new GzipCodec::Encoder([&](const uint8_t* d, size_t n) {
                    output.insert(output.end(), d, d + n);
                }, 6)));
    } catch (const ArchiveInterruption::Interrupted&) {
        interrupted = true;
    }
    check(interrupted, "interruption stops a compression loop");
    stop = false;
    writer.close();
    check(ZipArchive::openMemory(writer.memoryBytes()).entries().size() == 1,
        "archive usable after the interruption check is cleared");
}
//=============================================================================
static void
testAddFiles()
{
    namespace fs = std::filesystem;
    const fs::path dir = fs::temp_directory_path() / "nelson_zip_parallel_tests";
    fs::remove_all(dir);
    fs::create_directories(dir);
    std::vector<ZipFileItem> items;
    for (int k = 0; k < 12; ++k) {
        const fs::path file = dir / ("f" + std::to_string(k) + ".txt");
        std::ofstream(file, std::ios::binary) << std::string(5000 + k * 977, char('a' + k));
        ZipFileItem item;
        item.name = "d/f" + std::to_string(k) + ".txt";
        item.filename = file.wstring();
        item.options.method = k % 3 == 0 ? ZipCompression::Store : ZipCompression::Deflate;
        items.push_back(item);
        if (k == 5) {
            ZipFileItem folder;
            folder.name = "d/sub";
            folder.isDirectory = true;
            items.push_back(folder);
        }
    }
    {
        ZipArchive writer = ZipArchive::createInMemory();
        writer.addFiles(items);
        writer.close();
        ZipArchive reader = ZipArchive::openMemory(writer.memoryBytes());
        bool same = reader.entries().size() == items.size();
        for (size_t i = 0; same && i < items.size(); ++i) {
            same = reader.entries()[i].name
                == (items[i].isDirectory ? items[i].name + "/" : items[i].name);
            if (same && !items[i].isDirectory) {
                std::ifstream in(fs::path(items[i].filename), std::ios::binary);
                const std::string expected(
                    (std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
                same = asText(reader.readEntry(i)) == expected;
            }
        }
        check(same, "addFiles keeps order and content");
    }
    ZipFileItem missing;
    missing.name = "missing.txt";
    missing.filename = (dir / "missing.txt").wstring();
    std::vector<ZipFileItem> withMissing = { items[0], missing, items[1] };
    ZipArchive writer = ZipArchive::createInMemory();
    checkThrows([&] { writer.addFiles(withMissing); }, ZipErrorCode::OpenFailed,
        "addFiles reports a missing file");
    fs::remove_all(dir);
}
//=============================================================================
static void
testGzipCodec()
{
    using namespace Nelson::GzipCodec;
    const std::string text(200000, 'g');
    const auto* data = reinterpret_cast<const uint8_t*>(text.data());
    std::vector<uint8_t> packed = compress(data, text.size(), -1);
    check(packed.size() > 18 && packed[0] == 0x1F && packed[1] == 0x8B, "gzip header");
    check(asText(decompress(packed.data(), packed.size(), noSizeLimit)) == text, "gzip round trip");
    // Two members read back as one stream.
    std::vector<uint8_t> second = compress(data, 10, 9);
    std::vector<uint8_t> both = packed;
    both.insert(both.end(), second.begin(), second.end());
    check(decompress(both.data(), both.size(), noSizeLimit).size() == text.size() + 10,
        "gzip members are concatenated");
    std::vector<uint8_t> truncated(packed.begin(), packed.end() - 4);
    bool rejected = false;
    try {
        decompress(truncated.data(), truncated.size(), noSizeLimit);
    } catch (const GzipError& error) {
        rejected = error.code() == GzipErrorCode::InvalidData;
    }
    check(rejected, "gzip truncated data");
    rejected = false;
    try {
        decompress(packed.data(), packed.size(), 100);
    } catch (const GzipError& error) {
        rejected = error.code() == GzipErrorCode::TooLarge;
    }
    check(rejected, "gzip size limit");
}
//=============================================================================
static void
testTarArchive(TarCompression compression)
{
    const std::string longName = std::string(120, 'n') + "/" + std::string(130, 'm') + ".txt";
    const std::string unicodeName = "dossier/\xC3\xA9t\xC3\xA9.txt";
    const std::string big(300000, 'b');
    TarArchive writer = TarArchive::createInMemory(compression, -1);
    writer.addDirectory("dossier", 1700000000);
    writer.addBuffer("dossier/a.txt", "hello", 5, 1700000000);
    writer.addBuffer(longName, "long", 4);
    writer.addBuffer(unicodeName, "u", 1);
    writer.addBuffer("big.bin", big.data(), big.size());
    writer.addBuffer("empty.txt", "", 0);
    writer.close();
    const std::string label = TarArchive::compressionName(compression);
    TarArchive reader = TarArchive::openMemory(writer.memoryBytes());
    check(reader.compression() == compression, ("tar compression found: " + label).c_str());
    const auto& entries = reader.entries();
    check(entries.size() == 6 && entries[0].isDirectory && entries[0].name == "dossier/",
        ("tar folder entry: " + label).c_str());
    check(entries[1].modified == 1700000000 && entries[1].size == 5,
        ("tar entry metadata: " + label).c_str());
    check(reader.find(longName).has_value() && reader.find(unicodeName).has_value(),
        ("tar long and unicode names: " + label).c_str());
    check(asText(reader.readEntry(*reader.find("big.bin"))) == big,
        ("tar big entry: " + label).c_str());
    check(asText(reader.readEntry(*reader.find("dossier/a.txt"))) == "hello",
        ("tar backward read: " + label).c_str());
    check(
        reader.readEntry(*reader.find("empty.txt")).empty(), ("tar empty entry: " + label).c_str());
    TarArchive other = reader.reopenForReading();
    other.openEntry(*other.find(longName));
    char block[3];
    std::string pulled;
    for (size_t count = other.readEntryData(block, sizeof(block)); count > 0;
        count = other.readEntryData(block, sizeof(block))) {
        pulled.append(block, count);
    }
    check(pulled == "long", ("tar pull reading: " + label).c_str());
    checkThrows([&] { reader.readEntry(*reader.find("big.bin"), 10); },
        ZipErrorCode::SizeLimitExceeded, ("tar size limit: " + label).c_str());
}
//=============================================================================
static void
testZstdStreams()
{
    using namespace Nelson::ZstdCodec;
    if (version().empty()) {
        return;
    }
    std::vector<uint8_t> packed;
    Encoder encoder(
        [&](const uint8_t* data, size_t size) { packed.insert(packed.end(), data, data + size); },
        3);
    const std::string part(70000, 'q');
    for (int k = 0; k < 3; ++k) {
        encoder.write(reinterpret_cast<const uint8_t*>(part.data()), part.size());
    }
    encoder.finish();
    check(decompress(packed.data(), packed.size(), noSizeLimit).size() == 3 * part.size(),
        "encoder output is one valid frame");
    size_t offset = 0;
    Decoder decoder(
        [&](uint8_t* buffer, size_t capacity) {
            const size_t count
                = std::min<size_t>(capacity, std::min<size_t>(7, packed.size() - offset));
            std::copy(packed.begin() + offset, packed.begin() + offset + count, buffer);
            offset += count;
            return count;
        },
        noSizeLimit);
    uint8_t block[333];
    size_t total = 0;
    for (size_t count = decoder.read(block, sizeof(block)); count > 0;
        count = decoder.read(block, sizeof(block))) {
        total += count;
    }
    check(total == 3 * part.size(), "decoder with tiny input and output blocks");
    offset = 0;
    decoder.reset();
    check(decoder.read(block, 10) == 10 && block[0] == 'q', "decoder reset");
    std::vector<uint8_t> truncated(packed.begin(), packed.end() - 3);
    offset = 0;
    Decoder broken(
        [&](uint8_t* buffer, size_t capacity) {
            const size_t count = std::min(capacity, truncated.size() - offset);
            std::copy(truncated.begin() + offset, truncated.begin() + offset + count, buffer);
            offset += count;
            return count;
        },
        noSizeLimit);
    checkThrowsZstd(
        [&] {
            while (broken.read(block, sizeof(block)) > 0) { }
        },
        ZstdErrorCode::InvalidData, "decoder truncated data");
}
//=============================================================================
static void
checkThrowsZstd(const std::function<void()>& action, Nelson::ZstdCodec::ZstdErrorCode expected,
    const char* message)
{
    try {
        action();
    } catch (const Nelson::ZstdCodec::ZstdError& error) {
        check(error.code() == expected, message);
        return;
    } catch (...) {
    }
    check(false, message);
}
//=============================================================================
// Archives written by 7-Zip: WinZip AES-256 (deflate) and traditional PKWARE encryption.
static void
testEncryptedEntries()
{
    std::cerr << "encrypted entries\n";
    std::vector<uint8_t> data(100000);
    for (size_t i = 0; i < data.size(); ++i) {
        data[i] = static_cast<uint8_t>((31 * i * i + 7 * i + 3) % 256);
    }
    const std::pair<const uint8_t*, size_t> archives[] = {
        { aes256DeflateArchive, sizeof(aes256DeflateArchive) },
        { zipCryptoArchive, sizeof(zipCryptoArchive) },
    };
    for (const auto& archive : archives) {
        std::vector<uint8_t> bytes(archive.first, archive.first + archive.second);
        ZipArchive reader = ZipArchive::openMemory(bytes);
        const auto hello = reader.find("hello.txt");
        const auto blob = reader.find("data.bin");
        check(hello && blob, "encrypted entries listed");
        if (!hello || !blob) {
            continue;
        }
        check(reader.entries()[*hello].isEncrypted, "entry flagged as encrypted");
        checkThrows(
            [&] { reader.readEntry(*hello); }, ZipErrorCode::EncryptedEntry, "password required");
        reader.setPassword("wrong");
        checkThrows([&] { reader.readEntry(*hello); }, ZipErrorCode::WrongPassword,
            "wrong password refused");
        reader.setPassword("Secret");
        check(asText(reader.readEntry(*hello)) == "Hello encrypted world!\n", "decrypted text");
        check(reader.readEntry(*blob) == data, "decrypted and inflated data");
        ZipArchive second = reader.reopenForReading();
        check(asText(second.readEntry(*hello)) == "Hello encrypted world!\n",
            "password kept by reopenForReading");
        // Pull reading in small blocks crosses the AES counter blocks.
        std::vector<uint8_t> pulled;
        uint8_t buffer[37];
        reader.openEntry(*blob);
        for (size_t count = reader.readEntryData(buffer, sizeof(buffer)); count > 0;
            count = reader.readEntryData(buffer, sizeof(buffer))) {
            pulled.insert(pulled.end(), buffer, buffer + count);
        }
        reader.closeEntry();
        check(pulled == data, "pull reading of an encrypted entry");
    }
    // A changed payload byte fails the AES authentication code or the CRC (traditional
    // encryption); the payload starts after the 16-byte salt and 2-byte verifier, or the 12-byte
    // header.
    const std::tuple<const uint8_t*, size_t, size_t, const char*> tampering[] = {
        { aes256DeflateArchive, sizeof(aes256DeflateArchive), 16 + 2, "tampered AES entry" },
        { zipCryptoArchive, sizeof(zipCryptoArchive), 12, "tampered traditional entry" },
    };
    for (const auto& [archive, size, skip, label] : tampering) {
        std::vector<uint8_t> bytes(archive, archive + size);
        size_t payload = std::string::npos;
        for (size_t at = findSignature(bytes, localHeader); at != std::string::npos;
            at = findSignature(bytes, localHeader, at + 4)) {
            const uint16_t nameLength = readUint16(bytes, at + 26);
            const uint16_t extraLength = readUint16(bytes, at + 28);
            if (std::string(bytes.begin() + at + 30, bytes.begin() + at + 30 + nameLength)
                == "hello.txt") {
                payload = at + 30 + nameLength + extraLength + skip;
            }
        }
        check(payload != std::string::npos, label);
        if (payload == std::string::npos) {
            continue;
        }
        bytes[payload] ^= 0x01;
        ZipArchive tampered = ZipArchive::openMemory(bytes);
        tampered.setPassword("Secret");
        checkThrows([&] { tampered.readEntry(*tampered.find("hello.txt")); },
            ZipErrorCode::CrcMismatch, label);
    }
}
//=============================================================================
static std::string
toHex(const uint8_t* bytes, size_t size)
{
    static const char digits[] = "0123456789abcdef";
    std::string text;
    for (size_t i = 0; i < size; ++i) {
        text += digits[bytes[i] >> 4];
        text += digits[bytes[i] & 0x0f];
    }
    return text;
}
//=============================================================================
static std::vector<uint8_t>
fromHex(const std::string& hex)
{
    std::vector<uint8_t> bytes;
    for (size_t i = 0; i + 1 < hex.size(); i += 2) {
        bytes.push_back(static_cast<uint8_t>(std::stoi(hex.substr(i, 2), nullptr, 16)));
    }
    return bytes;
}
//=============================================================================
