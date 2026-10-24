//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "ApplicationExecutable.hpp"
#include "ApplicationDependencies.hpp"
#include "ApplicationDistribution.hpp"
#include "ApplicationArchiveFormat.hpp"
#include "ApplicationDirectory.hpp"
#include "characters_encoding.hpp"
#include <filesystem>
#include <fstream>
#include <map>
#include <random>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
namespace Nelson {
//=============================================================================
namespace {
    namespace fs = std::filesystem;
    namespace D = Application::Distribution;
    using Application::require;
    using Inventory = std::map<std::string, std::pair<fs::path, std::string>>;
    constexpr const char* transactionName = ".nelsonc-publish";
    constexpr const char* lockName = ".nelsonc-publish.lock";
    // The empty lock file is permanent: unlinking it would let another process lock a new inode.
    class PublicationLock
    {
    public:
        explicit PublicationLock(const fs::path& directory)
        {
            const auto path = directory / lockName;
#ifdef _WIN32
            handle = CreateFileW(path.c_str(), GENERIC_READ | GENERIC_WRITE, 0, nullptr,
                OPEN_ALWAYS, FILE_FLAG_OPEN_REPARSE_POINT, nullptr);
            require(handle != INVALID_HANDLE_VALUE,
                "Application publication is busy or its lock is unavailable.");
            BY_HANDLE_FILE_INFORMATION info {};
            if (!GetFileInformationByHandle(handle, &info)
                || (info.dwFileAttributes
                    & (FILE_ATTRIBUTE_REPARSE_POINT | FILE_ATTRIBUTE_DIRECTORY))
                || info.nFileSizeHigh || info.nFileSizeLow || info.nNumberOfLinks != 1) {
                CloseHandle(handle);
                throw std::runtime_error("Invalid application publication lock file.");
            }
#else
            handle = open(path.c_str(), O_CREAT | O_RDWR | O_NOFOLLOW | O_CLOEXEC, 0600);
            require(handle >= 0, "Application publication lock is unavailable.");
            struct stat info {}, named {};
            if (flock(handle, LOCK_EX | LOCK_NB) != 0 || fstat(handle, &info) != 0
                || lstat(path.c_str(), &named) != 0 || !S_ISREG(info.st_mode) || info.st_size != 0
                || info.st_nlink != 1 || info.st_dev != named.st_dev
                || info.st_ino != named.st_ino) {
                close(handle);
                throw std::runtime_error("Application publication is busy or its lock is invalid.");
            }
#endif
        }
        ~PublicationLock()
        {
#ifdef _WIN32
            CloseHandle(handle);
#else
            close(handle);
#endif
        }
        PublicationLock(const PublicationLock&) = delete;
        PublicationLock&
        operator=(const PublicationLock&)
            = delete;

    private:
#ifdef _WIN32
        HANDLE handle = INVALID_HANDLE_VALUE;
#else
        int handle = -1;
#endif
    };
    //=============================================================================
    std::string
    pathKey(const fs::path& path)
    {
#ifdef _WIN32
        return D::key(path.u8string(), true);
#else
        return path.u8string();
#endif
    }
    //=============================================================================
    bool
    missing(const fs::path& path)
    {
        return fs::symlink_status(path).type() == fs::file_type::not_found;
    }
    //=============================================================================
    fs::path
    outputDirectory(const std::wstring& value)
    {
        require(!value.empty() && value.find(L'\0') == std::wstring::npos,
            "Application output directory must be a nonempty path.");
        const auto path = fs::u8path(wstring_to_utf8(value));
        require(fs::is_directory(fs::symlink_status(path)),
            "An ordinary application output directory is required.");
        return path;
    }
    //=============================================================================
    Inventory
    inventory(const D::Json& records)
    {
        require(records.is_array() && records.size() <= 64,
            "Expected application outputs must be an array of at most 64 files.");
        Inventory result;
        for (const auto& item : records) {
            require(item.is_object() && item.size() == 2, "Invalid application output record.");
            const auto name = D::text(item.at("relative"));
            const auto path = fs::u8path(name);
            const auto hash = D::text(item.at("sha256"));
            const auto key = pathKey(path);
            require(ApplicationArchive::validRelativePath(name) && path == path.filename()
                    && key.rfind(".nelsonc-", 0) != 0 && ApplicationArchive::validDigest(hash),
                "Invalid application output identity.");
            require(result.emplace(key, std::make_pair(path, hash)).second,
                "Duplicate application output.");
        }
        require(result.empty() || result.count("buildresult.json"),
            "Application outputs require their build report identity.");
        return result;
    }
    //=============================================================================
    D::Json
    snapshot(const fs::path& directory)
    {
        auto result = D::Json::array();
        for (const auto& item : fs::directory_iterator(directory)) {
            require(fs::is_regular_file(item.symlink_status()) && result.size() < 64,
                "Application outputs must be at most 64 ordinary files.");
            result.push_back({ { "relative", item.path().filename().u8string() },
                { "sha256", Application::digestFile(item.path()) } });
        }
        require(!result.empty(), "No application output files to publish.");
        return result;
    }
    //=============================================================================
    bool
    matches(const fs::path& path, const std::string& hash)
    {
        return !hash.empty() && fs::is_regular_file(fs::symlink_status(path))
            && Application::digestFile(path) == hash;
    }
    //=============================================================================
    void
    verify(const fs::path& directory, const Inventory& files)
    {
        for (const auto& item : files) {
            require(matches(directory / item.second.first, item.second.second),
                "Application output changed or is missing; retained publication needs inspection.");
        }
    }
    //=============================================================================
    void
    checkSubset(const fs::path& directory, const Inventory& files)
    {
        if (missing(directory)) {
            return;
        }
        require(fs::is_directory(fs::symlink_status(directory)), "Invalid publication directory.");
        for (const auto& item : fs::directory_iterator(directory)) {
            const auto found = files.find(pathKey(item.path().filename()));
            require(found != files.end() && matches(item.path(), found->second.second),
                "Unknown or changed file in retained application publication.");
        }
    }
    //=============================================================================
    void
    moveExclusive(const fs::path& source, const fs::path& target)
    {
#ifdef _WIN32
        if (!MoveFileExW(source.c_str(), target.c_str(), MOVEFILE_WRITE_THROUGH)) {
            throw fs::filesystem_error("Cannot publish application file", source, target,
                std::error_code(GetLastError(), std::system_category()));
        }
#else
        // Same-filesystem link/unlink never replaces a competing writer's file.
        fs::create_hard_link(source, target);
        fs::remove(source);
#endif
    }
    //=============================================================================
    void
    cleanSubset(const fs::path& directory, const Inventory& files)
    {
        checkSubset(directory, files);
        if (!missing(directory)) {
            for (const auto& item : files) {
                const auto path = directory / item.second.first;
                if (!missing(path)) {
                    require(
                        matches(path, item.second.second), "Publication changed during cleanup.");
                    fs::remove(path);
                }
            }
            fs::remove(directory);
        }
    }
    //=============================================================================
    void
    cleanTransaction(const fs::path& transaction, const Inventory& previous, const Inventory& next)
    {
        cleanSubset(transaction / "new", next);
        cleanSubset(transaction / "previous", previous);
        fs::remove(transaction / "journal.json");
        fs::remove(transaction);
    }
    //=============================================================================
    struct Publication
    {
        Inventory previous;
        Inventory next;
    };
    //=============================================================================
    Publication
    readJournal(const fs::path& transaction)
    {
        require(fs::is_directory(fs::symlink_status(transaction)), "Invalid retained publication.");
        for (const auto& item : fs::directory_iterator(transaction)) {
            const auto name = item.path().filename();
            require(name == "journal.json" || name == "new" || name == "previous",
                "Unknown retained publication content; inspection is required.");
        }
        const auto file = transaction / "journal.json";
        require(fs::is_regular_file(fs::symlink_status(file)) && fs::file_size(file) <= 1024 * 1024,
            "Missing or invalid publication journal; inspection is required.");
        std::ifstream input(file, std::ios::binary);
        const std::string encoded((std::istreambuf_iterator<char>(input)), {});
        require(!input.bad() && encoded.size() <= 1024 * 1024, "Cannot read publication journal.");
        const auto journal = D::Json::parse(encoded);
        require(journal.is_object() && journal.size() == 4 && journal.at("format") == 1
                && journal.at("kind") == "nelson-application-publication",
            "Unsupported publication journal.");
        Publication result { inventory(journal.at("previous")), inventory(journal.at("next")) };
        require(!result.next.empty(), "Missing next application inventory.");
        checkSubset(transaction / "previous", result.previous);
        checkSubset(transaction / "new", result.next);
        return result;
    }
    //=============================================================================
    std::string
    digestFor(const Inventory& files, const std::string& key)
    {
        const auto found = files.find(key);
        return found == files.end() ? std::string() : found->second.second;
    }
    //=============================================================================
    bool
    nextComplete(const fs::path& target, const Publication& state)
    {
        for (const auto& item : state.next) {
            if (!matches(target / item.second.first, item.second.second)) {
                return false;
            }
        }
        for (const auto& item : state.previous) {
            if (!state.next.count(item.first) && !missing(target / item.second.first)) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    void
    restore(const fs::path& target, const fs::path& transaction, const Publication& state)
    {
        for (const auto& item : state.previous) {
            const auto path = target / item.second.first;
            const bool old = matches(path, item.second.second);
            require(old || missing(path) || matches(path, digestFor(state.next, item.first)),
                "Application output was modified during interrupted publication; recovery "
                "refused.");
            require(
                old || matches(transaction / "previous" / item.second.first, item.second.second),
                "Previous application output is unavailable; recovery refused.");
        }
        for (const auto& item : state.next) {
            const auto path = target / item.second.first;
            require(state.previous.count(item.first) || missing(path)
                    || matches(path, item.second.second),
                "New application output was modified; recovery refused.");
        }
        for (const auto& item : state.next) {
            const auto path = target / item.second.first;
            if (matches(path, item.second.second)
                && !matches(path, digestFor(state.previous, item.first))) {
                fs::remove(path);
            }
        }
        for (const auto& item : state.previous) {
            const auto path = target / item.second.first;
            if (!matches(path, item.second.second)) {
                moveExclusive(transaction / "previous" / item.second.first, path);
            }
        }
        verify(target, state.previous);
    }
    //=============================================================================
    void
    recover(const fs::path& target)
    {
        const auto transaction = target / transactionName;
        if (missing(transaction)) {
            return;
        }
        if (fs::is_directory(fs::symlink_status(transaction)) && fs::is_empty(transaction)) {
            fs::remove(transaction);
            return;
        }
        const auto state = readJournal(transaction);
        if (!nextComplete(target, state)) {
            restore(target, transaction, state);
        }
        cleanTransaction(transaction, state.previous, state.next);
    }
    //=============================================================================
    fs::path
    prepare(const fs::path& from, const fs::path& target, const D::Json& previous)
    {
        const auto next = snapshot(from);
        const auto files = inventory(next);
        const auto old = inventory(previous);
        for (const auto& item : files) {
            require(old.count(item.first) || missing(target / item.second.first),
                "An unrelated file occupies an application output destination.");
        }
        std::random_device random;
        const auto staged = target
            / (".nelsonc-stage-" + std::to_string(random()) + "-" + std::to_string(random()));
        ApplicationArchive::createPrivateDirectory(staged);
        try {
            fs::create_directory(staged / "new");
            fs::create_directory(staged / "previous");
            publishApplicationFiles(
                utf8_to_wstring(from.u8string()), utf8_to_wstring((staged / "new").u8string()));
            verify(staged / "new", files);
            std::ofstream journal(staged / "journal.json", std::ios::binary);
            journal << D::Json({ { "format", 1 }, { "kind", "nelson-application-publication" },
                                   { "previous", previous }, { "next", next } })
                           .dump();
            journal.close();
            require(!journal.fail(), "Cannot write application publication journal.");
            fs::rename(staged, target / transactionName);
        } catch (...) {
            std::error_code ignored;
            fs::remove_all(staged, ignored);
            throw;
        }
        return target / transactionName;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
recoverApplicationFiles(const std::wstring& destination)
{
    const auto to = outputDirectory(destination);
    PublicationLock lock(to);
    recover(to);
}
//=============================================================================
void
replaceApplicationFiles(
    const std::wstring& source, const std::wstring& destination, const std::wstring& expected)
{
    const auto from = outputDirectory(source);
    const auto to = outputDirectory(destination);
    require(!fs::equivalent(from, to), "Distinct application output directories are required.");
    const auto records = D::parse(expected);
    const auto previous = inventory(records);
    PublicationLock lock(to);
    recover(to);
    verify(to, previous);
    const auto transaction = prepare(from, to, records);
    try {
        const auto state = readJournal(transaction);
        for (const auto& item : state.next) {
            require(previous.count(item.first) || missing(to / item.second.first),
                "An unrelated file occupies an application output destination.");
        }
        verify(to, previous);
        for (const auto& item : previous) {
            moveExclusive(to / item.second.first, transaction / "previous" / item.second.first);
        }
        verify(transaction / "previous", previous);
        for (const auto& item : state.next) {
            if (item.first != "buildresult.json") {
                moveExclusive(transaction / "new" / item.second.first, to / item.second.first);
            }
        }
        const auto& report = state.next.at("buildresult.json").first;
        moveExclusive(transaction / "new" / report, to / report);
        verify(to, state.next);
        cleanTransaction(transaction, state.previous, state.next);
    } catch (const std::exception& error) {
        const std::string message = error.what();
        try {
            recover(to);
        } catch (const std::exception& recoveryError) {
            throw std::runtime_error(message + " Recovery retained in " + transaction.u8string()
                + ": " + recoveryError.what());
        }
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
