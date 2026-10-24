//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RuntimeInstaller.hpp"
#include <picoSHA2/picosha2.h>
#include <array>
#include <cerrno>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
//=============================================================================
namespace Nelson::RuntimeInstaller {
//=============================================================================
namespace {
    struct Descriptor
    {
        explicit Descriptor(int value) : value(value) { }
        ~Descriptor()
        {
            if (value >= 0) {
                ::close(value);
            }
        }
        int value;
    };
    //=============================================================================
    int
    openInput(const fs::path& path)
    {
        safePath(path);
        const auto fd = ::open(path.c_str(), O_RDONLY | O_CLOEXEC | O_NOFOLLOW | O_NONBLOCK);
        require(fd >= 0, "Cannot open runtime file: " + path.u8string());
        struct stat info {};
        if (::fstat(fd, &info) != 0 || !S_ISREG(info.st_mode)) {
            ::close(fd);
            require(false, "Runtime input is not a regular file.");
        }
        return fd;
    }
    //=============================================================================
    ssize_t
    readBlock(int fd, char* data, size_t size)
    {
        ssize_t count;
        do {
            count = ::read(fd, data, size);
        } while (count < 0 && errno == EINTR);
        require(count >= 0, "Cannot read runtime input.");
        return count;
    }
    //=============================================================================
    void
    writeBlock(int fd, const char* data, size_t size)
    {
        while (size != 0) {
            const auto count = ::write(fd, data, size);
            if (count < 0 && errno == EINTR) {
                continue;
            }
            require(count > 0, "Cannot write runtime data.");
            data += count;
            size -= static_cast<size_t>(count);
        }
    }
    //=============================================================================
    class Temporary
    {
    public:
        explicit Temporary(const fs::path& parent)
        {
            safePath(parent);
            auto text = (parent / ".nelson-runtime-write-XXXXXX").native();
            std::vector<char> buffer(text.begin(), text.end());
            buffer.push_back(0);
            fd = ::mkstemp(buffer.data());
            require(fd >= 0, "Cannot stage runtime file.");
            path = buffer.data();
        }
        ~Temporary()
        {
            if (fd >= 0) {
                ::close(fd);
            }
            if (!path.empty()) {
                ::unlink(path.c_str());
            }
        }
        int fd = -1;
        fs::path path;
    };
    //=============================================================================
} // namespace
//=============================================================================
bool
pathExists(const fs::path& path)
{
    struct stat info {};
    if (::lstat(path.c_str(), &info) == 0) {
        return true;
    }
    require(errno == ENOENT, "Cannot inspect runtime path: " + path.u8string());
    return false;
}
//=============================================================================
void
safePath(const fs::path& path)
{
    require(path.is_absolute() && plain(path.u8string()),
        "Runtime paths must be absolute plain paths.");
    fs::path cursor;
    for (const auto& part : path) {
        require(part != "." && part != "..", "Runtime paths cannot contain dot segments.");
        cursor /= part;
        struct stat info {};
        if (::lstat(cursor.c_str(), &info) != 0) {
            require(errno == ENOENT, "Cannot inspect runtime ancestor.");
            continue;
        }
        require(!S_ISLNK(info.st_mode), "Runtime paths cannot contain symbolic links.");
        if (cursor != path) {
            require(S_ISDIR(info.st_mode), "Runtime ancestor is not a directory.");
        }
    }
}
//=============================================================================
void
directory(const fs::path& path, unsigned mode)
{
    safePath(path);
    fs::path cursor;
    for (const auto& part : path) {
        cursor /= part;
        if (!pathExists(cursor)) {
            require(::mkdir(cursor.c_str(), mode) == 0 && ::chmod(cursor.c_str(), mode) == 0,
                "Cannot create runtime directory.");
        }
    }
    require(fs::is_directory(path), "Expected a runtime directory.");
}
//=============================================================================
std::string
read(const fs::path& path, size_t limit)
{
    Descriptor fd(openInput(path));
    struct stat info {};
    require(::fstat(fd.value, &info) == 0 && info.st_size >= 0
            && static_cast<uint64_t>(info.st_size) <= limit,
        "Runtime metadata size limit exceeded.");
    std::string result;
    std::array<char, 32768> buffer;
    for (;;) {
        const auto count = readBlock(fd.value, buffer.data(), buffer.size());
        if (count == 0) {
            break;
        }
        require(result.size() + count <= limit, "Runtime metadata grew beyond its limit.");
        result.append(buffer.data(), static_cast<size_t>(count));
    }
    return result;
}
//=============================================================================
std::string
digest(const std::string& value)
{
    return picosha2::hash256_hex_string(value);
}
//=============================================================================
std::string
fileDigest(const fs::path& path)
{
    Descriptor fd(openInput(path));
    picosha2::hash256_one_by_one hash;
    std::array<char, 32768> buffer;
    for (;;) {
        const auto count = readBlock(fd.value, buffer.data(), buffer.size());
        if (count == 0) {
            break;
        }
        hash.process(buffer.begin(), buffer.begin() + count);
    }
    hash.finish();
    return picosha2::get_hash_hex_string(hash);
}
//=============================================================================
bool
matches(const fs::path& path, const File& file)
{
    safePath(path);
    struct stat info {};
    if (::lstat(path.c_str(), &info) != 0) {
        require(errno == ENOENT, "Cannot inspect runtime file.");
        return false;
    }
    return S_ISREG(info.st_mode) && (info.st_mode & 07777) == file.mode
        && fileDigest(path) == file.digest;
}
//=============================================================================
void
write(const fs::path& path, const std::string& value, unsigned mode)
{
    safePath(path);
    Temporary temporary(path.parent_path());
    writeBlock(temporary.fd, value.data(), value.size());
    require(::fchmod(temporary.fd, mode) == 0 && ::fsync(temporary.fd) == 0,
        "Cannot flush runtime metadata.");
    safePath(path);
    require(
        ::rename(temporary.path.c_str(), path.c_str()) == 0, "Cannot publish runtime metadata.");
    temporary.path.clear();
}
//=============================================================================
void
copyNew(const fs::path& from, const fs::path& to, const File& expected)
{
    safePath(to);
    Descriptor input(openInput(from));
    Temporary temporary(to.parent_path());
    std::array<char, 32768> buffer;
    for (;;) {
        const auto count = readBlock(input.value, buffer.data(), buffer.size());
        if (count == 0) {
            break;
        }
        writeBlock(temporary.fd, buffer.data(), static_cast<size_t>(count));
    }
    struct stat info {};
    require(::fstat(input.value, &info) == 0, "Cannot inspect copied runtime file.");
#ifdef __APPLE__
    const timespec times[] = { info.st_atimespec, info.st_mtimespec };
#else
    const timespec times[] = { info.st_atim, info.st_mtim };
#endif
    require(::fchmod(temporary.fd, expected.mode) == 0 && ::futimens(temporary.fd, times) == 0
            && ::fsync(temporary.fd) == 0,
        "Cannot flush staged runtime file.");
    require(fileDigest(temporary.path) == expected.digest,
        "Runtime payload changed during installation.");
    safePath(to);
    require(::link(temporary.path.c_str(), to.c_str()) == 0,
        "Runtime destination already exists or cannot be published.");
}
//=============================================================================
Lock::Lock(const fs::path& filename)
{
    safePath(filename);
    descriptor
        = ::open(filename.c_str(), O_RDWR | O_CREAT | O_NOFOLLOW | O_NONBLOCK | O_CLOEXEC, 0600);
    struct stat info {};
    if (descriptor < 0 || ::fstat(descriptor, &info) != 0 || !S_ISREG(info.st_mode)
        || info.st_uid != ::geteuid() || info.st_nlink != 1
        || ::flock(descriptor, LOCK_EX | LOCK_NB) != 0) {
        if (descriptor >= 0) {
            ::close(descriptor);
            descriptor = -1;
        }
        require(false, "Runtime operation lock is unavailable or unsafe.");
    }
}
//=============================================================================
Lock::~Lock()
{
    if (descriptor >= 0) {
        ::close(descriptor);
    }
}
//=============================================================================
void
verify(const fs::path& root, const Inventory& inventory)
{
    for (const auto& name : inventory.directories) {
        const auto path = root / fs::u8path(name);
        safePath(path);
        require(fs::is_directory(path), "Missing runtime directory: " + name);
    }
    for (const auto& [name, file] : inventory.files) {
        require(matches(root / fs::u8path(name), file), "Missing or changed runtime file: " + name);
    }
}
//=============================================================================
} // namespace Nelson::RuntimeInstaller
//=============================================================================
