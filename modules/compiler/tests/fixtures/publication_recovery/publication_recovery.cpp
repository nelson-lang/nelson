// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#include "ApplicationExecutable.hpp"
#include "ApplicationDistribution.hpp"
#include "picoSHA2/picosha2.h"
#include "characters_encoding.hpp"
#include <codecvt>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <locale>
#include <thread>
#ifndef _WIN32
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#endif

namespace fs = std::filesystem;
using Json = Nelson::Application::Distribution::Json;

// Only external packaging services are adapted; the production publisher is linked unchanged.
namespace Nelson {
std::wstring
utf8_to_wstring(const std::string& value)
{
    return std::wstring_convert<std::codecvt_utf8<wchar_t>>().from_bytes(value);
}
std::string
wstring_to_utf8(const std::wstring& value)
{
    return std::wstring_convert<std::codecvt_utf8<wchar_t>>().to_bytes(value);
}
void
publishApplicationFiles(const std::wstring& source, const std::wstring& destination)
{
    for (const auto& file : fs::directory_iterator(fs::u8path(wstring_to_utf8(source)))) {
        fs::copy_file(
            file.path(), fs::u8path(wstring_to_utf8(destination)) / file.path().filename());
    }
}
namespace Application {
    std::string
    digestFile(const fs::path& path)
    {
        std::ifstream input(path, std::ios::binary);
        if (!input) {
            throw std::runtime_error("Missing digest input");
        }
        picosha2::hash256_one_by_one hash;
        char buffer[65536];
        while (input) {
            input.read(buffer, sizeof(buffer));
            hash.process(buffer, buffer + input.gcount());
        }
        hash.finish();
        return picosha2::get_hash_hex_string(hash);
    }
    namespace Distribution {
        Json
        parse(const std::wstring& value)
        {
            return Json::parse(wstring_to_utf8(value));
        }
        std::string
        text(const Json& value)
        {
            return value.get<std::string>();
        }
    }
}
}

void
check(bool value)
{
    if (!value) {
        throw std::runtime_error("Publication fixture assertion failed");
    }
}
void
write(const fs::path& file, const std::string& value)
{
    std::ofstream(file, std::ios::binary) << value;
}
std::wstring
wide(const fs::path& path)
{
    return Nelson::utf8_to_wstring(path.u8string());
}
Json
inventory(const fs::path& directory)
{
    auto result = Json::array();
    for (const auto& file : fs::directory_iterator(directory)) {
        if (file.path().filename().u8string()[0] != '.') {
            result.push_back({ { "relative", file.path().filename().u8string() },
                { "sha256", Nelson::Application::digestFile(file.path()) } });
        }
    }
    return result;
}
void
verify(const fs::path& directory, const Json& files)
{
    for (const auto& file : files) {
        check(Nelson::Application::digestFile(directory / file.at("relative").get<std::string>())
            == file.at("sha256"));
    }
}
void
rejected(const fs::path& target)
{
    bool failed = false;
    try {
        Nelson::recoverApplicationFiles(wide(target));
    } catch (...) {
        failed = true;
    }
    check(failed);
}
void
journal(const fs::path& transaction, const Json& previous, const Json& next)
{
    write(transaction / "journal.json",
        Json({ { "format", 1 }, { "kind", "nelson-application-publication" },
                 { "previous", previous }, { "next", next } })
            .dump());
}

#ifndef _WIN32
void
checkPosix(const fs::path& source, const fs::path& target)
{
    const auto files = inventory(target);
    const auto transaction = target / ".nelsonc-publish";
    fs::create_directories(transaction / "new");
    fs::create_directories(transaction / "previous");
    Nelson::publishApplicationFiles(wide(source), wide(transaction / "new"));
    journal(transaction, files, inventory(source));
    // Termination between link and unlink can leave two names for the same old inode.
    fs::create_hard_link(target / "app", transaction / "previous/app");
    Nelson::recoverApplicationFiles(wide(target));
    verify(target, files);
    check(!fs::exists(transaction));
    fs::create_directories(transaction / "new");
    fs::create_directories(transaction / "previous");
    Nelson::publishApplicationFiles(wide(source), wide(transaction / "new"));
    journal(transaction, files, inventory(source));
    for (const auto& file : files) {
        const auto name = file.at("relative").get<std::string>();
        fs::rename(target / name, transaction / "previous" / name);
    }
    // The same interrupted operation on a new inode must not remove its old backup.
    fs::create_hard_link(transaction / "new/app", target / "app");
    Nelson::recoverApplicationFiles(wide(target));
    verify(target, files);
    const auto lock = target / ".nelsonc-publish.lock";
    fs::rename(lock, target / "original-lock");
    fs::create_symlink(target / "original-lock", lock);
    rejected(target);
    fs::remove(lock);
    fs::rename(target / "original-lock", lock);
    fs::create_hard_link(lock, target / "linked-lock");
    rejected(target);
    fs::remove(target / "linked-lock");
    fs::create_directory_symlink(source, transaction);
    rejected(target);
    fs::remove(transaction);
}

void
checkTermination(const fs::path& source, const fs::path& target)
{
    fs::resize_file(target / "app", 128 * 1024 * 1024);
    const auto previous = inventory(target);
    const auto encoded = Nelson::utf8_to_wstring(previous.dump());
    const auto child = fork();
    check(child >= 0);
    if (child == 0) {
        try {
            Nelson::replaceApplicationFiles(wide(source), wide(target), encoded);
            _exit(0);
        } catch (...) {
            _exit(2);
        }
    }
    int status = 0;
    bool running = true;
    try {
        const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(90);
        while (!fs::exists(target / ".nelsonc-publish/previous/app")) {
            const auto observed = waitpid(child, &status, WNOHANG);
            if (observed == child) {
                running = false;
            }
            check(observed == 0);
            check(std::chrono::steady_clock::now() < deadline);
            std::this_thread::sleep_for(std::chrono::milliseconds(2));
        }
        rejected(target);
        check(kill(child, SIGKILL) == 0);
        const auto observed = waitpid(child, &status, 0);
        running = observed != child;
        check(!running && WIFSIGNALED(status));
    } catch (...) {
        if (running) {
            kill(child, SIGKILL);
            waitpid(child, &status, 0);
        }
        throw;
    }
    Nelson::recoverApplicationFiles(wide(target));
    verify(target, previous);
    check(!fs::exists(target / ".nelsonc-publish"));
    Nelson::replaceApplicationFiles(wide(source), wide(target), encoded);
    verify(target, inventory(source));
    check((fs::status(target / "app").permissions() & fs::perms::owner_exec) != fs::perms::none);
    std::cout << "POSIX_TERMINATION_RECOVERY_OK\n";
}
#endif

int
main(int argc, char** argv)
{
    try {
        check(argc == 2);
        const auto work = fs::absolute(argv[1]);
        check(!fs::exists(work));
        const auto source = work / "source";
        const auto target = work / "target";
        fs::create_directories(source);
        fs::create_directories(target);
        for (const auto* name : { "app", "buildresult.json" }) {
            write(source / name, std::string("new ") + name);
            write(target / name, std::string("old ") + name);
        }
        fs::permissions(source / "app", fs::perms::owner_exec, fs::perm_options::add);
#ifndef _WIN32
        checkPosix(source, target);
        checkTermination(source, target);
#endif
        Nelson::replaceApplicationFiles(
            wide(source), wide(target), Nelson::utf8_to_wstring(inventory(target).dump()));
        verify(target, inventory(source));
        check(!fs::exists(target / ".nelsonc-publish"));
        std::cout << "PUBLICATION_RECOVERY_NATIVE_OK\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
