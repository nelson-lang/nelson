//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlDocLinkIndex.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <filesystem>
#include <set>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
pageStem(const std::wstring& relativeFilename)
{
    std::filesystem::path stem(relativeFilename);
    stem.replace_extension();
    return stem.generic_wstring();
}
//=============================================================================
void
XmlDocLinkIndex::splitLinkend(
    const std::wstring& linkend, std::wstring& moduleName, std::wstring& name)
{
    moduleName.clear();
    name = linkend;
    StringHelpers::trim(name);
    size_t prefixLength = 0;
    if (StringHelpers::starts_with(name, L"${")) {
        prefixLength = 2;
    } else if (StringHelpers::starts_with(name, L"{")) {
        prefixLength = 1;
    }
    if (prefixLength > 0) {
        size_t end = name.find(L'}', prefixLength);
        if (end != std::wstring::npos) {
            moduleName = name.substr(prefixLength, end - prefixLength);
            name = name.substr(end + 1);
            StringHelpers::trim(moduleName);
            StringHelpers::trim(name);
        }
    }
}
//=============================================================================
void
XmlDocLinkIndex::addKey(const std::wstring& moduleName, const std::wstring& key,
    const XmlDocLinkTarget& target, int priority)
{
    if (key.empty()) {
        return;
    }
    XmlDocLinkTarget ranked = target;
    ranked.priority = priority;
    // Best rank wins; at equal rank the first page keeps the key (root pages
    // are indexed before nested ones).
    std::map<std::wstring, XmlDocLinkTarget>& pages = byModule[moduleName];
    auto existing = pages.find(key);
    if (existing == pages.end()) {
        pages.emplace(key, ranked);
    } else if (priority < existing->second.priority) {
        existing->second = ranked;
    }
    std::vector<XmlDocLinkTarget>& targets = byName[key];
    bool known = false;
    for (const auto& candidate : targets) {
        if (candidate.moduleName == target.moduleName
            && candidate.relativeFilename == target.relativeFilename) {
            known = true;
            break;
        }
    }
    if (!known) {
        targets.push_back(ranked);
    }
}
//=============================================================================
void
XmlDocLinkIndex::addSection(const XmlDocSection& section)
{
    for (const auto& page : section.pages) {
        XmlDocLinkTarget target;
        target.moduleName = section.moduleName;
        target.relativeFilename = page.relativeFilename;
        addKey(section.moduleName, pageStem(page.relativeFilename), target, 1);
        // keywordAndAlias holds the <keyword> first, then the <keyword_alias> entries.
        for (size_t k = 0; k < page.keywordAndAlias.size(); ++k) {
            addKey(section.moduleName, utf8_to_wstring(page.keywordAndAlias[k]), target,
                k == 0 ? 0 : 2);
        }
    }
    for (const auto& child : section.children) {
        addSection(child);
    }
}
//=============================================================================
void
XmlDocLinkIndex::addSections(const std::vector<XmlDocSection>& sections)
{
    for (const auto& section : sections) {
        addSection(section);
    }
}
//=============================================================================
bool
XmlDocLinkIndex::findInModule(
    const std::wstring& moduleName, const std::wstring& name, XmlDocLinkTarget& target) const
{
    auto module = byModule.find(moduleName);
    if (module == byModule.end()) {
        return false;
    }
    auto page = module->second.find(name);
    if (page == module->second.end()) {
        return false;
    }
    target = page->second;
    return true;
}
//=============================================================================
bool
XmlDocLinkIndex::findUnique(const std::wstring& name, XmlDocLinkTarget& target) const
{
    auto it = byName.find(name);
    if (it == byName.end() || it->second.empty()) {
        return false;
    }
    const std::wstring& moduleName = it->second.front().moduleName;
    for (const auto& candidate : it->second) {
        if (candidate.moduleName != moduleName) {
            return false;
        }
    }
    // Several pages of one module may share a key: use the module's own choice.
    return findInModule(moduleName, name, target);
}
//=============================================================================
bool
XmlDocLinkIndex::isValidLinkend(const std::wstring& linkend) const
{
    std::wstring moduleName;
    std::wstring name;
    splitLinkend(linkend, moduleName, name);
    if (name.empty()) {
        return false;
    }
    if (!moduleName.empty()) {
        XmlDocLinkTarget target;
        return findInModule(moduleName, name, target);
    }
    return byName.find(name) != byName.end();
}
//=============================================================================
namespace {
    struct CachedDirectory
    {
        std::uintmax_t fileCount = 0;
        std::filesystem::file_time_type newestWrite;
        std::vector<XmlDocSection> sections;
    };
    //=============================================================================
    std::mutex cacheMutex;
    std::map<std::wstring, CachedDirectory> directoryCache;
    //=============================================================================
} // namespace
//=============================================================================
static void
directorySignature(const std::filesystem::path& directory, std::uintmax_t& fileCount,
    std::filesystem::file_time_type& newestWrite)
{
    fileCount = 0;
    newestWrite = std::filesystem::file_time_type::min();
    std::error_code ec;
    std::filesystem::recursive_directory_iterator it(
        directory, std::filesystem::directory_options::skip_permission_denied, ec);
    std::filesystem::recursive_directory_iterator end;
    for (; !ec && it != end; it.increment(ec)) {
        if (!it->is_regular_file(ec) || it->path().extension() != L".xml") {
            continue;
        }
        fileCount++;
        std::filesystem::file_time_type writeTime = it->last_write_time(ec);
        if (!ec && writeTime > newestWrite) {
            newestWrite = writeTime;
        }
    }
}
//=============================================================================
std::shared_ptr<const XmlDocLinkIndex>
xmlDocLinkIndexForDirectories(const wstringVector& xmlDirectories, std::wstring& errorMessage)
{
    errorMessage.clear();
    std::shared_ptr<XmlDocLinkIndex> index = std::make_shared<XmlDocLinkIndex>();
    std::set<std::wstring> seen;
    std::lock_guard<std::mutex> lock(cacheMutex);
    for (const auto& directory : xmlDirectories) {
        std::error_code ec;
        std::filesystem::path normalized
            = std::filesystem::absolute(std::filesystem::path(directory), ec);
        if (ec) {
            normalized = std::filesystem::path(directory);
        }
        std::wstring key = normalized.lexically_normal().generic_wstring();
        while (key.size() > 1 && key.back() == L'/') {
            key.pop_back();
        }
        if (!seen.insert(key).second) {
            continue;
        }
        std::uintmax_t fileCount = 0;
        std::filesystem::file_time_type newestWrite;
        directorySignature(normalized, fileCount, newestWrite);

        auto cached = directoryCache.find(key);
        if (cached == directoryCache.end() || cached->second.fileCount != fileCount
            || cached->second.newestWrite != newestWrite) {
            CachedDirectory entry;
            entry.fileCount = fileCount;
            entry.newestWrite = newestWrite;
            std::string language;
            if (!xmlDocListOfFiles({ directory }, entry.sections, language, errorMessage)) {
                return nullptr;
            }
            cached = directoryCache.insert_or_assign(key, std::move(entry)).first;
        }
        index->addSections(cached->second.sections);
    }
    return index;
}
//=============================================================================
XmlDocLinkResolver::XmlDocLinkResolver(std::shared_ptr<const XmlDocLinkIndex> linkIndex,
    wstringVector modules, DOCUMENT_OUTPUT documentOutput)
    : index(std::move(linkIndex))
    , currentModules(std::move(modules))
    , extension(documentOutputExtension(documentOutput))
{
}
//=============================================================================
bool
XmlDocLinkResolver::isCurrentModule(const std::wstring& moduleName) const
{
    return std::find(currentModules.begin(), currentModules.end(), moduleName)
        != currentModules.end();
}
//=============================================================================
void
XmlDocLinkResolver::fillResolved(const XmlDocLinkTarget& target, const std::string& rootPathPrefix,
    bool qualified, XmlResolvedLink& resolved) const
{
    std::wstring page = target.relativeFilename;
    StringHelpers::replace_last(page, L".xml", extension);
    resolved.targetModule = wstring_to_utf8(target.moduleName);
    resolved.targetPage = wstring_to_utf8(pageStem(target.relativeFilename));
    resolved.href = rootPathPrefix;
    if (qualified) {
        resolved.href += "../" + resolved.targetModule + "/";
    }
    resolved.href += wstring_to_utf8(page);
}
//=============================================================================
void
XmlDocLinkResolver::addWarning(const std::wstring& message)
{
    std::lock_guard<std::mutex> lock(warningsMutex);
    // The summary XML is transformed twice (toc + summary): report once.
    if (std::find(warningMessages.begin(), warningMessages.end(), message)
        == warningMessages.end()) {
        warningMessages.push_back(message);
    }
}
//=============================================================================
bool
XmlDocLinkResolver::resolve(const std::string& linkend, const std::string& rootPathPrefix,
    const std::wstring& xmlfile, XmlResolvedLink& resolved)
{
    if (!index) {
        return false;
    }
    std::wstring moduleName;
    std::wstring name;
    XmlDocLinkIndex::splitLinkend(utf8_to_wstring(linkend), moduleName, name);
    if (name.empty()) {
        return false;
    }
    XmlDocLinkTarget target;
    if (!moduleName.empty()) {
        if (index->findInModule(moduleName, name, target)) {
            fillResolved(target, rootPathPrefix, true, resolved);
            return true;
        }
        if (index->findUnique(name, target)) {
            addWarning(L"Help link module mismatch in " + xmlfile + L": " + utf8_to_wstring(linkend)
                + L" resolved in module '" + target.moduleName + L"'");
            fillResolved(target, rootPathPrefix, true, resolved);
            return true;
        }
        return false;
    }
    for (const auto& currentModule : currentModules) {
        if (index->findInModule(currentModule, name, target)) {
            fillResolved(target, rootPathPrefix, false, resolved);
            return true;
        }
    }
    if (index->findUnique(name, target)) {
        fillResolved(target, rootPathPrefix, !isCurrentModule(target.moduleName), resolved);
        return true;
    }
    return false;
}
//=============================================================================
void
XmlDocLinkResolver::unresolved(const std::string& linkend, const std::wstring& xmlfile)
{
    addWarning(L"Unresolved help link in " + xmlfile + L": " + utf8_to_wstring(linkend));
}
//=============================================================================
wstringVector
XmlDocLinkResolver::warnings() const
{
    std::lock_guard<std::mutex> lock(warningsMutex);
    return warningMessages;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
