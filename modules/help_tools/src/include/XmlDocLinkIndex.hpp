//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Types.hpp"
#include "XmlDocListOfFiles.hpp"
#include "XmlTarget.hpp"
#include "XmlTransform.hpp"
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// Destination of a help link: the module and the page inside that module.
struct XmlDocLinkTarget
{
    std::wstring moduleName;
    // Path of the XML page relative to the module help root (".xml" extension).
    std::wstring relativeFilename;
    // Rank of the key that reached this page: 0 keyword, 1 page path, 2 alias.
    // A page's own keyword wins over an alias declared by another page.
    int priority = 0;
};
//=============================================================================
// Index "linkend -> page" built from the XML help tree of one or more modules.
// A page is reachable by its <keyword>, its <alias> entries and by its path
// relative to the module root without extension ("plots/surf"), each form with
// or without a "${module}" / "{module}" prefix. These are the same rules as
// xmldoclinkchecker, so a link accepted by the checker is always resolvable.
class XmlDocLinkIndex
{
public:
    void
    addSections(const std::vector<XmlDocSection>& sections);
    // "${module}name" / "{module}name" / "name" -> (module, name), both trimmed.
    static void
    splitLinkend(const std::wstring& linkend, std::wstring& moduleName, std::wstring& name);
    bool
    findInModule(
        const std::wstring& moduleName, const std::wstring& name, XmlDocLinkTarget& target) const;
    // Succeeds when exactly one module defines the name.
    bool
    findUnique(const std::wstring& name, XmlDocLinkTarget& target) const;
    bool
    isValidLinkend(const std::wstring& linkend) const;

private:
    void
    addSection(const XmlDocSection& section);
    void
    addKey(const std::wstring& moduleName, const std::wstring& key, const XmlDocLinkTarget& target,
        int priority);
    std::map<std::wstring, std::map<std::wstring, XmlDocLinkTarget>> byModule;
    std::map<std::wstring, std::vector<XmlDocLinkTarget>> byName;
};
//=============================================================================
// Builds the index of the given XML help directories. Directories are parsed
// once per process and re-read only when their XML files change.
std::shared_ptr<const XmlDocLinkIndex>
xmlDocLinkIndexForDirectories(const wstringVector& xmlDirectories, std::wstring& errorMessage);
//=============================================================================
// Resolves <link linkend="..."> while pages of `currentModules` are generated:
//  - "${module}name": page of that module, as "<rootpath>../module/<page>";
//  - "name": page of a current module, as "<rootpath><page>", otherwise the
//    unique module defining it.
// Links that cannot be resolved are reported through warnings() and left to the
// stylesheet legacy behaviour.
class XmlDocLinkResolver : public XmlLinkResolver
{
public:
    XmlDocLinkResolver(std::shared_ptr<const XmlDocLinkIndex> linkIndex, wstringVector modules,
        DOCUMENT_OUTPUT documentOutput);
    bool
    resolve(const std::string& linkend, const std::string& rootPathPrefix,
        const std::wstring& xmlfile, XmlResolvedLink& resolved) override;
    void
    unresolved(const std::string& linkend, const std::wstring& xmlfile) override;
    wstringVector
    warnings() const;

private:
    void
    fillResolved(const XmlDocLinkTarget& target, const std::string& rootPathPrefix, bool qualified,
        XmlResolvedLink& resolved) const;
    bool
    isCurrentModule(const std::wstring& moduleName) const;
    void
    addWarning(const std::wstring& message);

    std::shared_ptr<const XmlDocLinkIndex> index;
    wstringVector currentModules;
    std::wstring extension;
    mutable std::mutex warningsMutex;
    wstringVector warningMessages;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
