//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
#endif
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "EvaluateScriptFile.hpp"
#include "BytecodeArtifact.hpp"
#include "ParserInterface.hpp"
#include "IsEmptyScriptFile.hpp"
#include "NelsonConfiguration.hpp"
#include "ChangeDirectory.hpp"
#include <cstdio>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
mustBeExistingFile(const std::wstring& filename)
{
    bool permissionDenied;
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(filename, permissionDenied);
    if (permissionDenied) {
        Error(L"Nelson:run:permissionDenied", L"Permission denied.");
    }
    if (!bIsFile) {
        RaiseError(
            Nelson::resolveMessageTemplate(L"Nelson:core:fileDoesExist", L"File does not exist.")
                + L"\n" + filename,
            L"Nelson:run:fileDoesNotExist");
    }
}
//=============================================================================
static FILE*
filePointerWithoutShebang(const FileSystemWrapper::Path& absolutePath)
{
#ifdef _MSC_BUILD
    FILE* fr = _wfopen(absolutePath.generic_wstring().c_str(), L"rt");
#else
    FILE* fr = fopen(absolutePath.generic_string().c_str(), "rt");
#endif
    if (!fr) {
        return fr;
    }
    bool bBOM = false;
    bool bSheBang = false;
    char _buffer[8192];
    fpos_t pos;
    fgetpos(fr, &pos);
    const char* utf8bom = "\xef\xbb\xbf";
    indexType nread = fread(_buffer, sizeof(char), strlen(utf8bom), fr);
    if (nread == 0) {
        fsetpos(fr, &pos);
        fgetpos(fr, &pos);
    } else {
        _buffer[nread] = '\0';
        bBOM = (strcmp(_buffer, utf8bom) == 0);
        if (bBOM) {
            fgetpos(fr, &pos);
        } else {
            fsetpos(fr, &pos);
        }
    }
    if (fgets(_buffer, 4096, fr) != nullptr) {
        const char* shebang = "#!";
        bSheBang = (strncmp(_buffer, shebang, strlen(shebang)) == 0);
        if (bSheBang) {
            fgetpos(fr, &pos);
        } else {
            fsetpos(fr, &pos);
        }
    } else {
        fsetpos(fr, &pos);
        fgetpos(fr, &pos);
    }
    return fr;
}
//=============================================================================
static bool
evaluateBytecodeFile(Evaluator* eval, const std::wstring& filename, bool changeDirectory)
{
    const auto originalDirectory = FileSystemWrapper::Path::current_path();
    const auto path = FileSystemWrapper::Path::absolute(FileSystemWrapper::Path(filename));
    std::shared_ptr<MacroFunctionDef> function;
    bool changed = false;
    bool pushed = false;
    bool registered = false;
    const auto restore = [&]() {
        if (registered) {
            unregisterTransientScriptOwner(function.get());
            registered = false;
        }
        if (pushed) {
            eval->popEvaluateFilenameList();
            pushed = false;
        }
        if (changed) {
            changed = false;
            ChangeDirectory(originalDirectory.generic_wstring(), false);
        }
    };
    try {
        if (changeDirectory) {
            ChangeDirectory(path.parent_path().generic_wstring(), false);
            changed = true;
        }
        eval->pushEvaluateFilenameList(path.generic_wstring());
        pushed = true;
        function = std::shared_ptr<MacroFunctionDef>(
            new MacroFunctionDef(path.generic_wstring(), false, false),
            [](MacroFunctionDef* definition) { destroyMacroFunctionDef(definition); });
        registerTransientScriptOwner(function);
        registered = true;
        function->evaluateFunction(eval, ArrayOfVector(), 0);
        restore();
    } catch (...) {
        restore();
        throw;
    }
    return true;
}
//=============================================================================
bool
EvaluateScriptFile(Evaluator* eval, const std::wstring& filename, bool bChangeDirectory)
{
    bool bNeedToRestoreDirectory = false;

    mustBeExistingFile(filename);
    if (isBytecodeArtifact(filename)) {
        return evaluateBytecodeFile(eval, filename, bChangeDirectory);
    }
    if (IsEmptyScriptFile(filename)) {
        return true;
    }

    FileSystemWrapper::Path initialDir = FileSystemWrapper::Path::current_path();
    FileSystemWrapper::Path fileToEvaluate(filename);
    FileSystemWrapper::Path absolutePath = FileSystemWrapper::Path::absolute(fileToEvaluate);
    if (absolutePath.has_parent_path() && bChangeDirectory) {
        bNeedToRestoreDirectory = true;
        FileSystemWrapper::Path newDir = absolutePath.parent_path();
        ChangeDirectory(newDir.generic_wstring(), false);
    }
    FILE* fr = filePointerWithoutShebang(absolutePath);
    if (fr == nullptr) {
        if (bNeedToRestoreDirectory) {
            ChangeDirectory(initialDir.generic_wstring(), false);
        }
        return false;
    }
    eval->pushEvaluateFilenameList(absolutePath.generic_wstring());
    ParserState pstate = ParseError;
    AbstractSyntaxTree::clearReferences();
    AbstractSyntaxTreePtrVector pt;

    try {
        pstate = parseFile(eval->lexerContext, fr, absolutePath.generic_string());
        pt = AbstractSyntaxTree::getReferences();
    } catch (const NelsonException&) {
        eval->popEvaluateFilenameList();
        AbstractSyntaxTree::deleteReferences();
        fclose(fr);
        if (bNeedToRestoreDirectory) {
            ChangeDirectory(initialDir.generic_wstring(), false);
        }
        throw;
    }

    NelsonException currentException;
    bool needThrowException = false;
    // Registered as a transient script owner while it runs, so anonymous function
    // handles created during the run can keep the local functions alive after the
    // script returns (onCleanup task, handle in the shared workspace). See
    // registerTransientScriptOwner / lookupTransientScriptOwner.
    // The last reference frees it through destroyMacroFunctionDef, not plain delete,
    // so the retain a capturing handle takes on the exact definition is honoured.
    std::shared_ptr<MacroFunctionDef> fptr;
    try {
        fptr = std::shared_ptr<MacroFunctionDef>(
            new MacroFunctionDef(absolutePath.generic_wstring(), true, false),
            [](MacroFunctionDef* definition) { destroyMacroFunctionDef(definition); });
        registerTransientScriptOwner(fptr);
        try {
            ArrayOfVector argIn;
            fptr->evaluateFunction(eval, argIn, 0);
        } catch (std::bad_alloc&) {
        }
    } catch (const NelsonException& ce) {
        currentException = ce;
        needThrowException = true;
    }
    if (fptr != nullptr) {
        unregisterTransientScriptOwner(fptr.get());
    }

    eval->popEvaluateFilenameList();

    AbstractSyntaxTree::deleteReferences(pt);
    AbstractSyntaxTree::clearReferences();

    fptr.reset();

    if (fr) {
        fclose(fr);
        fr = nullptr;
    }

    if (bNeedToRestoreDirectory) {
        ChangeDirectory(initialDir.generic_wstring(), false);
    }

    if (needThrowException) {
        throw currentException;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
