//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
#endif
//=============================================================================
#include <boost/filesystem.hpp>
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "ParserInterface.hpp"
#include "IsEmptyScriptFile.hpp"
#include "NelsonConfiguration.hpp"
#include "ChangeDirectory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
mustBeExistingFile(const std::wstring& filename)
{
    bool bIsFile;
    try {
        bIsFile = boost::filesystem::exists(filename) && !boost::filesystem::is_directory(filename);
    } catch (const boost::filesystem::filesystem_error&) {
        bIsFile = false;
    }
    if (!bIsFile) {
        Error(_W("File does not exist.") + L"\n" + filename);
    }
}
//=============================================================================
static FILE*
filePointerWithoutShebang(const boost::filesystem::path& absolutePath)
{
#ifdef _MSC_BUILD
    FILE* fr = _wfopen(absolutePath.generic_wstring().c_str(), L"rt");
#else
    FILE* fr = fopen(absolutePath.generic_string().c_str(), "rt");
#endif
    bool bBOM = false;
    bool bSheBang = false;
    char _buffer[8192];
    fpos_t pos;
    fgetpos(fr, &pos);
    const char* utf8bom = "\xef\xbb\xbf";
    // UTF-8 bom
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
bool
EvaluateScriptFile(Evaluator* eval, const std::wstring& filename, bool bChangeDirectory)
{
    bool bNeedToRestoreDirectory = false;

    mustBeExistingFile(filename);
    if (IsEmptyScriptFile(filename)) {
        return true;
    }
    boost::filesystem::path initialDir = boost::filesystem::current_path();
    boost::filesystem::path fileToEvaluate(filename);
    boost::filesystem::path absolutePath = boost::filesystem::absolute(fileToEvaluate);
    if (fileToEvaluate.has_branch_path() && bChangeDirectory) {
        bNeedToRestoreDirectory = true;
        boost::filesystem::path newDir = fileToEvaluate.parent_path();
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
        pstate = parseFile(fr, absolutePath.generic_string());
        pt = AbstractSyntaxTree::getReferences();
    } catch (const Exception&) {
        eval->popEvaluateFilenameList();
        AbstractSyntaxTree::deleteReferences();
        fclose(fr);
        if (bNeedToRestoreDirectory) {
            ChangeDirectory(initialDir.generic_wstring(), false);
        }
        throw;
    }

    Exception currentException;
    bool needThrowException = false;
    MacroFunctionDef* fptr = nullptr;
    try {
        fptr = new MacroFunctionDef(absolutePath.generic_wstring(), true);
        if (fptr != nullptr) {
            ArrayOfVector argIn;
            fptr->evaluateFunction(eval, argIn, 0);
        }
    } catch (const Exception& ce) {
        currentException = ce;
        needThrowException = true;
    }

    eval->popEvaluateFilenameList();

    AbstractSyntaxTree::deleteReferences(pt);
    AbstractSyntaxTree::clearReferences();

    if (fptr != nullptr) {
        delete fptr;
        fptr = nullptr;
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
