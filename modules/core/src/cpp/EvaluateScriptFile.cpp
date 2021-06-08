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
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <cstdio>
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "characters_encoding.hpp"
#include "ParserInterface.hpp"
#include "IsEmptyScriptFile.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
changeDir(const wchar_t* path, bool doException)
{
    try {
        boost::filesystem::current_path(path);
    } catch (const boost::filesystem::filesystem_error& e) {
        e.what();
        if (doException) {
            std::string msg
                = str(boost::format(_("Cannot change directory '%s'.")) % wstring_to_utf8(path));
            Error(msg);
        }
    }
}
//=============================================================================
bool
EvaluateScriptFile(Evaluator* eval, const wchar_t* filename, bool bChangeDirectory)
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
    if (IsEmptyScriptFile(filename)) {
        return true;
    }
    boost::filesystem::path initialDir = boost::filesystem::current_path();
    boost::filesystem::path fileToEvaluate(filename);
    boost::filesystem::path absolutePath = boost::filesystem::absolute(fileToEvaluate);
    bool bNeedToRestoreDirectory = false;
    if (fileToEvaluate.has_branch_path()) {
        if (bChangeDirectory) {
            bNeedToRestoreDirectory = true;
            boost::filesystem::path newDir = fileToEvaluate.parent_path();
            changeDir(newDir.generic_wstring().c_str(), false);
        }
    }
    FILE* fr;
#ifdef _MSC_BUILD
    fr = _wfopen(absolutePath.generic_wstring().c_str(), L"rt");
#else
    fr = fopen(absolutePath.generic_string().c_str(), "rt");
#endif
    if (fr == nullptr) {
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        return false;
    }
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
    eval->pushEvaluateFilenameList(absolutePath.generic_wstring());
    ParserState pstate = ParseError;
    AbstractSyntaxTree::clearReferences();
    AbstractSyntaxTreePtrVector pt;
    try {
        pstate = parseFile(fr, absolutePath.generic_string());
        pt = AbstractSyntaxTree::getReferences();
    } catch (const Exception&) {
        AbstractSyntaxTree::deleteReferences();
        fclose(fr);
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        throw;
    }
    if (pstate == FuncDef) {
        MacroFunctionDef* cp = getParsedFunctionDef();
        if (cp == nullptr) {
            pstate = ScriptBlock;
        }
    }
    if (pstate != ScriptBlock) {
        AbstractSyntaxTree::deleteReferences(pt);
        AbstractSyntaxTree::clearReferences();
        fclose(fr);
        Exception e(_W("An valid script expected."));
        eval->popEvaluateFilenameList();
        e.printMe(eval->getInterface());
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        return false;
    } // (pstate == ScriptBlock)

    char* buffer = nullptr;
    rewind(fr);
    struct stat st;
    clearerr(fr);
#ifdef _MSC_VER
    fstat(_fileno(fr), &st);
#else
    fstat(fileno(fr), &st);
#endif
    long cpos = st.st_size;
    if (cpos == 0) {
        fclose(fr);
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        AbstractSyntaxTree::deleteReferences(pt);
        AbstractSyntaxTree::clearReferences();
        return true;
    }
    try {
        buffer = new char[size_t(cpos) + size_t(2)];
        memset(buffer, 0, size_t(cpos) + size_t(2));
    } catch (const std::bad_alloc&) {
        AbstractSyntaxTree::deleteReferences(pt);
        AbstractSyntaxTree::clearReferences();
        fclose(fr);
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        Error(_W("Memory allocation."));
    }
    if (bSheBang || bBOM) {
        fsetpos(fr, &pos);
    }
    indexType n = 0;
    if (buffer != nullptr) {
        n = fread(buffer, sizeof(char), size_t(cpos), fr);
        buffer[n] = '\n';
        buffer[n + 1] = 0;
    }
    fclose(fr);
    size_t stackdepth = eval->callstack.size();
    eval->setCLI(true);
    try {
        NelsonConfiguration::getInstance()->setInterruptPending(false);
        AbstractSyntaxTreePtr tree = getParsedScriptBlock();
        if (tree == nullptr) {
            AbstractSyntaxTree::deleteReferences(pt);
            AbstractSyntaxTree::clearReferences();
            eval->popEvaluateFilenameList();
            if (buffer != nullptr) {
                delete[] buffer;
                buffer = nullptr;
            }
            if (bNeedToRestoreDirectory) {
                changeDir(initialDir.generic_wstring().c_str(), false);
            }
            return false;
        }
        std::string filenameUtf8 = wstring_to_utf8(filename);
        eval->callstack.pushDebug(filenameUtf8, "filename " + filenameUtf8);
        try {
            eval->block(tree);
        } catch (const Exception&) {
            AbstractSyntaxTree::deleteReferences(pt);
            AbstractSyntaxTree::clearReferences();
            tree = nullptr;
            eval->callstack.popDebug();
            eval->popEvaluateFilenameList();
            if (buffer != nullptr) {
                delete[] buffer;
                buffer = nullptr;
            }
            if (bNeedToRestoreDirectory) {
                changeDir(initialDir.generic_wstring().c_str(), false);
            }
            throw;
        }
        AbstractSyntaxTree::deleteReferences(pt);
        AbstractSyntaxTree::clearReferences();
        if (eval->getState() == NLS_STATE_RETURN) {
            if (eval->getDebugDepth() > 0) {
                eval->callstack.popDebug();
                eval->increaseDebugDepth();
                eval->popEvaluateFilenameList();
                if (buffer != nullptr) {
                    delete[] buffer;
                    buffer = nullptr;
                }
                if (bNeedToRestoreDirectory) {
                    changeDir(initialDir.generic_wstring().c_str(), false);
                }
                return true;
            }
        }
        if (eval->getState() == NLS_STATE_QUIT || eval->getState() == NLS_STATE_ABORT) {
            eval->callstack.popDebug();
            eval->popEvaluateFilenameList();
            if (buffer != nullptr) {
                delete[] buffer;
                buffer = nullptr;
            }
            if (bNeedToRestoreDirectory) {
                changeDir(initialDir.generic_wstring().c_str(), false);
            }
            return true;
        }
        eval->callstack.popDebug();
        if (buffer != nullptr) {
            delete[] buffer;
            buffer = nullptr;
        }
        eval->popEvaluateFilenameList();
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        return true;
    } catch (const Exception&) {
        AbstractSyntaxTree::deleteReferences();
        // removes stack
        while (eval->callstack.size() > stackdepth) {
            eval->callstack.popID();
        }
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        throw;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
