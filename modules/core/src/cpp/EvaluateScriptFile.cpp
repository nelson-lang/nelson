//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include <boost/filesystem.hpp>
#include <cstdio>
#include "Error.hpp"
#include "EvaluateScriptFile.hpp"
#include "characters_encoding.hpp"
#include "ParserInterface.hpp"
#include "IsEmptyScriptFile.hpp"
#include "AstManager.hpp"
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
            Error(_("Cannot change directory '") + wstring_to_utf8(path) + "'.");
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
        Error(_W("File does not exist."));
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
    if (!fr) {
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        return false;
    }
    bool bBOM = false;
    bool bSheBang = false;
    char buffer[8192];
    fpos_t pos;
    fgetpos(fr, &pos);
    const char* utf8bom = "\xef\xbb\xbf";
    // UTF-8 bom
    indexType nread = fread(buffer, sizeof(char), strlen(utf8bom), fr);
    if (nread == 0) {
        fsetpos(fr, &pos);
        fgetpos(fr, &pos);
    } else {
        buffer[nread] = '\0';
        bBOM = (strcmp(buffer, utf8bom) == 0);
        if (bBOM) {
            fgetpos(fr, &pos);
        } else {
            fsetpos(fr, &pos);
        }
    }
    if (fgets(buffer, 4096, fr) != nullptr) {
        const char* shebang = "#!";
        bSheBang = (strncmp(buffer, shebang, strlen(shebang)) == 0);
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
    resetAstBackupPosition();
    std::vector<ASTPtr> pt;
    try {
        pstate = parseFile(fr, absolutePath.generic_string().c_str());
        pt = getAstUsed();
    } catch (const Exception&) {
        deleteAstVector(getAstUsed());
        resetAstBackupPosition();
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
        deleteAstVector(pt);
        resetAstBackupPosition();
        fclose(fr);
        Exception e(_W("An valid script expected."));
        eval->popEvaluateFilenameList();
        e.printMe(eval->getInterface());
        if (bNeedToRestoreDirectory) {
            changeDir(initialDir.generic_wstring().c_str(), false);
        }
        return false;
    } else // (pstate == ScriptBlock)
    {
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
            deleteAstVector(pt);
            resetAstBackupPosition();
            return true;
        }
        try {
            buffer = new char[cpos + 2];
            memset(buffer, 0, cpos + 2);
        } catch (const std::bad_alloc& ba) {
            deleteAstVector(pt);
            resetAstBackupPosition();
            ba.what();
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
        if (buffer) {
            n = fread(buffer, sizeof(char), cpos, fr);
            buffer[n] = '\n';
            buffer[n + 1] = 0;
        }
        fclose(fr);
        size_t stackdepth = eval->cstack.size();
        eval->setCLI(true);
        try {
            NelsonConfiguration::getInstance()->setInterruptPending(false);
            ASTPtr tree = getParsedScriptBlock();
            if (tree == nullptr) {
                deleteAstVector(pt);
                resetAstBackupPosition();
                eval->popEvaluateFilenameList();
                if (buffer) {
                    delete[] buffer;
                    buffer = nullptr;
                }
                if (bNeedToRestoreDirectory) {
                    changeDir(initialDir.generic_wstring().c_str(), false);
                }
                return false;
            }
            eval->pushDebug(wstring_to_utf8(filename), buffer);
            try {
                if (tree) {
                    eval->block(tree);
                }
            } catch (const Exception&) {
                deleteAstVector(pt);
                resetAstBackupPosition();
                tree = nullptr;
                eval->popDebug();
                eval->popEvaluateFilenameList();
                if (buffer) {
                    delete[] buffer;
                    buffer = nullptr;
                }
                if (bNeedToRestoreDirectory) {
                    changeDir(initialDir.generic_wstring().c_str(), false);
                }
                throw;
            }
            deleteAstVector(pt);
            resetAstBackupPosition();
            if (eval->getState() == NLS_STATE_RETURN) {
                if (eval->getDebugDepth() > 0) {
                    eval->popDebug();
                    eval->increaseDebugDepth();
                    eval->popEvaluateFilenameList();
                    if (buffer) {
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
                eval->popDebug();
                eval->popEvaluateFilenameList();
                if (buffer) {
                    delete[] buffer;
                    buffer = nullptr;
                }
                if (bNeedToRestoreDirectory) {
                    changeDir(initialDir.generic_wstring().c_str(), false);
                }
                return true;
            }
            eval->popDebug();
            if (buffer) {
                delete[] buffer;
                buffer = nullptr;
            }
            eval->popEvaluateFilenameList();
            if (bNeedToRestoreDirectory) {
                changeDir(initialDir.generic_wstring().c_str(), false);
            }
            return true;
        } catch (const Exception&) {
            deleteAstVector(getAstUsed());
            resetAstBackupPosition();
            // removes stack
            while (eval->cstack.size() > stackdepth) {
                eval->cstack.pop_back();
            }
            if (bNeedToRestoreDirectory) {
                changeDir(initialDir.generic_wstring().c_str(), false);
            }
            throw;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
