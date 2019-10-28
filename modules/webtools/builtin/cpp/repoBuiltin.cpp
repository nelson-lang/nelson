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
#include "repoBuiltin.hpp"
#include "Error.hpp"
#include "Repository.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// repo - basic git command
// repo('clone', http_path, destination_path)
// repo('checkout', local_path, branch_tag_name)
// repo('pull', local_path)
// branches = repo('branch', local_path)
// tags = repo('tag', local_path)
//=============================================================================
ArrayOfVector
Nelson::WebtoolsGateway::repoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::wstring errorMessage;
    std::wstring command = param1.getContentAsWideString();
    if (command == L"clone") {
        if (nLhs > 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() < 3 || argIn.size() > 4) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring url;
        std::wstring localPath;
        std::wstring branchOrTag;

        url = argIn[1].getContentAsWideString();
        if (argIn.size() == 4) {
            branchOrTag = argIn[2].getContentAsWideString();
            localPath = argIn[3].getContentAsWideString();
        } else {
            localPath = argIn[2].getContentAsWideString();
        }
        RepositoryClone(url, branchOrTag, localPath, errorMessage);
    } else if (command == L"checkout") {
        if (nLhs > 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 3) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        std::wstring branchOrTag = argIn[2].getContentAsWideString();
        RepositoryCheckout(localPath, branchOrTag, errorMessage);
    } else if (command == L"branch") {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 2) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        wstringVector branches = RepositoryBranchList(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval.push_back(ToCellStringAsColumn(branches));
        }
    } else if (command == L"tag") {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 2) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        wstringVector branches = RepositoryTagList(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval.push_back(ToCellStringAsColumn(branches));
        }
    } else if (command == L"remove_branch") {
        if (nLhs > 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 3) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        std::wstring branchName = argIn[2].getContentAsWideString();
        RepositoryRemoveBranch(localPath, branchName, errorMessage);
    } else if (command == L"fetch") {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 2) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        RepositoryFetch(localPath, errorMessage);
    } else if (command == L"log") {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 2) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        ArrayOf logs = RepositoryLog(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval.push_back(logs);
        }
    } else if (command == L"current_branch") {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (argIn.size() != 2) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        std::wstring currentBranchName = RepositoryGetCurrentBranchName(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval.push_back(ArrayOf::characterArrayConstructor(currentBranchName));
        }
    } else {
        Error(_W("Wrong value for #1 argument."));
    }
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
