//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "repoBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Repository.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// repo - basic git command
// repo('clone', http_path, destination_path)
// repo('checkout', local_path, branch_tag_name)
// branches = repo('branch', local_path)
// tags = repo('tag', local_path)
//=============================================================================
ArrayOfVector
Nelson::WebtoolsGateway::repoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    std::wstring errorMessage;
    std::wstring command = argIn[0].getContentAsWideString();
    if (command == L"clone") {
        nargoutcheck(nLhs, 0, 0);
        std::wstring url;
        std::wstring localPath;
        std::wstring branchOrTag;
        std::wstring username;
        std::wstring password;
        switch (argIn.size()) {
        case 3: {
            // repo('clone', url, destination)
            localPath = argIn[2].getContentAsWideString();
        } break;
        case 4: {
            // repo('clone', url, branch, destination)
            branchOrTag = argIn[2].getContentAsWideString();
            localPath = argIn[3].getContentAsWideString();
        } break;
        case 5: {
            // repo('clone', url, destination, username, password)
            localPath = argIn[2].getContentAsWideString();
            username = argIn[3].getContentAsWideString();
            password = argIn[4].getContentAsWideString();
        } break;
        case 6: {
            // repo('clone', url, branch, destination, username, password)
            branchOrTag = argIn[2].getContentAsWideString();
            localPath = argIn[3].getContentAsWideString();
            username = argIn[4].getContentAsWideString();
            password = argIn[5].getContentAsWideString();
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        } break;
        }
        url = argIn[1].getContentAsWideString();
        if (argIn.size() == 4) {
            branchOrTag = argIn[2].getContentAsWideString();
            localPath = argIn[3].getContentAsWideString();
        } else {
            localPath = argIn[2].getContentAsWideString();
        }
        RepositoryClone(url, username, password, branchOrTag, localPath, errorMessage);
    } else if (command == L"export") {
        nargoutcheck(nLhs, 0, 0);
        std::wstring url;
        std::wstring localPath;
        std::wstring branchOrTag;
        std::wstring username;
        std::wstring password;

        switch (argIn.size()) {
        case 3: {
            // repo('export', url, destination)
            url = argIn[1].getContentAsWideString();
            localPath = argIn[2].getContentAsWideString();
        } break;
        case 4: {
            // repo('export', url, branch, destination)
            url = argIn[1].getContentAsWideString();
            branchOrTag = argIn[2].getContentAsWideString();
            localPath = argIn[3].getContentAsWideString();
        } break;
        case 5: {
            // repo('export', url, destination, username, password)
            url = argIn[1].getContentAsWideString();
            localPath = argIn[2].getContentAsWideString();
            username = argIn[3].getContentAsWideString();
            password = argIn[4].getContentAsWideString();
        } break;
        case 6: {
            // repo('export', url, branch, destination, username, password)
            url = argIn[1].getContentAsWideString();
            branchOrTag = argIn[2].getContentAsWideString();
            localPath = argIn[3].getContentAsWideString();
            username = argIn[4].getContentAsWideString();
            password = argIn[5].getContentAsWideString();
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        }
        RepositoryExport(url, username, username, branchOrTag, localPath, errorMessage);
    } else if (command == L"checkout") {
        nargoutcheck(nLhs, 0, 0);
        nargincheck(argIn, 3, 3);
        std::wstring localPath = argIn[1].getContentAsWideString();
        std::wstring branchOrTag = argIn[2].getContentAsWideString();
        RepositoryCheckout(localPath, branchOrTag, errorMessage);
    } else if (command == L"branch") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].getContentAsWideString();
        wstringVector branches = RepositoryBranchList(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(branches);
        }
    } else if (command == L"tag") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].getContentAsWideString();
        wstringVector branches = RepositoryTagList(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(branches);
        }
    } else if (command == L"remove_branch") {
        nargoutcheck(nLhs, 0, 0);
        nargincheck(argIn, 3, 3);
        std::wstring localPath = argIn[1].getContentAsWideString();
        std::wstring branchName = argIn[2].getContentAsWideString();
        RepositoryRemoveBranch(localPath, branchName, errorMessage);
    } else if (command == L"fetch") {
        // repo('fetch', destination)
        // repo('fetch', destination, username, password)
        nargoutcheck(nLhs, 0, 1);
        bool checkNbArgsIn = (argIn.size() == 2) || (argIn.size() == 4);
        if (!checkNbArgsIn) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        std::wstring username;
        std::wstring password;
        if (argIn.size() == 4) {
            username = argIn[2].getContentAsWideString();
            password = argIn[3].getContentAsWideString();
        }
        std::wstring localPath = argIn[1].getContentAsWideString();
        RepositoryFetch(localPath, username, password, errorMessage);
    } else if (command == L"log") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].getContentAsWideString();
        ArrayOf logs = RepositoryLog(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << logs;
        }
    } else if (command == L"current_branch") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].getContentAsWideString();
        std::wstring currentBranchName = RepositoryGetCurrentBranchName(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << ArrayOf::characterArrayConstructor(currentBranchName);
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
