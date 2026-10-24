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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Repository.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isRepositoryCredentialOption(const std::wstring& name)
{
    return name == L"Username" || name == L"Password" || name == L"UseAgent"
        || name == L"PrivateKey" || name == L"PublicKey" || name == L"Passphrase";
}
//=============================================================================
static bool
isRepositoryCredentialOptionAt(const ArrayOfVector& argIn, size_t index)
{
    return index < argIn.size() && isRepositoryCredentialOption(argIn[index].asWideString());
}
//=============================================================================
static void
setRepositoryCredentialOption(
    RepositoryCredentialsOptions& credentials, const std::wstring& name, const ArrayOf& value)
{
    if (name == L"Username") {
        credentials.username = value.asWideString();
    } else if (name == L"Password") {
        credentials.password = value.asWideString();
    } else if (name == L"UseAgent") {
        credentials.useAgent = value.asLogicalScalar();
    } else if (name == L"PrivateKey") {
        credentials.privateKey = value.asWideString();
    } else if (name == L"PublicKey") {
        credentials.publicKey = value.asWideString();
    } else if (name == L"Passphrase") {
        credentials.passphrase = value.asWideString();
    } else {
        Error(L"Nelson:repository:invalidOption", L"Wrong repository credential option.");
    }
}
//=============================================================================
static void
parseRepositoryCredentialOptions(
    const ArrayOfVector& argIn, size_t firstOption, RepositoryCredentialsOptions& credentials)
{
    if ((argIn.size() - firstOption) % 2 != 0) {
        Error(L"Nelson:webtools:nameValueExpected", L"Name-value arguments expected.");
    }
    for (size_t k = firstOption; k < argIn.size(); k += 2) {
        std::wstring name = argIn[k].asWideString();
        setRepositoryCredentialOption(credentials, name, argIn[k + 1]);
    }
}
//=============================================================================
static void
repoCloneCommand(const ArrayOfVector& argIn, std::wstring& errorMessage)
{
    nargincheck(argIn, 3);
    std::wstring url = argIn[1].asWideString();
    std::wstring localPath;
    std::wstring branchOrTag;
    RepositoryCredentialsOptions credentials;
    size_t optionStart = argIn.size();
    if (argIn.size() == 3) {
        localPath = argIn[2].asWideString();
    } else if (argIn.size() == 4) {
        branchOrTag = argIn[2].asWideString();
        localPath = argIn[3].asWideString();
    } else if (isRepositoryCredentialOptionAt(argIn, 3)) {
        localPath = argIn[2].asWideString();
        optionStart = 3;
    } else if (isRepositoryCredentialOptionAt(argIn, 4)) {
        branchOrTag = argIn[2].asWideString();
        localPath = argIn[3].asWideString();
        optionStart = 4;
    } else if (argIn.size() == 5) {
        localPath = argIn[2].asWideString();
        credentials.username = argIn[3].asWideString();
        credentials.password = argIn[4].asWideString();
    } else if (argIn.size() == 6) {
        branchOrTag = argIn[2].asWideString();
        localPath = argIn[3].asWideString();
        credentials.username = argIn[4].asWideString();
        credentials.password = argIn[5].asWideString();
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    parseRepositoryCredentialOptions(argIn, optionStart, credentials);
    RepositoryClone(url, credentials, branchOrTag, localPath, errorMessage);
}
//=============================================================================
static void
repoExportCommand(const ArrayOfVector& argIn, std::wstring& errorMessage)
{
    nargincheck(argIn, 3);
    std::wstring url = argIn[1].asWideString();
    std::wstring localPath;
    std::wstring branchOrTag;
    RepositoryCredentialsOptions credentials;
    size_t optionStart = argIn.size();
    if (argIn.size() == 3) {
        localPath = argIn[2].asWideString();
    } else if (argIn.size() == 4) {
        branchOrTag = argIn[2].asWideString();
        localPath = argIn[3].asWideString();
    } else if (isRepositoryCredentialOptionAt(argIn, 3)) {
        localPath = argIn[2].asWideString();
        optionStart = 3;
    } else if (isRepositoryCredentialOptionAt(argIn, 4)) {
        branchOrTag = argIn[2].asWideString();
        localPath = argIn[3].asWideString();
        optionStart = 4;
    } else if (argIn.size() == 5) {
        localPath = argIn[2].asWideString();
        credentials.username = argIn[3].asWideString();
        credentials.password = argIn[4].asWideString();
    } else if (argIn.size() == 6) {
        branchOrTag = argIn[2].asWideString();
        localPath = argIn[3].asWideString();
        credentials.username = argIn[4].asWideString();
        credentials.password = argIn[5].asWideString();
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    parseRepositoryCredentialOptions(argIn, optionStart, credentials);
    RepositoryExport(url, credentials, branchOrTag, localPath, errorMessage);
}
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
    nargincheck(argIn, 1);
    std::wstring errorMessage;
    std::wstring command = argIn[0].asWideString();
    if (command == L"version") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 1, 1);
        retval << ArrayOf::characterArray(RepositoryVersion());
    } else if (command == L"capabilities") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 1, 1);
        retval << RepositoryCapabilities();
    } else if (command == L"clone") {
        nargoutcheck(nLhs, 0, 0);
        repoCloneCommand(argIn, errorMessage);
    } else if (command == L"export") {
        nargoutcheck(nLhs, 0, 0);
        repoExportCommand(argIn, errorMessage);
    } else if (command == L"checkout") {
        nargoutcheck(nLhs, 0, 0);
        nargincheck(argIn, 3, 3);
        std::wstring localPath = argIn[1].asWideString();
        std::wstring branchOrTag = argIn[2].asWideString();
        RepositoryCheckout(localPath, branchOrTag, errorMessage);
    } else if (command == L"branch") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].asWideString();
        wstringVector branches = RepositoryBranchList(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(branches);
        }
    } else if (command == L"tag") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].asWideString();
        wstringVector branches = RepositoryTagList(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(branches);
        }
    } else if (command == L"remove_branch") {
        nargoutcheck(nLhs, 0, 0);
        nargincheck(argIn, 3, 3);
        std::wstring localPath = argIn[1].asWideString();
        std::wstring branchName = argIn[2].asWideString();
        RepositoryRemoveBranch(localPath, branchName, errorMessage);
    } else if (command == L"fetch") {
        // repo('fetch', destination)
        // repo('fetch', destination, username, password)
        nargoutcheck(nLhs, 0, 1);
        if (argIn.size() < 2) {
            Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
        }
        RepositoryCredentialsOptions credentials;
        if (argIn.size() > 2) {
            if (isRepositoryCredentialOptionAt(argIn, 2)) {
                parseRepositoryCredentialOptions(argIn, 2, credentials);
            } else if (argIn.size() == 4) {
                credentials.username = argIn[2].asWideString();
                credentials.password = argIn[3].asWideString();
            } else if (argIn.size() > 4) {
                Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
            } else {
                Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
            }
        }
        std::wstring localPath = argIn[1].asWideString();
        RepositoryFetch(localPath, credentials, errorMessage);
    } else if (command == L"log") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].asWideString();
        ArrayOf logs = RepositoryLog(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << logs;
        }
    } else if (command == L"current_branch") {
        nargoutcheck(nLhs, 0, 1);
        nargincheck(argIn, 2, 2);
        std::wstring localPath = argIn[1].asWideString();
        std::wstring currentBranchName = RepositoryGetCurrentBranchName(localPath, errorMessage);
        if (errorMessage.empty()) {
            retval << ArrayOf::characterArray(currentBranchName);
        }
    } else {
        Error(L"Nelson:repository:invalidCommand", L"Wrong value for #%d argument.", 1);
    }
    if (!errorMessage.empty()) {
        Error(L"Nelson:repo:operationFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
