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
namespace {
//=============================================================================
// Shared transfer arguments (clone / export)
//=============================================================================
struct RepoTransferArgs
{
    std::wstring url;
    std::wstring branchOrTag;
    std::wstring localPath;
    std::wstring username;
    std::wstring password;
};
//=============================================================================
RepoTransferArgs
parseRepoTransferArgs(const ArrayOfVector& argIn)
{
    RepoTransferArgs args;

    switch (argIn.size()) {
    case 3:
        args.url = argIn[1].getContentAsWideString();
        args.localPath = argIn[2].getContentAsWideString();
        break;

    case 4:
        args.url = argIn[1].getContentAsWideString();
        args.branchOrTag = argIn[2].getContentAsWideString();
        args.localPath = argIn[3].getContentAsWideString();
        break;

    case 5:
        args.url = argIn[1].getContentAsWideString();
        args.localPath = argIn[2].getContentAsWideString();
        args.username = argIn[3].getContentAsWideString();
        args.password = argIn[4].getContentAsWideString();
        break;

    case 6:
        args.url = argIn[1].getContentAsWideString();
        args.branchOrTag = argIn[2].getContentAsWideString();
        args.localPath = argIn[3].getContentAsWideString();
        args.username = argIn[4].getContentAsWideString();
        args.password = argIn[5].getContentAsWideString();
        break;

    default:
        if (argIn.size() < 3) {
            raiseError2(L"nelson:arguments:tooFewInputs");
        } else {
            raiseError2(L"nelson:arguments:tooManyInputs");
        }
    }

    return args;
}
//=============================================================================
// Command handlers
//=============================================================================
using CommandHandler = void (*)(const ArrayOfVector&, ArrayOfVector&, int, std::wstring&);
//=============================================================================
void
handleClone(const ArrayOfVector& in, ArrayOfVector&, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 0);
    RepoTransferArgs args = parseRepoTransferArgs(in);
    RepositoryClone(args.url, args.username, args.password, args.branchOrTag, args.localPath, err);
}
//=============================================================================
void
handleExport(const ArrayOfVector& in, ArrayOfVector&, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 0);
    RepoTransferArgs args = parseRepoTransferArgs(in);
    RepositoryExport(args.url, args.username, args.password, args.branchOrTag, args.localPath, err);
}
//=============================================================================
void
handleCheckout(const ArrayOfVector& in, ArrayOfVector&, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(in, 3, 3);

    RepositoryCheckout(in[1].getContentAsWideString(), in[2].getContentAsWideString(), err);
}
//=============================================================================
void
handleBranch(const ArrayOfVector& in, ArrayOfVector& out, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(in, 2, 2);

    wstringVector branches = RepositoryBranchList(in[1].getContentAsWideString(), err);

    if (err.empty()) {
        out << ArrayOf::toCellArrayOfCharacterColumnVectors(branches);
    }
}
//=============================================================================
void
handleTag(const ArrayOfVector& in, ArrayOfVector& out, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(in, 2, 2);

    wstringVector tags = RepositoryTagList(in[1].getContentAsWideString(), err);

    if (err.empty()) {
        out << ArrayOf::toCellArrayOfCharacterColumnVectors(tags);
    }
}
//=============================================================================
void
handleRemoveBranch(const ArrayOfVector& in, ArrayOfVector&, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(in, 3, 3);

    RepositoryRemoveBranch(in[1].getContentAsWideString(), in[2].getContentAsWideString(), err);
}
//=============================================================================
void
handleFetch(const ArrayOfVector& in, ArrayOfVector&, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 0);

    if (in.size() != 2 && in.size() != 4) {
        raiseError2(L"nelson:arguments:wrongNumberOfInputs");
    }

    std::wstring username;
    std::wstring password;

    if (in.size() == 4) {
        username = in[2].getContentAsWideString();
        password = in[3].getContentAsWideString();
    }

    RepositoryFetch(in[1].getContentAsWideString(), username, password, err);
}

void
handleLog(const ArrayOfVector& in, ArrayOfVector& out, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(in, 2, 2);

    ArrayOf logs = RepositoryLog(in[1].getContentAsWideString(), err);

    if (err.empty()) {
        out << logs;
    }
}
//=============================================================================
void
handleCurrentBranch(const ArrayOfVector& in, ArrayOfVector& out, int nLhs, std::wstring& err)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(in, 2, 2);

    std::wstring branch = RepositoryGetCurrentBranchName(in[1].getContentAsWideString(), err);

    if (err.empty()) {
        out << ArrayOf::characterArrayConstructor(branch);
    }
}
//=============================================================================
// Dispatch table
//=============================================================================
struct CommandEntry
{
    const wchar_t* name;
    CommandHandler handler;
};
//=============================================================================
static const CommandEntry commandTable[] = { { L"clone", handleClone }, { L"export", handleExport },
    { L"checkout", handleCheckout }, { L"branch", handleBranch }, { L"tag", handleTag },
    { L"remove_branch", handleRemoveBranch }, { L"fetch", handleFetch }, { L"log", handleLog },
    { L"current_branch", handleCurrentBranch } };
//=============================================================================
static const size_t commandTableSize = sizeof(commandTable) / sizeof(commandTable[0]);
//=============================================================================
} // anonymous namespace
//=============================================================================
// repoBuiltin
//=============================================================================
ArrayOfVector
Nelson::WebtoolsGateway::repoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    nargoutcheck(nLhs, 0, 2);

    if (argIn.empty()) {
        raiseError2(L"nelson:arguments:tooFewInputs");
    }

    std::wstring errorMessage;
    std::wstring command = argIn[0].getContentAsWideString();

    for (size_t i = 0; i < commandTableSize; ++i) {
        if (command == commandTable[i].name) {
            commandTable[i].handler(argIn, retval, nLhs, errorMessage);

            if (!errorMessage.empty()) {
                Error(errorMessage, L"Nelson:webtools:ERROR_REPO_ERROR");
            }

            return retval;
        }
    }
    raiseError2(L"nelson:validators:invalidValueAtPosition", 1);
    return retval;
}
//=============================================================================
