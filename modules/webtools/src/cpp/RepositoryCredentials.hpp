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
#include <git2.h>
#include <string>
#include "Repository.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct RepositoryCredentialsPayload
{
    explicit RepositoryCredentialsPayload(const RepositoryCredentialsOptions& credentials)
        : username(wstring_to_utf8(credentials.username))
        , password(wstring_to_utf8(credentials.password))
        , useAgent(credentials.useAgent)
        , privateKey(wstring_to_utf8(credentials.privateKey))
        , publicKey(wstring_to_utf8(credentials.publicKey))
        , passphrase(wstring_to_utf8(credentials.passphrase))
    {
    }

    std::string username;
    std::string password;
    bool useAgent;
    std::string privateKey;
    std::string publicKey;
    std::string passphrase;
};
//=============================================================================
static inline const char*
asNullableCString(const std::string& value)
{
    return value.empty() ? nullptr : value.c_str();
}
//=============================================================================
static inline std::string
credentialUsername(const RepositoryCredentialsPayload& credentials, const char* usernameFromUrl)
{
    if (!credentials.username.empty()) {
        return credentials.username;
    }
    return usernameFromUrl ? usernameFromUrl : "";
}
//=============================================================================
static inline int
repositoryCredentialsCallback(git_credential** credential, const char* url,
    const char* usernameFromUrl, unsigned int allowedTypes, void* payload)
{
    (void)url;
    auto* credentials = static_cast<RepositoryCredentialsPayload*>(payload);
    if (credentials == nullptr) {
        return GIT_PASSTHROUGH;
    }

    std::string username = credentialUsername(*credentials, usernameFromUrl);
    if ((allowedTypes & GIT_CREDENTIAL_USERNAME) && !username.empty()) {
        return git_credential_username_new(credential, username.c_str());
    }
    if ((allowedTypes & GIT_CREDENTIAL_SSH_KEY) && !username.empty()) {
        if (credentials->useAgent) {
            return git_credential_ssh_key_from_agent(credential, username.c_str());
        }
        if (!credentials->privateKey.empty()) {
            return git_credential_ssh_key_new(credential, username.c_str(),
                asNullableCString(credentials->publicKey), credentials->privateKey.c_str(),
                asNullableCString(credentials->passphrase));
        }
    }
    if ((allowedTypes & GIT_CREDENTIAL_USERPASS_PLAINTEXT)
        && (!credentials->username.empty() || !credentials->password.empty())) {
        return git_credential_userpass_plaintext_new(
            credential, credentials->username.c_str(), credentials->password.c_str());
    }
    return GIT_PASSTHROUGH;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
