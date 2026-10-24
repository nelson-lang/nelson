//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <git2.h>
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "characters_encoding.hpp"
#include "Repository.hpp"
#include "RepositoryHelpers.hpp"
#include "ArrayOf.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
startsWith(const std::string& value, const std::string& prefix)
{
    return value.rfind(prefix, 0) == 0;
}
//=============================================================================
std::wstring
gitErrorCodeToMessage(int errorCode)
{
    std::wstring errorMessage;
    if (errorCode != 0) {
        const git_error* e = giterr_last();
        if (e) {
            std::wstring msg = utf8_to_wstring(e->message);
            errorMessage = formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:webtools:repositoryError", L"repository error %d/%d: %s"),
                errorCode, e->klass, msg);
        } else {
            errorMessage = formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:webtools:repositoryErrorCode", L"repository error %d"),
                errorCode);
        }
    }
    return errorMessage;
}
//=============================================================================
bool
RepositoryUrlRequiresSSH(const std::string& url)
{
    if (startsWith(url, "ssh://") || startsWith(url, "git+ssh://")
        || startsWith(url, "ssh+git://")) {
        return true;
    }
    size_t schemePosition = url.find("://");
    size_t atPosition = url.find('@');
    size_t colonPosition = url.find(':');
    return schemePosition == std::string::npos && atPosition != std::string::npos
        && colonPosition != std::string::npos && atPosition < colonPosition;
}
//=============================================================================
bool
RepositorySupportsSSH()
{
    git_libgit2_init();
    bool supportsSSH = (git_libgit2_features() & GIT_FEATURE_SSH) != 0;
    git_libgit2_shutdown();
    return supportsSSH;
}
//=============================================================================
std::wstring
RepositorySSHUnsupportedMessage()
{
    return Nelson::resolveMessageTemplate(L"Nelson:webtools:sshSupportAvailableNelson",
        L"SSH support is not available in this Nelson build of libgit2.");
}
//=============================================================================
std::wstring
RepositoryVersion()
{
    int major = 0;
    int minor = 0;
    int rev = 0;
    git_libgit2_init();
    git_libgit2_version(&major, &minor, &rev);
    git_libgit2_shutdown();
    return std::to_wstring(major) + L"." + std::to_wstring(minor) + L"." + std::to_wstring(rev);
}
//=============================================================================
ArrayOf
RepositoryCapabilities()
{
    git_libgit2_init();
    int features = git_libgit2_features();
#if defined(LIBGIT2_VER_MAJOR)                                                                     \
    && (LIBGIT2_VER_MAJOR > 1 || (LIBGIT2_VER_MAJOR == 1 && LIBGIT2_VER_MINOR >= 9))
    const char* httpsBackend = git_libgit2_feature_backend(GIT_FEATURE_HTTPS);
    const char* sshBackend = git_libgit2_feature_backend(GIT_FEATURE_SSH);
#else
    const char* httpsBackend = nullptr;
    const char* sshBackend = nullptr;
#endif
    git_libgit2_shutdown();

    stringVector fieldnames;
    fieldnames.reserve(7);
    fieldnames.push_back("version");
    fieldnames.push_back("threads");
    fieldnames.push_back("https");
    fieldnames.push_back("ssh");
    fieldnames.push_back("nsec");
    fieldnames.push_back("https_backend");
    fieldnames.push_back("ssh_backend");

    Dimensions dims(1, 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false));
    ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

    auto feature = [features](git_feature_t flag) { return (features & flag) != 0; };
    ArrayOfVector version;
    ArrayOfVector threads;
    ArrayOfVector https;
    ArrayOfVector ssh;
    ArrayOfVector nsec;
    ArrayOfVector httpsBackendField;
    ArrayOfVector sshBackendField;

    version << ArrayOf::characterArray(RepositoryVersion());
    threads << ArrayOf::logicalScalar(feature(GIT_FEATURE_THREADS));
    https << ArrayOf::logicalScalar(feature(GIT_FEATURE_HTTPS));
    ssh << ArrayOf::logicalScalar(feature(GIT_FEATURE_SSH));
    nsec << ArrayOf::logicalScalar(feature(GIT_FEATURE_NSEC));
    httpsBackendField << ArrayOf::characterArray(httpsBackend ? httpsBackend : "");
    sshBackendField << ArrayOf::characterArray(sshBackend ? sshBackend : "");

    st.setFieldAsList("version", version);
    st.setFieldAsList("threads", threads);
    st.setFieldAsList("https", https);
    st.setFieldAsList("ssh", ssh);
    st.setFieldAsList("nsec", nsec);
    st.setFieldAsList("https_backend", httpsBackendField);
    st.setFieldAsList("ssh_backend", sshBackendField);
    return st;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
