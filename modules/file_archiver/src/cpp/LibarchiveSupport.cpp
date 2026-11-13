//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LibarchiveSupport.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
libarchiveErrorText(struct archive* handle, const char* fallback)
{
    const char* text = handle != nullptr ? archive_error_string(handle) : nullptr;
    return text != nullptr ? std::string(text) : std::string(fallback);
}
//=============================================================================
ZipErrorCode
libarchiveReadErrorCode(struct archive* handle)
{
    // libarchive tells these cases apart through its message only.
    const std::string message = libarchiveErrorText(handle, "");
    if (message.find("ncrypted") != std::string::npos
        && message.find("not supported") != std::string::npos) {
        return ZipErrorCode::UnsupportedEncryption;
    }
    if (message.find("assphrase") != std::string::npos) {
        return message.find("required") != std::string::npos ? ZipErrorCode::EncryptedEntry
                                                           : ZipErrorCode::WrongPassword;
    }
    if (message.find("CRC") != std::string::npos
        || message.find("uthentication") != std::string::npos) {
        return ZipErrorCode::CrcMismatch;
    }
    return ZipErrorCode::ReadFailed;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
