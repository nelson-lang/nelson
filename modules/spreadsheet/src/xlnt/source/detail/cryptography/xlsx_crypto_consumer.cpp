// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2024-2026 xlnt-community
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
// @license: http://www.opensource.org/licenses/mit-license.php
// @author: see AUTHORS file

#include <cstdint>
#include <vector>

#include <xlnt/utils/exceptions.hpp>
#include <detail/binary.hpp>
#include <detail/constants.hpp>
#include <detail/cryptography/aes.hpp>
#include <detail/cryptography/base64.hpp>
#include <detail/cryptography/compound_document.hpp>
#include <detail/cryptography/encryption_info.hpp>
#include <detail/cryptography/value_traits.hpp>
#include <detail/cryptography/xlsx_crypto_consumer.hpp>
#include <detail/external/include_libstudxml.hpp>
#include <detail/serialization/vector_streambuf.hpp>
#include <detail/serialization/xlsx_consumer.hpp>
#include <detail/unicode.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wduplicate-enum"
#pragma clang diagnostic ignored "-Wswitch-enum"
#endif
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

// NOTE: encryption and decryption is not part of the OOXML specification (ECMA-376).
// This implementation is based on the "[MS-OFFCRYPTO]: Office Document Cryptography Structure"
// specification.
namespace {

using xlnt::detail::byte;
using xlnt::detail::encryption_info;
using xlnt::detail::read;

template <typename T>
std::string
format_hex(T value)
{
    // Format example: 0x0000660F
    return fmt::format("{:#010x}", value);
}

template <typename T>
std::string
format_bin(T value)
{
    // Format example: 0b00100100
    return fmt::format("{:#010b}", value);
}

void
check_generic_encryption_flags(std::uint32_t encryption_flags)
{
    // A – Reserved1 (1 bit): A value that MUST be 0 and MUST be ignored.
    // B – Reserved2 (1 bit): A value that MUST be 0 and MUST be ignored.
    if ((encryption_flags & 0x03) != 0) {
        throw xlnt::invalid_file("bad header - encryption flags " + format_bin(encryption_flags));
    }

    // C – fCryptoAPI (1 bit): A flag that specifies whether CryptoAPI RC4 or ECMA-376 encryption
    // [ECMA-376] is used. It MUST be 1 unless fExternal is 1. If fExternal is 1, it MUST be 0.
    //
    // E – fExternal (1 bit): A value that MUST be 1 if extensible encryption is used. If this value
    // is 1, the value of every other field in this structure MUST be 0.
    auto fCryptoAPI = encryption_flags & 0x04;
    auto fExternal = encryption_flags & 0x10;
    if ((fCryptoAPI == 0 && fExternal == 0) || (fCryptoAPI != 0 && fExternal != 0)) {
        throw xlnt::invalid_file(
            "invalid encryption method - encryption flags " + format_bin(encryption_flags));
    }
    //  If this value is 1, the value of every other field in this structure MUST be 0.
    auto fDocProps = encryption_flags & 0x08;
    auto fAES = encryption_flags & 0x20;
    if (fExternal != 0 && !(fCryptoAPI == 0 && fDocProps == 0 && fAES == 0)) {
        throw xlnt::invalid_file(
            "invalid encryption flags for external encryption - encryption flags "
            + format_bin(encryption_flags));
    }

    // F – fAES (1 bit): A value that MUST be 1 if the protected content is an ECMA-376 document
    // [ECMA-376]; otherwise, it MUST be 0. If the fAES bit is 1, the fCryptoAPI bit MUST also be 1.
    if (fAES != 0 && fCryptoAPI == 0) {
        throw xlnt::invalid_file(
            "AES needs crypto API being set - encryption flags " + format_bin(encryption_flags));
    }
}

void
check_standard_encryption_flags(std::uint32_t encryption_flags)
{
    check_generic_encryption_flags(encryption_flags);

    // Flags - The fCryptoAPI and fAES bits MUST be set. The fDocProps bit MUST be 0.
    auto fCryptoAPI = encryption_flags & 0x04;
    auto fAES = encryption_flags & 0x20;
    auto fDocProps = encryption_flags & 0x08;
    if (!(fCryptoAPI != 0 && fAES != 0 && fDocProps == 0)) {
        throw xlnt::invalid_file(
            "invalid standard encryption flags " + format_bin(encryption_flags));
    }
}

void
check_rc4_cryptoapi_encryption_flags(std::uint32_t encryption_flags)
{
    check_generic_encryption_flags(encryption_flags);

    // Flags - The fCryptoAPI bit MUST be set. The fDocProps bit MUST be set
    // if the document properties are not encrypted.
    auto fCryptoAPI = encryption_flags & 0x04;
    if (fCryptoAPI == 0) {
        throw xlnt::invalid_file(
            "invalid RC4 CryptoAPI encryption flags " + format_bin(encryption_flags));
    }
}

void
check_extensible_encryption_flags(std::uint32_t encryption_flags)
{
    check_generic_encryption_flags(encryption_flags);

    // Flags - A value that MUST have the fExternal bit set to 1. All other bits MUST be set to 0.
    // NOTE: the other flags already checked by the generic encryption flags.
    auto fExternal = encryption_flags & 0x10;
    if (fExternal == 0) {
        throw xlnt::invalid_file(
            "invalid extensible encryption flags " + format_bin(encryption_flags));
    }
}

xlnt::detail::encryption_header
read_encryption_header(std::istream& info_stream, std::uint32_t previous_flags_copy)
{
    auto header_size = read<std::uint32_t>(info_stream);
    auto index_at_start = info_stream.tellg();

    xlnt::detail::encryption_header result {};
    result.flags = read<std::uint32_t>(info_stream);
    check_generic_encryption_flags(result.flags);

    // Sanity check: the copy that exists outside of the header must be exactly the same as the one
    // inside the header.
    if (result.flags != previous_flags_copy) {
        throw xlnt::invalid_file("invalid encryption flags: copy has flags "
            + format_bin(previous_flags_copy) + " but header has flags "
            + format_bin(result.flags));
    }

    // SizeExtra (4 bytes): A field that is reserved and for which the value MUST be 0x00000000.
    auto size_extra = read<std::uint32_t>(info_stream);
    if (size_extra != 0) {
        throw xlnt::invalid_file(
            "invalid extra size - expected 0 but got " + std::to_string(size_extra));
    }

    // AlgID (4 bytes): A signed integer that specifies the encryption algorithm.
    result.alg_id = read<std::uint32_t>(info_stream);
    if (result.alg_id != 0x00000000 && // Determined by Flags
        result.alg_id != 0x00006801 && // RC4
        result.alg_id != 0x0000660E && // 128-bit AES
        result.alg_id != 0x0000660F && // 192-bit AES
        result.alg_id != 0x00006610) // 256-bit AES
    {
        throw xlnt::invalid_file(
            "invalid encryption algorithm - alg_id " + format_hex(result.alg_id));
    }
    auto fCryptoAPI = result.flags & 0x04;
    auto fExternal = result.flags & 0x10;
    auto fAES = result.flags & 0x20;
    if (!((fCryptoAPI == 0 && fAES == 0 && fExternal != 0 && result.alg_id == 0)
            || // Determined by the application
            (fCryptoAPI != 0 && fAES == 0 && fExternal == 0 && result.alg_id == 0) || // RC4
            (fCryptoAPI != 0 && fAES == 0 && fExternal == 0 && result.alg_id == 0x00006801)
            || // RC4
            (fCryptoAPI != 0 && fAES != 0 && fExternal == 0 && result.alg_id == 0) || // 128-bit AES
            (fCryptoAPI != 0 && fAES != 0 && fExternal == 0 && result.alg_id == 0x0000660E)
            || // 128-bit AES
            (fCryptoAPI != 0 && fAES != 0 && fExternal == 0 && result.alg_id == 0x0000660F)
            || // 192-bit AES
            (fCryptoAPI != 0 && fAES != 0 && fExternal == 0
                && result.alg_id == 0x00006610))) // 256-bit AES
    {
        throw xlnt::invalid_file("invalid encryption algorithm flags combination - fCryptoAPI = "
            + std::to_string(fCryptoAPI) + "; fAES = " + std::to_string(fAES) + "; fExternal = "
            + std::to_string(fExternal) + "; alg_id = " + format_hex(result.alg_id));
    }

    // AlgIDHash (4 bytes): A signed integer that specifies the hashing algorithm together with the
    // Flags.fExternal bit.
    result.alg_id_hash = read<std::uint32_t>(info_stream);
    if (!((result.alg_id_hash == 0 && fExternal != 0) || // Determined by the application
            (result.alg_id_hash == 0 && fExternal == 0) || // SHA-1
            (result.alg_id_hash == 0x00008004 && fExternal == 0))) // SHA-1
    {
        throw xlnt::invalid_file("invalid hashing algorithm combination - alg_id_hash = "
            + format_hex(result.alg_id_hash) + "; fExternal = " + std::to_string(fExternal));
    }

    // KeySize (4 bytes): An unsigned integer that specifies the number of bits in the encryption
    // key.
    result.key_size = read<std::uint32_t>(info_stream);
    if (result.key_size % 8 != 0) {
        throw xlnt::invalid_file(
            "invalid key size: expected a multiple of 8 but got " + format_hex(result.key_size));
    }
    if (!((result.key_size == 0) || // Determined by Flags
            (result.alg_id == 0x00006801
                && // RC4
                   // 0x00000028 – 0x00000080 (inclusive) in 8-bit increments
                (result.key_size >= 0x00000028 && result.key_size <= 0x00000080))
            || ((result.alg_id == 0x0000660E && result.key_size == 0x00000080) || // 128-bit AES
                (result.alg_id == 0x0000660F && result.key_size == 0x000000C0) || // 192-bit AES
                (result.alg_id == 0x00006610 && result.key_size == 0x00000100))) // 256-bit AES
    ) {
        throw xlnt::invalid_file("invalid key size for algorithm - key_size = "
            + format_hex(result.key_size) + "; alg_id = " + format_hex(result.alg_id));
    }
    // If the Flags field does not have the fCryptoAPI bit set, the KeySize field MUST be
    // 0x00000000. If RC4 is used, the value MUST be compatible with the chosen cryptographic
    // service provider (CSP).
    if (fCryptoAPI == 0 && result.key_size != 0) {
        throw xlnt::invalid_file(
            "fCryptoAPI is not set; expected key size 0 but got " + format_hex(result.key_size));
    }

    // ProviderType (4 bytes): An implementation-specific value that corresponds to constants
    // accepted by the specified CSP. It MUST be compatible with the chosen CSP. If the Flags field
    // does not have the fCryptoAPI bit set, the ProviderType field MUST be 0x00000000.
    result.provider_type = read<std::uint32_t>(info_stream);
    if (fCryptoAPI == 0 && result.provider_type != 0) {
        throw xlnt::invalid_file("fCryptoAPI is not set; expected provider type 0 but got "
            + format_hex(result.provider_type));
    }

    // Reserved1 (4 bytes): A value that is undefined and MUST be ignored.
    /*auto reserved1 =*/read<std::uint32_t>(info_stream);

    // Reserved2 (4 bytes): A value that MUST be 0x00000000 and MUST be ignored.
    auto reserved2 = read<std::uint32_t>(info_stream);
    if (reserved2 != 0) {
        throw xlnt::invalid_file("Reserved2 expected to be 0 but got " + format_hex(reserved2));
    }

    // CSPName (variable): A null-terminated Unicode string that specifies the CSP name.
    const auto csp_name_length
        = static_cast<std::size_t>((header_size - (info_stream.tellg() - index_at_start)) / 2);
    result.csp_name = xlnt::detail::read_string<char16_t>(info_stream, csp_name_length, true);

    return result;
}

void
check_standard_encryption_header(const xlnt::detail::encryption_header& header)
{
    // Flags - The fCryptoAPI and fAES bits MUST be set. The fDocProps bit MUST be 0.
    check_standard_encryption_flags(header.flags);

    // AlgID - This value MUST be 0x0000660E (AES-128), 0x0000660F (AES-192), or 0x00006610
    // (AES-256). KeySize - This value MUST be 0x00000080 (AES-128), 0x000000C0 (AES-192), or
    // 0x00000100 (AES-256).
    if (!((header.alg_id == 0x0000660E && header.key_size == 0x00000080)
            || (header.alg_id == 0x0000660F && header.key_size == 0x000000C0)
            || (header.alg_id == 0x00006610 && header.key_size == 0x00000100))) {
        throw xlnt::invalid_file("AES is required for standard encryption, but got alg_id = "
            + format_hex(header.alg_id) + "; key_size = " + format_hex(header.key_size));
    }

    // AlgIDHash - This value MUST be 0x00008004 (SHA-1).
    if (header.alg_id_hash != 0x00008004) {
        throw xlnt::invalid_file("SHA-1 is required for standard encryption, but got alg_id_hash "
            + format_hex(header.alg_id_hash));
    }
}

void
check_rc4_cryptoapi_encryption_header(const xlnt::detail::encryption_header& header)
{
    // Flags - The fCryptoAPI bit MUST be set. The fDocProps bit MUST be set if the document
    // properties are not encrypted.
    check_rc4_cryptoapi_encryption_flags(header.flags);

    // AlgID - MUST be 0x00006801 (RC4 encryption).
    if (header.alg_id != 0x00006801) {
        throw xlnt::invalid_file("RC4 is required for RC4 CryptoAPI encryption, but got alg_id "
            + format_hex(header.alg_id));
    }

    // AlgIDHash - MUST be 0x00008004 (SHA-1).
    if (header.alg_id_hash != 0x00008004) {
        throw xlnt::invalid_file(
            "SHA-1 is required for RC4 CryptoAPI encryption, but got alg_id_hash "
            + format_hex(header.alg_id_hash));
    }

    // KeySize - MUST be greater than or equal to 0x00000028 bits and less than
    // or equal to 0x00000080 bits, in increments of 8 bits. If set to
    // 0x00000000, it MUST be interpreted as 0x00000028 bits. It MUST
    // be compatible with the chosen cryptographic service provider (CSP).
    if (!(header.key_size == 0
            || (header.key_size >= 0x00000028 && header.key_size <= 0x00000080))) {
        throw xlnt::invalid_file("invalid key size for RC4 CryptoAPI encryption, expected either 0 "
                                 "or >= 0x00000028 and <= 0x00000080, but got "
            + format_hex(header.key_size));
    }

    // ProviderType - MUST be 0x00000001.
    if (header.provider_type == 0) {
        throw xlnt::invalid_file(
            "invalid provider type for RC4 CryptoAPI encryption, expected 1 but got "
            + format_hex(header.provider_type));
    }
}

void
check_extensible_encryption_header(const xlnt::detail::encryption_header& header)
{
    // Flags - A value that MUST have the fExternal bit set to 1. All other bits MUST be set to 0.
    check_extensible_encryption_flags(header.flags);

    // AlgID - A value that MUST be 0x00000000.
    if (header.alg_id != 0) {
        throw xlnt::invalid_file(
            "invalid encryption algorithm for extensible encryption, expected 0 but got "
            + format_hex(header.alg_id));
    }

    // AlgIDHash - A value that MUST be 0x00000000.
    if (header.alg_id_hash != 0) {
        throw xlnt::invalid_file(
            "invalid encryption algorithm hash for extensible encryption, expected 0 but got "
            + format_hex(header.alg_id_hash));
    }

    // KeySize - A value that MUST be 0x00000000.
    if (header.key_size != 0) {
        throw xlnt::invalid_file("invalid key size for extensible encryption, expected 0 but got "
            + format_hex(header.key_size));
    }

    // ProviderType - A value that MUST be 0x00000000.
    if (header.provider_type != 0) {
        throw xlnt::invalid_file(
            "invalid provider type for extensible encryption, expected 0 but got "
            + format_hex(header.provider_type));
    }
}

enum class encryption_verifier_type
{
    rc4,
    aes,
    other
};

xlnt::detail::encryption_verifier
read_encryption_verifier(std::istream& info_stream, encryption_verifier_type encryption_type)
{
    xlnt::detail::encryption_verifier result {};

    // SaltSize (4 bytes): An unsigned integer that specifies the size of the Salt field. It MUST be
    // 0x00000010.
    auto salt_size = read<std::uint32_t>(info_stream);
    if (salt_size != 0x00000010) {
        throw xlnt::invalid_file(
            "invalid salt size: expected 0x00000010 but got " + format_hex(salt_size));
    }

    // Salt (16 bytes): An array of bytes that specifies the salt value used during password hash
    // generation. It MUST NOT be the same data used for the verifier stored encrypted in the
    // EncryptedVerifier field.
    result.salt = xlnt::detail::read_vector<byte>(info_stream, salt_size);

    // EncryptedVerifier (16 bytes): A value that MUST be the randomly generated Verifier value
    // encrypted using the algorithm chosen by the implementation
    result.encrypted_verifier = xlnt::detail::read_vector<byte>(info_stream, 16);

    // The Salt MUST NOT be the same data used for the verifier stored encrypted in the
    // EncryptedVerifier field.
    if (result.salt == result.encrypted_verifier) {
        throw xlnt::invalid_file(
            "the salt must not use the same data used for the encrypted verifier");
    }

    // VerifierHashSize (4 bytes): An unsigned integer that specifies the number of bytes needed to
    // contain the hash of the data used to generate the EncryptedVerifier field.
    result.verifier_hash_size = read<std::uint32_t>(info_stream);

    // Password Verification (Standard Encryption):
    // The number of bytes used by the decrypted Verifier hash is given by the VerifierHashSize
    // field, which MUST be 20.
    if (encryption_type == encryption_verifier_type::aes && result.verifier_hash_size != 20) {
        throw xlnt::invalid_file(
            "encryption algorithm AES must have a verifier hash size of 20 bytes, but got "
            + std::to_string(result.verifier_hash_size));
    }

    // EncryptedVerifierHash (variable): An array of bytes that contains the encrypted form of the
    // hash of the randomly generated Verifier value. The length of the array MUST be the size of
    // the encryption block size multiplied by the number of blocks needed to encrypt the hash of
    // the Verifier. If the encryption algorithm is RC4, the length MUST be 20 bytes. If the
    // encryption algorithm is AES, the length MUST be 32 bytes. After decrypting the
    // EncryptedVerifierHash field, only the first VerifierHashSize bytes MUST be used.
    std::size_t verifier_hash_length = 0;
    switch (encryption_type) {
    case encryption_verifier_type::rc4:
        // If the encryption algorithm is RC4, the length MUST be 20 bytes.
        verifier_hash_length = 20;
        break;
    case encryption_verifier_type::aes:
        // If the encryption algorithm is AES, the length MUST be 32 bytes.
        verifier_hash_length = 32;
        break;
    case encryption_verifier_type::other:
    default:
        throw xlnt::unsupported("unsupported encryption verifier type");
    }

    result.encrypted_verifier_hash
        = xlnt::detail::read_vector<byte>(info_stream, verifier_hash_length);

    return result;
}

std::vector<std::uint8_t>
decrypt_xlsx_standard(const encryption_info& info, std::istream& encrypted_package_stream)
{
    const auto key = info.calculate_key();

    auto decrypted_size = read<std::uint64_t>(encrypted_package_stream);

    std::vector<std::uint8_t> encrypted_segment(4096, 0);
    std::vector<std::uint8_t> decrypted_package;

    while (encrypted_package_stream) {
        encrypted_package_stream.read(reinterpret_cast<char*>(encrypted_segment.data()),
            static_cast<std::streamsize>(encrypted_segment.size()));
        auto decrypted_segment = xlnt::detail::aes_ecb_decrypt(encrypted_segment, key);

        decrypted_package.insert(
            decrypted_package.end(), decrypted_segment.begin(), decrypted_segment.end());
    }

    decrypted_package.resize(static_cast<std::size_t>(decrypted_size));

    return decrypted_package;
}

std::vector<std::uint8_t>
decrypt_xlsx_agile(const encryption_info& info, std::istream& encrypted_package_stream)
{
    const auto key = info.calculate_key();

    auto salt_size = info.agile.key_data.salt_size;
    auto salt_with_block_key = info.agile.key_data.salt_value;
    salt_with_block_key.resize(salt_size + sizeof(std::uint32_t), 0);

    auto& segment = *reinterpret_cast<std::uint32_t*>(salt_with_block_key.data() + salt_size);
    auto total_size = read<std::uint64_t>(encrypted_package_stream);

    std::vector<std::uint8_t> encrypted_segment(4096, 0);
    std::vector<std::uint8_t> decrypted_package;

    while (encrypted_package_stream) {
        auto iv = hash(info.agile.key_encryptor.hash, salt_with_block_key);
        iv.resize(16);

        encrypted_package_stream.read(reinterpret_cast<char*>(encrypted_segment.data()),
            static_cast<std::streamsize>(encrypted_segment.size()));
        auto decrypted_segment = xlnt::detail::aes_cbc_decrypt(encrypted_segment, key, iv);

        decrypted_package.insert(
            decrypted_package.end(), decrypted_segment.begin(), decrypted_segment.end());

        ++segment;
    }

    decrypted_package.resize(static_cast<std::size_t>(total_size));

    return decrypted_package;
}

encryption_info::standard_encryption_info
get_standard_encryption_info(const xlnt::detail::encryption_header& header,
    const xlnt::detail::encryption_verifier& verifier)
{
    encryption_info::standard_encryption_info result {};

    result.cipher = xlnt::detail::cipher_algorithm::aes;
    result.hash = xlnt::detail::hash_algorithm::sha1;
    result.key_bits = header.key_size;
    result.key_bytes = result.key_bits / 8;
    result.salt = verifier.salt;
    result.encrypted_verifier = verifier.encrypted_verifier;
    result.verifier_hash_size = verifier.verifier_hash_size;
    result.encrypted_verifier_hash = verifier.encrypted_verifier_hash;

    return result;
}

template <typename T>
void
parse_with_min(xml::parser& parser, T min, const char* key_name, T& value)
{
    value = parser.attribute<T>(key_name);

    if (value < min) {
        throw xlnt::invalid_file(std::string(key_name) + " must be at least " + std::to_string(min)
            + ", but is " + std::to_string(value));
    }
}

template <typename T>
void
parse_with_max(xml::parser& parser, T max, const char* key_name, T& value)
{
    value = parser.attribute<T>(key_name);

    if (value > max) {
        throw xlnt::invalid_file(std::string(key_name) + " must be at most " + std::to_string(max)
            + ", but is " + std::to_string(value));
    }
}

template <typename T>
void
parse_with_min_max(xml::parser& parser, T min, T max, const char* key_name, T& value)
{
    value = parser.attribute<T>(key_name);

    if (value < min || value > max) {
        throw xlnt::invalid_file(std::string(key_name) + " must be between " + std::to_string(min)
            + " and " + std::to_string(max) + ", but is " + std::to_string(value));
    }
}

void
parse_str_not_empty(xml::parser& parser, const char* key_name, std::string& value)
{
    value = parser.attribute<std::string>(key_name);

    if (value.empty()) {
        throw xlnt::invalid_file(std::string(key_name) + " must not be empty");
    }
}

encryption_info::agile_encryption_info
read_agile_encryption_info(std::istream& info_stream)
{
    using xlnt::detail::decode_base64;

    static const auto& xmlns = xlnt::constants::ns("encryption");
    static const auto& xmlns_p = xlnt::constants::ns("encryption-password");
    // static const auto &xmlns_c = xlnt::constants::namespace_("encryption-certificate");

    encryption_info::agile_encryption_info result {};

    xml::parser parser(info_stream, "EncryptionInfo");

    parser.next_expect(xml::parser::event_type::start_element, xmlns, "encryption");

    auto& key_data = result.key_data;
    parser.next_expect(xml::parser::event_type::start_element, xmlns, "keyData");
    parse_with_min_max<std::size_t>(parser, 1, 65536, "saltSize", key_data.salt_size);
    parse_with_min_max<std::size_t>(parser, 2, 4096, "blockSize", key_data.block_size);
    parse_with_min<std::size_t>(parser, 8, "keyBits", key_data.key_bits);
    parse_with_min_max<std::size_t>(parser, 1, 65536, "hashSize", key_data.hash_size);
    parse_str_not_empty(parser, "cipherAlgorithm", key_data.cipher_algorithm);
    parse_str_not_empty(parser, "cipherChaining", key_data.cipher_chaining);
    parse_str_not_empty(parser, "hashAlgorithm", key_data.hash_algorithm);

    key_data.salt_value = decode_base64(parser.attribute("saltValue"));
    parser.next_expect(xml::parser::event_type::end_element, xmlns, "keyData");

    auto& data_integrity = result.data_integrity;
    parser.next_expect(xml::parser::event_type::start_element, xmlns, "dataIntegrity");
    data_integrity.hmac_key = decode_base64(parser.attribute("encryptedHmacKey"));
    data_integrity.hmac_value = decode_base64(parser.attribute("encryptedHmacValue"));
    parser.next_expect(xml::parser::event_type::end_element, xmlns, "dataIntegrity");

    auto& key_encryptor = result.key_encryptor;
    parser.next_expect(xml::parser::event_type::start_element, xmlns, "keyEncryptors");
    parser.next_expect(xml::parser::event_type::start_element, xmlns, "keyEncryptor");
    parser.attribute("uri");
    bool any_password_key = false;

    while (parser.peek() != xml::parser::event_type::end_element) {
        parser.next_expect(xml::parser::event_type::start_element);

        if (parser.namespace_() == xmlns_p && parser.name() == "encryptedKey") {
            any_password_key = true;
            parse_with_max<std::size_t>(parser, 10000000, "spinCount", key_encryptor.spin_count);
            parse_with_min_max<std::size_t>(parser, 1, 65536, "saltSize", key_encryptor.salt_size);
            parse_with_min_max<std::size_t>(parser, 2, 4096, "blockSize", key_encryptor.block_size);
            parse_with_min<std::size_t>(parser, 8, "keyBits", key_encryptor.key_bits);
            parse_with_min_max<std::size_t>(parser, 1, 65536, "hashSize", key_encryptor.hash_size);
            parse_str_not_empty(parser, "cipherAlgorithm", key_encryptor.cipher_algorithm);
            parse_str_not_empty(parser, "cipherChaining", key_encryptor.cipher_chaining);

            key_encryptor.hash = parser.attribute<xlnt::detail::hash_algorithm>("hashAlgorithm");
            key_encryptor.salt_value = decode_base64(parser.attribute("saltValue"));
            key_encryptor.verifier_hash_input
                = decode_base64(parser.attribute("encryptedVerifierHashInput"));
            key_encryptor.verifier_hash_value
                = decode_base64(parser.attribute("encryptedVerifierHashValue"));
            key_encryptor.encrypted_key_value
                = decode_base64(parser.attribute("encryptedKeyValue"));
        } else {
            throw xlnt::invalid_file("invalid encrypted key type (namespace \""
                + parser.namespace_() + "\", key \"" + parser.name() + "\")");
        }

        parser.next_expect(xml::parser::event_type::end_element);
    }

    if (!any_password_key) {
        throw xlnt::invalid_file("no password key in keyEncryptors");
    }

    parser.next_expect(xml::parser::event_type::end_element, xmlns, "keyEncryptor");
    parser.next_expect(xml::parser::event_type::end_element, xmlns, "keyEncryptors");

    parser.next_expect(xml::parser::event_type::end_element, xmlns, "encryption");

    return result;
}

encryption_info
read_encryption_info(std::istream& info_stream, const std::u16string& password)
{
    encryption_info info {};

    info.password = password;

    // NOTE:
    // - Office Binary Document:
    //   - XOR obfuscation: for backward compatibility with older file formats.
    //   - RC4 Encryption: Version.vMajor MUST be 0x0001 and Version.vMinor MUST be 0x0001
    //   - RC4 CryptoAPI Encryption: Version.vMajor MUST be 0x0002, 0x0003, or 0x0004 and
    //   Version.vMinor MUST be 0x0002
    // - ECMA-376 Document Encryption:
    //   - Standard Encryption: Version.vMajor MUST be 0x0002, 0x0003 or 0x0004, and Version.vMinor
    //   MUST be 0x0002
    //   - Extensible Encryption: Version.vMajor MUST be 0x0003 or 0x0004 and Version.vMinor MUST be
    //   0x0003
    //   - Agile Encryption: Version.vMajor MUST be 0x0004 and Version.vMinor MUST be 0x0004

    auto version_major = read<std::uint16_t>(info_stream);
    auto version_minor = read<std::uint16_t>(info_stream);

    info.is_agile = version_major == 4 && version_minor == 4;

    // Office Binary RC4 Encryption
    if (version_major == 1 && version_minor == 1) {
        throw xlnt::unsupported("unsupported RC4 encryption");
    }
    // ECMA-376 Standard Encryption or Office Binary RC4 CryptoAPI Encryption
    else if ((version_major == 2 || version_major == 3 || version_major == 4)
        && version_minor == 2) {
        auto encryption_flags = read<std::uint32_t>(info_stream);
        // Check generic first - could be either Standard or RC4 CryptoAPI
        check_generic_encryption_flags(encryption_flags);
        auto fCryptoAPI = encryption_flags & 0x04;
        auto fAES = encryption_flags & 0x20;
        auto fExternal = encryption_flags & 0x10;
        bool is_standard = fCryptoAPI != 0 && fAES != 0 && fExternal == 0;
        bool is_rc4_cryptoapi = fCryptoAPI != 0 && fAES == 0 && fExternal == 0;

        if (!is_standard && !is_rc4_cryptoapi) {
            throw xlnt::invalid_file(
                "invalid encryption: version_major = " + std::to_string(version_major)
                + "; version_minor = " + std::to_string(version_minor)
                + "; encryption_flags = " + format_bin(encryption_flags));
        }

        if (is_standard) {
            check_standard_encryption_flags(encryption_flags);
            xlnt::detail::encryption_header header
                = read_encryption_header(info_stream, encryption_flags);
            check_standard_encryption_header(header);
            xlnt::detail::encryption_verifier verifier
                = read_encryption_verifier(info_stream, encryption_verifier_type::aes);
            info.standard = get_standard_encryption_info(header, verifier);
        } else if (is_rc4_cryptoapi) {
            check_rc4_cryptoapi_encryption_flags(encryption_flags);
            xlnt::detail::encryption_header header
                = read_encryption_header(info_stream, encryption_flags);
            check_rc4_cryptoapi_encryption_header(header);
            xlnt::detail::encryption_verifier verifier
                = read_encryption_verifier(info_stream, encryption_verifier_type::rc4);
            throw xlnt::unsupported("unsupported RC4 CryptoAPI encryption");
        }
    }
    // ECMA-376 Extensible Encryption
    else if ((version_major == 3 || version_major == 4) && version_minor == 3) {
        auto encryption_flags = read<std::uint32_t>(info_stream);
        check_extensible_encryption_flags(encryption_flags);
        xlnt::detail::encryption_header header
            = read_encryption_header(info_stream, encryption_flags);
        check_extensible_encryption_header(header);
        xlnt::detail::encryption_verifier verifier
            = read_encryption_verifier(info_stream, encryption_verifier_type::other);
        throw xlnt::unsupported("unsupported extensible encryprion");
    }
    // ECMA-376 Agile Encryption
    else if (version_major == 4 && version_minor == 4) {
        auto reserved = read<std::uint32_t>(info_stream);
        if (reserved != 0x00000040) {
            throw xlnt::invalid_file("bad header for agile encryption flags, reserved was expected "
                                     "to be 0x00000040 but was "
                + format_hex(reserved));
        }

        info.agile = read_agile_encryption_info(info_stream);
    } else {
        throw xlnt::invalid_file("invalid encryption version " + std::to_string(version_major) + "."
            + std::to_string(version_minor));
    }

    return info;
}

std::vector<std::uint8_t>
decrypt_xlsx(const std::vector<std::uint8_t>& bytes, const std::u16string& password)
{
    if (bytes.empty()) {
        throw xlnt::invalid_file("empty file");
    }

    xlnt::detail::vector_istreambuf buffer(bytes);
    std::istream stream(&buffer);
    xlnt::detail::compound_document document(stream);

    auto& encryption_info_stream = document.open_read_stream("/EncryptionInfo");
    auto encryption_info = read_encryption_info(encryption_info_stream, password);

    auto& encrypted_package_stream = document.open_read_stream("/EncryptedPackage");

    return encryption_info.is_agile
        ? decrypt_xlsx_agile(encryption_info, encrypted_package_stream)
        : decrypt_xlsx_standard(encryption_info, encrypted_package_stream);
}

} // namespace

namespace xlnt {
namespace detail {

    std::vector<std::uint8_t>
    decrypt_xlsx(const std::vector<std::uint8_t>& data, const std::string& password)
    {
        return ::decrypt_xlsx(data, utf8_to_utf16(password));
    }

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    std::vector<std::uint8_t>
    decrypt_xlsx(const std::vector<std::uint8_t>& data, std::u8string_view password)
    {
        return ::decrypt_xlsx(data, utf8_to_utf16(password));
    }
#endif

    template <typename T>
    void
    xlsx_consumer::read_internal(std::istream& source, const T& password)
    {
        std::vector<std::uint8_t> data(
            (std::istreambuf_iterator<char>(source)), (std::istreambuf_iterator<char>()));
        const auto decrypted = decrypt_xlsx(data, password);
        vector_istreambuf decrypted_buffer(decrypted);
        std::istream decrypted_stream(&decrypted_buffer);
        read(decrypted_stream);
    }

    void
    xlsx_consumer::read(std::istream& source, const std::string& password)
    {
        return read_internal(source, password);
    }

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    void
    xlsx_consumer::read(std::istream& source, std::u8string_view password)
    {
        return read_internal(source, password);
    }
#endif

} // namespace detail
} // namespace xlnt
