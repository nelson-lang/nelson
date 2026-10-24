// Copyright (c) 2017-2022 Thomas Fussell
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
#include <string>
#include <vector>

#include <detail/cryptography/cipher.hpp>
#include <detail/cryptography/hash.hpp>

namespace xlnt {
namespace detail {

    struct encryption_header
    {
        std::uint32_t flags;
        std::uint32_t alg_id;
        std::uint32_t alg_id_hash;
        std::uint32_t key_size;
        std::uint32_t provider_type;
        std::u16string csp_name;
    };

    struct encryption_verifier
    {
        std::vector<std::uint8_t> salt;
        std::vector<std::uint8_t> encrypted_verifier;
        std::uint32_t verifier_hash_size;
        std::vector<std::uint8_t> encrypted_verifier_hash;
    };

    struct encryption_info
    {
        bool is_agile = true;

        std::u16string password;

        struct standard_encryption_info
        {
            std::size_t spin_count = 50000;
            std::size_t block_size;
            std::size_t key_bits;
            std::size_t key_bytes;
            std::size_t hash_size;
            cipher_algorithm cipher;
            cipher_chaining chaining;
            hash_algorithm hash;
            std::vector<std::uint8_t> salt;
            std::vector<std::uint8_t> encrypted_verifier;
            // After decrypting the EncryptedVerifierHash field, only the first VerifierHashSize
            // bytes MUST be used.
            std::uint32_t verifier_hash_size;
            // Please check VerifierHashSize after decryption (see above)!
            std::vector<std::uint8_t> encrypted_verifier_hash;
        } standard;

        struct agile_encryption_info
        {
            // key data
            struct
            {
                std::size_t salt_size;
                std::size_t block_size;
                std::size_t key_bits;
                std::size_t hash_size;
                std::string cipher_algorithm;
                std::string cipher_chaining;
                std::string hash_algorithm;
                std::vector<std::uint8_t> salt_value;
            } key_data;

            struct
            {
                std::vector<std::uint8_t> hmac_key;
                std::vector<std::uint8_t> hmac_value;
            } data_integrity;

            struct
            {
                std::size_t spin_count;
                std::size_t salt_size;
                std::size_t block_size;
                std::size_t key_bits;
                std::size_t hash_size;
                std::string cipher_algorithm;
                std::string cipher_chaining;
                hash_algorithm hash;
                std::vector<std::uint8_t> salt_value;
                std::vector<std::uint8_t> verifier_hash_input;
                std::vector<std::uint8_t> verifier_hash_value;
                std::vector<std::uint8_t> encrypted_key_value;
            } key_encryptor;
        } agile;

        std::vector<std::uint8_t>
        calculate_key() const;
    };

} // namespace detail
} // namespace xlnt
