%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.aead.encrypt'), 4);
asserts.isequal(nargin('crypto.aead.decrypt'), 4);
%=============================================================================
% Known answer produced by the reference XChaCha20-Poly1305 implementation
key = uint8(0:31);
nonce = uint8(64:87);
ad = uint8(240:251);
plaintext = uint8([double('Nelson crypto!!'), 0]);
% boxed = mac(16) followed by ciphertext(16)
mac = uint8(hex2dec(reshape('998ae781650c50be7013711e60c3abcb', 2, [])'))';
ciphertext = uint8(hex2dec(reshape('9a5c6903bf8e5975fd8df7cac0bd4492', 2, [])'))';
boxed = crypto.aead.encrypt(key, nonce, plaintext, ad);
asserts.isequal(class(boxed), 'uint8');
asserts.isequal(boxed, [mac, ciphertext]);
asserts.isequal(crypto.aead.decrypt(key, nonce, boxed, ad), plaintext);
%=============================================================================
% round-trip without associated data, hexadecimal key and nonce accepted
keyHex = crypto.random(32, '-hex');
nonceHex = crypto.random(24, '-hex');
message = uint8('a longer secret message that spans more than one block of the cipher');
boxed = crypto.aead.encrypt(keyHex, nonceHex, message);
asserts.isequal(length(boxed), length(message) + 16);
asserts.isequal(crypto.aead.decrypt(keyHex, nonceHex, boxed), message);
% text plaintext returns its UTF-8 bytes
boxed = crypto.aead.encrypt(key, nonce, 'hello');
asserts.isequal(crypto.aead.decrypt(key, nonce, boxed), uint8('hello'));
% empty plaintext: only the tag
boxed = crypto.aead.encrypt(key, nonce, uint8([]));
asserts.isequal(length(boxed), 16);
asserts.istrue(isempty(crypto.aead.decrypt(key, nonce, boxed)));
%=============================================================================
% authentication rejects any tampering
boxed = crypto.aead.encrypt(key, nonce, plaintext, ad);
tampered = boxed;
tampered(20) = bitxor(tampered(20), uint8(1));
asserts.checkerror('crypto.aead.decrypt(key, nonce, tampered, ad)', _('Authentication failed: the key, nonce, associated data or encrypted message is wrong.'), 'Nelson:core:aeadAuthenticationFailed');
asserts.checkerror('crypto.aead.decrypt(key, nonce, boxed, uint8(1:12))', _('Authentication failed: the key, nonce, associated data or encrypted message is wrong.'), 'Nelson:core:aeadAuthenticationFailed');
asserts.checkerror('crypto.aead.decrypt(uint8(1:32), nonce, boxed, ad)', _('Authentication failed: the key, nonce, associated data or encrypted message is wrong.'), 'Nelson:core:aeadAuthenticationFailed');
asserts.checkerror('crypto.aead.decrypt(key, uint8(1:24), boxed, ad)', _('Authentication failed: the key, nonce, associated data or encrypted message is wrong.'), 'Nelson:core:aeadAuthenticationFailed');
%=============================================================================
% argument errors
asserts.checkerror('crypto.aead.encrypt(key, nonce)', _('Not enough input arguments.'));
asserts.checkerror('crypto.aead.encrypt(uint8(1:8), nonce, plaintext)', sprintf(_('%s must be 32 bytes (uint8 vector) or 64 hexadecimal characters.'), 'Key'), 'Nelson:core:aeadKeySizeExpected');
asserts.checkerror('crypto.aead.encrypt(key, uint8(1:8), plaintext)', sprintf(_('%s must be 24 bytes (uint8 vector) or 48 hexadecimal characters.'), 'Nonce'), 'Nelson:core:aeadNonceSizeExpected');
asserts.checkerror('crypto.aead.decrypt(key, nonce, uint8(1:8))', _('Encrypted message must be at least 16 bytes (the authentication tag).'), 'Nelson:core:aeadCiphertextTooShort');
%=============================================================================
