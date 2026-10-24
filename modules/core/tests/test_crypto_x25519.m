%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.x25519.public'), 1);
asserts.isequal(nargin('crypto.x25519.shared'), 2);
asserts.isequal(nargout('crypto.x25519.keypair'), 2);
%=============================================================================
% RFC 7748 section 6.1
aliceSecret = '77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a';
alicePublic = '8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a';
bobSecret = '5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb';
bobPublic = 'de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f';
shared = '4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742';
%=============================================================================
P = crypto.x25519.public(aliceSecret);
asserts.isequal(class(P), 'char');
asserts.isequal(P, alicePublic);
asserts.isequal(crypto.x25519.public(bobSecret), bobPublic);
% uint8 secret key accepted
aliceSecretBytes = uint8(hex2dec(reshape(aliceSecret, 2, [])'))';
asserts.isequal(crypto.x25519.public(aliceSecretBytes), alicePublic);
%=============================================================================
% shared secret is symmetric and matches the RFC
asserts.isequal(crypto.x25519.shared(aliceSecret, bobPublic), shared);
asserts.isequal(crypto.x25519.shared(bobSecret, alicePublic), shared);
%=============================================================================
% keypair generates a working pair
[pub, sec] = crypto.x25519.keypair();
asserts.isequal(length(pub), 64);
asserts.isequal(length(sec), 64);
asserts.isequal(crypto.x25519.public(sec), pub);
% two peers agree on the same secret
[pubA, secA] = crypto.x25519.keypair();
[pubB, secB] = crypto.x25519.keypair();
asserts.isequal(crypto.x25519.shared(secA, pubB), crypto.x25519.shared(secB, pubA));
asserts.isfalse(strcmp(pubA, pubB));
%=============================================================================
% argument errors
asserts.checkerror('crypto.x25519.public()', _('Not enough input arguments.'));
asserts.checkerror('crypto.x25519.public(aliceSecret(1:10))', sprintf(_('%s must be 32 bytes (uint8 vector) or 64 hexadecimal characters.'), 'Secret key'), 'Nelson:core:x25519KeySizeExpected');
asserts.checkerror('crypto.x25519.shared(aliceSecret, bobPublic(1:4))', sprintf(_('%s must be 32 bytes (uint8 vector) or 64 hexadecimal characters.'), 'Public key'), 'Nelson:core:x25519KeySizeExpected');
%=============================================================================
