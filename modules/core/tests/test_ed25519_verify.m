%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.ed25519.verify'), 4);
asserts.isequal(nargout('crypto.ed25519.verify'), 1);
%=============================================================================
% RFC 8032, section 7.1, TEST 1 (empty message), TEST 2 (1 byte), TEST 3 (2 bytes)
pk1 = 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a';
msg1 = uint8([]);
sig1 = ['e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb882', ...
  '1590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b'];
pk2 = '3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c';
msg2 = uint8(114);
sig2 = ['92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1', ...
  'e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00'];
pk3 = 'fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025';
msg3 = uint8([175, 130]);
sig3 = ['6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b', ...
  '538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a'];
%=============================================================================
R = crypto.ed25519.verify(msg1, sig1, pk1);
asserts.isequal(R, true);
asserts.isequal(class(R), 'logical');
R = crypto.ed25519.verify(msg2, sig2, pk2);
asserts.isequal(R, true);
R = crypto.ed25519.verify(msg3, sig3, pk3);
asserts.isequal(R, true);
%=============================================================================
% uint8 forms for signature and public key, uppercase hex accepted
sig3bytes = uint8(hex2dec(reshape(sig3, 2, [])'))';
pk3bytes = uint8(hex2dec(reshape(pk3, 2, [])'))';
asserts.isequal(crypto.ed25519.verify(msg3, sig3bytes, pk3bytes), true);
asserts.isequal(crypto.ed25519.verify(msg3, upper(sig3), upper(pk3)), true);
asserts.isequal(crypto.ed25519.verify(msg3, string(sig3), string(pk3)), true);
asserts.isequal(crypto.ed25519.verify(msg3', sig3, pk3), true);
%=============================================================================
% tampered signature, message and public key are rejected
badsig = sig3;
badsig(1) = '7';
asserts.isequal(crypto.ed25519.verify(msg3, badsig, pk3), false);
asserts.isequal(crypto.ed25519.verify(uint8([175, 131]), sig3, pk3), false);
badpk = pk3;
badpk(end) = '6';
asserts.isequal(crypto.ed25519.verify(msg3, sig3, badpk), false);
asserts.isequal(crypto.ed25519.verify(msg2, sig3, pk3), false);
%=============================================================================
% char message = UTF-8 bytes; '-file' = raw bytes of a file
msg2char = char(114);
asserts.isequal(crypto.ed25519.verify(msg2char, sig2, pk2), true);
filename = [tempname(), '.bin'];
fid = fopen(filename, 'wb');
fwrite(fid, msg3, 'uint8');
fclose(fid);
asserts.isequal(crypto.ed25519.verify(filename, sig3, pk3, '-file'), true);
asserts.isequal(crypto.ed25519.verify(filename, sig2, pk2, '-file'), false);
rmfile(filename);
%=============================================================================
% argument errors
asserts.checkerror('crypto.ed25519.verify(msg3, sig3)', _('Not enough input arguments.'));
asserts.checkerror('crypto.ed25519.verify(msg3, sig3(1:100), pk3)', _('Signature must be 64 bytes (uint8 vector) or 128 hexadecimal characters.'), 'Nelson:core:ed25519SignatureSizeExpected');
asserts.checkerror('crypto.ed25519.verify(msg3, sig3, pk3(1:10))', _('Public key must be 32 bytes (uint8 vector) or 64 hexadecimal characters.'), 'Nelson:core:ed25519PublicKeySizeExpected');
badhex = ['zz', sig3(3:end)];
asserts.checkerror('crypto.ed25519.verify(msg3, badhex, pk3)', _('#2 argument: hexadecimal characters expected.'), 'Nelson:core:ed25519HexStringExpected');
asserts.checkerror('crypto.ed25519.verify(msg3, sig3, pk3, ''-string'')', _('#4 argument: ''-file'' expected.'), 'Nelson:core:cryptoFileOptionExpected');
asserts.checkerror('crypto.ed25519.verify(3, sig3, pk3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''uint8'' or ''char'''), 'Nelson:types:wrongArgumentTypeExpected');
missing = [tempname(), '.bin'];
asserts.checkerror('crypto.ed25519.verify(missing, sig3, pk3, ''-file'')', sprintf(_('File cannot be read: %s'), missing), 'Nelson:core:cryptoFileNotReadable');
%=============================================================================
