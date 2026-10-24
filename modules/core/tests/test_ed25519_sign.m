%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.ed25519.sign'), 3);
asserts.isequal(nargout('crypto.ed25519.sign'), 2);
%=============================================================================
% RFC 8032, section 7.1: signatures are deterministic, TEST 1 and TEST 3
seed1 = '9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60';
pk1 = 'd75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a';
sig1 = ['e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb882', ...
  '1590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b'];
seed3 = 'c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7';
pk3 = 'fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025';
msg3 = uint8([175, 130]);
sig3 = ['6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b', ...
  '538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a'];
%=============================================================================
[S, P] = crypto.ed25519.sign(uint8([]), seed1);
asserts.isequal(S, sig1);
asserts.isequal(P, pk1);
[S, P] = crypto.ed25519.sign(msg3, seed3);
asserts.isequal(S, sig3);
asserts.isequal(P, pk3);
S = crypto.ed25519.sign(msg3, upper(seed3));
asserts.isequal(S, sig3);
seed3bytes = uint8(hex2dec(reshape(seed3, 2, [])'))';
S = crypto.ed25519.sign(msg3, seed3bytes);
asserts.isequal(S, sig3);
%=============================================================================
% sign then verify, text message and file message
[S, P] = crypto.ed25519.sign('Nelson', seed3);
asserts.isequal(crypto.ed25519.verify('Nelson', S, P), true);
asserts.isequal(crypto.ed25519.verify('nelson', S, P), false);
filename = [tempname(), '.json'];
filewrite(filename, '{"packages": []}');
[S, P] = crypto.ed25519.sign(filename, seed3, '-file');
asserts.isequal(crypto.ed25519.verify(filename, S, P, '-file'), true);
asserts.isequal(crypto.ed25519.verify(fileread(filename), S, P), true);
rmfile(filename);
%=============================================================================
% argument errors
asserts.checkerror('crypto.ed25519.sign(msg3)', _('Not enough input arguments.'));
asserts.checkerror('crypto.ed25519.sign(msg3, seed3(1:20))', _('Private seed must be 32 bytes (uint8 vector) or 64 hexadecimal characters.'), 'Nelson:core:ed25519SeedSizeExpected');
badhex = ['zz', seed3(3:end)];
asserts.checkerror('crypto.ed25519.sign(msg3, badhex)', _('#2 argument: hexadecimal characters expected.'), 'Nelson:core:ed25519HexStringExpected');
asserts.checkerror('crypto.ed25519.sign(msg3, seed3, ''-string'')', _('#3 argument: ''-file'' expected.'), 'Nelson:core:cryptoFileOptionExpected');
asserts.checkerror('crypto.ed25519.sign({}, seed3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''uint8'' or ''char'''), 'Nelson:types:wrongArgumentTypeExpected');
%=============================================================================
