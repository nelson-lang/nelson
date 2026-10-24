%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.blake2b'), 4);
asserts.isequal(nargout('crypto.blake2b'), 1);
%=============================================================================
% RFC 7693 appendix A and expected behavior vectors
REF_ABC = ['ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d1', ...
  '7d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923'];
REF_EMPTY = ['786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419', ...
  'd25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce'];
R = crypto.blake2b('abc');
asserts.isequal(R, REF_ABC);
asserts.isequal(class(R), 'char');
asserts.isequal(crypto.blake2b(uint8('abc')), REF_ABC);
asserts.isequal(crypto.blake2b(''), REF_EMPTY);
asserts.isequal(crypto.blake2b(uint8([])), REF_EMPTY);
asserts.isequal(crypto.blake2b('abc', []), REF_ABC);
asserts.isequal(crypto.blake2b('abc', [], 64), REF_ABC);
%=============================================================================
% digest sizes
asserts.isequal(crypto.blake2b('abc', [], 20), '384264f676f39536840523f284921cdc68b6846b');
asserts.isequal(crypto.blake2b('abc', [], 1), '6b');
%=============================================================================
% keyed hashing (reference KAT: key = 0x00..0x3f, empty message)
key64 = uint8(0:63);
asserts.isequal(crypto.blake2b(uint8([]), key64), ['10ebb67700b1868efb4417987acf4690ae9d972fb7a590c2f02871799aaa4786', ...
  'b5e996e8f0f4eb981fc214b005f42d2ff4233499391653df7aefcbc13fc51568']);
asserts.isequal(crypto.blake2b('abc', 'Jefe'), ['e3273144bfa7f9fccd57a52f2b7e3b1a91207a669233c8e67ae06d8ddf45050e', ...
  '0e87162c463c402bbbe654a562b1abd5ba5477ed2501578099bd722b1911b5b3']);
asserts.isequal(crypto.blake2b('abc', 'Jefe', 20), 'c54f0157a473365614e6e6838971e4e20c4ceeaa');
asserts.isfalse(strcmp(crypto.blake2b('abc', 'Jefe'), crypto.blake2b('abc', 'jefe')));
%=============================================================================
% file input
filename = [tempname(), '.txt'];
filewrite(filename, 'abc');
asserts.isequal(crypto.blake2b(filename, '-file'), REF_ABC);
asserts.isequal(crypto.blake2b(filename, 'Jefe', 20, '-file'), 'c54f0157a473365614e6e6838971e4e20c4ceeaa');
rmfile(filename);
%=============================================================================
% argument errors
asserts.checkerror('crypto.blake2b()', _('Not enough input arguments.'));
asserts.checkerror('crypto.blake2b(''abc'', uint8(1:65))', _('BLAKE2b key must be at most 64 bytes.'), 'Nelson:core:blake2bKeySizeExpected');
asserts.checkerror('crypto.blake2b(''abc'', [], 65)', _('BLAKE2b digest size must be an integer between 1 and 64.'), 'Nelson:core:blake2bDigestSizeExpected');
asserts.checkerror('crypto.blake2b(''abc'', [], 2.5)', _('BLAKE2b digest size must be an integer between 1 and 64.'), 'Nelson:core:blake2bDigestSizeExpected');
asserts.checkerror('crypto.blake2b(''abc'', [], 32, ''-string'')', sprintf(_('#%d argument: ''-file'' expected.'), 4), 'Nelson:core:cryptoFileOptionExpected');
asserts.checkerror('crypto.blake2b(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''uint8'' or ''char'''), 'Nelson:types:wrongArgumentTypeExpected');
%=============================================================================
