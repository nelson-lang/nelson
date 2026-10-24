%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.hmac'), 4);
asserts.isequal(nargout('crypto.hmac'), 1);
%=============================================================================
% RFC 4231 test case 1: key = 20 bytes 0x0b, data = "Hi There"
key1 = uint8(repmat(11, 1, 20));
data1 = 'Hi There';
REF1_256 = 'b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7';
REF1_512 = ['87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cde', ...
  'daa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854'];
asserts.isequal(crypto.hmac('sha256', key1, data1), REF1_256);
asserts.isequal(crypto.hmac('sha512', key1, data1), REF1_512);
asserts.isequal(crypto.hmac('SHA-512', key1, uint8(data1)), REF1_512);
asserts.isequal(crypto.hmac("sha256", key1, string(data1)), REF1_256);
%=============================================================================
% RFC 4231 test case 2: key = "Jefe" (text key = UTF-8 bytes)
REF2_256 = '5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843';
REF2_512 = ['164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea250554', ...
  '9758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737'];
asserts.isequal(crypto.hmac('sha256', 'Jefe', 'what do ya want for nothing?'), REF2_256);
asserts.isequal(crypto.hmac('sha512', 'Jefe', 'what do ya want for nothing?'), REF2_512);
%=============================================================================
% RFC 4231 test case 6: key longer than the block size (131 bytes 0xaa)
key6 = uint8(repmat(170, 1, 131));
data6 = 'Test Using Larger Than Block-Size Key - Hash Key First';
REF6_256 = '60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54';
REF6_512 = ['80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f352', ...
  '6b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598'];
asserts.isequal(crypto.hmac('sha256', key6, data6), REF6_256);
asserts.isequal(crypto.hmac('sha512', key6, data6), REF6_512);
%=============================================================================
% message from a file, empty key and empty message accepted
filename = [tempname(), '.txt'];
filewrite(filename, data1);
asserts.isequal(crypto.hmac('sha256', key1, filename, '-file'), REF1_256);
asserts.isequal(crypto.hmac('sha512', key1, filename, '-file'), REF1_512);
rmfile(filename);
R = crypto.hmac('sha256', '', '');
asserts.isequal(length(R), 64);
asserts.isequal(R, crypto.hmac('sha256', uint8([]), uint8([])));
%=============================================================================
% argument errors
asserts.checkerror('crypto.hmac(''sha256'', key1)', _('Not enough input arguments.'));
asserts.checkerror('crypto.hmac(''md5'', key1, data1)', sprintf(_('Unsupported HMAC algorithm: %s (''sha256'' or ''sha512'' expected).'), 'md5'), 'Nelson:core:hmacAlgorithmExpected');
asserts.checkerror('crypto.hmac(''sha256'', key1, data1, ''-string'')', sprintf(_('#%d argument: ''-file'' expected.'), 4), 'Nelson:core:cryptoFileOptionExpected');
asserts.checkerror('crypto.hmac(''sha256'', 3, data1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 2, '''uint8'' or ''char'''), 'Nelson:types:wrongArgumentTypeExpected');
missing = [tempname(), '.txt'];
asserts.checkerror('crypto.hmac(''sha256'', key1, missing, ''-file'')', sprintf(_('File cannot be read: %s'), missing), 'Nelson:core:cryptoFileNotReadable');
%=============================================================================
