%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('crypto.sha512'), 2);
asserts.isequal(nargout('crypto.sha512'), 1);
%=============================================================================
% FIPS 180-4 test vectors
REF_ABC = ['ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a', ...
  '2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f'];
REF_EMPTY = ['cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce', ...
  '47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e'];
R = crypto.sha512('abc');
asserts.isequal(R, REF_ABC);
asserts.isequal(class(R), 'char');
asserts.isequal(crypto.sha512(''), REF_EMPTY);
asserts.isequal(crypto.sha512(uint8([])), REF_EMPTY);
asserts.isequal(crypto.sha512(uint8('abc')), REF_ABC);
asserts.isequal(crypto.sha512(uint8('abc')'), REF_ABC);
asserts.isequal(crypto.sha512(string('abc')), string(REF_ABC));
asserts.isequal(crypto.sha512('abc', '-string'), REF_ABC);
%=============================================================================
% cell and string arrays keep their shape
R = crypto.sha512({'abc'; ''});
asserts.isequal(R, {REF_ABC; REF_EMPTY});
R = crypto.sha512(["abc", ""]);
asserts.isequal(R, [string(REF_ABC), string(REF_EMPTY)]);
%=============================================================================
% files: automatic detection, '-file' and '-string'
filename = [tempname(), '.txt'];
filewrite(filename, 'abc');
asserts.isequal(crypto.sha512(filename), REF_ABC);
asserts.isequal(crypto.sha512(filename, '-file'), REF_ABC);
asserts.isfalse(strcmp(crypto.sha512(filename, '-string'), REF_ABC));
asserts.isequal(crypto.sha512({filename}, '-file'), {REF_ABC});
rmfile(filename);
asserts.isequal(crypto.sha512(filename, '-file'), '');
%=============================================================================
% argument errors
asserts.checkerror('crypto.sha512()', _('Not enough input arguments.'));
asserts.checkerror('crypto.sha512(''abc'', ''-other'')', sprintf(_('#%d argument: ''-file'' or ''-string'' expected.'), 2), 'Nelson:core:cryptoInputOptionExpected');
asserts.checkerror('crypto.sha512(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''uint8'', ''char'', ''string'' or ''cell'''), 'Nelson:types:wrongArgumentTypeExpected');
asserts.checkerror('crypto.sha512(uint8(''abc''), ''-file'')', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''char'' or ''string'''), 'Nelson:types:wrongArgumentTypeExpected');
%=============================================================================
