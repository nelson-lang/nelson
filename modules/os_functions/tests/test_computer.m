%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('computer'), 0);
asserts.isequal(nargout('computer'), 3);
%=============================================================================
computer()
%=============================================================================
str = computer();
POSSIBLE = {'PCWOA64', 'PCWIN', 'PCWIN64', 'GLNXA64', 'GLNXA32', 'MACI32', 'MACI64', 'MACA64', 'WASM32'};
asserts.istrue(contains(str, POSSIBLE));
%=============================================================================
str = computer('arch');
POSSIBLE_STR = {'win64', 'woa64', 'win32', 'glnxa64', 'glnxa32', 'maci64', 'maci32', 'maca64', 'wasm32'};
asserts.istrue(contains(str, POSSIBLE_STR));
%=============================================================================
[str, maxsize] = computer();
POSSIBLE_STR = {'PCWOA64', 'PCWIN', 'PCWIN64', 'GLNXA64', 'GLNXA32', 'MACI32', 'MACI64', 'MACA64', 'WASM32'};
asserts.istrue(contains(str, POSSIBLE_STR));
asserts.istrue(isdouble(maxsize));
%=============================================================================
[str, maxsize, endian] = computer();
POSSIBLE_STR = {'PCWOA64', 'PCWIN', 'PCWIN64', 'GLNXA64', 'GLNXA32', 'MACI32', 'MACI64', 'MACA64', 'WASM32'};
asserts.istrue(contains(str, POSSIBLE_STR));
asserts.istrue(isdouble(maxsize));
POSSIBLE_E = {'L', 'B'};
asserts.istrue(contains(endian, POSSIBLE_E));
%=============================================================================
%=============================================================================
% the WebAssembly engine reports its own tag; native builds never do
if iswasm()
  asserts.isequal(computer('arch'), 'wasm32');
  asserts.isequal(computer(), 'WASM32');
else
  asserts.isfalse(strcmp(computer('arch'), 'wasm32'));
  asserts.isfalse(strcmp(computer(), 'WASM32'));
end
%=============================================================================
