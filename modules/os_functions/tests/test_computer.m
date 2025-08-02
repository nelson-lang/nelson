%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('computer'), 0);
assert_isequal(nargout('computer'), 3);
%=============================================================================
computer()
%=============================================================================
str = computer();
POSSIBLE = {'PCWIN', 'PCWIN64', 'GLNXA64', 'GLNXA32', 'MACI32', 'MACI64'};
assert_istrue(contains(str, POSSIBLE));
%=============================================================================
str = computer('arch');
POSSIBLE_STR = {'win64', 'win32', 'glnxa64', 'glnxa32', 'maci64', 'maci32'};
assert_istrue(contains(str, POSSIBLE_STR));
%=============================================================================
[str, maxsize] = computer()
POSSIBLE_STR = {'PCWIN', 'PCWIN64', 'GLNXA64', 'GLNXA32', 'MACI32', 'MACI64'};
assert_istrue(contains(str, POSSIBLE_STR));
assert_istrue(isdouble(maxsize));
%=============================================================================
[str, maxsize, endian] = computer()
POSSIBLE_STR = {'PCWIN', 'PCWIN64', 'GLNXA64', 'GLNXA32', 'MACI32', 'MACI64'};
assert_istrue(contains(str, POSSIBLE_STR));
assert_istrue(isdouble(maxsize));
POSSIBLE_E = {'L', 'B'};
assert_istrue(contains(endian, POSSIBLE_E));
%=============================================================================
