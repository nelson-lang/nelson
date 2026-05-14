%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('regexpi'), -2);
assert_isequal(nargout('regexpi'), -1);
%=============================================================================
m = regexpi('A character vector with UPPERCASE and lowercase text.', '\w*case', 'match');
assert_isequal(m, {'UPPERCASE', 'lowercase'});
%=============================================================================
m = regexp('A character vector with UPPERCASE and lowercase text.', '\w*case', 'match', 'ignorecase');
assert_isequal(m, {'UPPERCASE', 'lowercase'});
%=============================================================================
m = regexpi('ABC abc', 'abc', 'match', 'matchcase');
assert_isequal(m, {'abc'});
%=============================================================================
idx = regexpi({'Abc', 'def'; 'ABC', 'xyz'}, 'abc');
assert_isequal(idx, {1, []; 1, []});
%=============================================================================
