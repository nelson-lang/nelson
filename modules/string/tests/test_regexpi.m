%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('regexpi'), -2);
asserts.isequal(nargout('regexpi'), -1);
%=============================================================================
m = regexpi('A character vector with UPPERCASE and lowercase text.', '\w*case', 'match');
asserts.isequal(m, {'UPPERCASE', 'lowercase'});
%=============================================================================
m = regexp('A character vector with UPPERCASE and lowercase text.', '\w*case', 'match', 'ignorecase');
asserts.isequal(m, {'UPPERCASE', 'lowercase'});
%=============================================================================
m = regexpi('ABC abc', 'abc', 'match', 'matchcase');
asserts.isequal(m, {'abc'});
%=============================================================================
idx = regexpi({'Abc', 'def'; 'ABC', 'xyz'}, 'abc');
asserts.isequal(idx, {1, []; 1, []});
%=============================================================================
% a scalar (1x1) cell input yields a 1x1 cell output (not an unwrapped result)
r = regexpi({'only1'}, '\d+', 'match');
asserts.isequal(iscell(r), true);
asserts.isequal(size(r), [1 1]);
asserts.isequal(r{1}, {'1'});
%=============================================================================
