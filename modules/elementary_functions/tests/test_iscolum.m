%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('iscolumn'), 1)
assert_isequal(nargout('iscolumn'), 1)
%=============================================================================
s.c = 1;
assert_istrue(iscolumn(s))
%=============================================================================
assert_istrue(iscolumn("n"))
assert_istrue(iscolumn('n'))
assert_istrue(iscolumn("nelson"))
assert_isfalse(iscolumn('nelson'))
assert_istrue(iscolumn(["test"; "ing"]))
assert_isfalse(iscolumn(["test", "ing"]))
%=============================================================================
assert_isfalse(iscolumn([1, 2, 3]))
assert_istrue(iscolumn([1; 2; 3]))
assert_istrue(iscolumn(1))
assert_isfalse(iscolumn([]))
assert_isfalse(iscolumn([1, 2; 3, 4]))
%=============================================================================

