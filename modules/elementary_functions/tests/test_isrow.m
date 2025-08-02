%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isrow'), 1)
assert_isequal(nargout('isrow'), 1)
%=============================================================================
s.c = 1;
assert_istrue(isrow(s))
%=============================================================================
assert_istrue(isrow("n"))
assert_istrue(isrow('n'))
assert_istrue(isrow("nelson"))
assert_istrue(isrow('nelson'))
assert_isfalse(isrow(["test"; "ing"]))
assert_istrue(isrow(["test", "ing"]))
%=============================================================================
assert_istrue(isrow ([1, 2, 3]))
assert_isfalse(isrow ([1; 2; 3]))
assert_istrue(isrow (1))
assert_isfalse(isrow ([]))
assert_isfalse(isrow ([1, 2; 3, 4]))
%=============================================================================

