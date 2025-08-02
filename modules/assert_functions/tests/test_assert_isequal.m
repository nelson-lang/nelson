%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('assert_isequal'), 2);
assert_isequal(nargout('assert_isequal'), 2);
%=============================================================================
assert_isequal('A', 'A')
%=============================================================================
A = str2func('cos');
B = str2func('cos');
assert_isequal(A, B)
%=============================================================================
assert_isequal(1, 1);
assert_isequal(ones(3,3), ones(3,3));
%=============================================================================
f = assert_isequal(1, 1);
assert_istrue(f);
%=============================================================================
f = assert_isequal(1, 2);
assert_isfalse(f);
%=============================================================================
[f, msg] = assert_isequal(1, 1);
assert_istrue(f);
assert_isequal(msg, '');
%=============================================================================
[f, msg] = assert_isequal(1, 2);
assert_isfalse(f);
msg_expected = sprintf(_('Assertion failed: expected (%g) and computed (%g) values are different.'), 2, 1);
assert_isequal(msg, msg_expected);
%=============================================================================
