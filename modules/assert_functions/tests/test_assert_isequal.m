%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('assert_isequal'), -2);
assert_isequal(nargout('assert_isequal'), 2);
assert_isequal(nargin('asserts.isequal'), -2);
assert_isequal(nargout('asserts.isequal'), 2);
%=============================================================================
assert_isequal('A', 'A')
%=============================================================================
A = str2func('cos');
B = str2func('cos');
assert_isequal(A, B)
%=============================================================================
assert_isequal(1, 1);
assert_isequal(ones(3, 3), ones(3, 3));
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
[f, msg] = assert_isequal(1, 2, 'custom failure');
assert_isfalse(f);
assert_istrue(contains(msg, 'custom failure'));
assert_istrue(contains(msg, msg_expected));
%=============================================================================
[f, msg] = asserts.isequal(1, 2, 'custom failure');
assert_isfalse(f);
assert_istrue(contains(msg, 'custom failure'));
%=============================================================================
[f, msg] = assert_isequal([1 2], [1 3]);
assert_isfalse(f);
assert_istrue(contains(msg, 'Expected: class'));
assert_istrue(contains(msg, 'Computed: class'));
assert_istrue(contains(msg, 'size'));
assert_istrue(contains(msg, 'First different index: (1, 2)'));
%=============================================================================
[f, msg] = assert_isequal({1}, {2});
assert_isfalse(f);
assert_istrue(contains(msg, 'Expected: class'));
assert_istrue(contains(msg, 'Computed: class'));
assert_istrue(contains(msg, 'size'));
assert_istrue(contains(msg, 'First different cell index: (1, 1)'));
%=============================================================================
computed.a = 1;
computed.b = 2;
expected.a = 1;
expected.b = 3;
[f, msg] = assert_isequal(computed, expected);
assert_isfalse(f);
assert_istrue(contains(msg, 'Expected: class'));
assert_istrue(contains(msg, 'Computed: class'));
assert_istrue(contains(msg, 'First different struct field: ''b'' at index (1, 1)'));
%=============================================================================
[f, msg] = assert_isequal(sparse([1 0]), sparse([0 1]));
assert_isfalse(f);
assert_istrue(contains(msg, 'Expected: class'));
assert_istrue(contains(msg, 'Computed: class'));
assert_istrue(contains(msg, 'size'));
%=============================================================================
