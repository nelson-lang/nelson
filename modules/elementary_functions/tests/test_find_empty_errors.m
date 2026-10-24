%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
E = sparse(zeros(0, 3));
[findRows, findColumns, findValues] = find(E);
asserts.isequal(size(find(E)), [0 1]);
asserts.isequal(size(findRows), [0 1]);
asserts.isequal(size(findColumns), [0 1]);
asserts.isequal(size(findValues), [0 1]);
%=============================================================================
asserts.checkerror('find([0 1], 0)', sprintf(_('%s must be a positive integer scalar.'), 'Second argument'));
asserts.checkerror('find([0 1], -1)', sprintf(_('%s must be a positive integer scalar.'), 'Second argument'));
asserts.checkerror('find([0 1], 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'Second argument'));
asserts.checkerror('find([0 1], NaN)', sprintf(_('%s must be a positive integer scalar.'), 'Second argument'));
asserts.checkerror('find([0 1], [1 2])', sprintf(_('%s must be a positive integer scalar.'), 'Second argument'));
asserts.checkerror('find([0 1], 1, ''bad'')', _('Invalid search option. Must be ''first'' or ''last''.'));
%=============================================================================
% empty-result shape follows the reference: a scalar or 0x0 input gives 0x0
assert_isequal(size(find([])), [0 0]);
assert_isequal(size(find(false)), [0 0]);
assert_isequal(size(find(0)), [0 0]);
%=============================================================================
% other empty shapes keep their row/column orientation
assert_isequal(size(find(zeros(1, 0))), [1 0]);
assert_isequal(size(find(zeros(0, 1))), [0 1]);
assert_isequal(size(find([0 0 0])), [1 0]);
assert_isequal(size(find([0; 0; 0])), [0 1]);
assert_isequal(size(find(zeros(0, 3))), [0 1]);
%=============================================================================
