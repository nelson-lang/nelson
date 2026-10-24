%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% xor expands its operands implicitly
R = xor([1 0], [1; 0]);
asserts.isequal(R, logical([0 1; 1 0]));
asserts.isequal(class(R), 'logical');
asserts.isequal(xor([1 2 3], [1; 2]), false(2, 3));
asserts.isequal(xor(true(2, 1), [true false]), logical([0 1; 0 1]));
assert_checkerror('xor([1 0], [1 0 0])', _('Size mismatch on arguments.'));
%=============================================================================
% true / false: empty size, logical size, class name, like with no size
R = true([]);
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'logical');
R = false([]);
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'logical');
asserts.isequal(true(2, 'logical'), true(2));
asserts.isequal(false(2, 'logical'), false(2));
asserts.isequal(true('like', true), true);
asserts.isequal(size(true('like', true)), [1 1]);
asserts.isequal(false('like', true), false);
asserts.isequal(true(true), true);
asserts.isequal(true(false), true(0, 0));
%=============================================================================
