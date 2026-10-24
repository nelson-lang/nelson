%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% an empty size argument gives an empty array
asserts.isequal(size(zeros([])), [0 0]);
asserts.isequal(size(zeros([], 3)), [0 3]);
asserts.isequal(size(zeros(3, [])), [3 0]);
asserts.isequal(size(zeros(zeros(1, 0))), [0 0]);
asserts.isequal(size(ones([])), [0 0]);
asserts.isequal(size(eye([])), [0 0]);
asserts.isequal(size(inf([])), [0 0]);
asserts.isequal(size(nan([])), [0 0]);
R = zeros([], 'like', int8(1));
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(R), 'int8');
%=============================================================================
% a logical scalar is a size
asserts.isequal(zeros(true), 0);
asserts.isequal(size(zeros(false)), [0 0]);
asserts.isequal(inf(true), Inf);
asserts.isequal(nan(true), NaN);
%=============================================================================
% class name as a string, 'like' in any case
R = zeros(2, 3, "int8");
asserts.isequal(class(R), 'int8');
asserts.isequal(size(R), [2 3]);
asserts.isequal(class(zeros(2, 3, 'Like', int8(1))), 'int8');
asserts.isequal(class(zeros(2, 3, 'LIKE', int8(1))), 'int8');
%=============================================================================
% diag: the diagonal number is an integer scalar
assert_checkerror('diag(1:3, 1.5)', _('Second argument must be an integer scalar.'));
assert_checkerror('diag(1:3, NaN)', _('Second argument must be an integer scalar.'));
assert_checkerror('diag(1:3, Inf)', _('Second argument must be an integer scalar.'));
asserts.isequal(diag(1:3, true), reshape([0 0 0 0 1 0 0 0 0 2 0 0 0 0 3 0], 4, 4));
asserts.isequal(size(diag(1:3, 'a')), [100 100]);
%=============================================================================
