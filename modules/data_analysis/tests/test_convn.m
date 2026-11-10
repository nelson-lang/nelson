%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(convn([1 2 3], [1 1]), [1 3 5 3]);
assert_isequal(convn([1 2; 3 4], [1 0; 0 1]), [1 2 0; 3 5 2; 0 3 4]);
C = convn(reshape(1:8, 2, 2, 2), ones(2, 2, 2));
assert_isequal(size(C), [3 3 3]);
assert_isequal(C(:)', [1 3 2 4 10 6 3 7 4 6 14 8 16 36 20 10 22 12 5 11 6 12 26 14 7 15 8]);
C = convn(reshape(1:8, 2, 2, 2), ones(2, 1, 2), 'same');
assert_isequal(C(:)', [14 8 22 12 11 6 15 8]);
C = convn(reshape(1:27, 3, 3, 3), ones(2, 2, 2), 'valid');
assert_isequal(C(:)', [60 68 84 92 132 140 156 164]);
assert_isequal(convn(ones(2, 2, 2), ones(1, 1, 3), 'valid'), zeros(2, 2, 0));
assert_isequal(class(convn(single([1 2]), [1 1])), 'single');
assert_isequal(convn(int8([1 2]), int8([1 1])), [1 3 2]);
assert_isequal(convn([true false], [1 1]), [1 1 0]);
assert_isequal(convn([1 2 3], [1 2 3 4 5], 'same'), [10 16 22]);
assert_isequal(size(convn([1 2 3], [1 2 3 4 5], 'valid')), [1 0]);
assert_isequal(convn([1 1i], [1 2]), [1 2+1i 2i]);
assert_isequal(convn([1 Inf 2], [1 0 1]), [1 Inf NaN Inf 2]);
assert_checkerror('convn(1, 1, ''foo'')', _('Shape must be ''full'', ''same'' or ''valid''.'));
%=============================================================================
