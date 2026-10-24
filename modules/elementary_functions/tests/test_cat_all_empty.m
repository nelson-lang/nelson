%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% cat along dim > 2 stacks 0x0 inputs when all of them are 0x0
assert_isequal(size(cat(3, [], [])), [0 0 2]);
assert_isequal(size(cat(3, zeros(0, 0), zeros(0, 0))), [0 0 2]);
assert_isequal(size(cat(4, [], [])), [0 0 1 2]);
assert_isequal(size(cat(3, [], [], [])), [0 0 3]);
%=============================================================================
% the common class is kept
R = cat(3, int8([]), []);
assert_isequal(class(R), 'int8');
assert_isequal(size(R), [0 0 2]);
R = cat(3, '', '');
assert_isequal(class(R), 'char');
assert_isequal(size(R), [0 0 2]);
R = cat(3, {}, {});
assert_isequal(class(R), 'cell');
assert_isequal(size(R), [0 0 2]);
R = cat(3, struct([]), struct([]));
assert_isequal(class(R), 'struct');
assert_isequal(size(R), [0 0 2]);
R = cat(3, true(0, 0), false(0, 0));
assert_isequal(class(R), 'logical');
assert_isequal(size(R), [0 0 2]);
R = cat(3, string.empty, string.empty);
assert_isequal(class(R), 'string');
assert_isequal(size(R), [0 0 2]);
%=============================================================================
% a 0x0 next to a non-empty input is still skipped
assert_isequal(size(cat(3, [], 1)), [1 1]);
assert_isequal(cat(3, [], [1 2], []), [1 2]);
assert_isequal(size(cat(3, zeros(0, 0))), [0 0]);
%=============================================================================
% dims 1 and 2 are unchanged
assert_isequal(size(cat(1, [], [])), [0 0]);
assert_isequal(size(cat(2, [], [])), [0 0]);
assert_isequal(size([[] []]), [0 0]);
assert_isequal(size([[]; []]), [0 0]);
assert_isequal(size(cat(3, zeros(1, 0), zeros(1, 0))), [1 0 2]);
%=============================================================================
