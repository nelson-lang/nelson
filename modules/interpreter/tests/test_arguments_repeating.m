%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
addpath([modulepath('interpreter', 'tests'), '/arguments']);
%=============================================================================
% repeating arguments are grouped into cell arrays (expected behavior)
[xs, ys] = arguments_repeating_pairs(1, 'a', 2, 'b');
asserts.isequal(xs, {1, 2});
asserts.isequal(ys, {'a', 'b'});
%=============================================================================
% empty repeating list is allowed
[xs, ys] = arguments_repeating_pairs();
asserts.isequal(xs, cell(1, 0));
asserts.isequal(ys, cell(1, 0));
%=============================================================================
% incomplete groups are rejected
asserts.checkerror('[xs, ys] = arguments_repeating_pairs(1, ''a'', 2);', ...
  _('Invalid argument list. Repeating arguments, if specified, must be in groups of 2.'));
%=============================================================================
% per-element validation applies
xs = arguments_repeating_single(1, 2, 3);
asserts.isequal(xs, {1, 2, 3});
asserts.checkerror('arguments_repeating_single(1, -2);', _('Value must be positive.'));
%=============================================================================
% trailing name-value pairs coexist with repeating arguments
[xs, scale] = arguments_repeating_nv(1, 2, 'Scale', 10);
asserts.isequal(xs, {1, 2});
asserts.isequal(scale, 10);
[xs, scale] = arguments_repeating_nv(5);
asserts.isequal(xs, {5});
asserts.isequal(scale, 1);
%=============================================================================
% optional positional before a repeating block
[a, xs] = arguments_opt_before_repeating();
asserts.isequal(a, 42);
asserts.isequal(xs, cell(1, 0));
[a, xs] = arguments_opt_before_repeating(7, 1, 2);
asserts.isequal(a, 7);
asserts.isequal(xs, {1, 2});
%=============================================================================
% N-D fixed sizes accept exact match and scalar expansion
y = arguments_nd_size(ones(2, 3, 4));
asserts.isequal(size(y), [2 3 4]);
y = arguments_nd_size(1);
asserts.isequal(size(y), [2 3 4]);
asserts.istrue(all(y(:) == 1));
%=============================================================================
% (n, :) patterns validate the fixed dimension
y = arguments_rows_pattern(ones(2, 7));
asserts.isequal(size(y), [2 7]);
asserts.checkerror('arguments_rows_pattern(ones(3, 7));', ...
  _('Value must match declared argument size.'));
%=============================================================================
