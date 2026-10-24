%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Inside a matrix, a whitespace after a '.' (non-conjugate) transpose separates
% columns, exactly like it does after a ''' (conjugate) transpose.
a = [1 2 3];
b = [4 5 6];
%=============================================================================
R = [a.' b.'];
assert_isequal(R, [1 4; 2 5; 3 6]);
%=============================================================================
% Same result with the conjugate transpose spelling.
assert_isequal([a' b'], [a.' b.']);
%=============================================================================
% A transpose element followed by an arithmetic element.
assert_isequal([a.' 2 * b.'], [1 8; 2 10; 3 12]);
%=============================================================================
% Parenthesized operand transposed.
assert_isequal([(a).' b.'], [1 4; 2 5; 3 6]);
%=============================================================================
