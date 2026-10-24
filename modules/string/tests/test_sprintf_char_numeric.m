%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A character argument passed to a numeric conversion is consumed as its
% character code, recycling for character vectors. All expected values are
% the expected behavior.
%=============================================================================
% scalar character with each numeric conversion
assert_isequal(sprintf('%d', 'A'), '65');
assert_isequal(sprintf('%i', 'A'), '65');
assert_isequal(sprintf('%u', 'A'), '65');
assert_isequal(sprintf('%f', 'A'), '65.000000');
assert_isequal(sprintf('%e', 'A'), '6.500000e+01');
assert_isequal(sprintf('%g', 'A'), '65');
assert_isequal(sprintf('%x', 'A'), '41');
assert_isequal(sprintf('%o', 'A'), '101');
%=============================================================================
% character vector recycles the format
assert_isequal(sprintf('%d ', 'AB'), '65 66 ');
%=============================================================================
% mixing a numeric-consumed character with %s
assert_isequal(sprintf('%d-%s', 'A', 'B'), '65-B');
%=============================================================================
% a character used as a dynamic '*' width takes its character code
assert_isequal(sprintf('%*d', '5', 42), sprintf('%*d', 53, 42));
%=============================================================================
% %c with a scalar character stays the character
assert_isequal(sprintf('%c', 'A'), 'A');
%=============================================================================
