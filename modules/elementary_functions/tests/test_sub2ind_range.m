%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% out of range subscript must error
asserts.checkerror('sub2ind([2 3], 3, 1)', _('Out of range subscript.'));
asserts.checkerror('sub2ind([2 3], 1, 4)', _('Out of range subscript.'));
% valid cases unchanged
asserts.isequal(sub2ind([2 3], 2, 3), 6);
asserts.isequal(sub2ind([3 4], [1 2 3], [2 3 4]), [4 8 12]);
asserts.isequal(sub2ind([2 3 4], 2, 3, 4), 24);
% fewer subscripts than dimensions: last subscript spans remaining dims
asserts.isequal(sub2ind([2 3 4], 2, 10), 20);
asserts.checkerror('sub2ind([2 3 4], 2, 13)', _('Out of range subscript.'));
%=============================================================================
