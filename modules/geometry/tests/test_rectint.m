%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% two unit squares overlapping in a quarter-unit corner
assert_isapprox(rectint([0 0 1 1], [0.5 0.5 1 1]), 0.25, 1e-12);
%=============================================================================
% disjoint rectangles do not intersect
assert_isequal(rectint([0 0 1 1], [2 2 1 1]), 0);
%=============================================================================
% a rectangle fully intersects itself, giving its own area
assert_isapprox(rectint([0 0 2 3], [0 0 2 3]), 6, 1e-12);
%=============================================================================
% one rectangle contained in another gives the smaller area
assert_isapprox(rectint([0 0 4 4], [1 1 2 2]), 4, 1e-12);
%=============================================================================
% M-by-4 and N-by-4 inputs give an M-by-N matrix of intersection areas
A = [0 0 1 1; 0 0 2 2];
B = [0.5 0.5 1 1; 5 5 1 1];
R = rectint(A, B);
assert_isequal(size(R), [2 2]);
assert_isapprox(R, [0.25 0; 1 0], 1e-12);
%=============================================================================
% four-column inputs are required
assert_checkerror('rectint([0 0 1], [0 0 1])', _('A and B must be numeric matrices with four columns.'));
%=============================================================================
