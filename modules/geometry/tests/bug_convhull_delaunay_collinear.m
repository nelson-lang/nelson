%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% convhull/delaunay on collinear points must raise a clean error (was a crash
% for convhull and a wrong degenerate result for delaunay)
%=============================================================================
asserts.checkerror('convhull([1 2 3], [1 2 3])', _('Error computing the convex hull. The points may be collinear.'));
asserts.checkerror('convhulln([1 1; 2 2; 3 3])', _('Error computing the convex hull. The points may be collinear.'));
asserts.checkerror('delaunay([1 2 3]'', [1 2 3]'')', _('Error computing the Delaunay triangulation. The points may be collinear.'));
asserts.checkerror('delaunayn([1 1; 2 2; 3 3; 4 4])', _('Error computing the Delaunay triangulation. The points may be collinear.'));
%=============================================================================
% not enough points is also degenerate
asserts.checkerror('convhull([1 2], [1 2])', _('Error computing the convex hull. The points may be collinear.'));
%=============================================================================
% valid inputs still work
k = convhull([0 1 0 1]', [0 0 1 1]');
asserts.istrue(~isempty(k));
%=============================================================================
% non-degenerate simplex shortcut of delaunay still works
tri = delaunay([0 1 0]', [0 0 1]');
asserts.isequal(sort(tri(:))', [1 2 3]);
%=============================================================================
tri = delaunay([0 1 0 1]', [0 0 1 1]');
asserts.isequal(size(tri, 2), 3);
asserts.isequal(size(tri, 1), 2);
%=============================================================================
