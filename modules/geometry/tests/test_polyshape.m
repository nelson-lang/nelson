%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
p = polyshape([0 1 1 0], [0 0 1 1]);
assert_isequal(class(p), 'polyshape');
assert_isequal(p.Vertices, [0 0; 1 0; 1 1; 0 1]);
assert_isapprox(area(p), 1, 1e-12);
assert_isequal(numsides(p), 4);
%=============================================================================
% N-by-2 constructor
r = polyshape([0 0; 2 0; 1 2]);
assert_isapprox(area(r), 2, 1e-12);
%=============================================================================
% perimeter of the unit square
assert_isapprox(perimeter(p), 4, 1e-12);
%=============================================================================
% plot returns a graphics object
figure();
h = plot(p, 'FaceColor', 'k', 'FaceAlpha', 1);
assert_istrue(isgraphics(h));
%=============================================================================
% nsidedpoly builds a regular polygon
q = nsidedpoly(100, 'Center', [5 6], 'Radius', 1.2);
assert_isequal(class(q), 'polyshape');
assert_isequal(numsides(q), 100);
assert_isapprox(area(q), pi * 1.2 ^ 2, 0.01);
%=============================================================================
% nsidedpoly side length
t = nsidedpoly(4, 'SideLength', 2);
assert_isapprox(area(t), 4, 1e-9);
%=============================================================================
% rotate turns the polygon counterclockwise about the origin by default
sq = polyshape([0 0; 1 0; 1 1; 0 1]);
r = rotate(sq, 90);
assert_isequal(class(r), 'polyshape');
assert_isapprox(r.Vertices(2, :), [0 1], 1e-12);
assert_isapprox(r.Vertices(3, :), [-1 1], 1e-12);
% area is preserved by rotation
assert_isapprox(area(r), area(sq), 1e-12);
%=============================================================================
% rotate about an explicit reference point leaves that point fixed
r2 = rotate(sq, 180, [0.5 0.5]);
assert_isapprox(sort(r2.Vertices), sort(sq.Vertices), 1e-12);
%=============================================================================
% translate shifts every vertex, both calling forms
t1 = translate(sq, [2 3]);
assert_isapprox(t1.Vertices, sq.Vertices + [2 3], 1e-12);
t2 = translate(sq, 2, 3);
assert_isapprox(t2.Vertices, t1.Vertices, 1e-12);
%=============================================================================
% errors
assert_checkerror('polyshape([1 2 3], [1 2])', _('X and Y must be numeric vectors of the same length.'));
assert_checkerror('nsidedpoly(2)', _('N must be an integer greater than or equal to 3.'));
assert_checkerror('rotate(polyshape([0 0;1 0;0 1]), [1 2])', _('Rotation angle must be a scalar.'));
%=============================================================================
