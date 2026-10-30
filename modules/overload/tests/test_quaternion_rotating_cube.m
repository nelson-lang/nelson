%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
quatCubeFigureVisible = 'off';
quatCubeAnimationEnabled = true;
quatCubeAnimationFrames = 15;
quatCubeFramePause = 0;
quatCubeAnimationContinuous = false;
run([modulepath('overload', 'root'), ...
  '/examples/quaternion/rotating_cube.m']);

assert_istrue(isgraphics(quatCubeFigure));
assert_istrue(isgraphics(quatCubePatch));
% The orientation is a quaternion built through the overloaded * operator.
assert_isequal(class(quatCubeOrientation), 'quaternion');
% It stays a unit quaternion, so rotations never scale the cube.
normValue = sqrt(quatCubeOrientation.w ^ 2 + quatCubeOrientation.x ^ 2 + ...
  quatCubeOrientation.y ^ 2 + quatCubeOrientation.z ^ 2);
assert_isapprox(normValue, 1, 1e-9);
% The rotated cube keeps its eight vertices and unit edge distances.
vertices = get(quatCubePatch, 'Vertices');
assert_isequal(size(vertices), [8 3]);
assert_isapprox(norm(vertices(1, :)), sqrt(3), 1e-9);
delete(quatCubeFigure);
drawnow();
%=============================================================================
