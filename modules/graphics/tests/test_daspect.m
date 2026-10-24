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
% <--ADV-CLI MODE-->
%=============================================================================
close all
reset(groot());
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
%=============================================================================
figure();
daspect([1 2 1]);
d = daspect();
assert_vector_isclose(d, [1 2 1], 0);
%=============================================================================
figure();
daspect([1 1 1]);
d = daspect();
assert_vector_isclose(d, [1 1 1], 0);
%=============================================================================
figure();
daspect([2 1 1]);
d = daspect();
assert_vector_isclose(d, [2 1 1], 0);
%=============================================================================
figure();
daspect([1 1 2]);
d = daspect();
assert_vector_isclose(d, [1 1 2], 0);
%=============================================================================
figure();
daspect('auto');
d = daspect();
assert_vector_isclose(d, [1 1 1], 0);
m = daspect('mode');
asserts.isequal(m, 'auto');
%=============================================================================
figure();
[x, y] = meshgrid(-2:.2:2);
z = x .* exp(-x .^ 2 - y .^ 2);
surf(x, y, z)
d = daspect();
REF = [4 4 1];
assert_vector_isclose(d, REF, 1e-4);
ax = gca();
assert_vector_isclose(ax.XTick, -2:0.5:2, 1e-12);
assert_vector_isclose(ax.YTick, [-2 -1 0 1 2], 1e-12);
assert_vector_isclose(ax.ZTick, [-0.5 0 0.5], 1e-12);
%=============================================================================
figure();
sphere(40);
daspect([2 1 0.5]);
d = daspect();
assert_vector_isclose(d, [2 1 0.5], 0);
ax = gca();
assert_vector_isclose(ax.XTick, [-1 0 1], 1e-12);
assert_vector_isclose(ax.YTick, [-1 0 1], 1e-12);
assert_vector_isclose(ax.ZTick, [-1 -0.8 -0.6 -0.4 -0.2 0 0.2 0.4 0.6 0.8 1], 1e-12);
%=============================================================================
close all
%=============================================================================
function assert_vector_isclose(actual, expected, tolerance)
  asserts.isequal(numel(actual), numel(expected));
  actual = reshape(actual, 1, []);
  expected = reshape(expected, 1, []);
  asserts.istrue(all(abs(actual - expected) <= tolerance));
end
%=============================================================================
