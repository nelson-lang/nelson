%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
figure();
x = linspace(-2 * pi, 2 * pi);
y = linspace(0, 4 * pi);
[X, Y] = meshgrid(x, y);
Z = sin(X) + cos(Y);
contour(X, Y, Z);
%=============================================================================
[M, h] = contour(X, Y, Z);
R = properties(h);
REF = {'ContourMatrix';
'Children';
'DisplayName';
'Floating';
'LevelList';
'LevelListMode';
'LevelStepMode';
'LevelStep';
'EdgeColor';
'EdgeAlpha';
'LineStyle';
'LineWidth';
'Parent';
'Tag';
'Type';
'UserData';
'Visible';
'XData';
'XDataMode';
'YData';
'YDataMode';
'ZData'};
assert_isequal(R, REF);
assert_isequal(M, h.ContourMatrix);
assert_isequal(h.Floating, 'off');
%=============================================================================
figure();
[X, Y, Z] = peaks;
contour(X, Y, Z, 20)
%=============================================================================
figure();
[X, Y, Z] = peaks;
v = [1,1];
contour(X, Y, Z, v)
%=============================================================================
figure();
[X, Y, Z] = peaks;
contour(X, Y, Z, '-.')
%=============================================================================
figure();
Z = peaks;
[M, c] = contour(Z);
c.LineWidth = 3;
%=============================================================================
figure();
Z = peaks;
Z(:,26) = NaN;
contour(Z)
%=============================================================================
figure();
[theta, r] = meshgrid (linspace (0,2*pi,64), linspace (0,1,64));
[X, Y] = pol2cart (theta, r);
Z = sin (2*theta) .* (1-r);
contour (X, Y, abs (Z), 10);
%=============================================================================
figure();
[x, y, z] = peaks ();
[c, h] = contour (x, y, z);
assert_isequal(h.LevelStepMode, 'auto');
h.LevelStep = 3;
assert_isequal(h.LevelStepMode, 'manual');
%=============================================================================
