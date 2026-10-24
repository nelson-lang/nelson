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
[x, y, z] = meshgrid(-1:1, -1:1, -1:1);
u = -y;
v = x;
w = ones(size(x));
f = figure('Visible', 'off');
h = coneplot(x, y, z, u, v, w);
asserts.istrue(isgraphics(h, 'patch'));
asserts.isequal(h.UserData.ChartType, 'coneplot');
asserts.isequal(numel(h.Children), 0);
close(f);
%=============================================================================
f = figure('Visible', 'off');
[x3d, y3d, z3d] = meshgrid(-2:0.5:2, -2:0.5:2, -2:0.5:2);
u3d = -y3d;
v3d = x3d;
w3d = z3d * 0.2;
[cx3d, cy3d, cz3d] = meshgrid(-1.5:1:1.5, -1.5:1:1.5, -1.5:1:1.5);
h = coneplot(x3d, y3d, z3d, u3d, v3d, w3d, cx3d, cy3d, cz3d, 0.4);
asserts.istrue(isgraphics(h, 'patch'));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = coneplot(ax, x, y, z, u, v, w, 0, 0, 0, 'LineWidth', 2);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.LineWidth, 2);
close(f);
%=============================================================================
asserts.checkerror('coneplot(1, 2, 3, 4, 5, {});', getString(message('Nelson:validators:mustBeNumeric', 'Value')));
asserts.checkerror('coneplot(1, 2, 3, 4, 5, 6, 7);', getString(message('Nelson:wrongNumberOfInputs')));
%=============================================================================
