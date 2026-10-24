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
f = figure('Visible', 'off');
theta = linspace(0, pi, 4);
radius = [1 2 3 4];
h = polarscatter(theta, radius, 49, 'r', 'filled');
ax = h.Parent;
asserts.istrue(isgraphics(h, 'scatter'));
asserts.isequal(h.Type, 'scatter');
asserts.isequal(ax.Type, 'polaraxes');
asserts.isequal(h.ThetaData(:), theta(:));
asserts.isequal(h.RData(:), radius(:));
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.SizeData, 49);
asserts.isequal(h.CData, [1 0 0]);
asserts.isequal(h.MarkerFaceColor, 'flat');
asserts.isequal(h.MarkerEdgeColor, 'none');
asserts.isequal(h.UserData.ChartType, 'polarscatter');
asserts.istrue(isgraphics(h.UserData.Renderer, 'line'));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes();
h = polarscatter(ax, theta, radius, 'Marker', 'x', 'LineWidth', 2);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.Marker, 'x');
asserts.isequal(h.LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
asserts.checkerror('polarscatter(ax, theta, radius);', getString(message('Nelson:graphics:invalidParentInput')));
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table([0; pi / 4; pi / 2], [1; 2; 3], [10; 20; 30], ...
  'VariableNames', {'theta', 'rho', 'sz'});
h = polarscatter(t, 'theta', 'rho', 'filled', 'DisplayName', 'from table');
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.Type, 'scatter');
asserts.isequal(h.ThetaData, [0 pi / 4 pi / 2]);
asserts.isequal(h.RData, [1 2 3]);
asserts.isequal(h.ThetaVariable, 'theta');
asserts.isequal(h.RVariable, 'rho');
asserts.isequal(h.DisplayName, 'from table');
asserts.isequal(h.MarkerFaceColor, 'flat');
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table([0; pi / 4; pi / 2], [1; 2; 3], [10; 20; 30], ...
  'VariableNames', {'theta', 'rho', 'sz'});
h = polarscatter(t, 'theta', {'rho', 'sz'});
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).RData, [1 2 3]);
asserts.isequal(h(2).RData, [10 20 30]);
asserts.isequal(h(1).RVariable, 'rho');
asserts.isequal(h(2).RVariable, 'sz');
asserts.isequal(h(1).ThetaVariable, 'theta');
asserts.isequal(h(2).ThetaVariable, 'theta');
close(f);
%=============================================================================
asserts.checkerror('polarscatter(1);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('polarscatter([1 2], [1 2 3]);', getString(message('Nelson:dimagree:valueValueNumberElements', 'Theta', 'radius')));
asserts.checkerror('polarscatter([1 2], [1 2], -1);', getString(message('Nelson:validators:mustBePositive', 'Size data')));
%=============================================================================
