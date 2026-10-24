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
theta = [0 pi / 4 pi / 2];
radius = [1 2 3];
h = polarbubblechart(theta, radius, [25 36 49], 'b');
asserts.istrue(isgraphics(h, 'bubblechart'));
asserts.isequal(h.Type, 'bubblechart');
asserts.isequal(h.Parent.Type, 'polaraxes');
asserts.isequal(numel(f.Children), 1);
asserts.isequal(f.Children.Type, 'polaraxes');
asserts.isequal(char(f.Children.Visible), 'off');
asserts.isequal(numel(f.Children.XTick), 0);
asserts.isequal(numel(f.Children.YTick), 0);
asserts.isequal(h.ThetaData, theta);
asserts.isequal(h.RData, radius);
asserts.isequal(h.SizeData, [25 36 49]);
asserts.isequal(h.MarkerFaceColor, 'flat');
asserts.isequal(h.UserData.ChartType, 'polarbubblechart');
asserts.isequal(h.UserData.BubbleSizeData, [25 36 49]);
asserts.istrue(isgraphics(h.UserData.Renderer, 'bubblechart'));
asserts.isequal(h.UserData.Renderer, h);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes();
h = polarbubblechart(ax, theta, radius, 64, 'MarkerEdgeColor', 'k');
asserts.isequal(size(h), [1 3]);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).MarkerEdgeColor, [0 0 0]);
asserts.isequal(h(1).SizeData, 64);
asserts.isequal(h(2).ThetaData, pi / 4);
asserts.isequal(h(3).RData, 3);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
asserts.checkerror('polarbubblechart(ax, [1 2], [1 2], [1 2]);', getString(message('Nelson:graphics:invalidParentInput')));
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table([0; pi / 4; pi / 2], [1; 2; 3], [25; 36; 49], [1; 2; 3], ...
  'VariableNames', {'theta', 'rho', 'sz', 'c'});
h = polarbubblechart(t, 'theta', 'rho', 'sz', 'c', 'DisplayName', 'from table');
asserts.isequal(size(h), [1 1]);
asserts.isequal(h.Type, 'bubblechart');
asserts.isequal(h.ThetaData, [0 pi / 4 pi / 2]);
asserts.isequal(h.RData, [1 2 3]);
asserts.isequal(h.SizeData, [25 36 49]);
asserts.isequal(h.CData(:).', [1 2 3]);
asserts.isequal(h.ThetaVariable, 'theta');
asserts.isequal(h.RVariable, 'rho');
asserts.isequal(h.SizeVariable, 'sz');
asserts.isequal(h.ColorVariable, 'c');
asserts.isequal(h.DisplayName, 'from table');
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = table([0; pi / 4; pi / 2], [1; 2; 3], [2; 3; 4], [25; 36; 49], ...
  'VariableNames', {'theta', 'rho1', 'rho2', 'sz'});
h = polarbubblechart(t, 'theta', {'rho1', 'rho2'}, 'sz');
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).RData, [1 2 3]);
asserts.isequal(h(2).RData, [2 3 4]);
asserts.isequal(h(1).RVariable, 'rho1');
asserts.isequal(h(2).RVariable, 'rho2');
asserts.isequal(h(1).ThetaVariable, 'theta');
asserts.isequal(h(2).ThetaVariable, 'theta');
close(f);
%=============================================================================
asserts.checkerror('polarbubblechart(1, 2);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('polarbubblechart({1}, 2, 3);', getString(message('Nelson:polarbubblechart:numericThetaRadiusExpected')));
asserts.checkerror('polarbubblechart([1 2], [1 2], [1 2], ''BubbleColor'', ''b'');', _('Unrecognized property BubbleColor for class BubbleChart.'));
asserts.checkerror('polarbubblechart([1 2], [1 2], [1 2], ''Bad'', 1);', _('Unrecognized property Bad for class BubbleChart.'));
%=============================================================================
