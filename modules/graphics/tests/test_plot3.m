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
f = figure();
t = 0:pi / 50:10 * pi;
L = plot3(sin(t), cos(t), t);
axis square
grid on
asserts.istrue(isgraphics(L, 'line'));
P = properties(L);
REF = {'BeingDeleted';
  'BusyAction';
  'Children';
  'Color';
  'CreateFcn';
  'DeleteFcn';
  'DisplayName';
  'Interruptible';
  'LineStyle';
  'LineWidth';
  'Marker';
  'MarkerEdgeColor';
  'MarkerFaceColor';
  'MarkerSize';
  'Parent';
  'Tag';
  'Type';
  'UserData';
  'Visible';
  'XData';
  'XDataMode';
  'YData';
  'ZData'};
asserts.istrue(all(ismember(REF, P)));
%=============================================================================
% abbreviated property names are accepted after a LineSpec
g = figure('Visible', 'off');
x = (1:5)';
h = plot3(x, x, x, 'ro', 'markers', 10, 'linew', 3);
asserts.isapprox(h.MarkerSize, 10, 1e-12);
asserts.isapprox(h.LineWidth, 3, 1e-12);
% a LineSpec that also abbreviates a property stays positional
h2 = plot3(x, x, x, 'm', 'linew', 2);
asserts.isequal(h2.Color, [1 0 1]);
asserts.isapprox(h2.LineWidth, 2, 1e-12);
close(g);
%=============================================================================
