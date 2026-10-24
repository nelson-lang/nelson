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
x = 0:pi / 100:2 * pi;
y = sin(x);
L = plot(x, y);
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
asserts.istrue(isempty(L.Children));
asserts.isapprox(L.Color, [0.0660 0.4430 0.7450], 1e-4);
asserts.isequal(L.DisplayName, '');
asserts.isequal(L.LineStyle, '-');
asserts.isapprox(L.LineWidth, 0.5000, 1e-4);
asserts.isequal(L.Marker, 'none')
asserts.isequal(L.MarkerEdgeColor, 'auto');
asserts.isequal(L.MarkerFaceColor, 'none');
asserts.isequal(L.MarkerSize, 6);
asserts.isequal(L.Parent, gca());
asserts.isequal(L.Tag, '');
asserts.isequal(L.Type, 'line');
asserts.isequal(L.UserData, []);
asserts.isequal(char(L.Visible), 'on');
asserts.isapprox(L.XData, x, 1e-4);
asserts.isequal(L.XDataMode, 'manual');
asserts.isapprox(L.YData, y, 1e-4);
asserts.isequal(L.ZData, zeros(1, 0));
close all;
%=============================================================================
% abbreviated property names are accepted after a LineSpec and do not create
% extra data series
f = figure('Visible', 'off');
xv = (1:5)';
L = plot(xv, xv, 'ro', 'linew', 3, 'markers', 8);
asserts.isequal(numel(L), 1);
asserts.isapprox(L.LineWidth, 3, 1e-12);
asserts.isapprox(L.MarkerSize, 8, 1e-12);
% a LineSpec that also abbreviates a property stays positional
L2 = plot(xv, xv, 'm', 'linew', 2);
asserts.isequal(L2.Color, [1 0 1]);
asserts.isapprox(L2.LineWidth, 2, 1e-12);
% multiple x-y pairs still create multiple lines
L3 = plot(xv, xv, xv, 2 * xv);
asserts.isequal(numel(L3), 2);
close(f);
%=============================================================================
