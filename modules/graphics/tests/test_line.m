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
x = linspace(0, 10);
y = sin(x);
R = line(x, y);
asserts.isequal(R.Type, 'line');
%=============================================================================
P = properties(R);
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
close all;
%=============================================================================
% matrix X and Y data draw one line object per column
f = figure('Visible', 'off');
h = line([0 0 0; 1 1 1], [0 1 2; 3 4 5]);
asserts.isequal(numel(h), 3);
asserts.istrue(all(isgraphics(h, 'line')));
asserts.isequal(h(2).XData, [0 1]);
asserts.isequal(h(2).YData, [1 4]);
% a shared vector partner is reused for every column of the matrix
h2 = line([0 1], [0 1; 2 3; 4 5]');
asserts.isequal(numel(h2), 3);
asserts.isequal(h2(1).XData, [0 1]);
% a plain vector is still a single line
h3 = line([0 1 2], [0 1 2]);
asserts.isequal(numel(h3), 1);
close(f);
%=============================================================================
