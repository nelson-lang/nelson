%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
f = figure('Visible', 'off');
X = [1 10 100; 2 20 50; 3 30 0];
h = parallelplot(X, 'CoordinateTickLabels', {'A', 'B', 'C'}, 'LineWidth', 2);
asserts.istrue(isgraphics(h, 'parallelplot'));
asserts.isequal(h.Type, 'parallelplot');
asserts.isequal(h.Data, X);
asserts.isequal(h.CoordinateTickLabels, {'A', 'B', 'C'});
asserts.isequal(h.DataNormalization, 'range');
asserts.isequal(h.LineWidth, 2);
state = h.UserData;
asserts.isequal(state.ChartType, 'parallelplot');
asserts.isequal(state.CoordinateTickLabels, {'A', 'B', 'C'});
asserts.isequal(size(state.DataHandles), [3 1]);
asserts.isequal(state.DataLimits, [1 10 0; 3 30 100]);
asserts.isequal(state.DataHandles(1).LineWidth, 2);
asserts.isequal(state.NormalizedData(1, :), [0 0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = parallelplot(X);
asserts.isequal(h.CoordinateTickLabels, {'1', '2', '3'});
asserts.isequal(h.LineWidth, 1);
asserts.isequal(h.UserData.DataHandles(1).LineWidth, 1);
asserts.isequal(h.UserData.BaselineHandle.Color, [0.25 0.25 0.25]);
tickText = gobjects(0, 1);
tickLines = gobjects(0, 1);
for k = 1:numel(h.UserData.TickHandles)
  if isgraphics(h.UserData.TickHandles(k), 'text')
    tickText(end + 1, 1) = h.UserData.TickHandles(k);
  else
    tickLines(end + 1, 1) = h.UserData.TickHandles(k);
  end
end
asserts.istrue(~isempty(tickText));
asserts.istrue(~isempty(tickLines));
asserts.isapprox(tickLines(1).XData, [1 1.03], 1e-12);
asserts.isapprox(tickLines(end).XData, [2.97 3], 1e-12);
lastTickText = tickText(end);
asserts.isequal(lastTickText.HorizontalAlignment, 'left');
asserts.istrue(lastTickText.Position(1) > 3);
close(f);
%=============================================================================
f = figure('Visible', 'off');
T = table([1; 2; 3], [4; 5; 6], {'a'; 'b'; 'c'}, 'VariableNames', {'A', 'B', 'Name'});
h = parallelplot(T, 'GroupData', [1; 1; 2], 'LegendVisible', 'on');
state = h.UserData;
asserts.isequal(h.Type, 'parallelplot');
asserts.isequal(state.CoordinateTickLabels, {'A', 'B'});
asserts.isequal(size(state.Data), [3 2]);
asserts.isequal(state.GroupData, [1; 1; 2]);
asserts.isapprox(state.LegendHandles(1).XData, [2.28 2.5 2.5 2.28 2.28], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
T = table([1; 2; 3], [4; 5; 6], {'a'; 'a'; 'b'}, 'VariableNames', {'A', 'B', 'G'});
h = parallelplot(T, 'CoordinateVariables', {'A', 'B'}, 'GroupVariable', 'G');
asserts.isequal(h.CoordinateVariables, {'A', 'B'});
asserts.isequal(h.GroupVariable, 'G');
asserts.isequal(h.GroupData, {'a'; 'a'; 'b'});
asserts.isequal(h.CoordinateTickLabels, {'A', 'B'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
asserts.checkerror('ax = axes(); parallelplot(ax, X(:, 1:2));', getString(message('Nelson:parallelplot:parallelCoordinatesPlotCannotBeA')));
h = parallelplot(X(:, 1:2), 'DataNormalization', 'none', 'Color', 'r');
asserts.isequal(h.UserData.NormalizedData, X(:, 1:2));
asserts.isequal(h.DataNormalization, 'none');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.UserData.DataHandles(1).Color, [1 0 0]);
close(f);
%=============================================================================
asserts.checkerror('parallelplot();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('parallelplot({1, 2});', getString(message('Nelson:parallelplot:numericMatrixOrExpected')));
asserts.checkerror('parallelplot([1 2; 3 4], ''GroupData'', [1 2 3]);', getString(message('Nelson:parallelplot:invalidGroupData')));
asserts.checkerror('parallelplot([1 2; 3 4], ''DataNormalization'', ''zscore'');', getString(message('Nelson:parallelplot:invalidDataNormalization')));
asserts.checkerror('parallelplot([1 2; 3 4], ''CoordinateLabels'', {''A'', ''B''});', getString(message('Nelson:graphics:unsupportedProperty', 'parallelplot')));
asserts.checkerror('T = table([1; 2], [3; 4], ''VariableNames'', {''A'', ''B''}); parallelplot(T, {''A''});', getString(message('Nelson:parallelplot:specifyTheGroupVariableUsing')));
%=============================================================================
