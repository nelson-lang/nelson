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
h = pareto([5 20 10], {'A', 'B', 'C'});
asserts.isequal(numel(h), 2);
asserts.istrue(isgraphics(h(1), 'bar'));
asserts.isequal(h(1).Type, 'bar');
asserts.istrue(isgraphics(h(2), 'line'));
asserts.isequal(h(2).XData, [1 2 3]);
ax = ancestor(h(1), 'axes');
asserts.isequal(ax.XTickLabel, {'B'; 'C'; 'A'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = pareto(ax, [0 0 0]);
asserts.isequal(h(2).YData, zeros(1, 3));
close(f);
%=============================================================================
f = figure('Visible', 'off');
x = ["Chocolate" "Apple" "Pecan" "Cherry" "Pumpkin"];
y = [35 50 30 5 80];
h = pareto(y, x, 1);
ylabel('Votes');
ax = ancestor(h(1), 'axes');
asserts.isequal(h(2).YData, [80 130 165 195 200]);
asserts.isapprox(h(2).Color, [0.8660 0.3290 0], 1e-12);
asserts.isequal(h(2).Marker, 'none');
asserts.isequal(h(2).LineStyle, '-');
asserts.isequal(h(2).LineWidth, 0.5);
asserts.isequal(ax.XTick, 1:5);
asserts.isequal(ax.XTickLabel, {'Pumpkin'; 'Apple'; 'Chocolate'; 'Pecan'; 'Cherry'});
asserts.isequal(ax.YLim, [0 200]);
asserts.isequal(ax.YLabel.String, 'Votes');
axesList = findall(f, 'Type', 'axes');
rightAx = findRightAxes(axesList);
asserts.istrue(~isempty(rightAx));
asserts.isequal(rightAx.YLim, [0 200]);
asserts.isequal(rightAx.YTickLabel, {'0%'; '10%'; '20%'; '30%'; '40%'; '50%'; '60%'; '70%'; '80%'; '90%'; '100%'});
asserts.istrue(isempty(rightAx.Children));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = pareto(y, 0.95);
ax = ancestor(h(1), 'axes');
asserts.isequal(ax.XTick, 1:4);
asserts.isequal(ax.XTickLabel, {'5'; '2'; '1'; '3'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = pareto(y, [10 20 30 40 50], 0.95);
ax = ancestor(h(1), 'axes');
asserts.isequal(ax.XTick, 1:4);
asserts.isequal(ax.XTickLabel, {'50'; '20'; '10'; '30'});
close(f);
%=============================================================================
asserts.checkerror('pareto();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('pareto({1, 2});', getString(message('Nelson:validators:mustBeNumeric', 'Value')));
asserts.checkerror('pareto([-1 2]);', getString(message('Nelson:pareto:invalidDataValues')));
asserts.checkerror('pareto([1 2], {''A''});', getString(message('Nelson:pareto:invalidNumberOfLabels')));
asserts.checkerror('pareto([1 2], {''A'', ''B''}, 2);', getString(message('Nelson:pareto:invalidThreshold')));
%=============================================================================
function rightAx = findRightAxes(axesList)
  rightAx = [];
  for k = 1:numel(axesList)
    if strcmp(axesList(k).YAxisLocation, 'right')
      rightAx = axesList(k);
      return
    end
  end
end
%=============================================================================
