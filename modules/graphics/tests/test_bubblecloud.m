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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NLS0004>
f = figure('Visible', 'off');
h = bubblecloud([10 20 30], {'a', 'b', 'c'}, {'g1', 'g1', 'g2'}, 'Title', 'Cloud');
asserts.istrue(isgraphics(h, 'bubblecloud'));
asserts.isequal(h.Type, 'bubblecloud');
asserts.isequal(h.Parent, f);
asserts.isequal(h.SizeData, [10 20 30]);
asserts.isequal(h.LabelData, {'a', 'b', 'c'});
asserts.isequal(h.GroupData, {'g1', 'g1', 'g2'});
asserts.isequal(h.Title, 'Cloud');
asserts.isequal(h.LegendTitle, '');
asserts.isequal(h.FaceColor, 'flat');
asserts.isequal(h.EdgeColor, [0.129411764705882, 0.129411764705882, 0.129411764705882]);
asserts.isapprox(h.Position, [0.13 0.11 0.775 0.815], 1e-15);
ax = h.Children;
patchHandles = findobj(ax, 'Type', 'patch');
asserts.isequal(numel(patchHandles), 3);
diameters = [];
for k = 1:numel(patchHandles)
  diameters = [diameters, max(patchHandles(k).XData) - min(patchHandles(k).XData)];
end
asserts.isapprox(sort(diameters), 2 * sqrt([10 20 30] / 30), 1e-12);
lineHandles = findobj(ax, 'Type', 'line');
markerSizes = [];
for k = 1:numel(lineHandles)
  markerSizes = [markerSizes, lineHandles(k).MarkerSize];
end
asserts.isequal(sort(markerSizes), [6 6]);
lgd = findobj(f, 'Type', 'legend');
% The legend sits outside the plot box, top right, and the axes gives it the
% room; the axes layout keeps it placed, so the figure SizeChangedFcn stays
% free (as a native chart leaves it).
asserts.istrue(isempty(f.SizeChangedFcn));
asserts.isequal(char(lgd.Location), 'northeastoutside');
assertBubbleCloudLegendOutside(ax, lgd, true);
legendPixelSize = lgd.Position(3:4) .* f.Position(3:4);
% A resize keeps it outside and in view at the same pixel size, with no
% SizeChangedFcn.
figPosition = f.Position;
f.Position = [figPosition(1:2), figPosition(3:4) * 1.25];
drawnow();
asserts.istrue(isempty(f.SizeChangedFcn));
assertBubbleCloudLegendOutside(ax, lgd, false);
asserts.isapprox(lgd.Position(3:4) .* f.Position(3:4), legendPixelSize, 1e-6);
f.Position = [figPosition(1:2), figPosition(3:4) * 0.6];
drawnow();
assertBubbleCloudLegendOutside(ax, lgd, false);
asserts.isapprox(lgd.Position(3:4) .* f.Position(3:4), legendPixelSize, 1e-6);
close(f);
%=============================================================================
f = figure('Visible', 'off');
tbl = table([5; 10; 20], {'A'; 'B'; 'C'}, {'G1'; 'G1'; 'G2'}, ...
  'VariableNames', {'Size', 'Label', 'Group'});
h = bubblecloud(tbl, 'Size', 'Label', 'Group');
asserts.istrue(isgraphics(h, 'bubblecloud'));
asserts.isequal(h.Type, 'bubblecloud');
asserts.isequal(h.SizeData, [5 10 20]);
asserts.isequal(h.LabelData, {'A', 'B', 'C'});
asserts.isequal(h.GroupData, {'G1', 'G1', 'G2'});
asserts.isequal(h.SizeVariable, 'Size');
asserts.isequal(h.LabelVariable, 'Label');
asserts.isequal(h.GroupVariable, 'Group');
asserts.isequal(h.LegendTitle, 'Group');
asserts.istrue(istable(h.SourceTable));
asserts.isapprox(f.Position(3:4), groot().DefaultFigurePosition(3:4), 1e-12);
asserts.isapprox(h.Position, [0.13 0.11 0.775 0.815], 1e-15);
ax = h.Children;
% the outside legend narrows the axes from the default box, keeping its left edge
asserts.isapprox(ax.Position([1 2 4]), [0.13 0.11 0.815], 1e-15);
asserts.istrue(ax.Position(3) < 0.775);
% the data box takes the aspect of the axes box, with a 0.12 margin around the bubbles
asserts.isapprox(ax.XLim, [-2.53421356237309 2.12], 1e-12);
boxPixels = ax.Position(3:4) .* f.Position(3:4);
asserts.isapprox(diff(ax.XLim) / diff(ax.YLim), boxPixels(1) / boxPixels(2), 1e-9);
asserts.isapprox(ax.DataAspectRatio, [1 1 1], 1e-12);
asserts.isequal(numel(ax.XTick), 0);
asserts.isequal(numel(ax.YTick), 0);
asserts.isequal(char(ax.Box), 'on');
asserts.isequal(ax.XColor, 'none');
asserts.isequal(ax.YColor, 'none');
lgd = findobj(f, 'Type', 'legend');
asserts.isequal(char(lgd.Location), 'northeastoutside');
assertBubbleCloudLegendOutside(ax, lgd, true);
asserts.isequal(lgd.String, {'G1', 'G2'});
asserts.isequal(lgd.Title.String, 'Group');
asserts.isapprox(bubbleCloudTextPosition(ax, 'A'), [-0.5 0], 1e-12);
asserts.isapprox(bubbleCloudTextPosition(ax, 'B'), [-1.70710678118655 0], 1e-12);
asserts.isapprox(bubbleCloudTextPosition(ax, 'C'), [1 0], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
n = [58 115 81 252 180 124 40 80 50 20]';
loc = ["NJ" "NY" "MA" "OH" "NH" "ME" "CT" "PA" "RI" "VT"]';
plant = ["Plant A" "Plant A" "Plant A" "Plant A" "Plant A" ...
  "Plant A" "Plant A" "Plant B" "Plant B" "Plant B"]';
tbl = table(n, loc, plant, 'VariableNames', ["Mislabeled" "State" "Manufacturing Plant"]);
h = bubblecloud(tbl, "Mislabeled", "State");
asserts.istrue(isgraphics(h, 'bubblecloud'));
asserts.isequal(h.SizeVariable, 'Mislabeled');
asserts.isequal(h.LabelVariable, 'State');
asserts.isequal(h.GroupVariable, []);
asserts.isequal(h.GroupData, repmat({''}, 1, numel(n)));
asserts.isequal(h.LegendTitle, '');
asserts.isequal(numel(findobj(f, 'Type', 'legend')), 0);
ax = h.Children;
patchHandles = findobj(ax, 'Type', 'patch');
asserts.isequal(numel(patchHandles), numel(n));
centersX = [];
centersY = [];
for k = 1:numel(patchHandles)
  centersX = [centersX, (max(patchHandles(k).XData) + min(patchHandles(k).XData)) / 2];
  centersY = [centersY, (max(patchHandles(k).YData) + min(patchHandles(k).YData)) / 2];
end
asserts.istrue(max(centersY) - min(centersY) > 2);
asserts.istrue(max(centersX) - min(centersX) > 3);
textHandles = findobj(ax, 'Type', 'text');
textX = [];
textY = [];
textLabels = {};
for k = 1:numel(textHandles)
  textX = [textX, textHandles(k).Position(1)];
  textY = [textY, textHandles(k).Position(2)];
  textLabels = [textLabels, textHandles(k).String];
end
idxOH = find(strcmp(textLabels, 'OH'));
idxNH = find(strcmp(textLabels, 'NH'));
idxME = find(strcmp(textLabels, 'ME'));
idxMA = find(strcmp(textLabels, 'MA'));
idxPA = find(strcmp(textLabels, 'PA'));
idxRI = find(strcmp(textLabels, 'RI'));
idxNJ = find(strcmp(textLabels, 'NJ'));
idxCT = find(strcmp(textLabels, 'CT'));
idxNY = find(strcmp(textLabels, 'NY'));
idxVT = find(strcmp(textLabels, 'VT'));
asserts.istrue(textX(idxOH) < textX(idxNH));
asserts.istrue(textX(idxNH) < textX(idxPA));
asserts.istrue(textX(idxNH) < textX(idxMA));
asserts.istrue(textX(idxNH) < textX(idxNJ));
asserts.istrue(textY(idxME) > textY(idxOH));
asserts.istrue(textY(idxMA) > textY(idxNH));
asserts.istrue(textY(idxNY) < textY(idxOH));
asserts.istrue(textY(idxCT) < textY(idxNY));
asserts.istrue(textY(idxVT) > textY(idxOH));
asserts.istrue(textY(idxNJ) < textY(idxPA));
asserts.istrue(textY(idxRI) < textY(idxNJ));
close(f);
%=============================================================================
f = figure('Visible', 'off');
n = [20 40 35 12 30 18 10 25 22]';
flavs = {'Vanilla', 'Chocolate', 'Strawberry', 'Rum', 'Pumpkin', 'Mint', ...
  'Twist', 'Coffee', 'Cookie'}';
ages = categorical({'40-90+', '5-15', '16-39', '40-90+', '5-15', '16-39', ...
  '5-15', '16-39', '40-90+'});
ages = reordercats(ages, {'5-15', '16-39', '40-90+'});
h = bubblecloud(n, flavs, ages);
ax = h.Children;
lgd = findobj(f, 'Type', 'legend');
heightWithoutTitle = lgd.Position(4);
h.LegendTitle = 'Age Range';
asserts.isequal(lgd.String, {'5-15', '16-39', '40-90+'});
asserts.isequal(lgd.Title.String, 'Age Range');
asserts.istrue(lgd.Position(4) > heightWithoutTitle);
chocolate = bubbleCloudTextPosition(ax, 'Chocolate');
pumpkin = bubbleCloudTextPosition(ax, 'Pumpkin');
twist = bubbleCloudTextPosition(ax, 'Twist');
strawberry = bubbleCloudTextPosition(ax, 'Strawberry');
mint = bubbleCloudTextPosition(ax, 'Mint');
coffee = bubbleCloudTextPosition(ax, 'Coffee');
vanilla = bubbleCloudTextPosition(ax, 'Vanilla');
rum = bubbleCloudTextPosition(ax, 'Rum');
cookie = bubbleCloudTextPosition(ax, 'Cookie');
blueCenter = mean([chocolate; pumpkin; twist]);
orangeCenter = mean([strawberry; mint; coffee]);
yellowCenter = mean([vanilla; rum; cookie]);
asserts.istrue(blueCenter(2) > orangeCenter(2));
asserts.istrue(blueCenter(2) > yellowCenter(2));
asserts.istrue(yellowCenter(1) < orangeCenter(1));
asserts.istrue(twist(2) > chocolate(2));
asserts.istrue(rum(2) > vanilla(2));
asserts.istrue(mint(2) > strawberry(2));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bubblecloud(f, [1 2], 'FaceColor', [0 1 0], 'EdgeColor', 'k', ...
  'LegendTitle', 'Groups');
asserts.isequal(h.Parent, f);
asserts.isequal(h.FaceColor, [0 1 0]);
asserts.isequal(h.EdgeColor, [0 0 0]);
asserts.isequal(h.LegendTitle, 'Groups');
close(f);
%=============================================================================
asserts.checkerror('bubblecloud();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('bubblecloud({1});', getString(message('Nelson:validators:mustBeNumeric', 'Bubble sizes')));
asserts.checkerror('bubblecloud([1 2; 3 4]);', getString(message('Nelson:validators:mustBeVector', 'Bubble sizes')));
asserts.checkerror('f = figure(''Visible'', ''off''); ax = axes(); bubblecloud(ax, [1 2]); close(f);', getString(message('Nelson:bubblecloud:bubbleCloudCannotBeA')));
asserts.checkerror('bubblecloud([1], {''a''}, {''g''}, ''BadName'', 1);', getString(message('Nelson:graphics:unsupportedProperty', 'bubblecloud')));
%=============================================================================
function position = bubbleCloudTextPosition(ax, label)
  handles = findobj(ax, 'Type', 'text');
  for k = 1:numel(handles)
    if strcmp(handles(k).String, label)
      position = handles(k).Position(1:2);
      return
    end
  end
  error(['Text label not found: ', label]);
end
%=============================================================================
function assertBubbleCloudLegendOutside(ax, lgd, checkWidth)
  % legend right of the drawn box and top aligned with it, inside the figure
  f = ancestor(ax, 'figure');
  W = f.Position(3);
  H = f.Position(4);
  if checkWidth
    % at creation the box gives up room only when the legend does not fit the
    % right margin: its width plus an 8 point gap and a 16 point pad
    gap = 8 * 96 / 72;
    legendWidth = lgd.Position(3) * W;
    expectedWidth = min(0.775, (W - legendWidth - 3 * gap) / W - 0.13);
    asserts.isapprox(ax.Position(3), expectedWidth, 1e-6);
  end
  % the locked data aspect ratio centres the data box inside Position
  pixelW = ax.Position(3) * W;
  pixelH = ax.Position(4) * H;
  dar = ax.DataAspectRatio;
  aspect = (diff(ax.XLim) / dar(1)) / (diff(ax.YLim) / dar(2));
  drawnW = min(pixelW, pixelH * aspect) / W;
  drawnH = min(pixelH, pixelW / aspect) / H;
  drawnTop = ax.Position(2) + (ax.Position(4) + drawnH) / 2;
  drawnRight = ax.Position(1) + (ax.Position(3) + drawnW) / 2;
  asserts.istrue(lgd.Position(1) >= drawnRight);
  asserts.isapprox(lgd.Position(2) + lgd.Position(4), drawnTop, 1e-9);
  asserts.istrue(lgd.Position(1) + lgd.Position(3) <= 1);
  asserts.istrue(lgd.Position(2) >= 0 && lgd.Position(2) + lgd.Position(4) <= 1);
end
%=============================================================================
