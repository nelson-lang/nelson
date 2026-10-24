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
close all
f = figure('Visible', 'off', 'Units', 'pixels', 'InnerPosition', [100 100 420 320], ...
  'Color', [1 1 1]);
p = piechart([1 2 3]);
asserts.istrue(isgraphics(p, 'piechart'));
asserts.isequal(p.Type, 'piechart');
assert_required_properties(p, reference_piechart_properties());
asserts.isequal(p.Data, [1 2 3]);
asserts.isequal(p.Names, ["data1", "data2", "data3"]);
asserts.isequal(p.Labels, ["16.7%", "33.3%", "50%"]);
asserts.isequal(p.LabelStyle, 'percent');
asserts.isequal(p.StartAngle, 0);
asserts.isequal(p.Direction, 'clockwise');
asserts.isequal(p.FaceColor, 'flat');
asserts.isapprox(p.EdgeColor, [33 33 33] / 255, 1e-12);
asserts.isapprox(p.FaceAlpha, 0.6, 1e-12);
asserts.isapprox(p.LineWidth, 0.5, 1e-12);
asserts.isequal(char(p.LegendVisible), 'off');
asserts.isequal(p.DisplayOrder, 'data');
asserts.istrue(isinf(p.NumDisplayWedges));
asserts.isequal(char(p.ShowOthers), 'on');
asserts.isequal(p.Proportions, [1 2 3] / 6);
asserts.isequal(p.WedgeDisplayData, [1 2 3]);
asserts.isequal(p.WedgeDisplayNames, ["data1", "data2", "data3"]);
close(f);

f = figure('Visible', 'off');
p = piechart([4 3 2], ["A", "B", "C"]);
asserts.isequal(p.Data, [4 3 2]);
asserts.isequal(p.Names, {'A', 'B', 'C'});
close(f);
caught = false;
try
  piechart({'A', 'B', 'C'}, [4 3 2]);
catch
  caught = true;
end
asserts.istrue(caught);
close all
%=============================================================================
function assert_required_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
end
%=============================================================================
function names = reference_piechart_properties()
  names = {'Data'; 'Names'; 'DataVariable'; 'NamesVariable'; 'Labels'; 'LabelStyle';
    'StartAngle'; 'Direction'; 'ExplodedWedges'; 'ColorOrder'; 'FaceColor'; 'EdgeColor';
    'FaceAlpha'; 'LineWidth'; 'FontSize'; 'FontName'; 'FontColor'; 'Interpreter';
    'LegendVisible'; 'LegendTitle'; 'Title'; 'DisplayOrder'; 'NumDisplayWedges';
    'ShowOthers'; 'Proportions'; 'CategoryCounts'; 'WedgeDisplayData';
    'WedgeDisplayNames'; 'DataMode'; 'FontSizeMode'; 'LabelsMode'; 'LabelStyleMode';
    'NamesMode'; 'Parent'; 'Visible'; 'HandleVisibility'; 'Units'; 'OuterPosition';
    'InnerPosition'; 'Position'; 'PositionConstraint'; 'Layout'; 'SourceTable'; 'Type'};
end
%=============================================================================
