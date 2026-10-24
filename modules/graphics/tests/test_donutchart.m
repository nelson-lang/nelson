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
d = donutchart([1 2 3]);
asserts.istrue(isgraphics(d, 'donutchart'));
asserts.isequal(d.Type, 'donutchart');
assert_required_properties(d, reference_donutchart_properties());
asserts.isequal(d.Data, [1 2 3]);
asserts.isequal(d.Names, ["data1", "data2", "data3"]);
asserts.isequal(d.Labels, ["16.7%", "33.3%", "50%"]);
asserts.isequal(d.LabelStyle, 'percent');
asserts.isapprox(d.InnerRadius, 0.6, 1e-12);
asserts.isequal(d.CenterLabel, '');
asserts.isequal(d.CenterLabelFontSize, 10);
asserts.isequal(d.CenterLabelFontSizeMode, 'auto');
asserts.isequal(d.FaceColor, 'flat');
asserts.isapprox(d.FaceAlpha, 0.6, 1e-12);
asserts.isequal(d.Proportions, [1 2 3] / 6);
asserts.isequal(d.WedgeDisplayData, [1 2 3]);
asserts.isequal(d.WedgeDisplayNames, ["data1", "data2", "data3"]);
close(f);

f = figure('Visible', 'off');
d = donutchart([4 3 2], ["A", "B", "C"]);
asserts.isequal(d.Data, [4 3 2]);
asserts.isequal(d.Names, {'A', 'B', 'C'});
close(f);
caught = false;
try
  donutchart({'A', 'B', 'C'}, [4 3 2]);
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
function names = reference_donutchart_properties()
  names = {'Data'; 'Names'; 'DataVariable'; 'NamesVariable'; 'Labels'; 'LabelStyle';
    'StartAngle'; 'Direction'; 'ExplodedWedges'; 'ColorOrder'; 'FaceColor'; 'EdgeColor';
    'FaceAlpha'; 'LineWidth'; 'FontSize'; 'FontName'; 'FontColor'; 'Interpreter';
    'LegendVisible'; 'LegendTitle'; 'Title'; 'DisplayOrder'; 'NumDisplayWedges';
    'ShowOthers'; 'Proportions'; 'CategoryCounts'; 'WedgeDisplayData';
    'WedgeDisplayNames'; 'DataMode'; 'FontSizeMode'; 'LabelsMode'; 'LabelStyleMode';
    'NamesMode'; 'Parent'; 'Visible'; 'HandleVisibility'; 'Units'; 'OuterPosition';
    'InnerPosition'; 'Position'; 'PositionConstraint'; 'Layout'; 'SourceTable';
    'InnerRadius'; 'CenterLabel'; 'CenterLabelFontSize'; 'CenterLabelFontSizeMode'; 'Type'};
end
%=============================================================================
