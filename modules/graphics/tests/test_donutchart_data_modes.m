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
%=============================================================================
d.Data = [5 4 3 2 1];
d.Names = ["A", "B", "C", "D", "E"];
d.DisplayOrder = 'descend';
d.NumDisplayWedges = 3;
d.ShowOthers = 'on';
d.LabelStyle = 'data';
d.InnerRadius = 0.35;
d.CenterLabel = 'Total';
d.CenterLabelFontSize = 15;
d.LegendVisible = 'on';
d.LegendTitle = 'Names';
d.FaceColor = 'flat';
d.EdgeColor = [0 0 0];
d.FaceAlpha = 0.5;
d.LineWidth = 1.5;
drawnow();
asserts.isequal(d.DataMode, 'manual');
asserts.isequal(d.NamesMode, 'manual');
asserts.isequal(d.LabelStyleMode, 'manual');
asserts.isequal(d.CenterLabelFontSizeMode, 'manual');
asserts.isequal(d.WedgeDisplayData, [5 4 3 3]);
asserts.isequal(d.WedgeDisplayNames, ["A", "B", "C", "Others"]);
asserts.isequal(d.Labels, ["5", "4", "3", "3"]);
asserts.isapprox(d.InnerRadius, 0.35, 1e-12);
asserts.isequal(d.CenterLabel, 'Total');
asserts.isequal(d.CenterLabelFontSize, 15);
close(f);
close all
%=============================================================================
