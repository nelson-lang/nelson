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
%=============================================================================
p.Data = [4 3 2 1];
p.DisplayOrder = 'ascend';
p.NumDisplayWedges = 2;
p.ShowOthers = 'on';
p.Names = ["A", "B", "C", "D"];
%=============================================================================
p.StartAngle = 45;
p.Direction = 'counterclockwise';
p.ExplodedWedges = [1 3];
p.ColorOrder = [1 0 0; 0 1 0; 0 0 1];
p.FaceColor = [0.2 0.3 0.4];
p.EdgeColor = 'none';
p.FaceAlpha = 0.25;
p.LineWidth = 2;
p.FontName = 'Helvetica';
p.FontColor = [0.1 0.2 0.3];
p.Interpreter = 'none';
p.LegendVisible = 'on';
p.LegendTitle = 'Legend';
p.Title = 'Chart';
p.Units = 'normalized';
p.OuterPosition = [0 0 1 1];
p.InnerPosition = [0.1 0.1 0.8 0.8];
p.Position = [0.15 0.15 0.7 0.7];
p.PositionConstraint = 'innerposition';
p.Visible = 'on';
p.HandleVisibility = 'off';
drawnow();
asserts.isequal(p.StartAngle, 45);
asserts.isequal(p.Direction, 'counterclockwise');
asserts.isequal(p.ExplodedWedges, [1 3]);
asserts.isequal(p.ColorOrder, [1 0 0; 0 1 0; 0 0 1]);
asserts.isequal(p.FaceColor, [0.2 0.3 0.4]);
asserts.isequal(p.EdgeColor, 'none');
asserts.isapprox(p.FaceAlpha, 0.25, 1e-12);
asserts.isequal(p.LineWidth, 2);
asserts.isequal(p.FontName, 'Helvetica');
asserts.isequal(p.FontColor, [0.1 0.2 0.3]);
asserts.isequal(p.Interpreter, 'none');
asserts.isequal(char(p.LegendVisible), 'on');
asserts.isequal(p.LegendTitle, 'Legend');
asserts.isequal(p.Title, 'Chart');
asserts.isequal(p.PositionConstraint, 'innerposition');
asserts.isequal(char(p.HandleVisibility), 'off');
%=============================================================================
assert_readonly(p, 'Proportions');
assert_readonly(p, 'CategoryCounts');
assert_readonly(p, 'WedgeDisplayData');
assert_readonly(p, 'WedgeDisplayNames');
%=============================================================================
asserts.istrue(isValidGraphicsProperty('piechart', 'Data'));
asserts.istrue(isValidGraphicsProperty('piechart', 'WedgeDisplayNames'));
asserts.istrue(isValidGraphicsProperty('piechart', 'Visible'));
%=============================================================================
close(f);
close all
%=============================================================================
function assert_readonly(h, name)
  failed = false;
  try
    set(h, name, []);
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
%=============================================================================
