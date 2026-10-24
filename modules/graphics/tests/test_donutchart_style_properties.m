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
d.Data = [5 4 3 2 1];
d.Names = ["A", "B", "C", "D", "E"];
d.DisplayOrder = 'descend';
d.NumDisplayWedges = 3;
d.ShowOthers = 'on';
d.LabelStyle = 'data';
d.InnerRadius = 0.35;
d.CenterLabel = 'Total';
d.CenterLabelFontSize = 15;
drawnow();
%=============================================================================
d.Labels = ["one", "two", "three", "other"];
d.StartAngle = 90;
d.Direction = 'counterclockwise';
d.ExplodedWedges = [2];
d.ColorOrder = [0.8 0.2 0.2; 0.2 0.8 0.2; 0.2 0.2 0.8];
d.FontColor = [0.2 0.2 0.2];
d.Interpreter = 'none';
d.Title = 'Donut';
d.Visible = 'on';
drawnow();
asserts.isequal(d.LabelsMode, 'manual');
asserts.isequal(d.Labels, ["one", "two", "three", "other"]);
asserts.isequal(d.StartAngle, 90);
asserts.isequal(d.Direction, 'counterclockwise');
asserts.isequal(d.ExplodedWedges, 2);
asserts.isequal(d.ColorOrder, [0.8 0.2 0.2; 0.2 0.8 0.2; 0.2 0.2 0.8]);
asserts.isequal(d.FontColor, [0.2 0.2 0.2]);
asserts.isequal(d.Interpreter, 'none');
asserts.isequal(d.Title, 'Donut');
%=============================================================================
assert_readonly(d, 'Proportions');
assert_readonly(d, 'CategoryCounts');
assert_readonly(d, 'WedgeDisplayData');
assert_readonly(d, 'WedgeDisplayNames');
%=============================================================================
asserts.istrue(isValidGraphicsProperty('donutchart', 'InnerRadius'));
asserts.istrue(isValidGraphicsProperty('donutchart', 'CenterLabel'));
asserts.istrue(isValidGraphicsProperty('donutchart', 'WedgeDisplayNames'));
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
