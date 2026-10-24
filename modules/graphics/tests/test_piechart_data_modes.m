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
p.LabelStyle = 'data';
p.FontSize = 12;
drawnow();
asserts.isequal(p.DataMode, 'manual');
asserts.isequal(p.LabelStyleMode, 'manual');
asserts.isequal(p.FontSizeMode, 'manual');
asserts.isequal(p.WedgeDisplayData, [1 2 7]);
asserts.isequal(p.WedgeDisplayNames, ["data4", "data3", "Others"]);
asserts.isequal(p.Labels, ["1", "2", "7"]);
%=============================================================================
p.Names = ["A", "B", "C", "D"];
p.Labels = ["a", "b", "c"];
drawnow();
asserts.isequal(p.NamesMode, 'manual');
asserts.isequal(p.LabelsMode, 'manual');
asserts.isequal(p.WedgeDisplayNames, ["D", "C", "Others"]);
asserts.isequal(p.Labels, ["a", "b", "c"]);
%=============================================================================
close(f);
close all
%=============================================================================
