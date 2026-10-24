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
h = swarmchart3([1 1 2], [1 2 1], [3 4 5], 'XJitter', 'none', 'YJitter', 'none');
asserts.istrue(isgraphics(h, 'scatter'));
asserts.isequal(h.XData, [1 1 2]);
asserts.isequal(h.YData, [1 2 1]);
asserts.isequal(h.ZData, [3 4 5]);
asserts.isequal(h.XJitter, 'none');
asserts.isequal(h.YJitter, 'none');
asserts.isapprox(h.XJitterWidth, 0.9, 1e-12);
asserts.isapprox(h.YJitterWidth, 0.9, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'manual');
asserts.isequal(h.YJitterWidthMode, 'manual');
asserts.isequal(h.UserData.ChartType, 'swarmchart3');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = swarmchart3([1 1 2], [1 2 1], [3 4 5]);
asserts.isequal(h.XData, [1 1 2]);
asserts.isequal(h.YData, [1 2 1]);
asserts.isequal(h.ZData, [3 4 5]);
asserts.isequal(h.XJitter, 'density');
asserts.isequal(h.YJitter, 'density');
asserts.isapprox(h.XJitterWidth, 0.9, 1e-12);
asserts.isapprox(h.YJitterWidth, 0.9, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'manual');
asserts.isequal(h.YJitterWidthMode, 'manual');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = swarmchart3(ax, [1 1], [2 2], [3 4], 36, [0 0 1], 'filled', 'YJitterWidth', 0.1);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.UserData.Options.YJitterWidth, 0.1);
asserts.isapprox(h.YJitterWidth, 0.1, 1e-12);
asserts.isequal(h.YJitterWidthMode, 'manual');
close(f);
%=============================================================================
asserts.checkerror('swarmchart3();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('swarmchart3({1}, 2, 3);', getString(message('Nelson:swarmchart3:invalidXYAnd')));
asserts.checkerror('swarmchart3([1 2], 2, 3);', getString(message('Nelson:swarmchart3:invalidXYAnd')));
%=============================================================================
