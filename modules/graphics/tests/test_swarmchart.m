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
h = swarmchart([1 1 2 2], [4 3 2 1], 'XJitter', 'none');
asserts.istrue(isgraphics(h, 'scatter'));
asserts.isequal(h.XData, [1 1 2 2]);
asserts.isequal(h.YData, [4 3 2 1]);
asserts.isequal(h.XJitter, 'none');
asserts.isapprox(h.XJitterWidth, 0.9, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'manual');
asserts.isequal(h.YJitter, 'none');
asserts.isequal(h.SizeData, 36);
asserts.isequal(h.MarkerFaceColor, 'none');
asserts.isequal(h.UserData.ChartType, 'swarmchart');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = swarmchart([1 1 2 2], [4 3 2 1]);
asserts.isequal(h.XData, [1 1 2 2]);
asserts.isequal(h.XJitter, 'density');
asserts.isapprox(h.XJitterWidth, 0.9, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'manual');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = swarmchart(ax, [1 1], [2 3], 36, [1 0 0], 'filled', 'XJitterWidth', 0.2);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.UserData.Options.XJitterWidth, 0.2);
asserts.isapprox(h.XJitterWidth, 0.2, 1e-12);
asserts.isequal(h.XJitterWidthMode, 'manual');
close(f);
%=============================================================================
asserts.checkerror('swarmchart();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('swarmchart({1}, 2);', getString(message('Nelson:swarmchart:invalidXAndY')));
asserts.checkerror('swarmchart([1 2], 3);', getString(message('Nelson:swarmchart:invalidXAndY')));
%=============================================================================
