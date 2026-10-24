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
h = meshc(peaks(8));
asserts.isequal(numel(h), 2);
asserts.istrue(isgraphics(h(1), 'surface'));
asserts.istrue(isgraphics(h(2), 'contour'));
asserts.isequal(h(1).FaceColor, [1 1 1]);
asserts.isequal(h(1).EdgeColor, 'flat');
asserts.isequal(h(2).ZLocation, 'zmin');
asserts.isapprox(ancestor(h(1), 'axes').ZLim, [min(h(1).ZData(:)) max(h(1).ZData(:))], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = meshc('Parent', ax, peaks(20));
asserts.isequal(ax.XTick, [5 10 15 20]);
asserts.isequal(ax.YTick, [5 10 15 20]);
asserts.isapprox(ax.ZLim, [min(h(1).ZData(:)) max(h(1).ZData(:))], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = meshc(ax, peaks(6), 'LineWidth', 2);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = meshc('Parent', ax, peaks(4), 'LineWidth', 2);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
Z = peaks(5);
C = ones(size(Z)) * 7;
h = meshc(Z, C);
asserts.isequal(h(1).CData, C);
asserts.isequal(h(2).ZData, Z);
close(f);
%=============================================================================
try
  meshc(1:3);
  asserts.fail('meshc accepted a vector input.');
catch ME
  asserts.isequal(ME.identifier, 'Nelson:meshc:invalidSurfaceZ');
end
%=============================================================================
