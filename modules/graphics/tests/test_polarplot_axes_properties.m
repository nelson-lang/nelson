%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
ax = polaraxes();
asserts.isequal(ax.Type, 'polaraxes');
asserts.istrue(isgraphics(ax, 'polaraxes'));
asserts.isfalse(isgraphics(ax, 'axes'));
polarProperties = {'RLim', 'RLimMode', 'RTick', 'RTickMode', ...
  'RTickLabel', 'RTickLabelMode', 'ThetaAxisUnits', 'ThetaLim', 'ThetaLimMode', ...
  'ThetaTick', 'ThetaTickMode', 'ThetaTickLabel', 'ThetaTickLabelMode'};
visibleProperties = properties(ax);
asserts.istrue(all(ismember(polarProperties, visibleProperties)));
asserts.isfalse(any(ismember({'XLim', 'YLim', 'ZLim', 'XTick', 'YTick', 'ZTick'}, visibleProperties)));
asserts.istrue(isprop(ax, 'RLim'));
asserts.istrue(isprop(ax, 'ThetaAxisUnits'));
asserts.istrue(isprop(ax, 'ThetaTickLabel'));
polarplot(ax, linspace(0, pi, 10), linspace(1, 2, 10));
rlim(ax, [0 3]);
thetalim(ax, [0 180]);
rticks(ax, [0 1.5 3]);
thetaticks(ax, [0 45 90 135 180]);
rticklabels(ax, {'zero'; 'mid'; 'max'});
thetaticklabels(ax, {'E'; 'NE'; 'N'; 'NW'; 'W'});
asserts.isequal(rlim(ax), [0 3]);
asserts.isequal(thetalim(ax), [0 180]);
asserts.isequal(rticks(ax), [0 1.5 3]);
asserts.isequal(thetaticks(ax), [0 45 90 135 180]);
asserts.isequal(rticklabels(ax), {'zero'; 'mid'; 'max'});
asserts.isequal(thetaticklabels(ax), {'E'; 'NE'; 'N'; 'NW'; 'W'});
asserts.isequal(ax.RLim, [0 3]);
asserts.isequal(ax.RLimMode, 'manual');
asserts.isequal(ax.ThetaLim, [0 180]);
asserts.isequal(ax.ThetaLimMode, 'manual');
asserts.isequal(ax.RTick, [0 1.5 3]);
asserts.isequal(ax.RTickMode, 'manual');
asserts.isequal(ax.ThetaTick, [0 45 90 135 180]);
asserts.isequal(ax.ThetaTickMode, 'manual');
ax.ThetaAxisUnits = 'radians';
asserts.isequal(ax.ThetaAxisUnits, 'radians');
asserts.isapprox(ax.ThetaLim, [0 pi], 1e-12);
asserts.isapprox(ax.ThetaTick, [0 pi / 4 pi / 2 3 * pi / 4 pi], 1e-12);
asserts.isequal(ax.ThetaTickLabel{2}, 'NE');
ax.ThetaTickLabelMode = 'auto';
ax.ThetaAxisUnits = 'degrees';
ax.ThetaAxisUnits = 'radians';
asserts.isequal(ax.ThetaTickLabel{3}, '\pi/2');
ax.RLim = [0 4];
asserts.isequal(ax.RLimMode, 'manual');
asserts.isequal(rlim(ax), [0 4]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
visibleProperties = properties(ax);
asserts.isfalse(any(ismember(polarProperties, visibleProperties)));
asserts.isfalse(isprop(ax, 'RLim'));
asserts.checkerror('get(ax, ''RLim'')', getString(message('Nelson:graphics:unrecognizedProperty', 'RLim')));
asserts.checkerror('set(ax, ''RLim'', [0 2])', graphicsPropertyErrorMessage('RLim', 'Nelson:graphics:unrecognizedProperty', 'RLim'));
close(f);
%=============================================================================
