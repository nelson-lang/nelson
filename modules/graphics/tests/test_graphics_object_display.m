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
close all;
reset(groot());
g = groot();
R = evalc('g');
withPropertiesText = _('with properties:');
asserts.istrue(contains(R, ['Graphics Root ', withPropertiesText]));
asserts.istrue(contains(R, ['CurrentFigure: [0', char(215), '0 GraphicsPlaceholder]']));
asserts.istrue(contains(R, ['ScreenPixelsPerInch: ', int2str(g.ScreenPixelsPerInch)]));
asserts.istrue(contains(R, 'ScreenSize: [1 1'));
asserts.istrue(contains(R, 'MonitorPositions: [1 1'));
asserts.istrue(contains(R, 'Units: ''pixels'''));
asserts.istrue(contains(R, _('Show all properties')));
asserts.isfalse(contains(R, [char(10), '  Root ', withPropertiesText]));
%=============================================================================
f = figure('Visible', 'off');
R = evalc('f');
asserts.istrue(contains(R, 'f ='));
asserts.istrue(contains(R, 'Figure'));
asserts.istrue(contains(R, withPropertiesText));
asserts.istrue(contains(R, 'Number:'));
asserts.istrue(contains(R, 'Name:'));
asserts.istrue(contains(R, 'Color:'));
asserts.istrue(contains(R, 'Position:'));
asserts.istrue(contains(R, 'Units:'));
asserts.istrue(contains(R, _('Show all properties')));
asserts.isfalse(contains(R, 'Alphamap: [0 0.0159 0.0317'));
asserts.isfalse(contains(R, '0.960800'));
%=============================================================================
ax = axes('Parent', f);
R = evalc('ax');
asserts.istrue(contains(R, 'ax ='));
asserts.istrue(contains(R, 'Axes'));
asserts.istrue(contains(R, 'XLim:'));
asserts.istrue(contains(R, 'YLim:'));
asserts.istrue(contains(R, 'XScale:'));
asserts.istrue(contains(R, 'YScale:'));
asserts.istrue(contains(R, 'GridLineStyle:'));
asserts.istrue(contains(R, 'Position: [0.1300 0.1100 0.7750 0.8150]'));
asserts.istrue(contains(R, 'Units:'));
asserts.istrue(contains(R, _('Show all properties')));
asserts.isfalse(contains(R, 'ALim: [0 1]'));
asserts.isfalse(contains(R, 'Alphamap: [0 0.0159 0.0317'));
asserts.isfalse(contains(R, 'AmbientLightColor: [1 1 1]'));
asserts.isfalse(contains(R, 'CameraPosition:'));
asserts.isfalse(contains(R, '0.130000'));
asserts.isequal(ax.ALim, [0 1]);
asserts.isequal(ax.Alphamap(1:8), [0 0.0159 0.0317 0.0476 0.0635 0.0794 0.0952 0.1111]);
asserts.isequal(size(ax.Alphamap), [1 64]);
asserts.isequal(ax.AmbientLightColor, [1 1 1]);
close(f);
%=============================================================================
