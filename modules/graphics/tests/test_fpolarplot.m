%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
h = fpolarplot(@(t) 2 + sin(t), [0 pi], 'r--', 'LineWidth', 2);
asserts.istrue(isgraphics(h, 'functionline'));
asserts.isequal(h.Type, 'functionline');
asserts.isequal(h.XRange, [0 pi]);
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.MeshDensity, 23);
asserts.isequal(size(h.XData), [1 0]);
asserts.isequal(size(h.YData), [1 0]);
asserts.isequal(size(h.ThetaData), [1 803]);
asserts.isequal(size(h.RData), [1 803]);
asserts.isapprox(h.ThetaData(1), 0, 1e-12);
asserts.isapprox(h.ThetaData(end), pi, 1e-12);
asserts.isapprox(h.RData(1), 2, 1e-12);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes();
h = fpolarplot(ax, @(t) t * 0 + 1, 'ob', 'MeshDensity', 11);
asserts.isequal(h.Parent, ax);
asserts.isequal(h.MeshDensity, 11);
asserts.isequal(h.Marker, 'o');
asserts.isequal(h.Color, [0 0 1]);
asserts.isequal(h.LineStyle, 'none');
asserts.isequal(size(h.XData), [1 0]);
asserts.isequal(numel(h.ThetaData), 383);
close(f);
%=============================================================================
h = fpolarplot(1);
asserts.isequal(h.Type, 'functionline');
asserts.isequal(h.RData, ones(1, 659));
asserts.isequal(h.Color, [0.066 0.443 0.745]);
close(gcf());
asserts.checkerror('fpolarplot(axes(), @sin);', getString(message('Nelson:graphics:invalidParentInput')));
asserts.checkerror('fpolarplot(@(t) t, [1 0]);', getString(message('Nelson:fplot:invalidRangeValue')));
asserts.checkerror('fpolarplot(@(t) {t});', getString(message('Nelson:graphics:invalidFunctionOutput')));
%=============================================================================
