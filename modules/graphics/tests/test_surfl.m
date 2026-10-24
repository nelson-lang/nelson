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
h = surfl(peaks(8));
asserts.istrue(isgraphics(h, 'surface'));
asserts.isequal(h.FaceLighting, 'flat');
asserts.isequal(h.EdgeLighting, 'none');
asserts.isapprox(h.AmbientStrength, 0.3, 1e-12);
asserts.isapprox(h.DiffuseStrength, 0.6, 1e-12);
asserts.isapprox(h.SpecularStrength, 0.9, 1e-12);
asserts.isequal(size(h.CData), [8 8]);
ax = ancestor(h, 'axes');
asserts.isequal(ax.View, [-37.5 30]);
lights = findobj(ax, 'Type', 'light');
asserts.isequal(numel(lights), 0);
asserts.isequal(ax.CLim, [0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = surfl(ax, peaks(6), 'FaceColor', 'interp');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.FaceColor, 'interp');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = surfl(peaks(8), 'light');
asserts.isequal(numel(h), 2);
asserts.istrue(isgraphics(h(1), 'surface'));
asserts.istrue(isgraphics(h(2), 'light'));
asserts.isapprox(h(1).AmbientStrength, 0.55, 1e-12);
asserts.isapprox(h(1).DiffuseStrength, 0.6, 1e-12);
asserts.isapprox(h(1).SpecularStrength, 0.4, 1e-12);
asserts.isequal(h(2).Style, 'infinite');
close(f);
%=============================================================================
