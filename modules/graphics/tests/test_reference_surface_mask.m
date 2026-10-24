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
% Reference-surface compatibility mask: several objects inherit properties from a
% shared base class that the reference numerical-computing language does not expose
% for the equivalent object. Those properties must be absent from the user-facing
% introspection surface (isprop / get(h) struct / properties), while remaining
% reachable through dotted access and get(h, name), which back the rendering path.
%=============================================================================
% area: patch/surface properties are hidden, but the object still works and the
% hidden property remains reachable through dotted access.
f = figure('Visible', 'off');
h = area([1 3 2]);
asserts.isfalse(isprop(h, 'Vertices'));
asserts.isfalse(isprop(h, 'CData'));
asserts.isfalse(isprop(h, 'FaceLighting'));
asserts.isfalse(isprop(h, 'YDataMode'));
g = get(h);
asserts.isfalse(isfield(g, 'Vertices'));
asserts.isfalse(isfield(g, 'CData'));
% dotted access and get(h, name) still resolve the hidden property
asserts.isequal(size(h.Vertices), [6 3]);
v = get(h, 'Vertices');
asserts.isequal(size(v), [6 3]);
close(f);
%=============================================================================
% Base/derived scoping: patch hides XDataMode and YDataMode; its derived area
% keeps XDataMode (only YDataMode is masked there).
f = figure('Visible', 'off');
hp = patch([0 1 1], [0 0 1], [1 0 0]);
asserts.isfalse(isprop(hp, 'XDataMode'));
asserts.isfalse(isprop(hp, 'YDataMode'));
close(f);
f = figure('Visible', 'off');
ha = area([1 3 2]);
asserts.istrue(isprop(ha, 'XDataMode'));
asserts.isfalse(isprop(ha, 'YDataMode'));
close(f);
%=============================================================================
% Cartesian axes: reference-absent properties are hidden.
f = figure('Visible', 'off');
ax = axes();
asserts.isfalse(isprop(ax, 'DataLimits'));
asserts.isfalse(isprop(ax, 'PositionMode'));
asserts.isfalse(isprop(ax, 'TextHeight'));
asserts.isfalse(isprop(ax, 'XTickLabelFormat'));
% legitimate cartesian properties stay visible
asserts.istrue(isprop(ax, 'CameraPosition'));
asserts.istrue(isprop(ax, 'View'));
asserts.istrue(isprop(ax, 'XLim'));
close(f);
%=============================================================================
% Polar axes: cartesian-only properties that leak onto polar axes are hidden.
f = figure('Visible', 'off');
pax = polaraxes();
asserts.isfalse(isprop(pax, 'CameraPosition'));
asserts.isfalse(isprop(pax, 'View'));
asserts.isfalse(isprop(pax, 'XAxis'));
asserts.isfalse(isprop(pax, 'DataAspectRatio'));
asserts.isfalse(isprop(pax, 'DataLimits'));
% polar-applicable properties stay visible
asserts.istrue(isprop(pax, 'RLim'));
asserts.istrue(isprop(pax, 'ThetaLim'));
close(f);
%=============================================================================
% Cartesian line: RData/ThetaData family is hidden.
f = figure('Visible', 'off');
hl = plot(1:10);
asserts.isfalse(isprop(hl, 'RData'));
asserts.isfalse(isprop(hl, 'ThetaData'));
asserts.isfalse(isprop(hl, 'RDataSource'));
asserts.isfalse(isprop(hl, 'ThetaVariable'));
% cartesian data properties stay visible
asserts.istrue(isprop(hl, 'XData'));
asserts.istrue(isprop(hl, 'YData'));
close(f);
%=============================================================================
% Polar line (polarplot): same class as a cartesian line, but the RData/ThetaData
% family IS exposed because it belongs to a polar axes.
f = figure('Visible', 'off');
hpl = polarplot(1:10, 1:10);
asserts.istrue(isprop(hpl, 'RData'));
asserts.istrue(isprop(hpl, 'ThetaData'));
close(f);
%=============================================================================
% stairs: never polar, RData/ThetaData family and extra cartesian props hidden.
f = figure('Visible', 'off');
hs = stairs(1:5);
asserts.isfalse(isprop(hs, 'RData'));
asserts.isfalse(isprop(hs, 'ThetaData'));
asserts.isfalse(isprop(hs, 'ZData'));
close(f);
%=============================================================================
% contour: Fill / Floating / LineColor are hidden.
f = figure('Visible', 'off');
[~, hc] = contour(peaks(20));
asserts.isfalse(isprop(hc, 'Fill'));
asserts.isfalse(isprop(hc, 'Floating'));
asserts.isfalse(isprop(hc, 'LineColor'));
close(f);
%=============================================================================
% bubblechart: RData/ThetaData family and jitter-width modes are hidden.
f = figure('Visible', 'off');
hb = bubblechart(1:5, 1:5, 1:5);
asserts.isfalse(isprop(hb, 'RData'));
asserts.isfalse(isprop(hb, 'ThetaJitter'));
asserts.isfalse(isprop(hb, 'XJitterWidthMode'));
close(f);
%=============================================================================
% Cartesian scatter: RData/ThetaData family and jitter-width modes hidden.
f = figure('Visible', 'off');
hsc = scatter(1:5, 1:5);
asserts.isfalse(isprop(hsc, 'RData'));
asserts.isfalse(isprop(hsc, 'ThetaData'));
asserts.isfalse(isprop(hsc, 'RJitter'));
asserts.isfalse(isprop(hsc, 'XJitterWidthMode'));
asserts.istrue(isprop(hsc, 'XData'));
close(f);
%=============================================================================
% Polar scatter (polarscatter): same class as a cartesian scatter, but the
% RData/ThetaData family IS exposed because it belongs to a polar axes.
f = figure('Visible', 'off');
hps = polarscatter(1:5, 1:5);
asserts.istrue(isprop(hps, 'RData'));
asserts.istrue(isprop(hps, 'ThetaData'));
close(f);
%=============================================================================
% Chart objects expose only a minimal surface: heatmap hides Type/Children/etc.
% from introspection, but dotted access still resolves them.
f = figure('Visible', 'off');
hh = heatmap(magic(4));
asserts.isfalse(isprop(hh, 'Type'));
asserts.isfalse(isprop(hh, 'Children'));
asserts.isfalse(isprop(hh, 'UserData'));
asserts.isequal(hh.Type, 'heatmap');
close(f);
%=============================================================================
% ui components: Units is hidden from introspection but dotted access works.
uf = uifigure('Visible', 'off');
hbtn = uibutton(uf);
asserts.isfalse(isprop(hbtn, 'Units'));
close(uf);
%=============================================================================
