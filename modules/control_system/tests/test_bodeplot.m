%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
G = tf([3], [1 1/2 4]);
%=============================================================================
% bodeplot auto-ranges one decade around the system corner frequency.
[mag, phase, wout] = bodeplot(G);
asserts.isequal(size(wout), [1 1000]);
asserts.isapprox(wout(1), 0.1, 1e-10);
asserts.isapprox(wout(end), 100, 1e-10);
%=============================================================================
% bodeplot data matches bode data on the same frequency grid.
wAuto = logspace(-1, 2, 1000);
[magRef, phaseRef, wRef] = bode(G, wAuto);
[mag, phase, wout] = bodeplot(G);
asserts.isequal(size(mag), size(magRef));
asserts.isapprox(mag, magRef, 1e-10);
asserts.isapprox(phase, phaseRef, 1e-10);
asserts.isapprox(wout, wRef, 1e-10);
%=============================================================================
% bodeplot honours an explicit frequency vector like bode.
w = logspace(-1, 2, 300);
[mag, phase, wout] = bodeplot(G, w);
[magRef, phaseRef, wRef] = bode(G, w);
asserts.isapprox(mag, magRef, 1e-10);
asserts.isapprox(phase, phaseRef, 1e-10);
asserts.isapprox(wout, wRef, 1e-10);
%=============================================================================
% Rendering variants run without error.
f = figure('Visible', 'off');
bodeplot(G);
close(f);
%=============================================================================
opt = bodeoptions();
opt.PhaseUnits = 'rad';
opt.Grid = 'on';
f = figure('Visible', 'off');
bodeplot(G, opt);
close(f);
%=============================================================================
f = figure('Visible', 'off');
bodeplot(G, opt, '--');
close(f);
%=============================================================================
f = figure('Visible', 'off');
bodeplot(G, '--');
close(f);
%=============================================================================
% Axis labels use the documented units (degree phase by default).
bodeplot(G);
ax = findobj(gcf(), 'type', 'axes');
magnitudeAxes = axesWithYLabel(ax, 'Magnitude (dB)');
phaseAxes = axesWithYLabel(ax, 'Phase (deg)');
asserts.istrue(~isempty(magnitudeAxes));
asserts.istrue(~isempty(phaseAxes));
asserts.isequal(phaseAxes.XLabel.String, 'Frequency (rad/s)');
close(gcf());
%=============================================================================
% Magnitude and phase plots share the frequency axis: the magnitude plot on
% top keeps its tick marks but drops the frequency number labels; those appear
% only under the phase plot at the bottom.
bodeplot(G);
ax = findobj(gcf(), 'type', 'axes');
magnitudeAxes = axesWithYLabel(ax, 'Magnitude (dB)');
phaseAxes = axesWithYLabel(ax, 'Phase (deg)');
asserts.istrue(isempty(magnitudeAxes.XTickLabel));
asserts.isfalse(isempty(phaseAxes.XTickLabel));
asserts.isequal(magnitudeAxes.XLabel.String, '');
asserts.isequal(phaseAxes.XLabel.String, 'Frequency (rad/s)');
close(gcf());
%=============================================================================
% Radian phase axis is labelled in radians and ticked with plain decimals.
opt = bodeoptions();
opt.PhaseUnits = 'rad';
bodeplot(G, opt);
ax = findobj(gcf(), 'type', 'axes');
phaseAxes = axesWithYLabel(ax, 'Phase (rad)');
asserts.istrue(~isempty(phaseAxes));
asserts.isapprox(phaseAxes.YTick, [-3.5 -3 -2.5 -2 -1.5 -1 -0.5 0], 1e-10);
close(gcf());
%=============================================================================
% Title and the magnitude/phase/frequency labels actually render (they were
% dropped off the log-axis projection even though their String was set).
opt = bodeoptions();
opt.PhaseUnits = 'rad';
bodeplot(G, opt);
f = gcf();
ax = findobj(f, 'type', 'axes');
magnitudeAxes = axesWithYLabel(ax, 'Magnitude (dB)');
phaseAxes = axesWithYLabel(ax, 'Phase (rad)');
asserts.istrue(~isempty(magnitudeAxes));
asserts.istrue(~isempty(phaseAxes));
asserts.isequal(magnitudeAxes.Title.String, 'Bode Diagram');
asserts.isequal(phaseAxes.XLabel.String, 'Frequency (rad/s)');
withLabels = darkPixelCount(f);
magnitudeAxes.YLabel.String = '';
phaseAxes.YLabel.String = '';
phaseAxes.XLabel.String = '';
withoutLabels = darkPixelCount(f);
% The three axis labels contribute a substantial number of dark glyph pixels;
% if they were clipped off the projection the two counts would be equal.
asserts.istrue((withLabels - withoutLabels) > 200);
close(f);
%=============================================================================
function ax = axesWithYLabel(axesList, label)
  ax = [];
  for k = 1:numel(axesList)
    if strcmp(axesList(k).YLabel.String, label)
      ax = axesList(k);
      return
    end
  end
end
%=============================================================================
function n = darkPixelCount(f)
  drawnow();
  frame = getframe(f);
  c = double(frame.cdata);
  mask = (c(:, :, 1) < 150) & (c(:, :, 2) < 150) & (c(:, :, 3) < 150);
  n = sum(mask(:));
end
%=============================================================================
