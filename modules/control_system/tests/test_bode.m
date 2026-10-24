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
H = tf([1 0.1 7.5], [1 0.12 9 0 0]);
[mag, phase, wout] = bode(H);
asserts.isequal(size(mag), [1 1 1000]);
asserts.isapprox(mag(1), 78.4163, 1e-4);
asserts.isapprox(mag(end), -79.9987, 1e-4);
asserts.isequal(size(phase), [1 1 1000]);
asserts.isapprox(phase(1), -180, 1e-4);
asserts.isapprox(phase(end), -179.9885, 1e-4);
asserts.isequal(size(wout), [1 1000]);
asserts.isapprox(wout(1), 0.01, 1e-4);
asserts.isapprox(wout(end), 100, 1e-4);
%=============================================================================
% Magnitude and phase plots share the frequency axis: the magnitude plot on
% top keeps its tick marks but drops the frequency number labels; those appear
% only under the phase plot at the bottom.
bode(H);
f = gcf();
ax = findobj(f, 'Type', 'axes');
magnitudeAxes = axesWithYLabel(ax, 'Magnitude [dB]');
phaseAxes = axesWithYLabel(ax, 'Phase [deg]');
asserts.istrue(~isempty(magnitudeAxes));
asserts.istrue(~isempty(phaseAxes));
asserts.istrue(isempty(magnitudeAxes.XTickLabel));
asserts.isfalse(isempty(phaseAxes.XTickLabel));
asserts.isequal(phaseAxes.XLabel.String, 'Frequency [rad/s]');
asserts.isequal(magnitudeAxes.XLabel.String, '');
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
