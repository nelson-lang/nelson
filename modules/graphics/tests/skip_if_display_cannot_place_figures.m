%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function skip_if_display_cannot_place_figures(reason)
  % Visible-figure geometry and pixel-readback tests need the OS to place and
  % size a window close to the request. During a long run the display can
  % sleep, lock or switch monitor, after which windows open off-screen or
  % collapsed (a 400x300 request comes back near 78x364 or at a large negative
  % position). That is an environment fault, not a Nelson defect: the same
  % tests pass on a live display, so skip instead of failing.
  % The bounds are loose on purpose: a window manager nudging the position or
  % a scaled display answering 398x298 never trips them, and any unexpected
  % read-back lets the test run so the guard never hides a real failure.
  if nargin < 1
    reason = 'figure placement';
  end
  requested = [100 100 400 300];
  probe = figure('Visible', 'on', 'Position', requested);
  cleaner = onCleanup(@() close(probe)); %#ok<NASGU>
  drawnow();
  placed = probe.Position;
  degraded = false;
  if isnumeric(placed) && numel(placed) == 4 && all(isfinite(placed))
    collapsed = placed(3) < 0.5 * requested(3) || placed(4) < 0.5 * requested(4);
    offScreen = placed(1) < -100 || placed(2) < -100;
    degraded = collapsed || offScreen;
  end
  skip_testsuite(degraded, ...
    ['Display cannot place figures (', reason, '): requested [', ...
    num2str(requested), '] but the window opened at [', num2str(placed), ']']);
end
