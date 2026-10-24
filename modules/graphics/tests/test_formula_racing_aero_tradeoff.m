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
% <--RELEASE ONLY-->
%=============================================================================
raceCircuitNamesToTest = {'Albert Park'; 'Monza'; 'Monaco'; ...
  'Spa-Francorchamps'; 'Suzuka'};
expectedTrackLengths = [5278 5800 3300 7000 5800];
trackSignatures = zeros(1, numel(raceCircuitNamesToTest));
lapTimeResults = zeros(2, numel(raceCircuitNamesToTest));

for circuitIndex = 1:numel(raceCircuitNamesToTest)
  raceFigureVisible = 'off';
  raceAnimationEnabled = false;
  raceAnimationFrames = 4;
  raceAnimationContinuous = false;
  raceCircuitName = raceCircuitNamesToTest{circuitIndex};
  run([modulepath('graphics', 'root'), ...
    '/examples/formula_racing_aero_tradeoff.m']);
  assert_isequal(raceCircuitNames, raceCircuitNamesToTest);
  assert_isequal(circuitSelector.Value, circuitIndex);
  assert_isapprox(sum(segmentLength), ...
    expectedTrackLengths(circuitIndex), 1e-10);
  assert_isequal(size(speedProfiles), [2 720]);
  assert_istrue(all(isfinite(speedProfiles(:))));
  assert_istrue(all(speedProfiles(:) > 0));
  trackSignatures(circuitIndex) = sum(trackCurvature .^ 2);
  lapTimeResults(:, circuitIndex) = lapTimes;
  close(figureHandle);
end
assert_isequal(numel(unique(trackSignatures)), ...
  numel(raceCircuitNamesToTest));
assert_istrue(max(lapTimeResults(:)) > min(lapTimeResults(:)));

raceFigureVisible = 'off';
raceAnimationEnabled = true;
raceAnimationFrames = 4;
raceAnimationContinuous = false;
raceCircuitName = raceCircuitNamesToTest{1};
run([modulepath('graphics', 'root'), ...
  '/examples/formula_racing_aero_tradeoff.m']);

assert_isequal(size(speedProfiles), [2 720]);
assert_isapprox(sum(segmentLength), 5278, 1e-10);
assert_istrue(all(isfinite(speedProfiles(:))));
assert_istrue(all(speedProfiles(:) > 0));
assert_istrue(terminalSpeeds(1) > terminalSpeeds(2));
assert_istrue(max(speedProfiles(1, :)) > max(speedProfiles(2, :)));

cornerThreshold = quantile(trackCurvature, 0.85);
tightCorners = trackCurvature >= cornerThreshold;
assert_istrue(mean(speedProfiles(2, tightCorners)) > ...
  mean(speedProfiles(1, tightCorners)));
assert_istrue(lapTimes(2) < lapTimes(1));
assert_isapprox(lapTimeDelta(end), lapTimes(1) - lapTimes(2), 1e-10);
assert_istrue(isgraphics(figureHandle));
assert_istrue(isgraphics(lowDragCar) && isgraphics(highDownforceCar));
[trackTraceX, trackTraceY] = getpoints(lowDragTrail);
[highTrackTraceX, highTrackTraceY] = getpoints(highDownforceTrail);
[lowSpeedTraceX, lowSpeedTraceY] = getpoints(lowDragSpeedTrace);
[highSpeedTraceX, highSpeedTraceY] = getpoints(highDownforceSpeedTrace);
[deltaTraceX, deltaTraceY] = getpoints(deltaTrace);
assert_isequal([numel(trackTraceX) numel(trackTraceY)], [4 4]);
assert_isequal([numel(highTrackTraceX) numel(highTrackTraceY)], [4 4]);
assert_isequal([numel(lowSpeedTraceX) numel(lowSpeedTraceY)], [4 4]);
assert_isequal([numel(highSpeedTraceX) numel(highSpeedTraceY)], [4 4]);
assert_isequal([numel(deltaTraceX) numel(deltaTraceY)], [4 4]);
delete(figureHandle);
drawnow();

raceFigureHandle = [];
raceFigureVisible = 'off';
raceAnimationEnabled = false;
raceAnimationContinuous = false;
raceCircuitName = raceCircuitNamesToTest{1};
run([modulepath('graphics', 'root'), ...
  '/examples/formula_racing_aero_tradeoff.m']);
retainedFigure = figureHandle;
retainedPosition = retainedFigure.Position;

raceCircuitName = raceCircuitNamesToTest{2};
run([modulepath('graphics', 'root'), ...
  '/examples/formula_racing_aero_tradeoff.m']);
assert_isequal(figureHandle, retainedFigure);
assert_isequal(figureHandle.Position, retainedPosition);
assert_isequal(circuitSelector.Value, 2);
assert_isapprox(sum(segmentLength), expectedTrackLengths(2), 1e-10);
assert_isequal(numel(findobj(figureHandle, 'Type', 'axes')), 3);
assert_isequal(numel(findobj(figureHandle, ...
  'Tag', 'formulaRacingCircuitSelector')), 1);
delete(figureHandle);
%=============================================================================
