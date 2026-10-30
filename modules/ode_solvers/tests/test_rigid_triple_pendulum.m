%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(isempty(which('odeset')), 'Rigid triple pendulum example requires ode solvers.');
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
triplePendulumFigureVisible = 'off';
triplePendulumAnimationEnabled = true;
triplePendulumAnimationFrames = 5;
triplePendulumFramePause = 0;
triplePendulumAnimationContinuous = false;
run([modulepath('ode_solvers', 'root'), ...
  '/examples/rigid_triple_pendulum.m']);

assert_isequal(size(triplePendulumState), [301 6]);
assert_isequal(size(triplePendulumAngles), [301 3]);
assert_isequal(size(triplePendulumAngularRates), [301 3]);
assert_isequal(size(triplePendulumRodPatches), [1 3]);
assert_istrue(all(isfinite(triplePendulumState(:))));
assert_istrue(all(isfinite(triplePendulumEnergy)));
assert_istrue(max(abs(triplePendulumEnergyDrift)) < 5e-4);
assert_istrue(isgraphics(triplePendulumFigure));
assert_istrue(all(isgraphics(triplePendulumRodPatches)));
assert_istrue(isgraphics(triplePendulumJointMarkers));
assert_istrue(isgraphics(triplePendulumCenterMarkers));
assert_istrue(isgraphics(triplePendulumFixedSupport));
assert_istrue(isgraphics(triplePendulumFixedPivot));
assert_isequal(triplePendulumMotionAxes.XLimMode, 'manual');
assert_isequal(triplePendulumMotionAxes.YLimMode, 'manual');

[tipX, tipY] = getpoints(triplePendulumTipTrail);
[energyTime, energyDrift] = getpoints(triplePendulumEnergyTrace);
assert_isequal([numel(tipX) numel(tipY)], [5 5]);
assert_isequal([numel(energyTime) numel(energyDrift)], [5 5]);
for rodIndex = 1:3
  [angleTime, angleValue] = ...
    getpoints(triplePendulumAngleTraces(rodIndex));
  assert_isequal([numel(angleTime) numel(angleValue)], [5 5]);
end
finalJoints = zeros(4, 2);
finalCenters = zeros(3, 2);
for rodIndex = 1:3
  finalDirection = [sin(triplePendulumAngles(end, rodIndex)) ...
    -cos(triplePendulumAngles(end, rodIndex))];
  finalCenters(rodIndex, :) = finalJoints(rodIndex, :) + ...
    triplePendulumCenters(rodIndex) * finalDirection;
  finalJoints(rodIndex + 1, :) = finalJoints(rodIndex, :) + ...
    triplePendulumLengths(rodIndex) * finalDirection;
end
assert_isapprox(tipX(end), finalJoints(end, 1), 1e-10);
assert_isapprox(tipY(end), finalJoints(end, 2), 1e-10);
assert_isapprox(triplePendulumJointMarkers.XData, ...
  finalJoints(:, 1).', 1e-10);
assert_isapprox(triplePendulumJointMarkers.YData, ...
  finalJoints(:, 2).', 1e-10);
assert_isapprox(triplePendulumJointMarkers.XData(1), 0, 1e-12);
assert_isapprox(triplePendulumJointMarkers.YData(1), 0, 1e-12);
assert_isapprox(triplePendulumCenterMarkers.XData, ...
  finalCenters(:, 1).', 1e-10);
assert_isapprox(triplePendulumCenterMarkers.YData, ...
  finalCenters(:, 2).', 1e-10);
delete(triplePendulumFigure);
drawnow();
%=============================================================================
