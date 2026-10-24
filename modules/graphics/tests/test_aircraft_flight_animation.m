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
flightFigureVisible = 'off';
flightAnimationEnabled = true;
flightAnimationFrames = 5;
flightAnimationContinuous = false;
run([modulepath('graphics', 'root'), ...
  '/examples/aircraft_flight_animation.m']);

assert_isequal(numel(flightTime), 720);
assert_isequal(size(flightAircraftVertices), [9 3]);
assert_isequal(size(flightAircraftFaces), [11 3]);
assert_istrue(all(isfinite(flightEast)));
assert_istrue(all(isfinite(flightNorth)));
assert_istrue(all(isfinite(flightAltitude)));
assert_istrue(all(isfinite(flightSpeed)));
assert_istrue(all(isfinite(flightRoll)));
assert_istrue(all(isfinite(flightPitch)));
assert_istrue(all(isfinite(flightYaw)));
assert_istrue(all(flightAltitude > 0));
assert_istrue(all(flightSpeed > 0));
assert_istrue(max(abs(flightRoll)) <= 55 * pi / 180 + eps);
assert_istrue(isgraphics(figureHandle));
assert_istrue(isgraphics(aircraftPatch));
assert_istrue(isgraphics(horizonSky));
assert_istrue(isgraphics(horizonGround));
assert_istrue(isgraphics(horizonLine));
assert_istrue(isgraphics(horizonStatus));
assert_istrue(all(horizonGround.XData .^ 2 + ...
  horizonGround.YData .^ 2 <= 1 + 1e-10));
assert_isapprox(horizonLine.XData .^ 2 + ...
  horizonLine.YData .^ 2, ones(size(horizonLine.XData)), 1e-10);
assert_istrue(contains(horizonStatus.String, 'Roll'));
assert_istrue(any(abs(aircraftPatch.Vertices(:) - ...
  flightAircraftVerticesInitial(:)) > 1e-8));

[trailX, trailY, trailZ] = getpoints(flightTrail);
[altitudeTime, altitudeValues] = getpoints(altitudeTrace);
[speedTime, speedValues] = getpoints(speedTrace);
assert_isequal([numel(trailX) numel(trailY) numel(trailZ)], [5 5 5]);
assert_isequal([numel(altitudeTime) numel(altitudeValues)], [5 5]);
assert_isequal([numel(speedTime) numel(speedValues)], [5 5]);
assert_isapprox(trailX(end), flightEast(end), 1e-10);
assert_isapprox(trailY(end), flightNorth(end), 1e-10);
assert_isapprox(trailZ(end), flightAltitude(end), 1e-10);
assert_isapprox(altitudeValues(end), flightAltitude(end), 1e-10);
assert_isapprox(speedValues(end), flightSpeed(end), 1e-10);
delete(figureHandle);
drawnow();
%=============================================================================
