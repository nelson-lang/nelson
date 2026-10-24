%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 5-->
% <--ADV-CLI MODE-->
%=============================================================================
skip_testsuite(isempty(which('odeset')), 'Ball-on-plate LQR example requires ode solvers.');
%=============================================================================
ballOnPlateFigureVisible = 'off';
ballOnPlateBatch = true;
ballOnPlateMaxFrames = 6;
ballOnPlateFramePause = 0;
run([modulepath('control_system', 'root'), '/examples/ball_on_plate_lqr.m']);

asserts.istrue(ballOnPlateAutoStart);
asserts.istrue(ballOnPlateLoop);
asserts.isequal(size(ballOnPlateA), [6 6]);
asserts.isequal(size(ballOnPlateB), [6 2]);
asserts.isequal(size(ballOnPlateK), [2 6]);
asserts.istrue(all(isfinite(ballOnPlateK(:))));
asserts.istrue(all(real(ballOnPlateClosedLoopPoles) < 0));
asserts.istrue(ballOnPlatePath.finalError < 0.02);
sampleCount = numel(ballOnPlatePath.trackingError);
asserts.istrue(max(ballOnPlatePath.trackingError(sampleCount - 50:sampleCount)) < 0.03);
asserts.istrue(max(abs(ballOnPlatePath.commandAlpha)) <= ...
  ballOnPlateParameters.maxTilt + eps);
asserts.istrue(max(abs(ballOnPlatePath.commandBeta)) <= ...
  ballOnPlateParameters.maxTilt + eps);
asserts.istrue(all(isfinite(ballOnPlatePath.motorSteps(:))));
asserts.istrue(isgraphics(ballOnPlateFigure));
asserts.istrue(isgraphics(ballOnPlateControls.plateSurface));
asserts.istrue(isgraphics(ballOnPlateControls.ballSurface));
asserts.istrue(all(isgraphics(ballOnPlateControls.supportLines)));

[ballTrailX, ballTrailY, ballTrailZ] = ...
  getpoints(ballOnPlateControls.ballTrail);
[actualTraceX, actualTraceY] = getpoints(ballOnPlateControls.actualTrace);
[alphaTraceTime, alphaTraceValue] = ...
  getpoints(ballOnPlateControls.alphaTrace);
asserts.isequal([numel(ballTrailX) numel(ballTrailY) numel(ballTrailZ)], ...
  [6 6 6]);
asserts.isequal([numel(actualTraceX) numel(actualTraceY)], [6 6]);
asserts.isequal([numel(alphaTraceTime) numel(alphaTraceValue)], [6 6]);
trailCount = numel(ballTrailX);
asserts.isapprox(ballTrailX(trailCount), ballOnPlatePath.x(sampleCount), 1e-10);
asserts.isapprox(ballTrailY(trailCount), ballOnPlatePath.y(sampleCount), 1e-10);
asserts.istrue(ballTrailZ(trailCount) > ballOnPlateParameters.plateHeight);
asserts.istrue(contains(ballOnPlateControls.titleText.String, ...
  'Final tracking error'));
close(ballOnPlateFigure);
drawnow();
%=============================================================================
