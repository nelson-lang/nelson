%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = ddeset('RelTol', 1e-5, 'AbsTol', 1e-7);
asserts.istrue(isstruct(options));
asserts.isequal(ddeget(options, 'RelTol'), 1e-5);
asserts.isequal(ddeget(options, 'InitialStep', 3), 3);
initialOptions = ddeset(options, 'InitialY', 2, 'Jumps', 0.1);
asserts.isequal(ddeget(initialOptions, 'InitialY'), 2);
asserts.isequal(ddeget(initialOptions, 'Jumps'), 0.1);
%=============================================================================
sol = dde23(@(t, y, z) - y + z, 0.1, 1, [0 0.2], options);
asserts.isequal(sol.solver, 'dde23');
asserts.istrue(isfield(sol, 'yp'));
asserts.istrue(sol.stats.nchunks >= 1);
asserts.istrue(abs(sol.y(1, length(sol.x)) - 1) < 1e-8);
[yq, ypq] = deval(sol, 0.1);
asserts.istrue(abs(yq(1) - 1) < 1e-8);
asserts.istrue(abs(ypq(1)) < 1e-6);
initialSol = dde23(@(t, y, z) - y + z, 0.1, 1, [0 0.05], initialOptions);
asserts.isequal(initialSol.y(1, 1), 2);
jumpSol = dde23(@(t, y, z) - y + z, 0.1, 1, [0 0.2], ddeset(options, 'Jumps', 0.1));
asserts.istrue(any(abs(jumpSol.x - 0.1) < 1e-12));
continuedSol = dde23(@(t, y, z) - y + z, 0.1, sol, [0.2 0.3], options);
asserts.istrue(abs(continuedSol.y(1, 1) - 1) < 1e-8);
asserts.istrue(abs(continuedSol.y(1, length(continuedSol.x)) - 1) < 1e-8);
%=============================================================================
eventOptions = ddeset('Events', @ddeTestEventThreshold, 'MaxStep', 0.02);
eventSol = dde23(@(t, y, z) - y, 0.1, 1, [0 1], eventOptions);
asserts.istrue(isfield(eventSol, 'xe'));
asserts.istrue(abs(eventSol.xe(1) + log(0.9)) < 2e-2);
asserts.isequal(eventSol.ie(1), 1);
%=============================================================================
stateDelay = @(t, y) t - 0.05 + 0 * y(1);
stateSol = ddesd(@(t, y, z) - y + z, stateDelay, 1, [0 0.2], options);
asserts.isequal(stateSol.solver, 'ddesd');
asserts.istrue(abs(stateSol.y(1, length(stateSol.x)) - 1) < 1e-8);
%=============================================================================
neutralDelay = @(t, y) t - 0.05 + 0 * y(1);
neutralSol = ddensd(@(t, y, z, zp) - y + z + 0 * zp, neutralDelay, neutralDelay, ...
  1, [0 0.2], options);
asserts.isequal(neutralSol.solver, 'ddensd');
asserts.istrue(abs(neutralSol.y(1, length(neutralSol.x)) - 1) < 1e-8);
asserts.istrue(abs(neutralSol.yp(1, length(neutralSol.x))) < 1e-6);
neutralCellSol = ddensd(@(t, y, z, zp) - y + z + 0 * zp, neutralDelay, neutralDelay, ...
  {1, 0}, [0 0.2], options);
asserts.istrue(abs(neutralCellSol.y(1, 1) - 1) < 1e-8);
asserts.istrue(abs(neutralCellSol.yp(1, 1)) < 1e-8);
%=============================================================================
