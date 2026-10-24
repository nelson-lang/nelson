%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% FMI 2.0 Model Exchange (F3): NFlow owns the solver and drives the FMU's
% derivatives / event indicators / discrete-update loop through the same
% abstraction as FMI 3.0. Validated on the Reference FMUs (BSD-2): the Van der
% Pol limit cycle and the bouncing ball (which exercises the event loop).
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
skip_testsuite(nflow_fmi_skip_bundled_fmi2_runtime(), ...
  'bundled FMI 2.0 reference FMUs only ship x86-64 binaries');
exdir = [modulepath('nflow_fmi', 'root'), '/examples'];
%=============================================================================
% Van der Pol (mu = 1) settles onto its limit cycle with amplitude ~2.0.
r = fmiModelExchange([exdir, '/VanDerPol_fmi2.fmu'], 20, 0.01);
x0 = r.outputs(:, 1);
% Use the second half, once the transient has decayed.
tail = x0(round(end / 2):end);
assert_istrue(max(tail) > 1.9 && max(tail) < 2.1);
assert_istrue(min(tail) < -1.9 && min(tail) > -2.1);
%=============================================================================
% Bouncing ball: height starts at 1, and the event loop (state resets at each
% bounce) must keep it from going meaningfully negative.
rb = fmiModelExchange([exdir, '/BouncingBall_fmi2.fmu'], 3, 0.005);
h = rb.outputs(:, 1);
assert_istrue(max(h) > 0.9 && max(h) <= 1.0 + 1e-6);
assert_istrue(min(h) > -1e-3);
% The ball loses energy: late peaks are lower than the initial drop height.
assert_istrue(h(end) < 0.5);
%=============================================================================
