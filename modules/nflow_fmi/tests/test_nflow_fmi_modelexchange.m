%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Model Exchange import: Nelson integrates the FMU with its own RK4 solver.
%=============================================================================
assert_istrue(exist('fmiModelExchange') > 0);
%=============================================================================
% Error path: missing path.
missing_fmu = [tempname(), '.fmu'];
assert_checkerror('fmiModelExchange(missing_fmu, 1.0)', ...
  _(['FMU path is neither a directory nor an existing file: ', missing_fmu]));
%=============================================================================
% Optional relative tolerance (arg 4, A3.2 site 4) is accepted by the wrapper: a
% 4-arg call still fails at the path check, not with a "wrong number of
% arguments" error -- proving the tolerance argument widened the signature.
assert_checkerror('fmiModelExchange(missing_fmu, 1.0, 0.01, 1e-6)', ...
  _(['FMU path is neither a directory nor an existing file: ', missing_fmu]));
%=============================================================================
exdir = [modulepath('nflow_fmi', 'root'), '/examples'];
vdp = [exdir, '/VanDerPol.fmu'];
bb = [exdir, '/BouncingBall.fmu'];
skip_testsuite(~isfile(vdp), ['VanDerPol.fmu not found: ', vdp]);
skip_testsuite(nflow_fmi_skip_no_toolchain(), ...
  'no C toolchain (compiler/cmake): bundled reference FMU cannot be built for this platform');
%=============================================================================
% Van der Pol (2 continuous states, no events): limit-cycle amplitude ~= 2.
r = fmiModelExchange(vdp, 20, 0.01);
assert_istrue(isstruct(r));
assert_isequal(r.time(1), 0);
col = find(strcmp(r.outputNames, 'x0'));
x0 = r.outputs(:, col);
assert_istrue(all(isfinite(x0)));
assert_istrue((max(x0) - min(x0)) > 1);
assert_istrue(max(abs(x0)) < 3);
%=============================================================================
% Bouncing ball (state events): the ball bounces and never goes far below 0.
if isfile(bb)
  rb = fmiModelExchange(bb, 3, 0.005);
  h = rb.outputs(:, find(strcmp(rb.outputNames, 'h')));
  v = rb.outputs(:, find(strcmp(rb.outputNames, 'v')));
  assert_istrue(all(h > -0.05));
  assert_istrue(max(h) > 0.5);
  % at least a few bounces (velocity sign flips) over 3 s.
  assert_istrue(sum(abs(diff(sign(v))) > 0) >= 3);
end
%=============================================================================
