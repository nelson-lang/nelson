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
% FMU state snapshot / rollback (rollback -> iterative co-simulation
% prerequisite). An FMU that declares canGetAndSetFMUState can save its full
% internal state, advance, then be rolled back and re-advanced to the identical
% result. Verified on the bundled Reference FMUs (BSD-2), both FMI 3.0 and 2.0.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
exdir = [modulepath('nflow_fmi', 'root'), '/examples'];
%=============================================================================
% One FMU per FMI version is enough to cover both save/restore paths; loading
% more same-model FMUs here only piles process-global FMU state onto the shared
% test-runner process without adding coverage.
%=============================================================================
% FMI 3.0 Co-Simulation.
r = __nflow_fmi_state_roundtrip__([exdir, '/Dahlquist.fmu'], 0.05);
assert_istrue(r.supported); % the Reference FMU declares canGetAndSetFMUState
assert_istrue(r.match); % restore -> identical re-step
%=============================================================================
% FMI 2.0 Co-Simulation uses the same version-neutral save/restore path.
if ~nflow_fmi_skip_bundled_fmi2_runtime()
  r = __nflow_fmi_state_roundtrip__([exdir, '/Dahlquist_fmi2.fmu'], 0.05);
  assert_istrue(r.supported);
  assert_istrue(r.match);
end
%=============================================================================
