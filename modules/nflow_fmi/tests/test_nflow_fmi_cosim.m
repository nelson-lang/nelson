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
% Wiring / error-path checks (always run, no network or FMU needed).
%=============================================================================
assert_istrue(__nflow_fmi_available__());
%=============================================================================
% A non-FMU directory must fail cleanly (structured error, no crash).
try
  __nflow_fmi_cosim__(tempname(), 1.0);
  error('NELSON:expectedError', 'expected a failure');
catch e
  assert_istrue(contains(e.message, 'failed to load FMU'));
end
%=============================================================================
% tStop must be a positive scalar.
assert_checkerror('__nflow_fmi_cosim__(tempdir(), -1.0)', ...
  _('tStop must be a positive scalar.'));
assert_checkerror('__nflow_fmi_cosim__(tempdir(), -1.0)', ...
  _('tStop must be a positive scalar.'), 'Nelson:nflow_fmi:badInputs:tStop');
assert_checkerror('__nflow_fmi_modelexchange__(tempdir(), -1.0)', ...
  _('tStop must be a positive scalar.'), 'Nelson:nflow_fmi:badInputs:tStop');
%=============================================================================
% Optional relative tolerance (arg 6, A3.2 site 3) is accepted: a 6-arg call to
% a non-FMU directory fails at FMU load, not with a "wrong number of arguments"
% error -- proving the tolerance argument widened the signature.
try
  __nflow_fmi_cosim__(tempname(), 1.0, 0.01, {}, [], 1e-4);
  error('NELSON:expectedError', 'expected a failure');
catch e
  assert_istrue(contains(e.message, 'failed to load FMU'));
end
%=============================================================================
% Full Co-Simulation run against a Reference FMU, when one is available.
% Point NFLOW_FMI_REFERENCE_FMUS_DIR at a folder containing VanDerPol.fmu
% (FMI 3.0 Co-Simulation). Skipped cleanly when the FMU is not present so
% the suite stays green without network access.
%=============================================================================
fmu = [modulepath('nflow_fmi', 'root'), '/examples/VanDerPol.fmu'];
if ~isfile(fmu)
  ref_dir = getenv('NFLOW_FMI_REFERENCE_FMUS_DIR');
  if ~isempty(ref_dir)
    fmu = [ref_dir, '/VanDerPol.fmu'];
  end
end
skip_testsuite(~isfile(fmu), ['VanDerPol.fmu not found: ', fmu]);
skip_testsuite(nflow_fmi_skip_no_toolchain(), ...
  'no C toolchain (compiler/cmake): bundled reference FMU cannot be built for this platform');
%=============================================================================
staging = tempname();
if isdir(staging)
  rmdir(staging, 's');
end
mkdir(staging);
unzip(fmu, staging);
r = __nflow_fmi_cosim__(staging, 4.0, 0.01);
%=============================================================================
% Structure shape.
assert_istrue(isstruct(r));
assert_istrue(isfield(r, 'time'));
assert_istrue(isfield(r, 'outputNames'));
assert_istrue(isfield(r, 'outputs'));
%=============================================================================
% time is a strictly increasing column starting at 0.
assert_isequal(size(r.time, 2), 1);
assert_isequal(r.time(1), 0);
assert_istrue(all(diff(r.time) > 0));
%=============================================================================
% Outputs are finite and shaped to match the recorded time / names.
assert_isequal(numel(r.outputNames), size(r.outputs, 2));
assert_isequal(size(r.outputs, 1), numel(r.time));
assert_istrue(all(isfinite(r.outputs(:))));
%=============================================================================
% The oscillator must actually move (not a frozen output).
assert_istrue(max(max(r.outputs, [], 1) - min(r.outputs, [], 1)) > 0.1);
% Note: a second standalone run on the same unzipped FMU is intentionally not
% exercised here -- re-loading the same modelIdentifier DLL in-process is the
% known Windows FMI limitation (the diagram path uses nflowMakeDllPathUnique).
% The relTol wiring is covered by the 6-arg acceptance check above.
%=============================================================================
rmdir(staging, 's');
%=============================================================================
