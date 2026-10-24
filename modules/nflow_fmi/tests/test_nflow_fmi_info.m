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
% Public .m API (fmiInfo / fmiCoSimulate); resolution and error paths always
% run (no network / FMU needed); the real FMU run is skipped when absent.
%=============================================================================
assert_istrue(exist('fmiInfo') > 0);
assert_istrue(exist('fmiCoSimulate') > 0);
%=============================================================================
% A path that is neither a directory nor a file is rejected by the wrapper.
missing_fmu = [tempname(), '.fmu'];
assert_checkerror('fmiInfo(missing_fmu)', ...
  _(['FMU path is neither a directory nor an existing file: ', missing_fmu]));
%=============================================================================
% fmiCoSimulate forwards to the same guard.
assert_checkerror('fmiCoSimulate(missing_fmu, 1.0)', ...
  _(['FMU path is neither a directory nor an existing file: ', missing_fmu]));
%=============================================================================
% Real FMU when available: fmiInfo reads metadata, fmiCoSimulate runs.
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
% fmiInfo accepts the .fmu archive directly (extraction handled internally).
info = fmiInfo(fmu);
assert_istrue(isstruct(info));
assert_istrue(isfield(info, 'coSimulation'));
assert_istrue(info.coSimulation);
assert_istrue(numel(info.names) >= 1);
%=============================================================================
% fmiCoSimulate runs a Co-Simulation straight from the .fmu archive.
r = fmiCoSimulate(fmu, 4.0, 0.01);
assert_istrue(isstruct(r));
assert_isequal(r.time(1), 0);
assert_istrue(all(isfinite(r.outputs(:))));
assert_istrue(max(max(r.outputs, [], 1) - min(r.outputs, [], 1)) > 0.1);
%=============================================================================
% The JSON inspection served by the nflow.fmi3.inspect RPC (via the same
% library helper) must parse and describe the FMU's variables.
j = jsondecode(__nflow_fmi_inspect_json__(fmu));
assert_istrue(j.success);
assert_isequal(j.fmiVersion, '3.0');
assert_istrue(j.coSimulation);
assert_istrue(numel(j.variables) >= 1);
je = jsondecode(__nflow_fmi_inspect_json__([tempname(), '.fmu']));
assert_isfalse(je.success);
%=============================================================================
