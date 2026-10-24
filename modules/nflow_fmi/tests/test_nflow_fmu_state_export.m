%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% An exported Co-Simulation FMU must support FMU-state get/set so it can be
% rolled back (iterative co-simulation coupling, step rejection). The generated
% wrapper declares canGetAndSetFMUState and implements fmi3Get/Set/FreeFMUState
% over the flat instance struct. Round-trip: step, save, step, restore, step must
% reproduce the outputs exactly.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
% A model with state: input u -> integrator -> output y.
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
  struct('id', 'i', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'u', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01;
d.duration = 1.0;
d.variables = struct();
f = [work, filesep, 'M.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
fmu = NFlow.exportfmu(f, [work, filesep, 'out']);
names = unzip(fmu, [work, filesep, 'chk']);
hasBin = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBin, 'no C toolchain: FMU exported source-only');
%=============================================================================
% The exported FMU reports canGetAndSetFMUState and round-trips its state.
info = fmiInfo(fmu);
assert_isequal(info.fmiVersion, '3.0');
r = __nflow_fmi_state_roundtrip__(fmu, 0.05);
assert_istrue(r.supported); % declares canGetAndSetFMUState
assert_istrue(r.match); % save -> step -> restore -> step reproduces exactly
%=============================================================================
