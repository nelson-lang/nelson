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
% FMU export: external inputs drive stateful blocks.
% input -> integrator -> output, driven by fmiCoSimulate with a nonzero
% constant: the compiled FMU integrates the input (y = u*t exactly, the
% forward-Euler update is exact for a constant input).
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
% u -> integrator -> y, u held at 3.0: y(t) = 3 t at every sample.
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'u', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.variables = struct();
mf = [tempname(), '.nflow'];
fid = fopen(mf, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);
fmu = NFlow.exportfmu(mf, tempdir());
co = tempname();
if isdir(co); rmdir(co, 's'); end
names = unzip(fmu, co);
hasBinary = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBinary, 'no C toolchain: FMU exported source-only, cannot load');
r = fmiCoSimulate(fmu, 1.0, 0.1, struct('u', 3.0));
assert_istrue(numel(r.time) >= 11);
assert_istrue(max(abs(r.outputs - 3.0 * r.time)) < 1e-9);
%=============================================================================
