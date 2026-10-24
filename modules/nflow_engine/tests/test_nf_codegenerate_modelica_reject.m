%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Modelica blocks are expanded to FMU blocks before code generation. The
% generated program cannot embed that runtime, so code generation must reject
% the expanded diagram instead of emitting a constant-0.0 output.
%=============================================================================
model = [tempname(), '.nflow'];
json = ['{', ...
  '"version":"1.0.0","format":"nflow-2","name":"ModelicaReject",', ...
  '"blocks":[', ...
  '{"id":"b1","type":"modelica","x":100,"y":100,"rotation":0,', ...
  '"params":{"source":"model M output Real y; equation y=1; end M;","modelName":"M"}},', ...
  '{"id":"b2","type":"scope","x":300,"y":100,"rotation":0,"params":{}}', ...
  '],', ...
  '"connections":[{"from":"b1","fromPort":0,"to":"b2","toPort":0}]', ...
  '}'];
fid = fopen(model, 'wt');
fwrite(fid, json);
fclose(fid);
%=============================================================================
expected = _(['Code generation is not supported for Modelica blocks; ', ...
  'they are compiled to FMUs for simulation, and generated standalone ', ...
  'code cannot embed the FMU runtime. Remove Modelica blocks before ', ...
  'generating code.']);
cmd = ['nflow_codegenerate(''', model, ''', tempdir(), false, ''C'')'];
assert_checkerror(cmd, expected);
assert_checkerror(cmd, expected, 'Nelson:nflow_engine:codegenUnsupported:codeGenerationModelicaBlocks');
%=============================================================================
cmd = ['nflow_codegenerate(''', model, ''', tempdir(), false, ''rust'')'];
assert_checkerror(cmd, expected);
%=============================================================================
rmfile(model);
%=============================================================================
