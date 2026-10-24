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
% nflow_codegenerate must run the same acausal pre-pass as simulation before it
% reaches the native C/Rust generators. A raw physicalWires document must not
% fall through to an "unknown" zero-output emission.
%=============================================================================
workC = tempname();
mkdir(workC);
workR = tempname();
mkdir(workR);
%=============================================================================
d = struct();
d.name = 'acausal_codegen';
d.solver = 'ode4';
d.sampleTime = 0.1;
d.runtime = 1.0;
d.blocks = {...
  struct('id', 'V1', 'type', 'ConstantVoltage', 'params', struct('V', 1)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 1)), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'Y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {struct('from', 'VS', 'to', 'Y', 'fromIndex', 0, 'toIndex', 0)};
d.physicalWires = {...
  struct('pins', {{'V1.p', 'R1.p', 'VS.p'}}), ...
  struct('pins', {{'V1.n', 'R1.n', 'VS.n', 'G.p'}})};
model = [workC, filesep, 'acausal_codegen.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
%=============================================================================
cgout = evalc('nflow_codegenerate(model, workC, false, ''C'')');
assert_istrue(isempty(strfind(cgout, 'not implemented')));
generated = [workC, filesep, 'acausal_codegen', filesep, 'model.c'];
assert_istrue(isfile(generated));
txt = fileread(generated);
assert_istrue(isempty(strfind(txt, 'unknown:')));
assert_istrue(isempty(strfind(txt, 'physicalWires')));
%=============================================================================
modelR = [workR, filesep, 'acausal_codegen.nflow'];
fid = fopen(modelR, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
rgout = evalc('nflow_codegenerate(modelR, workR, false, ''rust'')');
assert_istrue(isempty(strfind(rgout, 'not implemented')));
generatedR = [workR, filesep, 'acausal_codegen', filesep, 'src', filesep, 'lib.rs'];
assert_istrue(isfile(generatedR));
rtxt = fileread(generatedR);
assert_istrue(isempty(strfind(rtxt, 'unknown:')));
assert_istrue(isempty(strfind(rtxt, 'physicalWires')));
%=============================================================================
rmdir(workC, 's');
rmdir(workR, 's');
%=============================================================================
