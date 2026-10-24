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
% <--WEIGHT 64-->
% <--TIMEOUT 600-->
%=============================================================================
% enableTrigger: an edge that counts only while an enable level is high. Two
% control wires into one subsystem - the enable on the port after the data
% inputs, the edge on the one after that - which is why the synthetic gate this
% lowers to carries TWO inputs where every other kind carries one. The edge
% memory is the same as a plain trigger's, latched once per step against the
% SECOND wire.
%
% The body counts its fires, so the trace says exactly which edges were
% honored: an enable of period 2 s over an edge of period 0.5 s honors the
% edges of the first second of each period and drops the rest.
%=============================================================================
function d = et_doc(varName, triggerType)
  inner = struct();
  inner.blocks = {...
    struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
      'params', struct('label', 'in1')), ...
    struct('id', 'dl', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, ...
      'params', struct('InitialCondition', 0)), ...
    struct('id', 'ad', 'type', 'sum', 'inputs', 2, 'outputs', 1, ...
      'params', struct('Inputs', '++')), ...
    struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('label', 'out1'))};
  inner.connections = {...
    struct('from', 'in1', 'to', 'ad', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'dl', 'to', 'ad', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 'ad', 'to', 'dl', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ad', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
  inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
  inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
  inner.controlPort = struct('kind', 'enableTrigger');
  d = struct();
  d.blocks = {...
    struct('id', 'u', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', 1)), ...
    struct('id', 'e', 'type', 'pulse', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Amplitude', 1, 'Period', 2.0, 'Width', 50.0, ...
        'StartTime', 0, 'Offset', 0)), ...
    struct('id', 't', 'type', 'pulse', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Amplitude', 1, 'Period', 0.5, 'Width', 50.0, ...
        'StartTime', 0, 'Offset', 0)), ...
    struct('id', 's', 'type', 'subsystem', 'inputs', 3, 'outputs', 1, ...
      'params', struct('subsystem', inner, ...
        'controlPort', struct('kind', 'enableTrigger', 'triggerType', triggerType))), ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', varName, 'SaveFormat', 'Array', ...
        'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'e', 'to', 's', 'fromIndex', 0, 'toIndex', 1), ...
    struct('from', 't', 'to', 's', 'fromIndex', 0, 'toIndex', 2), ...
    struct('from', 's', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.01;
  d.duration = 4.0;
  d.variables = struct();
end
%=============================================================================
dR = et_doc('nfet_r', 'rising');
rR = jsondecode(__nflow_simulate__(jsonencode(dR)));
assert_isequal(rR.status, 'done');
yR = evalin('base', 'nfet_r');
assert_istrue(all(isfinite(yR)));
% It counts, and it stops counting: the enable is low for half of every 2 s.
assert_istrue(yR(end) > 2);
assert_istrue(any(diff(yR) == 0));
% Never more fires than there are edges in the whole run.
assert_istrue(yR(end) <= 4.0 / 0.5);
evalin('base', 'clear(''nfet_r'')');
%=============================================================================
fR = [tempname(), '.nflow'];
[~, stemR] = fileparts(fR);
fid = fopen(fR, 'wt');
fwrite(fid, jsonencode(dR));
fclose(fid);
nflow_codegenerate(fR, tempdir(), false, 'C');
srcR = fileread([tempdir(), stemR, '/model.c']);
assert_istrue(isempty(strfind(srcR, 'unknown:')));
% The gate reads both wires: the enable level and the edge memory.
assert_istrue(~isempty(strfind(srcR, 's->nfgp_s__nfg')));
assert_istrue(~isempty(strfind(srcR, 'out_e) > 0.0')));
rmfile(fR);
%=============================================================================
% The three edge conventions, each compared with the simulation.
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dR, 'nfet_rn', 'C', {'y'}, {'nfet_r'}, 5e-6, 1e-9);
  dF = et_doc('nfet_f', 'falling');
  nflow_codegen_check_numeric(dF, 'nfet_fn', 'C', {'y'}, {'nfet_f'}, 5e-6, 1e-9);
  dE = et_doc('nfet_e', 'either');
  nflow_codegen_check_numeric(dE, 'nfet_en', 'C', {'y'}, {'nfet_e'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dR, 'nfet_rn', 'rust', {'y'}, {'nfet_r'}, 5e-6, 1e-9);
    nflow_codegen_check_numeric(dE, 'nfet_en', 'rust', {'y'}, {'nfet_e'}, 5e-6, 1e-9);
  end
end
%=============================================================================
% Under an RK solver it lowers as well. A subsystem gated on an EDGE cannot
% hold a continuous state at all - the engine refuses one - so its members are
% discrete by construction and the RK seam has nothing of theirs to freeze.
dS = et_doc('nfet_s', 'rising');
dS.solver = 'ode4';
rS = jsondecode(__nflow_simulate__(jsonencode(dS)));
assert_isequal(rS.status, 'done');
evalin('base', 'clear(''nfet_s'')');
fS = [tempname(), '.nflow'];
[~, stemS] = fileparts(fS);
fid = fopen(fS, 'wt');
fwrite(fid, jsonencode(dS));
fclose(fid);
nflow_codegenerate(fS, tempdir(), false, 'C');
srcS = fileread([tempdir(), stemS, '/model.c']);
assert_istrue(isempty(strfind(srcS, 'unknown:')));
rmfile(fS);
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dS, 'nfet_sn', 'C', {'y'}, {'nfet_s'}, 5e-6, 1e-9);
end
%=============================================================================
% A CONTINUOUS state inside it is refused, by the engine itself: an edge-gated
% subsystem runs only at its edges and has nothing to integrate between them.
dC = et_doc('nfet_c', 'rising');
dC.blocks{4}.params.subsystem.blocks{2} = struct('id', 'dl', 'type', 'integrator', ...
  'inputs', 1, 'outputs', 1, 'params', struct('InitialCondition', 0));
dC.blocks{4}.params.subsystem.connections = {...
  struct('from', 'in1', 'to', 'dl', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dl', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
rC = jsondecode(__nflow_simulate__(jsonencode(dC)));
assert_isequal(rC.status, 'error');
assert_istrue(~isempty(strfind(lower(rC.reason), 'continuous')));
%=============================================================================
