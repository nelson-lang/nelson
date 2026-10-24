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
%=============================================================================
% combinatorialLogic used to hit the unknownTypes error. The vector
% expansion now keeps ONE block whose input elements become scalar ports
% (MSB-first, the runtime's bit-fold order) and the truth table is baked as
% a constant array. XOR table [0 1 1 0] over (clock >= 0.2, clock >= 0.4):
% y = 0 (t < 0.2), 1 (0.2 <= t < 0.4), 0 (t >= 0.4).
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'ca', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, ...
    'params', struct('relop', 'ge', 'const', 0.2)), ...
  struct('id', 'cb', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, ...
    'params', struct('relop', 'ge', 'const', 0.4)), ...
  struct('id', 'm', 'type', 'mux', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 'cl', 'type', 'combinatorialLogic', 'inputs', 1, 'outputs', 1, ...
    'params', struct('TruthTable', [0 1 1 0])), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfcl_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'clk', 'to', 'ca', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'clk', 'to', 'cb', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ca', 'to', 'm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cb', 'to', 'm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'm', 'to', 'cl', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cl', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cl', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.6;
d.variables = struct();
%=============================================================================
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nfcl_y');
assert_isequal(ysim(1), 0);
assert_isequal(ysim(4), 1); % t = 0.2 (rows start at t = 0)
assert_isequal(ysim(6), 0); % t = 0.4
evalin('base', 'clear(''nfcl_y'')');
%=============================================================================
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, 'cl_tbl_cl[4]')));
assert_istrue(isempty(strfind(src, 'unknown:')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'cl_tbl: [f64; 4]')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence vs the simulator (both targets).
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nfcl_num', 'C', {'y'}, {'nfcl_y'}, 0, 0);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfcl_num', 'rust', {'y'}, {'nfcl_y'}, 0, 0);
  end
end
%=============================================================================
