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
% single and the small integer types (int8..uint32) used to be REJECTED by
% code generation (and before that, silently emitted as plain double). The
% generators now mirror the simulator: computation stays in the double lane
% and every small-typed output port is quantized right after its producing
% block (NaN -> 0, round to nearest, then saturate or wrap modulo the type
% span; single is a float round-trip). Reference values from the simulator:
% int8 100*2 saturates to 127, wraps to -56.
%=============================================================================
function d = small_int_model(saturate)
  d = struct();
  d.blocks = {...
    struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', 100, 'OutDataType', 'int8')), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Gain', 2, 'SaturateOnOverflow', saturate)), ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'nfsi_y', 'SaveFormat', 'Array', ...
        'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1;
  d.duration = 0.3;
  d.variables = struct();
end
%=============================================================================
% Saturating: 100 * 2 -> 127 in the simulation AND in the emitted source.
d = small_int_model(true);
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nfsi_y');
assert_istrue(all(ysim(:) == 127));
evalin('base', 'clear(''nfsi_y'')');
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, 'nflow_quant_i(')));
assert_istrue(~isempty(strfind(src, ', -128, 127, 1);')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'fn nflow_quant_i(')));
assert_istrue(~isempty(strfind(srcR, ', -128.0_f64, 127.0_f64, true);')));
rmfile(f);
%=============================================================================
% Wrapping (SaturateOnOverflow false): 100 * 2 -> -56.
d = small_int_model(false);
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nfsi_y');
assert_istrue(all(ysim(:) == -56));
evalin('base', 'clear(''nfsi_y'')');
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, ', -128, 127, 0);')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence (both targets) through the shared harness.
if __nflow_codegen_toolchain__('C')
  d = small_int_model(true);
  nflow_codegen_check_numeric(d, 'nfsmallint_num', 'C', {'y'}, {'nfsi_y'}, 0, 0);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfsmallint_num', 'rust', {'y'}, {'nfsi_y'}, 0, 0);
  end
end
%=============================================================================
