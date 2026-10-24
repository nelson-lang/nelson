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
% width and assignment used to hit the unknownTypes error (no template, no
% lowering). The vector expansion now lowers them: width becomes a constant
% carrying the input element count (only known at expansion time), and
% assignment resolves to element rewiring (1-based Indices, replacement port
% over the base signal, first matching index wins -- the runtime rule).
%=============================================================================
% width: |[1 2 3]| = 3, folded into the generated source as a constant.
d = struct();
d.blocks = {...
  struct('id', 'v', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', [1 2 3])), ...
  struct('id', 'w', 'type', 'width', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfrl_w', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'v', 'to', 'w', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'w', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'w', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.2;
d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ws = evalin('base', 'nfrl_w');
assert_istrue(all(ws(:) == 3));
evalin('base', 'clear(''nfrl_w'')');
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(~isempty(strfind(src, 'out_w = 3;')));
assert_istrue(isempty(strfind(src, 'unknown:')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, 'out_w = 3')));
rmfile(f);
%=============================================================================
% assignment: base [1 2 3], Indices = 2, replacement 9 -> y = [1 9 3] in the
% simulation; the generated code wires element 1 straight to the replacement.
d2 = struct();
d2.blocks = {...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', [1 2 3])), ...
  struct('id', 'rp', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 9)), ...
  struct('id', 'asg', 'type', 'assignment', 'inputs', 2, 'outputs', 1, ...
    'params', struct('Indices', 2)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfrl_a', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {...
  struct('from', 'b', 'to', 'asg', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'rp', 'to', 'asg', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'asg', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'asg', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1;
d2.duration = 0.2;
d2.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r.status, 'done');
av = evalin('base', 'nfrl_a');
assert_isequal(av(end, :), [1 9 3]);
evalin('base', 'clear(''nfrl_a'')');
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d2));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(isempty(strfind(src, 'unknown:')));
% Element 1 of the sink reads the replacement constant directly.
assert_istrue(~isempty(strfind(src, '= out_rp;')));
rmfile(f);
%=============================================================================
