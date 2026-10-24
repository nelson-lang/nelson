%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Vector code generation: the vector -> scalar diagram expansion must simulate
% identically to the vector diagram, and code generation must emit per-element
% scalar variables.
%=============================================================================
% Expansion equivalence: constant [1 2 3] -> gain 2 -> scope.
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2 3])), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();

expanded = __nflow_expand_vector__(jsonencode(d));
exj = jsondecode(expanded);
assert_isfalse(isfield(exj, 'error'));
re = jsondecode(__nflow_simulate__(expanded));
assert_isequal(re.status, 'done');
% Each element of the vector gain is a distinct scalar block g__e<k>.
assert_isequal(nflow_test_sig(re, 'g__e0'), 2);
assert_isequal(nflow_test_sig(re, 'g__e1'), 4);
assert_isequal(nflow_test_sig(re, 'g__e2'), 6);
%=============================================================================
% Code generation of the vector diagram succeeds and emits per-element vars.
outdir = [tempdir, filesep, 'nflow_vec_codegen_test'];
if isdir(outdir)
  rmdir(outdir, 's');
end
mkdir(outdir);
d.blocks{end + 1} = struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 0, 'params', struct('GotoTag', 'y'));
d.connections{end + 1} = struct('from', 'g', 'to', 'y', 'fromIndex', 0, 'toIndex', 0);
model = [outdir, filesep, 'vec.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(model, outdir, false, 'C');
gen = fileread([outdir, filesep, 'vec', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen, 'out_g__e0')));
assert_istrue(~isempty(strfind(gen, 'out_g__e2')));
assert_istrue(~isempty(strfind(gen, 'y_e0')));
%=============================================================================
% matmul (element-wise): [1 2] .* [3 4] = [3 8]; expansion emits scalar mult
% blocks and simulates identically to the vector matmul.
d2 = struct();
d2.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2])), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [3 4])), ...
  struct('id', 'mm', 'type', 'matmul', 'inputs', 2, 'outputs', 1, 'params', struct('MultiplicationRule', 'elementwise')), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d2.connections = {struct('from', 'a', 'to', 'mm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'mm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'mm', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.01; d2.duration = 0.05; d2.variables = struct();
rv2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(nflow_test_sig(rv2, 'mm'), [3 8]);
ex2 = __nflow_expand_vector__(jsonencode(d2));
assert_isfalse(isfield(jsondecode(ex2), 'error'));
re2 = jsondecode(__nflow_simulate__(ex2));
assert_isequal(nflow_test_sig(re2, 'mm__mme0'), 3);
assert_isequal(nflow_test_sig(re2, 'mm__mme1'), 8);
%=============================================================================
% matmul (matrix rule) with a vector operand: A[2x2] * x[2x1] -> y[2x1].
% A = [1 3; 2 4] (col-major reshape of [1 2 3 4]), x = [5; 6], y = [23; 34].
% Expansion lowers each output element to K mult blocks feeding a sum, and
% code generation emits the per-element scalar variables.
d3 = struct();
d3.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2 3 4])), ...
  struct('id', 'ra', 'type', 'reshape', 'inputs', 1, 'outputs', 1, 'params', struct('OutputDimensions', [2 2])), ...
  struct('id', 'x', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [5 6])), ...
  struct('id', 'mm', 'type', 'matmul', 'inputs', 2, 'outputs', 1, 'params', struct('MultiplicationRule', 'matrix')), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d3.connections = {struct('from', 'a', 'to', 'ra', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ra', 'to', 'mm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'x', 'to', 'mm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'mm', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.01; d3.duration = 0.05; d3.variables = struct();
rv3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(nflow_test_sig(rv3, 'mm'), [23 34]);
ex3 = __nflow_expand_vector__(jsonencode(d3));
assert_isfalse(isfield(jsondecode(ex3), 'error'));
re3 = jsondecode(__nflow_simulate__(ex3));
assert_isequal(nflow_test_sig(re3, 'mm__mme0'), 23);
assert_isequal(nflow_test_sig(re3, 'mm__mme1'), 34);
model3 = [outdir, filesep, 'mmvec.nflow'];
fid = fopen(model3, 'wt');
fwrite(fid, jsonencode(d3));
fclose(fid);
nflow_codegenerate(model3, outdir, false, 'C');
gen3 = fileread([outdir, filesep, 'mmvec', filesep, 'model.c']);
assert_istrue(~isempty(strfind(gen3, 'mm__mme0')));
assert_istrue(~isempty(strfind(gen3, 'mm__mme1')));
%=============================================================================
function v = nflow_test_sig(r, id)
  v = r.signals.(id);
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:)';
end
%=============================================================================

