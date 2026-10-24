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
% Numeric equivalence of GENERATED code vs the interpreter, part 1 of 3:
% vector expansion. This family compiles and runs the generated program
% through the local toolchain and compares trajectories sample-by-sample,
% C AND Rust.
% (Split from test_nflow_codegen_numeric_families to keep each file well under
% the per-file timeout under parallel load; the lowering and islands families
% live in the sibling _lowering / _islands files.)
%=============================================================================
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'C toolchain (cmake) not available');
haveRust = __nflow_codegen_toolchain__('rust');
%=============================================================================
% Family 1 -- vectors: [1 2 3] * 2 through demux, recombined by a 3-input
% sum. Pure constants: the generated trajectories must be exact.
d = struct();
d.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2 3])), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 'dm', 'type', 'demux', 'inputs', 1, 'outputs', 3, 'params', struct()), ...
  struct('id', 's', 'type', 'sum', 'inputs', 3, 'outputs', 1, 'params', struct('Inputs', '+++')), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfvec_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'dm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dm', 'to', 's', 'fromIndex', 1, 'toIndex', 1), ...
  struct('from', 'dm', 'to', 's', 'fromIndex', 2, 'toIndex', 2), ...
  struct('from', 's', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.variables = struct();
nflow_codegen_check_numeric(d, 'nffam_vec', 'C', {'y'}, {'nfvec_y'}, 0, 0);
if haveRust
  nflow_codegen_check_numeric(d, 'nffam_vec', 'rust', {'y'}, {'nfvec_y'}, 0, 0);
end
%=============================================================================
