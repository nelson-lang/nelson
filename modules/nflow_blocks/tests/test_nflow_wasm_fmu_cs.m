%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Browser integration fixture: the Worker registers a real component first.
fmuPath = getenv('NELSON_WASM_FMU_PATH');
skip_testsuite(~iswasm() || isempty(fmuPath), ...
  'Requires a WebAssembly Worker with a registered FMI-LS-WASM component.');
d = struct('solver', 'discrete', 'duration', 0.5, 'sampleTime', 0.1, ...
  'variables', struct());
d.blocks = {...
  struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 3)), ...
  struct('id', 'fmu', 'type', 'fmu', 'inputs', 2, 'outputs', 1, ...
    'params', struct('path', fmuPath, 'inputNames', {{'input_a', 'input_b'}}, 'outputNames', {{'sum'}})), ...
  struct('id', 'scope', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {...
  struct('from', 'a', 'fromIndex', 0, 'to', 'fmu', 'toIndex', 0), ...
  struct('from', 'b', 'fromIndex', 0, 'to', 'fmu', 'toIndex', 1), ...
  struct('from', 'fmu', 'fromIndex', 0, 'to', 'scope', 'toIndex', 0)};
for runIndex = 1:2
  result = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(result.status, 'done');
  y = result.scopes(1).series;
  if iscell(y)
    y = cell2mat(y);
  end
  y = y(:);
  assert_istrue(numel(y) >= 5);
  assert_istrue(all(abs(y(2:end) - 5) < 1e-12));
end
d.blocks{3}.params.inputNames = {'unknown_input', 'input_b'};
result = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(result.status, 'error');
assert_istrue(contains(result.reason, 'Unknown FMU variable'));
