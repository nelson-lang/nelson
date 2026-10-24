%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Exact int64/uint64 signals through the simulation engine.
% Diagrams carrying values above 2^53 are written as raw JSON text: a
% jsonencode round-trip through double would already lose the exactness the
% engine is being tested for.
%=============================================================================
% int64 vector through gain: type propagates, values exact.
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [1 2 3], 'OutDataType', 'int64')), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 3)), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'c', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
assert_isequal(r.signalTypes.c{1}, 'int64');
assert_isequal(r.signalTypes.g{1}, 'int64');
assert_isequal(nflow_test_sig(r, 'g'), [3 6 9]);
%=============================================================================
% uint64 saturation at UINT64_MAX: a value no double can represent. The
% raw result JSON must carry the exact digits.
dj = ['{"blocks":[', ...
  '{"id":"a","type":"constant","inputs":0,"outputs":1,"params":{"Value":18446744073709551615,"OutDataType":"uint64"}},', ...
  '{"id":"b","type":"constant","inputs":0,"outputs":1,"params":{"Value":10,"OutDataType":"uint64"}},', ...
  '{"id":"s1","type":"sum","inputs":2,"outputs":1,"params":{"SaturateOnOverflow":true}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"a","to":"s1","fromIndex":0,"toIndex":0},', ...
  '{"from":"b","to":"s1","fromIndex":0,"toIndex":1},', ...
  '{"from":"s1","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
raw = __nflow_simulate__(dj);
assert_istrue(~isempty(strfind(raw, '18446744073709551615')));
r2 = jsondecode(raw);
assert_isequal(r2.status, 'done');
assert_isequal(r2.signalTypes.s1{1}, 'uint64');
%=============================================================================
% uint64 wrap: UINT64_MAX + 10 wraps to 9 exactly.
dj3 = strrep(dj, '"SaturateOnOverflow":true', '"SaturateOnOverflow":false');
r3 = jsondecode(__nflow_simulate__(dj3));
assert_isequal(r3.status, 'done');
assert_isequal(nflow_test_sig(r3, 's1'), 9);
%=============================================================================
% Exact comparison above 2^53: INT64_MAX vs INT64_MAX-1 differ (their double
% projections collapse to the same value).
dj4 = ['{"blocks":[', ...
  '{"id":"a","type":"constant","inputs":0,"outputs":1,"params":{"Value":9223372036854775807,"OutDataType":"int64"}},', ...
  '{"id":"b","type":"constant","inputs":0,"outputs":1,"params":{"Value":9223372036854775806,"OutDataType":"int64"}},', ...
  '{"id":"ne1","type":"relationalOperator","inputs":2,"outputs":1,"params":{"Operator":"ne"}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"a","to":"ne1","fromIndex":0,"toIndex":0},', ...
  '{"from":"b","to":"ne1","fromIndex":0,"toIndex":1},', ...
  '{"from":"ne1","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
r4 = jsondecode(__nflow_simulate__(dj4));
assert_isequal(r4.status, 'done');
assert_isequal(nflow_test_sig(r4, 'ne1'), 1);
%=============================================================================
% An exact-64 signal into a block without an exact path is a clean error.
d5 = struct();
d5.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1, 'OutDataType', 'uint64')), ...
  struct('id', 'u', 'type', 'unitDelay', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d5.connections = {struct('from', 'c', 'to', 'u', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'u', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d5.sampleTime = 0.01; d5.duration = 0.05; d5.variables = struct();
r5 = jsondecode(__nflow_simulate__(jsonencode(d5)));
assert_isequal(r5.status, 'error');
assert_istrue(~isempty(strfind(r5.reason, 'uint64')));
assert_istrue(~isempty(strfind(r5.reason, 'convert')));
%=============================================================================
% convert casts double -> int64 and back; saturation applies on the way in.
d6 = struct();
d6.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1e19)), ...
  struct('id', 'cv', 'type', 'convert', 'inputs', 1, 'outputs', 1, 'params', struct('OutDataType', 'int64')), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d6.connections = {struct('from', 'c', 'to', 'cv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'cv', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d6.sampleTime = 0.01; d6.duration = 0.05; d6.variables = struct();
raw6 = __nflow_simulate__(jsonencode(d6));
assert_istrue(~isempty(strfind(raw6, '9223372036854775807')));
r6 = jsondecode(raw6);
assert_isequal(r6.status, 'done');
assert_isequal(r6.signalTypes.cv{1}, 'int64');
%=============================================================================
% Integer division rounds floor by default: 7/2 = 3, -7/2 = -4.
d7 = struct();
d7.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [7 -7], 'OutDataType', 'int64')), ...
  struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [2 2], 'OutDataType', 'int64')), ...
  struct('id', 'dv', 'type', 'divide', 'inputs', 2, 'outputs', 1, 'params', struct()), ...
  struct('id', 's', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d7.connections = {struct('from', 'a', 'to', 'dv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'b', 'to', 'dv', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'dv', 'to', 's', 'fromIndex', 0, 'toIndex', 0)};
d7.sampleTime = 0.01; d7.duration = 0.05; d7.variables = struct();
r7 = jsondecode(__nflow_simulate__(jsonencode(d7)));
assert_isequal(r7.status, 'done');
assert_isequal(nflow_test_sig(r7, 'dv'), [3 -4]);
%=============================================================================
function v = nflow_test_sig(r, id)
  v = r.signals.(id);
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:)';
end
%=============================================================================
