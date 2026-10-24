%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Typed workspace bridge: complex signals flow through fromWorkspace /
% nelsonFunction / toWorkspace, exact-64 signals log exactly.
%=============================================================================
% Complex round-trip: a complex workspace series drives the diagram, a
% nelsonFunction conjugates it, toWorkspace stores a complex variable.
simin = struct('time', (0:0.01:0.05)', 'signals', struct('values', complex(3, 4) * ones(6, 1)));
assignin('base', 'simin', simin);
d = struct();
d.blocks = {struct('id', 'src', 'type', 'fromWorkspace', 'inputs', 0, 'outputs', 1, 'params', struct('VariableName', 'simin')), ...
  struct('id', 'fn', 'type', 'nelsonFunction', 'inputs', 1, 'outputs', 1, 'params', struct('Fcn', 'conj')), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'simout_c', 'SaveFormat', 'Structure With Time')), ...
  struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
d.connections = {struct('from', 'src', 'to', 'fn', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'fn', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'fn', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; d.duration = 0.05; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
% conj(3+4i) = 3-4i on the wire
assert_isequal(r.signals.fn, 3);
assert_isequal(r.signalsImag.fn, -4);
% and the logged workspace variable is complex
so = evalin('base', 'simout_c');
assert_istrue(~isreal(so.signals.values));
assert_isequal(real(so.signals.values(end)), 3);
assert_isequal(imag(so.signals.values(end)), -4);
%=============================================================================
% Exact uint64 logging: UINT64_MAX survives toWorkspace exactly.
dj = ['{"blocks":[', ...
  '{"id":"c","type":"constant","inputs":0,"outputs":1,"params":{"Value":18446744073709551615,"OutDataType":"uint64"}},', ...
  '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"simout_u64","SaveFormat":"Array"}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[', ...
  '{"from":"c","to":"tw","fromIndex":0,"toIndex":0},', ...
  '{"from":"c","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"sampleTime":0.01,"duration":0.05,"variables":{}}'];
ru = jsondecode(__nflow_simulate__(dj));
assert_isequal(ru.status, 'done');
su = evalin('base', 'simout_u64');
assert_isequal(class(su), 'uint64');
assert_isequal(su(end), uint64(18446744073709551615));
%=============================================================================
