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
% The integrator had no way to be reset from the diagram: the only way back to
% an initial condition was to wrap it in a resettable subsystem. It now takes
% an ExternalReset - rising, falling, either, or a level that holds it - and an
% InitialConditionSource that can be a wired signal rather than a parameter.
%
% The extra ports follow the signal in that order, so port 1 is the reset when
% there is one and the initial condition otherwise. Absent, the block is
% exactly what it always was.
%=============================================================================
function d = int_doc(resetKind, solver, extraBlocks, extraConns, icParam)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'integrator_reset';
  d.runtime = 3.0;
  d.sampleTime = 0.01;
  if ~isempty(solver)
    d.solver = solver;
  end
  p = struct('InitialCondition', icParam);
  if ~isempty(resetKind)
    p.ExternalReset = resetKind;
  end
  blocks = {...
    struct('id', 'u', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', 1)), ...
    struct('id', 'i', 'type', 'integrator', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', p), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 640, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_ir', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  for k = 1:numel(extraBlocks)
    blocks{end + 1} = extraBlocks{k};
  end
  d.blocks = blocks;
  conns = {...
    struct('from', 'u', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'i', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  for k = 1:numel(extraConns)
    conns{end + 1} = extraConns{k};
  end
  d.connections = conns;
end
function y = int_run(d)
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  out = sim(model);
  rmfile(model);
  y = [out.tout(:), out.y_ir(:)];
  evalin('base', 'clear(''y_ir'')');
end
function v = at(y, tt)
  k = find(abs(y(:, 1) - tt) < 1e-9, 1);
  v = y(k, 2);
end
%=============================================================================
% A level reset holds the state at its initial condition for as long as the
% line is up. The pulse is up on [1, 2) and [3, ...), so a constant 1
% integrates to 1 by t = 1, is pinned to 0 through the second, then climbs
% again from zero.
pulseLevel = {struct('id', 'r', 'type', 'pulse', 'x', 240, 'y', 260, ...
  'rotation', 0, 'params', struct('Amplitude', 1, 'Period', 2, 'Width', 50, ...
  'StartTime', 1, 'Offset', 0))};
connLevel = {struct('from', 'r', 'to', 'i', 'fromIndex', 0, 'toIndex', 1)};
for solver = {'', 'ode4'}
  y = int_run(int_doc('level', solver{1}, pulseLevel, connLevel, 0));
  assert_isapprox(at(y, 0.99), 0.99, 1e-9, 1e-9);
  % the sample that raises the line already reads the initial condition
  assert_istrue(abs(at(y, 1.0)) < 1e-12);
  assert_istrue(abs(at(y, 1.5)) < 1e-12);
  assert_istrue(abs(at(y, 1.99)) < 1e-12);
  % and it climbs again from zero once the line drops
  assert_isapprox(at(y, 2.5), 0.5, 1e-9, 1e-9);
  assert_isapprox(at(y, 2.99), 0.99, 1e-9, 1e-9);
end
%=============================================================================
% A rising-edge reset snaps the state back once, on the edge, and integrates
% on. Edges at t = 1 and t = 2 (the line is already up at t = 0, which is a
% level and not an edge).
pulseEdge = {struct('id', 'r', 'type', 'pulse', 'x', 240, 'y', 260, ...
  'rotation', 0, 'params', struct('Amplitude', 1, 'Period', 1, 'Width', 50, ...
  'StartTime', 0.5, 'Offset', 0))};
for solver = {'', 'ode4'}
  y = int_run(int_doc('rising', solver{1}, pulseEdge, connLevel, 0));
  assert_isapprox(at(y, 0.49), 0.49, 1e-9, 1e-9);
  assert_istrue(abs(at(y, 0.5)) < 1e-12); % edge
  assert_isapprox(at(y, 1.0), 0.5, 1e-9, 1e-9); % integrating again
  assert_istrue(abs(at(y, 1.5)) < 1e-12); % next edge
end
%=============================================================================
% A falling-edge reset answers on the other side of the same pulse.
for solver = {'', 'ode4'}
  y = int_run(int_doc('falling', solver{1}, pulseEdge, connLevel, 0));
  assert_isapprox(at(y, 0.99), 0.99, 1e-9, 1e-9);
  assert_istrue(abs(at(y, 1.0)) < 1e-12);
  assert_isapprox(at(y, 1.5), 0.5, 1e-9, 1e-9);
end
%=============================================================================
% The initial condition can be a wired signal instead of a parameter: with a
% reset present it is the third port. The reset then returns the state to 4.7.
icBlocks = {pulseEdge{1}, struct('id', 'c0', 'type', 'constant', 'x', 240, ...
  'y', 340, 'rotation', 0, 'params', struct('Value', 4.7))};
icConns = {connLevel{1}, ...
  struct('from', 'c0', 'to', 'i', 'fromIndex', 0, 'toIndex', 2)};
dic = int_doc('rising', 'ode4', icBlocks, icConns, 0);
dic.blocks{2}.params.InitialConditionSource = 'external';
y = int_run(dic);
assert_isapprox(at(y, 0.5), 4.7, 1e-9, 1e-9);
assert_isapprox(at(y, 1.0), 4.7 + 0.5, 1e-9, 1e-9);
assert_isapprox(at(y, 1.5), 4.7, 1e-9, 1e-9);
%=============================================================================
% Code generation has no lowering for a discrete event on a continuous state,
% and says so rather than emitting an integrator that never resets.
dcg = int_doc('rising', 'ode4', pulseEdge, connLevel, 0);
f = [tempname(), '.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(dcg));
fclose(fid);
msg = '';
try
  nflow_codegenerate(f, tempdir(), false, 'C');
catch e
  msg = e.message;
end
assert_istrue(~isempty(strfind(msg, 'external')));
assert_istrue(~isempty(strfind(msg, 'reset')));
rmfile(f);
%=============================================================================
% Without ExternalReset the block has one port and integrates as it always
% has, wherever it runs.
for solver = {'', 'ode4'}
  y = int_run(int_doc('', solver{1}, {}, {}, 0));
  assert_isapprox(at(y, 1.0), 1.0, 1e-9, 1e-9);
  assert_isapprox(at(y, 3.0), 3.0, 1e-9, 1e-9);
end
%=============================================================================
