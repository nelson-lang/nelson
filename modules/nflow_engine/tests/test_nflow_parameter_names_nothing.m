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
% Regression: a numeric parameter holding text that names nothing - a variable
% the model never defines, or a typo for one it does - read back as its default
% and the run went on to simulate something that was not the model. A mistyped
% gain produced a flat zero and said nothing about it. The engine now names the
% block and the parameter instead.
%=============================================================================
function d = local_doc(gainValue, variables)
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'param_names_nothing';
  d.runtime = 0.3;
  d.sampleTime = 0.1;
  d.blocks = {...
    struct('id', 'k', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', 5)), ...
    struct('id', 'g', 'type', 'gain', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', struct('Gain', gainValue)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_par', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'k', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  if ~isempty(variables)
    d.variables = variables;
  end
end
function msg = local_run(d)
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  msg = '';
  try
    sim(model);
  catch e
    msg = e.message;
  end
  rmfile(model);
end
%=============================================================================
% A gain naming a variable the model never defines: refused, and the message
% says which block and which parameter.
msg = local_run(local_doc('kp', []));
asserts.istrue(~isempty(strfind(msg, 'Gain')));
asserts.istrue(~isempty(strfind(msg, 'kp')));
asserts.istrue(~isempty(strfind(msg, 'gain')));
%=============================================================================
% A typo for a variable that does exist is refused just the same.
msg = local_run(local_doc('kpp', struct('kp', 3)));
asserts.istrue(~isempty(strfind(msg, 'kpp')));
%=============================================================================
% The variable itself, and an expression over it, are values: they run.
asserts.istrue(isempty(local_run(local_doc('kp', struct('kp', 3)))));
y = evalin('base', 'y_par');
asserts.isapprox(y(1), 15, 1e-12, 1e-12);
evalin('base', 'clear(''y_par'')');

asserts.istrue(isempty(local_run(local_doc('2*kp', struct('kp', 3)))));
y2 = evalin('base', 'y_par');
asserts.isapprox(y2(1), 30, 1e-12, 1e-12);
evalin('base', 'clear(''y_par'')');
%=============================================================================
% A plain number is untouched by any of this, and so is a parameter the block
% library declares as text: 'exp' is a mathFunction operator, not a variable.
asserts.istrue(isempty(local_run(local_doc(4, []))));
evalin('base', 'clear(''y_par'')');
%=============================================================================
% One level down is no better: the same typo inside a subsystem is refused, and
% the same diagram with the variable it means runs.
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'gi', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 'kpp')), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'gi', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'gi', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};

ds = struct();
ds.version = '1.0.0';
ds.format = 'nflow-2';
ds.name = 'param_names_nothing_inner';
ds.runtime = 0.3;
ds.sampleTime = 0.1;
ds.variables = struct('kp', 3);
ds.blocks = {...
  struct('id', 'k', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 5)), ...
  struct('id', 's', 'type', 'subsystem', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('name', 's', 'subsystem', inner)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_par', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
ds.connections = {...
  struct('from', 'k', 'to', 's', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 's', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

msg = local_run(ds);
asserts.istrue(~isempty(strfind(msg, 'kpp')));

ds.blocks{2}.params.subsystem.blocks{2}.params.Gain = 'kp';
asserts.istrue(isempty(local_run(ds)));
yi = evalin('base', 'y_par');
asserts.isapprox(yi(1), 15, 1e-12, 1e-12);
evalin('base', 'clear(''y_par'')');
%=============================================================================
