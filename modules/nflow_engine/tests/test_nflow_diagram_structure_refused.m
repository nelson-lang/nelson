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
% Regression: a wire naming a block that does not exist was dropped without a
% word, and two blocks sharing an id left whichever one happened to win. The
% run was quietly missing a connection, or fed by the wrong block. Both are now
% named. A negative port index is refused too; a POSITIVE one out of range is
% not, because port counts are otherwise inferred from the wires - a
% hand-written model usually omits them - so only a count the model states
% itself can be contradicted.
%=============================================================================
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
function d = local_doc(declarePorts)
  k = struct('id', 'k', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 1));
  g = struct('id', 'g', 'type', 'gain', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('Gain', 2));
  w = struct('id', 'w', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_str', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'));
  if declarePorts
    k.inputs = 0; k.outputs = 1;
    g.inputs = 1; g.outputs = 1;
    w.inputs = 1; w.outputs = 0;
  end
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'diagram_structure';
  d.runtime = 0.3;
  d.sampleTime = 0.1;
  d.blocks = {k, g, w};
  d.connections = {...
    struct('from', 'k', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
end
%=============================================================================
% A wire whose destination, or source, names no block.
d = local_doc(false);
d.connections{1}.to = 'nosuchblock';
msg = local_run(d);
asserts.istrue(~isempty(strfind(msg, 'nosuchblock')));

d = local_doc(false);
d.connections{1}.from = 'nosuchblock';
asserts.istrue(~isempty(strfind(local_run(d), 'nosuchblock')));
%=============================================================================
% Two blocks answering to the same id.
d = local_doc(false);
d.blocks{2}.id = 'k';
asserts.istrue(~isempty(strfind(local_run(d), 'share the id')));
%=============================================================================
% A negative port index makes no sense whichever end it is on.
d = local_doc(false);
d.connections{1}.toIndex = -5;
asserts.istrue(~isempty(strfind(local_run(d), 'port -5')));

d = local_doc(false);
d.connections{2}.fromIndex = -1;
asserts.istrue(~isempty(strfind(local_run(d), 'port -1')));
%=============================================================================
% A port beyond a count the model STATES is a contradiction in the file.
d = local_doc(true);
d.connections{1}.toIndex = 9;
msg = local_run(d);
asserts.istrue(~isempty(strfind(msg, 'input port 9')));
asserts.istrue(~isempty(strfind(msg, 'declares 1')));
%=============================================================================
% The same wire in a model that states no counts keeps the inference that
% hand-written diagrams rely on: the port is created, not refused.
d = local_doc(false);
d.connections{1}.toIndex = 9;
asserts.istrue(isempty(local_run(d)));
evalin('base', 'clear(''y_str'')');
%=============================================================================
% An inner diagram answers to the same rules.
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'gi', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
    'params', struct('Gain', 2)), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'gi', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'nosuchinner', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
d = local_doc(false);
d.blocks{2} = struct('id', 'g', 'type', 'subsystem', 'x', 240, 'y', 100, ...
  'rotation', 0, 'params', struct('name', 'g', 'subsystem', inner));
msg = local_run(d);
asserts.istrue(~isempty(strfind(msg, 'nosuchinner')));
asserts.istrue(~isempty(strfind(msg, 'subsystem')));
%=============================================================================
% The diagram as written still runs.
asserts.istrue(isempty(local_run(local_doc(false))));
evalin('base', 'clear(''y_str'')');
asserts.istrue(isempty(local_run(local_doc(true))));
evalin('base', 'clear(''y_str'')');
%=============================================================================
