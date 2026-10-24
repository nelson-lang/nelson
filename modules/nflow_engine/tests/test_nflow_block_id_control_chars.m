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
% Regression: the result JSON escaped only the quote and the backslash, so a
% block id holding a control character came back unescaped and the reader threw
% the whole result away - after a run that had otherwise succeeded. Diagram
% editors wrap long labels across lines, so a block id carrying a newline is
% completely ordinary, and every such diagram was unusable.
%=============================================================================
nl = sprintf('\n');
tabc = sprintf('\t');
weird = ['Two', nl, 'Line "Gain"', tabc, 'x\y'];

doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'block_id_control_chars';
doc.runtime = 0.05;
doc.sampleTime = 0.01;
doc.blocks = {...
  struct('id', weird, 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 3)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_weird', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', weird, 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model);
asserts.isequal(out.y_weird(end), 3);
rmfile(model);
%=============================================================================
% The same id in a block the engine names back in an error: the diagnostic must
% still come back as a readable result rather than as a decode failure.
doc.blocks{1}.type = 'no_such_block_type';
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
msg = '';
id = '';
try
  sim(model2);
catch e
  msg = e.message;
  id = e.identifier;
end
asserts.isequal(id, 'Nelson:nflow_engine:simFailed');
asserts.istrue(~isempty(strfind(msg, 'no_such_block_type')));
rmfile(model2);
%=============================================================================
