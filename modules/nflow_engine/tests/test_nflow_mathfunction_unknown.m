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
% Regression: a Function name mathFunction does not implement fell through to an
% identity fallback, so the block became a plain wire and answered with its own
% input - a wrong result with nothing to see. Its siblings (trigFunction,
% roundingFunction, sqrt) fall back to a real default operation; 'Math Function'
% has none to fall back on, so an unknown name is reported.
%=============================================================================
doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'mathfunction_unknown';
doc.runtime = 0.05;
doc.sampleTime = 0.01;
doc.blocks = {...
  struct('id', 'c', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
    'params', struct('Value', 3)), ...
  struct('id', 'f', 'type', 'mathFunction', 'x', 240, 'y', 100, 'rotation', 0, ...
    'params', struct('Function', 'no_such_function')), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
    'params', struct('VariableName', 'y_mf', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'c', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'f', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
msg = '';
try
  sim(model);
catch e
  msg = e.message;
end
asserts.istrue(~isempty(strfind(msg, 'mathFunction')));
asserts.istrue(~isempty(strfind(msg, 'no_such_function')));
rmfile(model);
%=============================================================================
% 'magnitude^2' is what diagrams from other tools carry for 'square'; it is
% accepted and squares its input rather than passing it through.
doc.blocks{2}.params.Function = 'magnitude^2';
model2 = [tempname(), '.nflow'];
fid = fopen(model2, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out = sim(model2);
asserts.isapprox(out.y_mf(end), 9, 0, 1e-9);
rmfile(model2);
%=============================================================================
% The names the block does implement keep working, single and two input.
doc.blocks{2}.params.Function = 'reciprocal';
model3 = [tempname(), '.nflow'];
fid = fopen(model3, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);
out3 = sim(model3);
asserts.isapprox(out3.y_mf(end), 1 / 3, 1e-9, 1e-9);
rmfile(model3);
%=============================================================================
