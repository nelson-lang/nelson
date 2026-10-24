%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelsonFunction block: named function, anonymous function, expression using
% u, vector output, and error propagation. The block calls the interpreter at
% every step through the workspace bridge (installed by __nflow_simulate__).
%=============================================================================
% Named function: sin(0.5).
r = run_nf('sin', '-1', 0.5);
asserts.isequal(r.status, 'done');
asserts.isapprox(nf_last_scalar(r), sin(0.5), 0, 1e-10);
%=============================================================================
% Anonymous function of u.
r = run_nf('@(u) 2*u + 1', '-1', 3);
asserts.isequal(r.status, 'done');
asserts.isapprox(nf_last_scalar(r), 7, 0, 1e-10);
%=============================================================================
% Expression using u.
r = run_nf('u^2', '-1', 4);
asserts.isequal(r.status, 'done');
asserts.isapprox(nf_last_scalar(r), 16, 0, 1e-10);
%=============================================================================
% Vector output: input [1 2 3], function @(u) u*10, OutputDimensions inherited.
r = run_nf('@(u) u*10', '-1', [1 2 3]);
asserts.isequal(r.status, 'done');
y = r.scopes(1).series;
if iscell(y)
  finalCol = cellfun(@(s) s(end), y);
else
  finalCol = y(:, end);
end
asserts.isapprox(finalCol(:)', [10 20 30], 0, 1e-10);
%=============================================================================
% Error: the user function raises -> the simulation fails with its message.
fid = fopen([tempdir, filesep, 'nf_boom.m'], 'w');
fprintf(fid, 'function y = nf_boom(u)\nerror(''nf:boom'', ''intentional'');\nend\n');
fclose(fid);
addpath(tempdir);
r = run_nf('nf_boom', '-1', 1);
asserts.isequal(r.status, 'error');
asserts.istrue(~isempty(strfind(r.reason, 'intentional')));
%=============================================================================
% Error: width mismatch (function returns 2 elements, OutputDimensions=1).
r = run_nf('@(u) [u; u]', '1', 5);
asserts.isequal(r.status, 'error');
asserts.istrue(~isempty(strfind(r.reason, 'width')));
%=============================================================================
function r = run_nf(fcn, outputDims, constValue)
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
      'params', struct('Value', constValue)), ...
    struct('id', 'f', 'type', 'nelsonFunction', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Fcn', fcn, 'OutputDimensions', outputDims)), ...
    struct('id', 'sc', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())};
  d.connections = {struct('from', 'c', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'f', 'to', 'sc', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
end
%=============================================================================
function v = nf_last_scalar(r)
  y = r.scopes(1).series; if iscell(y), y = y{1}; else, y = y(1, :); end
  v = y(end);
end
%=============================================================================
