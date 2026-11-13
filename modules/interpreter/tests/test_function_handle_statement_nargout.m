%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% A statement call on a variable holding a function handle asks for no output,
% inside a function too. Calls with local-variable arguments (f(k, n);) and
% chained calls (c{k}(x);) used to ask for one, so a target without outputs
% failed with "Too many output arguments".
%=============================================================================
global HANDLE_NARGOUT
WORK = [tempdir(), 'handle_stmt_', createGUID()];
mkdir(WORK);
fid = fopen([WORK, '/handleNargoutRecord.m'], 'w');
fprintf(fid, 'function varargout = handleNargoutRecord(varargin)\n');
fprintf(fid, '  global HANDLE_NARGOUT\n');
fprintf(fid, '  HANDLE_NARGOUT(end + 1) = nargout;\n');
fprintf(fid, '  varargout = num2cell(1:nargout);\n');
fprintf(fid, 'end\n');
fclose(fid);
fid = fopen([WORK, '/handleNoOutput.m'], 'w');
fprintf(fid, 'function handleNoOutput(a, b)\n');
fprintf(fid, '  global HANDLE_NARGOUT\n');
fprintf(fid, '  HANDLE_NARGOUT(end + 1) = nargout;\n');
fprintf(fid, 'end\n');
fclose(fid);
fid = fopen([WORK, '/handleStatementCalls.m'], 'w');
fprintf(fid, 'function handleStatementCalls(options, n)\n');
fprintf(fid, '  if nargin < 2\n');
fprintf(fid, '    n = 3;\n');
fprintf(fid, '  end\n');
fprintf(fid, '  f = options.Callback;\n');
fprintf(fid, '  c = {f};\n');
fprintf(fid, '  s.t.f = f;\n');
fprintf(fid, '  f(1, 2);\n');
fprintf(fid, '  for k = 1:1\n');
fprintf(fid, '    f(k, n);\n');
fprintf(fid, '    f(k);\n');
fprintf(fid, '    f(k + 1);\n');
fprintf(fid, '    f(k - n);\n');
fprintf(fid, '    c{k}(k, n);\n');
fprintf(fid, '    c{1}(1, 2);\n');
fprintf(fid, '    options.Callback(k, n);\n');
fprintf(fid, '    s.t.f(k, n);\n');
fprintf(fid, '    feval(f, k, n);\n');
fprintf(fid, '  end\n');
fprintf(fid, 'end\n');
fclose(fid);
fid = fopen([WORK, '/handleValueCalls.m'], 'w');
fprintf(fid, 'function [y, a, b, z] = handleValueCalls(f)\n');
fprintf(fid, '  k = 1;\n');
fprintf(fid, '  n = 2;\n');
fprintf(fid, '  y = f(k, n);\n');
fprintf(fid, '  [a, b] = f();\n');
fprintf(fid, '  c = {f};\n');
fprintf(fid, '  z = c{k}(k, n);\n');
fprintf(fid, 'end\n');
fclose(fid);
fid = fopen([WORK, '/handleAnonymousCalls.m'], 'w');
fprintf(fid, 'function r = handleAnonymousCalls()\n');
fprintf(fid, '  g = @(x) x + 1;\n');
fprintf(fid, '  k = 2;\n');
fprintf(fid, '  g(k);\n');
fprintf(fid, '  r = g(k);\n');
fprintf(fid, 'end\n');
fclose(fid);
addpath(WORK);
%=============================================================================
% Every statement call asks for no output.
HANDLE_NARGOUT = [];
handleStatementCalls(struct('Callback', @handleNargoutRecord));
assert_isequal(HANDLE_NARGOUT, zeros(1, 10));
%=============================================================================
% A target without outputs runs.
HANDLE_NARGOUT = [];
handleStatementCalls(struct('Callback', @handleNoOutput), 4);
assert_isequal(HANDLE_NARGOUT, zeros(1, 10));
%=============================================================================
% Assignments still ask for their outputs.
HANDLE_NARGOUT = [];
[y, a, b, z] = handleValueCalls(@handleNargoutRecord);
assert_isequal(HANDLE_NARGOUT, [1 2 1]);
assert_isequal(y, 1);
assert_isequal([a, b], [1 2]);
assert_isequal(z, 1);
%=============================================================================
% Anonymous handles.
assert_isequal(handleAnonymousCalls(), 3);
%=============================================================================
% Without a semicolon, the call displays ans.
h = @(x) 2 * x;
k = 1;
clear ans
R = evalc('h(k)');
assert_istrue(contains(R, 'ans ='));
assert_isequal(ans, 2);
%=============================================================================
% Script level.
f = @handleNargoutRecord;
HANDLE_NARGOUT = [];
f(k, 2);
c = {f};
c{k}(k, 2);
assert_isequal(HANDLE_NARGOUT, [0 0]);
%=============================================================================
rmpath(WORK);
rmdir(WORK, 's');
clear global HANDLE_NARGOUT
%=============================================================================
