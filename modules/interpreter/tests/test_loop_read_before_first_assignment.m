%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
%=============================================================================
% A read compiled before the first assignment of a variable in the same code
% runs again after that assignment when a loop jumps back: it must see the new
% value. The counter n stops a loop that would otherwise never end.
%=============================================================================
v = 0;
n = 0;
eval('while v < 2, v = v + 1; n = n + 1; if n > 10, break; end; end');
asserts.isequal(v, 2);
asserts.isequal(n, 2);
%=============================================================================
v = 0;
n = 0;
eval(sprintf('while v < 2\n v = v + 1;\n n = n + 1;\n if n > 10, break; end\nend'));
asserts.isequal(v, 2);
%=============================================================================
v = 0;
n = 0;
eval('while (v < 2), v = v + 1; n = n + 1; if n > 10, break; end; end');
asserts.isequal(v, 2);
%=============================================================================
s = struct('n', 0);
n = 0;
eval('while s.n < 2, s.n = s.n + 1; n = n + 1; if n > 10, break; end; end');
asserts.isequal(s.n, 2);
%=============================================================================
x = [0 0];
n = 0;
eval('while x(1) < 2, x(1) = x(1) + 1; n = n + 1; if n > 10, break; end; end');
asserts.isequal(x, [2 0]);
%=============================================================================
v = 5;
n = 0;
eval('while v < 7, v = v + 1; n = n + 1; if n > 10, break; end; end');
asserts.isequal(v, 7);
%=============================================================================
v = 0;
eval('for k = 1:3, v = v + k; end');
asserts.isequal(v, 6);
%=============================================================================
d = [tempdir(), 'test_loop_read_before_first_assignment'];
if ~isdir(d)
  mkdir(d);
end
fid = fopen([d, '/loopReadAssignedLaterScript.m'], 'wt');
fprintf(fid, '%s\n', 'while w < 2, w = w + 1; n = n + 1; if n > 10, break; end; end');
fclose(fid);
fid = fopen([d, '/loopReadAssignedLater.m'], 'wt');
fprintf(fid, '%s\n', 'function r = loopReadAssignedLater()', ...
  'r = [];', ...
  'for k = 1:3', ...
  '  if k > 1', ...
  '    r(end + 1) = v;', ...
  '  end', ...
  '  v = 10 * k;', ...
  'end', ...
  'end');
fclose(fid);
fid = fopen([d, '/loopEvalCallerVariable.m'], 'wt');
fprintf(fid, '%s\n', 'function r = loopEvalCallerVariable()', ...
  'v = 0;', ...
  'n = 0;', ...
  'eval(''while v < 2, v = v + 1; n = n + 1; if n > 10, break; end; end'');', ...
  'r = v;', ...
  'end');
fclose(fid);
addpath(d);
%=============================================================================
w = 0;
n = 0;
loopReadAssignedLaterScript;
asserts.isequal(w, 2);
%=============================================================================
asserts.isequal(loopReadAssignedLater(), [10 20]);
%=============================================================================
asserts.isequal(loopEvalCallerVariable(), 2);
%=============================================================================
rmpath(d);
%=============================================================================
