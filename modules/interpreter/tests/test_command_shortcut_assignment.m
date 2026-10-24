%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: a variable whose name collides with a path-command
% shortcut ('ls', 'cd', 'dir') must be treated as an assignment, not as a
% command, whatever the spacing around '='. Previously "dir = c{1}" was
% misparsed as the 'dir' command; inside a loop the resulting error recovery
% dereferenced the parser's uninitialized 'error' token value and crashed the
% interpreter (EXCEPTION_ACCESS_VIOLATION).
%=============================================================================
% assignment with spaces on both sides, cell-index RHS
c = {40, 41};
dir = c{1};
assert_isequal(dir, 40);
%=============================================================================
% all spacings around '=' must assign, not run the 'dir' command
dir = 1;
assert_isequal(dir, 1);
dir = 2;
assert_isequal(dir, 2);
dir = 3;
assert_isequal(dir, 3);
%=============================================================================
% ls / cd names too, with paren-index RHS
v = [10 20 30];
ls = v(2);
assert_isequal(ls, 20);
cd = v(3);
assert_isequal(cd, 30);
%=============================================================================
% builtin-named variable used on both sides of an expression
dir = dir + 1;
assert_isequal(dir, 4);
%=============================================================================
% the exact crashing pattern: builtin-named var assigned inside a for loop
% from an indexed expression.
cases = {5, 7, 13};
for k = 1:numel(cases)
  c = cases{k};
  dir = c;
  assert_isequal(dir, cases{k});
end
%=============================================================================
% comparison ('==') must remain an expression, not a command
dir = 4;
assert_istrue(dir == 4);
%=============================================================================
% Hardening: an unterminated block must raise a catchable error instead of
% crashing the interpreter (the parser error productions no longer dereference
% the uninitialized 'error' token semantic value).
assert_checkerror('eval(sprintf(''for k=1:2\n x=1;''))', ...
    'Expecting ''end'' to match ''for'' statement from line 1');
%=============================================================================
R = false;
try
  eval(sprintf('while true\n x=1;'));
catch
  R = true;
end
assert_istrue(R);
%=============================================================================
R = false;
try
  eval(sprintf('if 1\n x=1;'));
catch
  R = true;
end
assert_istrue(R);
%=============================================================================
