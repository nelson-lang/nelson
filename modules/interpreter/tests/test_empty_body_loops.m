%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Non-regression: empty-body 'for' and 'while' loops must not be elided by
% the parser. An empty-body loop still iterates and evaluates its condition
% (or advances its loop variable) on every pass.
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% empty-body 'for' still iterates and leaves the loop variable at its last value
for i = 1:5
end
asserts.isequal(i, 5);
%=============================================================================
% empty-body 'for' over a row vector
last = -1;
for k = [2 4 6 8]
end
asserts.isequal(k, 8);
%=============================================================================
% empty-body 'while' with a false condition terminates immediately
executed = true;
while false
end
asserts.istrue(executed);
%=============================================================================
% empty-body 'while' must re-evaluate its condition each pass (not be elided).
% The condition function decrements a shared counter; if the loop were elided
% the counter would keep its initial value.
global TEST_EMPTY_WHILE_COUNTER
TEST_EMPTY_WHILE_COUNTER = 4;
while local_step_condition()
end
asserts.isequal(TEST_EMPTY_WHILE_COUNTER, 0);
%=============================================================================
% empty-body 'while true' with a break in an outer guard still works when the
% body is not empty (sanity: the fix must not affect non-empty loops)
c = 0;
while true
  c = c + 1;
  if c >= 3
    break;
  end
end
asserts.isequal(c, 3);
%=============================================================================
% nested empty-body loops
total = 0;
for a = 1:3
  for b = 1:2
  end
end
asserts.isequal(a, 3);
asserts.isequal(b, 2);
%=============================================================================
function tf = local_step_condition()
  global TEST_EMPTY_WHILE_COUNTER
  tf = TEST_EMPTY_WHILE_COUNTER > 0;
  if tf
    TEST_EMPTY_WHILE_COUNTER = TEST_EMPTY_WHILE_COUNTER - 1;
  end
end
%=============================================================================
