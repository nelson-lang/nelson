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
% Functions whose body only computes on its locals are called directly with
% any number of inputs and outputs: same results, same errors, and errors
% raised in them name the called function.
%=============================================================================
assert_isequal(call_two_outputs(3), [4 2 4 2 4 2]);
assert_isequal(call_two_inputs(3), [3 12 27]);
assert_isequal(call_first_output_only(3), [2 3 4]);
assert_isequal(call_three_of_three(), {1, [2 2], 'c'});
assert_isequal(call_varargout(), 7);
%=============================================================================
% errors keep their identifier, message and called function
% the frame is the one a call through feval reports
e = error_of(@() call_unassigned_second());
assert_isequal(e.identifier, 'Nelson:interpreter:outputNotAssigned');
assert_isequal(e.message, 'Output argument ''b'' is not assigned during call to ''only_a''.');
assert_isequal(e.stack(1).name, generic_frame(@() feval_two_outputs('only_a')));
assert_istrue(endsWith(e.stack(1).name, 'only_a'));
e = error_of(@() call_unassigned_single());
assert_isequal(e.message, 'Output argument ''y'' is not assigned during call to ''no_output_set''.');
assert_isequal(e.stack(1).name, generic_frame(@() feval_one_output('no_output_set', 1)));
e = error_of(@() call_body_error());
assert_isequal(e.identifier, 'Nelson:dimagree:sizeMismatchArgumentsArithmeticOperatorValue');
assert_isequal(e.stack(1).name, generic_frame(@() feval('add_two', [1 2], [1 2 3])));
assert_istrue(endsWith(e.stack(1).name, 'add_two'));
e = error_of(@() call_too_many_inputs());
assert_isequal(e.identifier, 'Nelson:tooManyInputs');
e = error_of(@() call_too_many_outputs());
assert_isequal(e.identifier, 'Nelson:tooManyOutputs');
%=============================================================================
function r = call_two_outputs(n)
  r = [];
  for k = 1:n
    [a, b] = two_outs(n);
    r = [r, a, b]; %#ok<NLS0039>
  end
end
%=============================================================================
function r = call_two_inputs(n)
  r = zeros(1, n);
  for k = 1:n
    r(k) = scale(k, 3 * k);
  end
end
%=============================================================================
function r = call_first_output_only(n)
  r = zeros(1, n);
  for k = 1:n
    r(k) = two_outs(k);
  end
end
%=============================================================================
function r = call_three_of_three()
  r = {};
  for k = 1:2
    [a, b, c] = three_outs(1);
    r = {a, b, c};
  end
end
%=============================================================================
function r = call_varargout()
  r = 0;
  for k = 1:2
    [r, q] = with_varargout(3, 4); %#ok<NLS0004>
  end
end
%=============================================================================
function r = call_unassigned_second()
  r = 0;
  for k = 1:2
    [a, b] = only_a(k);
    r = r + a + b;
  end
end
%=============================================================================
function r = call_unassigned_single()
  r = 0;
  for k = 1:2
    r = r + no_output_set(k);
  end
end
%=============================================================================
function r = call_body_error()
  r = 0;
  for k = 1:2
    r = r + add_two([1 2], [1 2 3]);
  end
end
%=============================================================================
function r = call_too_many_inputs()
  r = 0;
  for k = 1:2
    r = r + scale(k, 2, 3);
  end
end
%=============================================================================
function r = call_too_many_outputs()
  r = 0;
  for k = 1:2
    [a, b, c] = two_outs(k);
    r = a + b + c;
  end
end
%=============================================================================
function e = error_of(f)
  e = [];
  try
    f();
  catch e
  end
end
%=============================================================================
function name = generic_frame(f)
  e = error_of(f);
  name = e.stack(1).name;
end
%=============================================================================
function feval_two_outputs(name)
  [a, b] = feval(name, 1); %#ok<NLS0004>
end
%=============================================================================
function r = feval_one_output(name, x)
  r = feval(name, x);
end
%=============================================================================
function [a, b] = two_outs(x)
  a = x + 1;
  b = x - 1;
end
%=============================================================================
function y = scale(x, z)
  y = x * z;
end
%=============================================================================
function [a, b, c] = three_outs(x)
  a = x;
  b = [2 2] * x;
  c = 'c';
end
%=============================================================================
function [a, varargout] = with_varargout(x, z)
  a = x + z;
  varargout{1} = x * z;
end
%=============================================================================
function [a, b] = only_a(x) %#ok<NLS0006>
  a = x;
end
%=============================================================================
function y = no_output_set(x) %#ok<NLS0006>
  z = x; %#ok<NLS0004>
end
%=============================================================================
function y = add_two(x, z)
  y = x + z;
end
%=============================================================================
