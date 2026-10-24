%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Command syntax must keep a parenthesised argument such as f(x, y) as a
% single literal character array. Separators (spaces, commas, semicolons)
% inside the parentheses no longer split the argument, while separators at
% bracket depth zero still separate arguments, so 'a b c' remains three
% arguments.
%=============================================================================
global COMMAND_SYNTAX_ARGS
%=============================================================================
capture_command_args f(x)
assert_isequal(COMMAND_SYNTAX_ARGS, {'f(x)'});
%=============================================================================
capture_command_args g(t)
assert_isequal(COMMAND_SYNTAX_ARGS, {'g(t)'});
%=============================================================================
capture_command_args f(x,y)
assert_isequal(COMMAND_SYNTAX_ARGS, {'f(x,y)'});
%=============================================================================
capture_command_args f(x, y)
assert_isequal(COMMAND_SYNTAX_ARGS, {'f(x, y)'});
%=============================================================================
capture_command_args h(a, b, c)
assert_isequal(COMMAND_SYNTAX_ARGS, {'h(a, b, c)'});
%=============================================================================
capture_command_args x y z
assert_isequal(COMMAND_SYNTAX_ARGS, {'x', 'y', 'z'});
%=============================================================================
capture_command_args a b
assert_isequal(COMMAND_SYNTAX_ARGS, {'a', 'b'});
%=============================================================================
% The normal function-call form must be unaffected by the command syntax.
r = capture_command_args('p', 'q');
assert_isequal(r, {'p', 'q'});
%=============================================================================
% A variable whose name collides with the helper must be treated as an
% assignment, not as a command.
capture_command_args = 42;
assert_isequal(capture_command_args, 42);
clear capture_command_args
%=============================================================================
% A '%' comment ends the command even without a preceding space; the comment
% text must not be collected as extra arguments.
capture_command_args a b% trailing comment
assert_isequal(COMMAND_SYNTAX_ARGS, {'a', 'b'});
capture_command_args c%glued comment
assert_isequal(COMMAND_SYNTAX_ARGS, {'c'});
%=============================================================================
function varargout = capture_command_args(varargin)
  global COMMAND_SYNTAX_ARGS
  COMMAND_SYNTAX_ARGS = varargin;
  varargout{1} = varargin;
end
%=============================================================================
