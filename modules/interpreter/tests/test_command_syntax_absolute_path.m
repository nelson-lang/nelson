%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Command syntax must recognize an absolute-path argument (leading '/'):
% for a non-variable identifier, whitespace before the
% '/' and none after it selects command syntax, so the whole path is taken as
% a single literal character array instead of being lexed as division. Binary
% division must stay unaffected when the identifier is a variable.
%=============================================================================
global COMMAND_SYNTAX_ARGS
%=============================================================================
% Leading-'/' absolute path argument -> command syntax with one string arg.
capture_command_args /tmp/foo
assert_isequal(COMMAND_SYNTAX_ARGS, {'/tmp/foo'});
%=============================================================================
% Deeper absolute path.
capture_command_args /usr/local/bin
assert_isequal(COMMAND_SYNTAX_ARGS, {'/usr/local/bin'});
%=============================================================================
% Absolute path argument ending with '/' (the dangling operator used to be a
% hard parse error; here it is part of the literal argument).
capture_command_args /abs/dir/
assert_isequal(COMMAND_SYNTAX_ARGS, {'/abs/dir/'});
%=============================================================================
% A path with a file extension, mixed with a following option and a second
% path (mirrors the redbKIT "mex <path>.c -outdir <path>/" pattern).
capture_command_args /a/b/file.c -outdir /a/b/
assert_isequal(COMMAND_SYNTAX_ARGS, {'/a/b/file.c', '-outdir', '/a/b/'});
%=============================================================================
% Division must remain division when the identifier is a variable, for every
% spacing of the operator.
x = 6;
y = 2;
assert_isequal(x/y, 3);
assert_isequal(x /y, 3);
assert_isequal(x / y, 3);
assert_isequal(x/ y, 3);
%=============================================================================
% A single-slash argument on a variable stays a division.
z = x /y;
assert_isequal(z, 3);
%=============================================================================
% An operator interrupting the run keeps it arithmetic: x /y*2 == (x/y)*2.
assert_isequal(x /y*2, 6);
%=============================================================================
% Variable rule: when the head is a variable, EVERY operator spacing
% stays an expression, including '-' and '+' with "space before, none after"
% and multi-slash forms (the classic "p /q/2" ambiguity).
assert_isequal(x -y, 4);
assert_isequal(x +y, 8);
assert_isequal(x -y +1, 5);
assert_isequal(x /y/1, 3);
%=============================================================================
% Non-variable head: single-component absolute path is now command syntax.
capture_command_args /foo
assert_isequal(COMMAND_SYNTAX_ARGS, {'/foo'});
%=============================================================================
% Non-variable head: option flags ('-'/'+' initial) are command arguments.
capture_command_args -append
assert_isequal(COMMAND_SYNTAX_ARGS, {'-append'});
capture_command_args -x data.mat
assert_isequal(COMMAND_SYNTAX_ARGS, {'-x', 'data.mat'});
capture_command_args +tag
assert_isequal(COMMAND_SYNTAX_ARGS, {'+tag'});
%=============================================================================
% An operator on the right-hand side of an assignment stays arithmetic even for
% a non-variable head (command syntax applies at statement position only):
% here the function twelve() is called, then divided.
q = twelve /2;
assert_isequal(q, 6);
%=============================================================================
% The normal function-call form must be unaffected.
r = capture_command_args('/tmp/foo');
assert_isequal(r, {'/tmp/foo'});
%=============================================================================
function varargout = capture_command_args(varargin)
  global COMMAND_SYNTAX_ARGS
  COMMAND_SYNTAX_ARGS = varargin;
  varargout{1} = varargin;
end
%=============================================================================
function r = twelve()
  r = 12;
end
%=============================================================================
