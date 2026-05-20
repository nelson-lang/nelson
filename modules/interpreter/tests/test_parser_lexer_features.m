%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
parse_cases = {};
parse_cases{end + 1} = 'A = ["left", "right"; "top", "bottom"];';
parse_cases{end + 1} = 'A = [''ab''; ''cd'']; B = A'';';
parse_cases{end + 1} = 'A = [1 2 3; 4 -5 6; 7 (8) 9];';
parse_cases{end + 1} = 'A = {1 2; 3 4};';
parse_cases{end + 1} = 'A = [1 +2 -3; 4 -5 +6];';
parse_cases{end + 1} = sprintf('A = [1, ...\n 2, ...\n 3];');
parse_cases{end + 1} = sprintf('A = [1 ... %% continued after comment text\n 2];');
parse_cases{end + 1} = sprintf('A = 1; %% trailing comment\n%% whole line comment\nB = 2;');
parse_cases{end + 1} = sprintf('A = 1;\n%%{\nthis is ignored = [1 2;\nif broken syntax\n%%}\nB = 2;');
parse_cases{end + 1} = sprintf('%%{\ncomment before first statement\n%%}\nA = 1;');
parse_cases{end + 1} = 'clear parserLexerCommandValue';
parse_cases{end + 1} = 'captureArgs = @(varargin) varargin; A = captureArgs(Name = 1, Other = 2);';
parse_cases{end + 1} = '[~, idx] = max([1 3 2]);';
%=============================================================================
for k = 1:numel(parse_cases)
  assert_isequal(parsestring(parse_cases{k}), 'script');
end
%=============================================================================
arguments_block_case = sprintf(['function y = parser_lexer_arguments_block(x)\n', ...
  'arguments\n', ...
  '  x double\n', ...
  'end\n', ...
  'y = x.'';\n', ...
  'end']);
assert_isequal(parsestring(arguments_block_case), 'function');
%=============================================================================
local_function_case = sprintf(['function y = parser_lexer_local_function(x)\n', ...
  'y = local_add_one(x);\n', ...
  'end\n', ...
  'function z = local_add_one(t)\n', ...
  'z = t + 1;\n', ...
  'end']);
assert_isequal(parsestring(local_function_case), 'function');
%=============================================================================
endfunction_case = sprintf(['function y = parser_lexer_endfunction(x)\n', ...
  'y = x;\n', ...
  'endfunction']);
assert_isequal(parsestring(endfunction_case), 'error');
slash_keyword_case = 'A(/Name = 1);';
assert_isequal(parsestring(slash_keyword_case), 'error');
slash_keyword_flag_case = 'A(/Name);';
assert_isequal(parsestring(slash_keyword_flag_case), 'error');
%=============================================================================
eof_function_case = sprintf(['function y = parser_lexer_eof_function(x)\n', ...
  'y = x;']);
assert_isequal(parsestring(eof_function_case), 'function');
eof_function_with_block_case = sprintf(['function y = parser_lexer_eof_function_with_block(x)\n', ...
  'if x > 0\n', ...
  '  y = x;\n', ...
  'else\n', ...
  '  y = -x;\n', ...
  'end\n', ...
  'y = y + 1;']);
assert_isequal(parsestring(eof_function_with_block_case), 'function');
extra_end_function_case = sprintf(['function y = parser_lexer_extra_end_function(x)\n', ...
  'if x > 0\n', ...
  '  y = x;\n', ...
  'end\n', ...
  'end\n', ...
  'y = y + 1;']);
assert_isequal(parsestring(extra_end_function_case), 'error');
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
mismatch_file = [path_test, '/parser_lexer_filename_mismatch.m'];
fd = fopen(mismatch_file, 'wt');
fprintf(fd, 'function y = parser_lexer_declared_name(x)\ny = x;\nend\n');
fclose(fd);
assert_isequal(parsefile(mismatch_file), 'error');
rmdir(path_test, 's');
%=============================================================================
assert_isequal(parsestring('function y = parser_lexer_broken_function(x)'), 'error');
assert_isequal(parsestring('A = [1, 2; 3];'), 'script');
%=============================================================================
S = ["alpha", "beta"];
assert_isequal(S(1), "alpha");
assert_isequal(S(2), "beta");
%=============================================================================
C = ['a', 'b'];
assert_isequal(C, 'ab');
%=============================================================================
A = [1 2 3];
assert_isequal(A', [1; 2; 3]);
assert_isequal(A.', [1; 2; 3]);
assert_isequal(eval('3.'''), 3);
assert_isequal(eval('3.12 .'''), 3.12);
%=============================================================================
B = [1 ...
  2 ...
  3];
assert_isequal(B, [1 2 3]);
%=============================================================================
C = [1 ... % comment after continuation
  2];
assert_isequal(C, [1 2]);
%=============================================================================
M = [1 2 3; 4 -5 6; 7 (8) 9];
assert_isequal(M, [1 2 3; 4 -5 6; 7 8 9]);
%=============================================================================
N = [1 +2 -3; 4 -5 +6];
assert_isequal(N, [1 2 -3; 4 -5 6]);
%=============================================================================
K = {1 2; 3 4};
assert_isequal(K{1, 2}, 2);
assert_isequal(K{2, 1}, 3);
%=============================================================================
parserLexerCommentValue = 1; % comment keeps the statement terminator visible
% another comment-only line
assert_isequal(parserLexerCommentValue, 1);
%=============================================================================
parserLexerBeforeBlockComment = 1;
%{
ignoredFunctionCall("not parsed")
if this_were_code_it_would_fail
%}
parserLexerAfterBlockComment = 2;
assert_isequal(parserLexerBeforeBlockComment + parserLexerAfterBlockComment, 3);
%=============================================================================
parserLexerCommandValue = 10;
clear parserLexerCommandValue
assert_isequal(exist('parserLexerCommandValue', 'var'), 0);
%=============================================================================
captureArgs = @(varargin) varargin;
namedArgs = captureArgs(Name = 1, Other = 2);
assert_isequal(namedArgs{1}, "Name");
assert_isequal(namedArgs{2}, 1);
assert_isequal(namedArgs{3}, "Other");
assert_isequal(namedArgs{4}, 2);
%=============================================================================
[~, idx] = max([1 3 2]);
assert_isequal(idx, 2);
%=============================================================================
assert_checkerror('B = [1, ~, 2];', _("Lexical error 'Incorrect use of tilde.'"));
msg = sprintf(_('Lexical error ''%s'''), _('unterminated character array'));
assert_checkerror('3. ''', msg);
%=============================================================================
