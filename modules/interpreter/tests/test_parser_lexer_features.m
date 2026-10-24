%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
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
parse_cases{end + 1} = 'ignoreArgs = @(~, value, ~) value; A = ignoreArgs(1, 2, 3);';
parse_cases{end + 1} = 'A = [1 missing 3];';
parse_cases{end + 1} = 'S = ["a" missing ""];';
parse_cases{end + 1} = 'try, error(''nelson:test:inlineCatch'', ''boom''), catch e, message = e.message; end';
parse_cases{end + 1} = 'try error(''nelson:test:inlineCatchTryNoSep'', ''boom''), catch e, message = e.message; end';
parse_cases{end + 1} = 'try; error(''nelson:test:inlineCatchSemicolon'', ''boom''); catch e; message = e.message; end';
parse_cases{end + 1} = 'try error(''nelson:test:inlineCatchNoIdentifier'', ''boom''); catch, message = ''caught''; end';
parse_cases{end + 1} = 'A = 10.^(0:6);';
% 'catch ex' with an empty body is valid in every layout (multiline, blank line,
% comment-only body, inline comma form, nested in a loop).
parse_cases{end + 1} = sprintf('try\n  z = 1;\ncatch ex\nend');
parse_cases{end + 1} = sprintf('try\n  z = 1;\ncatch ex\n\nend');
parse_cases{end + 1} = sprintf('try\n  z = 1;\ncatch ex\n  %% only a comment\nend');
parse_cases{end + 1} = 'try, z = 1, catch ex, end';
parse_cases{end + 1} = sprintf('try\n  z = 1;\ncatch ex;\nend');
parse_cases{end + 1} = sprintf('for k = 1:2\n  try\n    z = k;\n  catch ex\n  end\nend');
parse_cases{end + 1} = sprintf('path_test = [tempdir(), createGUID()]; mkdir(path_test); cd(path_test); mkdir(''utils''); addpath ./utils/; rmpath ./utils/; rmdir(path_test, ''s'');');
parse_cases{end + 1} = sprintf('path_test = [tempdir(), createGUID()]; mkdir(path_test); nested = [path_test, ''/nested'']; mkdir(nested); utils = [path_test, ''/utils'']; mkdir(utils); cd(nested); addpath ../utils/; rmpath ../utils/; rmdir(path_test, ''s'');');
%=============================================================================
for k = 1:numel(parse_cases)
  asserts.isequal(parsestring(parse_cases{k}), 'script');
end
%=============================================================================
arguments_block_case = sprintf(['function y = parser_lexer_arguments_block(x)\n', ...
  'arguments\n', ...
  '  x double\n', ...
  'end\n', ...
  'y = x.'';\n', ...
  'end']);
asserts.isequal(parsestring(arguments_block_case), 'function');
%=============================================================================
local_function_case = sprintf(['function y = parser_lexer_local_function(x)\n', ...
  'y = local_add_one(x);\n', ...
  'end\n', ...
  'function z = local_add_one(t)\n', ...
  'z = t + 1;\n', ...
  'end']);
asserts.isequal(parsestring(local_function_case), 'function');
%=============================================================================
ignored_function_input_case = sprintf(['function y = parser_lexer_ignored_function_input(~, x, ~)\n', ...
  'y = x;\n', ...
  'end']);
asserts.isequal(parsestring(ignored_function_input_case), 'function');
%=============================================================================
endfunction_case = sprintf(['function y = parser_lexer_endfunction(x)\n', ...
  'y = x;\n', ...
  'endfunction']);
asserts.isequal(parsestring(endfunction_case), 'error');
slash_keyword_case = 'A(/Name = 1);';
asserts.isequal(parsestring(slash_keyword_case), 'error');
slash_keyword_flag_case = 'A(/Name);';
asserts.isequal(parsestring(slash_keyword_flag_case), 'error');
%=============================================================================
eof_function_case = sprintf(['function y = parser_lexer_eof_function(x)\n', ...
  'y = x;']);
asserts.isequal(parsestring(eof_function_case), 'function');
eof_function_with_block_case = sprintf(['function y = parser_lexer_eof_function_with_block(x)\n', ...
  'if x > 0\n', ...
  '  y = x;\n', ...
  'else\n', ...
  '  y = -x;\n', ...
  'end\n', ...
  'y = y + 1;']);
asserts.isequal(parsestring(eof_function_with_block_case), 'function');
% An 'end'-terminated function followed by top-level script statements is a script
% with a local function, not an error (the trailing statement runs at script scope).
extra_end_function_case = sprintf(['function y = parser_lexer_extra_end_function(x)\n', ...
  'if x > 0\n', ...
  '  y = x;\n', ...
  'end\n', ...
  'end\n', ...
  'y = y + 1;']);
asserts.isequal(parsestring(extra_end_function_case), 'script');
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
mismatch_file = [path_test, '/parser_lexer_filename_mismatch.m'];
fd = fopen(mismatch_file, 'wt');
fprintf(fd, 'function y = parser_lexer_declared_name(x)\ny = x;\nend\n');
fclose(fd);
asserts.isequal(parsefile(mismatch_file), 'error');
rmdir(path_test, 's');
%=============================================================================
inlineCatchSeen = false;
try, error('nelson:test:inlineCatch', 'boom'), catch e, inlineCatchSeen = true; inlineCatchMessage = e.message; inlineCatchIdentifier = e.identifier; end
asserts.istrue(inlineCatchSeen);
asserts.isequal(inlineCatchMessage, 'boom');
asserts.isequal(inlineCatchIdentifier, 'nelson:test:inlineCatch');
inlineCatchTryNoSeparatorSeen = false;
try error('nelson:test:inlineCatchTryNoSep', 'boom'), catch e, inlineCatchTryNoSeparatorSeen = true; inlineCatchTryNoSeparatorMessage = e.message; inlineCatchTryNoSeparatorIdentifier = e.identifier; end
asserts.istrue(inlineCatchTryNoSeparatorSeen);
asserts.isequal(inlineCatchTryNoSeparatorMessage, 'boom');
asserts.isequal(inlineCatchTryNoSeparatorIdentifier, 'nelson:test:inlineCatchTryNoSep');
inlineCatchSemicolonSeen = false;
try; error('nelson:test:inlineCatchSemicolon', 'boom semicolon'); catch e; inlineCatchSemicolonSeen = true; inlineCatchSemicolonMessage = e.message; inlineCatchSemicolonIdentifier = e.identifier; end
asserts.istrue(inlineCatchSemicolonSeen);
asserts.isequal(inlineCatchSemicolonMessage, 'boom semicolon');
asserts.isequal(inlineCatchSemicolonIdentifier, 'nelson:test:inlineCatchSemicolon');
inlineCatchNoIdentifierSeen = false;
try error('nelson:test:inlineCatchNoIdentifier', 'boom no identifier'); catch, inlineCatchNoIdentifierSeen = true; end
asserts.istrue(inlineCatchNoIdentifierSeen);
%=============================================================================
% 'catch ex' with an empty body still assigns the exception to ex.
clear emptyCatchException;
try
  error('nelson:test:emptyCatch', 'boom empty');
catch emptyCatchException
end
asserts.isequal(emptyCatchException.identifier, 'nelson:test:emptyCatch');
% the no-error path leaves the catch variable untouched
clear emptyCatchException;
try
  emptyCatchValue = 1;
catch emptyCatchException
end
asserts.isequal(emptyCatchValue, 1);
asserts.istrue(exist('emptyCatchException', 'var') == 0);
% empty body whose only content is a comment
clear commentCatchException;
try
  error('nelson:test:commentCatch', 'boom comment');
catch commentCatchException
  % intentionally left blank
end
asserts.isequal(commentCatchException.identifier, 'nelson:test:commentCatch');
% inline comma form with an empty body
clear inlineEmptyCatchException;
try, error('nelson:test:inlineEmptyCatch', 'boom inline'), catch inlineEmptyCatchException, end
asserts.isequal(inlineEmptyCatchException.identifier, 'nelson:test:inlineEmptyCatch');
% empty catch body nested in a loop
clear loopCatchException;
for emptyCatchLoopIndex = 1:2
  try
    error('nelson:test:loopCatch', 'boom loop');
  catch loopCatchException
  end
end
asserts.isequal(loopCatchException.identifier, 'nelson:test:loopCatch');
%=============================================================================
asserts.isequal(parsestring('function y = parser_lexer_broken_function(x)'), 'function');
asserts.isequal(parsestring('A = [1, 2; 3];'), 'script');
asserts.isequal(parsestring('[1, 2, 3,]'), 'script');
asserts.isequal(parsestring('{1, 2, 3,}'), 'script');
asserts.isequal(parsestring('[1, 2,; 3, 4,]'), 'script');
asserts.isequal(parsestring('{1, 2,; 3, 4,}'), 'script');
asserts.isequal(parsestring('[,1, 2]'), 'script');
asserts.isequal(parsestring('{,1, 2}'), 'script');
asserts.isequal(parsestring('[1, 2,;]'), 'script');
asserts.isequal(parsestring('{1, 2,;}'), 'script');
asserts.isequal(parsestring('[;1, 2,]'), 'script');
asserts.isequal(parsestring('{;1, 2,}'), 'script');
asserts.isequal(parsestring('[1, 2,, 3]'), 'error');
asserts.isequal(parsestring('{1, 2,, 3}'), 'error');
%=============================================================================
S = ["alpha", "beta"];
asserts.isequal(S(1), "alpha");
asserts.isequal(S(2), "beta");
S = ["a" missing ""];
asserts.isequal(ismissing(S), [false true false]);
numericMissing = [1 missing 3];
asserts.isequal(numericMissing, [1 NaN 3]);
%=============================================================================
C = ['a', 'b'];
asserts.isequal(C, 'ab');
%=============================================================================
A = [1 2 3];
asserts.isequal(A', [1; 2; 3]);
asserts.isequal(A.', [1; 2; 3]);
asserts.isequal(eval('3.'''), 3);
asserts.isequal(eval('3.12 .'''), 3.12);
%=============================================================================
B = [1 ...
  2 ...
  3];
asserts.isequal(B, [1 2 3]);
%=============================================================================
C = [1 ... % comment after continuation
  2];
asserts.isequal(C, [1 2]);
%=============================================================================
M = [1 2 3; 4 -5 6; 7 (8) 9];
asserts.isequal(M, [1 2 3; 4 -5 6; 7 8 9]);
%=============================================================================
trailingCommaRow = [3, 3, 4, 4,];
asserts.isequal(trailingCommaRow, [3 3 4 4]);
trailingCommaMatrix = [1, 2,; 3, 4,];
asserts.isequal(trailingCommaMatrix, [1 2; 3 4]);
leadingCommaRow = [, 1, 2];
asserts.isequal(leadingCommaRow, [1 2]);
trailingCommaBeforeTerminator = [1, 2,;];
asserts.isequal(trailingCommaBeforeTerminator, [1 2]);
leadingSeparatorTrailingComma = [; 1, 2,];
asserts.isequal(leadingSeparatorTrailingComma, [1 2]);
%=============================================================================
N = [1 +2 -3; 4 -5 +6];
asserts.isequal(N, [1 2 -3; 4 -5 6]);
%=============================================================================
K = {1 2; 3 4};
asserts.isequal(K{1, 2}, 2);
asserts.isequal(K{2, 1}, 3);
%=============================================================================
trailingCommaCell = {3, 3, 4, 4,};
asserts.isequal(trailingCommaCell, {3, 3, 4, 4});
trailingCommaCellMatrix = {1, 2,; 3, 4,};
asserts.isequal(trailingCommaCellMatrix, {1, 2; 3, 4});
leadingCommaCell = {, 1, 2};
asserts.isequal(leadingCommaCell, {1, 2});
trailingCommaCellBeforeTerminator = {1, 2,;};
asserts.isequal(trailingCommaCellBeforeTerminator, {1, 2});
leadingSeparatorTrailingCommaCell = {; 1, 2,};
asserts.isequal(leadingSeparatorTrailingCommaCell, {1, 2});
%=============================================================================
parserLexerCommentValue = 1; % comment keeps the statement terminator visible
% another comment-only line
asserts.isequal(parserLexerCommentValue, 1);
%=============================================================================
parserLexerBeforeBlockComment = 1;
%{
ignoredFunctionCall("not parsed")
if this_were_code_it_would_fail
%}
parserLexerAfterBlockComment = 2;
asserts.isequal(parserLexerBeforeBlockComment + parserLexerAfterBlockComment, 3);
%=============================================================================
parserLexerCommandValue = 10;
clear parserLexerCommandValue
asserts.isequal(exist('parserLexerCommandValue', 'var'), 0);
%=============================================================================
captureArgs = @(varargin) varargin;
namedArgs = captureArgs(Name = 1, Other = 2);
asserts.isequal(namedArgs{1}, "Name");
asserts.isequal(namedArgs{2}, 1);
asserts.isequal(namedArgs{3}, "Other");
asserts.isequal(namedArgs{4}, 2);
%=============================================================================
[~, idx] = max([1 3 2]);
asserts.isequal(idx, 2);
%=============================================================================
asserts.checkerror('B = [1, ~, 2];', _("Lexical error 'Incorrect use of tilde.'"));
msg = sprintf(_('Lexical error ''%s'''), _('unterminated character array'));
asserts.checkerror('3. ''', msg);
%=============================================================================
