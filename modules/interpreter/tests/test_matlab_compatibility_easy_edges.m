%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
%=============================================================================
continuation_block_comment = sprintf(['A = [1 ... %%{\n', ...
  'ignored text that is not Nelson syntax\n', ...
  '%%}\n', ...
  '2 ... %% plain continuation comment\n', ...
  '...\n', ...
  '3];']);
assert_isequal(parsestring(continuation_block_comment), 'script');
eval(continuation_block_comment);
assert_isequal(A, [1 2 3]);
%=============================================================================
continuation_inline_block_comment = sprintf(['B = 10 + ... %%{ inline block opener\n', ...
  'ignored = [1 2\n', ...
  '%%}\n', ...
  '5;']);
assert_isequal(parsestring(continuation_inline_block_comment), 'script');
eval(continuation_inline_block_comment);
assert_isequal(B, 15);
%=============================================================================
function_call_postfix = sprintf(['function out = parser_easy_postfix_case()\n', ...
  'out = makeCell(){1};\n', ...
  'end\n', ...
  'function c = makeCell()\n', ...
  'c = {42};\n', ...
  'end']);
assert_isequal(parsestring(function_call_postfix), 'function');
%=============================================================================
struct_call_postfix = sprintf(['function out = parser_easy_dot_postfix_case()\n', ...
  'out = makeStruct().Field;\n', ...
  'end\n', ...
  'function s = makeStruct()\n', ...
  's.Field = 7;\n', ...
  'end']);
assert_isequal(parsestring(struct_call_postfix), 'function');
%=============================================================================
assert_isequal(parsestring('(makeStruct()).Field'), 'script');
assert_isequal(parsestring('(makeCell()){1}'), 'script');
%=============================================================================
postfixCell = {42};
assert_isequal((postfixCell){1}, 42);
postfixVector = [10 20 30];
assert_isequal((postfixVector)(2), 20);
postfixStruct.Field = 7;
assert_isequal((postfixStruct).Field, 7);
fieldName = 'Field';
assert_isequal((postfixStruct).(fieldName), 7);
%=============================================================================
assert_isequal(parsestring('[10 20 30](2)'), 'script');
assert_isequal(parsestring('{10, 20}{2}'), 'script');
assert_isequal(parsestring('''abc''(2)'), 'script');
assert_isequal([10 20 30](2), 20);
assert_isequal({10, 20}{2}, 20);
assert_isequal('abc'(2), 'b');
%=============================================================================
postfixNestedCell = {postfixStruct};
assert_isequal((postfixNestedCell){1}.Field, 7);
postfixNestedStruct.Field = [11 12 13];
assert_isequal((postfixNestedStruct).Field(3), 13);
%=============================================================================
