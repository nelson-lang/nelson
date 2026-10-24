%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
% <--CLI MODE-->
%=============================================================================
% everything after '...' on a line is dropped, including a '%{' marker:
% it does not open a block comment inside the continuation
continuation_block_comment = sprintf(['A = [1 ... %%{ text after continuation is dropped\n', ...
  '2 ... %% plain continuation comment\n', ...
  '...\n', ...
  '3];']);
asserts.isequal(parsestring(continuation_block_comment), 'script');
eval(continuation_block_comment);
asserts.isequal(A, [1 2 3]);
%=============================================================================
continuation_inline_block_comment = sprintf(['B = 10 + ... %%{ inline block opener is ignored\n', ...
  '5;']);
asserts.isequal(parsestring(continuation_inline_block_comment), 'script');
eval(continuation_inline_block_comment);
asserts.isequal(B, 15);
%=============================================================================
function_call_postfix = sprintf(['function out = parser_easy_postfix_case()\n', ...
  'out = makeCell(){1};\n', ...
  'end\n', ...
  'function c = makeCell()\n', ...
  'c = {42};\n', ...
  'end']);
asserts.isequal(parsestring(function_call_postfix), 'function');
%=============================================================================
struct_call_postfix = sprintf(['function out = parser_easy_dot_postfix_case()\n', ...
  'out = makeStruct().Field;\n', ...
  'end\n', ...
  'function s = makeStruct()\n', ...
  's.Field = 7;\n', ...
  'end']);
asserts.isequal(parsestring(struct_call_postfix), 'function');
%=============================================================================
asserts.isequal(parsestring('(makeStruct()).Field'), 'script');
asserts.isequal(parsestring('(makeCell()){1}'), 'script');
%=============================================================================
postfixCell = {42};
asserts.isequal((postfixCell){1}, 42);
postfixVector = [10 20 30];
asserts.isequal((postfixVector)(2), 20);
postfixStruct.Field = 7;
asserts.isequal((postfixStruct).Field, 7);
fieldName = 'Field';
asserts.isequal((postfixStruct).(fieldName), 7);
postfixStruct.f = @(x) x * 2;
asserts.isequal((postfixStruct).f(5), 10);
%=============================================================================
asserts.isequal(parsestring('[10 20 30](2)'), 'script');
asserts.isequal(parsestring('{10, 20}{2}'), 'script');
asserts.isequal(parsestring('''abc''(2)'), 'script');
asserts.isequal([10 20 30](2), 20);
asserts.isequal({10, 20}{2}, 20);
asserts.isequal('abc'(2), 'b');
%=============================================================================
postfixNestedCell = {postfixStruct};
asserts.isequal((postfixNestedCell){1}.Field, 7);
postfixNestedStruct.Field = [11 12 13];
asserts.isequal((postfixNestedStruct).Field(3), 13);
postfixNestedStruct.Inner.f = @(x) x + 100;
asserts.isequal((postfixNestedStruct).Inner.f(7), 107);
%=============================================================================
