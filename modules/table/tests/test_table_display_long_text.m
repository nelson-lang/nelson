%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_table_display_long_text()
  % a long text shows its text, not its size and class
  T = table(categorical({'a_very_long_category_name'}));
  assert_isequal(display_lines(T), "a_very_long_category_name");
  %=============================================================================
  T = table(categorical({'short'; 'a_very_long_category_name'}));
  assert_isequal(display_lines(T), ["short"; "a_very_long_category_name"]);
  %=============================================================================
  T = table("a rather long string value");
  assert_isequal(display_lines(T), """a rather long string value""");
  %=============================================================================
  T = table(["x"; "a rather long string value"]);
  assert_isequal(display_lines(T), ["""x"""; """a rather long string value"""]);
  %=============================================================================
  T = table('a long char row of text');
  assert_isequal(display_lines(T), "a long char row of text");
  %=============================================================================
  T = table(['a long char row of text'; 'another long char row!!']);
  assert_isequal(display_lines(T), ...
    ["a long char row of text"; "another long char row!!"]);
  %=============================================================================
  % unchanged: cell contents, numeric rows and other classes
  T = table({'a_very_long_char_text_here'});
  assert_isequal(display_lines(T), "{'a_very_long_char_text_here'}");
  %=============================================================================
  T = table([1:8; 11:18]);
  L = display_lines(T);
  assert_isequal(L(1), "1     2     3     4     5     6     7     8");
  %=============================================================================
  T = table(ones(1, 2, 2));
  x = char(215);
  assert_isequal(display_lines(T), string(['1', x, '2', x, '2 double']));
end
%=============================================================================
function L = display_lines(T)
  % the data rows of disp(T), without the header and the rule
  L = strtrim(splitlines(string(evalc('disp(T)'))));
  L(L == "") = [];
  L = L(3:end);
end
%=============================================================================
