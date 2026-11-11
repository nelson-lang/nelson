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
% x = [x y] grows x in place only when nothing else holds its value: the
% result is the concatenation, and every other copy keeps its old value.
%=============================================================================
assert_isequal(grow_row(5), 1:5);
assert_isequal(grow_text(4), 'abcd');
assert_isequal(grow_logical(4), [true false true false]);
assert_isequal(grow_class(4, 'int8'), int8(1:4));
assert_isequal(grow_class(4, 'single'), single(1:4));
assert_isequal(grow_class(4, 'uint64'), uint64(1:4));
assert_isequal(grow_cell(3), {1, [2 4], [3 6]});
assert_isequal(grow_by_rows(3), [1 1 2 2 3 3]);
%=============================================================================
% other holders of the value keep it
[a, b] = shared_copy();
assert_isequal(a, [1 2 3 4]);
assert_isequal(b, [1 2]);
[v, c] = held_by_cell();
assert_isequal(v, [1 2 3]);
assert_isequal(c, {[1 2]});
before = [1 2];
after = grow_argument(before);
assert_isequal(before, [1 2]);
assert_isequal(after, [1 2 9]);
assert_isequal(grow_global(), [1 2 3]);
global append_in_place_global
assert_isequal(append_in_place_global, [1 2 3]);
clear global append_in_place_global
assert_isequal(grow_after_eval(), [7 8 9]);
%=============================================================================
% other shapes and classes keep the generic concatenation and its errors
assert_isequal(mixed_class(), 'abB');
assert_isequal(grow_empty_tail(), [1 2]);
assert_checkerror('v = [1; 2]; v = [v 3];', ...
  'Dimensions of arrays being concatenated are not consistent.');
assert_checkerror('v = [1 2]; v = [v [3; 4]];', ...
  'Dimensions of arrays being concatenated are not consistent.');
s = string('a');
s = [s 'b'];
assert_isequal(s, ["a", "b"]);
%=============================================================================
function v = grow_row(n)
  v = [];
  for k = 1:n
    v = [v k]; %#ok<NLS0039>
  end
end
%=============================================================================
function s = grow_text(n)
  s = '';
  for k = 1:n
    s = [s char('a' + k - 1)]; %#ok<NLS0039>
  end
end
%=============================================================================
function v = grow_logical(n)
  v = true;
  for k = 2:n
    v = [v mod(k, 2) == 1]; %#ok<NLS0039>
  end
end
%=============================================================================
function v = grow_class(n, className)
  v = cast(1, className);
  for k = 2:n
    v = [v cast(k, className)]; %#ok<NLS0039>
  end
end
%=============================================================================
function c = grow_cell(n)
  c = {1};
  for k = 2:n
    c = [c {k * [1 2]}]; %#ok<NLS0039>
  end
end
%=============================================================================
function v = grow_by_rows(n)
  v = [1 1];
  for k = 2:n
    v = [v [k k]]; %#ok<NLS0039>
  end
end
%=============================================================================
function [a, b] = shared_copy()
  a = [1 2];
  b = a;
  for k = 3:4
    a = [a k]; %#ok<NLS0039>
  end
end
%=============================================================================
function [v, c] = held_by_cell()
  v = [1 2];
  c = {v};
  v = [v 3];
end
%=============================================================================
function v = grow_argument(v)
  for k = 1:2
    if k == 2
      v = [v 9]; %#ok<NLS0039>
    end
  end
end
%=============================================================================
function r = grow_global()
  global append_in_place_global
  append_in_place_global = [1 2];
  for k = 3
    append_in_place_global = [append_in_place_global k]; %#ok<NLS0039>
  end
  r = append_in_place_global;
end
%=============================================================================
function v = grow_after_eval()
  v = 7;
  v = [v 8];
  eval('w = 1;'); %#ok<NLS0022>
  v = [v 9];
end
%=============================================================================
function s = mixed_class()
  s = 'ab';
  s = [s 66];
end
%=============================================================================
function v = grow_empty_tail()
  v = [1 2];
  v = [v []];
end
%=============================================================================
