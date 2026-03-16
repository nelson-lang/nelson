%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1547
% <-- Short Description -->
% Parsing of ~= operator with spaces was not working correctly, causing it to be misinterpreted as a matrix row separator.
%=============================================================================
% Basic ~= parsing with spaces
a = rand(10,2);
t = 1:10;
ok = 0;
if size(a,2) ~= length(t); ok = 1; end
assert_isequal(ok, 1);
%=============================================================================
% No-space variant
ok = 0;
if size(a,2)~=length(t); ok = 1; end
assert_isequal(ok, 1);
%=============================================================================
% Parenthesized variant
ok = 0;
if (size(a,2) ~= length(t)); ok = 1; end
assert_isequal(ok, 1);
%=============================================================================
% elseif context
ok = 0;
if 0
  ok = 0;
elseif size(a,2) ~= length(t)
  ok = 1;
end
assert_isequal(ok, 1);
%=============================================================================
% while context
i = 0;
while size(a,2) ~= length(t)
  i = 1; break;
end
assert_isequal(i, 1);
%=============================================================================
% Unary ~ still works
ok = 0;
if ~true
  ok = 0;
else
  ok = 1;
end
assert_isequal(ok, 1);
%=============================================================================
% Virtual comma with unary ~ inside matrix
x = [1 ~true 0];
assert_isequal(x, [1 0 0]);
%=============================================================================
% Tilde placeholder (destructuring) still works
[~, b] = size([1 2; 3 4]);
assert_isequal(b, 2);
%=============================================================================