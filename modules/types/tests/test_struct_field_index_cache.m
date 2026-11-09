%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A structure with many fields reads and writes every field by name, also
% after a field is added or removed, the fields reordered or the value copied.
%=============================================================================
names = cell(1, 40);
s = struct();
for k = 1:40
  names{k} = sprintf('f%02d', k);
  s.(names{k}) = k;
end
for k = 1:40
  assert_isequal(s.(names{k}), k);
end
s.extra = 41;
assert_isequal(s.extra, 41);
assert_isequal(s.f40, 40);
s = rmfield(s, 'f10');
assert_isfalse(isfield(s, 'f10'));
assert_isequal(s.f11, 11);
assert_isequal(s.extra, 41);
t = orderfields(s);
assert_isequal(t.f01, 1);
assert_isequal(t.extra, 41);
c = s;
c.f05 = -5;
assert_isequal(s.f05, 5);
assert_isequal(c.f05, -5);
small = struct('a', 1, 'b', 2);
assert_isequal(small.b, 2);
%=============================================================================
