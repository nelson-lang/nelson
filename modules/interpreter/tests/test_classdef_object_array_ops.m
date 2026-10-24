%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% reshape on value object arrays
a(6) = ClassdefPoint(0, 0);
for k = 1:6
  a(k) = ClassdefPoint(k, -k);
end
b = reshape(a, 2, 3);
asserts.isequal(size(b), [2 3]);
asserts.isequal(class(b), 'ClassdefPoint');
asserts.isequal(b(2, 1).X, 2);
asserts.isequal(b(1, 2).X, 3);
%=============================================================================
% permute on value object arrays
c = permute(b, [2 1]);
asserts.isequal(size(c), [3 2]);
asserts.isequal(class(c), 'ClassdefPoint');
asserts.isequal(c(1, 2).X, 2);
%=============================================================================
% reshape/permute on handle object arrays
h = [ClassdefCounter(1), ClassdefCounter(2); ClassdefCounter(3), ClassdefCounter(4)];
hr = reshape(h, 1, 4);
asserts.isequal(size(hr), [1 4]);
asserts.isequal(class(hr), 'ClassdefCounter');
asserts.isequal(hr(2).Count, 3);
hp = permute(h, [2 1]);
asserts.isequal(hp(1, 2).Count, 3);
% handle identity preserved through reshape
hr(1).increment();
asserts.isequal(h(1, 1).Count, 2);
%=============================================================================
% isequal on value objects compares class and properties
p = ClassdefPoint(1, 2);
q = ClassdefPoint(1, 2);
asserts.istrue(isequal(p, q));
q.X = 9;
asserts.isfalse(isequal(p, q));
asserts.isfalse(isequal(p, struct('X', 1, 'Y', 2)));
asserts.istrue(isequaln(ClassdefPoint(NaN, 2), ClassdefPoint(NaN, 2)));
%=============================================================================
% methods() lists the implicit or explicit visible constructor
m = methods('ClassdefPoint');
asserts.istrue(any(strcmp(m, 'ClassdefPoint')));
m2 = methods('ClassdefProtectedCtor');
asserts.isfalse(any(strcmp(m2, 'ClassdefProtectedCtor')));
%=============================================================================
