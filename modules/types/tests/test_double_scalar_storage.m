%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Scalar copies remain independent through writes and storage transitions.
a = 3.5;
b = a;
a(1) = 7;
asserts.isequal(b, 3.5);
asserts.isequal(a, 7);
b(3) = 9;
asserts.isequal(b, [3.5 0 9]);
asserts.isequal(a, 7);
%=============================================================================
a = 3.5;
b = a;
a(2, 2) = 8;
asserts.isequal(a, [3.5 0; 0 8]);
asserts.isequal(b, 3.5);
a = b;
a(1) = [];
asserts.istrue(isempty(a));
asserts.isequal(b, 3.5);
%=============================================================================
asserts.isequal(size(b), [1 1]);
asserts.isequal(size(b, 3), 1);
asserts.isequal(size(b, 8), 1);
asserts.isequal(b(1, 1, 1), 3.5);
asserts.isequal(reshape(b, [1 1 1]), 3.5);
asserts.isequal(diag(b), 3.5);
asserts.isequal(diag(b, 1), [0 3.5; 0 0]);
asserts.checkerror('b(2)', _('Index exceeds array bounds.'), 'Nelson:badsubscript');
%=============================================================================
converted = single(b);
asserts.isequal(class(converted), 'single');
asserts.isequal(double(converted), b);
asserts.isequal(int32(b), int32(4));
asserts.isequal(complex(b, 2), 3.5 + 2i);
asserts.isequal(full(sparse(b)), b);
asserts.isequal(b, 3.5);
asserts.isequal(nnz(b), 1);
asserts.isequal(nnz(0), 0);
asserts.isequal(nnz(NaN), 1);
asserts.isequal(nnz(Inf), 1);
asserts.isequal(nzmax(b), 1);
%=============================================================================
items = {b, b};
items{1}(1) = 11;
asserts.isequal(items{1}, 11);
asserts.isequal(items{2}, 3.5);
s = struct('value', b);
t = s;
s.value(2) = 4;
asserts.isequal(s.value, [3.5 4]);
asserts.isequal(t.value, 3.5);
numericValue = 3.5;
caught = [];
try
  numericValue.value = 1;
catch caught
end
asserts.isfalse(isempty(caught));
asserts.isequal(caught.identifier, 'Nelson:structAssToNonStruct');
asserts.isequal(numericValue, 3.5);
%=============================================================================
values = [0, -0.0, 3.5, -2, Inf, -Inf, NaN];
for k = 1:numel(values)
  value = values(k) + 0;
  copy = value;
  asserts.isequal(class(value), 'double');
  asserts.isequal(size(value), [1 1]);
  asserts.istrue(isequaln(copy, value));
  value(2) = 1;
  asserts.istrue(isequaln(copy, values(k)));
end
negativeZero = -0.0;
copy = negativeZero;
asserts.isequal(1 / negativeZero, -Inf);
asserts.isequal(1 / copy, -Inf);
asserts.istrue(isnan(Inf - Inf));
%=============================================================================
a = 0;
b = 1;
for k = 1:10
  c = a + b;
  a = b;
  b = c;
end
asserts.isequal(a, 55);
asserts.isequal(b, 89);
%=============================================================================
