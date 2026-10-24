%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
constructors = {@() true, @() false, @() true(1), @() false(1), ...
  @() logical(3), @() logical(single(0)), @() full(sparse(true)), ...
  @() any([false true]), @() all([true true]), @() 1 < 2, @() ~ false};
expected = [true false true false true false true true true true true];
for k = 1:numel(constructors)
  value = constructors{k}();
  asserts.isequal(class(value), 'logical');
  asserts.isequal(size(value), [1 1]);
  asserts.isequal(value, expected(k));
  asserts.isequal(nnz(value), double(expected(k)));
  asserts.isfalse(isnumeric(value));
  asserts.istrue(isreal(value));
  copy = value;
  value(1) = ~value;
  asserts.isequal(copy, expected(k));
  value(2) = true;
  asserts.isequal(value, [~expected(k), true]);
  asserts.isequal(copy, expected(k));
  asserts.isequal(double(copy), double(expected(k)));
  asserts.isequal(single(copy), single(expected(k)));
  asserts.isequal(class(double(copy)), 'double');
  asserts.isequal(class(single(copy)), 'single');
  asserts.isequal(copy(true), copy);
  asserts.isequal(class(copy(false)), 'logical');
  asserts.istrue(isempty(copy(false)));
  asserts.isequal(class(copy([])), 'logical');
  asserts.isequal(size(copy([])), [0 0]);
  asserts.isequal(copy(1, 1), copy);
  copy(1, 1) = true;
  asserts.istrue(copy);
end
asserts.isequal(double(true), 1);
asserts.isequal(double(false), 0);
%=============================================================================
mask = true;
values = [4 5];
asserts.isequal(values(mask), 4);
asserts.isequal(values(false), []);
cellValue = {true};
original = cellValue;
cellValue{1}(1) = false;
asserts.istrue(original{1});
value = struct('flag', true);
original = value;
value.flag(1) = false;
asserts.istrue(original.flag);
%=============================================================================
for value = [false true]
  if value
    result = 1;
  else
    result = 0;
  end
  asserts.isequal(result, double(value));
  asserts.isequal(full(sparse(value)), value);
  asserts.isequal(diag(value), value);
end
asserts.isequal(size(false(0, 1)), [0 1]);
asserts.isequal(true(1, 2), [true true]);
asserts.isequal(reshape([true false], [1 1 2]), cat(3, true, false));
%=============================================================================
