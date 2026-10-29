%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Both vector and matrix paths must sort their nonmissing prefix.
a = [3 + 1i, 1 + 1i, 2 + 1i];
for type = {'double', 'single'}
  value = feval(type{1}, a);
  for method = {'auto', 'real', 'abs'}
    for direction = {'ascend', 'descend'}
      expected = [1 + 1i, 2 + 1i, 3 + 1i];
      if strcmp(direction{1}, 'descend'), expected = [3 + 1i, 2 + 1i, 1 + 1i]; end
      expected = feval(type{1}, expected);
      b = sort(value, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, expected);
      b = sort(value', direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      % Conjugation reverses only the sign of the common imaginary component.
      asserts.isequal(b, expected');
      b = sort([value; value], 2, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, [expected; expected]);
      b = sort([value', value'], 1, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, [expected', expected']);
      nd = cat(3, [value; value], [value; value]);
      b = sort(nd, 2, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, cat(3, [expected; expected], [expected; expected]));
      missing = feval(type{1}, complex(NaN, NaN));
      mixed = [missing, value, missing];
      b = sort(mixed, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, [expected, missing, missing]);
      matrix = [mixed; mixed];
      matrixResult = sort(matrix, 2, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(matrixResult, repmat([expected, missing, missing], 2, 1));
      [indexed, indices] = sort(mixed, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, indexed);
      b = sort(repmat(missing, 3, 2), 1, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', 'last');
      asserts.isequal(b, repmat(missing, 3, 2));
    end
  end
end
% A different real and magnitude order verifies all comparison branches.
a = [3, -2i, -3, 2i];
asserts.isequal(sort(a, 'ComparisonMethod', 'real', 'MissingPlacement', 'last'), [-3, -2i, 2i, 3]);
asserts.isequal(sort(a, 'ComparisonMethod', 'abs', 'MissingPlacement', 'last'), [-2i, 2i, 3, -3]);
% NaN in either component remains missing, including an infinite other component.
a = [complex(Inf, NaN), 3 + 1i, 1 + 1i, complex(NaN, Inf), 2 + 1i];
b = sort(a, 'MissingPlacement', 'last');
asserts.isequal(b(1:3), [1 + 1i, 2 + 1i, 3 + 1i]);
asserts.istrue(all(isnan(b(4:5))));
