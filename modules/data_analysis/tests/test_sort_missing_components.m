%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%=============================================================================
% Missing complex values carry observable, nonmissing components.
short = [complex(NaN, 11), 2, complex(NaN, 22), 1, complex(33, NaN), 3, complex(44, NaN)];
long = complex(mod(1:60, 3), zeros(1, 60));
long(3:3:60) = complex(NaN(1, 20), 1:20);
cases = {short, long, complex(NaN(1, 40), 1:40)};
missingOrders = {[1, 3, 5, 7], 3:3:60, 1:40};
presentOrders = {[4, 2, 6], [1:3:60, 2:3:60], []};
descendingOrders = {[6, 2, 4], [2:3:60, 1:3:60], []};
for type = {'double', 'single'}
  for pattern = 1:numel(cases)
    value = feval(type{1}, cases{pattern});
    for method = {'auto', 'real', 'abs'}
      for placement = {'auto', 'first', 'last'}
        for direction = {'ascend', 'descend'}
          present = presentOrders{pattern};
          if strcmp(direction{1}, 'descend'), present = descendingOrders{pattern}; end
          missing = missingOrders{pattern};
          missingFirst = strcmp(placement{1}, 'first') || ...
            (strcmp(placement{1}, 'auto') && strcmp(direction{1}, 'descend'));
          order = [present, missing];
          if missingFirst, order = [missing, present]; end
          expected = value(order);
          b = sort(value, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', placement{1});
          asserts.isequal(real(b), real(expected));
          asserts.isequal(imag(b), imag(expected));
          [indexed, indices] = sort(value, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', placement{1});
          asserts.isequal(indices, order);
          asserts.isequal(real(b), real(indexed));
          asserts.isequal(imag(b), imag(indexed));
          b = sort(value.', direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', placement{1});
          asserts.isequal(real(b), real(expected.'));
          asserts.isequal(imag(b), imag(expected.'));
          b = sort([value; value], 2, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', placement{1});
          asserts.isequal(real(b), real([expected; expected]));
          asserts.isequal(imag(b), imag([expected; expected]));
          b = sort([value.', value.'], 1, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', placement{1});
          asserts.isequal(real(b), real([expected.', expected.']));
          asserts.isequal(imag(b), imag([expected.', expected.']));
          nd = cat(3, [value; value], [value; value]);
          b = sort(nd, 2, direction{1}, 'ComparisonMethod', method{1}, 'MissingPlacement', placement{1});
          expectedND = cat(3, [expected; expected], [expected; expected]);
          asserts.isequal(real(b), real(expectedND));
          asserts.isequal(imag(b), imag(expectedND));
        end
      end
    end
  end
end
% Infinite nonmissing components of missing entries must also remain attached.
a = [complex(Inf, NaN), 3 + 1i, complex(NaN, -Inf), 1 + 1i];
expected = a([4, 2, 1, 3]);
b = sort(a);
asserts.isequal(real(b), real(expected));
asserts.isequal(imag(b), imag(expected));
