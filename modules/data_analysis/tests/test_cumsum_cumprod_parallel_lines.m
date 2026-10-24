%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Lines of a cumulative sum/product are distributed over threads: the result
% must be bit-identical to scanning each line alone (a single line is always
% computed serially), whatever the dimension, direction, NaN flag or type.
%=============================================================================
initialThreshold = feature('ompThreshold');
feature('ompThreshold', 0);
%=============================================================================
rng(7);
base = randi([-3, 3], 7, 5, 4);
X = base + 0.1 * rand(7, 5, 4);
X(2, 3, 1) = NaN;
X(5, 1, 4) = NaN;
Z = complex(X, fliplr(X));
Z(6, 2, 2) = complex(1, NaN);
data = {X, single(X), Z, single(Z), X > 0, int8(base), uint8(abs(base)), ...
  int16(base), int32(base), int64(base), uint64(abs(base))};
functions = {@cumsum, @cumprod};
directions = {'forward', 'reverse'};
nanflags = {'includenan', 'omitnan'};
%=============================================================================
for f = 1:numel(functions)
  fun = functions{f};
  for t = 1:numel(data)
    A = data{t};
    for d = 1:3
      order = [d, setdiff(1:3, d)];
      P = permute(A, order);
      lines = reshape(P, size(P, 1), []);
      for r = 1:numel(directions)
        for n = 1:numel(nanflags)
          if isinteger(A) || islogical(A)
            R = fun(A, d, directions{r});
          else
            R = fun(A, d, directions{r}, nanflags{n});
          end
          columns = cell(1, size(lines, 2));
          for c = 1:size(lines, 2)
            if isinteger(A) || islogical(A)
              columns{c} = fun(lines(:, c), 1, directions{r});
            else
              columns{c} = fun(lines(:, c), 1, directions{r}, nanflags{n});
            end
          end
          REF = ipermute(reshape([columns{:}], size(P)), order);
          asserts.isequal(class(R), class(REF));
          asserts.isequal(R, REF);
        end
      end
    end
  end
end
%=============================================================================
feature('ompThreshold', initialThreshold);
%=============================================================================
% Above the default threshold: one long line, many lines along both dims.
A = rand(1000, 300) > 0.5;
R1 = cumsum(A, 1);
R2 = cumsum(A, 2);
asserts.isequal(R1(end, :), sum(A, 1));
asserts.isequal(R2(:, end), sum(A, 2));
asserts.isequal(R1(:, 17), cumsum(double(A(:, 17))));
asserts.isequal(R2(42, :), cumsum(double(A(42, :))));
v = cumsum(ones(1e6, 1));
asserts.isequal(v(end), 1e6);
%=============================================================================
