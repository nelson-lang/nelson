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
%=============================================================================
% median, var and std of an empty array reduce the working dimension to 1:
% the result keeps the other dimensions and is NaN where it is not empty.
% [] (0x0) without dimension gives NaN, with a dimension it follows the rule.
%=============================================================================
asserts.isequal(median(zeros(0, 3)), NaN(1, 3));
asserts.isequal(median(zeros(0, 3), 2), zeros(0, 1));
asserts.isequal(median(zeros(3, 0)), zeros(1, 0));
asserts.isequal(median(zeros(3, 0), 2), NaN(3, 1));
asserts.isequal(median(zeros(1, 0)), NaN);
asserts.isequal(median(zeros(0, 3, 2)), NaN(1, 3, 2));
asserts.isequal(median(zeros(0, 3), 'omitnan'), NaN(1, 3));
asserts.isequal(median(zeros(0, 3), 'all'), NaN);
asserts.isequal(median([]), NaN);
asserts.isequal(median([], 'omitnan'), NaN);
asserts.isequal(median([], 1), zeros(1, 0));
asserts.isequal(median([], 2), zeros(0, 1));
asserts.isequal(median([], [1 2]), NaN);
asserts.isequal(median(single(zeros(0, 3))), single(NaN(1, 3)));
asserts.isequal(median(int8(zeros(0, 3))), int8([0 0 0]));
asserts.isequal(median(int8([])), int8(0));
asserts.isequal(median(int8([]), 1), int8(zeros(1, 0)));
%=============================================================================
for f = {@var, @std}
  fn = f{1};
  asserts.isequal(fn(zeros(0, 3)), NaN(1, 3));
  asserts.isequal(fn(zeros(0, 3), 0, 2), zeros(0, 1));
  asserts.isequal(fn(zeros(3, 0)), zeros(1, 0));
  asserts.isequal(fn(zeros(3, 0), 0, 2), NaN(3, 1));
  asserts.isequal(fn(zeros(1, 0)), NaN);
  asserts.isequal(fn(zeros(0, 3, 2)), NaN(1, 3, 2));
  asserts.isequal(fn(zeros(0, 3), 1), NaN(1, 3));
  asserts.isequal(fn(zeros(0, 3), 'omitnan'), NaN(1, 3));
  asserts.isequal(fn(zeros(0, 3), 0, 'all'), NaN);
  asserts.isequal(fn(zeros(0, 3), 0, [1 2]), NaN);
  asserts.isequal(fn([]), NaN);
  asserts.isequal(fn([], 0, 'all'), NaN);
  asserts.isequal(fn([], 0, 1), zeros(1, 0));
  asserts.isequal(fn([], 0, 2), zeros(0, 1));
  asserts.isequal(fn([], 0, [1 2]), NaN);
  asserts.isequal(fn(single(zeros(0, 3))), single(NaN(1, 3)));
  asserts.isequal(fn(single([]), 0, 1), single(zeros(1, 0)));
  asserts.isequal(fn(zeros(0, 3), ones(0, 1)), NaN(1, 3));
end
%=============================================================================
% second output (mean) follows the same shapes
[v, m] = var(zeros(0, 3));
asserts.isequal(v, NaN(1, 3));
asserts.isequal(m, NaN(1, 3));
[v, m] = var([], 0, 1);
asserts.isequal(v, zeros(1, 0));
asserts.isequal(m, zeros(1, 0));
[s, m] = std(zeros(0, 3), 0, 'all');
asserts.isequal(s, NaN);
asserts.isequal(m, NaN);
[s, m] = std(zeros(3, 0), 0, 2);
asserts.isequal(s, NaN(3, 1));
asserts.isequal(m, NaN(3, 1));
%=============================================================================
