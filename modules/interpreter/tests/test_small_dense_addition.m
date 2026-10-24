%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Check dimensions, operand order and independent ownership of small sums.
shapes = {[1, 2], [2, 1], [2, 3], [2, 2, 2], [1, 2, 1, 3]};
for shapeIndex = 1:numel(shapes)
  dimensions = shapes{shapeIndex};
  a = reshape(1:prod(dimensions), dimensions);
  before = a;
  twice = reshape(2:2:2 * numel(a), dimensions);
  shifted = reshape(4:3 + numel(a), dimensions);
  asserts.isequal(a + a, twice);
  asserts.isequal(a + 3, shifted);
  asserts.isequal(3 + a, shifted);
  asserts.isequal(plus(a, a), twice);
  asserts.isequal(plus(a, 3), shifted);
  asserts.isequal(plus(3, a), shifted);
  alias = a;
  result = a + a;
  result(1) = 99;
  asserts.isequal(a, before);
  asserts.isequal(alias, before);
  asserts.isequal(a + a, twice);
  for repetition = 1:10
    result = a + 3;
  end
  asserts.isequal(result, shifted);
  result = a;
  for repetition = 1:10
    result = result + 3;
  end
  asserts.isequal(result, before + 30);
  asserts.isequal(a, before);
end

for count = [63, 64, 65, 128, 129, 1000]
  a = 1:count;
  asserts.isequal(a + a, 2:2:2 * count);
  asserts.isequal(a + 3, 4:count + 3);
  asserts.isequal(3 + a, 4:count + 3);
end

% General paths must retain broadcasting, empty sizes and type promotion.
asserts.isequal([1, 2] + [10; 20], [11, 12; 21, 22]);
a = reshape(1:4, 2, 1, 2);
b = reshape([10, 20, 30], 1, 3);
asserts.isequal(a + b, cat(3, [11, 21, 31; 12, 22, 32], [13, 23, 33; 14, 24, 34]));
asserts.isequal(zeros(0, 3) + 1, zeros(0, 3));
asserts.isequal(1 + zeros(2, 0, 3), zeros(2, 0, 3));
asserts.isequal(single([1, 2]) + single([3, 4]), single([4, 6]));
asserts.isequal(single([1, 2]) + 3, single([4, 5]));
asserts.isequal(logical([1, 0]) + logical([0, 1]), [1, 1]);
asserts.isequal(uint8([250, 1]) + uint8([10, 2]), uint8([255, 3]));
asserts.isequal([1 + 2i, 3 + 4i] + [5, 6], [6 + 2i, 9 + 4i]);
asserts.isequal(full(sparse([1, 2]) + sparse([3, 4])), [4, 6]);
asserts.isequal(full(sparse([1, 2]) + 3), [4, 5]);
asserts.isequal([1, 2] + sparse(3), [4, 5]);
asserts.isequal(string({'a', 'b'}) + string({'c', 'd'}), string({'ac', 'bd'}));
asserts.isequal(plus([1, 2], [3, 4]), [4, 6]);

% Special floating-point values and the sign of zero must be preserved.
values = [Inf, -Inf, NaN, 0, -0.0];
asserts.istrue(isequaln(values + [1, -1, 2, -0.0, -0.0], [Inf, -Inf, NaN, 0, -0.0]));
zerosResult = [-0.0, -0.0] + [-0.0, 0.0];
asserts.isequal(1 ./ zerosResult, [-Inf, Inf]);
asserts.istrue(isequaln(values + 0, [Inf, -Inf, NaN, 0, 0]));

% Incompatible shapes must continue to report the same error as plus().
try
  unused = plus(ones(2, 3), ones(2, 2));
  error('Expected incompatible sizes.');
catch expected
  assert(~strcmp(expected.message, 'Expected incompatible sizes.'));
end
asserts.checkerror('ones(2,3) + ones(2,2)', expected.message);
asserts.checkerror('ones(2,3) + ones(3,2)', expected.message);

profile('clear');

% A primitive-type overload must still run before any dense arithmetic shortcut.
savedPath = path();
pathCleanup = onCleanup(@() restore_addition_path(savedPath));
directory = tempname();
mkdir(directory);
directoryCleanup = onCleanup(@() rmdir(directory, 's'));
mkdir([directory, '/@double']);
filewrite([directory, '/@double/plus.m'], ...
  {'function out = plus(a, b)', 'out = [701, 702];', 'end'});
addpath(directory);
asserts.isequal([1, 2] + [3, 4], [701, 702]);
asserts.isequal([1, 2] + 3, [701, 702]);
asserts.isequal(3 + [1, 2], [701, 702]);
clear pathCleanup;
asserts.isequal([1, 2] + [3, 4], [4, 6]);
profile('on');
profiledResult = [1, 2] + [3, 4];
profile('off');
info = profile('info');
entries = strcmp({info.FunctionName}, 'plus');
asserts.istrue(any(entries));
asserts.isequal(sum([info(entries).NumCalls]), 1);
asserts.isequal(profiledResult, [4, 6]);
profile('clear');

function restore_addition_path(saved)
  path(saved);
  clear('functions');
end
