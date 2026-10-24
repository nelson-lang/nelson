%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
% zeros(...,'gpuArray') / ones(...,'gpuArray') build the array directly on the
% device (the array-creation function routes to the class's own constructor).
Z = zeros(2, 3, 'gpuArray');
assert_istrue(isgpuarray(Z));
assert_isequal(gather(Z), single(zeros(2, 3)));
assert_isequal(size(gather(Z)), [2 3]);
O = ones(3, 'gpuArray');
assert_istrue(isgpuarray(O));
assert_isequal(gather(O), single(ones(3)));
%=============================================================================
% size-vector form and square form
assert_isequal(gather(zeros([2 4], 'gpuArray')), single(zeros(2, 4)));
assert_isequal(gather(ones([3 1], 'gpuArray')), single(ones(3, 1)));
assert_isequal(size(gather(zeros(4, 'gpuArray'))), [4 4]);
%=============================================================================
% 'like' a gpuArray prototype also builds a gpuArray
P = gpuArray(single(rand(2, 2)));
Zl = zeros(2, 2, 'like', P);
Ol = ones(2, 2, 'like', P);
assert_istrue(isgpuarray(Zl));
assert_istrue(isgpuarray(Ol));
assert_isequal(gather(Zl), single(zeros(2, 2)));
assert_isequal(gather(Ol), single(ones(2, 2)));
%=============================================================================
% device-resident results are usable by the other gpuArray operations
R = zeros(2, 2, 'gpuArray') + ones(2, 2, 'gpuArray');
assert_isequal(gather(R), single(ones(2, 2)));
assert_isequal(gather(sum(ones(4, 'gpuArray'), 1)), single(4 * ones(1, 4)));
%=============================================================================
% rand(...,'gpuArray') / randn(...,'gpuArray') build a device random array
R = rand(2000, 1, 'gpuArray');
assert_istrue(isgpuarray(R));
assert_isequal(size(gather(R)), [2000 1]);
hr = gather(R);
assert_istrue(all(hr >= 0) && all(hr < 1)); % uniform [0,1)
assert_istrue(abs(mean(hr) - 0.5) < 0.05);
N = gather(randn(5000, 1, 'gpuArray'));
assert_istrue(abs(mean(N)) < 0.1); % standard normal
assert_istrue(abs(std(N) - 1) < 0.1);
% successive calls advance the stream (different values)
assert_isfalse(isequal(gather(rand(4, 1, 'gpuArray')), gather(rand(4, 1, 'gpuArray'))));
% 'like' form
assert_istrue(isgpuarray(rand(2, 2, 'like', gpuArray(single(1)))));
%=============================================================================
% eye / nan / inf / true / false on the device
assert_isequal(gather(eye(3, 'gpuArray')), single(eye(3)));
assert_isequal(gather(eye(2, 3, 'gpuArray')), single(eye(2, 3)));
assert_istrue(isgpuarray(eye(2, 'gpuArray')));
assert_istrue(all(isnan(gather(nan(2, 2, 'gpuArray'))(:))));
Ii = gather(inf(2, 2, 'gpuArray'));
assert_istrue(all(isinf(Ii(:))) && all(Ii(:) > 0));
T = true(2, 3, 'gpuArray');
assert_isequal(underlyingType(T), 'logical');
assert_isequal(gather(T), true(2, 3));
assert_isequal(gather(false(2, 'gpuArray')), false(2));
% 'like' forms
assert_istrue(isgpuarray(eye(2, 'like', gpuArray(single(1)))));
assert_istrue(isgpuarray(true(2, 'like', gpuArray(true))));
assert_istrue(all(isnan(gather(nan(2, 'like', gpuArray(single(1))))(:))));
%=============================================================================
% zeros/ones/eye 'like' propagate the integer/logical class of the prototype
intClasses = {'int8', 'int16', 'int32', 'uint8', 'uint32'};
for k = 1:numel(intClasses)
  cls = intClasses{k};
  P = gpuArray(cast(1, cls));
  assert_isequal(underlyingType(zeros(2, 2, 'like', P)), cls);
  assert_isequal(gather(zeros(2, 2, 'like', P)), cast(zeros(2, 2), cls));
  assert_isequal(underlyingType(ones(2, 3, 'like', P)), cls);
  assert_isequal(gather(ones(2, 3, 'like', P)), cast(ones(2, 3), cls));
  assert_isequal(gather(eye(3, 'like', P)), cast(eye(3), cls));
end
assert_isequal(underlyingType(zeros(2, 'like', gpuArray(true))), 'logical');
% the string form stays single
assert_isequal(underlyingType(zeros(2, 'gpuArray')), 'single');
%=============================================================================
% the core constructors are unchanged for the built-in types
assert_isequal(class(zeros(2, 3)), 'double');
assert_isequal(class(ones(2, 2, 'int8')), 'int8');
assert_isequal(class(rand(2, 2)), 'double');
assert_isequal(class(rand(2, 2, 'single')), 'single');
assert_isequal(class(randn(2, 2)), 'double');
assert_isequal(class(eye(3)), 'double');
assert_isequal(class(nan(2)), 'double');
assert_isequal(class(inf(2)), 'double');
assert_isequal(class(true(2)), 'logical');
assert_isequal(class(false(2)), 'logical');
%=============================================================================
