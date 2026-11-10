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
% randi with the gpuArray typename gives integers in range on the device
R = randi(5, 2, 3, 'gpuArray');
assert_isequal(class(R), 'gpuArray');
assert_isequal(size(R), [2 3]);
g = double(gather(R));
assert_istrue(all(g(:) >= 1 & g(:) <= 5 & g(:) == round(g(:))));
R = randi([3 4], 1, 4, 'int8', 'gpuArray');
assert_isequal(class(R), 'gpuArray');
assert_isequal(underlyingType(R), 'int8');
assert_checkerror('randi(5, ''foo'')', sprintf(_('Unknown typename: %s.'), 'foo'), 'Nelson:randi:unknownTypename');
%=============================================================================
% eps of a gpuArray is element-wise, constants follow a gpuArray prototype
R = eps(gpuArray(single([1 2 4])));
assert_isequal(class(R), 'gpuArray');
assert_isequal(gather(R), eps(single([1 2 4])));
R = intmax('like', gpuArray(int8(1)));
assert_isequal(class(R), 'gpuArray');
assert_isequal(gather(R), int8(127));
R = realmax('like', gpuArray(single(1)));
assert_isequal(class(R), 'gpuArray');
assert_isequal(gather(R), realmax('single'));
%=============================================================================
