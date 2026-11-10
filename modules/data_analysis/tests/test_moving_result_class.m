%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the class of moving statistics follows the data
v = single([3 1 4 1 5 9 2]);
w = int8([3 1 4 1 5 9 2]);
b = [true false true true false true false];
names = {'movmedian', 'movmad', 'movsum', 'movmean', 'movmax', 'movmin', 'movprod', 'movvar', 'movstd'};
for k = 1:numel(names)
  assert_isequal(class(feval(names{k}, v, 3)), 'single');
end
assert_isequal(class(movmedian(w, 3)), 'int8');
assert_isequal(class(movmad(w, 3)), 'int8');
assert_isequal(class(movmax(w, 3)), 'int8');
assert_isequal(class(movmin(w, 3)), 'int8');
assert_isequal(class(movsum(w, 3)), 'double');
assert_isequal(class(movmean(w, 3)), 'double');
assert_isequal(class(movprod(w, 3)), 'double');
assert_isequal(class(movmax(b, 3)), 'logical');
assert_isequal(class(movmin(b, 3)), 'logical');
assert_isequal(class(movmedian(b, 3)), 'double');
assert_isequal(movmedian(w, 3), int8([2 3 1 4 5 5 6]));
assert_isapprox(double(movvar(v, 3)), [2 7/3 3 13/3 16 37/3 24.5], 1e-5);
assert_checkerror('movvar(int8([1 2 3]), 3)', _('The data must be single or double.'));
assert_checkerror('movstd(int8([1 2 3]), 3)', _('The data must be single or double.'));
%=============================================================================
