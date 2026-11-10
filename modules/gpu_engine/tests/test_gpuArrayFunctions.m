%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% gpuArrayFunctions lists the functions supported on a gpuArray, with no GPU device needed
c = gpuArrayFunctions();
assert_isequal(class(c), 'cell');
assert_istrue(iscolumn(c));
assert_istrue(numel(c) > 40);
%=============================================================================
% the names are plain (no '@gpuArray/' prefix) and sorted with no duplicates
for k = 1:numel(c)
  assert_isequal(class(c{k}), 'char');
  assert_istrue(isempty(strfind(c{k}, '@')));
end
assert_isequal(c, sort(c));
assert_isequal(numel(c), numel(unique(c)));
%=============================================================================
% a representative set is present: builtin overloads and .m method overloads
expected = {'plus', 'minus', 'times', 'mtimes', 'sum', 'prod', 'mean', ...
  'sort', 'find', 'abs', 'sqrt', 'floor', 'mod', 'zeros', 'ones', 'rand', ...
  'dot', 'norm', 'eig', 'svd', 'inv', 'fft'};
for k = 1:numel(expected)
  assert_istrue(any(strcmp(c, expected{k})), expected{k});
end
%=============================================================================
% generic functions without a gpuArray method are listed too, and all exist
generic = {'psi', 'gammaincinv', 'betaincinv', 'legendre', 'lscov', 'lsqminnorm', 'gsvd', 'tfqmr', ...
  'pagemldivide', 'pagesvd', 'convn', 'trimdata', 'paddata', 'inpolygon', 'createArray', 'rmse', 'mape', ...
  'anynan', 'numunique', 'isuniform', 'pinv', 'null', 'orth', 'rank', 'normalize', 'rescale'};
for k = 1:numel(generic)
  assert_istrue(any(strcmp(c, generic{k})), generic{k});
  assert_istrue(exist(generic{k}) > 0, generic{k});
end
%=============================================================================
