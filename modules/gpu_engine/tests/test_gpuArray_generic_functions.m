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
% every generic function listed by gpuArrayFunctions gives the host result on gpuArray inputs
v = [3 1 4 1 5 9 2 6];
M = magic(4) + 4 * eye(4);
A3 = full(spdiags([-ones(12, 1), 3 * ones(12, 1), -0.5 * ones(12, 1)], -1:1, 12, 12));
b3 = (1:12).';
pages = cat(3, [4 1; 2 3], [2 0; 1 5]);
cases = {
  'allunique', @(g) allunique(g([1 2 3]))
  'anymissing', @(g) anymissing(g([1 NaN]))
  'anynan', @(g) anynan(g([1 NaN]))
  'betaincinv', @(g) betaincinv(g([0.1 0.5]), 2, 3)
  'cart2pol', @(g) cart2pol(g([1 2]), g([2 1]))
  'cart2sph', @(g) cart2sph(g([1 2]), g([2 1]), g([1 1]))
  'colperm', @(g) colperm(g([1 0 1 1; 1 1 0 0]))
  'cond', @(g) cond(g(M))
  'convn', @(g) convn(g(reshape(1:8, 2, 2, 2)), g(ones(2, 1, 2)))
  'cov', @(g) cov(g(M))
  'createArray', @(g) createArray(2, 3, 'like', g(1), 'FillValue', 4)
  'curl', @(g) curl(g(magic(4)), g(magic(4)'))
  'deconv', @(g) deconv(g([1 3 5 3]), g([1 1]))
  'del2', @(g) del2(g(M))
  'detrend', @(g) detrend(g(v))
  'fillmissing', @(g) fillmissing(g([1 NaN 3]), 'linear')
  'filter2', @(g) filter2(g([1 2; 3 4]), g(M))
  'freqspace', @(g) freqspace(g(4))
  'gammaincinv', @(g) gammaincinv(g([0.1 0.5]), 2)
  'getrangefromclass', @(g) getrangefromclass(g(single(1)))
  'gradient', @(g) gradient(g(v))
  'gsvd', @(g) gsvd(g([1 2; 3 4; 5 6; 7 8]), g([1 0; 1 1]))
  'head', @(g) head(g(M), 2)
  'height', @(g) height(g(M))
  'im2gray', @(g) im2gray(g(reshape(0.1:0.1:1.2, 2, 2, 3)))
  'ind2sub', @(g) ind2sub([3 4], g(5))
  'inpolygon', @(g) inpolygon(g([0.5 2]), g([0.5 2]), [0 1 1 0], [0 0 1 1])
  'isUnderlyingType', @(g) isUnderlyingType(g(single(1)), 'float')
  'isoutlier', @(g) isoutlier(g([1 2 3 100 2 1]))
  'issorted', @(g) issorted(g(v))
  'isuniform', @(g) isuniform(g([1 2 3]))
  'legendre', @(g) legendre(2, g([0.1 0.5]))
  'lscov', @(g) lscov(g([1 1; 1 2; 1 3]), g([1; 2; 2]))
  'logspace', @(g) logspace(g(0), 2, 3)
  'lsqminnorm', @(g) lsqminnorm(g([1 2; 2 4]), g([1; 2]))
  'makima', @(g) makima(g(0:4), g([0 1 4 9 16]), g(1.5))
  'mape', @(g) mape(g([1 2 4]), g([1 2 3]))
  'mode', @(g) mode(g([1 2 2 3]))
  'movmad', @(g) movmad(g(v), 3)
  'movmedian', @(g) movmedian(g(v), 3)
  'nonzeros', @(g) nonzeros(g([0 1 0 2]))
  'normalize', @(g) normalize(g(v))
  'normest', @(g) normest(g(M))
  'null', @(g) abs(null(g([1 1; 1 1])))
  'numunique', @(g) numunique(g([1 2 2]))
  'orth', @(g) abs(orth(g([1 1; 1 1])))
  'paddata', @(g) paddata(g(1:3), 5)
  'pagectranspose', @(g) pagectranspose(g(reshape(1:8, 2, 2, 2)))
  'pagelsqminnorm', @(g) pagelsqminnorm(g(cat(3, [1 2; 2 4], [1 0; 0 2])), g(cat(3, [1; 2], [1; 1])))
  'pagemldivide', @(g) pagemldivide(g(pages), g(cat(3, [1; 2], [3; 4])))
  'pagemrdivide', @(g) pagemrdivide(g(cat(3, [1 2], [3 4])), g(pages))
  'pagenorm', @(g) pagenorm(g(pages))
  'pagepinv', @(g) pagepinv(g(cat(3, [1 2; 2 4], [1 0; 0 2])))
  'pagesvd', @(g) pagesvd(g(pages))
  'pchip', @(g) pchip(g(0:4), g([0 1 4 9 16]), g(1.5))
  'perms', @(g) perms(g([1 2 3]))
  'pinv', @(g) pinv(g([1 2; 2 4]))
  'pol2cart', @(g) pol2cart(g([0 1]), g([1 2]))
  'poly', @(g) poly(g([1 2; 3 4]))
  'polyarea', @(g) polyarea(g([0 1 1 0]), g([0 0 1 1]))
  'polyder', @(g) polyder(g([1 2 3]))
  'polyfit', @(g) polyfit(g(0:4), g([1 3 7 13 21]), 2)
  'polyint', @(g) polyint(g([1 2 3]))
  'polyvalm', @(g) polyvalm([1 2 3], g([1 2; 3 4]))
  'ppval', @(g) ppval(spline(0:4, (0:4) .^ 2), g([0.5 1.5]))
  'psi', @(g) psi(g([0.5 1 2]))
  'rank', @(g) rank(g([1 2; 2 4]))
  'rectint', @(g) rectint(g([0 0 2 2]), g([1 1 2 2]))
  'rescale', @(g) rescale(g(v))
  'resize', @(g) resize(g(1:3), 5)
  'rmmissing', @(g) rmmissing(g([1 NaN 3]))
  'rmoutliers', @(g) rmoutliers(g([1 2 3 100 2 1]))
  'rmse', @(g) rmse(g([1 2 3]), g([1 2 4]))
  'sortrows', @(g) sortrows(g([3 1; 1 2; 2 0]))
  'sph2cart', @(g) sph2cart(g([0 1]), g([0 0.5]), g([1 2]))
  'subspace', @(g) subspace(g([1; 0]), g([1; 1]))
  'tail', @(g) tail(g(M), 2)
  'tensorprod', @(g) tensorprod(g([1 2; 3 4]), g([1 0; 0 1]), 2, 1)
  'tfqmr', @(g) tfqmr(g(A3), g(b3), 1e-6, 20)
  'trimdata', @(g) trimdata(g(1:6), 4)
  'uniquetol', @(g) uniquetol(g([1 1.5 1.5 2]))
  'unwrap', @(g) unwrap(g([0 3 6.5 -3]))
  'width', @(g) width(g(M))
  'xcorr', @(g) xcorr(g([1 2 3]))
  'xcov', @(g) xcov(g([1 2 3]))
};
listed = gpuArrayFunctions();
for k = 1:size(cases, 1)
  name = cases{k, 1};
  assert_istrue(any(strcmp(listed, name)), name);
  expected = cases{k, 2}(@(x) x);
  computed = gather(cases{k, 2}(@gpuArray));
  assert_isequal(size(computed), size(expected), name);
  if isnumeric(expected)
    scale = max(1, max(abs(double(expected(:)))));
    assert_istrue(max(abs(double(computed(:)) - double(expected(:)))) <= 1e-4 * scale, name);
  else
    assert_isequal(computed, expected, name);
  end
end
%=============================================================================
