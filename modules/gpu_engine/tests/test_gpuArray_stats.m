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
% var / std along a dimension match the host result. The default normalization
% is 1/(N-1); the weight 1 selects 1/N.
x = single([2 4 4 4 5 5 7 9]);
X = gpuArray(x);
tol = 1e-4;
assert_isapprox(gather(var(X)), var(x), tol);
assert_isapprox(gather(var(X, 1)), var(x, 1), tol);
assert_isapprox(gather(std(X)), std(x), tol);
assert_isapprox(gather(std(X, 1)), std(x, 1), tol);
%=============================================================================
% per-column and per-row (dim argument)
M = single([1 2; 3 4; 5 6]);
G = gpuArray(M);
assert_isapprox(gather(var(G)), var(M), tol);
assert_isapprox(gather(var(G, 0, 2)), var(M, 0, 2), tol);
assert_isapprox(gather(std(G, 0, 2)), std(M, 0, 2), tol);
%=============================================================================
% variance of a single element is 0; result class is single; result resident
assert_isequal(gather(var(gpuArray(single(5)))), single(0));
assert_isequal(class(gather(var(X))), 'single');
assert_istrue(isgpuarray(std(X)));
%=============================================================================
% integer and complex inputs are rejected, as on the host
assert_checkerror('gather(var(gpuArray(int8([1 2 3]))))', ...
  _('var/std require a floating-point gpuArray.'));
%=============================================================================
% order statistics and set functions (host fallback), re-wrapped as gpuArray
w = single([3 1 4 1 5 9 2 6 5 3]);
W = gpuArray(w);
assert_isapprox(gather(median(W)), median(w), tol);
% the device path stores single; the host mode() of a single returns double
assert_isequal(gather(mode(W)), single(mode(w)));
assert_isapprox(gather(prctile(W, 25)), prctile(w, 25), tol);
assert_isapprox(gather(prctile(W, [25 50 75])), prctile(w, [25 50 75]), tol);
assert_isequal(gather(unique(W)), unique(w));
assert_isequal(gather(ismember(W, gpuArray(single([1 5])))), ismember(w, [1 5]));
assert_isequal(gather(sortrows(gpuArray(single([3 1; 1 2; 2 0])))), ...
  sortrows(single([3 1; 1 2; 2 0])));
assert_istrue(issorted(gpuArray(single([1 2 3 4]))));
assert_isfalse(issorted(gpuArray(single([3 1 2]))));
assert_isequal(gather(nonzeros(gpuArray(single([0 2 0 3])))), nonzeros(single([0 2 0 3])));
% results stay on the device
assert_istrue(isgpuarray(median(W)));
assert_istrue(isgpuarray(unique(W)));
%=============================================================================
% median, rms and bounds on the device
A = single([3 1 NaN 2; 5 4 6 7; 9 8 0 1]);
G = gpuArray(A);
assert_isequal(gather(median(G)), median(A));
assert_isequal(gather(median(G, 2)), median(A, 2));
assert_isequal(gather(median(G, 'omitnan')), median(A, 'omitnan'));
assert_istrue(isnan(gather(median(G, 'all'))));
assert_isequal(gather(median(gpuArray(single([1 2 3 4])))), single(2.5));
w = rand(1, 10001, 'single');
assert_isequal(gather(median(gpuArray(w))), median(w));
B = A(:, [1 2 4]);
assert_isapprox(gather(rms(gpuArray(B))), rms(B), 1e-6);
assert_isapprox(gather(rms(gpuArray(B), 'all')), rms(B, 'all'), 1e-6);
[s, l] = bounds(G);
[rs, rl] = bounds(A);
assert_isequal(gather(s), rs);
assert_isequal(gather(l), rl);
%=============================================================================
% quantiles, percentiles and interquartile range (reference values)
q = quantile(gpuArray(single([3 1 2 2 5])), [0.1 0.5 0.9 0 1 0.01]);
assert_istrue(isgpuarray(q));
assert_isequal(gather(q), single([1 2 5 1 5 1]));
assert_isapprox(gather(quantile(gpuArray(single(magic(4))), [0.3 0.7])), single([4.7 5.5 5.1 5.9; 11.1 11.9 11.5 12.3]), 1e-5);
assert_isapprox(gather(iqr(gpuArray(single(magic(5))))), single([10 13.75 15 13.75 10]), 1e-5);
assert_isapprox(gather(prctile(gpuArray(single(1:10)), 37)), single(4.2), 1e-5);
X = single(rand(3, 4, 2));
assert_isapprox(gather(quantile(gpuArray(X), [0.2 0.8], 3)), quantile(X, [0.2 0.8], 3), 1e-6);
assert_isequal(gather(quantile(gpuArray(single([1 NaN 3 4])), 0.5)), single(3));
% correlation coefficients
rng(16);
A = single(rand(10, 3));
R = corrcoef(gpuArray(A));
assert_istrue(isgpuarray(R));
assert_isapprox(gather(R), corrcoef(A), 1e-5);
x = single(rand(8, 1));
y = single(rand(8, 1));
assert_isapprox(gather(corrcoef(gpuArray(x), gpuArray(y))), corrcoef(x, y), 1e-5);
C = gather(corrcoef(gpuArray(single([1 1 1 1]')), gpuArray(single([0.2 0.5 0.1 0.9]'))));
assert_isequal(isnan(C), [true true; true false]);
% sign keeps NaN; gpuArray of a device array is that array
assert_isequal(isnan(gather(sign(gpuArray(single([NaN 1]))))), [true false]);
assert_isequal(isnan(gather(arrayfun(@(t) sign(t), gpuArray(single([NaN 1]))))), [true false]);
g = gpuArray(single(3));
assert_isequal(gather(gpuArray(g)), single(3));
%=============================================================================
% missing values
x = single([1 NaN 3 NaN NaN 6]);
m = ismissing(gpuArray(x));
assert_istrue(isgpuarray(m));
assert_isequal(gather(m), isnan(x));
assert_isequal(gather(ismissing(gpuArray(int8([1 2])))), [false false]);
assert_isequal(gather(rmmissing(gpuArray(x))), single([1 3 6]));
assert_istrue(gather(anymissing(gpuArray(x))));
assert_isequal(gather(fillmissing(gpuArray(x), 'previous')), fillmissing(x, 'previous'));
%=============================================================================
% functions written with device operations
rng(2);
x = single(linspace(0, 1, 9));
y = single(sin(3 * x) + 0.1 * rand(1, 9));
v = single([3 1 4 1 5 9 2 6 5 3 5]);
p = polyfit(gpuArray(x), gpuArray(y), 2);
assert_istrue(isgpuarray(p));
assert_isapprox(gather(p), polyfit(x, y, 2), 1e-4);
assert_isequal(gather(mode(gpuArray(v))), mode(v));
assert_isequal(gather(isoutlier(gpuArray(single([1 2 3 2 50 2 1])))), isoutlier(single([1 2 3 2 50 2 1])));
assert_isapprox(gather(xcorr(gpuArray(v), gpuArray(v(end:-1:1)))), xcorr(v, v(end:-1:1)), 1e-4);
assert_isapprox(gather(pchip(gpuArray(x), gpuArray(y), gpuArray(single([0.15 0.5 0.9])))), pchip(x, y, single([0.15 0.5 0.9])), 1e-5);
assert_isapprox(gather(makima(gpuArray(x), gpuArray(y), gpuArray(single([0.15 0.5 0.9])))), makima(x, y, single([0.15 0.5 0.9])), 1e-5);
assert_isequal(gather(movmedian(gpuArray(v), 3)), movmedian(v, 3));
assert_isequal(gather(movmad(gpuArray(v), 3)), movmad(v, 3));
[q, r] = deconv(gpuArray(single([1 5 6 2])), gpuArray(single([1 2])));
assert_isequal(gather(q), single([1 3 0]));
u = uniquetol(gpuArray(single([1 1.0000001 2 3 2.5 3.0000002])));
assert_isequal(gather(u), uniquetol(single([1 1.0000001 2 3 2.5 3.0000002])));
%=============================================================================
% interval tests on the device
a = single([1 2 3 4 NaN]);
for t = {'closed', 'open', 'openleft', 'openright', 'closedleft', 'closedright'}
  g = isbetween(gpuArray(a), 2, 4, t{1});
  assert_istrue(isgpuarray(g));
  assert_isequal(gather(g), isbetween(a, 2, 4, t{1}));
end
assert_isequal(gather(isbetween(gpuArray(a), gpuArray(single([1 2 2 2 2])), 3)), isbetween(a, [1 2 2 2 2], 3));
assert_istrue(isgpuarray(isbetween(gpuArray(single(2)), 1, 3)));
t = allbetween(gpuArray(a(1:4)), 1, 4);
assert_istrue(isgpuarray(t));
assert_istrue(gather(t));
assert_isfalse(gather(allbetween(gpuArray(a), 1, 4)));
%=============================================================================
