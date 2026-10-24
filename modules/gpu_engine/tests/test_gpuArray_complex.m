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
a = single(rand(4, 5)) + 1i * single(rand(4, 5));
b = single(rand(4, 5)) + 1i * single(rand(4, 5));
A = gpuArray(a);
B = gpuArray(b);
tol = 1e-4;
%=============================================================================
% round-trip and introspection
assert_istrue(isgpuarray(A));
assert_isfalse(isreal(A));
assert_isapprox(gather(A), a, tol);
assert_isequal(class(gather(A)), 'single');
assert_istrue(isreal(gpuArray(single(rand(3)))));
%=============================================================================
% complex arithmetic
assert_isapprox(gather(A + B), a + b, tol);
assert_isapprox(gather(A - B), a - b, tol);
assert_isapprox(gather(A .* B), a .* b, tol);
assert_isapprox(gather(A ./ B), a ./ b, tol);
assert_isapprox(gather(-A), -a, tol);
assert_istrue(isgpuarray(A + B));
assert_isfalse(isreal(A .* B));
%=============================================================================
% mixed real/complex
r = single(rand(4, 5));
R = gpuArray(r);
assert_isapprox(gather(A + R), a + r, tol);
assert_isapprox(gather(R .* A), r .* a, tol);
assert_isapprox(gather(A + 2), a + 2, tol);
%=============================================================================
% real / imag / conj / abs / angle
assert_isapprox(gather(real(A)), real(a), tol);
assert_isapprox(gather(imag(A)), imag(a), tol);
assert_isapprox(gather(conj(A)), conj(a), tol);
assert_isapprox(gather(abs(A)), abs(a), tol);
assert_isapprox(gather(angle(A)), angle(a), tol);
assert_istrue(isreal(real(A)));
assert_istrue(isreal(abs(A)));
assert_isfalse(isreal(conj(A)));
%=============================================================================
% real input to real/imag/conj/angle behaves correctly
assert_isapprox(gather(real(R)), r, tol);
assert_isapprox(gather(imag(R)), zeros(4, 5, 'single'), tol);
assert_isapprox(gather(conj(R)), r, tol);
%=============================================================================
% reorganization preserves complex data
assert_isapprox(gather(A.'), a.', tol); % non-conjugate transpose
assert_isapprox(gather(A'), a', tol); % conjugate transpose
assert_isapprox(gather(reshape(A, 5, 4)), reshape(a, 5, 4), tol);
assert_isapprox(gather(A(2, 3)), a(2, 3), tol);
assert_isapprox(gather(A([1 3], [2 4])), a([1 3], [2 4]), tol);
assert_isapprox(gather(A(:)), a(:), tol);
assert_isapprox(gather([A, B]), [a, b], tol);
assert_isapprox(gather([A; B]), [a; b], tol);
assert_isfalse(isreal(A.'));
%=============================================================================
% indexed assignment into a complex array preserves complex data
C = A;
C(1, 1) = single(7 - 3i);
ref = a; ref(1, 1) = 7 - 3i;
assert_isapprox(gather(C), ref, tol);
%=============================================================================
% complex reductions
assert_isapprox(gather(sum(A)), sum(a), tol);
assert_isapprox(gather(sum(A, 2)), sum(a, 2), tol);
% NB: host mean() is currently wrong for complex inputs, so compare the GPU
% mean against the mathematically correct sum/n instead of host mean().
assert_isapprox(gather(mean(A)), sum(a) / size(a, 1), 1e-3);
% prod of [1+1i, 2-1i, 1i] = -1+3i (computed by hand)
assert_isapprox(gather(prod(gpuArray(single([1 + 1i, 2 - 1i, 1i])))), single(-1 + 3i), 1e-3);
assert_isfalse(isreal(sum(A)));
%=============================================================================
% complex matrix product (and mixed real/complex)
p = single(rand(4, 3)) + 1i * single(rand(4, 3));
q = single(rand(3, 5)) + 1i * single(rand(3, 5));
P = gpuArray(p); Q = gpuArray(q);
assert_isapprox(gather(P * Q), p * q, 1e-3);
r2 = single(rand(3, 5));
assert_isapprox(gather(P * gpuArray(r2)), p * r2, 1e-3); % complex * real
%=============================================================================
% complex elementwise math
assert_isapprox(gather(exp(A)), exp(a), 1e-4);
assert_isapprox(gather(log(A)), log(a), 1e-4);
assert_isapprox(gather(sqrt(A)), sqrt(a), 1e-4);
assert_isapprox(gather(sin(A)), sin(a), 1e-4);
assert_isapprox(gather(cos(A)), cos(a), 1e-4);
assert_isapprox(gather(tan(A)), tan(a), 1e-3);
assert_isapprox(gather(A .^ B), a .^ b, 1e-3);
assert_isfalse(isreal(exp(A)));
%=============================================================================
% complex equality / inequality (compare real and imaginary parts) -> logical
assert_isequal(gather(A == A), a == a);
assert_isequal(gather(A == B), a == b);
assert_isequal(gather(A ~= B), a ~= b);
assert_isequal(underlyingType(A == B), 'logical');
%=============================================================================
% complex cumulative scans
assert_isapprox(gather(cumsum(A)), cumsum(a), 1e-4);
assert_isapprox(gather(cumsum(A, 2)), cumsum(a, 2), 1e-4);
assert_isapprox(gather(cumprod(A)), cumprod(a), 1e-2);
%=============================================================================
% any / all on complex (element is true when nonzero: re ~= 0 or im ~= 0)
m = single([0, 1 + 0i, 0 + 2i; 0, 0, 3 - 1i]);
M = gpuArray(m);
assert_isequal(gather(any(M)), any(m));
assert_isequal(gather(all(M)), all(m));
assert_isequal(gather(any(M, 2)), any(m, 2));
assert_isequal(underlyingType(any(M)), 'logical');
%=============================================================================
% find on complex (non-zero real or imaginary part)
assert_isequal(gather(find(M)), single(find(m)));
% max / min by magnitude (complex ordering), single output
z = single([1 + 1i, 3 + 0i, 0 + 4i, 2 - 2i]);
Z = gpuArray(z);
assert_isapprox(gather(max(Z)), max(z), 1e-4);
assert_isapprox(gather(min(Z)), min(z), 1e-4);
% sort by magnitude, values match host sort
assert_isapprox(gather(sort(Z)), sort(z), 1e-4);
assert_isapprox(gather(sort(Z, 'descend')), sort(z, 'descend'), 1e-4);
assert_isfalse(isreal(sort(Z)));
%=============================================================================
% unsupported complex ops still error with no silent corruption
assert_checkerror('gather(A > B)', _('This operation is not supported for complex gpuArray yet.'));
%=============================================================================
% rounding / fix apply to each component
zr = single([1.7 + 2.3i, -3.2 - 4.8i, 5.5 - 6.5i]);
Zr = gpuArray(zr);
assert_isequal(gather(floor(Zr)), floor(zr));
assert_isequal(gather(ceil(Zr)), ceil(zr));
assert_isequal(gather(round(Zr)), round(zr)); % round half away from zero
assert_isequal(gather(fix(Zr)), fix(zr));
%=============================================================================
% sign(z) = z / |z|, and sign(0) = 0
assert_isapprox(gather(sign(gpuArray(single(3 + 4i)))), sign(single(3 + 4i)), 1e-5);
assert_isequal(gather(sign(gpuArray(single(0 + 0i)))), single(0));
%=============================================================================
% NaN / Inf predicates test both components (logical result)
zp = single([1 + 2i, complex(NaN, 1), complex(1, Inf), complex(Inf, NaN)]);
Zp = gpuArray(zp);
assert_isequal(gather(isnan(Zp)), isnan(zp));
assert_isequal(gather(isinf(Zp)), isinf(zp));
assert_isequal(gather(isfinite(Zp)), isfinite(zp));
assert_isequal(class(gather(isnan(Zp))), 'logical');
%=============================================================================
% mod / rem of a complex gpuArray is rejected (as on the host)
assert_checkerror('gather(mod(gpuArray(single(1+1i)), gpuArray(single(2))))', ...
  _('Argument must be real.'));
%=============================================================================
