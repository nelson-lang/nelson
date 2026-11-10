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
% every length: powers of two, primes and other composites
for n = [1 2 3 5 7 8 12 16 17 100 127 128 1000 1024 4097]
  x = single(rand(n, 1) + 1i * rand(n, 1));
  r = single(rand(n, 1));
  ref = fft(double(x));
  assert_istrue(max(abs(double(gather(fft(gpuArray(x)))) - ref)) / max(1, max(abs(ref))) < 2e-6);
  ref = fft(double(r));
  assert_istrue(max(abs(double(gather(fft(gpuArray(r)))) - ref)) / max(1, max(abs(ref))) < 2e-6);
  ref = ifft(double(x));
  assert_istrue(max(abs(double(gather(ifft(gpuArray(x)))) - ref)) / max(1, max(abs(ref))) < 2e-6);
end
%=============================================================================
% dimension, padding and truncation, 2-D and N-D transforms
M = single(rand(6, 10));
G = gpuArray(M);
r = fft(G);
assert_istrue(isgpuarray(r));
assert_isapprox(double(gather(r)), fft(double(M)), 1e-5);
assert_isapprox(double(gather(fft(G, [], 2))), fft(double(M), [], 2), 1e-5);
assert_isapprox(double(gather(fft(G, 9))), fft(double(M), 9), 1e-5);
assert_isapprox(double(gather(fft(G, 4))), fft(double(M), 4), 1e-5);
assert_isapprox(double(gather(fft2(G))), fft2(double(M)), 1e-5);
assert_isapprox(double(gather(fftn(G))), fftn(double(M)), 1e-5);
assert_isapprox(double(gather(ifft2(fft2(G)))), double(M), 1e-5);
assert_isapprox(double(gather(ifftn(fftn(G)))), double(M), 1e-5);
assert_isequal(size(fft(gpuArray(single([1 2 3])), 5)), [1 5]);
assert_isequal(size(fft(gpuArray(zeros(0, 3, 'single')))), [0 3]);
assert_isequal(size(fft(gpuArray(single([1; 2; 3])))), [3 1]);
%=============================================================================
% the spectrum of real data is conjugate symmetric, so its inverse is real again
assert_istrue(isreal(gather(ifft(fft(gpuArray(single([1 2 3])))))));
assert_istrue(isreal(gather(ifft2(fft2(G)))));
assert_isfalse(isreal(gather(ifft(gpuArray(single([1 2 3]))))));
assert_isfalse(isreal(gather(ifft(gpuArray(single([1, 2 + 1i, 3]))))));
assert_istrue(isreal(gather(ifft(gpuArray(single([1 2 3])), 'symmetric'))));
assert_istrue(isreal(gather(fft(gpuArray(single(5))))));
s = gather(fft(gpuArray(single([1 2 3 4 5]))));
assert_isequal(s(2:3), conj(s([5 4])));
%=============================================================================
