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
g = gpuArray(single([0.5 1 1.5 2 2.5 3 NaN -1 4 3]));
%=============================================================================
% histcounts with explicit edges: [e(k), e(k+1)), last bin closed, row result
r = histcounts(g, [0 1 2 3]);
assert_istrue(isgpuarray(r));
assert_isequal(gather(r), single([1 2 4]));
assert_isequal(gather(histcounts(g, [1 2 3 4])), single([2 2 3]));
assert_isequal(size(histcounts(g', [0 1 2 3])), [1 3]);
% automatic edges are chosen on the host
assert_isequal(gather(histcounts(g, 3)), single([2 3 4]));
assert_isequal(gather(histcounts(g)), single([1 5 3]));
w = rand(1e6, 1, 'single');
e = single(0:0.1:1);
assert_isequal(gather(histcounts(gpuArray(w), e)), single(histcounts(w, e)));
%=============================================================================
% discretize: bin number, NaN outside the edges
r = gather(discretize(g, [0 1 2 3]));
assert_isequal(isnan(r), [false false false false false false true true true false]);
assert_isequal(r(~isnan(r)), single([1 2 2 3 3 3 3]));
%=============================================================================
