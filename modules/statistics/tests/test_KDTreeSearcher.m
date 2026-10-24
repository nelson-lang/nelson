%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% construction stores the data and default options
X = [0 0; 1 0; 0 1; 1 1; 2 2];
mdl = KDTreeSearcher(X);
assert_isequal(mdl.X, X);
assert_isequal(mdl.Distance, 'euclidean');
assert_isequal(mdl.BucketSize, 50);
%=============================================================================
% knnsearch through the searcher matches the direct builtin call
Y = [0.1 0.1; 1.9 1.9];
[idx, d] = knnsearch(mdl, Y, 'K', 2);
[idxRef, dRef] = knnsearch(X, Y, 'K', 2);
assert_isequal(idx, idxRef);
assert_isapprox(d, dRef, 1e-12);
%=============================================================================
% nearest neighbor of a point coinciding with a data row is that row, dist 0
[idx1, d1] = knnsearch(mdl, [1 1], 'K', 1);
assert_isequal(idx1, 4);
assert_istrue(abs(d1) < 1e-12);
%=============================================================================
% rangesearch through the searcher matches the direct builtin call
[ri, rd] = rangesearch(mdl, Y, 0.5);
[riRef, rdRef] = rangesearch(X, Y, 0.5);
assert_isequal(ri, riRef);
assert_isequal(rd, rdRef);
%=============================================================================
% a non-euclidean distance set on the searcher is forwarded
mdlC = KDTreeSearcher(X, 'Distance', 'cityblock');
assert_isequal(mdlC.Distance, 'cityblock');
[idxC, dC] = knnsearch(mdlC, [0.2 0.2], 'K', 1);
[idxCRef, dCRef] = knnsearch(X, [0.2 0.2], 'K', 1, 'Distance', 'cityblock');
assert_isequal(idxC, idxCRef);
assert_isapprox(dC, dCRef, 1e-12);
%=============================================================================
% a non-numeric data matrix is rejected
assert_checkerror('KDTreeSearcher({1, 2})', _('X must be a real numeric matrix.'));
%=============================================================================
