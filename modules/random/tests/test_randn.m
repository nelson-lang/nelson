%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng('default');
r = rng();
R = randn(5);
REF = [0.5377 -1.3077 -1.3499 -0.2050 0.6715;
    1.8339 -0.4336 3.0349 -0.1241 -1.2075;
   -2.2588 0.3426 0.7254 1.4897 0.7172;
    0.8622 3.5784 -0.0631 1.4090 1.6302;
    0.3188 2.7694 0.7147 1.4172 0.4889];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
rng('default');
R = randn(3, 2, 'single');
REF = [0.5377 0.8622;
    1.8339 0.3188;
   -2.2588 -1.3077];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = randn(2, 0, 3);
asserts.isequal(size(R), [2 0 3]);
asserts.isequal(class(R), 'double');
%=============================================================================
R = randn([2 0 3], 'single');
asserts.isequal(size(R), [2 0 3]);
asserts.isequal(class(R), 'single');
%=============================================================================
R = randn([2 0], 'like', single(3i));
asserts.isequal(size(R), [2 0]);
asserts.isequal(class(R), 'single');
asserts.isfalse(isreal(R));
%=============================================================================
savedRandomState = rng();
rng('default');
normalValues = randn(2, 2);
asserts.isequal(size(normalValues), [2 2]);
asserts.istrue(all(isfinite(normalValues(:))));
rng(savedRandomState);
%=============================================================================
