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
%=============================================================================
res = randi(3, 3);
ref = [3 3 1;
     3 2 2;
     1 1 3];
asserts.isapprox(res, ref, 1e-4);
%=============================================================================
res = randi(3, 3);
ref = [3 3 1;
     1 2 2;
     3 3 3];
asserts.isapprox(res, ref, 1e-4);
%=============================================================================
savedRandomState = rng();
rng('default');
integerValues = randi(5, 2, 3);
asserts.isequal(size(integerValues), [2 3]);
asserts.istrue(all(integerValues(:) >= 1));
asserts.istrue(all(integerValues(:) <= 5));
rng(savedRandomState);
%=============================================================================
