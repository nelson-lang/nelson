%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = sparse([1 + i, -i; i, 2i]);
r = conj(x);
ref = sparse([1 - i, i; -i, -2i]);
asserts.isequal(r, ref);
%=============================================================================
x = sparse(single([1 + 2i 0; -3i 4]));
r = conj(x);
asserts.istrue(issparse(r));
asserts.isequal(class(r), 'single');
ref = single([1 - 2i 0; 3i 4]);
asserts.isapprox(full(r), ref, 1e-6);
%=============================================================================
x = sparse(single([1 0; 0 -4]));
r = conj(x);
asserts.istrue(issparse(r));
asserts.isequal(class(r), 'single');
asserts.isequal(full(r), single([1 0; 0 -4]));
%=============================================================================
