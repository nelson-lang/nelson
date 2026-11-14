%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Mixed double/single transposed solve: the operator narrows to single first.
Ad = reshape(1:16, 4, 4) / 4 + 4 * eye(4);
As = single(Ad);
bd = (1:4)';
bs = single(bd);
%=============================================================================
% single' \ double narrows the right-hand side to single
asserts.isequal(class(As' \ bd), 'single');
asserts.isequal(As' \ bd, As' \ bs);
%=============================================================================
% double' \ single narrows the matrix to single
asserts.isequal(class(Ad' \ bs), 'single');
asserts.isequal(Ad' \ bs, As' \ bs);
%=============================================================================
% the narrowed solve still factors A itself: A' \ b == (b' / A)'
asserts.isequal(As' \ bd, (single(bd)' / As)');
%=============================================================================
% .' on a real mixed operand behaves the same
asserts.isequal(As.' \ bd, As.' \ bs);
asserts.isequal(class(As.' \ bd), 'single');
%=============================================================================
% complex mixed operands narrow to single complex, imaginary part kept
Ac = single(Ad) + 2i * single(flipud(Ad));
bc = bd + 1i * (4:-1:1)';
asserts.isequal(class(Ac' \ bc), 'single');
asserts.isequal(Ac' \ bc, Ac' \ single(bc));
asserts.istrue(~isreal(Ac' \ bc));
%=============================================================================
% the function forms are not fused: they keep the materialised transpose result
At = As';
asserts.isequal(mldivide(As', bd), At \ bd);
asserts.isequal(ctranspose(As) \ bd, At \ bd);
asserts.isequal(class(mldivide(As', bd)), 'single');
%=============================================================================
% pure double stays double
asserts.isequal(class(Ad' \ bd), 'double');
%=============================================================================
