%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('exprnd'), -1);
asserts.isequal(nargout('exprnd'), 1);
%=============================================================================
rng(0);
R = exprnd(2, 2, 3);
REF = [3.371813962263367 0.2716092432909177 2.001298060376156;
       4.724499014771342 4.892353409599266 0.2052627197195187];
asserts.isapprox(R, REF, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 4);
E = -2 .* log1p(-U);
rng(0);
R = exprnd(2, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(1, 3);
E = -[1 2 3] .* log1p(-U);
rng(0);
R = exprnd([1 2 3]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 3, 'single');
E = -single(2) .* log1p(-U);
rng(0);
R = exprnd(single(2), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), double(E), 1e-6);
%=============================================================================
R = exprnd(0, [1 3]);
asserts.istrue(all(isnan(R)));
R = exprnd(-1, [1 3]);
asserts.istrue(all(isnan(R)));
R = exprnd(NaN, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = exprnd(1, 0, 3);
asserts.isequal(size(R), [0 3]);
R = exprnd(single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = exprnd(1, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('exprnd([1 2], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('exprnd(1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('exprnd(1, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('exprnd({1})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
