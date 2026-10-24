%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('evrnd'), -1);
asserts.isequal(nargout('evrnd'), 1);
%=============================================================================
rng(0);
R = evrnd(0, 1, 2, 3);
E = [0.5223036867241486 -1.996538031701304 0.0006488196590738350;
     0.8596143461445075 0.8945262770103424 -2.276611741292844];
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 4);
E = evinv(U, 0, 1);
rng(0);
R = evrnd(0, 1, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(1, 3);
E = evinv(U, [0 1 2], [1 2 3]);
rng(0);
R = evrnd([0 1 2], [1 2 3]);
asserts.isapprox(R, E, 1e-14);
%=============================================================================
rng(0);
U = rand(2, 3, 'single');
E = evinv(U, single(0), single(1));
rng(0);
R = evrnd(single(0), single(1), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), double(E), 1e-6);
%=============================================================================
R = evrnd(0, 0, [1 3]);
asserts.istrue(all(isnan(R)));
R = evrnd(0, -1, [1 3]);
asserts.istrue(all(isnan(R)));
R = evrnd(NaN, 1, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = evrnd(0, 1, 0, 3);
asserts.isequal(size(R), [0 3]);
R = evrnd(single(0), single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = evrnd(0, 1, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('evrnd(0)', _('Not enough input arguments.'));
asserts.checkerror('evrnd([0 1], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('evrnd(0, 1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('evrnd(0, 1, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('evrnd({0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('evrnd(0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
