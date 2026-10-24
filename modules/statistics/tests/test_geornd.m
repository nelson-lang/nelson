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
asserts.isequal(nargin('geornd'), -1);
asserts.isequal(nargout('geornd'), 1);
%=============================================================================
rng(0);
R = geornd(0.25, 2, 3);
asserts.isapprox(R, [5 0 3; 8 8 0], 1e-15);
%=============================================================================
rng(0);
U = rand(2, 4);
E = floor(log1p(-U) ./ log1p(-0.25));
rng(0);
R = geornd(0.25, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.isapprox(R, E, 1e-15);
%=============================================================================
rng(0);
U = rand(1, 3);
E = floor(log1p(-U) ./ log1p(-[0.25 0.5 0.75]));
rng(0);
R = geornd([0.25 0.5 0.75]);
asserts.isapprox(R, E, 1e-15);
%=============================================================================
rng(0);
U = rand(2, 3, 'single');
E = floor(log1p(-U) ./ log1p(-single(0.25)));
rng(0);
R = geornd(single(0.25), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), double(E), 1e-6);
%=============================================================================
R = geornd(0, [1 3]);
asserts.istrue(all(isinf(R)));
R = geornd(1, [1 3]);
asserts.isapprox(R, zeros(1, 3), 1e-15);
R = geornd(-0.1, [1 3]);
asserts.istrue(all(isnan(R)));
R = geornd(1.1, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = geornd(0.5, 0, 3);
asserts.isequal(size(R), [0 3]);
R = geornd(single(0.5), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
R = geornd(0.5, -1, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('geornd([0.25 0.5], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('geornd(0.5, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('geornd(0.5, Inf, 2)', _('Size must contain integer values.'));
asserts.checkerror('geornd({0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
