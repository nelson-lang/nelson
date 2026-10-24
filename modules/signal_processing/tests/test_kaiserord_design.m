%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[n, wn, beta, ftype] = kaiserord([0.2 0.3], [1 0], [0.01 0.01]);
asserts.isequal(n, 45);
asserts.isapprox(wn, 0.25, 1e-12);
asserts.isapprox(beta, 3.39532105226146, 1e-12);
asserts.isequal(ftype, 'low');
%=============================================================================
[n, wn, beta, ftype] = kaiserord([0.2 0.3], [1 0], [0.01 0.001]);
asserts.isequal(n, 73);
asserts.isapprox(wn, 0.25, 1e-12);
asserts.isapprox(beta, 5.65326, 1e-12);
asserts.isequal(ftype, 'low');
%=============================================================================
[n, wn, beta, ftype] = kaiserord([0.2 0.3], [0 1], [0.001 0.01]);
asserts.isequal(n, 74);
asserts.isapprox(wn, 0.25, 1e-12);
asserts.isapprox(beta, 5.65326, 1e-12);
asserts.isequal(ftype, 'high');
%=============================================================================
[n, wn, beta, ftype] = kaiserord([0.1 0.2 0.4 0.5], [0 1 0], [0.001 0.01 0.001]);
asserts.isequal(n, 73);
asserts.isapprox(wn, [0.15 0.45], 1e-12);
asserts.isapprox(beta, 5.65326, 1e-12);
asserts.isequal(ftype, 'DC-0');
%=============================================================================
[n, wn, beta, ftype] = kaiserord([0.1 0.2 0.4 0.5], [1 0 1], [0.01 0.001 0.01]);
asserts.isequal(n, 74);
asserts.isapprox(wn, [0.15 0.45], 1e-12);
asserts.isapprox(beta, 5.65326, 1e-12);
asserts.isequal(ftype, 'stop');
%=============================================================================
[n, wn, beta, ftype] = kaiserord([1000 1500], [1 0], [0.01 0.001], 8000);
asserts.isequal(n, 59);
asserts.isapprox(wn, 0.3125, 1e-12);
asserts.isapprox(beta, 5.65326, 1e-12);
asserts.isequal(ftype, 'low');
%=============================================================================
