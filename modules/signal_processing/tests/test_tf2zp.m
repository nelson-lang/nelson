%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[z, p, k] = tf2zp([1 2 1], [1 -0.5]);
% [1 2 1] has a double zero at -1; a repeated root is ill-conditioned for the
% companion-matrix eigenvalue solver, so its accuracy floor is ~sqrt(eps) and
% varies across LAPACK backends. 1e-7 stays well under that floor yet still
% catches regressions.
asserts.isapprox(z, [-1; -1], 1e-7);
asserts.isapprox(p, 0.5, 1e-12);
asserts.isequal(k, 1);
%=============================================================================
[z, p, k] = tf2zp([0 0], [1 0.5]);
asserts.istrue(isempty(z));
asserts.isapprox(p, -0.5, 1e-12);
asserts.isequal(k, 0);
%=============================================================================
asserts.checkerror('tf2zp([1 2], [1 0; 0 1])', _('Second argument must be a vector.'));
%=============================================================================
