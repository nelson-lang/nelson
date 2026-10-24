%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('beta'), 2);
asserts.isequal(nargout('beta'), 1);
%=============================================================================
asserts.isapprox(beta(2, 3), 1 / 12, 1e-12);
asserts.isapprox(beta(0.5, 0.5), pi, 1e-12);
asserts.isapprox(beta([1 2 3], [2 2 2]), [1 / 2 1 / 6 1 / 12], 1e-12);
asserts.isapprox(beta(10, 20), gamma(10) * gamma(20) / gamma(30), 1e-10);
asserts.isapprox(beta(1, 1), 1, 1e-12);
%=============================================================================
asserts.checkerror('beta(1i, 2)', _('Inputs must be real.'), 'Nelson:beta:mustBeReal');
%=============================================================================
