%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('betaln'), 2);
asserts.isequal(nargout('betaln'), 1);
%=============================================================================
asserts.isapprox(betaln(2, 3), log(1 / 12), 1e-12);
asserts.isapprox(betaln(10, 20), log(beta(10, 20)), 1e-10);
asserts.isapprox(betaln([1 2 3], [2 2 2]), log([1 / 2 1 / 6 1 / 12]), 1e-12);
asserts.isapprox(betaln(100, 200), gammaln(100) + gammaln(200) - gammaln(300), 1e-9);
%=============================================================================
asserts.checkerror('betaln(1i, 2)', _('Inputs must be real.'), 'Nelson:beta:mustBeReal');
%=============================================================================
