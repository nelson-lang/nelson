%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mkpp'), 3);
asserts.isequal(nargout('mkpp'), 1);
%=============================================================================
pp = mkpp([0 1 2], [1 0; 1 1]);
asserts.isequal(pp.form, 'pp');
asserts.isequal(pp.breaks, [0 1 2]);
asserts.isequal(pp.coefs, [1 0; 1 1]);
asserts.isequal(pp.pieces, 2);
asserts.isequal(pp.order, 2);
asserts.isequal(pp.dim, 1);
%=============================================================================
R = ppval(pp, 0.5);
asserts.isapprox(R, 0.5, 1e-12);
%=============================================================================
pp2 = mkpp([0 2], [1 2 3 4]);
asserts.isequal(pp2.pieces, 1);
asserts.isequal(pp2.order, 4);
asserts.isapprox(ppval(pp2, 1), 10, 1e-12);
%=============================================================================
ppc = mkpp([0 1 2 3], [5; 6; 7]);
asserts.isequal(ppc.order, 1);
asserts.isequal(ppc.pieces, 3);
asserts.isapprox(ppval(ppc, [0.5 1.5 2.5]), [5 6 7], 1e-12);
%=============================================================================
ppv = mkpp([0 1 2], [1 0; 2 0; 1 1; 2 1], 2);
asserts.isequal(ppv.dim, 2);
asserts.isequal(ppv.pieces, 2);
asserts.isequal(ppv.order, 2);
asserts.isapprox(ppval(ppv, 0.5), [0.5; 1], 1e-12);
asserts.isapprox(ppval(ppv, [0.5 1.5]), [0.5 1.5; 1 2], 1e-12);
%=============================================================================
ppd = mkpp([0 1 2], [1 0; 2 0; 1 1; 2 1], [2 1]);
asserts.isequal(ppd.dim, [2 1]);
%=============================================================================
asserts.checkerror('mkpp([0 1 2], [1 2 3])', _('The number of rows of COEFS must equal PIECES times PROD(D).'));
asserts.checkerror('mkpp(0, [1 2])', _('BREAKS must be a vector with at least two elements.'));
asserts.checkerror('mkpp()', _('Not enough input arguments.'));
%=============================================================================
