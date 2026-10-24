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
asserts.isequal(nargin('ecdf'), -2);
asserts.isequal(nargout('ecdf'), 4);
%=============================================================================
[f, x, flo, fup] = ecdf([3 1 2 2]);
asserts.isequal(x, [1; 1; 2; 3]);
asserts.isequal(f, [0; 0.25; 0.75; 1]);
asserts.isapprox(flo, [0; 0; 0.325655349721435; 1], 1e-12);
asserts.isapprox(fup, [0; 0.674344650278565; 1; 1], 1e-12);
%=============================================================================
[f, x] = ecdf([1 2 3 4], 'Function', 'survivor');
asserts.isequal(x, [1; 1; 2; 3; 4]);
asserts.isequal(f, [1; 0.75; 0.5; 0.25; 0]);
%=============================================================================
[f, x] = ecdf([1 2 3 4], 'Function', 'cumhazard');
asserts.isequal(x, [1; 1; 2; 3; 4]);
asserts.isapprox(f(1:4), [0; 0.287682072451781; 0.693147180559945; 1.386294361119891], 1e-12);
asserts.istrue(isinf(f(5)));
%=============================================================================
[f, x] = ecdf([1 2 3 4], 'Censoring', [0 1 0 0]);
asserts.isequal(x, [1; 1; 3; 4]);
asserts.isequal(f, [0; 0.25; 0.625; 1]);
%=============================================================================
[f, x] = ecdf([1 2], 'Frequency', [2 3]);
asserts.isequal(x, [1; 1; 2]);
asserts.isequal(f, [0; 0.4; 1]);
%=============================================================================
[f, x] = ecdf([1 NaN 2], 'Frequency', [1 1 NaN]);
asserts.isequal(x, [1; 1]);
asserts.isequal(f, [0; 1]);
%=============================================================================
[f, x] = ecdf([1 Inf; 2 Inf]);
asserts.isequal(x, 1);
asserts.isequal(f, 0);
%=============================================================================
asserts.checkerror('ecdf()', _('Not enough input arguments.'));
asserts.checkerror('ecdf({1})', _('Y must be a real numeric vector or two-column matrix.'));
asserts.checkerror('ecdf([1 2], ''Censoring'', [0 1 0])', sprintf(_('%s must match Y.'), 'Censoring and frequency'));
asserts.checkerror('ecdf([1 2], ''Censoring'', [0 2])', _('Censoring values must be -1, 0, or 1.'));
asserts.checkerror('ecdf([1 2], ''Function'', ''bad'')', sprintf(_('Unsupported %s.'), 'function type'));
asserts.checkerror('ecdf([1 2], ''Bounds'', ''bad'')', _('Bounds must be ''on'' or ''off''.'));
asserts.checkerror('ecdf([1 2], ''Alpha'', 2)', _('Alpha must be a scalar between 0 and 1.'));
%=============================================================================
