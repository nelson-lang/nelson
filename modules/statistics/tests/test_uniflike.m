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
asserts.isequal(nargin('uniflike'), 4);
asserts.isequal(nargout('uniflike'), 2);
%=============================================================================
x = [2 5 3 4];
[NLOGL, AVAR] = uniflike([1 6], x);
asserts.isapprox(NLOGL, 6.437751649736401, 1e-12);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = uniflike(single([1 6]), single([2 3 4]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, 4.828313737302301, 1e-12);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
censor = [0 0 1 0];
freq = [1 2 1 1];
[NLOGL, AVAR] = uniflike([1 6], x, censor, freq);
asserts.isapprox(NLOGL, 6.948577273502392, 1e-12);
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
[NLOGL, AVAR] = uniflike([2 5], x);
asserts.isapprox(NLOGL, 4.394449154672439, 1e-12);
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = uniflike([3 5], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = uniflike([1 1], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = uniflike([NaN 6], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = uniflike([1 6], [1 NaN 2]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('uniflike([1 2])', _('Not enough input arguments.'));
asserts.checkerror('uniflike({1 2}, [1 2 3])', sprintf(_('%s must be real numeric.'), 'PARAMS'));
asserts.checkerror('uniflike([1 2 3], [1 2 3])', sprintf(_('PARAMS must be a %s vector.'), 'two-element'));
asserts.checkerror('uniflike([1 2], {1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('uniflike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('uniflike([1 2], [1 2 3], [0 2 0])', _('CENSORING must contain 0 or 1 values.'));
asserts.checkerror('uniflike([1 2], [1 2 3], [], [1 -1 1])', sprintf(_('%s must contain nonnegative finite values.'), 'FREQ'));
asserts.checkerror('uniflike([1 2], [1 2 3], ones(2, 2))', _('Size mismatch on input arguments'));
%=============================================================================
