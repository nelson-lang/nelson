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
asserts.isequal(nargin('unidinv'), 2);
asserts.isequal(nargout('unidinv'), 1);
%=============================================================================
p = [0 0.01 0.2 0.21 0.5 1];
R = unidinv(p, 5);
asserts.isequal(R, [1 1 1 2 3 5]);
%=============================================================================
R = unidinv([0.5 0.5 0.5], [1 2 3]);
asserts.isequal(R, [1 1 2]);
%=============================================================================
R = unidinv(single([0.25 0.5 0.75]), single(4));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single([1 2 3]));
%=============================================================================
R = unidinv([-0.1 1.1 NaN 0.5 0.5 0.5], [5 5 5 0 -1 1.5]);
asserts.istrue(all(isnan(R)));
R = unidinv([], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('unidinv(0.5)', _('Not enough input arguments.'));
asserts.checkerror('unidinv(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('unidinv({0.5}, 4)', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('unidinv(0.5, {4})', _('N must be a positive integer numeric array.'));
%=============================================================================
