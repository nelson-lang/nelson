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
asserts.isequal(nargin('normrnd'), -1);
asserts.isequal(nargout('normrnd'), 1);
%=============================================================================
rng(0);
R = normrnd(2, 3, 2, 4);
asserts.isequal(size(R), [2 4]);
asserts.istrue(all(isfinite(R(:))));
%=============================================================================
rng(0);
R = normrnd(single(0), single(1), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
asserts.istrue(all(isfinite(R(:))));
%=============================================================================
R = normrnd([0 10], [1 2]);
asserts.isequal(size(R), [1 2]);
asserts.istrue(all(isfinite(R(:))));
%=============================================================================
R = normrnd(0, -1, [1 3]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = normrnd(0, 1, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
R = normrnd(single(0), single(1), [2 0 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 0 3]);
%=============================================================================
R = normrnd([1 2], 0);
asserts.isequal(R, [1 2]);
%=============================================================================
asserts.checkerror('normrnd(0)', _('Not enough input arguments.'));
asserts.checkerror('normrnd([1 2], 1, 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('normrnd(0, 1, -1, 2)', sprintf(_('%s must contain nonnegative integer values.'), 'Size'));
asserts.checkerror('normrnd(0, 1, 1.5, 2)', sprintf(_('%s must contain nonnegative integer values.'), 'Size'));
%=============================================================================
