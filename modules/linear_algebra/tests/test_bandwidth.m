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
asserts.isequal(nargin('bandwidth'), -1);
asserts.isequal(nargout('bandwidth'), -1);
%=============================================================================
asserts.isequal(bandwidth([]), 0);
asserts.isequal(bandwidth([], 'upper'), 0);
asserts.isequal(bandwidth([], 'lower'), 0);
%=============================================================================
asserts.isequal(bandwidth(NaN), 0);
asserts.isequal(bandwidth(NaN, 'upper'), 0);
asserts.isequal(bandwidth(NaN, 'lower'), 0);
%=============================================================================
asserts.isequal(bandwidth(Inf), 0);
asserts.isequal(bandwidth(Inf, 'upper'), 0);
asserts.isequal(bandwidth(Inf, 'lower'), 0);
%=============================================================================
A = tril(magic(6));
R = bandwidth(A, 'lower');
REF = 5;
asserts.isequal(R, REF);
%=============================================================================
R = bandwidth(A, 'upper');
REF = 0;
asserts.isequal(R, REF);
%=============================================================================
[lowerBandwidth, upperBandwidth] = bandwidth([1 0 0; 2 3 0; 0 4 5]);
asserts.isequal([lowerBandwidth upperBandwidth], [1 0]);
%=============================================================================
