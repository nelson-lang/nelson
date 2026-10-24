%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
V = reshape(1:27, [3 3 3]);
W = smooth3(V);
asserts.isequal(size(W), size(V));
asserts.isequal(class(W), 'double');
asserts.isapprox(W(2, 2, 2), 14, 1e-12);
asserts.isapprox(W(1, 1, 1), 16 / 3, 1e-12);
%=============================================================================
G = smooth3(V, 'gaussian', 3, 0.65);
asserts.isequal(size(G), size(V));
asserts.isapprox(G(2, 2, 2), 14, 1e-12);
asserts.istrue(G(1, 1, 1) < W(1, 1, 1));
%=============================================================================
W5 = smooth3(V, 'box', [3 3 1]);
asserts.isequal(size(W5), size(V));
asserts.isapprox(W5(2, 2, 2), 14, 1e-12);
%=============================================================================
VNaN = V;
VNaN(2, 2, 2) = NaN;
WNaN = smooth3(VNaN);
asserts.istrue(isnan(WNaN(2, 2, 2)));
asserts.istrue(isnan(WNaN(1, 1, 1)));
asserts.isequal(size(WNaN), size(VNaN));
%=============================================================================
asserts.checkerror('smooth3(ones(3));', getString(message('Nelson:parseSmooth3Inputs:invalidV')));
asserts.checkerror('smooth3(ones(3,3,3), ''bad'');', getString(message('Nelson:validateSmooth3Method:invalidSmoothingMethod')));
asserts.checkerror('smooth3(ones(3,3,3), ''box'', 4);', getString(message('Nelson:validateSmooth3WindowSize:invalidWindowSize')));
asserts.checkerror('smooth3(ones(3,3,3), ''gaussian'', 3, 0);', getString(message('Nelson:validateSmooth3StandardDeviation:invalidStandardDeviation')));
%=============================================================================
