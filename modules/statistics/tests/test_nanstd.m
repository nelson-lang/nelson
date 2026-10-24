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
asserts.isequal(nargin('nanstd'), -2);
asserts.isequal(nargout('nanstd'), 1);
%=============================================================================
X = magic(3);
X([1 6:9]) = NaN;
asserts.isapprox(nanstd(X), [0.707106781186548 2.828427124746190 NaN], 1e-12);
asserts.isapprox(nanstd(X, 1), [0.5 2 NaN], 1e-12);
asserts.isapprox(nanstd(X, 0, 2), [0; 1.414213562373095; 0], 1e-12);
%=============================================================================
A = reshape(1:24, [3 4 2]);
A([8:10 18]) = NaN;
asserts.isapprox(nanstd(A, 0, 'all'), 7.538497686437335, 1e-12);
S12 = nanstd(A, 0, [1 2]);
asserts.isequal(size(S12), [1 1 2]);
asserts.isapprox(S12(:), [3.807886552931954; 3.777926319123661], 1e-12);
S23 = nanstd(A, 0, [2 3]);
asserts.isequal(size(S23), [3 1]);
asserts.isapprox(S23(:), [7.910210399946930; 7.690439333539869; 8.215838362577491], 1e-12);
%=============================================================================
Z = nanstd([1 NaN 3], 0, 3);
asserts.isequal(Z, [0 NaN 0]);
asserts.istrue(isnan(nanstd([NaN NaN])));
asserts.istrue(isnan(nanstd([])));
S = single([1 NaN 4]);
Y = nanstd(S);
asserts.isequal(class(Y), 'single');
asserts.isapprox(double(Y), 2.1213202, 1e-6);
%=============================================================================
asserts.checkerror('nanstd()', _('Not enough input arguments.'));
asserts.checkerror('nanstd({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('nanstd([1 2], 2)', sprintf(_('%s must be 0 or 1.'), 'Flag'));
asserts.checkerror('nanstd([1 2], 0, 0)', _('Dimension must be a positive integer scalar, vector, or ''all''.'));
asserts.checkerror('nanstd([1 2], 0, ''bad'')', _('Invalid option.'));
%=============================================================================
