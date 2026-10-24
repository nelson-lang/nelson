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
asserts.isequal(nargin('unifit'), 2);
asserts.isequal(nargout('unifit'), 4);
%=============================================================================
[aHat, bHat] = unifit([2 5 3 4]);
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([2; 5; 3; 4]);
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
asserts.isapprox(aCI, [-1.3442275806433845; 2], 1e-14);
asserts.isapprox(bCI, [5; 8.3442275806433845], 1e-14);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([2 5 3 4], 0.01);
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
asserts.isapprox(aCI, [-4.486832980505138; 2], 1e-14);
asserts.isapprox(bCI, [5; 11.486832980505138], 1e-14);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([1 2; 3 4; 4 9]);
asserts.isequal(aHat, [1 2]);
asserts.isequal(bHat, [4 9]);
asserts.isapprox(aCI, [-4.143252849784719 -10.000923316164345; 1 2], 1e-14);
asserts.isapprox(bCI, [4 9; 9.143252849784719 21.000923316164345], 1e-14);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([2 5 3 4], []);
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
asserts.isapprox(aCI, [-1.3442275806433845; 2], 1e-14);
asserts.isapprox(bCI, [5; 8.3442275806433845], 1e-14);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([2 5 3 4], 1);
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
asserts.isequal(aCI, [2; 2]);
asserts.isequal(bCI, [5; 5]);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([2 5 3 4], 0);
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
asserts.istrue(isinf(aCI(1)) && aCI(1) < 0);
asserts.isequal(aCI(2), 2);
asserts.isequal(bCI(1), 5);
asserts.istrue(isinf(bCI(2)) && bCI(2) > 0);
%=============================================================================
[aHat, bHat] = unifit(single([2 5 3 4]));
asserts.isequal(class(aHat), 'double');
asserts.isequal(class(bHat), 'double');
asserts.isequal(aHat, 2);
asserts.isequal(bHat, 5);
%=============================================================================
[aHat, bHat, aCI, bCI] = unifit([1 NaN 3]);
asserts.istrue(isnan(aHat));
asserts.istrue(isnan(bHat));
asserts.istrue(all(isnan(aCI)));
asserts.istrue(all(isnan(bCI)));
%=============================================================================
asserts.checkerror('unifit()', _('Not enough input arguments.'));
asserts.checkerror('unifit([], 0.05)', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('unifit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('unifit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unifit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('unifit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('unifit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
%=============================================================================
