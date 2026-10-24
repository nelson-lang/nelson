%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
properNumerator = [10 10 0];
properDenominator = [10 20 30];
gainNumerator = 10;
gainDenominator = 10;
descriptorNumerator = [0 10 10];
descriptorDenominator = [1 1 10];

tic();
for k = 1:250 %#ok<NLS0004>
  [Ap, Bp, Cp, Dp, Ep] = compreal(properNumerator, properDenominator);
  [Ag, Bg, Cg, Dg, Eg] = compreal(gainNumerator, gainDenominator);
  [Ad, Bd, Cd, Dd, Ed] = compreal(descriptorNumerator, descriptorDenominator);
end
toc();

asserts.isequal(size(Ap), [2 2]);
asserts.isequal(size(Bp), [2 1]);
asserts.isequal(size(Cp), [1 2]);
asserts.isequal(size(Dp), [1 1]);
asserts.isequal(Ep, []);
asserts.isequal(Ag, []);
asserts.isequal(size(Bg), [0 1]);
asserts.isequal(size(Cg), [1 0]);
asserts.isequal(Dg, 1);
asserts.isequal(Eg, []);
asserts.isequal(size(Ad), [2 2]);
asserts.isequal(size(Bd), [2 1]);
asserts.isequal(size(Cd), [1 2]);
asserts.isequal(size(Dd), [1 1]);
asserts.isequal(Ed, []);
%=============================================================================
