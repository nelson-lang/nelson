%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Xc = [1 2; 2 NaN; 3 4; 4 3; 5 6];
yc = [1; 2; NaN; 4; 5];
mc = fitrensemble(Xc, yc, 'NumLearningCycles', 3, 'MaxNumSplits', 2, 'MinParentSize', 2);
asserts.isequal(mc.NumObservations, 3);
asserts.isapprox(mc.ResubstitutionLoss, 0.925925925925926, 1e-12);
asserts.isapprox(predict(mc, [1 2; 5 6]), [1; 3.666666666666667], 1e-12);
%=============================================================================
