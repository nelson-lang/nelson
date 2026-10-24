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
X = rand(250, 4);
rng(10);
tic();
eva = evalclusters(X, 'kmeans', 'CalinskiHarabasz', 'KList', 2:5);
t = toc();
asserts.isequal(eva.NumObservations, 250);
asserts.isequal(size(eva.CriterionValues), [1 4]);
asserts.istrue(t >= 0);
%=============================================================================
