%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% non integer number of points: floor(n)
asserts.isequal(linspace(0, 1, 2.5), [0 1]);
asserts.isequal(linspace(0, 1, 3.9), [0 0.5 1]);
asserts.isequal(linspace(0, 1, 0.5), zeros(1, 0));
% unchanged behaviors
asserts.isequal(linspace(0, 1, 5), [0 0.25 0.5 0.75 1]);
asserts.isequal(linspace(1, 2, 2), [1 2]);
asserts.isequal(linspace(2, 5, NaN), NaN);
%=============================================================================
