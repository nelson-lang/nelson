%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('RelTol', 1e-4, 'AbsTol', 1e-8);
asserts.isequal(odeget(options, 'RelTol'), 1e-4);
asserts.isequal(odeget(options, 'AbsTol'), 1e-8);
asserts.isequal(odeget(options, 'MaxStep', 12), 12);
asserts.isequal(odeget([], 'RelTol', 7), 7);
%=============================================================================
