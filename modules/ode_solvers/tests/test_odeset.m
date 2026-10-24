%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('RelTol', 1e-4, 'AbsTol', 1e-8, 'MaxStep', 0.25);
asserts.istrue(isstruct(options));
asserts.isequal(options.RelTol, 1e-4);
asserts.isequal(options.AbsTol, 1e-8);
asserts.isequal(options.MaxStep, 0.25);
asserts.checkerror('odeset(''RelTol'')', _('Name-value arguments expected.'));
%=============================================================================
