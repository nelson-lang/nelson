%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = ddeset('RelTol', 1e-5, 'AbsTol', 1e-7);
asserts.istrue(isstruct(options));
asserts.isequal(options.RelTol, 1e-5);
asserts.isequal(options.AbsTol, 1e-7);
asserts.checkerror('ddeset(''RelTol'')', _('Name-value arguments expected.'));
%=============================================================================
