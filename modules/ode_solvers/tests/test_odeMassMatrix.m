%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
mass = odeMassMatrix(eye(2), 'MStateDependence', 'none', 'MvPattern', true(2));
asserts.isequal(mass.MassMatrix, eye(2));
asserts.isequal(mass.Singular, 'no');
asserts.isequal(mass.StateDependence, 'none');
asserts.isequal(mass.SparsityPattern, true(2));
asserts.checkerror('odeMassMatrix({1})', sprintf(_('%s must be numeric or a function handle.'), 'Mass matrix'));
%=============================================================================
