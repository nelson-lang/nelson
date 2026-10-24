%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
u = (1:16)';
y = filter([1 0.5], 1, u);
[Hv, f, Puu, Pyy, Puy, coh, Sv] = tfe(u, y, 4, 8);
Hv_REF = [1.453929539295393 + 0i; 1.455387102864952 - 0.009111884952900i; 1.5 + 0i];
asserts.isapprox(Hv, Hv_REF, 1e-12);
asserts.isapprox(f.', [0 2 4], 1e-12);
asserts.isequal(size(Puu), size(Hv));
asserts.isequal(size(Pyy), size(Hv));
asserts.isequal(size(Puy), size(Hv));
asserts.isequal(size(coh), size(Hv));
asserts.isequal(size(Sv), size(Hv));
asserts.istrue(all(coh >= 0 & coh <= 1 + 100 * eps()));
%=============================================================================
[Hv2, f2] = tfe(u, y);
asserts.isequal(size(Hv2), size(f2));
%=============================================================================
asserts.checkerror('tfe(1:4, 1:3)', _('Input signals must have the same length.'));
%=============================================================================
