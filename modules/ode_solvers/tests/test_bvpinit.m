%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
solinit = bvpinit(linspace(0, 1, 4), [1; 2], 3);
asserts.isequal(solinit.x, [0 1 / 3 2 / 3 1]);
asserts.isequal(size(solinit.y), [2 4]);
asserts.isequal(solinit.parameters, 3);
asserts.checkerror('bvpinit([0 0], [1; 2])', _('Initial mesh must be strictly increasing.'));
%=============================================================================
