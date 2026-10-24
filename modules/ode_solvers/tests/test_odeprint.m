%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = evalc('status = odeprint([0 1], [1; 2], ''init'');');
asserts.isequal(status, 0);
asserts.istrue(contains(R, 't ='));
R = evalc('status = odeprint(0.5, 1.5, '''');');
asserts.isequal(status, 0);
asserts.istrue(contains(R, 'y ='));
asserts.isequal(odeprint([], [], 'done'), 0);
%=============================================================================
