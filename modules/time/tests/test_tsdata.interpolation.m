%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
interp = tsdata.interpolation('zoh');
asserts.isequal(interp.Method, 'zoh');
asserts.isequal(interp == tsdata.interpolation('zoh'), true);
asserts.isequal(get(interp, 'Method'), 'zoh');
%=============================================================================
