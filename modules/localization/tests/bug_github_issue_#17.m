%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/17
% <-- Short Description -->
% 'locales' directory renamed as 'locale' (more standard).
%=============================================================================
assert_istrue(isdir([nelsonroot(), '/locale']));
assert_isfalse(isdir([nelsonroot(), '/locales']));