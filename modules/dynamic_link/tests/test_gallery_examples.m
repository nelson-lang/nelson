%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
helperPath = [modulepath('tests_manager', 'tests'), '/helpers'];
previousPath = addpath(helperPath);
cleanupPath = onCleanup(@() path(previousPath));
validateExampleModule('dynamic_link');
clear cleanupPath;
%=============================================================================
