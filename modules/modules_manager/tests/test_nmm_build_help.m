%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
moduleRoot = tempname();
mkdir(moduleRoot);
moduleName = ['nelson_help_', strrep(createGUID(), '-', '_')];
%=============================================================================
nmm_build_help(moduleName, moduleRoot);
asserts.isfalse(ismodule(moduleName));
%=============================================================================
rmdir(moduleRoot);
%=============================================================================
