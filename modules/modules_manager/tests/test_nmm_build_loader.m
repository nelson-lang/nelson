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
moduleName = ['nelson_loader_', strrep(createGUID(), '-', '_')];
%=============================================================================
nmm_build_loader(moduleName, moduleRoot);
loaderPath = fullfile(moduleRoot, 'loader.m');
asserts.istrue(isfile(loaderPath));
txt = fileread(loaderPath);
asserts.istrue(contains(txt, ['addmodule(fileparts(nfilename(''fullpath'')), ''', moduleName, ''');']));
%=============================================================================
rmfile(loaderPath);
rmdir(moduleRoot);
%=============================================================================
