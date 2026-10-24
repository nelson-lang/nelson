%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test, '-begin');
clear functions
asserts.isequal(exist('nelson.classdefpkg.Options'), 8);
asserts.isequal(exist('nelson.classdefpkg.Options', 'class'), 8);
asserts.isequal(exist('nelson.classdefpkg.options.DeepClass'), 8);
asserts.isequal(exist('nelson.classdefpkg.options.DeepClass', 'class'), 8);
asserts.isequal(exist('nelson.classdefpkg.options.DerivedOptions'), 8);
asserts.isequal(exist('nelson.classdefpkg.options.DerivedOptions', 'class'), 8);
clear functions
run([path_test, '/classdefPackageResolutionScript.m']);
rmpath(path_test);
%=============================================================================
