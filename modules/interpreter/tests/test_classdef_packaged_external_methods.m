%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test, '-begin');
cleanup = onCleanup(@() rmpath(path_test));
%=============================================================================
obj = classdefpkg.PackagedExternalMethod(7);
asserts.isequal(class(obj), 'classdefpkg.PackagedExternalMethod');
asserts.isequal(obj.externalScale(6), 42);
asserts.isequal(externalScale(obj, 5), 35);
asserts.isequal(classdefpkg.PackagedExternalMethod.staticShift(4), 15);
method_names = methods('classdefpkg.PackagedExternalMethod');
asserts.istrue(any(strcmp(method_names, 'externalScale')));
%=============================================================================
