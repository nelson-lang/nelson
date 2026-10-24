%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% A property default may continue implicitly across a newline while brackets
% stay open: matrix and cell row breaks, and multi-line calls. The structural
% scanner used to fold only explicit "..." continuations, so it truncated the
% default at the first newline and the class failed to load or lost rows. This
% locks the corrected behavior: a matrix/cell newline is a row separator, an
% explicit "..." join is a space.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
obj = ClassdefImplicitContinuationDefaults();
asserts.isequal(obj.V, 7);
asserts.isequal(obj.C, {1, 2; 3, 4});
asserts.isequal(obj.W, [10 20; 30 40]);
%=============================================================================
rmpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
