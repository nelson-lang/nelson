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
% Enumeration member argument lists may span a line continuation. The
% structural scanner used to parse enumeration lines one raw line at a time, so
% "Big (1000 + ..." was rejected as a malformed member and the class failed to
% load. This locks the corrected behavior.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
asserts.isequal(int32(ClassdefEnumContinuationArgs.Big), int32(1024));
asserts.isequal(int32(ClassdefEnumContinuationArgs.Small), int32(7));
%=============================================================================
rmpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
