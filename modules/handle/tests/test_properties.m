%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
c = ClassdefCounter();
names = properties(c);
asserts.istrue(any(strcmp(names, 'Count')));
asserts.istrue(any(strcmp(names, 'LastEvent')));
delete(c);
%=============================================================================
