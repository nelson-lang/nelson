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
c = ClassdefCounter(7);
asserts.isequal(get(c, 'Count'), 7);
dyn = ClassdefDynamicProps();
token = addprop(dyn, 'Extra');
asserts.isequal(get(token, 'Name'), 'Extra');
delete(token);
delete(c);
%=============================================================================
