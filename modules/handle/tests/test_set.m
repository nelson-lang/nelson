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
set(c, 'Count', 5);
asserts.isequal(c.Count, 5);
dyn = ClassdefDynamicProps();
token = addprop(dyn, 'Extra');
set(token, 'SetObservable', true);
asserts.istrue(get(token, 'SetObservable'));
delete(token);
delete(c);
%=============================================================================
