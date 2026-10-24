%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
global CLASSDEF_SUBSASGN_END_TRACE;
CLASSDEF_SUBSASGN_END_TRACE = {};
obj = ClassdefSubsasgnEndProbe();
obj.x(end) = 0;
asserts.isequal(numel(CLASSDEF_SUBSASGN_END_TRACE), 2);
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{1}.kind, 'subsref');
asserts.isequal(numel(CLASSDEF_SUBSASGN_END_TRACE{1}.S), 1);
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{1}.S(1).type, '.');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{1}.S(1).subs, 'x');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.kind, 'subsasgn');
asserts.isequal(numel(CLASSDEF_SUBSASGN_END_TRACE{2}.S), 2);
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(1).type, '.');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(1).subs, 'x');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(2).type, '()');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(2).subs, {5});
%=============================================================================
CLASSDEF_SUBSASGN_END_TRACE = {};
obj.y(end) = 0;
asserts.isequal(numel(CLASSDEF_SUBSASGN_END_TRACE), 2);
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{1}.kind, 'subsref');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{1}.S(1).subs, 'y');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.kind, 'subsasgn');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(1).subs, 'y');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(2).subs, {5});
%=============================================================================
CLASSDEF_SUBSASGN_END_TRACE = {};
obj.x(1:end) = 0;
asserts.isequal(numel(CLASSDEF_SUBSASGN_END_TRACE), 2);
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{1}.S(1).subs, 'x');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(1).subs, 'x');
asserts.isequal(CLASSDEF_SUBSASGN_END_TRACE{2}.S(2).subs, {1:5});
%=============================================================================
CLASSDEF_SUBSASGN_END_TRACE = {};
%=============================================================================
