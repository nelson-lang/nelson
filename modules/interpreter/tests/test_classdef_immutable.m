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
% SetAccess = immutable: a property may be assigned only while the object is
% being constructed (in the defining class constructor or a subclass
% constructor through @Super), and is read-only afterwards.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% Value class: set in the constructor works, defaults respected.
p = ClassdefImmutablePoint(3, 7);
asserts.isequal(p.X, 3);
asserts.isequal(p.Y, 7);
%=============================================================================
% Unset immutable property keeps its default.
q = ClassdefImmutablePoint(1);
asserts.isequal(q.X, 1);
asserts.isequal(q.Y, 5);
%=============================================================================
% Non-immutable property stays writable.
p.Label = 'moved';
asserts.isequal(p.Label, 'moved');
%=============================================================================
% Setting an immutable property outside construction is rejected.
asserts.throws('p.X = 99;', 'X');
asserts.throws('r = p.trySetX(99);', 'X');
%=============================================================================
% Subclass constructor sets its own and (via @Super) the inherited immutable.
cp = ClassdefImmutableColorPoint(1, 2, 'red');
asserts.isequal(cp.X, 1);
asserts.isequal(cp.Y, 2);
asserts.isequal(cp.Color, 'red');
asserts.throws('cp.Color = ''blue'';', 'Color');
asserts.throws('cp.X = 8;', 'X');
%=============================================================================
% Handle class: same rule.
h = ClassdefImmutableHandle(42);
asserts.isequal(h.Id, 42);
asserts.throws('h.Id = 7;', 'Id');
%=============================================================================
rmpath(path_test);
%=============================================================================
