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
%=============================================================================
% hidden subsasgn and subsref are dispatched
h = ClassdefHandleDotIndex();
h.a = magic(3);
asserts.isequal(receivedTypes(h), {'.'});
asserts.isequal(writes(h), 1);
asserts.isequal(h.a, magic(3));
asserts.isequal(receivedTypes(h), {'.'});
asserts.isfalse(any(strcmp(methods(h), 'subsref')));
asserts.isfalse(any(strcmp(methods(h), 'subsasgn')));
%=============================================================================
% h.name(indices) reaches subsref once with both subscripts
before = reads(h);
asserts.isequal(h.a(2, 3), 7);
asserts.isequal(receivedTypes(h), {'.', '()'});
asserts.isequal(reads(h), before + 1);
asserts.isequal(h.a(:, 2), [1; 5; 9]);
asserts.isequal(receivedTypes(h), {'.', '()'});
%=============================================================================
% h.name(indices) = value reaches subsasgn once with both subscripts, without a read
before = reads(h);
h.a(1, 2) = 50;
asserts.isequal(receivedTypes(h), {'.', '()'});
asserts.isequal(reads(h), before);
asserts.isequal(h.a(1, 2), 50);
%=============================================================================
% a new name and a deletion take the same path
before = reads(h);
h.b(2, 2) = 4;
asserts.isequal(receivedTypes(h), {'.', '()'});
asserts.isequal(reads(h), before);
asserts.isequal(h.b, [0 0; 0 4]);
h.a(:, 1) = [];
asserts.isequal(receivedTypes(h), {'.', '()'});
asserts.isequal(size(h.a), [3 2]);
%=============================================================================
% end reads the whole value first
asserts.isequal(h.a(end, 1), 9);
h.a(end, 1) = 40;
asserts.isequal(h.a(3, 1), 40);
%=============================================================================
% same behavior inside a function handle body
f = @(obj) obj.a(1, 2);
asserts.isequal(f(h), 6);
asserts.isequal(receivedTypes(h), {'.', '()'});
%=============================================================================
