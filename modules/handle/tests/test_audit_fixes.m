%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(isempty(which('libpointer')), 'libpointer is not available.');
% handle == non-handle must be false (never garbage)
p = libpointer('doublePtr', [1 2 3]);
R = (p == 5);
asserts.isequal(class(R), 'logical');
asserts.isequal(size(R), [1 1]);
asserts.isfalse(R);
%=============================================================================
R = (p == [1 2 3]);
asserts.isequal(size(R), [1 3]);
asserts.isfalse(any(R));
%=============================================================================
R = (5 == p);
asserts.isfalse(R);
%=============================================================================
q = [p p];
R = (q == 5);
asserts.isequal(size(R), [1 2]);
asserts.isequal(R, [false false]);
%=============================================================================
% handle == handle still works
asserts.istrue(p == p);
p2 = libpointer('doublePtr', [1 2 3]);
asserts.isfalse(p == p2);
%=============================================================================
