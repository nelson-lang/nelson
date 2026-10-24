%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: an overloaded subsref/subsasgn defined in a SUPERCLASS must
% be dispatched for dot-access on a SUBCLASS instance, exactly as if the method
% were defined directly on the class. Nelson routes all out-of-method indexing
% through the (possibly inherited) overloaded subsref/subsasgn.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% Sanity: subsref/subsasgn defined directly on the class already worked.
b = ClassdefInheritedIndexBase();
asserts.isequal(b.vectorize, true);
asserts.isequal(b.maxLength, 65);
b.vectorize = false;
b.maxLength = 128;
asserts.isequal(b.vectorize, false);
asserts.isequal(b.maxLength, 128);
%=============================================================================
% Inherited subsref (read) from the immediate superclass.
c = ClassdefInheritedIndexChild();
asserts.isequal(c.vectorize, true);
asserts.isequal(c.maxLength, 65);
%=============================================================================
% Inherited subsasgn (write) from the immediate superclass.
c.vectorize = false;
asserts.isequal(c.vectorize, false);
c.maxLength = 200;
asserts.isequal(c.maxLength, 200);
c.vectorize = true;
asserts.isequal(c.vectorize, true);
%=============================================================================
% Value semantics: assigning through the inherited subsasgn does not mutate a
% previously taken copy.
c1 = ClassdefInheritedIndexChild();
c2 = c1;
c1.vectorize = false;
asserts.isequal(c1.vectorize, false);
asserts.isequal(c2.vectorize, true);
%=============================================================================
% Two-level inheritance: the dispatch walks the full superclass chain
% (grandchild -> child -> base).
g = ClassdefInheritedIndexGrandchild();
asserts.isequal(g.vectorize, true);
asserts.isequal(g.maxLength, 65);
g.vectorize = false;
g.maxLength = 4096;
asserts.isequal(g.vectorize, false);
asserts.isequal(g.maxLength, 4096);
%=============================================================================
