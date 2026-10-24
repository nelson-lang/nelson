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
% Distinct failures carry distinct identifiers.
%=============================================================================
asserts.checkerror('class(struct(), ''foo'')', 'When called from outside a class constructor, the CLASS function must be called with one input.', 'Nelson:types:classOutsideConstructor');
%=============================================================================
S = sparse(eye(2));
asserts.checkerror('S(1, 1, 2)', 'Multidimensional indexing (more than 2 dimensions) not legal for sparse arrays.', 'Nelson:types:sparseNDimIndexing');
%=============================================================================
