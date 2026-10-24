%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/3
% <-- Short Description -->
% Invalid extraction & insertion on function_handle.
%=============================================================================
A = str2func('sin');
%=============================================================================
asserts.checkerror('A(1).b= 1', _('Cannot apply A.field_name = B to non struct-array object A.'));
%=============================================================================
asserts.checkerror('A(1).b', _('Invalid indexing.'));
%=============================================================================
asserts.checkerror('A.b = 3', _('Cannot apply A.field_name = B to non struct-array object A.'));
%=============================================================================
asserts.checkerror('A{1}', _('Attempt to apply contents-indexing to non cell-array object.'));
%=============================================================================
asserts.checkerror('A{1}', _('Attempt to apply contents-indexing to non cell-array object.'));
%=============================================================================
asserts.checkerror('A{1} = 1', _('Unable to perform assignment because brace indexing is not supported for variables of this type.'));
asserts.checkerror('A{1} = 1', _('Unable to perform assignment because brace indexing is not supported for variables of this type.'), 'Nelson:cellAssToNonCell');
%=============================================================================
