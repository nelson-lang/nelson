%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
a = [];
asserts.istrue(isempty(a));
asserts.isequal(size(a), [0 0]);
R = a == [];
asserts.istrue(isempty(R));
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(a == []), 'logical');
R = (1 == []);
asserts.istrue(isempty(R));
asserts.isequal(size(R), [0 0]);
asserts.isequal(class(1 == []), 'logical');
%=============================================================================
