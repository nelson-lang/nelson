%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
assert_isequal(class(T), 'table');
assert_istrue(isa(T, 'table'));
assert_istrue(istable(T));
assert_isequal(T.Properties.VariableNames, {'A', 'B'});
%=============================================================================
