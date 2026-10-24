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
asserts.isequal(class(T), 'table');
asserts.istrue(isa(T, 'table'));
asserts.istrue(isa(T, 'tabular'));
asserts.istrue(istable(T));
asserts.istrue(istabular(T));
asserts.isequal(T.Properties.VariableNames, {'A', 'B'});
%=============================================================================
