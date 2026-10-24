%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table({'r1'; 'r2'}, [10; 20], 'VariableNames', {'Name', 'Value'});
R = rows2vars(T, 'VariableNamesSource', 'Name');
asserts.isequal(R.Properties.VariableNames, {'OriginalVariableNames', 'r1', 'r2'});
asserts.isequal(R.r1, {'r1'; 10});
asserts.isequal(R.r2, {'r2'; 20});
%=============================================================================
