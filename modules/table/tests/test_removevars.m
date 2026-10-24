%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Name = {'A'; 'B'};
Age = [10; 20];
Height = [100; 120];
T = table(Name, Age, Height);
R = removevars(T, 'Age');
asserts.isequal(R.Properties.VariableNames, {'Name', 'Height'});
asserts.isequal(R.Height, Height);
%=============================================================================
