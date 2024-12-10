%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table();
assert_isequal(T.Properties.VariableTypes, string([]));
%=============================================================================
A = 1;
B = 2;
T1 = table(A);
T2 = table(B);
T = horzcat(T1, T2);
assert_isequal(T.Properties.VariableTypes, ["double", "double"]);
%=============================================================================
T1 = table(int8(1));
T2 = table(int16(2));
T = [T1; T2];
assert_isequal(T.Properties.VariableTypes, ["int8"]);
assert_isequal(class(T{:,1}), 'int8');
T.Properties.VariableTypes = "int16";
assert_isequal(class(T{:,1}), 'int16');
%=============================================================================
Names = {'John'; 'Alice'; 'Bob'; 'Diana'};
Age = [28; 34; 22; 30];
Height = [175; 160; 180; 165];
Weight = [70; 55; 80; 60];
T = table(Names, Age, Height, Weight);
assert_isequal(T.Properties.VariableTypes, [ "cell"    "double"    "double"    "double"]);
T.Properties.VariableTypes = ["string"    "int8"    "double"    "double"];
assert_isequal(class(T{:,1}), 'string');
assert_isequal(class(T{:,2}), 'int8');
assert_isequal(T.Properties.VariableTypes, [ "string"    "int8"    "double"    "double"]);
%=============================================================================
