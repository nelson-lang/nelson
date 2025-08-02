%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = struct();
S.Name = ["Chang";"Brown";"Ruiz"];
S.Smoker = ["Y";"N";"Y"];
S.SystolicBP = [124;122;130];
S.DiastolicBP = [93;80;92];
T = struct2table(S);
REF = table(["Chang";"Brown";"Ruiz"],["Y";"N";"Y"],[124;122;130],[93;80;92],'VariableNames',{'Name','Smoker','SystolicBP','DiastolicBP'});
assert_isequal(T, REF);
%=============================================================================
S = struct();
S(1,1).Name = "Chang";
S(1,1).Smoker = "Y";
S(1,1).SystolicBP = 124;
S(1,1).DiastolicBP = 93;
S(2,1).Name = "Brown";
S(2,1).Smoker = "N";
S(2,1).SystolicBP = 122;
S(2,1).DiastolicBP = 80;
S(3,1).Name = "Ruiz";
S(3,1).Smoker = "Y";
S(3,1).SystolicBP = 130;
S(3,1).DiastolicBP = 92;
T = struct2table(S);
REF = table(["Chang";"Brown";"Ruiz"],["Y";"N";"Y"],[124;122;130],[93;80;92],'VariableNames',{'Name','Smoker','SystolicBP','DiastolicBP'});
assert_isequal(T, REF);
%=============================================================================
S = struct();
S(1).a = [1 2];
S(2).a = [3 4];
S(1).b = 5;
S(2).b = 6;
T = struct2table(S);
REF = table([1 2;3 4],[5;6],'VariableNames',{'a','b'});
assert_isequal(T, REF);
%=============================================================================
S = struct();
S(1).a = [1 2];
S(2).a = [3 4 5 6];
S(1).b = 7;
S(2).b = 8;
T = struct2table(S);
%=============================================================================
S = struct();
S.a = [1;2;3];
S.b = [4 5;6 7;8 9];
T = struct2table(S);
REF = table([1;2;3],[4 5;6 7;8 9],'VariableNames',{'a','b'});
assert_isequal(T, REF);
%=============================================================================
