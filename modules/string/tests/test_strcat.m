%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.isequal(nargin('strcat'), -1);
asserts.isequal(nargout('strcat'), 1);
%=============================================================================
R = strcat([]);
REF = '';
asserts.isequal(R, REF);
%=============================================================================
R = strcat({});
REF = {};
asserts.isequal(R, REF);
%=============================================================================
R = strcat('');
REF = '';
asserts.isequal(R, REF);
%=============================================================================
R = strcat('.');
REF = '.';
asserts.isequal(R, REF);
%=============================================================================
R = strcat("rr");
REF = "rr";
asserts.isequal(R, REF);
%=============================================================================
R = strcat(string(NaN));
REF = string(NaN);
asserts.istrue(ismissing(R));
%=============================================================================
R = strcat(["rr", string(NaN)]);
REF = ["rr", string(NaN)];
asserts.isequal(R, REF);
%=============================================================================
R = strcat('toolbox', 'nelson', 'general', 'Contents.m');
REF = 'toolboxnelsongeneralContents.m';
asserts.isequal(R, REF);
%=============================================================================
R = strcat('red ', 'yellow    ', 'white ');
REF = 'redyellowwhite';
asserts.isequal(R, REF);
%=============================================================================
R = strcat('toolbox', "nelson", 'general', 'Contents.m');
REF = "toolboxnelsongeneralContents.m";
asserts.isequal(R, REF);
%=============================================================================
R = strcat('toolbox', string([]), 'general', 'Contents.m');
REF = string([]);
asserts.isequal(R, REF);
%=============================================================================
R = strcat('username', {'data1', 'data2'; 'data3', 'data4'}, 'fff');
REF = {'usernamedata1fff', 'usernamedata2fff'; 'usernamedata3fff', 'usernamedata4fff'};
asserts.isequal(R, REF);
%=============================================================================
R = strcat('username   ', ["data2    "], 'fff');
REF = "usernamedata2    fff";
asserts.isequal(R, REF);
%=============================================================================
R = strcat('username   ', ["data1", "data2    "], 'fff');
REF = ["usernamedata1fff", "usernamedata2    fff"];
asserts.isequal(R, REF);
%=============================================================================
R = strcat('username   ', {'data1', 'data2'; 'data3', 'data4'}, 'fff');
REF = {'usernamedata1fff', 'usernamedata2fff'; 'usernamedata3fff', 'usernamedata4fff'};
asserts.isequal(R, REF);
%=============================================================================
R = strcat('username   ', ["data1", "data2    "; "data3", "data4"], '   fff');
REF = ["usernamedata1   fff", "usernamedata2       fff"; "usernamedata3   fff" "usernamedata4   fff"];
asserts.isequal(R, REF);
%=============================================================================
R = strcat('username', {'data1 ', 'data2  '; 'data3  ', 'data4  '}, 'fff');
REF = {'usernamedata1 fff', 'usernamedata2  fff'; 'usernamedata3  fff', 'usernamedata4  fff'};
asserts.isequal(R, REF);
%=============================================================================
R = strcat('/home/username', [string(NaN); "data2"], {'f1.csv'});
REF = [string(NaN); "/home/usernamedata2f1.csv"];
asserts.isequal(R, REF);
%=============================================================================
CMD = 'strcat( {''data''; ''data2''}, {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
asserts.checkerror(CMD, MSG);
%=============================================================================

