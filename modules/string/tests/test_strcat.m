%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('strcat'), -1);
assert_isequal(nargout('strcat'), 1);
%=============================================================================
R = strcat([]);
REF = '';
assert_isequal(R, REF);
%=============================================================================
R = strcat({});
REF = {};
assert_isequal(R, REF);
%=============================================================================
R = strcat('');
REF = '';
assert_isequal(R, REF);
%=============================================================================
R = strcat('.');
REF = '.';
assert_isequal(R, REF);
%=============================================================================
R = strcat("rr");
REF = "rr";
assert_isequal(R, REF);
%=============================================================================
R = strcat(string(NaN));
REF = string(NaN);
assert_istrue(ismissing(R));
%=============================================================================
R = strcat(["rr", string(NaN)]);
REF = ["rr", string(NaN)];
assert_isequal(R, REF);
%=============================================================================
R = strcat('toolbox','nelson','general','Contents.m');
REF = 'toolboxnelsongeneralContents.m';
assert_isequal(R, REF);
%=============================================================================
R = strcat('red ', 'yellow    ', 'white ');
REF = 'redyellowwhite';
assert_isequal(R, REF);
%=============================================================================
R = strcat('toolbox', "nelson", 'general','Contents.m');
REF = "toolboxnelsongeneralContents.m";
assert_isequal(R, REF);
%=============================================================================
R = strcat('toolbox', string([]), 'general','Contents.m');
REF = string([]);
assert_isequal(R, REF);
%=============================================================================
R = strcat('username', {'data1', 'data2' ; 'data3', 'data4'}, 'fff');
REF = {'usernamedata1fff', 'usernamedata2fff'; 'usernamedata3fff', 'usernamedata4fff'};
assert_isequal(R, REF);
%=============================================================================
R = strcat('username   ', ["data2    "], 'fff');
REF = "usernamedata2    fff";
assert_isequal(R, REF);
%=============================================================================
R = strcat('username   ', ["data1", "data2    "], 'fff');
REF = ["usernamedata1fff", "usernamedata2    fff"];
assert_isequal(R, REF);
%=============================================================================
R = strcat('username   ', {'data1', 'data2' ; 'data3', 'data4'}, 'fff');
REF = {'usernamedata1fff', 'usernamedata2fff'; 'usernamedata3fff', 'usernamedata4fff'};
assert_isequal(R, REF);
%=============================================================================
R = strcat('username   ', ["data1", "data2    " ; "data3", "data4"], '   fff');
REF = [    "usernamedata1   fff", "usernamedata2       fff"; "usernamedata3   fff"    "usernamedata4   fff"];
assert_isequal(R, REF);
%=============================================================================
R = strcat('username', {'data1 ', 'data2  ' ; 'data3  ', 'data4  '}, 'fff');
REF = {'usernamedata1 fff', 'usernamedata2  fff'; 'usernamedata3  fff', 'usernamedata4  fff'};
assert_isequal(R, REF);
%=============================================================================
R = strcat('/home/username', [string(NaN); "data2"], {'f1.csv'});
REF = [string(NaN);  "/home/usernamedata2f1.csv"];
assert_isequal(R, REF);
%=============================================================================
CMD = 'strcat( {''data''; ''data2''}, {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
assert_checkerror(CMD, MSG);
%=============================================================================

