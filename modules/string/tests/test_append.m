%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('append'), -1);
assert_isequal(nargout('append'), 1);
%=============================================================================
R = append('toolbox','nelson','general','Contents.m');
REF = 'toolboxnelsongeneralContents.m';
assert_isequal(R, REF);
%=============================================================================
R = append('red ', 'yellow    ', 'white ');
REF = 'red yellow    white ';
assert_isequal(R, REF);
%=============================================================================
R = append('username   ', ["data2    "], 'fff');
REF = "username   data2    fff";
assert_isequal(R, REF);
%=============================================================================
R = append('username   ', ["data1", "data2    "], 'fff');
REF = ["username   data1fff",    "username   data2    fff"];
assert_isequal(R, REF);
%=============================================================================
R = append('username   ', {'data1', 'data2' ; 'data3', 'data4'}, 'fff');
REF =  {'username   data1fff', 'username   data2fff'; 'username   data3fff', 'username   data4fff'};
assert_isequal(R, REF);
%=============================================================================
R = append('username   ', ["data1", "data2    " ; "data3", "data4"], '   fff');
REF = ["username   data1   fff", "username   data2       fff"; "username   data3   fff", "username   data4   fff" ];
assert_isequal(R, REF);
%=============================================================================
R = append('username', {'data1 ', 'data2  ' ; 'data3  ', 'data4  '}, 'fff');
REF = {'usernamedata1 fff', 'usernamedata2  fff'; 'usernamedata3  fff', 'usernamedata4  fff'};
assert_isequal(R, REF);
%=============================================================================
R = append('/home/username', [string(NaN); "data2"], {'f1.csv'});
REF = [string(NaN);  "/home/usernamedata2f1.csv"];
assert_isequal(R, REF);
%=============================================================================
CMD = 'append( {''data''; ''data2''}, {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
assert_checkerror(CMD, MSG);
%=============================================================================

