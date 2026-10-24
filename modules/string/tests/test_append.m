%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('append'), -1);
asserts.isequal(nargout('append'), 1);
%=============================================================================
R = append('toolbox', 'nelson', 'general', 'Contents.m');
REF = 'toolboxnelsongeneralContents.m';
asserts.isequal(R, REF);
%=============================================================================
R = append('red ', 'yellow    ', 'white ');
REF = 'red yellow    white ';
asserts.isequal(R, REF);
%=============================================================================
R = append('username   ', ["data2    "], 'fff');
REF = "username   data2    fff";
asserts.isequal(R, REF);
%=============================================================================
R = append('username   ', ["data1", "data2    "], 'fff');
REF = ["username   data1fff", "username   data2    fff"];
asserts.isequal(R, REF);
%=============================================================================
R = append('username   ', {'data1', 'data2'; 'data3', 'data4'}, 'fff');
REF = {'username   data1fff', 'username   data2fff'; 'username   data3fff', 'username   data4fff'};
asserts.isequal(R, REF);
%=============================================================================
R = append('username   ', ["data1", "data2    "; "data3", "data4"], '   fff');
REF = ["username   data1   fff", "username   data2       fff"; "username   data3   fff", "username   data4   fff"];
asserts.isequal(R, REF);
%=============================================================================
R = append('username', {'data1 ', 'data2  '; 'data3  ', 'data4  '}, 'fff');
REF = {'usernamedata1 fff', 'usernamedata2  fff'; 'usernamedata3  fff', 'usernamedata4  fff'};
asserts.isequal(R, REF);
%=============================================================================
R = append('/home/username', [string(NaN); "data2"], {'f1.csv'});
REF = [string(NaN); "/home/usernamedata2f1.csv"];
asserts.isequal(R, REF);
%=============================================================================
CMD = 'append( {''data''; ''data2''}, {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
asserts.checkerror(CMD, MSG);
%=============================================================================

