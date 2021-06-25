%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
% <--EXCEL REQUIRED-->
%=============================================================================
assert_isequal(nargin('COM_xlsread'), -1);
assert_isequal(nargout('COM_xlsread'), 3);
%=============================================================================
[numeric_data, text_data, raw_data] = COM_xlsread([nelsonroot(), '/modules/com_engine/tests/format_excel97.xls']);
%=============================================================================
numeric_data_ref = [1, 2, 3, NaN, NaN, NaN;
0.5, 0.6, 0.9, NaN, NaN, NaN;
NaN, NaN, NaN, NaN, NaN, NaN;
NaN, NaN, NaN, NaN, NaN, 97];
assert_isequal(numeric_data, numeric_data_ref);
%=============================================================================
text_data_ref = {'', '', '', '', '', '';
'', '', '', '', '', '';
'', '', '', '', '', '';
'Nel', 'son', 'can', 'read', 'excel', ''};
assert_isequal(text_data, text_data_ref);
%=============================================================================
raw_data_ref = {[1.000000], [2.000000], [3.000000], [  NaN], [  NaN], [  NaN];
[0.500000], [0.600000], [0.900000], [  NaN], [  NaN], [  NaN];
[  NaN], [  NaN], [  NaN], [  NaN], [  NaN], [  NaN];
[  NaN], [  NaN], [  NaN], [  NaN], [  NaN], [97.000000]};
assert_isequal(raw_data, raw_data_ref);
%=============================================================================
[numeric_data, text_data, raw_data] = COM_xlsread([nelsonroot(), '/modules/com_engine/tests/format_excel97.xls'], 1);
assert_isequal(numeric_data, numeric_data_ref);
assert_isequal(text_data, text_data_ref);
assert_isequal(raw_data, raw_data_ref);
%=============================================================================
[numeric_data_1, text_data_1, raw_data_1] = COM_xlsread([nelsonroot(), '/modules/com_engine/tests/format_excel97.xls'], 1);
[numeric_data_2, text_data_2, raw_data_2] = COM_xlsread([nelsonroot(), '/modules/com_engine/tests/format_excel97.xls'], 'Feuil1');
assert_isequal(numeric_data_1, numeric_data_2);
assert_isequal(text_data_1, text_data_2);
assert_isequal(raw_data_1, raw_data_2);
%=============================================================================
[numeric_data, text_data] = COM_xlsread([nelsonroot(), '/modules/com_engine/tests/format_excel97.xls'], 2);
numeric_data_ref = [1,  NaN,  NaN,  NaN,  NaN;
NaN,    2,  NaN,  NaN,  NaN;
NaN,  NaN,    3,  NaN,  NaN;
NaN,  NaN,  NaN,    4,  NaN;
NaN,  NaN,  NaN,  NaN,    5;
NaN,  NaN,  NaN,  NaN,  NaN;
NaN,  NaN,  NaN,  NaN,  NaN;
NaN,  NaN,  NaN,  NaN,  NaN;
NaN,  NaN,   10,   20,  NaN;
NaN,  NaN,   30,   40,  NaN];
assert_isequal(numeric_data, numeric_data_ref);
assert_isequal(text_data, {});
%=============================================================================
[numeric_data, text_data] = COM_xlsread([nelsonroot(), '/modules/com_engine/tests/format_excel97.xls'], 2, 'A1:B2');
numeric_data_ref = [1,  NaN;
NaN,    2];
assert_isequal(numeric_data, numeric_data_ref);
assert_isequal(text_data, {});
%=============================================================================
