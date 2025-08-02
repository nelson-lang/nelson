%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--EXCEL REQUIRED-->
%=============================================================================
assert_isequal(nargin('COM_xlswrite'), -3);
assert_isequal(nargout('COM_xlswrite'), -1);
%=============================================================================
if isfile([tempdir(), 'test_xlswrite_1.xlsx'])
  rmfile([tempdir(), 'test_xlswrite_1.xlsx']);
end
%=============================================================================
r = COM_xlswrite([tempdir(), 'test_xlswrite_1.xlsx'], eye(3, 3));
assert_istrue(r);
%=============================================================================
res = COM_xlsread([tempdir(), 'test_xlswrite_1.xlsx']);
assert_isequal(res, eye(3, 3));
%=============================================================================
if isfile([tempdir(), 'test_xlswrite_2.xlsx'])
  rmfile([tempdir(), 'test_xlswrite_2.xlsx']);
end
%=============================================================================
data = {'Time', 'Temp'; 12 98; 13 99; 14 97};
s = COM_xlswrite([tempdir(), 'test_xlswrite_2.xlsx'], data, 'Temperatures');
assert_istrue(s);
[r, s, d] = COM_xlsfinfo([tempdir(), 'test_xlswrite_2.xlsx']);
assert_isequal(s{1}, 'Temperatures');
%=============================================================================
