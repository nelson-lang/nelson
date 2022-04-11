%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://httpbin.org/get';
filename = [tempdir(), 'test.txt'];
o = weboptions('ContentType','json');
o.Timeout = 60;
destination_filename = websave(filename, url, o);
txt = fileread(filename);
st = jsondecode(txt);
assert_isequal(st.url, url);
%=============================================================================
