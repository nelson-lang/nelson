%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://jsonplaceholder.typicode.com/posts/1/comments';
filename = [tempdir(), 'test.txt'];
o = weboptions('ContentType','json');
o.Timeout = 60;
destination_filename = websave(filename, url, o);
txt = fileread(filename);
st = jsondecode(txt);
assert_isequal(st(1).email, 'Eliseo@gardner.biz');
%=============================================================================
