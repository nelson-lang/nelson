%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://stooq.com/q/d/l/?s=^aor&d1=20190401&d2=20190405&i=d/^aor_d.csv';
filename = [tempdir(), 'test.csv'];
o = weboptions();
o.Timeout = 60;
destination_filename = websave(filename, url, o);
info = dir(destination_filename);
assert_istrue(info.bytes > 200);
%=============================================================================
