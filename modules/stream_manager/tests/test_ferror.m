%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ferror'), 2);
asserts.isequal(nargout('ferror'), 2);
%=============================================================================
R = ferror(stdout);
asserts.isequal(R, '');
%=============================================================================
[R1, R2] = ferror(stdout);
asserts.isequal(R1, '');
asserts.isequal(R2, 0);
%=============================================================================
filename = [tempname(), '.csv'];
fid = fopen(filename, 'w');
res = fgets(fid);
[msg, code] = ferror(fid);
fclose(fid);
asserts.isequal(code, -1);
asserts.istrue(ischar(msg));
asserts.isfalse(isempty(msg));
asserts.istrue(ndims(msg) == 2 && any(size(msg) <= 1));
rmfile(filename);
%=============================================================================
filename = [tempname(), '.csv'];
fid = fopen(filename, 'w');
res = fgets(fid);
[msg1, code1] = ferror(fid, 'clear');
[msg2, code2] = ferror(fid);
fclose(fid);
asserts.isequal(code1, -1);
asserts.isequal(code2, 0);
asserts.istrue(ischar(msg2));
asserts.istrue(isempty(msg2));
rmfile(filename);
%=============================================================================
