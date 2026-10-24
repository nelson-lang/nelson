%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
esc = char(27);
%=============================================================================
styled = [esc, '[1;34mBold blue text', esc, '[0m'];
R = evalc('n = fprintf(styled);');
REF = 'Bold blue text';
asserts.isequal(R, REF);
asserts.isequal(n, length(REF));
%=============================================================================
styled = [esc, '[31mred', esc, '[0mplain'];
R = evalc('n = fprintf(styled);');
REF = 'redplain';
asserts.isequal(R, REF);
asserts.isequal(n, length(REF));
%=============================================================================
styled = [esc, '[3;4;9;48;2;10;20;30mfull style', esc, '[23;24;29;49m'];
R = evalc('n = fprintf(styled);');
REF = 'full style';
asserts.isequal(R, REF);
asserts.isequal(n, length(REF));
%=============================================================================
styled = [esc, '[48;2;10;20;30mbackground truecolor', esc, '[49m'];
R = evalc('n = fprintf(styled);');
REF = 'background truecolor';
asserts.isequal(R, REF);
asserts.isequal(n, length(REF));
%=============================================================================
styled = [esc, '[1;3;4;9;31;48;5;22mstyled', ...
          esc, '[22m normal-weight', ...
          esc, '[23m upright', ...
          esc, '[24m no-underline', ...
          esc, '[29m no-strike', ...
          esc, '[39m default-fg', ...
          esc, '[49m default-bg'];
R = evalc('n = fprintf(styled);');
REF = ['styled normal-weight upright no-underline no-strike ', ...
       'default-fg default-bg'];
asserts.isequal(R, REF);
asserts.isequal(n, length(REF));
%=============================================================================
styled = [esc, '[38;2;80;120;220mstderr text', esc, '[0m'];
R = evalc('n = fprintf(2, styled);');
REF = 'stderr text';
asserts.isequal(R, REF);
asserts.isequal(n, length(REF));
%=============================================================================
malformed = [esc, '[31bad'];
R = evalc('n = fprintf(malformed);');
asserts.isequal(R, malformed);
asserts.isequal(n, length(malformed));
%=============================================================================
notSgr = [esc, '[2J'];
R = evalc('n = fprintf(notSgr);');
asserts.isequal(R, notSgr);
asserts.isequal(n, length(notSgr));
%=============================================================================
filename = [tempname(), '.txt'];
fileID = fopen(filename, 'wt');
styled = [esc, '[38;5;12mfile text', esc, '[0m'];
n = fprintf(fileID, '%s', styled);
fclose(fileID);
R = fileread(filename);
asserts.isequal(R, styled);
asserts.isequal(n, length(styled));
rmfile(filename);
%=============================================================================
