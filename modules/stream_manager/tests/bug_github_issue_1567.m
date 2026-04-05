%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1567
% <-- Short Description -->
% UTF-16LE output despite encoding='UTF-8' in fprintf builtin (Windows)
%=============================================================================
testdir = fileparts(mfilename('fullpath'));
if isempty(testdir)
	testdir = pwd;
end
testfile = fullfile(testdir, 'bug_1567_results.txt');
if isfile(testfile)
	rmfile(testfile);
end

% Write using fopen with explicit UTF-8 encoding
fid = fopen(testfile, 'w', 'n', 'UTF-8');
if fid < 0
	error('bug_github_issue_1567: cannot open test file for writing');
end
fprintf(fid, '1.000000 1.000000  0.106451  0.820279\n');
fclose(fid);
fid = fopen(testfile, 'rb');
assert_isfalse(fid < 0);
bytes = fread(fid, Inf, 'uint8');
fclose(fid);
assert_isfalse(isempty(bytes));
assert_isfalse(numel(bytes) >= 2 && all(bytes(2:2:end) == 0));
rmfile(testfile);
